library(tidyverse)
library(rredlist)
library(performance)
library(modelbased)
library(parameters)
library(see)
library(cowplot)
library(broom)
library(knitr)
library(MuMIn)
library(lme4)
library(ggsci)
library(rgbif)
library(car)
library(plotly)
library(magrittr)

source("get_iucn_status.R")
source("plot_iucn_means.R")
source("monkey_theme.R")
source("check_assumptions_model.R")
source("check_residual_variance.R")

#Scott's IUCN api token
Sys.setenv(IUCN_REDLIST_KEY = "3e6d8e748f8f736fdc9fbab9670c1c436b1059670a5df563583323f18cc8d3ad")

#color-blind friendly palette
Pal <- c("#E69F00", "#009E73", "#0072B2", "#D55E00")

datasets <- tryCatch(
  read_rds("../datasets.rds"), #this will load the ranked datasets already computed
  error = function(e) NULL #exception for missing data
)

if (is_null(datasets)) { #skip if ../datasets.rds already present
  source("1_wrangle_data.R") #this will also get IUCN conservation status for all species and may take some time.
}

# fix negative values for log-transformation ----
datasets %>% 
  group_by(Dataset) %>% 
  summarise(Min = min(Raw))

# add 0.01 to brambilla and 1.08 to MacDonald
datasets$Raw[datasets$Dataset == "Brambilla"] <- datasets$Raw[datasets$Dataset == "Brambilla"] + 1.08
datasets$Raw[datasets$Dataset == "MacDonald"] <- datasets$Raw[datasets$Dataset == "MacDonald"] + 1.08

# transform data ----------------------------------------------------------
datasets <- datasets %>% 
  filter(IUCN != "EW",
         IUCN != "EX", #remove extinct species
         str_detect(IUCN, "/", negate = TRUE)) %>% #remove dubious IUCN status 
  group_by(Dataset) %>% 
  mutate(ScaledLogMass = scale(log10(Mass))[, 1],
         ScaledLogRaw = scale(log10(Raw))[, 1]) %>% 
  ungroup()

historic <- datasets %>% 
  filter(Dataset == "Monsarrat")

d <- datasets %>% 
  filter(Dataset != "Monsarrat") %>% 
  mutate(IUCN = factor(IUCN, levels = c('DD', 'LC', 'NT', 'VU', 'EN', 'CR')),
         Dataset = factor(Dataset),
         Class = factor(Class)) %>% 
  filter(!is.na(IUCN))

# remove one outlier
d %<>% filter(!(Species == "Mus musculus" & Dataset == "MacDonald"))

# plot data ------
datasets %>% 
  ggplot() +
  aes(ScaledLogMass, ScaledLogRaw, col = Class, shape = Dataset) +
  geom_point(alpha = 1) +
  geom_smooth(method= "lm") +
  scale_shape_manual(values = 1:10) +
  facet_wrap( ~ Dataset, scales = "free", ncol = 2) +
  theme_classic() +
  theme(strip.background = element_blank())

datasets %>% 
  ggplot() +
  aes(ScaledLogRaw, fill = Class) +
  geom_histogram() +
  facet_wrap( ~ Dataset, scales = "free", ncol = 2) +
  theme_classic() +
  theme(strip.background = element_blank())


# analyses ----------
options(na.action = "na.fail")

# Two things to note:
# 1. slope of the relationship changes across datasets and classes
# 2. intercept of the relationship changes across datasets and classes

# I initialize what it looks like the best model, i.e. with random slope and
# random intercept the interaction between class and dataset.

# m1 <- lmer( # this did not converge. 12 hours long  
#   ScaledLogRaw ~ 1 + ScaledLogMass * IUCN + Class + (Dataset:Class | Dataset:Class),
#   data = d,
#   REML = FALSE,
#   lmerControl(optimizer = "bobyqa")
# )

m1 <- lmer( # this is the best model I found
  ScaledLogRaw ~ 1 + ScaledLogMass * IUCN + Class + (1 | Dataset),
  data = d,
  REML = FALSE,
  lmerControl(optimizer = "bobyqa")
)

m2 <- lmer(
  ScaledLogRaw ~ 1 + ScaledLogMass * IUCN + Class + (1 | Class:Dataset),
  data = d,
  REML = FALSE,
  lmerControl(optimizer = "bobyqa")
)

anova(m1, m2)

m <- m1

check_assumptions_model(m)
check_residual_variance(m, c("IUCN", "Class", "Dataset"))

dredge(m) #full model strongly supported (DAICc > 2)

m <- lmer(
  m@call$formula,
  data = d,
  REML = TRUE,
  lmerControl(optimizer = "bobyqa")
)

check_assumptions_model(m)
check_residual_variance(m, groups = list("IUCN", "Class", "Dataset"))

Anova(m, type = "III")
confint(m)
r2_nakagawa(m)

m %>% 
  parameters() %>% 
  plot()

preds <- predict(m, d)

status.colors <- c("EP" = "#87421F", "EX" = "#8F47B3", "EW" = "#8F47B3", 
                   "CR" = "#D81E05", "EN" = "#FC7F3F", "VU" = "#F9E814", 
                   "NT" = "#CCE226", "LC" = "#60C659", "DD" = "#D1D1C6")
p <- d %>% 
  mutate(Pred = preds) %>% 
  arrange(Mass, Pred) %>%
  group_by(Class) %>% 
  mutate(Max_mass = max(Mass)) %>% 
  add_tally() %>%
  ungroup() %>% 
  mutate(Class = map(Class, function(x){
    switch(x,
           "Amphibia" = "Amphibians",
           "Aves" = "Birds",
           "Mammalia" = "Mammals",
           "Reptilia" = "Reptiles")
  }) %>% unlist()) # %>% 

# colour result figure ----
p %>% 
  mutate(IUCN = factor(IUCN, levels = rev(levels(IUCN)))) %>% 
  ggplot() +
  geom_point(aes(ScaledLogMass, ScaledLogRaw), 
             col = "grey50", pch = 20, alpha = 0.35) +
  geom_smooth(aes(ScaledLogMass, Pred, col = IUCN), 
              method = "lm", alpha = 0, size = 1) +
  scale_color_manual(values = status.colors, 
                     name = "IUCN") +
  ylab("Charisma") +
  xlab("Body size (g)") +
  theme_modern() +
  theme(legend.position = "right",
        panel.grid = element_blank(),
        axis.ticks = element_line(),
        legend.key.width = unit(2, "cm"),
        legend.title = element_text(hjust = 0.5))

ggsave("Figures/results1_col.png", width = 6, height = 5)

# residuals ------------------------
d %<>% mutate(Res = resid(m))

res_plot <- d %>% 
  ggplot() +
  geom_point(aes(ScaledLogMass, Res, col = Dataset, group = Species), 
             size = 1, show.legend = FALSE, alpha = 1) +
  geom_hline(yintercept = 0, linetype = "dashed", col = "tomato", size = 1) +
  theme_bw() +
  scale_color_brewer(type = "qual", palette = "Set2")

ggplotly(res_plot, tooltip = "Species")

# IUCN means and contrasts ------------------------------
emmeans::emm_options(pbkrtest.limit = 15000)
emmeans::emm_options(lmerTest.limit = 15000)

#means <- estimate_means(m, levels = "IUCN") #long - load the already computed at next line
means <- read_rds("Results/IUCN_means.rds")

datasets %>% 
  filter(!(Species == "Mus musculus" & Dataset == "MacDonald")) %>% 
  mutate(IUCN = factor(IUCN, levels = c("DD", "LC", "NT",
                                        "VU", "EN", "CR"))) %>% 
  ggplot(aes(IUCN, ScaledLogRaw, fill = IUCN)) +
  geom_jitter2(width = 0.025, aes(group = IUCN), alpha = 0.25) +
  geom_violin(bw = "nrd", alpha = 0.5) +
  geom_pointrange(data = means, 
                  aes(y = Mean, ymin = CI_low, ymax = CI_high), 
                  size = 0.75, color = "white") +
  scale_fill_manual(values = status.colors, name = "IUCN conservation status") +
  xlab("IUCN conservation status") +
  ylab("Charisma") +
  theme_modern(legend.position = "none")

ggsave("Figures/IUCN_means.svg", width = 8, height = 6)
ggsave("Figures/IUCN_means.png", width = 8, height = 6)

#conts <- estimate_contrasts(m, levels = "IUCN", adjust = "bonferroni") #long - load the already computed at next line
conts <- read_rds("Results/IUCN_contrast.rds")

conts %>% 
  as_tibble() %>% 
  mutate(Cont = paste0(Level1, " - ", Level2),
         Col = map2(Difference, p, function(x, y) {
           if (y >= 0.05) {
             "non-sign"
           } else {
             if (x > 0) {
               "greater"
             } else {
               "lower"
             }
           }
         }) %>% unlist()) %>% 
  mutate(Cont = factor(Cont, levels = rev(unique(Cont)))) %>% 
  ggplot() +
  geom_abline(intercept = 0, slope = 0, linetype = "dashed") +
  geom_point(aes(Cont, Difference, col = Col), show.legend = FALSE) +
  geom_errorbar(aes(Cont, ymin = CI_low, ymax = CI_high, col = Col), width = 0.0, show.legend = FALSE) +
  scale_color_manual(values = c("tomato", "grey50")) +
  xlab("Contrast") +
  ylab("Difference between model coefficients") +
  coord_flip() +
  theme_classic() 

ggsave("Figures/IUCN_contrast.png", width = 6, height = 4)

# Class mean and contrast ------------------------------
emmeans::emm_options(pbkrtest.limit = 15000)
emmeans::emm_options(lmerTest.limit = 15000)

#means <- estimate_means(m, levels = "Class")
means <- read_rds("Results/Class_mean.rds") %>% #long - load the already computed at next line
  as_tibble() %>% 
  mutate(Class = modify(as.character(Class), function(x) {
    switch (x,
            "Amphibia" = "Amphibians",
            "Aves" = "Birds",
            "Mammalia" = "Mammals",
            "Reptilia" = "Reptiles"
    )
  }))

datasets %>% 
  filter(!(Species == "Mus musculus" & Dataset == "MacDonald")) %>% 
  mutate(Class = modify(Class, function(x) {
    switch (x,
            "Amphibia" = "Amphibians",
            "Aves" = "Birds",
            "Mammalia" = "Mammals",
            "Reptilia" = "Reptiles"
    )
  })) %>% 
  ggplot(aes(Class, ScaledLogRaw, fill = Class)) +
  geom_jitter2(width = 0.025, aes(group = Class), alpha = 0.25) +
  geom_violin(bw = "nrd", alpha = 0.5) +
  geom_pointrange(data = means, 
                  aes(y = Mean, ymin = CI_low, ymax = CI_high), 
                  size = 0.75, color = "white") +
  scale_fill_startrek(name = "Vertebrate class") +
  xlab("Vertebrate class") +
  ylab("Carismatic value") +
  theme_modern(legend.position = "none")

ggsave("Figures/Class_means.png", width = 6, height = 5)

#conts <- estimate_contrasts(m, levels = "Class", adjust = "bonferroni")
conts <- read_rds("Results/Class_contrast.rds") %>% #long - load the already computed at next line
  as_tibble() %>% 
  mutate(
    Level1 = modify(as.character(Level1), function(x) {
      switch (x,
              "Amphibia" = "Amphibians",
              "Aves" = "Birds",
              "Mammalia" = "Mammals",
              "Reptilia" = "Reptiles"
      )
    }),
    Level2 = modify(as.character(Level2), function(x) {
      switch (x,
              "Amphibia" = "Amphibians",
              "Aves" = "Birds",
              "Mammalia" = "Mammals",
              "Reptilia" = "Reptiles"
      )
    })
  )

conts %>% 
  as_tibble() %>% 
  mutate(Cont = paste0(Level1, " - ", Level2),
         Col = map2(Difference, p, function(x, y) {
           if (y >= 0.05) {
             "non-sign"
           } else {
             if (x > 0) {
               "greater"
             } else {
               "lower"
             }
           }
         }) %>% unlist()) %>% 
  mutate(Cont = factor(Cont, levels = rev(unique(Cont)))) %>% 
  ggplot() +
  geom_abline(intercept = 0, slope = 0, linetype = "dashed") +
  geom_point(aes(Cont, Difference, col = Col), show.legend = FALSE) +
  geom_errorbar(aes(Cont, ymin = CI_low, ymax = CI_high, col = Col), width = 0.0, show.legend = FALSE) +
  scale_color_manual(values = c("steelblue", "tomato")) +
  xlab("Contrast") +
  ylab("Difference between model coefficients") +
  coord_flip() +
  theme_classic() 

ggsave("Figures/Class_contrast.png", width = 6, height = 3)

# multiple LMs ------------------------------------------------------------
datasets <- read_rds("../datasets.rds") #this loads the ranked datasets as computed in the next line
Monsarrat <- datasets %>% filter(Dataset == "Monsarrat")
datasets$Raw[datasets$Dataset == "Brambilla"] <- datasets$Raw[datasets$Dataset == "Brambilla"] + 1.08
datasets$Raw[datasets$Dataset == "MacDonald"] <- datasets$Raw[datasets$Dataset == "MacDonald"] + 1.08
datasets <- datasets %>% 
  filter(IUCN != "EW",
         IUCN != "EX", #remove extinct species
         str_detect(IUCN, "/", negate = TRUE)) %>% #remove dubious IUCN status 
  group_by(Dataset) %>% 
  mutate(ScaledLogMass = scale(log10(Mass))[, 1],
         ScaledLogRaw = scale(log10(Raw))[, 1]) %>% 
  ungroup() %>% 
  mutate(Dataset = factor(Dataset))

ans <- list()

x <- levels(datasets$Dataset)[1]
d <- filter(datasets, Dataset == x)
d %>% group_by(IUCN) %>% tally()
m <- lm(ScaledLogRaw ~ 1 + ScaledLogMass + IUCN, data = d)
car::vif(m)
dredge(m) # without IUCN
m <- lm(ScaledLogRaw ~ 1 + ScaledLogMass, data = d)
check_assumptions_model(m)
m$model <- cbind(m$model, 
                 Species = d$Species,
                 IUCN = d$IUCN,
                 Class = d$Class)
m$model <- m$model[, !duplicated(names(m$model))]
ans[[x]][["model"]] <- m
ans[[x]][["Anova"]] <- Anova(m, type = "III")
ans[[x]][["Residuals"]] <- resid(m)

x <- levels(datasets$Dataset)[2]
d <- filter(datasets, Dataset == x)
d %>% group_by(IUCN) %>% tally()
m <- lm(ScaledLogRaw ~ 1 + ScaledLogMass, data = d)
check_assumptions_model(m)
dredge(m) #second model to be considered
m$model <- cbind(m$model, 
                 Species = d$Species,
                 IUCN = d$IUCN,
                 Class = d$Class)
m$model <- m$model[, !duplicated(names(m$model))]
ans[[x]][["model"]] <- m
ans[[x]][["Anova"]] <- car::Anova(m, type = "III")
ans[[x]][["Residuals"]] <- resid(m)

x <- levels(datasets$Dataset)[3]
d <- filter(datasets, Dataset == x)
d %>% group_by(IUCN) %>% tally()
m <- lm(ScaledLogRaw ~ 1 + ScaledLogMass + IUCN + ScaledLogMass:IUCN, data = d)
car::vif(m)
dredge(m)
m <- lm(ScaledLogRaw ~ 1 + ScaledLogMass + IUCN, data = d)
check_assumptions_model(m)
m$model <- cbind(m$model, 
                 Species = d$Species,
                 IUCN = d$IUCN,
                 Class = d$Class)
m$model <- m$model[, !duplicated(names(m$model))]
ans[[x]][["model"]] <- m
ans[[x]][["Anova"]] <- car::Anova(m, type = "III")
ans[[x]][["Residuals"]] <- resid(m)

x <- levels(datasets$Dataset)[4]
d <- filter(datasets, Dataset == x)
d %>% group_by(IUCN) %>% tally()
m <- lm(ScaledLogRaw ~ 1 + ScaledLogMass * IUCN, data = d)
dredge(m)
check_assumptions_model(m)
m$model <- cbind(m$model, 
                 Species = d$Species,
                 IUCN = d$IUCN,
                 Class = d$Class)
m$model <- m$model[, !duplicated(names(m$model))]
ans[[x]][["model"]] <- m
ans[[x]][["Anova"]] <- Anova(m, type = "III")
ans[[x]][["Residuals"]] <- resid(m)

x <- levels(datasets$Dataset)[5]
d <- filter(datasets, Dataset == x)
d %>% group_by(IUCN) %>% tally()
m <- lm(ScaledLogRaw ~ 1 + ScaledLogMass + IUCN, data = d)
car::vif(m)
dredge(m) #second model to be considered
m <- lm(ScaledLogRaw ~ 1 + ScaledLogMass, data = d)
check_assumptions_model(m)
m$model <- cbind(m$model, 
                 Species = d$Species,
                 IUCN = d$IUCN,
                 Class = d$Class)
m$model <- m$model[, !duplicated(names(m$model))]
ans[[x]][["model"]] <- m
ans[[x]][["Anova"]] <- car::Anova(m, type = "III")
ans[[x]][["Residuals"]] <- resid(m)

x <- levels(datasets$Dataset)[6]
d <- filter(datasets, Dataset == x)
d %>% group_by(IUCN) %>% tally()
m <- lm(ScaledLogRaw ~ 1 + ScaledLogMass * IUCN, data = d)
car::vif(m)
dredge(m)
m <- lm(ScaledLogRaw ~ 1 + ScaledLogMass, data = d)
check_assumptions_model(m)
m$model <- cbind(m$model, 
                 Species = d$Species,
                 IUCN = d$IUCN,
                 Class = d$Class)
m$model <- m$model[, !duplicated(names(m$model))]
ans[[x]][["model"]] <- m
ans[[x]][["Anova"]] <- car::Anova(m, type = "III")
ans[[x]][["Residuals"]] <- resid(m)

x <- levels(datasets$Dataset)[7]
d <- filter(datasets, Dataset == x)
d %>% group_by(IUCN) %>% tally()
m <- lm(ScaledLogRaw ~ 1 + ScaledLogMass + IUCN + ScaledLogMass:IUCN, data = d)
car::vif(m)
dredge(m)
check_assumptions_model(m)
m$model <- cbind(m$model, 
                 Species = d$Species,
                 IUCN = d$IUCN,
                 Class = d$Class)
m$model <- m$model[, !duplicated(names(m$model))]
ans[[x]][["model"]] <- m
ans[[x]][["Anova"]] <- car::Anova(m, type = "III")
ans[[x]][["Residuals"]] <- resid(m)

x <- levels(datasets$Dataset)[8]
d <- filter(datasets, Dataset == x)
d %>% group_by(IUCN) %>% tally()
m <- lm(ScaledLogRaw ~ 1 + ScaledLogMass + IUCN + Class + ScaledLogMass:IUCN, 
        data = d)
car::vif(m)
dredge(m)
m <- lm(ScaledLogRaw ~ 1 + ScaledLogMass + IUCN + Class, data = d)
check_assumptions_model(m)
m$model <- cbind(m$model, 
                 Species = d$Species,
                 IUCN = d$IUCN,
                 Class = d$Class)
m$model <- m$model[, !duplicated(names(m$model))]
ans[[x]][["model"]] <- m
ans[[x]][["Anova"]] <- car::Anova(m, type = "III")
ans[[x]][["Residuals"]] <- resid(m)

x <- "Monsarrat"
d <- Monsarrat %>% 
  mutate(ScaledLogMass = scale(log10(Mass))[, 1],
         ScaledLogRaw = scale(log10(Raw))[, 1])
m <- lm(ScaledLogRaw ~ 1 + ScaledLogMass, data = d)
dredge(m)
check_assumptions_model(m)
m$model <- cbind(m$model, 
                 Species = d$Species,
                 IUCN = d$IUCN,
                 Class = d$Class)
m$model <- m$model[, !duplicated(names(m$model))]
ans[[x]][["model"]] <- m
ans[[x]][["Anova"]] <- car::Anova(m, type = "III")
ans[[x]][["Residuals"]] <- resid(m)

# R2
r2 <- sapply(names(ans), function(x) {r2(ans[[x]]$model)[[2]]}) %>% 
  round(., 2)

# summary results from LMs
res <- lapply(names(ans), function(x)
  ans[[x]][["Anova"]] %>% 
    as_tibble %>% 
    mutate(Var = rownames(ans[[x]][["Anova"]]),
           Dataset = x)) 

res <- bind_rows(res) %>% 
  filter(Var != "Residuals")

res %>% 
  filter(Var == "ScaledLogMass")

res %>% 
  filter(Var == "ScaledLogMass",
         `Pr(>F)` < 0.05)

res <- list()
for (x in names(ans)) {
  res[[x]] <- tibble(Mean = coef(ans[[x]]$model)["ScaledLogMass"],
                     CI_low = confint(ans[[x]]$model)["ScaledLogMass", 1],
                     CI_high = confint(ans[[x]]$model)["ScaledLogMass", 2],
                     Dataset = x,
                     p = ans[[x]]$Anova[2, 4])
}

res <- bind_rows(res) %>% 
  mutate(Dataset = modify(Dataset, function(x) {
    switch (x,
            "Brambilla" = "Brambilla et al. (2013)",
            "Correia" = "Correia et al. (2016)",
            "Garnett" = "Garnett et al. (2018)",
            "MacDonald" = "MacDonald et al. (2015)",
            "Polish" = "Żmihorski et al. (2013)",
            "Roberge" = "Roberge (2014)",
            "Roll" = "Roll et al. (2016)",
            "Willemen" = "Willemen et al. (2015)",
            "Monsarrat" = "Monsarrat & Kerley (2018)"
    )
  }) %>% as_factor()) %>% 
  mutate(Dataset = factor(Dataset, levels = rev(levels(Dataset)))) %>% 
  mutate(Col = c("steelblue", "grey50", "tomato", 
                 "steelblue", "grey50", "steelblue",
                 "steelblue","steelblue","steelblue"))

# gray-scale figure -----------
res %>% 
  mutate(Col = Col,
         p = as.character(p),
         p = modify(p, function(x) {
           if (x < 0.05) {
             "p < 0.05"
           } else {
             "p \u2265 0.05"
           }
         })) %>% 
  ggplot() +
  geom_abline(intercept = 0, slope = 0, linetype = "dashed") +
  geom_errorbar(aes(Dataset, ymin = CI_low, ymax = CI_high), width = 0, show.legend = FALSE) +
  geom_point(aes(Dataset, Mean, shape = Col), fill = "white", show.legend = FALSE) +
  #geom_text(aes(x = Dataset, y = CI_high, label = p)) +
  xlab("Study") +
  ylab("Body size coefficient") +
  scale_color_manual(values = c("grey50", "steelblue", "tomato")) +
  scale_shape_manual(values = c(21, 19, 19)) +
  coord_flip() +
  theme_classic()

ggsave("Figures/LM_means.png", width = 6, height = 4)

# Residuals LMs -----------------------------------------------------------
res <- lapply(names(ans), function(x)
  ans[[x]]$model$model %>%
    as_tibble() %>%
    mutate(Dataset = x,
           Res = ans[[x]][["Residuals"]])
) %>%
  bind_rows() %>%
  unique()

data_to_shiny <- res %>% 
  group_by(Species, Dataset, IUCN, Class) %>% 
  summarize(Mass = mean(Mass),
            ScaledLogRaw = mean(ScaledLogRaw),
            Res = mean(Res)) %>% 
  ungroup() %>% 
  mutate(Dataset = modify(Dataset, function(x) {
    switch (x,
            "Brambilla" = "Brambilla et al. (2013)",
            "Correia" = "Correia et al. (2016)",
            "Garnett" = "Garnett et al. (2018)",
            "MacDonald" = "MacDonald et al. (2015)",
            "Polish" = "Żmihorski et al. (2013)",
            "Roberge" = "Roberge (2014)",
            "Roll" = "Roll et al. (2016)",
            "Willemen" = "Willemen et al. (2015)",
            "Monsarrat" = "Monsarrat & Kerley (2018)"
    )
  })) %>% 
  arrange(Dataset)

# write_rds(data_to_shiny, "../shiny.rds")
