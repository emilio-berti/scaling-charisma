if(!"datasets" %in% ls()){
  
  marine.families <- c("Balaenidae", "Balaenopteridae", "Delphinidae", "Dugongidae", "Eschrichtiidae", "Iniidae", "Monodontidae", "Neobalaenidae", "Odobenidae", "Otariidae", "Phocidae", "Phocoenidae", "Physeteridae", "Platanistidae", "Trichechidae", "Ziphiidae")
  
  mammals <- read_csv("https://raw.githubusercontent.com/MegaPast2Future/PHYLACINE_1.2/master/Data/Traits/Trait_data.csv", col_types = cols()) %>% #this reads from online repository
    filter(!Family.1.2 %in% marine.families) %>% 
    mutate(
      Species = sub("_", " ", Binomial.1.2),
      Mass = Mass.g
    )
  
  birds <- read_tsv("http://www.esapubs.org/archive/ecol/E088/096/avian_ssd_jan07.txt", col_types = cols()) %>% #this reads from online repository
    mutate(
      Species = Species_name, 
      Mass = F_mass
    ) %>% 
    filter(Mass > 0)
  
  reptiles <- read_csv("Data/reptiles.csv", skip = 2, col_types = cols()) %>% #download the dataset from https://onlinelibrary.wiley.com/doi/full/10.1111/geb.12491
    mutate(Species = Binomial) %>% 
    mutate(Mass = 10^`Maximum mass (log10(g))`) %>% 
    filter(!is.na(Mass))
  
  amphibians <- read_csv("Data/amphibians.csv", col_types = cols()) %>% #download the dataset from https://www.nature.com/articles/sdata2017123
    mutate(Mass = Body_mass_g) %>% 
    filter(!is.na(Mass))
  
  masses <- bind_rows(
    select(mammals, Species, Mass), 
    select(birds, Species, Mass),
    select(reptiles, Species, Mass),
    select(amphibians, Species, Mass)
  )
  
  Cottam <- read_csv("Data/Cottam_data.csv", col_types = cols()) %>% 
    mutate(Species = binomial) %>% 
    full_join(masses) %>% 
    filter(
      !is.na(Mass),
      !is.na(class),
      flickr_image_count > 0,
      Species %in% c(mammals$Species, birds$Species, reptiles$Species, amphibians$Species)
    ) %>% 
    arrange(desc(flickr_image_count)) %>% 
    transmute(
      Species,
      Raw = flickr_image_count,
      Rank = (nrow(.):1) / nrow(.),
      IUCN = factor(red_list_status, levels = c('LC', 'NT', 'VU', 'EN', 'CR')),
      Dataset = "Willemen",
      Class = class
    )
  Cottam <- Cottam %>% 
    left_join(masses) %>% 
    filter(!is.na(Mass))
  
  Brambilla <- read_delim("Data/Brambilla et al 2013.csv", col_type = cols(), delim = ";") %>% 
    arrange(desc(`anthropic value`)) %>% 
    transmute(Species,
              Raw = `anthropic value`,
              Rank = (nrow(.):1) / nrow(.),
              Class = "Aves",
              Dataset = "Brambilla") %>% 
    left_join(masses)
  Brambilla <- Brambilla %>% 
    filter(!is.na(Mass))
  Brambilla$IUCN <- get_iucn_status(Brambilla$Species)
  
  Roberge <- read_csv("Data/Roberge2014mammals.csv", col_types = cols(), col_names = "Species") %>% 
    transmute(Species,
              Rank = (nrow(.):1) / nrow(.),
              Class = "Mammalia",
              Dataset = "Roberge") 
  Roberge <- Roberge %>% 
    left_join(masses) %>% 
    filter(!is.na(Mass))
  Roberge$IUCN <- get_iucn_status(Roberge$Species)
  
  Polish <- read_csv("Data/Zmihorski2012.csv", col_type = cols(), skip = 1, col_names = c("Species", "Name", "Total")) %>% 
    transmute(Species,
              Raw = Total, 
              Rank = (nrow(.):1) / nrow(.),
              Class = "Aves",
              Dataset = "Polish") 
  Polish <- Polish %>% 
    left_join(masses) %>% 
    filter(!is.na(Mass))
  Polish$IUCN <- get_iucn_status(Polish$Species)
  
  MacDonald <- read_csv("Data/MacDonald2015.csv", col_types = cols(), skip = 1,
                        col_names = c("ID", "Common name", "Species", "score", "regions", "Rank")) %>% 
    left_join(mammals) %>% 
    transmute(Species,
              Raw = score,
              Rank = Rank,
              Class = "Mammalia",
              Dataset = "MacDonald",
              Mass = Mass.g,
              IUCN = IUCN.Status.1.2) %>% 
    filter(!is.na(Mass))
  
  Monsarrat <- read_csv("Data/MonsarratKerley2018_score.csv", col_types = cols(), col_names = c("Species", "Rank")) %>% 
    transmute(Species,
              Raw = Rank,
              Rank = (nrow(.):1) / nrow(.),
              Class = "Mammalia",
              Dataset = "Monsarrat") %>% 
    left_join(masses) %>% 
    filter(!is.na(Mass))
  
  Correia <- read_delim("Data/Correia.csv", delim = ';', col_types = cols()) %>% 
    transmute(Species = `Scientific name`,
              Raw = `English webpage hits`,
              Rank = (nrow(.):1) / nrow(.),
              Class = "Aves",
              Dataset = "Correia") 
  Correia <- Correia %>% 
    left_join(masses) %>% 
    filter(!is.na(Mass))
  Correia$IUCN <- get_iucn_status(Correia$Species)
  
  Roll <- read_csv("Data/roll.csv", col_types = cols()) %>% 
    arrange(desc(Total)) %>% 
    transmute(Species = Binomial,
              Raw = Total, 
              Rank = (nrow(.):1) / nrow(.),
              Class = "Reptilia",
              Dataset = "Roll") 
  Roll <- Roll %>% 
    left_join(masses) %>% 
    filter(!is.na(Mass))
  # I modified the step to get the IUCN status for Roll et al. (2016) as it kept
  # crashing.
  tmp_iucn <- list()
  for (x in Roll$Species) {
    tmp_iucn[[x]] <- tryCatch(
      get_iucn_status(x),
      error = function(x) NA
    )
  }
  if (all(Roll$Species == names(tmp_iucn))) {
    Roll$IUCN <- unlist(tmp_iucn)
    rm(tmp_iucn)
  }
  
  Garnett <- read_csv("Data/Garnett.csv", col_types = cols()) %>% 
    transmute(Species = `Taxon Scientific Name`,
              Raw = `Attractiveness Score`,
              Rank = (nrow(.):1) / nrow(.),
              Class = "Aves",
              Dataset = "Garnett") 
  Garnett <- Garnett %>% 
    left_join(masses) %>% 
    filter(!is.na(Mass))
  Garnett$IUCN <- get_iucn_status(Garnett$Species)
  
  datasets <- bind_rows(Cottam,
                        Brambilla,
                        Roberge,
                        Polish,
                        MacDonald,
                        Correia,
                        Monsarrat,
                        Roll,
                        Garnett) %>% 
    mutate(Dataset = factor(Dataset)) 
  
} else{
  print("You have already loaded the previously wrangled dataset: using that instead.")
}
