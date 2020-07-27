#' @param x is the model

check_assumptions_model <- function(x, groups) {
  if (class(x) == "lmerMod") {
    par(mfrow = c(1, 2))
    random_effects <- names(ranef(x))
    for (i in seq_len(length(random_effects))) {
      r <- ranef(x)[[random_effects[i]]][, 1]
      message(paste0("Random effect ", random_effects[i],
                     " normality - Shapiro's test p: ", 
                     round(shapiro.test(r)$p.value, 2)))
    }
  } else {
    par(mfrow = c(1, 2))
  }
  plot(fitted(x), resid(x, type = "pearson"),
       pch = 20, col = rgb(0.5, 0.5, 0.5, 0.5),
       xlab = "Fitted", ylab = "Residuals")
  abline(h = 0, lty = 2, lw = 2)
  qqnorm(resid(x, type = "pearson"),
         pch = 20, col = rgb(0.5, 0.5, 0.5, 0.5))
  qqline(resid(x, type = "pearson"),
         lty = 2, lw = 2)
}
