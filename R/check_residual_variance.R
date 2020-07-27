#' @param x is the model
#' @groups is the grouping variable(s), passed as a list if more than one.

check_residual_variance <- function(x, groups) {
  if (class(x) == "lmerMod") {
    resp <- strsplit(as.character(x@call$formula), " ~ ")[[2]]
    par(mfrow = c(length(groups), 2))
    for (i in seq_len(length(groups))) {
      boxplot(
        x@frame[, resp] ~ x@frame[, groups[[i]]],
        xlab = groups[[i]], ylab = resp, col = "gray80",
        main = paste0("Bartlett's test p: ",
                      round(bartlett.test(x@frame[, resp],
                                          x@frame[, groups[[i]]])$p.value, 6))
      )
      boxplot(
        resid(x, type = "pearson") ~ x@frame[, groups[[i]]],
        xlab = groups[[i]], ylab = "Residuals", col = "gray80",
        main = paste0("Bartlett's test p: ",
                      round(bartlett.test(resid(x, type = "pearson"),
                                          x@frame[, groups[[i]]])$p.value, 6))
      )
    }
  } else if (class(x) == "lm") {
    resp <- strsplit(as.character(x$call$formula), " ~ ")[[2]]
    par(mfrow = c(length(groups), 2))
    for (i in seq_len(length(groups))) {
      boxplot(
        x$model[, resp] ~ x$model[, groups[[i]]],
        xlab = groups[[i]], ylab = resp, col = "gray80",
        main = paste0("Bartlett's test p: ",
                      round(bartlett.test(x$model[, resp],
                                          x$model[, groups[[i]]])$p.value, 6))
      )
      boxplot(
        resid(x, type = "pearson") ~ x$model[, groups[[i]]],
        xlab = groups[[i]], ylab = "Residuals", col = "gray80",
        main = paste0("Bartlett's test p: ",
                      round(bartlett.test(resid(x, type = "pearson"),
                                          x$model[, groups[[i]]])$p.value, 6))
      )
    }
  } else {
    stop("Model not of class 'lm' or 'lmerMod'")
  }
}
