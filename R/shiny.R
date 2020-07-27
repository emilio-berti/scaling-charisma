pkg <- c("tidyverse",
         "plotly",
         "shiny",
         "magrittr")

package.check <- lapply(
  pkg,
  FUN = function(x) {
    if (!require(x, character.only = TRUE)) {
      install.packages(x, dependencies = TRUE)
      library(x, character.only = TRUE)
    }
  }
)

source("app.R")
shinyApp(ui = ui, server = server)
