# Body size is a good proxy for vertebrate charisma

This repository contains the code and data used in the study *Body size is a good proxy for vertebrate charisma* (Berti et al., ...) under revision in *Biological Conservation*. R code is in the folder *R*. *datasets.rds* is the dataset used in the study, compiled from nine sources:

| Study | Vertebrate class | Sample size | Methodology used to estimate charisma | Area covered |
|:------|:-----------------|:------------|:--------------------------------------|:-------------|
|Brambilla et al. (2013) | Birds | 59 | Anthropic value from species occurrences in naturalistic journals and congresses| Italy |
|Correia et al. (2016) | Birds | 28 | Number of webpages containing the species name obtained using Google search engine | Brazil |
|Garnett et al. (2018) | Birds | 419 | Preference of people for species as determined from surveys | Australia |
| Macdonald et al. (2015) |Mammals | 92|Preference of people for species as determined from surveys |UK, USA, India, South Africa, and Australia|
|Monsarrat and Kerley (2018)|Mammals|37|Interpretation of species description from the historical literature|South Africa|
|Roberge (2014)|Mammals|12|Number of posts on Twitter for a species|USA|
|Roll et al. (2016)|Reptiles|7,133|Number of views of Wikipedia page (English version) for a species|Global
|Willemen et al. (2015)|||Number of images on Flickr for a species|Global|
||Amphibians|472|||
||Birds|2,365|||
||Mammals|1,952|||
||Reptiles|966|||
|Żmihorski et al. (2013)|Birds|145|Number of webpages for a species|Poland|

*charisma.csv* contains the data for the Shiny applications (below).

## Shiny app to explore residuals

I made a Shiny app to explore residuals and outliers. This can be launched downloading the entire repository and launching the script *R/shiny.R*. This will install also the required R packages: tidyverse, plotly, shiny, magrittr.