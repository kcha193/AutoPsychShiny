

# Installing and loading required packages --------------------------------

# Ensure all necessary packages installed on machine (tidyverse covers ggplot2 and dplyr)
# autopsych_pack <- c("ggrepel", "plyr", "scales", "tidyverse", "ShinyItemAnalysis",
#                     "CTT", "psychometric", "irr", "TAM", "cowplot", "openxlsx", "reshape2",
#                     "Hmisc",  "NCmisc" ,"xtable", "knitr", "rmarkdown", "kableExtra",
#                     "english", "shiny", "shinyjs", "shinythemes", "shinyBS", "bsplus",
#                     "shinyWidgets", "janitor", "magrittr", "emmeans", "s20x")
# packages_required <- autopsych_pack[!autopsych_pack %in% installed.packages()]
# 
# 
# if(length(packages_required) > 0) {
#   install.packages(packages_required)
# }
# 
# if(packageVersion("shiny") < "1.5.0") {
#   install.packages("shiny")
# }

# Ensure all necessary packages loaded loaded to library (some use package::function throughout script so do not need to be loaded)
# packages_to_load <- c("scales", "shinythemes",  "shinyBS", "bsplus", "shinyWidgets",
#                       "magrittr", "ggplot2", "shiny")

# Ensure all necessary packages loaded to library
# lapply(packages_to_load, library, character.only = TRUE)
# For list of packages, see associated publication.


library(scales)
library(shinythemes)
library(shinyBS)
library(bsplus)
library(shinyWidgets)
library(magrittr)
library(ggplot2)
library(shiny)


# Global constant variables -----------------------------------------------


# global constant variables currently storing the h1, h2 and h3 CSS

h1_css <- "h1{font-family: 'Open Sans'; font-weight: 500;
            line-height: 1.1; font-size: 60px; color: #FFFFFF;}"
  
h2_css <- "h2{font-family: 'Open Sans'; font-weight: 500;
            line-height: 1.1; font-size: 18pt; color: #FFFFFF;}"

h3_css <- "h3{font-family: 'Open Sans'; font-weight: 500;
            line-height: 1.1; font-size: 10pt; color: #FFFFFF;}"