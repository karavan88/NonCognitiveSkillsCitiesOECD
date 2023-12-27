#-------------------------------------------------------------------
# Project: Non-Cognitive Skills
# Script: Upload packages
# Author: Garen Avanesian
# Date: 21 October 2023
#-------------------------------------------------------------------

# List of required packages

required_packages <- c("tidyverse",
                       "haven",
                       "lme4",
                       "lmerTest",
                       "lmtest",
                       "sjPlot",
                       "srvyr",
                       "psych",
                       "corrplot",
                       "RColorBrewer",
                       "here",
                       "knitr",
                       "glue",
                       "gtsummary",
                       "gt",
                       "kableExtra",
                       "broom",
                       "broom.mixed",
                       "ggeffects",
                       "flextable",
                       "skimr")


# Function to check and install packages
check_and_install_packages <- function(packages) {
  for (package in packages) {
    if (!requireNamespace(package, quietly = TRUE)) {
      install.packages(package)
    }
    library(package, character.only = TRUE)
  }
}


# Call the function with the list of required packages
check_and_install_packages(required_packages)