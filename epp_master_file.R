################################################################################
# r code related to                                                            #
# "Global Relationships between Time Preference and Environmental Policy       #
# Performance" by Sergei Schaub published in Environmental Science & Policy    #
# -----------------------------------------------------------------------------#
# 2021, ETH Zurich                                                             #
# developed by Sergei Schaub                                                   #
# -----------------------------------------------------------------------------#
# master R script                                                              #
################################################################################


# table of contents:
# 0. settings
# 1. create folders and specify directories
# 2. call r scripts


# !!! Important remarks !!!:
# a) before running the r code you need to specify your main working directory, i.e., 'cd_main' (you find it in section 0),
#    and manually create in the main directory a folder 'r_code' in which you save all the provide r scripts
# b) after doing a) run the code until the end of section 1.
# c) save the online provided data and downloaded data in the folder 'input_data'.
## note: it is recommended to everything but the data generation.
# d) run section 2

# The final preparation and analysis of the data were done on February 10, 2020.

################################################################################
# 0. general settings
################################################################################

# clear global workspace
rm(list = ls())

# install packages
# install.packages(c("tidymodels","tidyverse","gtrendsR","scales","usethis","imputeTS","viridis","WDI","jtools","ggpubr","tabulizer",
#                    "ggthemes","corrplot","countrycode","devtools","ggstance","broom.mixed","scico", "broom","gtools","maps",
#                    "schoolmath","patchwork","DirectEffects","sessioninfo","zoo","tseries","urca","stinepack","dplyr", "renv"))

# load packages
require(tidymodels)
require(tidyverse)
require(gtrendsR)
require(scales)
require(imputeTS)
require(viridis)
require(WDI)
require(jtools)
require(ggpubr)
require(tabulizer)
require(ggthemes)
require(corrplot)
require(countrycode)
require(ggstance)
require(broom.mixed)
require(devtools)
require(scico)
require(broom)
require(gtools)
require(schoolmath)
require(patchwork)
require(DirectEffects)
require(maps)


# install and load the package in development stage
# devtools::install_github("alastairrushworth/inspectdf")
require(inspectdf)

# require(renv)
# renv::snapshot() # take snapshot of package version used
# renv::restore()  # restore package version originally used

################################################################################
# 1. create folders and specify directories
################################################################################

# define main working directory
cd_main <- "xxxx"        #### !!! Needs to be specified !!!
setwd(cd_main) # set directory

# create additional folders and working directories
## r code
dir.create("code")
cd_code <- paste0(cd_main,"/code")

## input data
dir.create("data")
cd_input_data <- paste0(cd_main,"/data")

## results
setwd(cd_main) # set directory
dir.create("results")
cd_results <- paste0(cd_main,"/results")


################################################################################
# 2. call r scripts
################################################################################

# run r scripts
setwd(cd_code) # set directory
# source("epp_data_preparation.R")                 # compile and generate data. this step can take some time.
                                                   # additionally, google returns a random sample, thus, it can change between downloads.
                                                   # thus, we provide the entire dataset and recommend to start with the next r script

setwd(cd_code) # set directory
source("epp_data_summary.R")                       # data summary and maps

setwd(cd_code) # set directory
source("epp_analysis.R")                           # main analysis

setwd(cd_code) # set directory
source("epp_specification_charts.R")               # specification charts

setwd(cd_code) # set directory
source("epp_non_bootstrapped_models.R")            # non-bootstrapped regression analysis

setwd(cd_code) # set directory
source("epp_direct_effects.R")                     # separate direct effects (sensu Acharya et al. (2016))

setwd(cd_code) # set directory
source("epp_alternative_time_preferece_data.R")    # analysis of alternative time preference indices
