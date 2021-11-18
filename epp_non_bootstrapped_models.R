################################################################################
# r code related to                                                            #
# "Global Relationships between Time Preference and Environmental Policy       #
# Performance" by Sergei Schaub published in Environmental Science & Policy    #
# -----------------------------------------------------------------------------#
# 2021, ETH Zurich                                                             #
# developed by Sergei Schaub                                                   #
# -----------------------------------------------------------------------------#
# regression models (without bootstrapping)                                    #
################################################################################


# table of contents:
# 1. load data and prepare data
# 2. analysis - economic time preference without bootstrapping
# 3. save results


################################################################################
# 1. load data and prepare data
################################################################################

# load data
setwd(cd_input_data)
load("epp_tp_data_main.RData")

# create data subsets 
## economic time preference 
d2_boot_econ <- d2 %>%
  filter(!is.na(gov_epp_ln),
         !is.na(gov_climate_ln),
         !is.na(gov_biodiversity_ln),
         !is.na(tp_gps_ln),
         !is.na(gdp_per_ln),
         !is.na(polity2_ln),
         !is.na(latitude_abs_ln))


## thrift time preference 
d2_boot_thrift <- d2 %>%
  filter(!is.na(gov_epp_ln),
         !is.na(gov_climate_ln),
         !is.na(gov_biodiversity_ln),
         !is.na(tp_hofstede_ln),
         !is.na(gdp_per_ln),
         !is.na(polity2_ln),
         !is.na(latitude_abs_ln))


## google time preference 
d2_boot_google <- d2 %>%
  filter(!is.na(gov_epp_ln),
         !is.na(gov_climate_ln),
         !is.na(gov_biodiversity_ln),
         !is.na(tp_google_2_ln),
         !is.na(gdp_per_ln),
         !is.na(polity2_ln),
         !is.na(latitude_abs_ln))


## subset for which all time preferences are available
d2_boot_sub <- d2 %>%
  filter(!is.na(gov_epp_ln),
         !is.na(gov_climate_ln),
         !is.na(gov_biodiversity_ln),
         !is.na(tp_gps_ln),
         !is.na(tp_hofstede_ln),
         !is.na(tp_google_2_ln),
         !is.na(gdp_per_ln),
         !is.na(polity2_ln),
         !is.na(latitude_abs_ln))


################################################################################
# 2. analysis - economic time preference without bootstrapping
################################################################################

##################--------------------------------------------------------------
# 2.1 regression models - entire sample
##################--------------------------------------------------------------

#-------------------------------------------------------------------------------
# 2.1.1  model without control variables
#-------------------------------------------------------------------------------

# environmental policy performance overall
model_1_overall_econ <- lm(gov_epp_ln~ tp_gps_ln, data = d2_boot_econ)
# summary(model_1_overall_econ)

# climate policy performance
model_1_cc_econ <- lm(gov_climate_ln ~ tp_gps_ln, data = d2_boot_econ)
# summary(model_1_cc_econ)

# biodiversity policy performance
model_1_bd_econ <- lm(gov_biodiversity_ln ~ tp_gps_ln, data = d2_boot_econ)
# summary(model_1_bd_econ)

#-------------------------------------------------------------------------------
# 2.1.2  model with control variables (main analysis)
#-------------------------------------------------------------------------------

# environmental policy performance overall
model_2_overall_econ <- lm(gov_epp_ln ~ tp_gps_ln + gdp_per_ln + polity2_ln + latitude_abs_ln,
                            data = d2_boot_econ, na.action = na.exclude)
# summary(model_2_overall_econ)


# climate policy performance
model_2_cc_econ <- lm(gov_climate_ln ~ tp_gps_ln + gdp_per_ln + polity2_ln + latitude_abs_ln,
                           data = d2_boot_econ, na.action = na.exclude)
# summary(model_2_cc_econ)

# biodiversity policy performance
model_2_bd_econ <- lm(gov_biodiversity_ln ~ tp_gps_ln + gdp_per_ln + polity2_ln + latitude_abs_ln,
                           data = d2_boot_econ, na.action = na.exclude)
# summary(model_2_bd_econ)


##################--------------------------------------------------------------
# 2.2 regression models - sub-sample
##################--------------------------------------------------------------

#-------------------------------------------------------------------------------
# 2.2.1  model without control variables
#-------------------------------------------------------------------------------

# environmental policy performance overall
model_1_overall_econ_sub <- lm(gov_epp_ln ~ tp_gps_ln, data = d2_boot_sub)
# summary(model_1_overall_econ_sub)

# climate policy performance
model_1_cc_econ_sub <- lm(gov_climate_ln~ tp_gps_ln, data = d2_boot_sub)
# summary(model_1_cc_econ_sub)

# biodiversity policy performance
model_1_bd_econ_sub <- lm(gov_biodiversity_ln ~ tp_gps_ln, data = d2_boot_sub)
# summary(model_1_bd_econ_sub)

#-------------------------------------------------------------------------------
# 2.2.2  model with control variables
#-------------------------------------------------------------------------------

# environmental policy performance overall
model_2_overall_econ_sub <- lm(gov_epp_ln ~ tp_gps_ln + gdp_per_ln + polity2_ln + latitude_abs_ln,
                                 data = d2_boot_sub, na.action = na.exclude)
summary(model_2_overall_econ_sub)

# climate policy performance
model_2_cc_econ_sub <- lm(gov_climate_ln ~ tp_gps_ln + gdp_per_ln + polity2_ln + latitude_abs_ln,
                                data = d2_boot_sub, na.action = na.exclude)
# summary(model_2_cc_econ_sub)

# biodiversity policy performance
model_2_bd_econ_sub <- lm(gov_biodiversity_ln ~ tp_gps_ln + gdp_per_ln + polity2_ln + latitude_abs_ln,
                                data = d2_boot_sub, na.action = na.exclude)
# summary(model_2_bd_econ_sub)


##################--------------------------------------------------------------
# 2.3 create output table
##################--------------------------------------------------------------

# load function that creates output plots
setwd(cd_code)
source("model_summary_se_function.R")

# clean and combine model output
model_summary_reg_econ <- full_join(
  model_summary_se(model = model_1_overall_econ, with_stars = FALSE) %>% dplyr::rename(model_1_overall_econ_reg = 2),
  model_summary_se(model = model_2_overall_econ, with_stars = FALSE) %>% dplyr::rename(model_2_overall_econ_reg = 2),
  by = "Variable") %>%
  full_join(model_summary_se(model = model_1_cc_econ, with_stars = FALSE) %>% dplyr::rename(model_1_cc_econ_reg = 2),
            by = "Variable") %>%
  full_join(model_summary_se(model = model_2_cc_econ, with_stars = FALSE) %>% dplyr::rename(model_2_cc_econ_reg = 2),
            by = "Variable") %>%
  full_join(model_summary_se(model = model_1_bd_econ, with_stars = FALSE) %>% dplyr::rename(model_1_bd_econ_reg = 2),
            by = "Variable") %>%
  full_join(model_summary_se(model = model_2_bd_econ, with_stars = FALSE) %>% dplyr::rename(model_2_bd_econ_reg = 2),
            by = "Variable")

# clean and combine model output
model_summary_reg_econ_sub <- full_join(
  model_summary_se(model = model_1_overall_econ_sub, with_stars = FALSE) %>% dplyr::rename(model_1_overall_econ_reg_sub = 2),
  model_summary_se(model = model_2_overall_econ_sub, with_stars = FALSE) %>% dplyr::rename(model_2_overall_econ_reg_sub = 2),
  by = "Variable") %>%
  full_join(model_summary_se(model = model_1_cc_econ_sub, with_stars = FALSE) %>% dplyr::rename(model_1_cc_econ_reg_sub = 2),
            by = "Variable") %>%
  full_join(model_summary_se(model = model_2_cc_econ_sub, with_stars = FALSE) %>% dplyr::rename(model_2_cc_econ_reg_sub = 2),
            by = "Variable") %>%
  full_join(model_summary_se(model = model_1_bd_econ_sub, with_stars = FALSE) %>% dplyr::rename(model_1_bd_econ_reg_sub = 2),
            by = "Variable") %>%
  full_join(model_summary_se(model = model_2_bd_econ_sub, with_stars = FALSE) %>% dplyr::rename(model_2_bd_econ_reg_sub = 2),
            by = "Variable")


################################################################################
# 3. save results
################################################################################

setwd(cd_results) # set working directory
# save
## result tables
write.csv(model_summary_reg_econ, file = "table_s5a_model_summary_reg_econ.csv")
write.csv(model_summary_reg_econ_sub, file = "table_s5b_model_summary_reg_econ_sub.csv")

