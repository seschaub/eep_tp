################################################################################
# r code related to                                                            #
# "Global Relationships between Time Preference and Environmental Policy       #
# Performance" by Sergei Schaub published in Environmental Science & Policy    #
# -----------------------------------------------------------------------------#
# 2021, ETH Zurich                                                             #
# developed by Sergei Schaub                                                   #
# -----------------------------------------------------------------------------#
# alternative time preference approximation                                    #
################################################################################


# table of contents:
# 1. load data and prepare data
# 2. analysis 
# 3. save results


################################################################################
# 1. load data and prepare data
################################################################################

# set working directory
setwd(cd_input_data)

# load main data
load("epp_tp_data_main.RData")


# create data from that includes country names and country codes that are used as default
code_info <- countryname_dict %>%
  dplyr::select(country.name.en) %>%
  distinct() %>%
  rename(country_name = 1) %>%
  mutate(iso2c = countrycode(country_name, origin = 'country.name', destination = 'iso2c') ) %>%
  filter(!is.na(iso2c)) %>%
  mutate(iso3c = tolower(countrycode(iso2c, origin = 'iso2c', destination = 'iso3c')),
         wb_code = tolower(countrycode(iso2c, origin = 'iso2c', destination = 'wb')),
         polity_code = tolower(countrycode(iso2c, origin = 'iso2c', destination = 'p4c')),
         iso2c = tolower(iso2c))


# read and prepare world value survey data
read.csv("wvs_2017.csv", stringsAsFactors = F) %>% 
  rename(country_name = 1, wvs_tp = 2) %>% 
  left_join(code_info %>% dplyr::select(country_name, iso3c), by = "country_name") %>% 
  filter(is.na(iso3c))

wvs_1 <- read.csv("wvs_2017.csv", stringsAsFactors = F) %>% 
  rename(country_name = 1, wvs_tp = 2) %>% 
  mutate(country_name  = ifelse(country_name == "Bosnia Herzegovina", "Bosnia & Herzegovina",
                                ifelse(country_name == "Myanmar", "Myanmar (Burma)",
                                       ifelse(country_name == "Taiwan ROC", "Taiwan",
                                              ifelse(country_name == "Czech Rep.", "Czechia",
                                                     ifelse(country_name == "Hong Kong SAR", "Hong Kong SAR China",
                                                            ifelse(country_name == "Macau SAR", "Macao SAR China",country_name))))))) %>% 
  left_join(code_info %>% dplyr::select(country_name, iso3c), by = "country_name") %>% 
  mutate(wvs_tp_ln = log(wvs_tp))
                                

# read and prepare GLOBE data
read.csv("GLOBE-Phase-2-Aggregated-Societal-Culture-Data.csv", stringsAsFactors = F) %>%
  dplyr::select(Country.Name, Future.Orientation.Societal.Values) %>% 
  rename(country_name = 1, globe_tp = 2) %>% 
  left_join(code_info %>% dplyr::select(country_name, iso3c), by = "country_name") %>% 
  filter(is.na(iso3c))


globe_1 <- read.csv("GLOBE-Phase-2-Aggregated-Societal-Culture-Data.csv", stringsAsFactors = F) %>%
  dplyr::select(Country.Name, Future.Orientation.Societal.Practices, Future.Orientation.Societal.Values) %>% 
  rename(country_name = 1, globe_tp_1 = 2, globe_tp_2 = 3) %>% 
  mutate(region_x = ifelse(country_name == "South Africa (Black Sample)", "South Africa",
                           ifelse(country_name == "South Africa (White Sample)", "South Africa",
                                  ifelse(country_name == "Germany (EAST)", "Germany",
                                         ifelse(country_name == "Germany (WEST)", "Germany", 
                                                ifelse(country_name == "French Switzerland", "Switzerland",
                                                       country_name)))))) %>% 
  group_by(region_x) %>% mutate(globe_tp_1x = mean(globe_tp_1), globe_tp_2x = mean(globe_tp_2)) %>% 
  dplyr::select(region_x, globe_tp_1x, globe_tp_2x) %>% distinct() %>% 
  rename(country_name = 1, globe_tp_1 = 2, globe_tp_2 = 3) %>% 
  mutate(country_name  = ifelse(country_name == "Czech Republic", "Czechia",
                                ifelse(country_name == "Hong Kong", "Hong Kong SAR China",
                                       ifelse(country_name == "IRAN", "Iran",
                                              ifelse(country_name == "Canada (English-speaking)", "Canada",
                                                     ifelse(country_name == "England", "United Kingdom",
                                                            ifelse(country_name == "USA", "United States",
                                                                   ifelse(country_name == "England", "United Kingdom",country_name)))))))) %>% 
  left_join(code_info %>% dplyr::select(country_name, iso3c), by = "country_name") %>% 
  mutate(globe_tp_ln = log(globe_tp_1 + globe_tp_2),
         globe_tp_ln_1 = log(globe_tp_1 ),
         globe_tp_ln_2 = log(globe_tp_2)) %>% ungroup()


# join datasets
d2 <- d2 %>% left_join(wvs_1 %>% dplyr::select(-country_name), by = "iso3c") %>% 
  left_join(globe_1 %>% dplyr::select(-country_name), by = "iso3c")



# create dataset to be re-sampled from
## economic time preference
d2_boot_wvs <- d2 %>%
  filter(!is.na(gov_epp_ln),
         !is.na(gov_climate_ln),
         !is.na(gov_biodiversity_ln),
         !is.na(wvs_tp_ln),
         !is.na(gdp_per_ln),
         !is.na(polity2_ln),
         !is.na(latitude_abs_ln))

length(d2_boot_wvs$country_name)


d2_boot_globe <- d2 %>%
  filter(!is.na(gov_epp_ln),
         !is.na(gov_climate_ln),
         !is.na(gov_biodiversity_ln),
         !is.na(globe_tp_ln),
         !is.na(gdp_per_ln),
         !is.na(polity2_ln),
         !is.na(latitude_abs_ln))

length(d2_boot_globe$country_name)


################################################################################
# 2. analysis 
################################################################################

##################--------------------------------------------------------------
# 2.1 bootstrapped regression models - world value survey
##################--------------------------------------------------------------

# bootstrap data
set.seed(123)
data_boot <- bootstraps(d2_boot_wvs %>% as_tibble(), times = 1000, apparent = FALSE)

# for viewing a single bootstrap: View(data_boot[[1]][[1]]$data)

## overall
### run models
boot_model_1 <- data_boot %>%
  mutate(model = purrr::map(splits, ~ lm(gov_epp_ln ~ wvs_tp_ln, data = .)),
         coef_info = purrr::map(model, tidy))

boot_model_2 <- data_boot %>%
  mutate(model = purrr::map(splits, ~ lm(gov_epp_ln ~ wvs_tp_ln + gdp_per_ln +
                                    polity2_ln + latitude_abs_ln, data = .)),
         coef_info = purrr::map(model, tidy))

### get coefficients
boot_coef_1 <- boot_model_1 %>%
  unnest(coef_info)

boot_coef_2 <- boot_model_2 %>%
  unnest(coef_info)

### extract info and summarize
boot_1_over_wvs <- boot_coef_1 %>% dplyr::select(term, estimate) %>%
  group_by(term) %>%
  summarise(mean_estimate = mean(estimate),
            ci_95_low  = quantile(estimate,probs=c(.025)),
            ci_95_high  = quantile(estimate,probs=c(.975)),
            ci_99_low  = quantile(estimate,probs=c(.005)),
            ci_99_high  = quantile(estimate,probs=c(.995))) %>%
  mutate(info = paste0(round(mean_estimate,2), " [",round(ci_95_low,2),
                       " to ",round(ci_95_high,2),"]")) %>%
  dplyr::rename(model_1_overall_econ_boot = 7) %>% mutate(model = "model_1_overall_econ_boot")

boot_2_over_wvs <- boot_coef_2 %>% dplyr::select(term, estimate) %>%
  group_by(term) %>%
  summarise(mean_estimate = mean(estimate),
            ci_95_low  = quantile(estimate,probs=c(.025)),
            ci_95_high  = quantile(estimate,probs=c(.975)),
            ci_99_low  = quantile(estimate,probs=c(.005)),
            ci_99_high  = quantile(estimate,probs=c(.995))) %>%
  mutate(info = paste0(round(mean_estimate,2), " [",round(ci_95_low,2),
                       " to ",round(ci_95_high,2),"]")) %>%
  dplyr::rename(model_2_overall_econ_boot = 7) %>% mutate(model = "model_2_overall_econ_boot")


## climate change
### run models
boot_model_1 <- data_boot %>%
  mutate(model = purrr::map(splits, ~ lm(gov_climate_ln ~ wvs_tp_ln, data = .)),
         coef_info = purrr::map(model, tidy))

boot_model_2 <- data_boot %>%
  mutate(model = purrr::map(splits, ~ lm(gov_climate_ln ~ wvs_tp_ln + gdp_per_ln +
                                           polity2_ln + latitude_abs_ln, data = .)),
         coef_info = purrr::map(model, tidy))

### get coefficients
boot_coef_1 <- boot_model_1 %>%
  unnest(coef_info)

boot_coef_2 <- boot_model_2 %>%
  unnest(coef_info)

### extract info and summarize
boot_1_cc_wvs <- boot_coef_1 %>% dplyr::select(term, estimate) %>%
  group_by(term) %>%
  summarise(mean_estimate = mean(estimate),
            ci_95_low  = quantile(estimate,probs=c(.025)),
            ci_95_high  = quantile(estimate,probs=c(.975)),
            ci_99_low  = quantile(estimate,probs=c(.005)),
            ci_99_high  = quantile(estimate,probs=c(.995))) %>%
  mutate(info = paste0(round(mean_estimate,2), " [",round(ci_95_low,2),
                       " to ",round(ci_95_high,2),"]")) %>%
  dplyr::rename(model_1_overall_econ_boot = 7) %>% mutate(model = "model_1_cc_econ_boot")

boot_2_cc_wvs <- boot_coef_2 %>% dplyr::select(term, estimate) %>%
  group_by(term) %>%
  summarise(mean_estimate = mean(estimate),
            ci_95_low  = quantile(estimate,probs=c(.025)),
            ci_95_high  = quantile(estimate,probs=c(.975)),
            ci_99_low  = quantile(estimate,probs=c(.005)),
            ci_99_high  = quantile(estimate,probs=c(.995))) %>%
  mutate(info = paste0(round(mean_estimate,2), " [",round(ci_95_low,2),
                       " to ",round(ci_95_high,2),"]")) %>%
  dplyr::rename(model_2_overall_econ_boot = 7) %>% mutate(model = "model_2_cc_econ_boot")


## biodiversity
### run models
boot_model_1 <- data_boot %>%
  mutate(model = purrr::map(splits, ~ lm(gov_biodiversity_ln ~ wvs_tp_ln, data = .)),
         coef_info = purrr::map(model, tidy))

boot_model_2 <- data_boot %>%
  mutate(model = purrr::map(splits, ~ lm(gov_biodiversity_ln ~ wvs_tp_ln + gdp_per_ln +
                                           polity2_ln + latitude_abs_ln, data = .)),
         coef_info = purrr::map(model, tidy))

### get coefficients
boot_coef_1 <- boot_model_1 %>%
  unnest(coef_info)

boot_coef_2 <- boot_model_2 %>%
  unnest(coef_info)

### extract info and summarize
boot_1_bd_wvs <- boot_coef_1 %>% dplyr::select(term, estimate) %>%
  group_by(term) %>%
  summarise(mean_estimate = mean(estimate),
            ci_95_low  = quantile(estimate,probs=c(.025)),
            ci_95_high  = quantile(estimate,probs=c(.975)),
            ci_99_low  = quantile(estimate,probs=c(.005)),
            ci_99_high  = quantile(estimate,probs=c(.995))) %>%
  mutate(info = paste0(round(mean_estimate,2), " [",round(ci_95_low,2),
                       " to ",round(ci_95_high,2),"]")) %>%
  dplyr::rename(model_1_overall_econ_boot = 7) %>% mutate(model = "model_1_cc_econ_boot")

boot_2_bd_wvs <- boot_coef_2 %>% dplyr::select(term, estimate) %>%
  group_by(term) %>%
  summarise(mean_estimate = mean(estimate),
            ci_95_low  = quantile(estimate,probs=c(.025)),
            ci_95_high  = quantile(estimate,probs=c(.975)),
            ci_99_low  = quantile(estimate,probs=c(.005)),
            ci_99_high  = quantile(estimate,probs=c(.995))) %>%
  mutate(info = paste0(round(mean_estimate,2), " [",round(ci_95_low,2),
                       " to ",round(ci_95_high,2),"]")) %>%
  dplyr::rename(model_2_overall_econ_boot = 7) %>% mutate(model = "model_2_cc_econ_boot")



## add outputs together for table
model_summary_boot_wvs <-
  full_join(boot_1_over_wvs %>% dplyr::select(term, contains("_boot")),
            boot_2_over_wvs %>% dplyr::select(term, contains("_boot")),
            by = "term") %>%
  full_join(boot_1_cc_wvs %>% dplyr::select(term, contains("_boot")),
            by = "term") %>%
  full_join(boot_2_cc_wvs %>% dplyr::select(term, contains("_boot")),
            by = "term") %>%
  full_join(boot_1_bd_wvs %>% dplyr::select(term, contains("_boot")),
            by = "term") %>%
  full_join(boot_2_bd_wvs %>% dplyr::select(term, contains("_boot")),
            by = "term")


##################--------------------------------------------------------------
# 3.2 bootstrapped regression models - GLOAB project - social practices 
##################--------------------------------------------------------------


# bootstrap data
set.seed(123)
data_boot <- bootstraps(d2_boot_globe %>% as_tibble(), times = 1000, apparent = FALSE)

# for viwing a single bootstrap: View(data_boot[[1]][[1]]$data)

## overall
### run models
boot_model_1 <- data_boot %>%
  mutate(model = purrr::map(splits, ~ lm(gov_epp_ln ~ globe_tp_ln_1, data = .)),
         coef_info = purrr::map(model, tidy))

boot_model_2 <- data_boot %>%
  mutate(model = purrr::map(splits, ~ lm(gov_epp_ln ~ globe_tp_ln_1 + gdp_per_ln +
                                           polity2_ln + latitude_abs_ln, data = .)),
         coef_info = purrr::map(model, tidy))

### get coefficients
boot_coef_1 <- boot_model_1 %>%
  unnest(coef_info)

boot_coef_2 <- boot_model_2 %>%
  unnest(coef_info)

### extract info and summarize
boot_1_over_globe <- boot_coef_1 %>% dplyr::select(term, estimate) %>%
  group_by(term) %>%
  summarise(mean_estimate = mean(estimate),
            ci_95_low  = quantile(estimate,probs=c(.025)),
            ci_95_high  = quantile(estimate,probs=c(.975)),
            ci_99_low  = quantile(estimate,probs=c(.005)),
            ci_99_high  = quantile(estimate,probs=c(.995))) %>%
  mutate(info = paste0(round(mean_estimate,2), " [",round(ci_95_low,2),
                       " to ",round(ci_95_high,2),"]")) %>%
  dplyr::rename(model_1_overall_econ_boot = 7) %>% mutate(model = "model_1_overall_econ_boot")

boot_2_over_globe <- boot_coef_2 %>% dplyr::select(term, estimate) %>%
  group_by(term) %>%
  summarise(mean_estimate = mean(estimate),
            ci_95_low  = quantile(estimate,probs=c(.025)),
            ci_95_high  = quantile(estimate,probs=c(.975)),
            ci_99_low  = quantile(estimate,probs=c(.005)),
            ci_99_high  = quantile(estimate,probs=c(.995))) %>%
  mutate(info = paste0(round(mean_estimate,2), " [",round(ci_95_low,2),
                       " to ",round(ci_95_high,2),"]")) %>%
  dplyr::rename(model_2_overall_econ_boot = 7) %>% mutate(model = "model_2_overall_econ_boot")


## climate change
### run models
boot_model_1 <- data_boot %>%
  mutate(model = purrr::map(splits, ~ lm(gov_climate_ln ~ globe_tp_ln_1, data = .)),
         coef_info = purrr::map(model, tidy))

boot_model_2 <- data_boot %>%
  mutate(model = purrr::map(splits, ~ lm(gov_climate_ln ~ globe_tp_ln_1 + gdp_per_ln +
                                           polity2_ln + latitude_abs_ln, data = .)),
         coef_info = purrr::map(model, tidy))

### get coefficients
boot_coef_1 <- boot_model_1 %>%
  unnest(coef_info)

boot_coef_2 <- boot_model_2 %>%
  unnest(coef_info)

### extract info and summarize
boot_1_cc_globe <- boot_coef_1 %>% dplyr::select(term, estimate) %>%
  group_by(term) %>%
  summarise(mean_estimate = mean(estimate),
            ci_95_low  = quantile(estimate,probs=c(.025)),
            ci_95_high  = quantile(estimate,probs=c(.975)),
            ci_99_low  = quantile(estimate,probs=c(.005)),
            ci_99_high  = quantile(estimate,probs=c(.995))) %>%
  mutate(info = paste0(round(mean_estimate,2), " [",round(ci_95_low,2),
                       " to ",round(ci_95_high,2),"]")) %>%
  dplyr::rename(model_1_overall_econ_boot = 7) %>% mutate(model = "model_1_overall_econ_boot")

boot_2_cc_globe <- boot_coef_2 %>% dplyr::select(term, estimate) %>%
  group_by(term) %>%
  summarise(mean_estimate = mean(estimate),
            ci_95_low  = quantile(estimate,probs=c(.025)),
            ci_95_high  = quantile(estimate,probs=c(.975)),
            ci_99_low  = quantile(estimate,probs=c(.005)),
            ci_99_high  = quantile(estimate,probs=c(.995))) %>%
  mutate(info = paste0(round(mean_estimate,2), " [",round(ci_95_low,2),
                       " to ",round(ci_95_high,2),"]")) %>%
  dplyr::rename(model_2_overall_econ_boot = 7) %>% mutate(model = "model_2_overall_econ_boot")



## biodiversity
### run models
boot_model_1 <- data_boot %>%
  mutate(model = purrr::map(splits, ~ lm(gov_biodiversity_ln ~ globe_tp_ln_1, data = .)),
         coef_info = purrr::map(model, tidy))

boot_model_2 <- data_boot %>%
  mutate(model = purrr::map(splits, ~ lm(gov_biodiversity_ln ~ globe_tp_ln_1 + gdp_per_ln +
                                           polity2_ln + latitude_abs_ln, data = .)),
         coef_info = purrr::map(model, tidy))

### get coefficients
boot_coef_1 <- boot_model_1 %>%
  unnest(coef_info)

boot_coef_2 <- boot_model_2 %>%
  unnest(coef_info)

### extract info and summarize
boot_1_bd_globe <- boot_coef_1 %>% dplyr::select(term, estimate) %>%
  group_by(term) %>%
  summarise(mean_estimate = mean(estimate),
            ci_95_low  = quantile(estimate,probs=c(.025)),
            ci_95_high  = quantile(estimate,probs=c(.975)),
            ci_99_low  = quantile(estimate,probs=c(.005)),
            ci_99_high  = quantile(estimate,probs=c(.995))) %>%
  mutate(info = paste0(round(mean_estimate,2), " [",round(ci_95_low,2),
                       " to ",round(ci_95_high,2),"]")) %>%
  dplyr::rename(model_1_overall_econ_boot = 7) %>% mutate(model = "model_1_overall_econ_boot")

boot_2_bd_globe <- boot_coef_2 %>% dplyr::select(term, estimate) %>%
  group_by(term) %>%
  summarise(mean_estimate = mean(estimate),
            ci_95_low  = quantile(estimate,probs=c(.025)),
            ci_95_high  = quantile(estimate,probs=c(.975)),
            ci_99_low  = quantile(estimate,probs=c(.005)),
            ci_99_high  = quantile(estimate,probs=c(.995))) %>%
  mutate(info = paste0(round(mean_estimate,2), " [",round(ci_95_low,2),
                       " to ",round(ci_95_high,2),"]")) %>%
  dplyr::rename(model_2_overall_econ_boot = 7) %>% mutate(model = "model_2_overall_econ_boot")



## add outputs together for table
model_summary_boot_globe <-
  full_join(boot_1_over_globe %>% dplyr::select(term, contains("_boot")),
            boot_2_over_globe %>% dplyr::select(term, contains("_boot")),
            by = "term") %>%
  full_join(boot_1_cc_globe %>% dplyr::select(term, contains("_boot")),
            by = "term") %>%
  full_join(boot_2_cc_globe %>% dplyr::select(term, contains("_boot")),
            by = "term") %>%
  full_join(boot_1_bd_globe %>% dplyr::select(term, contains("_boot")),
            by = "term") %>%
  full_join(boot_2_bd_globe %>% dplyr::select(term, contains("_boot")),
            by = "term")


cor.test(d2$tp_gps_ln, d2$globe_tp_ln_1)
cor.test(d2$tp_gps_ln, d2$wvs_tp_ln)

################################################################################
# 3. save results
################################################################################
setwd(cd_results) # set working directory

# tables
write.csv(model_summary_boot_wvs %>%
            mutate(term = factor(term, levels=c("(Intercept)","wvs_tp_ln", "gdp_per_ln","polity2_ln" ,"latitude_abs_ln"))) %>%
            arrange(term, desc()), file = "table_s9a_model_summary_boot_wvs.csv")

write.csv(model_summary_boot_globe %>%
            mutate(term = factor(term, levels=c("(Intercept)","globe_tp_ln_1", "gdp_per_ln","polity2_ln" ,"latitude_abs_ln"))) %>%
            arrange(term, desc()), file = "table_s9b_model_summary_boot_globe.csv")

