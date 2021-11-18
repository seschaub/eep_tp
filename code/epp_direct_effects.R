################################################################################
# r code related to                                                            #
# "Global Relationships between Time Preference and Environmental Policy       #
# Performance" by Sergei Schaub published in Environmental Science & Policy    #
# -----------------------------------------------------------------------------#
# 2021, ETH Zurich                                                             #
# developed by Sergei Schaub                                                   #
# -----------------------------------------------------------------------------#
# direct effects                                                               #
################################################################################


# table of contents:
# 1. load data and prepare data
# 2. assessment of direct relationships using g-estimation using the base model
# 3. assessment of direct relationships using g-estimation using the extended model
# 4. save figures


################################################################################
# 1. load data and prepare data
################################################################################

# load data
setwd(cd_input_data)
load("epp_tp_data_main.RData")

# create dataset for resampling
## economic time preference
d2_boot_econ <- d2 %>%
  filter(!is.na(gov_epp_ln),
         !is.na(gov_climate_ln),
         !is.na(gov_biodiversity_ln),
         !is.na(tp_gps_ln),
         !is.na(gdp_per_ln),
         !is.na(polity2_ln),
         !is.na(latitude_abs_ln))
length(d2_boot_econ$country_name) # number of observations

## thrift time preference
d2_boot_thrift <- d2 %>%
  filter(!is.na(gov_epp_ln),
         !is.na(gov_climate_ln),
         !is.na(gov_biodiversity_ln),
         !is.na(tp_hofstede_ln),
         !is.na(gdp_per_ln),
         !is.na(polity2_ln),
         !is.na(latitude_abs_ln))
length(d2_boot_thrift$country_name) # number of observations

## google time preference
d2_boot_google <- d2 %>%
  filter(!is.na(gov_epp_ln),
         !is.na(gov_climate_ln),
         !is.na(gov_biodiversity_ln),
         !is.na(tp_google_2_ln),
         !is.na(gdp_per_ln),
         !is.na(polity2_ln),
         !is.na(latitude_abs_ln))
length(d2_boot_google$country_name) # number of observations

## subset for which all indices are available
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
length(d2_boot_sub$country_name) # number of observations


################################################################################
# 2. assessment of direct relationships using g-estimation using the base model
################################################################################

##################--------------------------------------------------------------
# 2.1 economic time preference
##################--------------------------------------------------------------

d2_boot_econ_direct <- d2_boot_econ %>% # first we need to center gdp
  mutate(gdp_per_ln_mean = mean(gdp_per_ln),
         gdp_per_ln_centered = gdp_per_ln -  gdp_per_ln_mean)


# get bootstrapped dataset
set.seed(123)
data_boot <- bootstraps(d2_boot_econ_direct, times = 1000, apparent = FALSE)


#-------------------------------------------------------------------------------
# 2.1.1 environmental policy performance overall
#-------------------------------------------------------------------------------

# run g-estimation
econ_gov1_direct <- sequential_g(gov_epp_ln ~ tp_gps_ln + latitude_abs_ln | polity2_ln | 
                                   gdp_per_ln_centered, data = d2_boot_econ_direct)
summary(econ_gov1_direct)


## set function and data for bootstrapping
lm_est <- function(split, ...) {
  sequential_g(gov_epp_ln ~ tp_gps_ln + latitude_abs_ln | polity2_ln | 
                 gdp_per_ln_centered, data = analysis(split)) %>%
    coef() %>% as.data.frame() %>% 
    tibble::rownames_to_column( "term") %>% dplyr::rename(estimate = 2) %>% as_tibble()}

## run multiple regression
cur_lm_boot <-
  data_boot %>%
  mutate(results = purrr::map(splits, lm_est))

## get coefficients
cur_lm_coef <- cur_lm_boot %>%
  unnest(results) 

## extract info and summarize
boot_1_gov1_econ_direct <- cur_lm_coef %>% dplyr::select(term, estimate) %>%
  group_by(term) %>%
  summarise(mean_estimate = mean(estimate),
            ci_95_low  = quantile(estimate,probs=c(.025)),
            ci_95_high  = quantile(estimate,probs=c(.975)),
            ci_99_low  = quantile(estimate,probs=c(.005)),
            ci_99_high  = quantile(estimate,probs=c(.995))) %>%
  mutate(info = paste0(round(mean_estimate,2), " [",round(ci_95_low,2),
                       " to ",round(ci_95_high,2),"]")) %>%
  dplyr::rename(boot_1_gov1_econ_direct = 7) %>% mutate(model = "boot_1_gov1_econ_direct")

boot_1_gov1_econ_direct



#-------------------------------------------------------------------------------
# 2.1.2 climate policy performance
#-------------------------------------------------------------------------------

# run g-estimation
econ_cc_direct <- sequential_g(gov_climate_ln ~ tp_gps_ln + latitude_abs_ln | polity2_ln| 
                                 gdp_per_ln_centered, data = d2_boot_econ_direct)
summary(econ_cc_direct)


## set function and data for bootstrapping
lm_est <- function(split, ...) {
  sequential_g(gov_climate_ln ~ tp_gps_ln + latitude_abs_ln | polity2_ln | 
                 gdp_per_ln_centered, data = analysis(split)) %>%
    coef() %>% as.data.frame() %>% 
    tibble::rownames_to_column( "term") %>% dplyr::rename(estimate = 2) %>% as_tibble()}

## run multiple regression
cur_lm_boot <-
  data_boot %>%
  mutate(results = purrr::map(splits, lm_est))

## get coefficients
cur_lm_coef <- cur_lm_boot %>%
  unnest(results) 

## extract info and summarize
boot_1_cc_econ_direct <- cur_lm_coef %>% dplyr::select(term, estimate) %>%
  group_by(term) %>%
  summarise(mean_estimate = mean(estimate),
            ci_95_low  = quantile(estimate,probs=c(.025)),
            ci_95_high  = quantile(estimate,probs=c(.975)),
            ci_99_low  = quantile(estimate,probs=c(.005)),
            ci_99_high  = quantile(estimate,probs=c(.995))) %>%
  mutate(info = paste0(round(mean_estimate,2), " [",round(ci_95_low,2),
                       " to ",round(ci_95_high,2),"]")) %>%
  dplyr::rename(boot_1_cc_econ_direct = 7) %>% mutate(model = "boot_1_cc_econ_direct")

boot_1_cc_econ_direct



#-------------------------------------------------------------------------------
# 2.1.3 biodiversity policy performance
#-------------------------------------------------------------------------------

# run g-estimation
econ_bd_direct <- sequential_g(gov_biodiversity_ln ~ tp_gps_ln + latitude_abs_ln | polity2_ln | 
                                 gdp_per_ln_centered, data = d2_boot_econ_direct)
summary(econ_bd_direct)


## set function and data for bootstrapping
lm_est <- function(split, ...) {
  sequential_g(gov_biodiversity_ln ~ tp_gps_ln + latitude_abs_ln | polity2_ln | 
                 gdp_per_ln_centered, data = analysis(split)) %>%
    coef() %>% as.data.frame() %>% 
    tibble::rownames_to_column( "term") %>% dplyr::rename(estimate = 2) %>% as_tibble()}

## run multiple regression
cur_lm_boot <-
  data_boot %>%
  mutate(results = purrr::map(splits, lm_est))

## get coefficients
cur_lm_coef <- cur_lm_boot %>%
  unnest(results) 

## extract info and summarize
boot_1_bd_econ_direct <- cur_lm_coef %>% dplyr::select(term, estimate) %>%
  group_by(term) %>%
  summarise(mean_estimate = mean(estimate),
            ci_95_low  = quantile(estimate,probs=c(.025)),
            ci_95_high  = quantile(estimate,probs=c(.975)),
            ci_99_low  = quantile(estimate,probs=c(.005)),
            ci_99_high  = quantile(estimate,probs=c(.995))) %>%
  mutate(info = paste0(round(mean_estimate,2), " [",round(ci_95_low,2),
                       " to ",round(ci_95_high,2),"]")) %>%
  dplyr::rename(boot_1_bd_econ_direct = 7) %>% mutate(model = "boot_1_bd_econ_direct")

boot_1_bd_econ_direct







##################--------------------------------------------------------------
# 2.2 thrift time preference
##################--------------------------------------------------------------

d2_boot_thrift_direct <- d2_boot_thrift %>% # first we need to center gdp
  mutate(gdp_per_ln_mean = mean(gdp_per_ln),
         gdp_per_ln_centered = gdp_per_ln -  gdp_per_ln_mean)


# get bootstrapped dataset
set.seed(123)
data_boot <- bootstraps(d2_boot_thrift_direct, times = 1000, apparent = FALSE)

#-------------------------------------------------------------------------------
# 2.2.1 environmental policy performance overall
#-------------------------------------------------------------------------------

# run g-estimation
thrift_gov1_direct <- sequential_g(gov_epp_ln ~ tp_hofstede_ln + latitude_abs_ln | polity2_ln | 
                                     gdp_per_ln_centered, data = d2_boot_thrift_direct)
summary(thrift_gov1_direct)


## set function and data for bootstrapping
lm_est <- function(split, ...) {
  sequential_g(gov_epp_ln ~ tp_hofstede_ln + latitude_abs_ln | polity2_ln| 
                 gdp_per_ln_centered, data = analysis(split)) %>%
    coef() %>% as.data.frame() %>% 
    tibble::rownames_to_column( "term") %>% dplyr::rename(estimate = 2) %>% as_tibble()}

## run multiple regression
cur_lm_boot <-
  data_boot %>%
  mutate(results = purrr::map(splits, lm_est))

## get coefficients
cur_lm_coef <- cur_lm_boot %>%
  unnest(results) 

## extract info and summarize
boot_1_gov1_thrift_direct <- cur_lm_coef %>% dplyr::select(term, estimate) %>%
  group_by(term) %>%
  summarise(mean_estimate = mean(estimate),
            ci_95_low  = quantile(estimate,probs=c(.025)),
            ci_95_high  = quantile(estimate,probs=c(.975)),
            ci_99_low  = quantile(estimate,probs=c(.005)),
            ci_99_high  = quantile(estimate,probs=c(.995))) %>%
  mutate(info = paste0(round(mean_estimate,2), " [",round(ci_95_low,2),
                       " to ",round(ci_95_high,2),"]")) %>%
  dplyr::rename(boot_1_gov1_thrift_direct = 7) %>% mutate(model = "boot_1_gov1_thrift_direct")

boot_1_gov1_thrift_direct



#-------------------------------------------------------------------------------
# 2.2.2 climate policy performance
#-------------------------------------------------------------------------------

# run g-estimation
thrift_cc_direct <- sequential_g(gov_climate_ln ~ tp_hofstede_ln + latitude_abs_ln | polity2_ln | 
                                   gdp_per_ln_centered, data = d2_boot_thrift_direct)
summary(thrift_cc_direct)



## set function and data for bootstrapping
lm_est <- function(split, ...) {
  sequential_g(gov_climate_ln ~ tp_hofstede_ln + latitude_abs_ln | polity2_ln | 
                 gdp_per_ln_centered, data = analysis(split)) %>%
    coef() %>% as.data.frame() %>% 
    tibble::rownames_to_column( "term") %>% dplyr::rename(estimate = 2) %>% as_tibble()}

## run multiple regression
cur_lm_boot <-
  data_boot %>%
  mutate(results = purrr::map(splits, lm_est))

## get coefficients
cur_lm_coef <- cur_lm_boot %>%
  unnest(results) 

## extract info and summarize
boot_1_cc_thrift_direct <- cur_lm_coef %>% dplyr::select(term, estimate) %>%
  group_by(term) %>%
  summarise(mean_estimate = mean(estimate),
            ci_95_low  = quantile(estimate,probs=c(.025)),
            ci_95_high  = quantile(estimate,probs=c(.975)),
            ci_99_low  = quantile(estimate,probs=c(.005)),
            ci_99_high  = quantile(estimate,probs=c(.995))) %>%
  mutate(info = paste0(round(mean_estimate,2), " [",round(ci_95_low,2),
                       " to ",round(ci_95_high,2),"]")) %>%
  dplyr::rename(boot_1_cc_thrift_direct = 7) %>% mutate(model = "boot_1_cc_thrift_direct")

boot_1_cc_thrift_direct



#-------------------------------------------------------------------------------
# 2.2.3 biodiversity policy performance
#-------------------------------------------------------------------------------

# run g-estimation
thrift_bd_direct <- sequential_g(gov_biodiversity_ln ~ tp_hofstede_ln + latitude_abs_ln | polity2_ln | 
                                   gdp_per_ln_centered, data = d2_boot_thrift_direct)
summary(thrift_bd_direct)


## set function and data for bootstrapping
lm_est <- function(split, ...) {
  sequential_g(gov_biodiversity_ln ~ tp_hofstede_ln + latitude_abs_ln | polity2_ln | 
                 gdp_per_ln_centered, data = analysis(split)) %>%
    coef() %>% as.data.frame() %>% 
    tibble::rownames_to_column( "term") %>% dplyr::rename(estimate = 2) %>% as_tibble()}

## run multiple regression
cur_lm_boot <-
  data_boot %>%
  mutate(results = purrr::map(splits, lm_est))

## get coefficients
cur_lm_coef <- cur_lm_boot %>%
  unnest(results) 

## extract info and summarize
boot_1_bd_thrift_direct <- cur_lm_coef %>% dplyr::select(term, estimate) %>%
  group_by(term) %>%
  summarise(mean_estimate = mean(estimate),
            ci_95_low  = quantile(estimate,probs=c(.025)),
            ci_95_high  = quantile(estimate,probs=c(.975)),
            ci_99_low  = quantile(estimate,probs=c(.005)),
            ci_99_high  = quantile(estimate,probs=c(.995))) %>%
  mutate(info = paste0(round(mean_estimate,2), " [",round(ci_95_low,2),
                       " to ",round(ci_95_high,2),"]")) %>%
  dplyr::rename(boot_1_bd_thrift_direct = 7) %>% mutate(model = "boot_1_bd_thrift_direct")

boot_1_bd_thrift_direct






##################--------------------------------------------------------------
# 2.3 google time preference
##################--------------------------------------------------------------

d2_boot_google_direct <- d2_boot_google %>% # first we need to center gdp
  mutate(gdp_per_ln_mean = mean(gdp_per_ln),
         gdp_per_ln_centered = gdp_per_ln -  gdp_per_ln_mean)


# get bootstrapped dataset
set.seed(123)
data_boot <- bootstraps(d2_boot_google_direct, times = 1000, apparent = FALSE)

#-------------------------------------------------------------------------------
# 2.3.1 environmental policy performance overall
#-------------------------------------------------------------------------------

# run g-estimation
google_gov1_direct <- sequential_g(gov_epp_ln ~ tp_google_2_ln + latitude_abs_ln | polity2_ln | 
                                     gdp_per_ln_centered, data = d2_boot_google_direct)
summary(google_gov1_direct)


## set function and data for bootstrapping
lm_est <- function(split, ...) {
  sequential_g(gov_epp_ln ~ tp_google_2_ln + latitude_abs_ln | polity2_ln | 
                 gdp_per_ln_centered, data = analysis(split)) %>%
    coef() %>% as.data.frame() %>% 
    tibble::rownames_to_column( "term") %>% dplyr::rename(estimate = 2) %>% as_tibble()}

## run multiple regression
cur_lm_boot <-
  data_boot %>%
  mutate(results = purrr::map(splits, lm_est))

## get coefficients
cur_lm_coef <- cur_lm_boot %>%
  unnest(results) 

## extract info and summarize
boot_1_gov1_google_direct <- cur_lm_coef %>% dplyr::select(term, estimate) %>%
  group_by(term) %>%
  summarise(mean_estimate = mean(estimate),
            ci_95_low  = quantile(estimate,probs=c(.025)),
            ci_95_high  = quantile(estimate,probs=c(.975)),
            ci_99_low  = quantile(estimate,probs=c(.005)),
            ci_99_high  = quantile(estimate,probs=c(.995))) %>%
  mutate(info = paste0(round(mean_estimate,2), " [",round(ci_95_low,2),
                       " to ",round(ci_95_high,2),"]")) %>%
  dplyr::rename(boot_1_gov1_google_direct = 7) %>% mutate(model = "boot_1_gov1_google_direct")

boot_1_gov1_google_direct



#-------------------------------------------------------------------------------
# 2.3.2 climate policy performance
#-------------------------------------------------------------------------------

# run g-estimation
google_cc_direct <- sequential_g(gov_climate_ln ~ tp_google_2_ln + latitude_abs_ln | polity2_ln | 
                                   gdp_per_ln_centered, data = d2_boot_google_direct)
summary(google_cc_direct)



## set function and data for bootstrapping
lm_est <- function(split, ...) {
  sequential_g(gov_climate_ln ~ tp_google_2_ln + latitude_abs_ln | polity2_ln | 
                 gdp_per_ln_centered, data = analysis(split)) %>%
    coef() %>% as.data.frame() %>% 
    tibble::rownames_to_column( "term") %>% dplyr::rename(estimate = 2) %>% as_tibble()}

## run multiple regression
cur_lm_boot <-
  data_boot %>%
  mutate(results = purrr::map(splits, lm_est))

## get coefficients
cur_lm_coef <- cur_lm_boot %>%
  unnest(results) 

## extract info and summarize
boot_1_cc_google_direct <- cur_lm_coef %>% dplyr::select(term, estimate) %>%
  group_by(term) %>%
  summarise(mean_estimate = mean(estimate),
            ci_95_low  = quantile(estimate,probs=c(.025)),
            ci_95_high  = quantile(estimate,probs=c(.975)),
            ci_99_low  = quantile(estimate,probs=c(.005)),
            ci_99_high  = quantile(estimate,probs=c(.995))) %>%
  mutate(info = paste0(round(mean_estimate,2), " [",round(ci_95_low,2),
                       " to ",round(ci_95_high,2),"]")) %>%
  dplyr::rename(boot_1_cc_google_direct = 7) %>% mutate(model = "boot_1_cc_google_direct")

boot_1_cc_google_direct



#-------------------------------------------------------------------------------
# 2.3.3 biodiversity policy performance
#-------------------------------------------------------------------------------

# run g-estimation
google_bd_direct <- sequential_g(gov_biodiversity_ln ~ tp_google_2_ln +latitude_abs_ln | polity2_ln | 
                                   gdp_per_ln_centered, data = d2_boot_google_direct)
summary(google_bd_direct)



## set function and data for bootstrapping
lm_est <- function(split, ...) {
  sequential_g(gov_biodiversity_ln ~ tp_google_2_ln + latitude_abs_ln | polity2_ln | 
                 gdp_per_ln_centered, data = analysis(split)) %>%
    coef() %>% as.data.frame() %>% 
    tibble::rownames_to_column( "term") %>% dplyr::rename(estimate = 2) %>% as_tibble()}

## run multiple regression
cur_lm_boot <-
  data_boot %>%
  mutate(results = purrr::map(splits, lm_est))

## get coefficients
cur_lm_coef <- cur_lm_boot %>%
  unnest(results) 

## extract info and summarize
boot_1_bd_google_direct <- cur_lm_coef %>% dplyr::select(term, estimate) %>%
  group_by(term) %>%
  summarise(mean_estimate = mean(estimate),
            ci_95_low  = quantile(estimate,probs=c(.025)),
            ci_95_high  = quantile(estimate,probs=c(.975)),
            ci_99_low  = quantile(estimate,probs=c(.005)),
            ci_99_high  = quantile(estimate,probs=c(.995))) %>%
  mutate(info = paste0(round(mean_estimate,2), " [",round(ci_95_low,2),
                       " to ",round(ci_95_high,2),"]")) %>%
  dplyr::rename(boot_1_bd_google_direct = 7) %>% mutate(model = "boot_1_bd_google_direct")

boot_1_bd_google_direct



##################--------------------------------------------------------------
# 2.4 tables and plots 
##################--------------------------------------------------------------

## add outputs together for ggplot
model_summary_boot_plot_direct <- bind_rows(boot_1_gov1_econ_direct %>% dplyr::select(term:ci_99_high,model),
                                            boot_1_cc_econ_direct %>% dplyr::select(term:ci_99_high,model),
                                            boot_1_bd_econ_direct %>% dplyr::select(term:ci_99_high,model),
                                            
                                            boot_1_gov1_thrift_direct %>% dplyr::select(term:ci_99_high,model),
                                            boot_1_cc_thrift_direct %>% dplyr::select(term:ci_99_high,model),
                                            boot_1_bd_thrift_direct %>% dplyr::select(term:ci_99_high,model),
                                            
                                            boot_1_gov1_google_direct %>% dplyr::select(term:ci_99_high,model),
                                            boot_1_cc_google_direct %>% dplyr::select(term:ci_99_high,model),
                                            boot_1_bd_google_direct %>% dplyr::select(term:ci_99_high,model))




theme_set(theme_bw()) # set main design
all_boot_plot_direct_base <- model_summary_boot_plot_direct %>%
  filter(str_detect(term, "tp_")) %>%
  mutate(family_model =
           ifelse(str_detect(model, "gov1"), "A) Environmental Policy\nPerformance Overall",
                  ifelse(str_detect(model, "cc"), "B) Climate Policy\nPerformance","C) Biodiversity Policy\nPerformance")),
         family_model = factor(family_model, levels=c("A) Environmental Policy\nPerformance Overall",
                                                      "B) Climate Policy\nPerformance",
                                                      "C) Biodiversity Policy\nPerformance")),
         tp_type =
           ifelse(str_detect(term, "tp_gps"), "Economic Long-Term\nOrientation (log)",
                  ifelse(str_detect(term, "tp_hofstede"), "Thrift Long-Term\nOrientation (log)","Google Long-Term\nOrientation (log)")),
         tp_type = factor(tp_type, levels=c("Google Long-Term\nOrientation (log)", "Thrift Long-Term\nOrientation (log)", "Economic Long-Term\nOrientation (log)"))
  ) %>%
  ggplot(aes(y=tp_type  , x=mean_estimate)) +
  geom_vline(xintercept = 0, color=c("#525252")) +
  geom_errorbar (aes(y=tp_type , xmin = ci_99_low, xmax = ci_99_high, colour = "99% Confidence Interval"), size = 2,width=0, position=position_dodge(width=0.5))+
  geom_errorbar (aes(y=tp_type , xmin = ci_95_low, xmax = ci_95_high, colour = "95% Confidence Interval"), size = 2,width=0, position=position_dodge(width=0.5))+
  # geom_errorbar (data = sum_table, aes(x=reorder(Model, Estimate),ymin = Conf_Low_90, ymax = Conf_High_90, colour = "90% Confidence Interval"), size = 2,width=0, position=position_dodge(width=0.9))+
  geom_point(aes(shape = "Point Esimate", colour = "Point Esimate"), shape = 21, fill = "white",
             size = 3.5, position=position_dodge(width=0.5))+
  facet_grid(~family_model, scales = "fixed") +
  theme(legend.position = "none",
        legend.justification = c(0, 1),
        legend.title=element_blank(),
        #legend.text=element_text(size=3.5),
        legend.direction = "horizontal",
        legend.margin = margin(0, 0, 0, 0),
        #legend.justification = "left",
        legend.spacing.y = unit(-0, "cm"),
        legend.spacing.x = unit(2, "pt"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        axis.title.y=element_blank(),
        strip.text.x = element_text(angle = 0, hjust = 0))  +
  scale_color_manual(values = c("#1f78b4","#a6cee3","#1f78b4"))+
  scale_x_continuous(name = "") 

all_boot_plot_direct_base




################################################################################
# 3. assessment of direct relationships using g-estimation using the extended model
################################################################################


##################--------------------------------------------------------------
# 3.1 economic time preference
##################--------------------------------------------------------------

d2_boot_econ_direct <- d2_boot_econ %>% # first we need to center gdp
  mutate(gdp_per_ln_mean = mean(gdp_per_ln),
         gdp_per_ln_centered = gdp_per_ln -  gdp_per_ln_mean)


# get bootstrapped dataset
set.seed(123)
data_boot <- bootstraps(d2_boot_econ_direct, times = 1000, apparent = FALSE)

#-------------------------------------------------------------------------------
# 3.1.1 environmental policy performance
#-------------------------------------------------------------------------------

# run g-estimation
econ_gov1_direct <- sequential_g(gov_epp_ln ~ tp_gps_ln + latitude_abs_ln | polity2_ln + education_ln +
                                   internet_share_ln + trade_ln + air_ln + agri_share_ln + population_ln + factor(soviet_hist)| 
                                   gdp_per_ln_centered, data = d2_boot_econ_direct)
summary(econ_gov1_direct)

## set function and data for bootstrapping
lm_est <- function(split, ...) {
  sequential_g(gov_epp_ln ~ tp_gps_ln + latitude_abs_ln | polity2_ln + education_ln +
                 internet_share_ln + trade_ln + air_ln + agri_share_ln + population_ln + factor(soviet_hist)| 
                 gdp_per_ln_centered, data = analysis(split)) %>%
    coef() %>% as.data.frame() %>% 
    tibble::rownames_to_column( "term") %>% dplyr::rename(estimate = 2) %>% as_tibble()}

## run multiple regression
cur_lm_boot <-
  data_boot %>%
  mutate(results = purrr::map(splits, lm_est))

## get coefficients
cur_lm_coef <- cur_lm_boot %>%
  unnest(results) 

## extract info and summarize
boot_1_gov1_econ_direct <- cur_lm_coef %>% dplyr::select(term, estimate) %>%
  group_by(term) %>%
  summarise(mean_estimate = mean(estimate),
            ci_95_low  = quantile(estimate,probs=c(.025)),
            ci_95_high  = quantile(estimate,probs=c(.975)),
            ci_99_low  = quantile(estimate,probs=c(.005)),
            ci_99_high  = quantile(estimate,probs=c(.995))) %>%
  mutate(info = paste0(round(mean_estimate,2), " [",round(ci_95_low,2),
                       " to ",round(ci_95_high,2),"]")) %>%
  dplyr::rename(boot_1_gov1_econ_direct = 7) %>% mutate(model = "boot_1_gov1_econ_direct")

boot_1_gov1_econ_direct



#-------------------------------------------------------------------------------
# 3.1.2 climate policy performance 
#-------------------------------------------------------------------------------

# run g-estimation
econ_cc_direct <- sequential_g(gov_climate_ln ~ tp_gps_ln + latitude_abs_ln | polity2_ln + education_ln +
                                 internet_share_ln + trade_ln + air_ln + agri_share_ln + population_ln + factor(soviet_hist)| 
                                 gdp_per_ln_centered, data = d2_boot_econ_direct)
summary(econ_cc_direct)


## set function and data for bootstrapping
lm_est <- function(split, ...) {
  sequential_g(gov_climate_ln ~ tp_gps_ln + latitude_abs_ln | polity2_ln + education_ln +
                 internet_share_ln + trade_ln + air_ln + agri_share_ln + population_ln + factor(soviet_hist)| 
                 gdp_per_ln_centered, data = analysis(split)) %>%
    coef() %>% as.data.frame() %>% 
    tibble::rownames_to_column( "term") %>% dplyr::rename(estimate = 2) %>% as_tibble()}

## run multiple regression
cur_lm_boot <-
  data_boot %>%
  mutate(results = purrr::map(splits, lm_est))

## get coefficients
cur_lm_coef <- cur_lm_boot %>%
  unnest(results) 

## extract info and summarize
boot_1_cc_econ_direct <- cur_lm_coef %>% dplyr::select(term, estimate) %>%
  group_by(term) %>%
  summarise(mean_estimate = mean(estimate),
            ci_95_low  = quantile(estimate,probs=c(.025)),
            ci_95_high  = quantile(estimate,probs=c(.975)),
            ci_99_low  = quantile(estimate,probs=c(.005)),
            ci_99_high  = quantile(estimate,probs=c(.995))) %>%
  mutate(info = paste0(round(mean_estimate,2), " [",round(ci_95_low,2),
                       " to ",round(ci_95_high,2),"]")) %>%
  dplyr::rename(boot_1_cc_econ_direct = 7) %>% mutate(model = "boot_1_cc_econ_direct")

boot_1_cc_econ_direct



#-------------------------------------------------------------------------------
# 3.1.3 biodiversity policy performance 
#-------------------------------------------------------------------------------

# run g-estimation
econ_bd_direct <- sequential_g(gov_biodiversity_ln ~ tp_gps_ln + latitude_abs_ln | polity2_ln + education_ln +
                                 internet_share_ln + trade_ln + air_ln + agri_share_ln + population_ln + factor(soviet_hist)| 
                                 gdp_per_ln_centered, data = d2_boot_econ_direct)
summary(econ_bd_direct)



## set function and data for bootstrapping
lm_est <- function(split, ...) {
  sequential_g(gov_biodiversity_ln ~ tp_gps_ln + latitude_abs_ln | polity2_ln + education_ln +
                 internet_share_ln + trade_ln + air_ln + agri_share_ln + population_ln + factor(soviet_hist)| 
                 gdp_per_ln_centered, data = analysis(split)) %>%
    coef() %>% as.data.frame() %>% 
    tibble::rownames_to_column( "term") %>% dplyr::rename(estimate = 2) %>% as_tibble()}

## run multiple regression
cur_lm_boot <-
  data_boot %>%
  mutate(results = purrr::map(splits, lm_est))

## get coefficients
cur_lm_coef <- cur_lm_boot %>%
  unnest(results) 

## extract info and summarize
boot_1_bd_econ_direct <- cur_lm_coef %>% dplyr::select(term, estimate) %>%
  group_by(term) %>%
  summarise(mean_estimate = mean(estimate),
            ci_95_low  = quantile(estimate,probs=c(.025)),
            ci_95_high  = quantile(estimate,probs=c(.975)),
            ci_99_low  = quantile(estimate,probs=c(.005)),
            ci_99_high  = quantile(estimate,probs=c(.995))) %>%
  mutate(info = paste0(round(mean_estimate,2), " [",round(ci_95_low,2),
                       " to ",round(ci_95_high,2),"]")) %>%
  dplyr::rename(boot_1_bd_econ_direct = 7) %>% mutate(model = "boot_1_bd_econ_direct")

boot_1_bd_econ_direct




##################--------------------------------------------------------------
# 3.2 thrift time preference
##################--------------------------------------------------------------

d2_boot_thrift_direct <- d2_boot_thrift %>% # first we need to center gdp
  mutate(gdp_per_ln_mean = mean(gdp_per_ln),
         gdp_per_ln_centered = gdp_per_ln -  gdp_per_ln_mean)


# get bootstrapped dataset
set.seed(123)
data_boot <- bootstraps(d2_boot_thrift_direct, times = 1000, apparent = FALSE)

#-------------------------------------------------------------------------------
# 3.2.1 environmental policy performance
#-------------------------------------------------------------------------------

# run g-estimation
thrift_gov1_direct <- sequential_g(gov_epp_ln ~ tp_hofstede_ln + latitude_abs_ln | polity2_ln + education_ln +
                                     internet_share_ln + trade_ln + air_ln + agri_share_ln + population_ln + factor(soviet_hist)| 
                                     gdp_per_ln_centered, data = d2_boot_thrift_direct)
summary(thrift_gov1_direct)


## set function and data for bootstrapping
lm_est <- function(split, ...) {
  sequential_g(gov_epp_ln ~ tp_hofstede_ln + latitude_abs_ln | polity2_ln + education_ln +
                 internet_share_ln + trade_ln + air_ln + agri_share_ln + population_ln + factor(soviet_hist)| 
                 gdp_per_ln_centered, data = analysis(split)) %>%
    coef() %>% as.data.frame() %>% 
    tibble::rownames_to_column( "term") %>% dplyr::rename(estimate = 2) %>% as_tibble()}

## run multiple regression
cur_lm_boot <-
  data_boot %>%
  mutate(results = purrr::map(splits, lm_est))

## get coefficients
cur_lm_coef <- cur_lm_boot %>%
  unnest(results) 

## extract info and summarize
boot_1_gov1_thrift_direct <- cur_lm_coef %>% dplyr::select(term, estimate) %>%
  group_by(term) %>%
  summarise(mean_estimate = mean(estimate),
            ci_95_low  = quantile(estimate,probs=c(.025)),
            ci_95_high  = quantile(estimate,probs=c(.975)),
            ci_99_low  = quantile(estimate,probs=c(.005)),
            ci_99_high  = quantile(estimate,probs=c(.995))) %>%
  mutate(info = paste0(round(mean_estimate,2), " [",round(ci_95_low,2),
                       " to ",round(ci_95_high,2),"]")) %>%
  dplyr::rename(boot_1_gov1_thrift_direct = 7) %>% mutate(model = "boot_1_gov1_thrift_direct")

boot_1_gov1_thrift_direct



#-------------------------------------------------------------------------------
# 3.2.2 climate policy performance 
#-------------------------------------------------------------------------------

# run g-estimation
thrift_cc_direct <- sequential_g(gov_climate_ln ~ tp_hofstede_ln + latitude_abs_ln | polity2_ln + education_ln +
                                   internet_share_ln + trade_ln + air_ln + agri_share_ln + population_ln + factor(soviet_hist)| 
                                   gdp_per_ln_centered, data = d2_boot_thrift_direct)
summary(thrift_cc_direct)


## set function and data for bootstrapping
lm_est <- function(split, ...) {
  sequential_g(gov_climate_ln ~ tp_hofstede_ln + latitude_abs_ln | polity2_ln + education_ln +
                 internet_share_ln + trade_ln + air_ln + agri_share_ln + population_ln + factor(soviet_hist)| 
                 gdp_per_ln_centered, data = analysis(split)) %>%
    coef() %>% as.data.frame() %>% 
    tibble::rownames_to_column( "term") %>% dplyr::rename(estimate = 2) %>% as_tibble()}

## run multiple regression
cur_lm_boot <-
  data_boot %>%
  mutate(results = purrr::map(splits, lm_est))

## get coefficients
cur_lm_coef <- cur_lm_boot %>%
  unnest(results) 

## extract info and summarize
boot_1_cc_thrift_direct <- cur_lm_coef %>% dplyr::select(term, estimate) %>%
  group_by(term) %>%
  summarise(mean_estimate = mean(estimate),
            ci_95_low  = quantile(estimate,probs=c(.025)),
            ci_95_high  = quantile(estimate,probs=c(.975)),
            ci_99_low  = quantile(estimate,probs=c(.005)),
            ci_99_high  = quantile(estimate,probs=c(.995))) %>%
  mutate(info = paste0(round(mean_estimate,2), " [",round(ci_95_low,2),
                       " to ",round(ci_95_high,2),"]")) %>%
  dplyr::rename(boot_1_cc_thrift_direct = 7) %>% mutate(model = "boot_1_cc_thrift_direct")

boot_1_cc_thrift_direct



#-------------------------------------------------------------------------------
# 3.2.3 biodiversity policy performance 
#-------------------------------------------------------------------------------

# run g-estimation
thrift_bd_direct <- sequential_g(gov_biodiversity_ln ~ tp_hofstede_ln + latitude_abs_ln | polity2_ln + education_ln +
                                   internet_share_ln + trade_ln + air_ln + agri_share_ln + population_ln + factor(soviet_hist)| 
                                   gdp_per_ln_centered, data = d2_boot_thrift_direct)
summary(thrift_bd_direct)


## set function and data for bootstrapping
lm_est <- function(split, ...) {
  sequential_g(gov_biodiversity_ln ~ tp_hofstede_ln + latitude_abs_ln | polity2_ln + education_ln +
                 internet_share_ln + trade_ln + air_ln + agri_share_ln + population_ln + factor(soviet_hist)| 
                 gdp_per_ln_centered, data = analysis(split)) %>%
    coef() %>% as.data.frame() %>% 
    tibble::rownames_to_column( "term") %>% dplyr::rename(estimate = 2) %>% as_tibble()}

## run multiple regression
cur_lm_boot <-
  data_boot %>%
  mutate(results = purrr::map(splits, lm_est))

## get coefficients
cur_lm_coef <- cur_lm_boot %>%
  unnest(results) 

## extract info and summarize
boot_1_bd_thrift_direct <- cur_lm_coef %>% dplyr::select(term, estimate) %>%
  group_by(term) %>%
  summarise(mean_estimate = mean(estimate),
            ci_95_low  = quantile(estimate,probs=c(.025)),
            ci_95_high  = quantile(estimate,probs=c(.975)),
            ci_99_low  = quantile(estimate,probs=c(.005)),
            ci_99_high  = quantile(estimate,probs=c(.995))) %>%
  mutate(info = paste0(round(mean_estimate,2), " [",round(ci_95_low,2),
                       " to ",round(ci_95_high,2),"]")) %>%
  dplyr::rename(boot_1_bd_thrift_direct = 7) %>% mutate(model = "boot_1_bd_thrift_direct")

boot_1_bd_thrift_direct






##################--------------------------------------------------------------
# 3.3 google time preference
##################--------------------------------------------------------------

d2_boot_google_direct <- d2_boot_google %>% # first we need to center gdp
  mutate(gdp_per_ln_mean = mean(gdp_per_ln),
         gdp_per_ln_centered = gdp_per_ln -  gdp_per_ln_mean)


# get bootstrapped dataset
set.seed(123)
data_boot <- bootstraps(d2_boot_google_direct, times = 1000, apparent = FALSE)

#-------------------------------------------------------------------------------
# 3.3.1 environmental policy performance
#-------------------------------------------------------------------------------

# run g-estimation
google_gov1_direct <- sequential_g(gov_epp_ln ~ tp_google_2_ln + latitude_abs_ln | polity2_ln + education_ln +
                                     internet_share_ln + trade_ln + air_ln + agri_share_ln + population_ln + factor(soviet_hist)| 
                                     gdp_per_ln_centered, data = d2_boot_google_direct)
summary(google_gov1_direct)


## set function and data for bootstrapping
lm_est <- function(split, ...) {
  sequential_g(gov_epp_ln ~ tp_google_2_ln + latitude_abs_ln | polity2_ln + education_ln +
                 internet_share_ln + trade_ln + air_ln + agri_share_ln + population_ln + factor(soviet_hist)| 
                 gdp_per_ln_centered, data = analysis(split)) %>%
    coef() %>% as.data.frame() %>% 
    tibble::rownames_to_column( "term") %>% dplyr::rename(estimate = 2) %>% as_tibble()}

## run multiple regression
cur_lm_boot <-
  data_boot %>%
  mutate(results = purrr::map(splits, lm_est))

## get coefficients
cur_lm_coef <- cur_lm_boot %>%
  unnest(results) 

## extract info and summarize
boot_1_gov1_google_direct <- cur_lm_coef %>% dplyr::select(term, estimate) %>%
  group_by(term) %>%
  summarise(mean_estimate = mean(estimate),
            ci_95_low  = quantile(estimate,probs=c(.025)),
            ci_95_high  = quantile(estimate,probs=c(.975)),
            ci_99_low  = quantile(estimate,probs=c(.005)),
            ci_99_high  = quantile(estimate,probs=c(.995))) %>%
  mutate(info = paste0(round(mean_estimate,2), " [",round(ci_95_low,2),
                       " to ",round(ci_95_high,2),"]")) %>%
  dplyr::rename(boot_1_gov1_google_direct = 7) %>% mutate(model = "boot_1_gov1_google_direct")

boot_1_gov1_google_direct



#-------------------------------------------------------------------------------
# 3.3.2 climate policy performance 
#-------------------------------------------------------------------------------

# run g-estimation
google_cc_direct <- sequential_g(gov_climate_ln ~ tp_google_2_ln + latitude_abs_ln | polity2_ln + education_ln +
                                   internet_share_ln + trade_ln + air_ln + agri_share_ln + population_ln + factor(soviet_hist)| 
                                   gdp_per_ln_centered, data = d2_boot_google_direct)
summary(google_cc_direct)


## set function and data for bootstrapping
lm_est <- function(split, ...) {
  sequential_g(gov_climate_ln ~ tp_google_2_ln + latitude_abs_ln | polity2_ln + education_ln +
                 internet_share_ln + trade_ln + air_ln + agri_share_ln + population_ln + factor(soviet_hist)| 
                 gdp_per_ln_centered, data = analysis(split)) %>%
    coef() %>% as.data.frame() %>% 
    tibble::rownames_to_column( "term") %>% dplyr::rename(estimate = 2) %>% as_tibble()}

## run multiple regression
cur_lm_boot <-
  data_boot %>%
  mutate(results = purrr::map(splits, lm_est))

## get coefficients
cur_lm_coef <- cur_lm_boot %>%
  unnest(results) 

## extract info and summarize
boot_1_cc_google_direct <- cur_lm_coef %>% dplyr::select(term, estimate) %>%
  group_by(term) %>%
  summarise(mean_estimate = mean(estimate),
            ci_95_low  = quantile(estimate,probs=c(.025)),
            ci_95_high  = quantile(estimate,probs=c(.975)),
            ci_99_low  = quantile(estimate,probs=c(.005)),
            ci_99_high  = quantile(estimate,probs=c(.995))) %>%
  mutate(info = paste0(round(mean_estimate,2), " [",round(ci_95_low,2),
                       " to ",round(ci_95_high,2),"]")) %>%
  dplyr::rename(boot_1_cc_google_direct = 7) %>% mutate(model = "boot_1_cc_google_direct")

boot_1_cc_google_direct



#-------------------------------------------------------------------------------
# 3.3.3 biodiversity policy performance 
#-------------------------------------------------------------------------------

# run g-estimation
google_bd_direct <- sequential_g(gov_biodiversity_ln ~ tp_google_2_ln +latitude_abs_ln | polity2_ln + education_ln +
                                   internet_share_ln + trade_ln + air_ln + agri_share_ln + population_ln + factor(soviet_hist)| 
                                   gdp_per_ln_centered, data = d2_boot_google_direct)
summary(google_bd_direct)


## set function and data for bootstrapping
lm_est <- function(split, ...) {
  sequential_g(gov_biodiversity_ln ~ tp_google_2_ln + latitude_abs_ln | polity2_ln + education_ln +
                 internet_share_ln + trade_ln + air_ln + agri_share_ln + population_ln + factor(soviet_hist)| 
                 gdp_per_ln_centered, data = analysis(split)) %>%
    coef() %>% as.data.frame() %>% 
    tibble::rownames_to_column( "term") %>% dplyr::rename(estimate = 2) %>% as_tibble()}

## run multiple regression
cur_lm_boot <-
  data_boot %>%
  mutate(results = purrr::map(splits, lm_est))

## get coefficients
cur_lm_coef <- cur_lm_boot %>%
  unnest(results) 

## extract info and summarize
boot_1_bd_google_direct <- cur_lm_coef %>% dplyr::select(term, estimate) %>%
  group_by(term) %>%
  summarise(mean_estimate = mean(estimate),
            ci_95_low  = quantile(estimate,probs=c(.025)),
            ci_95_high  = quantile(estimate,probs=c(.975)),
            ci_99_low  = quantile(estimate,probs=c(.005)),
            ci_99_high  = quantile(estimate,probs=c(.995))) %>%
  mutate(info = paste0(round(mean_estimate,2), " [",round(ci_95_low,2),
                       " to ",round(ci_95_high,2),"]")) %>%
  dplyr::rename(boot_1_bd_google_direct = 7) %>% mutate(model = "boot_1_bd_google_direct")

boot_1_bd_google_direct






##################--------------------------------------------------------------------------------
# 3.4 tables and plots 
##################--------------------------------------------------------------------------------


## add outputs together for ggplot
model_summary_boot_plot_direct <- bind_rows(boot_1_gov1_econ_direct %>% dplyr::select(term:ci_99_high,model),
                                            boot_1_cc_econ_direct %>% dplyr::select(term:ci_99_high,model),
                                            boot_1_bd_econ_direct %>% dplyr::select(term:ci_99_high,model),
                                            
                                            boot_1_gov1_thrift_direct %>% dplyr::select(term:ci_99_high,model),
                                            boot_1_cc_thrift_direct %>% dplyr::select(term:ci_99_high,model),
                                            boot_1_bd_thrift_direct %>% dplyr::select(term:ci_99_high,model),
                                            
                                            boot_1_gov1_google_direct %>% dplyr::select(term:ci_99_high,model),
                                            boot_1_cc_google_direct %>% dplyr::select(term:ci_99_high,model),
                                            boot_1_bd_google_direct %>% dplyr::select(term:ci_99_high,model))




theme_set(theme_bw()) # set main design
all_boot_plot_direct_extended <- model_summary_boot_plot_direct %>%
  filter(str_detect(term, "tp_")) %>%
  mutate(family_model =
           ifelse(str_detect(model, "gov1"), "D) Environmental Policy\nPerformance Overall",
                  ifelse(str_detect(model, "cc"), "E) Climate Policy\nPerformance","F) Biodiversity Policy\nPerformance")),
         family_model = factor(family_model, levels=c("D) Environmental Policy\nPerformance Overall",
                                                      "E) Climate Policy\nPerformance",
                                                      "F) Biodiversity Policy\nPerformance")),
         tp_type =
           ifelse(str_detect(term, "tp_gps"), "Economic Long-Term\nOrientation (log)",
                  ifelse(str_detect(term, "tp_hofstede"), "Thrift Long-Term\nOrientation (log)","Google Long-Term\nOrientation (log)")),
         tp_type = factor(tp_type, levels=c("Google Long-Term\nOrientation (log)", "Thrift Long-Term\nOrientation (log)", "Economic Long-Term\nOrientation (log)"))
  ) %>%
  ggplot(aes(y=tp_type  , x=mean_estimate)) +
  geom_vline(xintercept = 0, color=c("#525252")) +
  geom_errorbar (aes(y=tp_type , xmin = ci_99_low, xmax = ci_99_high, colour = "99% Confidence Interval"), size = 2,width=0, position=position_dodge(width=0.5))+
  geom_errorbar (aes(y=tp_type , xmin = ci_95_low, xmax = ci_95_high, colour = "95% Confidence Interval"), size = 2,width=0, position=position_dodge(width=0.5))+
  # geom_errorbar (data = sum_table, aes(x=reorder(Model, Estimate),ymin = Conf_Low_90, ymax = Conf_High_90, colour = "90% Confidence Interval"), size = 2,width=0, position=position_dodge(width=0.9))+
  geom_point(aes(shape = "Point Esimate", colour = "Point Esimate"), shape = 21, fill = "white",
             size = 3.5, position=position_dodge(width=0.5))+
  facet_grid(~family_model, scales = "fixed") +
  theme(legend.position = "none",
        legend.justification = c(0, 1),
        legend.title=element_blank(),
        #legend.text=element_text(size=3.5),
        legend.direction = "horizontal",
        legend.margin = margin(0, 0, 0, 0),
        #legend.justification = "left",
        legend.spacing.y = unit(-0, "cm"),
        legend.spacing.x = unit(2, "pt"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        axis.title.y=element_blank(),
        strip.text.x = element_text(angle = 0, hjust = 0))  +
  scale_color_manual(values = c("#1f78b4","#a6cee3","#1f78b4"))+
  scale_x_continuous(name = "") 

all_boot_plot_direct_extended



################################################################################
# 4. save figures
################################################################################

# combine figures
figure_s3 <- all_boot_plot_direct_base / all_boot_plot_direct_extended + 
  plot_annotation('Direct Relationships using Sequential G-Estimation', tag_levels = 'I')

setwd(cd_results) # set working directory
ggsave(figure_s3, filename = "v1_figure_s3.pdf",     width=17/4*3*.85, height=8.5/6*4.5*.60*2)