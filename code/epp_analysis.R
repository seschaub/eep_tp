################################################################################
# r code related to                                                            #
# "Global Relationships between Time Preference and Environmental Policy       #
# Performance" by Sergei Schaub published in Environmental Science & Policy    #
# -----------------------------------------------------------------------------#
# 2021, ETH Zurich                                                             #
# developed by Sergei Schaub                                                   #
# -----------------------------------------------------------------------------#
# analysis                                                                     #
################################################################################



# table of contents:
# 1. load data and prepare data
# 2. main analysis - economic time preference 
# 3. main analysis - thrift time preference 
# 4. main analysis - google time preference 
# 5. sub analysis - public environmental priority  
# 6. save results

################################################################################
# 1. load data and prepare data
################################################################################

# load data
setwd(cd_input_data)
load("epp_tp_data_main.RData")
load("epp_tp_data_availability.RData")

# create datasets to be re-sampled from
## economic time preference 
d2_boot_econ <- d2 %>%
  filter(!is.na(gov_epp_ln),
         !is.na(gov_climate_ln),
         !is.na(gov_biodiversity_ln),
         !is.na(tp_gps_ln),
         !is.na(gdp_per_ln),
         !is.na(polity2_ln),
         !is.na(latitude_abs_ln))

length(d2_boot_econ$country_name)


## thrift time preference 
d2_boot_thrift <- d2 %>%
  filter(!is.na(gov_epp_ln),
         !is.na(gov_climate_ln),
         !is.na(gov_biodiversity_ln),
         !is.na(tp_hofstede_ln),
         !is.na(gdp_per_ln),
         !is.na(polity2_ln),
         !is.na(latitude_abs_ln))

length(d2_boot_thrift$country_name)


## google time preference 
d2_boot_google <- d2 %>%
  filter(!is.na(gov_epp_ln),
         !is.na(gov_climate_ln),
         !is.na(gov_biodiversity_ln),
         !is.na(tp_google_2_ln),
         !is.na(gdp_per_ln),
         !is.na(polity2_ln),
         !is.na(latitude_abs_ln))

length(d2_boot_google$country_name)


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

length(d2_boot_sub$country_name)

## public environmental priority - index 1
d2_boot_pub1 <- d2 %>%
  filter(!is.na(gov_epp_ln),
         !is.na(gov_climate_ln),
         !is.na(gov_biodiversity_ln),
         !is.na(tp_gps_ln),
         !is.na(pub_prio_1_ln),
         !is.na(gdp_per_ln),
         !is.na(polity2_ln),
         !is.na(latitude_abs_ln))

length(d2_boot_pub1$country_name)
min(exp(d2_boot_pub1$pub_prio_1_ln))
max(exp(d2_boot_pub1$pub_prio_1_ln))

## public environmental priority - index 2
d2_boot_pub2 <- d2 %>%
  filter(!is.na(gov_epp_ln),
         !is.na(gov_climate_ln),
         !is.na(gov_biodiversity_ln),
         !is.na(tp_gps_ln),
         !is.na(pub_prio_2_ln),
         !is.na(gdp_per_ln),
         !is.na(polity2_ln),
         !is.na(latitude_abs_ln))

length(d2_boot_pub2$country_name)
min(d2_boot_pub2$pub_prio_2)
max(d2_boot_pub2$pub_prio_2)



################################################################################
# 2. main analysis - economic time preference
################################################################################

##################--------------------------------------------------------------
# 2.1 bootstrapped regression models - economic time preference, entire sample
##################--------------------------------------------------------------

# bootstrap data
set.seed(123)
data_boot <- bootstraps(d2_boot_econ %>% as_tibble(), times = 1000, apparent = FALSE)

# for viewing a single bootstrap: View(data_boot[[1]][[1]]$data)

## overall
### run models
boot_model_1 <- data_boot %>%
  mutate(model = purrr::map(splits, ~ lm(gov_epp_ln ~ tp_gps_ln, data = .)),
    coef_info = purrr::map(model, tidy))

boot_model_2 <- data_boot %>%
  mutate(model = purrr::map(splits, ~ lm(gov_epp_ln ~ tp_gps_ln + gdp_per_ln +
                                    polity2_ln + latitude_abs_ln, data = .)),
    coef_info = purrr::map(model, tidy))

### get coefficients
boot_coef_1 <- boot_model_1 %>%
  unnest(coef_info)

boot_coef_2 <- boot_model_2 %>%
  unnest(coef_info)

### extract info and summarize
boot_1_over_econ <- boot_coef_1 %>% dplyr::select(term, estimate) %>%
  group_by(term) %>%
  summarise(mean_estimate = mean(estimate),
            ci_95_low  = quantile(estimate,probs=c(.025)),
            ci_95_high  = quantile(estimate,probs=c(.975)),
            ci_99_low  = quantile(estimate,probs=c(.005)),
            ci_99_high  = quantile(estimate,probs=c(.995))) %>%
  mutate(info = paste0(round(mean_estimate,2), " [",round(ci_95_low,2),
                       " to ",round(ci_95_high,2),"]")) %>%
  dplyr::rename(model_1_overall_econ_boot = 7) %>% mutate(model = "model_1_overall_econ_boot")

boot_2_over_econ <- boot_coef_2 %>% dplyr::select(term, estimate) %>%
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
  mutate(model = purrr::map(splits, ~ lm(gov_climate_ln ~ tp_gps_ln, data = .)),
         coef_info = purrr::map(model, tidy))

boot_model_2 <- data_boot %>%
  mutate(model = purrr::map(splits, ~ lm(gov_climate_ln ~ tp_gps_ln + gdp_per_ln +
                                    polity2_ln + latitude_abs_ln, data = .)),
         coef_info = purrr::map(model, tidy))

### get coefficients
boot_coef_1 <- boot_model_1 %>%
  unnest(coef_info)

boot_coef_2 <- boot_model_2 %>%
  unnest(coef_info)

### extract info and summarize
boot_1_cc_econ <- boot_coef_1 %>% dplyr::select(term, estimate) %>%
  group_by(term) %>%
  summarise(mean_estimate = mean(estimate),
            ci_95_low  = quantile(estimate,probs=c(.025)),
            ci_95_high  = quantile(estimate,probs=c(.975)),
            ci_99_low  = quantile(estimate,probs=c(.005)),
            ci_99_high  = quantile(estimate,probs=c(.995))) %>%
  mutate(info = paste0(round(mean_estimate,2), " [",round(ci_95_low,2),
                       " to ",round(ci_95_high,2),"]")) %>%
  dplyr::rename(model_1_cc_econ_boot = 7) %>% mutate(model = "model_1_cc_econ_boot")

boot_2_cc_econ <- boot_coef_2 %>% dplyr::select(term, estimate) %>%
  group_by(term) %>%
  summarise(mean_estimate = mean(estimate),
            ci_95_low  = quantile(estimate,probs=c(.025)),
            ci_95_high  = quantile(estimate,probs=c(.975)),
            ci_99_low  = quantile(estimate,probs=c(.005)),
            ci_99_high  = quantile(estimate,probs=c(.995))) %>%
  mutate(info = paste0(round(mean_estimate,2), " [",round(ci_95_low,2),
                       " to ",round(ci_95_high,2),"]")) %>%
  dplyr::rename(model_2_cc_econ_boot = 7) %>% mutate(model = "model_2_cc_econ_boot")



## biodiversity
### run models
boot_model_1 <- data_boot %>%
  mutate(model = purrr::map(splits, ~ lm(gov_biodiversity_ln ~ tp_gps_ln, data = .)),
         coef_info = purrr::map(model, tidy))

boot_model_2 <- data_boot %>%
  mutate(model = purrr::map(splits, ~ lm(gov_biodiversity_ln ~ tp_gps_ln + gdp_per_ln +
                                    polity2_ln + latitude_abs_ln, data = .)),
         coef_info = purrr::map(model, tidy))

### get coefficients
boot_coef_1 <- boot_model_1 %>%
  unnest(coef_info)

boot_coef_2 <- boot_model_2 %>%
  unnest(coef_info)

### extract info and summarize
boot_1_bd_econ <- boot_coef_1 %>% dplyr::select(term, estimate) %>%
  group_by(term) %>%
  summarise(mean_estimate = mean(estimate),
            ci_95_low  = quantile(estimate,probs=c(.025)),
            ci_95_high  = quantile(estimate,probs=c(.975)),
            ci_99_low  = quantile(estimate,probs=c(.005)),
            ci_99_high  = quantile(estimate,probs=c(.995))) %>%
  mutate(info = paste0(round(mean_estimate,2), " [",round(ci_95_low,2),
                       " to ",round(ci_95_high,2),"]")) %>%
  dplyr::rename(model_1_bd_econ_boot = 7) %>% mutate(model = "model_1_bd_econ_boot")

boot_2_bd_econ <- boot_coef_2 %>% dplyr::select(term, estimate) %>%
  group_by(term) %>%
  summarise(mean_estimate = mean(estimate),
            ci_95_low  = quantile(estimate,probs=c(.025)),
            ci_95_high  = quantile(estimate,probs=c(.975)),
            ci_99_low  = quantile(estimate,probs=c(.005)),
            ci_99_high  = quantile(estimate,probs=c(.995))) %>%
  mutate(info = paste0(round(mean_estimate,2), " [",round(ci_95_low,2),
                       " to ",round(ci_95_high,2),"]")) %>%
  dplyr::rename(model_2_bd_econ_boot = 7) %>% mutate(model = "model_2_bd_econ_boot")

## add outputs together for ggplot
model_summary_boot_plot <- bind_rows(boot_1_over_econ %>% dplyr::select(term:ci_99_high,model),
          boot_2_over_econ %>% dplyr::select(term:ci_99_high,model),
          boot_1_cc_econ %>% dplyr::select(term:ci_99_high,model),
          boot_2_cc_econ %>% dplyr::select(term:ci_99_high,model),
          boot_1_bd_econ %>% dplyr::select(term:ci_99_high,model),
          boot_2_bd_econ %>% dplyr::select(term:ci_99_high,model))


## add outputs together for table
model_summary_boot_econ <-
  full_join(boot_1_over_econ %>% dplyr::select(term, contains("_boot")),
            boot_2_over_econ %>% dplyr::select(term, contains("_boot")),
            by = "term") %>%
  full_join(boot_1_cc_econ %>% dplyr::select(term, contains("_boot")),
            by = "term") %>%
  full_join(boot_2_cc_econ %>% dplyr::select(term, contains("_boot")),
            by = "term") %>%
  full_join(boot_1_bd_econ %>% dplyr::select(term, contains("_boot")),
            by = "term") %>%
  full_join(boot_2_bd_econ %>% dplyr::select(term, contains("_boot")),
            by = "term")



theme_set(theme_bw()) # set main design
figure_2 <- model_summary_boot_plot %>%
   filter(term != "(Intercept)") %>%
  mutate(family_model =
           ifelse(str_detect(model, "overall"), "A) Environmental Policy Performance Overall", 
                  ifelse(str_detect(model, "cc"), "B) Climate Policy Performance","C) Biodiversity Policy Performance")),
         family_model = factor(family_model, levels=c("A) Environmental Policy Performance Overall",
                                                      "B) Climate Policy Performance",
                                                      "C) Biodiversity Policy Performance")),
         model_1_2 =
           ifelse(str_detect(model, "model_1"), "Model without Controls","Model with Controls"),
         term = factor(term, levels=c("latitude_abs_ln",  "polity2_ln", "gdp_per_ln", "tp_gps_ln"))
         ) %>%
  ggplot(aes(y=term  , x=mean_estimate, color = factor(model_1_2))) +
  geom_vline(xintercept = 0, color=c("#525252")) +
  geom_errorbar (aes(y=term , xmin = ci_99_low, xmax = ci_99_high, colour = paste(model_1_2,"99% Confidence Interval")), size = 2,width=0, position=position_dodge(width=0.5))+
   geom_errorbar (aes(y=term , xmin = ci_95_low, xmax = ci_95_high, colour = paste(model_1_2,"95% Confidence Interval")), size = 2,width=0, position=position_dodge(width=0.5))+
    # geom_errorbar (data = sum_table, aes(x=reorder(Model, Estimate),ymin = Conf_Low_90, ymax = Conf_High_90, colour = "90% Confidence Interval"), size = 2,width=0, position=position_dodge(width=0.9))+
  geom_point(aes(shape = "Point Esimate", colour = factor(model_1_2)), shape = 21, fill = "white",
             size = 3.5, position=position_dodge(width=0.5))+
  facet_grid(~family_model, scales = "fixed") +
  theme(legend.position = "bottom",
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
  scale_color_manual(values = c("#1f78b4","#1f78b4","#a6cee3","#33a02c","#33a02c","#b2df8a"))+
  scale_x_continuous(name = "")+
  scale_y_discrete(labels =c('Latitude\n(absolute; log)', 'Democracy \nIndex (log)',
                             'GDP per \nCapita (log)', 'Long-Term Orientation\n(log; economic)'))

figure_2


##################--------------------------------------------------------------
# 2.2 bootstrapped regression models - economic time preference, sub-sample
##################--------------------------------------------------------------


# bootstrap data
set.seed(123)
data_boot <- bootstraps(d2_boot_sub, times = 1000, apparent = FALSE)

## overall
### run models
boot_model_1 <- data_boot %>%
  mutate(model = purrr::map(splits, ~ lm(gov_epp_ln ~ tp_hofstede_ln, data = .)),
         coef_info = purrr::map(model, tidy))

boot_model_2 <- data_boot %>%
  mutate(model = purrr::map(splits, ~ lm(gov_epp_ln ~ tp_hofstede_ln + gdp_per_ln +
                                    polity2_ln + latitude_abs_ln, data = .)),
         coef_info = purrr::map(model, tidy))

### get coefficients
boot_coef_1 <- boot_model_1 %>%
  unnest(coef_info)

boot_coef_2 <- boot_model_2 %>%
  unnest(coef_info)

### extract info and summarize
boot_1_over_econ <- boot_coef_1 %>% dplyr::select(term, estimate) %>%
  group_by(term) %>%
  summarise(mean_estimate = mean(estimate),
            ci_95_low  = quantile(estimate,probs=c(.025)),
            ci_95_high  = quantile(estimate,probs=c(.975)),
            ci_99_low  = quantile(estimate,probs=c(.005)),
            ci_99_high  = quantile(estimate,probs=c(.995))) %>%
  mutate(info = paste0(round(mean_estimate,2), " [",round(ci_95_low,2),
                       " to ",round(ci_95_high,2),"]")) %>%
  dplyr::rename(model_1_overall_econ_boot = 7) %>% mutate(model = "model_1_overall_econ_boot")

boot_2_over_econ <- boot_coef_2 %>% dplyr::select(term, estimate) %>%
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
  mutate(model = purrr::map(splits, ~ lm(gov_climate_ln ~ tp_hofstede_ln, data = .)),
         coef_info = purrr::map(model, tidy))

boot_model_2 <- data_boot %>%
  mutate(model = purrr::map(splits, ~ lm(gov_climate_ln ~ tp_hofstede_ln + gdp_per_ln +
                                    polity2_ln + latitude_abs_ln, data = .)),
         coef_info = purrr::map(model, tidy))

### get coefficients
boot_coef_1 <- boot_model_1 %>%
  unnest(coef_info)

boot_coef_2 <- boot_model_2 %>%
  unnest(coef_info)

### extract info and summarize
boot_1_cc_econ <- boot_coef_1 %>% dplyr::select(term, estimate) %>%
  group_by(term) %>%
  summarise(mean_estimate = mean(estimate),
            ci_95_low  = quantile(estimate,probs=c(.025)),
            ci_95_high  = quantile(estimate,probs=c(.975)),
            ci_99_low  = quantile(estimate,probs=c(.005)),
            ci_99_high  = quantile(estimate,probs=c(.995))) %>%
  mutate(info = paste0(round(mean_estimate,2), " [",round(ci_95_low,2),
                       " to ",round(ci_95_high,2),"]")) %>%
  dplyr::rename(model_1_cc_econ_boot = 7) %>% mutate(model = "model_1_cc_econ_boot")

boot_2_cc_econ <- boot_coef_2 %>% dplyr::select(term, estimate) %>%
  group_by(term) %>%
  summarise(mean_estimate = mean(estimate),
            ci_95_low  = quantile(estimate,probs=c(.025)),
            ci_95_high  = quantile(estimate,probs=c(.975)),
            ci_99_low  = quantile(estimate,probs=c(.005)),
            ci_99_high  = quantile(estimate,probs=c(.995))) %>%
  mutate(info = paste0(round(mean_estimate,2), " [",round(ci_95_low,2),
                       " to ",round(ci_95_high,2),"]")) %>%
  dplyr::rename(model_2_cc_econ_boot = 7) %>% mutate(model = "model_2_cc_econ_boot")



## biodiversity
### run models
boot_model_1 <- data_boot %>%
  mutate(model = purrr::map(splits, ~ lm(gov_biodiversity_ln ~ tp_hofstede_ln, data = .)),
         coef_info = purrr::map(model, tidy))

boot_model_2 <- data_boot %>%
  mutate(model = purrr::map(splits, ~ lm(gov_biodiversity_ln ~ tp_hofstede_ln + gdp_per_ln +
                                    polity2_ln + latitude_abs_ln, data = .)),
         coef_info = purrr::map(model, tidy))

### get coefficients
boot_coef_1 <- boot_model_1 %>%
  unnest(coef_info)

boot_coef_2 <- boot_model_2 %>%
  unnest(coef_info)

### extract info and summarize
boot_1_bd_econ <- boot_coef_1 %>% dplyr::select(term, estimate) %>%
  group_by(term) %>%
  summarise(mean_estimate = mean(estimate),
            ci_95_low  = quantile(estimate,probs=c(.025)),
            ci_95_high  = quantile(estimate,probs=c(.975)),
            ci_99_low  = quantile(estimate,probs=c(.005)),
            ci_99_high  = quantile(estimate,probs=c(.995))) %>%
  mutate(info = paste0(round(mean_estimate,2), " [",round(ci_95_low,2),
                       " to ",round(ci_95_high,2),"]")) %>%
  dplyr::rename(model_1_bd_econ_boot = 7) %>% mutate(model = "model_1_bd_econ_boot")

boot_2_bd_econ <- boot_coef_2 %>% dplyr::select(term, estimate) %>%
  group_by(term) %>%
  summarise(mean_estimate = mean(estimate),
            ci_95_low  = quantile(estimate,probs=c(.025)),
            ci_95_high  = quantile(estimate,probs=c(.975)),
            ci_99_low  = quantile(estimate,probs=c(.005)),
            ci_99_high  = quantile(estimate,probs=c(.995))) %>%
  mutate(info = paste0(round(mean_estimate,2), " [",round(ci_95_low,2),
                       " to ",round(ci_95_high,2),"]")) %>%
  dplyr::rename(model_2_bd_econ_boot = 7) %>% mutate(model = "model_2_bd_econ_boot")

## add outputs together for ggplot
model_summary_boot_plot <- bind_rows(boot_1_over_econ %>% dplyr::select(term:ci_99_high,model),
                                     boot_2_over_econ %>% dplyr::select(term:ci_99_high,model),
                                     boot_1_cc_econ %>% dplyr::select(term:ci_99_high,model),
                                     boot_2_cc_econ %>% dplyr::select(term:ci_99_high,model),
                                     boot_1_bd_econ %>% dplyr::select(term:ci_99_high,model),
                                     boot_2_bd_econ %>% dplyr::select(term:ci_99_high,model))


## add outputs together for table
model_summary_boot_econ_sub <-
  full_join(boot_1_over_econ %>% dplyr::select(term, contains("_boot")),
            boot_2_over_econ %>% dplyr::select(term, contains("_boot")),
            by = "term") %>%
  full_join(boot_1_cc_econ %>% dplyr::select(term, contains("_boot")),
            by = "term") %>%
  full_join(boot_2_cc_econ %>% dplyr::select(term, contains("_boot")),
            by = "term") %>%
  full_join(boot_1_bd_econ %>% dplyr::select(term, contains("_boot")),
            by = "term") %>%
  full_join(boot_2_bd_econ %>% dplyr::select(term, contains("_boot")),
            by = "term")



theme_set(theme_bw()) # set main design
econ_boot_plot_sub <- model_summary_boot_plot %>%
  filter(term != "(Intercept)") %>%
  mutate(family_model =
           ifelse(str_detect(model, "overall"), "A) Environmental Policy Performance Overall",
                  ifelse(str_detect(model, "cc"), "B) Climate Policy Performance","C) Biodiversity Policy Performance")),
         family_model = factor(family_model, levels=c("A) Environmental Policy Performance Overall",
                                                      "B) Climate Policy Performance",
                                                      "C) Biodiversity Policy Performance")),
         model_1_2 =
           ifelse(str_detect(model, "model_1"), "Model without Controls","Model with Controls"),
         term = factor(term, levels=c("latitude_abs_ln",  "polity2_ln", "gdp_per_ln", "tp_gps_ln"))
  ) %>%
  ggplot(aes(y=term  , x=mean_estimate, color = factor(model_1_2))) +
  geom_vline(xintercept = 0, color=c("#525252")) +
  geom_errorbar (aes(y=term , xmin = ci_99_low, xmax = ci_99_high, colour = paste(model_1_2,"99% Confidence Interval")), size = 2,width=0, position=position_dodge(width=0.5))+
  geom_errorbar (aes(y=term , xmin = ci_95_low, xmax = ci_95_high, colour = paste(model_1_2,"95% Confidence Interval")), size = 2,width=0, position=position_dodge(width=0.5))+
  # geom_errorbar (data = sum_table, aes(x=reorder(Model, Estimate),ymin = Conf_Low_90, ymax = Conf_High_90, colour = "90% Confidence Interval"), size = 2,width=0, position=position_dodge(width=0.9))+
  geom_point(aes(shape = "Point Esimate", colour = factor(model_1_2)), shape = 21, fill = "white",
             size = 3.5, position=position_dodge(width=0.5))+
  facet_grid(~family_model, scales = "fixed") +
  theme(legend.position = "bottom",
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
  scale_color_manual(values = c("#1f78b4","#1f78b4","#a6cee3","#33a02c","#33a02c","#b2df8a"))+
  scale_x_continuous(name = "")+
  scale_y_discrete(labels =c('Latitude\n(absolute; log)', 'Democracy \nIndex (log)',
                             'GDP per \nCapita (log)', 'Long-Term Orientation\n(log; economic)'))

econ_boot_plot_sub


################################################################################
# 3. main analysis - thrift time preference
################################################################################

##################--------------------------------------------------------------
# 3.1 bootstrapped regression models - thrift time preference, entire sample
##################--------------------------------------------------------------

# bootstrap data
set.seed(123)
data_boot <- bootstraps(d2_boot_thrift, times = 1000, apparent = FALSE)

## overall
### run models
boot_model_1 <- data_boot %>%
  mutate(model = purrr::map(splits, ~ lm(gov_epp_ln ~ tp_hofstede_ln, data = .)),
         coef_info = purrr::map(model, tidy))

boot_model_2 <- data_boot %>%
  mutate(model = purrr::map(splits, ~ lm(gov_epp_ln ~ tp_hofstede_ln + gdp_per_ln +
                                    polity2_ln + latitude_abs_ln, data = .)),
         coef_info = purrr::map(model, tidy))

### get coefficients
boot_coef_1 <- boot_model_1 %>%
  unnest(coef_info)

boot_coef_2 <- boot_model_2 %>%
  unnest(coef_info)

### extract info and summarize
boot_1_over_thrift <- boot_coef_1 %>% dplyr::select(term, estimate) %>%
  group_by(term) %>%
  summarise(mean_estimate = mean(estimate),
            ci_95_low  = quantile(estimate,probs=c(.025)),
            ci_95_high  = quantile(estimate,probs=c(.975)),
            ci_99_low  = quantile(estimate,probs=c(.005)),
            ci_99_high  = quantile(estimate,probs=c(.995))) %>%
  mutate(info = paste0(round(mean_estimate,2), " [",round(ci_95_low,2),
                       " to ",round(ci_95_high,2),"]")) %>%
  dplyr::rename(model_1_overall_thrift_boot = 7) %>% mutate(model = "model_1_overall_thrift_boot")

boot_2_over_thrift <- boot_coef_2 %>% dplyr::select(term, estimate) %>%
  group_by(term) %>%
  summarise(mean_estimate = mean(estimate),
            ci_95_low  = quantile(estimate,probs=c(.025)),
            ci_95_high  = quantile(estimate,probs=c(.975)),
            ci_99_low  = quantile(estimate,probs=c(.005)),
            ci_99_high  = quantile(estimate,probs=c(.995))) %>%
  mutate(info = paste0(round(mean_estimate,2), " [",round(ci_95_low,2),
                       " to ",round(ci_95_high,2),"]")) %>%
  dplyr::rename(model_2_overall_thrift_boot = 7) %>% mutate(model = "model_2_overall_thrift_boot")




## climate change
### run models
boot_model_1 <- data_boot %>%
  mutate(model = purrr::map(splits, ~ lm(gov_climate_ln ~ tp_hofstede_ln, data = .)),
         coef_info = purrr::map(model, tidy))

boot_model_2 <- data_boot %>%
  mutate(model = purrr::map(splits, ~ lm(gov_climate_ln ~ tp_hofstede_ln + gdp_per_ln +
                                    polity2_ln + latitude_abs_ln, data = .)),
         coef_info = purrr::map(model, tidy))

### get coefficients
boot_coef_1 <- boot_model_1 %>%
  unnest(coef_info)

boot_coef_2 <- boot_model_2 %>%
  unnest(coef_info)

### extract info and summarize
boot_1_cc_thrift <- boot_coef_1 %>% dplyr::select(term, estimate) %>%
  group_by(term) %>%
  summarise(mean_estimate = mean(estimate),
            ci_95_low  = quantile(estimate,probs=c(.025)),
            ci_95_high  = quantile(estimate,probs=c(.975)),
            ci_99_low  = quantile(estimate,probs=c(.005)),
            ci_99_high  = quantile(estimate,probs=c(.995))) %>%
  mutate(info = paste0(round(mean_estimate,2), " [",round(ci_95_low,2),
                       " to ",round(ci_95_high,2),"]")) %>%
  dplyr::rename(model_1_cc_thrift_boot = 7) %>% mutate(model = "model_1_cc_thrift_boot")

boot_2_cc_thrift <- boot_coef_2 %>% dplyr::select(term, estimate) %>%
  group_by(term) %>%
  summarise(mean_estimate = mean(estimate),
            ci_95_low  = quantile(estimate,probs=c(.025)),
            ci_95_high  = quantile(estimate,probs=c(.975)),
            ci_99_low  = quantile(estimate,probs=c(.005)),
            ci_99_high  = quantile(estimate,probs=c(.995))) %>%
  mutate(info = paste0(round(mean_estimate,2), " [",round(ci_95_low,2),
                       " to ",round(ci_95_high,2),"]")) %>%
  dplyr::rename(model_2_cc_thrift_boot = 7) %>% mutate(model = "model_2_cc_thrift_boot")



## biodiversity
### run models
boot_model_1 <- data_boot %>%
  mutate(model = purrr::map(splits, ~ lm(gov_biodiversity_ln ~ tp_hofstede_ln, data = .)),
         coef_info = purrr::map(model, tidy))

boot_model_2 <- data_boot %>%
  mutate(model = purrr::map(splits, ~ lm(gov_biodiversity_ln ~ tp_hofstede_ln + gdp_per_ln +
                                    polity2_ln + latitude_abs_ln, data = .)),
         coef_info = purrr::map(model, tidy))

### get coefficients
boot_coef_1 <- boot_model_1 %>%
  unnest(coef_info)

boot_coef_2 <- boot_model_2 %>%
  unnest(coef_info)

### extract info and summarize
boot_1_bd_thrift <- boot_coef_1 %>% dplyr::select(term, estimate) %>%
  group_by(term) %>%
  summarise(mean_estimate = mean(estimate),
            ci_95_low  = quantile(estimate,probs=c(.025)),
            ci_95_high  = quantile(estimate,probs=c(.975)),
            ci_99_low  = quantile(estimate,probs=c(.005)),
            ci_99_high  = quantile(estimate,probs=c(.995))) %>%
  mutate(info = paste0(round(mean_estimate,2), " [",round(ci_95_low,2),
                       " to ",round(ci_95_high,2),"]")) %>%
  dplyr::rename(model_1_bd_thrift_boot = 7) %>% mutate(model = "model_1_bd_thrift_boot")

boot_2_bd_thrift <- boot_coef_2 %>% dplyr::select(term, estimate) %>%
  group_by(term) %>%
  summarise(mean_estimate = mean(estimate),
            ci_95_low  = quantile(estimate,probs=c(.025)),
            ci_95_high  = quantile(estimate,probs=c(.975)),
            ci_99_low  = quantile(estimate,probs=c(.005)),
            ci_99_high  = quantile(estimate,probs=c(.995))) %>%
  mutate(info = paste0(round(mean_estimate,2), " [",round(ci_95_low,2),
                       " to ",round(ci_95_high,2),"]")) %>%
  dplyr::rename(model_2_bd_thrift_boot = 7) %>% mutate(model = "model_2_bd_thrift_boot")

## add outputs together for ggplot
model_summary_boot_plot <- bind_rows(boot_1_over_thrift %>% dplyr::select(term:ci_99_high,model),
                                     boot_2_over_thrift %>% dplyr::select(term:ci_99_high,model),
                                     boot_1_cc_thrift %>% dplyr::select(term:ci_99_high,model),
                                     boot_2_cc_thrift %>% dplyr::select(term:ci_99_high,model),
                                     boot_1_bd_thrift %>% dplyr::select(term:ci_99_high,model),
                                     boot_2_bd_thrift %>% dplyr::select(term:ci_99_high,model))


## add outputs together for table
model_summary_boot_thrift <-
  full_join(boot_1_over_thrift %>% dplyr::select(term, contains("_boot")),
            boot_2_over_thrift %>% dplyr::select(term, contains("_boot")),
            by = "term") %>%
  full_join(boot_1_cc_thrift %>% dplyr::select(term, contains("_boot")),
            by = "term") %>%
  full_join(boot_2_cc_thrift %>% dplyr::select(term, contains("_boot")),
            by = "term") %>%
  full_join(boot_1_bd_thrift %>% dplyr::select(term, contains("_boot")),
            by = "term") %>%
  full_join(boot_2_bd_thrift %>% dplyr::select(term, contains("_boot")),
            by = "term")



theme_set(theme_bw()) # set main design
figure_3a <- model_summary_boot_plot %>%
  filter(term != "(Intercept)") %>%
  mutate(family_model =
           ifelse(str_detect(model, "overall"), "A) Environmental Policy Performance Overall",
                  ifelse(str_detect(model, "cc"), "B) Climate Policy Performance","C) Biodiversity Policy Performance")),
         family_model = factor(family_model, levels=c("A) Environmental Policy Performance Overall",
                                                      "B) Climate Policy Performance",
                                                      "C) Biodiversity Policy Performance")),
         model_1_2 =
           ifelse(str_detect(model, "model_1"), "Model without Controls","Model with Controls"),
         term = factor(term, levels=c("latitude_abs_ln",  "polity2_ln", "gdp_per_ln", "tp_gps_ln"))
  ) %>%
  ggplot(aes(y=term  , x=mean_estimate, color = factor(model_1_2))) +
  geom_vline(xintercept = 0, color=c("#525252")) +
  geom_errorbar (aes(y=term , xmin = ci_99_low, xmax = ci_99_high, colour = paste(model_1_2,"99% Confidence Interval")), size = 2,width=0, position=position_dodge(width=0.5))+
  geom_errorbar (aes(y=term , xmin = ci_95_low, xmax = ci_95_high, colour = paste(model_1_2,"95% Confidence Interval")), size = 2,width=0, position=position_dodge(width=0.5))+
  # geom_errorbar (data = sum_table, aes(x=reorder(Model, Estimate),ymin = Conf_Low_90, ymax = Conf_High_90, colour = "90% Confidence Interval"), size = 2,width=0, position=position_dodge(width=0.9))+
  geom_point(aes(shape = "Point Esimate", colour = factor(model_1_2)), shape = 21, fill = "white",
             size = 3.5, position=position_dodge(width=0.5))+
  facet_grid(~family_model, scales = "fixed") +
  theme(legend.position = "bottom",
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
  scale_color_manual(values = c("#1f78b4","#1f78b4","#a6cee3","#33a02c","#33a02c","#b2df8a"))+
  scale_x_continuous(name = "")+
  scale_y_discrete(labels =c('Latitude\n(absolute; log)', 'Democracy \nIndex (log)',
                             'GDP per \nCapita (log)', 'Long-Term Orientation\n(log; thrift)'))

figure_3a


##################--------------------------------------------------------------
# 3.2 bootstrapped regression models - thrift time preference, sub-sample
##################--------------------------------------------------------------


# bootstrap data
set.seed(123)
data_boot <- bootstraps(d2_boot_sub, times = 1000, apparent = FALSE)

## overall
### run models
boot_model_1 <- data_boot %>%
  mutate(model = purrr::map(splits, ~ lm(gov_epp_ln ~ tp_hofstede_ln, data = .)),
         coef_info = purrr::map(model, tidy))

boot_model_2 <- data_boot %>%
  mutate(model = purrr::map(splits, ~ lm(gov_epp_ln ~ tp_hofstede_ln + gdp_per_ln +
                                    polity2_ln + latitude_abs_ln, data = .)),
         coef_info = purrr::map(model, tidy))

### get coefficients
boot_coef_1 <- boot_model_1 %>%
  unnest(coef_info)

boot_coef_2 <- boot_model_2 %>%
  unnest(coef_info)

### extract info and summarize
boot_1_over_thrift <- boot_coef_1 %>% dplyr::select(term, estimate) %>%
  group_by(term) %>%
  summarise(mean_estimate = mean(estimate),
            ci_95_low  = quantile(estimate,probs=c(.025)),
            ci_95_high  = quantile(estimate,probs=c(.975)),
            ci_99_low  = quantile(estimate,probs=c(.005)),
            ci_99_high  = quantile(estimate,probs=c(.995))) %>%
  mutate(info = paste0(round(mean_estimate,2), " [",round(ci_95_low,2),
                       " to ",round(ci_95_high,2),"]")) %>%
  dplyr::rename(model_1_overall_thrift_boot = 7) %>% mutate(model = "model_1_overall_thrift_boot")

boot_2_over_thrift <- boot_coef_2 %>% dplyr::select(term, estimate) %>%
  group_by(term) %>%
  summarise(mean_estimate = mean(estimate),
            ci_95_low  = quantile(estimate,probs=c(.025)),
            ci_95_high  = quantile(estimate,probs=c(.975)),
            ci_99_low  = quantile(estimate,probs=c(.005)),
            ci_99_high  = quantile(estimate,probs=c(.995))) %>%
  mutate(info = paste0(round(mean_estimate,2), " [",round(ci_95_low,2),
                       " to ",round(ci_95_high,2),"]")) %>%
  dplyr::rename(model_2_overall_thrift_boot = 7) %>% mutate(model = "model_2_overall_thrift_boot")




## climate change
### run models
boot_model_1 <- data_boot %>%
  mutate(model = purrr::map(splits, ~ lm(gov_climate_ln ~ tp_hofstede_ln, data = .)),
         coef_info = purrr::map(model, tidy))

boot_model_2 <- data_boot %>%
  mutate(model = purrr::map(splits, ~ lm(gov_climate_ln ~ tp_hofstede_ln + gdp_per_ln +
                                    polity2_ln + latitude_abs_ln, data = .)),
         coef_info = purrr::map(model, tidy))

### get coefficients
boot_coef_1 <- boot_model_1 %>%
  unnest(coef_info)

boot_coef_2 <- boot_model_2 %>%
  unnest(coef_info)

### extract info and summarize
boot_1_cc_thrift <- boot_coef_1 %>% dplyr::select(term, estimate) %>%
  group_by(term) %>%
  summarise(mean_estimate = mean(estimate),
            ci_95_low  = quantile(estimate,probs=c(.025)),
            ci_95_high  = quantile(estimate,probs=c(.975)),
            ci_99_low  = quantile(estimate,probs=c(.005)),
            ci_99_high  = quantile(estimate,probs=c(.995))) %>%
  mutate(info = paste0(round(mean_estimate,2), " [",round(ci_95_low,2),
                       " to ",round(ci_95_high,2),"]")) %>%
  dplyr::rename(model_1_cc_thrift_boot = 7) %>% mutate(model = "model_1_cc_thrift_boot")

boot_2_cc_thrift <- boot_coef_2 %>% dplyr::select(term, estimate) %>%
  group_by(term) %>%
  summarise(mean_estimate = mean(estimate),
            ci_95_low  = quantile(estimate,probs=c(.025)),
            ci_95_high  = quantile(estimate,probs=c(.975)),
            ci_99_low  = quantile(estimate,probs=c(.005)),
            ci_99_high  = quantile(estimate,probs=c(.995))) %>%
  mutate(info = paste0(round(mean_estimate,2), " [",round(ci_95_low,2),
                       " to ",round(ci_95_high,2),"]")) %>%
  dplyr::rename(model_2_cc_thrift_boot = 7) %>% mutate(model = "model_2_cc_thrift_boot")



## biodiversity
### run models
boot_model_1 <- data_boot %>%
  mutate(model = purrr::map(splits, ~ lm(gov_biodiversity_ln ~ tp_hofstede_ln, data = .)),
         coef_info = purrr::map(model, tidy))

boot_model_2 <- data_boot %>%
  mutate(model = purrr::map(splits, ~ lm(gov_biodiversity_ln ~ tp_hofstede_ln + gdp_per_ln +
                                    polity2_ln + latitude_abs_ln, data = .)),
         coef_info = purrr::map(model, tidy))

### get coefficients
boot_coef_1 <- boot_model_1 %>%
  unnest(coef_info)

boot_coef_2 <- boot_model_2 %>%
  unnest(coef_info)

### extract info and summarize
boot_1_bd_thrift <- boot_coef_1 %>% dplyr::select(term, estimate) %>%
  group_by(term) %>%
  summarise(mean_estimate = mean(estimate),
            ci_95_low  = quantile(estimate,probs=c(.025)),
            ci_95_high  = quantile(estimate,probs=c(.975)),
            ci_99_low  = quantile(estimate,probs=c(.005)),
            ci_99_high  = quantile(estimate,probs=c(.995))) %>%
  mutate(info = paste0(round(mean_estimate,2), " [",round(ci_95_low,2),
                       " to ",round(ci_95_high,2),"]")) %>%
  dplyr::rename(model_1_bd_thrift_boot = 7) %>% mutate(model = "model_1_bd_thrift_boot")

boot_2_bd_thrift <- boot_coef_2 %>% dplyr::select(term, estimate) %>%
  group_by(term) %>%
  summarise(mean_estimate = mean(estimate),
            ci_95_low  = quantile(estimate,probs=c(.025)),
            ci_95_high  = quantile(estimate,probs=c(.975)),
            ci_99_low  = quantile(estimate,probs=c(.005)),
            ci_99_high  = quantile(estimate,probs=c(.995))) %>%
  mutate(info = paste0(round(mean_estimate,2), " [",round(ci_95_low,2),
                       " to ",round(ci_95_high,2),"]")) %>%
  dplyr::rename(model_2_bd_thrift_boot = 7) %>% mutate(model = "model_2_bd_thrift_boot")

## add outputs together for ggplot
model_summary_boot_plot <- bind_rows(boot_1_over_thrift %>% dplyr::select(term:ci_99_high,model),
                                     boot_2_over_thrift %>% dplyr::select(term:ci_99_high,model),
                                     boot_1_cc_thrift %>% dplyr::select(term:ci_99_high,model),
                                     boot_2_cc_thrift %>% dplyr::select(term:ci_99_high,model),
                                     boot_1_bd_thrift %>% dplyr::select(term:ci_99_high,model),
                                     boot_2_bd_thrift %>% dplyr::select(term:ci_99_high,model))


## add outputs together for table
model_summary_boot_thrift_sub <-
  full_join(boot_1_over_thrift %>% dplyr::select(term, contains("_boot")),
            boot_2_over_thrift %>% dplyr::select(term, contains("_boot")),
            by = "term") %>%
  full_join(boot_1_cc_thrift %>% dplyr::select(term, contains("_boot")),
            by = "term") %>%
  full_join(boot_2_cc_thrift %>% dplyr::select(term, contains("_boot")),
            by = "term") %>%
  full_join(boot_1_bd_thrift %>% dplyr::select(term, contains("_boot")),
            by = "term") %>%
  full_join(boot_2_bd_thrift %>% dplyr::select(term, contains("_boot")),
            by = "term")



theme_set(theme_bw()) # set main design
thrift_boot_plot_sub <- model_summary_boot_plot %>%
  filter(term != "(Intercept)") %>%
  mutate(family_model =
           ifelse(str_detect(model, "overall"), "A) Environmental Policy Performance Overall",
                  ifelse(str_detect(model, "cc"), "B) Climate Policy Performance","C) Biodiversity Policy Performance")),
         family_model = factor(family_model, levels=c("A) Environmental Policy Performance Overall",
                                                      "B) Climate Policy Performance",
                                                      "C) Biodiversity Policy Performance")),
         model_1_2 =
           ifelse(str_detect(model, "model_1"), "Model without Controls","Model with Controls"),
         term = factor(term, levels=c("latitude_abs_ln",  "polity2_ln", "gdp_per_ln", "tp_gps_ln"))
  ) %>%
  ggplot(aes(y=term  , x=mean_estimate, color = factor(model_1_2))) +
  geom_vline(xintercept = 0, color=c("#525252")) +
  geom_errorbar (aes(y=term , xmin = ci_99_low, xmax = ci_99_high, colour = paste(model_1_2,"99% Confidence Interval")), size = 2,width=0, position=position_dodge(width=0.5))+
  geom_errorbar (aes(y=term , xmin = ci_95_low, xmax = ci_95_high, colour = paste(model_1_2,"95% Confidence Interval")), size = 2,width=0, position=position_dodge(width=0.5))+
  # geom_errorbar (data = sum_table, aes(x=reorder(Model, Estimate),ymin = Conf_Low_90, ymax = Conf_High_90, colour = "90% Confidence Interval"), size = 2,width=0, position=position_dodge(width=0.9))+
  geom_point(aes(shape = "Point Esimate", colour = factor(model_1_2)), shape = 21, fill = "white",
             size = 3.5, position=position_dodge(width=0.5))+
  facet_grid(~family_model, scales = "fixed") +
  theme(legend.position = "bottom",
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
  scale_color_manual(values = c("#1f78b4","#1f78b4","#a6cee3","#33a02c","#33a02c","#b2df8a"))+
  scale_x_continuous(name = "")+
  scale_y_discrete(labels =c('Latitude\n(absolute; log)', 'Democracy \nIndex (log)',
                             'GDP per \nCapita (log)', 'Long-Term Orientation\n(log; thrift)'))

thrift_boot_plot_sub



################################################################################
# 4. main analysis - google time preference
################################################################################

##################--------------------------------------------------------------
# 4.1 bootstrapped regression models - google time preference, entire sample
##################--------------------------------------------------------------

# bootstrap data
set.seed(123)
data_boot <- bootstraps(d2_boot_google, times = 1000, apparent = FALSE)

## overall
### run models
boot_model_1 <- data_boot %>%
  mutate(model = purrr::map(splits, ~ lm(gov_epp_ln ~ tp_google_2_ln, data = .)),
         coef_info = purrr::map(model, tidy))

boot_model_2 <- data_boot %>%
  mutate(model = purrr::map(splits, ~ lm(gov_epp_ln ~ tp_google_2_ln + gdp_per_ln +
                                    polity2_ln + latitude_abs_ln, data = .)),
         coef_info = purrr::map(model, tidy))

### get coefficients
boot_coef_1 <- boot_model_1 %>%
  unnest(coef_info)

boot_coef_2 <- boot_model_2 %>%
  unnest(coef_info)

### extract info and summarize
boot_1_over_google <- boot_coef_1 %>% dplyr::select(term, estimate) %>%
  group_by(term) %>%
  summarise(mean_estimate = mean(estimate),
            ci_95_low  = quantile(estimate,probs=c(.025)),
            ci_95_high  = quantile(estimate,probs=c(.975)),
            ci_99_low  = quantile(estimate,probs=c(.005)),
            ci_99_high  = quantile(estimate,probs=c(.995))) %>%
  mutate(info = paste0(round(mean_estimate,2), " [",round(ci_95_low,2),
                       " to ",round(ci_95_high,2),"]")) %>%
  dplyr::rename(model_1_overall_google_boot = 7) %>% mutate(model = "model_1_overall_google_boot")

boot_2_over_google <- boot_coef_2 %>% dplyr::select(term, estimate) %>%
  group_by(term) %>%
  summarise(mean_estimate = mean(estimate),
            ci_95_low  = quantile(estimate,probs=c(.025)),
            ci_95_high  = quantile(estimate,probs=c(.975)),
            ci_99_low  = quantile(estimate,probs=c(.005)),
            ci_99_high  = quantile(estimate,probs=c(.995))) %>%
  mutate(info = paste0(round(mean_estimate,2), " [",round(ci_95_low,2),
                       " to ",round(ci_95_high,2),"]")) %>%
  dplyr::rename(model_2_overall_google_boot = 7) %>% mutate(model = "model_2_overall_google_boot")




## climate change
### run models
boot_model_1 <- data_boot %>%
  mutate(model = purrr::map(splits, ~ lm(gov_climate_ln ~ tp_google_2_ln, data = .)),
         coef_info = purrr::map(model, tidy))

boot_model_2 <- data_boot %>%
  mutate(model = purrr::map(splits, ~ lm(gov_climate_ln ~ tp_google_2_ln + gdp_per_ln +
                                    polity2_ln + latitude_abs_ln, data = .)),
         coef_info = purrr::map(model, tidy))

### get coefficients
boot_coef_1 <- boot_model_1 %>%
  unnest(coef_info)

boot_coef_2 <- boot_model_2 %>%
  unnest(coef_info)

### extract info and summarize
boot_1_cc_google <- boot_coef_1 %>% dplyr::select(term, estimate) %>%
  group_by(term) %>%
  summarise(mean_estimate = mean(estimate),
            ci_95_low  = quantile(estimate,probs=c(.025)),
            ci_95_high  = quantile(estimate,probs=c(.975)),
            ci_99_low  = quantile(estimate,probs=c(.005)),
            ci_99_high  = quantile(estimate,probs=c(.995))) %>%
  mutate(info = paste0(round(mean_estimate,2), " [",round(ci_95_low,2),
                       " to ",round(ci_95_high,2),"]")) %>%
  dplyr::rename(model_1_cc_google_boot = 7) %>% mutate(model = "model_1_cc_google_boot")

boot_2_cc_google <- boot_coef_2 %>% dplyr::select(term, estimate) %>%
  group_by(term) %>%
  summarise(mean_estimate = mean(estimate),
            ci_95_low  = quantile(estimate,probs=c(.025)),
            ci_95_high  = quantile(estimate,probs=c(.975)),
            ci_99_low  = quantile(estimate,probs=c(.005)),
            ci_99_high  = quantile(estimate,probs=c(.995))) %>%
  mutate(info = paste0(round(mean_estimate,2), " [",round(ci_95_low,2),
                       " to ",round(ci_95_high,2),"]")) %>%
  dplyr::rename(model_2_cc_google_boot = 7) %>% mutate(model = "model_2_cc_google_boot")



## biodiversity
### run models
boot_model_1 <- data_boot %>%
  mutate(model = purrr::map(splits, ~ lm(gov_biodiversity_ln ~ tp_google_2_ln, data = .)),
         coef_info = purrr::map(model, tidy))

boot_model_2 <- data_boot %>%
  mutate(model = purrr::map(splits, ~ lm(gov_biodiversity_ln ~ tp_google_2_ln + gdp_per_ln +
                                    polity2_ln + latitude_abs_ln, data = .)),
         coef_info = purrr::map(model, tidy))

### get coefficients
boot_coef_1 <- boot_model_1 %>%
  unnest(coef_info)

boot_coef_2 <- boot_model_2 %>%
  unnest(coef_info)

### extract info and summarize
boot_1_bd_google <- boot_coef_1 %>% dplyr::select(term, estimate) %>%
  group_by(term) %>%
  summarise(mean_estimate = mean(estimate),
            ci_95_low  = quantile(estimate,probs=c(.025)),
            ci_95_high  = quantile(estimate,probs=c(.975)),
            ci_99_low  = quantile(estimate,probs=c(.005)),
            ci_99_high  = quantile(estimate,probs=c(.995))) %>%
  mutate(info = paste0(round(mean_estimate,2), " [",round(ci_95_low,2),
                       " to ",round(ci_95_high,2),"]")) %>%
  dplyr::rename(model_1_bd_google_boot = 7) %>% mutate(model = "model_1_bd_google_boot")

boot_2_bd_google <- boot_coef_2 %>% dplyr::select(term, estimate) %>%
  group_by(term) %>%
  summarise(mean_estimate = mean(estimate),
            ci_95_low  = quantile(estimate,probs=c(.025)),
            ci_95_high  = quantile(estimate,probs=c(.975)),
            ci_99_low  = quantile(estimate,probs=c(.005)),
            ci_99_high  = quantile(estimate,probs=c(.995))) %>%
  mutate(info = paste0(round(mean_estimate,2), " [",round(ci_95_low,2),
                       " to ",round(ci_95_high,2),"]")) %>%
  dplyr::rename(model_2_bd_google_boot = 7) %>% mutate(model = "model_2_bd_google_boot")

## add outputs together for ggplot
model_summary_boot_plot <- bind_rows(boot_1_over_google %>% dplyr::select(term:ci_99_high,model),
                                     boot_2_over_google %>% dplyr::select(term:ci_99_high,model),
                                     boot_1_cc_google %>% dplyr::select(term:ci_99_high,model),
                                     boot_2_cc_google %>% dplyr::select(term:ci_99_high,model),
                                     boot_1_bd_google %>% dplyr::select(term:ci_99_high,model),
                                     boot_2_bd_google %>% dplyr::select(term:ci_99_high,model))


## add outputs together for table
model_summary_boot_google <-
  full_join(boot_1_over_google %>% dplyr::select(term, contains("_boot")),
            boot_2_over_google %>% dplyr::select(term, contains("_boot")),
            by = "term") %>%
  full_join(boot_1_cc_google %>% dplyr::select(term, contains("_boot")),
            by = "term") %>%
  full_join(boot_2_cc_google %>% dplyr::select(term, contains("_boot")),
            by = "term") %>%
  full_join(boot_1_bd_google %>% dplyr::select(term, contains("_boot")),
            by = "term") %>%
  full_join(boot_2_bd_google %>% dplyr::select(term, contains("_boot")),
            by = "term")



theme_set(theme_bw()) # set main design
figure_3b <- model_summary_boot_plot %>%
  filter(term != "(Intercept)") %>%
  mutate(family_model =
           ifelse(str_detect(model, "overall"), "A) Environmental Policy Performance Overall",
                  ifelse(str_detect(model, "cc"), "B) Climate Policy Performance","C) Biodiversity Policy Performance")),
         family_model = factor(family_model, levels=c("A) Environmental Policy Performance Overall",
                                                      "B) Climate Policy Performance",
                                                      "C) Biodiversity Policy Performance")),
         model_1_2 =
           ifelse(str_detect(model, "model_1"), "Model without Controls","Model with Controls"),
         term = factor(term, levels=c("latitude_abs_ln",  "polity2_ln", "gdp_per_ln", "tp_gps_ln"))
  ) %>%
  ggplot(aes(y=term  , x=mean_estimate, color = factor(model_1_2))) +
  geom_vline(xintercept = 0, color=c("#525252")) +
  geom_errorbar (aes(y=term , xmin = ci_99_low, xmax = ci_99_high, colour = paste(model_1_2,"99% Confidence Interval")), size = 2,width=0, position=position_dodge(width=0.5))+
  geom_errorbar (aes(y=term , xmin = ci_95_low, xmax = ci_95_high, colour = paste(model_1_2,"95% Confidence Interval")), size = 2,width=0, position=position_dodge(width=0.5))+
  # geom_errorbar (data = sum_table, aes(x=reorder(Model, Estimate),ymin = Conf_Low_90, ymax = Conf_High_90, colour = "90% Confidence Interval"), size = 2,width=0, position=position_dodge(width=0.9))+
  geom_point(aes(shape = "Point Esimate", colour = factor(model_1_2)), shape = 21, fill = "white",
             size = 3.5, position=position_dodge(width=0.5))+
  facet_grid(~family_model, scales = "fixed") +
  theme(legend.position = "bottom",
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
  scale_color_manual(values = c("#1f78b4","#1f78b4","#a6cee3","#33a02c","#33a02c","#b2df8a"))+
  scale_x_continuous(name = "")+
  scale_y_discrete(labels =c('Latitude\n(absolute; log)', 'Democracy \nIndex (log)',
                             'GDP per \nCapita (log)', 'Long-Term Orientation\n(log; Google)'))

figure_3b


##################--------------------------------------------------------------
# 4.2 bootstrapped regression models - google time preference, sub-sample
##################--------------------------------------------------------------

# bootstrap data
set.seed(123)
data_boot <- bootstraps(d2_boot_sub, times = 1000, apparent = FALSE)

## overall
### run models
boot_model_1 <- data_boot %>%
  mutate(model = purrr::map(splits, ~ lm(gov_epp_ln ~ tp_google_2_ln, data = .)),
         coef_info = purrr::map(model, tidy))

boot_model_2 <- data_boot %>%
  mutate(model = purrr::map(splits, ~ lm(gov_epp_ln ~ tp_google_2_ln + gdp_per_ln +
                                    polity2_ln + latitude_abs_ln, data = .)),
         coef_info = purrr::map(model, tidy))

### get coefficients
boot_coef_1 <- boot_model_1 %>%
  unnest(coef_info)

boot_coef_2 <- boot_model_2 %>%
  unnest(coef_info)

### extract info and summarize
boot_1_over_google <- boot_coef_1 %>% dplyr::select(term, estimate) %>%
  group_by(term) %>%
  summarise(mean_estimate = mean(estimate),
            ci_95_low  = quantile(estimate,probs=c(.025)),
            ci_95_high  = quantile(estimate,probs=c(.975)),
            ci_99_low  = quantile(estimate,probs=c(.005)),
            ci_99_high  = quantile(estimate,probs=c(.995))) %>%
  mutate(info = paste0(round(mean_estimate,2), " [",round(ci_95_low,2),
                       " to ",round(ci_95_high,2),"]")) %>%
  dplyr::rename(model_1_overall_google_boot = 7) %>% mutate(model = "model_1_overall_google_boot")

boot_2_over_google <- boot_coef_2 %>% dplyr::select(term, estimate) %>%
  group_by(term) %>%
  summarise(mean_estimate = mean(estimate),
            ci_95_low  = quantile(estimate,probs=c(.025)),
            ci_95_high  = quantile(estimate,probs=c(.975)),
            ci_99_low  = quantile(estimate,probs=c(.005)),
            ci_99_high  = quantile(estimate,probs=c(.995))) %>%
  mutate(info = paste0(round(mean_estimate,2), " [",round(ci_95_low,2),
                       " to ",round(ci_95_high,2),"]")) %>%
  dplyr::rename(model_2_overall_google_boot = 7) %>% mutate(model = "model_2_overall_google_boot")




## climate change
### run models
boot_model_1 <- data_boot %>%
  mutate(model = purrr::map(splits, ~ lm(gov_climate_ln ~ tp_google_2_ln, data = .)),
         coef_info = purrr::map(model, tidy))

boot_model_2 <- data_boot %>%
  mutate(model = purrr::map(splits, ~ lm(gov_climate_ln ~ tp_google_2_ln + gdp_per_ln +
                                    polity2_ln + latitude_abs_ln, data = .)),
         coef_info = purrr::map(model, tidy))

### get coefficients
boot_coef_1 <- boot_model_1 %>%
  unnest(coef_info)

boot_coef_2 <- boot_model_2 %>%
  unnest(coef_info)

### extract info and summarize
boot_1_cc_google <- boot_coef_1 %>% dplyr::select(term, estimate) %>%
  group_by(term) %>%
  summarise(mean_estimate = mean(estimate),
            ci_95_low  = quantile(estimate,probs=c(.025)),
            ci_95_high  = quantile(estimate,probs=c(.975)),
            ci_99_low  = quantile(estimate,probs=c(.005)),
            ci_99_high  = quantile(estimate,probs=c(.995))) %>%
  mutate(info = paste0(round(mean_estimate,2), " [",round(ci_95_low,2),
                       " to ",round(ci_95_high,2),"]")) %>%
  dplyr::rename(model_1_cc_google_boot = 7) %>% mutate(model = "model_1_cc_google_boot")

boot_2_cc_google <- boot_coef_2 %>% dplyr::select(term, estimate) %>%
  group_by(term) %>%
  summarise(mean_estimate = mean(estimate),
            ci_95_low  = quantile(estimate,probs=c(.025)),
            ci_95_high  = quantile(estimate,probs=c(.975)),
            ci_99_low  = quantile(estimate,probs=c(.005)),
            ci_99_high  = quantile(estimate,probs=c(.995))) %>%
  mutate(info = paste0(round(mean_estimate,2), " [",round(ci_95_low,2),
                       " to ",round(ci_95_high,2),"]")) %>%
  dplyr::rename(model_2_cc_google_boot = 7) %>% mutate(model = "model_2_cc_google_boot")



## biodiversity
### run models
boot_model_1 <- data_boot %>%
  mutate(model = purrr::map(splits, ~ lm(gov_biodiversity_ln ~ tp_google_2_ln, data = .)),
         coef_info = purrr::map(model, tidy))

boot_model_2 <- data_boot %>%
  mutate(model = purrr::map(splits, ~ lm(gov_biodiversity_ln ~ tp_google_2_ln + gdp_per_ln +
                                    polity2_ln + latitude_abs_ln, data = .)),
         coef_info = purrr::map(model, tidy))

### get coefficients
boot_coef_1 <- boot_model_1 %>%
  unnest(coef_info)

boot_coef_2 <- boot_model_2 %>%
  unnest(coef_info)

### extract info and summarize
boot_1_bd_google <- boot_coef_1 %>% dplyr::select(term, estimate) %>%
  group_by(term) %>%
  summarise(mean_estimate = mean(estimate),
            ci_95_low  = quantile(estimate,probs=c(.025)),
            ci_95_high  = quantile(estimate,probs=c(.975)),
            ci_99_low  = quantile(estimate,probs=c(.005)),
            ci_99_high  = quantile(estimate,probs=c(.995))) %>%
  mutate(info = paste0(round(mean_estimate,2), " [",round(ci_95_low,2),
                       " to ",round(ci_95_high,2),"]")) %>%
  dplyr::rename(model_1_bd_google_boot = 7) %>% mutate(model = "model_1_bd_google_boot")

boot_2_bd_google <- boot_coef_2 %>% dplyr::select(term, estimate) %>%
  group_by(term) %>%
  summarise(mean_estimate = mean(estimate),
            ci_95_low  = quantile(estimate,probs=c(.025)),
            ci_95_high  = quantile(estimate,probs=c(.975)),
            ci_99_low  = quantile(estimate,probs=c(.005)),
            ci_99_high  = quantile(estimate,probs=c(.995))) %>%
  mutate(info = paste0(round(mean_estimate,2), " [",round(ci_95_low,2),
                       " to ",round(ci_95_high,2),"]")) %>%
  dplyr::rename(model_2_bd_google_boot = 7) %>% mutate(model = "model_2_bd_google_boot")

## add outputs together for ggplot
model_summary_boot_plot <- bind_rows(boot_1_over_google %>% dplyr::select(term:ci_99_high,model),
                                     boot_2_over_google %>% dplyr::select(term:ci_99_high,model),
                                     boot_1_cc_google %>% dplyr::select(term:ci_99_high,model),
                                     boot_2_cc_google %>% dplyr::select(term:ci_99_high,model),
                                     boot_1_bd_google %>% dplyr::select(term:ci_99_high,model),
                                     boot_2_bd_google %>% dplyr::select(term:ci_99_high,model))


## add outputs together for table
model_summary_boot_google_sub <-
  full_join(boot_1_over_google %>% dplyr::select(term, contains("_boot")),
            boot_2_over_google %>% dplyr::select(term, contains("_boot")),
            by = "term") %>%
  full_join(boot_1_cc_google %>% dplyr::select(term, contains("_boot")),
            by = "term") %>%
  full_join(boot_2_cc_google %>% dplyr::select(term, contains("_boot")),
            by = "term") %>%
  full_join(boot_1_bd_google %>% dplyr::select(term, contains("_boot")),
            by = "term") %>%
  full_join(boot_2_bd_google %>% dplyr::select(term, contains("_boot")),
            by = "term")


theme_set(theme_bw()) # set main design
google_boot_plot_sub <- model_summary_boot_plot %>%
  filter(term != "(Intercept)") %>%
  mutate(family_model =
           ifelse(str_detect(model, "overall"), "A) Environmental Policy Performance Overall",
                  ifelse(str_detect(model, "cc"), "B) Climate Policy Performance","C) Biodiversity Policy Performance")),
         family_model = factor(family_model, levels=c("A) Environmental Policy Performance Overall",
                                                      "B) Climate Policy Performance",
                                                      "C) Biodiversity Policy Performance")),
         model_1_2 =
           ifelse(str_detect(model, "model_1"), "Model without Controls","Model with Controls"),
         term = factor(term, levels=c("latitude_abs_ln",  "polity2_ln", "gdp_per_ln", "tp_gps_ln"))
  ) %>%
  ggplot(aes(y=term  , x=mean_estimate, color = factor(model_1_2))) +
  geom_vline(xintercept = 0, color=c("#525252")) +
  geom_errorbar (aes(y=term , xmin = ci_99_low, xmax = ci_99_high, colour = paste(model_1_2,"99% Confidence Interval")), size = 2,width=0, position=position_dodge(width=0.5))+
  geom_errorbar (aes(y=term , xmin = ci_95_low, xmax = ci_95_high, colour = paste(model_1_2,"95% Confidence Interval")), size = 2,width=0, position=position_dodge(width=0.5))+
  # geom_errorbar (data = sum_table, aes(x=reorder(Model, Estimate),ymin = Conf_Low_90, ymax = Conf_High_90, colour = "90% Confidence Interval"), size = 2,width=0, position=position_dodge(width=0.9))+
  geom_point(aes(shape = "Point Esimate", colour = factor(model_1_2)), shape = 21, fill = "white",
             size = 3.5, position=position_dodge(width=0.5))+
  facet_grid(~family_model, scales = "fixed") +
  theme(legend.position = "bottom",
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
  scale_color_manual(values = c("#1f78b4","#1f78b4","#a6cee3","#33a02c","#33a02c","#b2df8a"))+
  scale_x_continuous(name = "")+
  scale_y_discrete(labels =c('Latitude\n(absolute; log)', 'Democracy \nIndex (log)',
                             'GDP per \nCapita (log)', 'Long-Term Orientation\n(log; Google)'))

google_boot_plot_sub




################################################################################
# 5. sub analysis - public environmental priority 
################################################################################

##################--------------------------------------------------------------
# 5.2 bootstrapped regression models - public environmental priority index 1
##################--------------------------------------------------------------


## public priority (environment vs economic)

### bootstrap data
set.seed(123)
data_boot <- bootstraps(d2_boot_pub1, times = 1000, apparent = FALSE)

### run models
boot_model_1 <- data_boot %>%
  mutate(model = purrr::map(splits, ~ lm(pub_prio_1_ln ~ tp_gps_ln, data = .)),
         coef_info = purrr::map(model, tidy))

boot_model_2 <- data_boot %>%
  mutate(model = purrr::map(splits, ~ lm(pub_prio_1_ln ~ tp_gps_ln + gdp_per_ln +
                                    polity2_ln + latitude_abs_ln, data = .)),
         coef_info = purrr::map(model, tidy))

### get coefficients
boot_coef_1 <- boot_model_1 %>%
  unnest(coef_info)

boot_coef_2 <- boot_model_2 %>%
  unnest(coef_info)

### extract info and summarize
boot_1_pub1_econ <- boot_coef_1 %>% dplyr::select(term, estimate) %>%
  group_by(term) %>%
  summarise(mean_estimate = mean(estimate),
            ci_95_low  = quantile(estimate,probs=c(.025)),
            ci_95_high  = quantile(estimate,probs=c(.975)),
            ci_99_low  = quantile(estimate,probs=c(.005)),
            ci_99_high  = quantile(estimate,probs=c(.995))) %>%
  mutate(info = paste0(round(mean_estimate,2), " [",round(ci_95_low,2),
                       " to ",round(ci_95_high,2),"]")) %>%
  dplyr::rename(model_1_pub1_econ_boot = 7) %>% mutate(model = "model_1_pub1_econ_boot")

boot_2_pub1_econ <- boot_coef_2 %>% dplyr::select(term, estimate) %>%
  group_by(term) %>%
  summarise(mean_estimate = mean(estimate),
            ci_95_low  = quantile(estimate,probs=c(.025)),
            ci_95_high  = quantile(estimate,probs=c(.975)),
            ci_99_low  = quantile(estimate,probs=c(.005)),
            ci_99_high  = quantile(estimate,probs=c(.995))) %>%
  mutate(info = paste0(round(mean_estimate,2), " [",round(ci_95_low,2),
                       " to ",round(ci_95_high,2),"]")) %>%
  dplyr::rename(model_2_pub1_econ_boot = 7) %>% mutate(model = "model_2_pub1_econ_boot")




## public priority ("looking after the environment" )

### bootstrap data
set.seed(123)
data_boot <- bootstraps(d2_boot_pub2, times = 1000, apparent = FALSE)

### run models
boot_model_1 <- data_boot %>%
  mutate(model = purrr::map(splits, ~ lm(pub_prio_2_ln ~ tp_gps_ln, data = .)),
         coef_info = purrr::map(model, tidy))

boot_model_2 <- data_boot %>%
  mutate(model = purrr::map(splits, ~ lm(pub_prio_2_ln ~ tp_gps_ln + gdp_per_ln +
                                    polity2_ln + latitude_abs_ln, data = .)),
         coef_info = purrr::map(model, tidy))

### get coefficients
boot_coef_1 <- boot_model_1 %>%
  unnest(coef_info)

boot_coef_2 <- boot_model_2 %>%
  unnest(coef_info)

### extract info and summarize
boot_1_pub2_econ <- boot_coef_1 %>% dplyr::select(term, estimate) %>%
  group_by(term) %>%
  summarise(mean_estimate = mean(estimate),
            ci_95_low  = quantile(estimate,probs=c(.025)),
            ci_95_high  = quantile(estimate,probs=c(.975)),
            ci_99_low  = quantile(estimate,probs=c(.005)),
            ci_99_high  = quantile(estimate,probs=c(.995))) %>%
  mutate(info = paste0(round(mean_estimate,2), " [",round(ci_95_low,2),
                       " to ",round(ci_95_high,2),"]")) %>%
  dplyr::rename(model_1_pub2_econ_boot = 7) %>% mutate(model = "model_1_pub2_econ_boot")

boot_2_pub2_econ <- boot_coef_2 %>% dplyr::select(term, estimate) %>%
  group_by(term) %>%
  summarise(mean_estimate = mean(estimate),
            ci_95_low  = quantile(estimate,probs=c(.025)),
            ci_95_high  = quantile(estimate,probs=c(.975)),
            ci_99_low  = quantile(estimate,probs=c(.005)),
            ci_99_high  = quantile(estimate,probs=c(.995))) %>%
  mutate(info = paste0(round(mean_estimate,2), " [",round(ci_95_low,2),
                       " to ",round(ci_95_high,2),"]")) %>%
  dplyr::rename(model_2_pub2_econ_boot = 7) %>% mutate(model = "model_2_pub2_econ_boot")





## add outputs together for ggplot
model_summary_pub_boot_plot <- bind_rows(boot_1_pub1_econ %>% dplyr::select(term:ci_99_high,model),
                                         boot_2_pub1_econ %>% dplyr::select(term:ci_99_high,model),
                                         boot_1_pub2_econ %>% dplyr::select(term:ci_99_high,model),
                                         boot_2_pub2_econ %>% dplyr::select(term:ci_99_high,model))


## add outputs together for table
model_summary_pub_boot_econ <-
  full_join(boot_1_pub1_econ %>% dplyr::select(term, contains("_boot")),
            boot_2_pub1_econ %>% dplyr::select(term, contains("_boot")),
            by = "term") %>%
  full_join(boot_1_pub2_econ %>% dplyr::select(term, contains("_boot")),
            by = "term") %>%
  full_join(boot_2_pub2_econ %>% dplyr::select(term, contains("_boot")),
            by = "term")


theme_set(theme_bw()) # set main design
pub_econ_boot_plot <- model_summary_pub_boot_plot %>%
  filter(term != "(Intercept)") %>%
  mutate(
    family_model =
      ifelse(str_detect(model, "pub1"),
             "A) Public Environmental\nPriority Index 1",
             "B) Public Environmental\nPriority Index 2"),
    family_model = factor(family_model, levels=c("A) Public Environmental\nPriority Index 1",
                                                 "B) Public Environmental\nPriority Index 2")),
    model_1_2 =
           ifelse(str_detect(model, "model_1"), "Model without Controls","Model with Controls"),
         term = factor(term, levels=c("latitude_abs_ln",  "polity2_ln", "gdp_per_ln", "tp_gps_ln"))
  ) %>%
  ggplot(aes(y=term  , x=mean_estimate, color = factor(model_1_2))) +
  geom_vline(xintercept = 0, color=c("#525252")) +
  geom_errorbar (aes(y=term , xmin = ci_99_low, xmax = ci_99_high, colour = paste(model_1_2,"99% Confidence Interval")), size = 2,width=0, position=position_dodge(width=0.5))+
  geom_errorbar (aes(y=term , xmin = ci_95_low, xmax = ci_95_high, colour = paste(model_1_2,"95% Confidence Interval")), size = 2,width=0, position=position_dodge(width=0.5))+
  # geom_errorbar (data = sum_table, aes(x=reorder(Model, Estimate),ymin = Conf_Low_90, ymax = Conf_High_90, colour = "90% Confidence Interval"), size = 2,width=0, position=position_dodge(width=0.9))+
  geom_point(aes(shape = "Point Esimate", colour = factor(model_1_2)), shape = 21, fill = "white",
             size = 3.5, position=position_dodge(width=0.5))+
  facet_grid(~family_model, scales = "fixed") +
  theme(legend.position = "bottom",
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
  scale_color_manual(values = c("#1f78b4","#1f78b4","#a6cee3","#33a02c","#33a02c","#b2df8a"))+
  scale_x_continuous(name = "")+
  scale_y_discrete(labels =c('Latitude\n(absolute; log)', 'Democracy \nIndex (log)',
                             'GDP per \nCapita (log)', 'Long-Term Orientation\n(log; economic)'))

pub_econ_boot_plot








##################################################################################################
# 6. save results
##################################################################################################

setwd(cd_results) # set working directory
## main figures
ggsave(figure_2  + ggtitle("Economic Time Preference"), filename = "v1_figure_2.pdf", width=17/4*3*.85, height=8.5/6*4.5*.65)
ggsave(figure_3a + ggtitle("Thrift Time Preference"), filename = "v1_figure_3a.pdf", width=17/4*3*.85, height=8.5/6*4.5*.65)
ggsave(figure_3b + ggtitle("Google Time Preference"), filename = "v1_figure_3b.pdf", width=17/4*3*.85, height=8.5/6*4.5*.65)

## additional figures (not part of the paper)
# ggsave(econ_boot_plot_sub + ggtitle("Economic Time Preference (Sub-Sample)"), filename = "v1_econ_boot_plot_sub.pdf",     width=17/4*3*.85, height=8.5/6*4.5*.65)
# ggsave(thrift_boot_plot_sub+ ggtitle("Thrift Time Preference (Sub-Sample)"), filename = "v1_thrift_boot_plot_sub.pdf",     width=17/4*3*.85, height=8.5/6*4.5*.65)
# ggsave(google_boot_plot_sub + ggtitle("Google Time Preference (Sub-Sample)"), filename = "v1_google_boot_plot_sub.pdf",     width=17/4*3*.85, height=8.5/6*4.5*.65)
# ggsave(pub_econ_boot_plot, filename = "v1_pub_econ_boot_plot.pdf", width=17/4*2*.85, height=8.5/6*4.5*.65)


## result tables
write.csv(model_summary_boot_econ %>%
            mutate(term = factor(term, levels=c("(Intercept)","tp_gps_ln", "gdp_per_ln","polity2_ln" ,"latitude_abs_ln"))) %>%
            arrange(term, desc()), file = "table_s4a_model_summary_boot_econ.csv")
write.csv(model_summary_boot_thrift %>%
            mutate(term = factor(term, levels=c("(Intercept)","tp_hofstede_ln", "gdp_per_ln","polity2_ln" ,"latitude_abs_ln"))) %>%
            arrange(term, desc()), file = "table_s7a_model_summary_boot_thrift.csv")
write.csv(model_summary_boot_google %>%
            mutate(term = factor(term, levels=c("(Intercept)","tp_google_2_ln", "gdp_per_ln","polity2_ln" ,"latitude_abs_ln"))) %>%
            arrange(term, desc()), file = "table_s8a_model_summary_boot_google.csv")
write.csv(model_summary_boot_econ_sub %>%
            mutate(term = factor(term, levels=c("(Intercept)","tp_gps_ln", "gdp_per_ln","polity2_ln" ,"latitude_abs_ln"))) %>%
            arrange(term, desc()), file = "table_s4b_model_summary_boot_econ_sub.csv")
write.csv(model_summary_boot_thrift_sub %>%
            mutate(term = factor(term, levels=c("(Intercept)","tp_hofstede_ln", "gdp_per_ln","polity2_ln" ,"latitude_abs_ln"))) %>%
            arrange(term, desc()), file = "table_s7b_model_summary_boot_thrift_sub.csv")
write.csv(model_summary_boot_google_sub %>%
            mutate(term = factor(term, levels=c("(Intercept)","tp_google_2_ln", "gdp_per_ln","polity2_ln" ,"latitude_abs_ln"))) %>%
            arrange(term, desc()), file = "table_s8b_model_summary_boot_google_sub.csv")
write.csv(model_summary_pub_boot_econ %>%
            mutate(term = factor(term, levels=c("(Intercept)","tp_gps_ln", "gdp_per_ln","polity2_ln" ,"latitude_abs_ln"))) %>%
            arrange(term, desc()), file = "table_s3_model_summary_pub_boot_econ.csv")

