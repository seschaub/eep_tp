################################################################################
# r code related to                                                            #
# "Global Relationships between Time Preference and Environmental Policy       #
# Performance" by Sergei Schaub published in Environmental Science & Policy    #
# -----------------------------------------------------------------------------#
# 2021, ETH Zurich                                                             #
# developed by Sergei Schaub                                                   #
# -----------------------------------------------------------------------------#
# data summary and maps                                                        #
################################################################################


# table of contents:
# 1. load data 
# 2. visualization of data properties
# 3. save summary information



################################################################################
# 1. load data 
################################################################################

# load data
setwd(cd_input_data)
load("epp_tp_data_main.RData")
load("epp_tp_data_availability.RData")
load("epp_tp_data_figure_s9.RData")

################################################################################
# 2. visualization of data properties
################################################################################


# show maximum and minimum values for environmental policy performance
max(d2$gov_epp, na.rm = T)
min(d2$gov_epp, na.rm = T)
max(d2$gov_climate, na.rm = T)
min(d2$gov_climate, na.rm = T)
max(d2$gov_biodiversity, na.rm = T)
min(d2$gov_biodiversity, na.rm = T)


##################--------------------------------------------------------------
# 2.1 data availability overview (before imputation)
##################--------------------------------------------------------------

all_data <- d1 %>% dplyr::select(-c(country_name:polity_code), - tp_google_1, -region, -income, - internet_user)
tp_all_complete <- all_data %>% filter(!is.na(tp_gps),!is.na(tp_google_2),!is.na(tp_hofstede))
tp_gps_complete <- all_data %>% filter(!is.na(tp_gps))
tp_hofstede_complete <- all_data %>% filter(!is.na(tp_hofstede))
tp_google_complete <- all_data %>% filter(!is.na(tp_google_2))

aux_all                   <- inspect_na(all_data) %>% dplyr::select(-cnt) %>%  rename(variable = 1, percent_missing_all_data = 2)
aux_tp_all_complete      <- inspect_na(tp_all_complete) %>% dplyr::select(-cnt) %>%  rename(variable = 1, percent_missing_tp_all_complete = 2)
aux_tp_gps_complete      <- inspect_na(tp_gps_complete) %>% dplyr::select(-cnt) %>%  rename(variable = 1, percent_missing_tp_gps_complete = 2)
aux_tp_hofstede_complete <- inspect_na(tp_hofstede_complete) %>% dplyr::select(-cnt) %>%  rename(variable = 1, percent_missing_tp_hofstede_complete = 2)
aux_tp_google_complete   <- inspect_na(tp_google_complete) %>% dplyr::select(-cnt) %>%  rename(variable = 1, percent_missing_tp_google_complete = 2)

aux <- full_join(aux_all,aux_tp_all_complete, by = "variable") %>%
  full_join(aux_tp_gps_complete, by = "variable") %>%
  full_join(aux_tp_hofstede_complete, by = "variable") %>%
  full_join(aux_tp_google_complete, by = "variable") %>%
  pivot_longer(!variable, names_to = "dataset", values_to = "percent_missing") %>%
  mutate(variable = ordered(variable, levels = c("gov_epp", "gov_climate", "gov_biodiversity",
                                                 "tp_gps", "tp_hofstede", "tp_google_2",
                                                 "gdp_per", "polity2", "latitude", "education","internet_share", "population",
                                                 "trade", "air", "agri_share", "island", "soviet_hist",
                                                 "pub_prio_1_ln","pub_prio_2")))


theme_set(theme_bw()) # set main design
fig_s10 <- aux %>% ggplot(aes( y = reorder(variable, desc(variable)), x = percent_missing/100, color = factor(dataset))) +
  geom_point(size = 4, alpha = 0.6)+
  theme(axis.title = element_text( size=13),
        axis.text  = element_text( size=13),
        legend.text = element_text( size=13),
        legend.title = element_text( size=13),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        legend.position = c(0.73,0.28),
        legend.key.size = unit(1.8, "cm"),
        panel.border = element_rect(colour = "black", fill=NA, size=1))+
  scale_x_continuous(name = "Percent of Not Available-Observations",labels = scales::percent, limits = c(0,1))+
  scale_y_discrete(name = "Variable",
                   labels = c("Public Environmental\nPriority Index 2",
                              "Public Environmental\nPriority Index 1",
                              "Soviet History",
                              "Island",
                              "Agricultural land",
                              "Air pollution",
                              "Trade",
                              "Population",
                              "Internet Users",
                              "Education",
                              "Latitude",
                              "Democracy Index",
                              "GDP per Capita",
                              "Google Time \nPreference",
                              "Thrift Time \nPreference",
                              "Economic Time \nPreference",
                              "Biodiversity Policy\nPerformance",
                              "Climate Policy\nPerformance",
                              "Environmental Policy\nPerformance Overall")) +
  scale_color_viridis(name = "Dataset:", discrete = TRUE, option = "viridis",
                      labels = c('All Observations',
                                 'Observations with all Time\nPreference Indices Available',
                                 'Observations with at least\nEconomic Time Preference \nAvailable',
                                 'Observations with at least\nThrift Time Preference \nAvailable',
                                 'Observations with at least\nGoogle Time Preference \nAvailable'))

fig_s10



##################--------------------------------------------------------------
# 2.2 correlation table
##################--------------------------------------------------------------


cor_over_spearman <- matrix(data = (""), ncol =4, nrow = 4)
cor_over_pearson <- matrix(data = (""), ncol =4, nrow = 4)
diag(cor_over_spearman) <- 1
diag(cor_over_pearson) <- 1

# correlations between different indicators
## correlation economic and thrift time preference
cor_over_pearson[2,1] <- paste0(round((cor.test(d2$tp_gps, d2$tp_hofstede, method=c("pearson")))$estimate,digits = 2),
                                ifelse((cor.test(d2$tp_gps, d2$tp_hofstede, method=c("pearson")))$p.value<0.01,"***",
                                       ifelse((cor.test(d2$tp_gps, d2$tp_hofstede, method=c("pearson")))$p.value<0.05,"**",
                                              ifelse((cor.test(d2$tp_gps, d2$tp_hofstede, method=c("pearson")))$p.value<0.1,"*",""))),
                                " (N = ",as.numeric((cor.test(d2$tp_gps, d2$tp_hofstede, method=c("pearson")))$parameter)+2,")")
cor_over_spearman[2,1] <- paste0(round((cor.test(d2$tp_gps, d2$tp_hofstede, method=c("spearman")))$estimate,digits = 2),
                                 ifelse((cor.test(d2$tp_gps, d2$tp_hofstede, method=c("spearman")))$p.value<0.01,"***",
                                        ifelse((cor.test(d2$tp_gps, d2$tp_hofstede, method=c("spearman")))$p.value<0.05,"**",
                                               ifelse((cor.test(d2$tp_gps, d2$tp_hofstede, method=c("spearman")))$p.value<0.1,"*",""))),
                                 " (N = ",as.numeric((cor.test(d2$tp_gps, d2$tp_hofstede, method=c("pearson")))$parameter)+2,")")


## correlation economic and google time preference (all and last three years)
cor_over_pearson[3,1] <- paste0(round((cor.test(d2$tp_gps, d2$tp_google_1, method=c("pearson")))$estimate,digits = 2),
                                ifelse((cor.test(d2$tp_gps, d2$tp_google_1, method=c("pearson")))$p.value<0.01,"***",
                                       ifelse((cor.test(d2$tp_gps, d2$tp_google_1, method=c("pearson")))$p.value<0.05,"**",
                                              ifelse((cor.test(d2$tp_gps, d2$tp_google_1, method=c("pearson")))$p.value<0.1,"*",""))),
                                " (N = ",as.numeric((cor.test(d2$tp_gps, d2$tp_google_1, method=c("pearson")))$parameter)+2,")")
cor_over_spearman[3,1] <- paste0(round((cor.test(d2$tp_gps, d2$tp_google_1, method=c("spearman")))$estimate,digits = 2),
                                 ifelse((cor.test(d2$tp_gps, d2$tp_google_1, method=c("spearman")))$p.value<0.01,"***",
                                        ifelse((cor.test(d2$tp_gps, d2$tp_google_1, method=c("spearman")))$p.value<0.05,"**",
                                               ifelse((cor.test(d2$tp_gps, d2$tp_google_1, method=c("spearman")))$p.value<0.1,"*",""))),
                                 " (N = ",as.numeric((cor.test(d2$tp_gps, d2$tp_google_1, method=c("pearson")))$parameter)+2,")")
cor_over_pearson[4,1] <- paste0(round((cor.test(d2$tp_gps, d2$tp_google_2, method=c("pearson")))$estimate,digits = 2),
                                ifelse((cor.test(d2$tp_gps, d2$tp_google_2, method=c("pearson")))$p.value<0.01,"***",
                                       ifelse((cor.test(d2$tp_gps, d2$tp_google_2, method=c("pearson")))$p.value<0.05,"**",
                                              ifelse((cor.test(d2$tp_gps, d2$tp_google_2, method=c("pearson")))$p.value<0.1,"*",""))),
                                " (N = ",as.numeric((cor.test(d2$tp_gps, d2$tp_google_2, method=c("pearson")))$parameter)+2,")")
cor_over_spearman[4,1] <- paste0(round((cor.test(d2$tp_gps, d2$tp_google_2, method=c("spearman")))$estimate,digits = 2),
                                 ifelse((cor.test(d2$tp_gps, d2$tp_google_2, method=c("spearman")))$p.value<0.01,"***",
                                        ifelse((cor.test(d2$tp_gps, d2$tp_google_2, method=c("spearman")))$p.value<0.05,"**",
                                               ifelse((cor.test(d2$tp_gps, d2$tp_google_2, method=c("spearman")))$p.value<0.1,"*",""))),
                                 " (N = ",as.numeric((cor.test(d2$tp_gps, d2$tp_google_2, method=c("pearson")))$parameter)+2,")")


## correlation thrift and google time preference (all and last three years)
cor_over_pearson[3,2] <- paste0(round((cor.test(d2$tp_hofstede, d2$tp_google_1, method=c("pearson")))$estimate,digits = 2),
                                ifelse((cor.test(d2$tp_hofstede, d2$tp_google_1, method=c("pearson")))$p.value<0.01,"***",
                                       ifelse((cor.test(d2$tp_hofstede, d2$tp_google_1, method=c("pearson")))$p.value<0.05,"**",
                                              ifelse((cor.test(d2$tp_hofstede, d2$tp_google_1, method=c("pearson")))$p.value<0.1,"*",""))),
                                " (N = ",as.numeric((cor.test(d2$tp_hofstede, d2$tp_google_1, method=c("pearson")))$parameter)+2,")")
cor_over_spearman[3,2] <- paste0(round((cor.test(d2$tp_hofstede, d2$tp_google_1, method=c("spearman")))$estimate,digits = 2),
                                 ifelse((cor.test(d2$tp_hofstede, d2$tp_google_1, method=c("spearman")))$p.value<0.01,"***",
                                        ifelse((cor.test(d2$tp_hofstede, d2$tp_google_1, method=c("spearman")))$p.value<0.05,"**",
                                               ifelse((cor.test(d2$tp_hofstede, d2$tp_google_1, method=c("spearman")))$p.value<0.1,"*",""))),
                                 " (N = ",as.numeric((cor.test(d2$tp_hofstede, d2$tp_google_1, method=c("pearson")))$parameter)+2,")")
cor_over_pearson[4,2] <- paste0(round((cor.test(d2$tp_hofstede, d2$tp_google_2, method=c("pearson")))$estimate,digits = 2),
                                ifelse((cor.test(d2$tp_hofstede, d2$tp_google_2, method=c("pearson")))$p.value<0.01,"***",
                                       ifelse((cor.test(d2$tp_hofstede, d2$tp_google_2, method=c("pearson")))$p.value<0.05,"**",
                                              ifelse((cor.test(d2$tp_hofstede, d2$tp_google_2, method=c("pearson")))$p.value<0.1,"*",""))),
                                " (N = ",as.numeric((cor.test(d2$tp_hofstede, d2$tp_google_2, method=c("pearson")))$parameter)+2,")")
cor_over_spearman[4,2] <- paste0(round((cor.test(d2$tp_hofstede, d2$tp_google_2, method=c("spearman")))$estimate,digits = 2),
                                 ifelse((cor.test(d2$tp_hofstede, d2$tp_google_2, method=c("spearman")))$p.value<0.01,"***",
                                        ifelse((cor.test(d2$tp_hofstede, d2$tp_google_2, method=c("spearman")))$p.value<0.05,"**",
                                               ifelse((cor.test(d2$tp_hofstede, d2$tp_google_2, method=c("spearman")))$p.value<0.1,"*",""))),
                                 " (N = ",as.numeric((cor.test(d2$tp_hofstede, d2$tp_google_2, method=c("pearson")))$parameter)+2,")")



## correlation google time preference (all) and google time preference (last three years)
cor_over_pearson[4,3] <- paste0(round((cor.test(d2$tp_google_1, d2$tp_google_2, method=c("pearson")))$estimate,digits = 2),
                                ifelse((cor.test(d2$tp_google_1, d2$tp_google_2, method=c("pearson")))$p.value<0.01,"***",
                                       ifelse((cor.test(d2$tp_google_1, d2$tp_google_2, method=c("pearson")))$p.value<0.05,"**",
                                              ifelse((cor.test(d2$tp_google_1, d2$tp_google_2, method=c("pearson")))$p.value<0.1,"*",""))),
                                " (N = ",as.numeric((cor.test(d2$tp_google_1, d2$tp_google_2, method=c("pearson")))$parameter)+2,")")
cor_over_spearman[4,3] <- paste0(round((cor.test(d2$tp_google_1, d2$tp_google_2, method=c("spearman")))$estimate,digits = 2),
                                 ifelse((cor.test(d2$tp_google_1, d2$tp_google_2, method=c("spearman")))$p.value<0.01,"***",
                                        ifelse((cor.test(d2$tp_google_1, d2$tp_google_2, method=c("spearman")))$p.value<0.05,"**",
                                               ifelse((cor.test(d2$tp_google_1, d2$tp_google_2, method=c("spearman")))$p.value<0.1,"*",""))),
                                 " (N = ",as.numeric((cor.test(d2$tp_google_1, d2$tp_google_2, method=c("pearson")))$parameter)+2,")")




rownames(cor_over_pearson) <- c("GPS","Hofstede","tp Google (2012-2018)","tp Google (2016-2018)")
colnames(cor_over_pearson) <- c("GPS","Hofstede","tp Google (2012-2018)","tp Google (2016-2018)")
rownames(cor_over_spearman) <- c("GPS","Hofstede","tp Google (2012-2018)","tp Google (2016-2018)")
colnames(cor_over_spearman) <- c("GPS","Hofstede","tp Google (2012-2018)","tp Google (2016-2018)")

# note to also report N or degrees of freedom when reporting the correlations



##################--------------------------------------------------------------
# 2.3 create maps
##################--------------------------------------------------------------


world_map <- map_data("world") # get map data


# add country code (for matching) to map data:
world_map_test <- data.frame(world_map$region) %>% distinct()
colnames(world_map_test) <- "region"



world_map_test$countryCode <- countrycode(world_map_test$region, origin = 'country.name', destination = 'iso2c')
world_map_test <- world_map_test %>%
  mutate(countryCode = tolower(countryCode),
         countryCode = ifelse(region == "Barbuda", "ag", countryCode),
         countryCode = ifelse(region == "Canary Islands", "ic", countryCode),
         countryCode = ifelse(region == "Micronesia", "fm", countryCode),
         countryCode = ifelse(region == "Heard Island", "hm", countryCode),
         countryCode = ifelse(region == "Chagos Archipelago", "io", countryCode),
         countryCode = ifelse(region == "Kosovo", "xk", countryCode),
         countryCode = ifelse(region == "Saint Martin", "mf", countryCode),
         countryCode = ifelse(region == "Bonaire", "bq", countryCode),
         countryCode = ifelse(region == "Sint Eustatius", "bq", countryCode),
         countryCode = ifelse(region == "Madeira Islands", "pt", countryCode),
         countryCode = ifelse(region == "Saba", "bq", countryCode),
         countryCode = ifelse(region == "Azores", "pt", countryCode),
         countryCode = ifelse(region == "Ascension Island", "sh", countryCode),
         countryCode = ifelse(region == "Grenadines", "vc", countryCode),
         countryCode = ifelse(region == "Virgin Islands", "vc", countryCode))



world_map <- left_join(world_map,world_map_test, by = "region") %>%
  mutate(countryCode = ifelse(subregion == "Hong Kong","hk",countryCode),
         countryCode = ifelse(subregion == "Macao","mo",countryCode))
world_map <- left_join(world_map,world_map_test, by = "region") %>%
  dplyr::select(long:group,region,countryCode.y) %>% rename(countryCode = 5)


checking <- anti_join(world_map, d2, by = c("countryCode" = "iso2c")) %>% dplyr::select(region,countryCode) %>% distinct() # join twitter and map data
checking <- anti_join(d2, world_map, by = c("iso2c"="countryCode")) # join twitter and map data


d2_map_with_gps <- left_join(world_map,
                    d2 %>% dplyr::select(iso2c,pub_prio_2:tp_google_2) %>% filter(!is.na(tp_gps)),
                    by = c("countryCode" = "iso2c")) # join twitter and map data

d2_map_with_all <- left_join(world_map,
                    d2 %>% dplyr::select(iso2c,pub_prio_2:tp_google_2) ,
                    by = c("countryCode" = "iso2c")) # join twitter and map data


#-------------------------------------------------------------------------------
# 2.3.1 maps on environmental policy performance  
#-------------------------------------------------------------------------------

map_gov1 <- 
  left_join(world_map,
            d2 %>%
              filter(!is.na(gov_epp_ln),
                     !is.na(tp_gps_ln) | !is.na(tp_hofstede) | !is.na(tp_google_2),
                     !is.na(gdp_per_ln),
                     !is.na(polity2_ln),
                     !is.na(latitude_abs_ln)) %>%
              dplyr::select(iso2c,gov_epp) ,
          by = c("countryCode" = "iso2c")) %>%
  mutate(gov_epp_scale = scale(gov_epp)) %>%
  ggplot(aes(long, lat, group = group))+
  geom_polygon(aes(fill = gov_epp_scale ),
               color = "white", size=0.001)+
  scale_fill_viridis(na.value = "grey80", name = "Standardized\nValue:",option = "viridis",direction = -1)+
  theme_map()+ coord_fixed() +
  theme(title  = element_text( size=15),
        legend.text = element_text(size=15),
        legend.title = element_text(size=15))+
  labs(title = "D) Environmental Policy Performance Overall")
#map_gov1

map_gov2 <-
  left_join(world_map,
            d2 %>%
              filter(!is.na(gov_climate),
                     !is.na(tp_gps_ln) | !is.na(tp_hofstede) | !is.na(tp_google_2),
                     !is.na(gdp_per_ln),
                     !is.na(polity2_ln),
                     !is.na(latitude_abs_ln)) %>%
              dplyr::select(iso2c,gov_climate) ,
            by = c("countryCode" = "iso2c")) %>%
  mutate(gov_climate_scale = scale(gov_climate)) %>%
  ggplot(aes(long, lat, group = group))+
  geom_polygon(aes(fill = gov_climate_scale),
               color = "white", size=0.001)+
  scale_fill_viridis(na.value = "grey80", name = "Standardized\nValue:",option = "viridis",direction = -1)+
  theme_map()+ coord_fixed() +
  theme(title  = element_text( size=15),
        legend.text = element_text(size=15),
        legend.title = element_text(size=15))+
  labs(title = "E) Climate Policy Performance")
#map_gov2

map_gov3 <-
  left_join(world_map,
            d2 %>%
              filter(!is.na(gov_biodiversity),
                     !is.na(tp_gps_ln) | !is.na(tp_hofstede) | !is.na(tp_google_2),
                     !is.na(gdp_per_ln),
                     !is.na(polity2_ln),
                     !is.na(latitude_abs_ln)) %>%
              dplyr::select(iso2c,gov_biodiversity) ,
            by = c("countryCode" = "iso2c")) %>%
  mutate(gov_biodiversity_scale = scale(gov_biodiversity)) %>%
  ggplot(aes(long, lat, group = group))+
  geom_polygon(aes(fill = gov_biodiversity_scale ),
               color = "white", size=0.001)+
  scale_fill_viridis(na.value = "grey80", name = "Standardized\nValue:",option = "viridis",direction = -1)+
  theme_map()+ coord_fixed() +
  theme(title  = element_text( size=15),
        legend.text = element_text(size=15),
        legend.title = element_text(size=15))+
  labs(title = "F) Biodiversity Policy Performance")
#map_gov3


#-------------------------------------------------------------------------------
# 2.3.2 maps on time preference
#-------------------------------------------------------------------------------

map_tp_gps <- 
  left_join(world_map,
            d2 %>%
              filter(!is.na(gov_epp_ln),
                     !is.na(tp_gps_ln),
                     !is.na(gdp_per_ln),
                     !is.na(polity2_ln),
                     !is.na(latitude_abs_ln)) %>%
              dplyr::select(iso2c,tp_gps) ,
            by = c("countryCode" = "iso2c")) %>%
  mutate(gov_epp_scale = scale(tp_gps)) %>%
  ggplot(aes(long, lat, group = group))+
  geom_polygon(aes(fill = gov_epp_scale ),
               color = "white", size=0.001)+
  scale_fill_viridis(na.value = "grey80", name = "Standardized\nValue:",option = "inferno",direction = -1)+
  theme_map()+ coord_fixed() +
  theme(title  = element_text( size=15),
        legend.text = element_text(size=15),
        legend.title = element_text(size=15))+
  labs(title = "A) Economic Time Prefernce")
#map_tp_gps


map_tp_hofstede <- 
  left_join(world_map,
            d2 %>%
              filter(!is.na(gov_epp_ln),
                     !is.na(tp_hofstede_ln),
                     !is.na(gdp_per_ln),
                     !is.na(polity2_ln),
                     !is.na(latitude_abs_ln)) %>%
              dplyr::select(iso2c,tp_hofstede) ,
            by = c("countryCode" = "iso2c")) %>%
  mutate(gov_epp_scale = scale(tp_hofstede)) %>%
  ggplot(aes(long, lat, group = group))+
  geom_polygon(aes(fill = gov_epp_scale ),
               color = "white", size=0.001)+
  scale_fill_viridis(na.value = "grey80", name = "Standardized\nValue:",option = "inferno",direction = -1)+
  theme_map()+ coord_fixed() +
  theme(title  = element_text( size=15),
        legend.text = element_text(size=15),
        legend.title = element_text(size=15))+
  labs(title = "B) Thrift Time Prefernce")
#map_tp_hofstede


map_tp_google <- 
  left_join(world_map,
            d2 %>%
              filter(!is.na(gov_epp_ln),
                     !is.na(tp_google_2_ln),
                     !is.na(gdp_per_ln),
                     !is.na(polity2_ln),
                     !is.na(latitude_abs_ln)) %>%
              dplyr::select(iso2c,tp_google_2) ,
            by = c("countryCode" = "iso2c")) %>%
  mutate(gov_epp_scale = scale(tp_google_2)) %>%
  ggplot(aes(long, lat, group = group))+
  geom_polygon(aes(fill = gov_epp_scale ),
               color = "white", size=0.001)+
  scale_fill_viridis(na.value = "grey80", name = "Standardized\nValue:",option = "inferno",direction = -1)+
  theme_map()+ coord_fixed() +
  theme(title  = element_text( size=15),
        legend.text = element_text(size=15),
        legend.title = element_text(size=15))+
  labs(title = "C) Google Time Prefernce")
#map_tp_google


#-------------------------------------------------------------------------------
# 2.3.3 maps on public environmental priority
#-------------------------------------------------------------------------------

map_pop1 <- 
  left_join(world_map,
            d2 %>%
              filter(!is.na(pub_prio_1_ln),
                     !is.na(tp_gps_ln) | !is.na(tp_hofstede) | !is.na(tp_google_2),
                     !is.na(gdp_per_ln),
                     !is.na(polity2_ln),
                     !is.na(latitude_abs_ln)) %>%
              dplyr::select(iso2c,pub_prio_1_ln) ,
            by = c("countryCode" = "iso2c")) %>%
  mutate(gov_epp_scale = scale(exp(pub_prio_1_ln))) %>%
  ggplot(aes(long, lat, group = group))+
  geom_polygon(aes(fill = gov_epp_scale ),
               color = "white", size=0.001)+
  scale_fill_scico(na.value = "grey80", name = "Standardized\nValue:", palette  = "roma",direction = -1)+
  theme_map()+ coord_fixed() +
  theme(title  = element_text( size=15),
        legend.text = element_text(size=15),
        legend.title = element_text(size=15))+
  labs(title = "A) Public Environmental Priority Index 1")
#map_pop1


map_pop2 <- 
  left_join(world_map,
            d2 %>%
              filter(!is.na(pub_prio_2_ln),
                     !is.na(tp_gps_ln) | !is.na(tp_hofstede) | !is.na(tp_google_2),
                     !is.na(gdp_per_ln),
                     !is.na(polity2_ln),
                     !is.na(latitude_abs_ln)) %>%
              dplyr::select(iso2c,pub_prio_2) ,
            by = c("countryCode" = "iso2c")) %>%
  mutate(gov_epp_scale = scale(pub_prio_2)) %>%
  ggplot(aes(long, lat, group = group))+
  geom_polygon(aes(fill = gov_epp_scale ),
               color = "white", size=0.001)+
  scale_fill_scico(na.value = "grey80", name = "Standardized\nValue:", palette  = "roma",direction = -1)+
  theme_map()+ coord_fixed() +
  theme(title  = element_text( size=15),
        legend.text = element_text(size=15),
        legend.title = element_text(size=15))+
  labs(title = "B) Public Environmental Priority Index 2")
#map_pop2


#-------------------------------------------------------------------------------
# 4.2.4 combine
#-------------------------------------------------------------------------------

# figure 1 - spatial distribution of long-term orientation and environmental policy performance
figure_1 <-
  ggarrange(map_tp_gps,map_tp_hofstede,map_tp_google,
            map_gov1, map_gov2, map_gov3,
            ncol=2, nrow=3, align = "hv")
# figure_1


# figure s9 - spatial distribution of public environmental priority
figure_s9 <-
  ggarrange(map_pop1,map_pop2,
            ncol=2, nrow=1, align = "hv")
# figure_s9


##################--------------------------------------------------------------
# 2.4 correlation matrix (using dataset of all variables)
##################--------------------------------------------------------------

data_for_cor <- d2 %>% filter(!is.na(tp_gps),!is.na(tp_google_2),!is.na(tp_hofstede)) %>%
  mutate(latitude_abs = abs(latitude),
         education = as.numeric(as.character(education))) %>%
  dplyr::select(gov_epp,gov_climate,gov_biodiversity,tp_gps:gdp_per,
                polity2,latitude_abs,education,internet_share,population,trade,air,agri_share,soviet_hist,-tp_google_1,
                pub_prio_2, pub_prio_1_ln)  %>%
  mutate(pub_prio_2 = log(pub_prio_2 + 1),
         gov_epp = log(gov_epp),
         gov_climate = log(gov_climate),
         gov_biodiversity = log(gov_biodiversity),
         tp_gps = log(tp_gps+1),
         tp_hofstede = log(tp_hofstede),
         tp_google_2 = log(tp_google_2),
         gdp_per = log(gdp_per),
         polity2 = log(polity2+11),
         latitude_abs = log(latitude_abs),
         education = log(education),
         internet_share = log(internet_share),
         population = log(population),
         trade = log(trade),
         air = log(air),
         agri_share = log(agri_share)
         ) %>%
  rename(`Environmental Policy\nPerformance Overall` = 1,
         `Climate Policy\nPerformance` = 2,
         `Biodiversity Policy\nPerformance` = 3,
         `Economic Time \nPreference`  = 4,
         `Thrift Time \nPreference` = 5,
         `Google Time \nPreference` = 6,
         `GDP per Capita`  = 7,
         `Democracy Index`  = 8,
         `Latitude (absolute)`  = 9,
         `Education`  = 10,
         `Internet Users`  = 11,
         `Population`  = 12,
         `Trade`  = 13,
         `Air pollution`  = 14,
         `Agricultural land`  = 15,
         `Soviet History`  = 16,
         `Public Environmental\nPriority Index 1` = 17,
         `Public Environmental\nPriority Index 2` = 18
         )



M <- cor(data_for_cor %>% mutate(`Soviet History` = as.numeric(as.character(`Soviet History`))), method = "pearson", use = "pairwise.complete.obs")
res1 <- cor.mtest(data_for_cor %>% mutate(`Soviet History` = as.numeric(as.character(`Soviet History`))), conf.level = .95)


col1 <- viridis(256, option = "D", direction = -1)



##################--------------------------------------------------------------
# 2.5 data availability 2
##################--------------------------------------------------------------

# economic time preference 
d2_boot_econ <- d2 %>%
  filter(!is.na(gov_epp_ln),
         !is.na(gov_climate_ln),
         !is.na(gov_biodiversity_ln),
         !is.na(tp_gps_ln),
         !is.na(gdp_per_ln),
         !is.na(polity2_ln),
         !is.na(latitude_abs_ln))

# google time preference 
d2_boot_google <- d2 %>%
  filter(!is.na(gov_epp_ln),
         !is.na(gov_climate_ln),
         !is.na(gov_biodiversity_ln),
         !is.na(tp_google_2_ln),
         !is.na(gdp_per_ln),
         !is.na(polity2_ln),
         !is.na(latitude_abs_ln))

# subset for which all time preferences are available
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

d2_not_in_sub <- inner_join(d2_boot_google,d2_boot_sub, by = "iso3c") %>% 
  mutate(Available_aux_1 = 1)

d2_in_econ <- inner_join(d2_boot_google,d2_boot_econ, by = "iso3c") %>% 
  mutate(Available_aux_2 = 1)

d2_boot_google_avail <- 
  d2_boot_google %>% dplyr::select(iso3c,iso2c) %>% 
  left_join(d2_not_in_sub %>% dplyr::select(iso3c, Available_aux_1), by = "iso3c") %>% 
  left_join(d2_in_econ %>% dplyr::select(iso3c, Available_aux_2), by = "iso3c") %>% 
  mutate(Available = ifelse(Available_aux_2 == 1 & is.na(Available_aux_1) , "For Economic and Google\nTime Preference",
                            ifelse(Available_aux_1 == 1, "For all Time\nPreference","x")),
         Available = ifelse(is.na(Available), "For Google Time\nPreference only",Available),
         
  )%>%
  dplyr::select(iso3c,iso2c,Available)


figure_s7 <- 
  left_join(world_map,
            d2_boot_google_avail  ,
            by = c("countryCode" = "iso2c")) %>%
  ggplot(aes(long, lat, group = group))+
  geom_polygon(aes(fill = Available ),
               color = "white", size=0.001)+
  scale_fill_manual(na.value = "grey80", name = "Data Available for:", values = c("#a6cee3", "#1f78b4", "#b2df8a"))+
  theme_map()+ coord_fixed() +
  theme(title  = element_text( size=15),
        legend.text = element_text(size=15),
        legend.title = element_text(size=15))
# figure_s7


################################################################################
# 2.6 google time preference of single years
################################################################################

theme_set(theme_bw()) # set main design
figure_s1 <-ggplot(f3_aux2, aes(x=year)) +
  geom_line(aes(y=val_norm, color=country_name), size =0.7, alpha = 0.5) +
  theme(legend.position = "none",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title = element_text( size=15),
        axis.text  = element_text( size=15))+
  scale_y_continuous(name = "Google Time Preference\n(normalized to 2016)",
                     limits = c(0,3.5), breaks = c(seq(0,3.5,0.5)))+
  scale_x_continuous(name = "Year", breaks = c(seq(2012,2018,1)))+
  scale_color_viridis(discrete =T)
# figure_s1


################################################################################
# 3. save summary information
################################################################################

# set working directory
setwd(cd_results)


# save
## data availability figure
ggsave(fig_s10, filename = "v1_fig_s10.pdf", width=10, height=8.5)

## correlation tables
write.csv(cor_over_pearson, file = "table_s2a.csv")
write.csv(cor_over_spearman, file = "table_s2b.csv")

## maps
ggsave(figure_1, filename = "v1_figure_1.pdf", width=21, height=17.535)
ggsave(figure_s9, filename = "v1_figure_s9.pdf", width=21, height=17.535/3)
ggsave(figure_s7, filename = "v1_figure_s7.pdf", width=21/2, height=17.535/3)
pdf(file = "v1_figure_s11.pdf", width=13, height=12)

corrplot(M, p.mat = res1$p, method = "color", type = "upper",
         sig.level = c(.01, .05, .1), pch.cex = .9,
         insig = "label_sig", pch.col = "black",
         tl.col = "black", col = col1)

dev.off()

# data properties
write.csv(d2_not_in_sub_x, file = "table_s6_data_avail_list_country.csv")
ggsave(figure_s1, filename = "v1_figure_s1.pdf",     width=17, height=8.5)