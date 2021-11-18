################################################################################
# r code related to                                                            #
# "Global Relationships between Time Preference and Environmental Policy       #
# Performance" by Sergei Schaub published in Environmental Science & Policy    #
# -----------------------------------------------------------------------------#
# 2021, ETH Zurich                                                             #
# developed by Sergei Schaub                                                   #
# -----------------------------------------------------------------------------#
# data preparation                                                             #
################################################################################


# table of contents:
# 1. country names and country codes
# 2. environmental policy performance
# 3. public environmental priority
# 4. time preference
# 5. macro data
# 6. combine, impute, clean, and save data


################################################################################
# 1. country names and country codes
################################################################################

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



################################################################################
# 2. environmental policy performance
################################################################################

# data is taken from wendling et al. (2020)

# read data
setwd(cd_input_data) # set working directory
p1 <- read.csv("epi_data.csv", stringsAsFactors = F)  %>%
  mutate(EPI = EPI.new,
         BDH = BDH.new,
         CCH = CCH.new) %>%
  dplyr::select(iso,country,EPI,BDH,CCH) %>%         # delete redundant information
  mutate(geo = tolower(iso))

# add country code information
p1 <- left_join(p1,code_info %>% mutate(geo = iso3c),by = "geo") %>%
  mutate(country_name = ifelse(country == "Micronesia", "Micronesia", country_name),
         iso2c = ifelse(country == "Micronesia", "fm", iso2c),
         iso3c = ifelse(country == "Micronesia", "fsm", iso3c)) %>%
  dplyr::select(-geo,-iso,-country)


################################################################################
# 3. public environmental priority
################################################################################

# data is taken from evs/wvs 2020 and milfont and schultz 2016

# read, extract, and prepare public environmental priority index 1 data
setwd(cd_input_data)
load("WVSEVS.Rdata")
WVS_Wave_joint <- fixed
WVS_Wave_joint_1  <-
  bind_cols(WVS_Wave_joint$cntry_AN %>% as.matrix() %>%  as.data.frame() %>%
              dplyr::rename(code = 1) %>% mutate(code = tolower(code)),
            WVS_Wave_joint$B008 %>% as.matrix() %>%  as.data.frame() %>%
              dplyr::rename(env_vs_econ = 1)) %>%
  filter(!is.na(env_vs_econ), env_vs_econ != 3) %>%
  group_by(code) %>%
  mutate(N = n()) %>%
  group_by(code, env_vs_econ) %>%
  mutate(N_env_vs_econ = n()) %>% ungroup() %>%
  distinct() %>%
  filter(env_vs_econ == 1) %>%
  mutate(N_env_vs_econ_share_env_ln = log(N_env_vs_econ/ N)) %>%
  dplyr::select(code, N_env_vs_econ_share_env_ln)

# add country code information
WVS_Wave_joint_1 <- left_join(WVS_Wave_joint_1,code_info, by = c("code" = "iso2c"))


# read, extract, and prepare public environmental priority index 2 data
e1 <-
  tribble(
    ~'country_name', ~'overall_mean',
    'Algeria', -0.09,
    'Andorra', 1.03,
    'Argentina', 0.66,
    'Armenia', 0.32,
    'Australia', 0.84,
    'Azerbaijan', 0.05,
    'Bahrain', -0.13,
    'Belarus', 0.24,
    'Brazil', 0.87,
    'Bulgaria', 0.58,
    'Burkina Faso', 0.28,
    'Canada', 0.95,
    'Chile', 0.19,
    'China', 0.60,
    'Colombia', 0.90,
    'Cyprus', 0.47,
    'Ecuador', 0.37,
    'Egypt', 0.41,
    'Estonia', 0.55,
    'Ethiopia', 0.35,
    'Finland', 1.06,
    'France', 0.63,
    'Georgia', 0.84,
    'Germany', 0.36,
    'Ghana', 0.32,
    'Great Britain', 0.73,
    'Hong Kong', 0.52,
    'Hungary', 0.89,
    'India', 0.24,
    'Indonesia', 0.73,
    'Iran', 0.61,
    'Iraq', 0.21,
    'Japan', 0.76,
    'Jordan', 0.33,
    'Kazakhstan', 0.16,
    'Kuwait', 0.02,
    'Kyrgyzstan', 0.12,
    'Lebanon', 0.05,
    'Libya', 0.55,
    'Malaysia', 0.45,
    'Mali', 0.28,
    'Mexico', 0.87,
    'Moldova', 0.49,
    'Morocco', 0.35,
    'Netherlands', 0.67,
    'New Zealand', 0.74,
    'Nigeria', 0.02,
    'Norway', 0.86,
    'Pakistan', -0.15,
    'Palestine', 0.26,
    'Peru', 0.64,
    'Philippines', 0.57,
    'Poland', 0.69,
    'Qatar', 0.56,
    'Romania', 0.68,
    'Russia', 0.24,
    'Rwanda', 0.13,
    'Serbia', 0.32,
    'Montenegro', 0.32,
    'Singapore', 0.06,
    'Slovenia', 0.99,
    'South Africa', -0.10,
    'South Korea', 0.26,
    'Spain', 0.67,
    'Sweden', 0.73,
    'Switzerland', 1.13,
    'Taiwan', 0.65,
    'Thailand', 0.38,
    'Trinidad and Tobago', 0.64,
    'Tunisia', -0.52,
    'Turkey', 0.43,
    'Ukraine', 0.25,
    'United States', 0.42,
    'Uruguay', 0.74,
    'Uzbekistan', 0.83,
    'Viet Nam', 0.47,
    'Yemen', 0.52,
    'Zambia', -0.05,
    'Zimbabwe', 0.01)

# add country code information
e1 <- left_join(e1 %>%
                         mutate(country_name =
                                  case_when(country_name == "Great Britain" ~ "United Kingdom",
                                            country_name == "Hong Kong" ~ "Hong Kong SAR China",
                                            country_name == "Palestine" ~ "Palestinian Territories",
                                            country_name == "Trinidad and Tobago" ~ "Trinidad & Tobago",
                                            country_name == "Viet Nam" ~ "Vietnam",
                                            TRUE ~ country_name)),code_info, by = "country_name")




################################################################################
# 4. time preference
################################################################################

##################--------------------------------------------------------------
# 4.1 economic time preference
##################--------------------------------------------------------------

# data is taken from falk et al. 2018

# read, extract, and prepare economic time preference data
k1 <- read.csv2("falk_et_al_data.csv", stringsAsFactors = F)  %>%
  mutate(geo = tolower(isocode),
         patience = as.numeric(as.character(patience))) %>%
  dplyr::select(geo,country,patience)   # delete redundant information

# add country code information
k1 <- left_join(k1,code_info %>% mutate(geo = iso3c),by = "geo") %>%
  dplyr::select(-geo,-country)


##################--------------------------------------------------------------
# 3.2 thrift time preference
##################--------------------------------------------------------------

# data is taken from hofstede 2001

# read, extract, and prepare thrift time preference data
setwd(cd_input_data) # set working directory
h1 <- read.csv("hofstede_data.csv", stringsAsFactors = F) %>%
  mutate(geo = tolower(ctr),
         ltowvs = as.numeric(as.character(ltowvs)),
         name = tolower(country)) %>%
  dplyr::select(geo,country,name,ltowvs) %>%
  filter(!is.na(ltowvs),
         country != "Africa East" & country !=  "Africa West" & country != "Germany East" & country !=  "Arab countries") # delete aggregates

# add country code information
h1 <- left_join(h1,code_info %>% mutate(geo = iso3c),by = "geo")
h1 <- left_join(h1,code_info%>% mutate(country = country_name),by = "country") %>%
  mutate(country_name = ifelse(!is.na(country_name.x),country_name.x,country_name.y),
         iso2c = ifelse(!is.na(iso2c.x),iso2c.x,iso2c.y),
         iso3c = ifelse(!is.na(iso2c.x),iso3c.x,iso3c.y),
         wb_code = ifelse(!is.na(wb_code.x),wb_code.x,wb_code.y),
         polity_code = ifelse(!is.na(polity_code.x),polity_code.x,polity_code.y)) %>%
  dplyr::select(-c(country_name.x:polity_code.y)) %>%
  mutate(country_name = ifelse(name == "bosnia", "Bosnia & Herzegovina", country_name),
         iso2c = ifelse(name == "bosnia", "ba", iso2c),
         iso3c = ifelse(name == "bosnia", "bih", iso3c),
         wb_code = ifelse(name == "bosnia", "bih", wb_code),
         polity_code = ifelse(name == "bosnia", "bos", polity_code),

         country_name = ifelse(name == "hong kong", "Hong Kong SAR China", country_name),
         iso2c = ifelse(name == "hong kong", "hk", iso2c),
         iso3c = ifelse(name == "hong kong", "hkg", iso3c),
         wb_code = ifelse(name == "hong kong", "hkg", wb_code),

         country_name = ifelse(name == "kyrgyz rep", "Kyrgyzstan", country_name),
         iso2c = ifelse(name == "kyrgyz rep", "kg", iso2c),
         iso3c = ifelse(name == "kyrgyz rep", "kgz", iso3c),
         wb_code = ifelse(name == "kyrgyz rep", "kgz", country_name),
         polity_code = ifelse(name == "kyrgyz rep", "kyr", country_name),

         country_name = ifelse(name == "slovak rep", "Slovakia", country_name),
         iso2c = ifelse(name == "slovak rep", "sk", iso2c),
         iso3c = ifelse(name == "slovak rep", "svk", iso3c),
         wb_code = ifelse(name == "slovak rep", "svk", country_name),
         polity_code = ifelse(name == "slovak rep", "slo", country_name),

         country_name = ifelse(name == "trinidad and tobago", "Trinidad & Tobago", country_name),
         iso2c = ifelse(name == "trinidad and tobago", "tt", iso2c),
         iso3c = ifelse(name == "trinidad and tobago", "tto", iso3c),
         wb_code = ifelse(name == "trinidad and tobago", "tto", country_name),
         polity_code = ifelse(name == "trinidad and tobago", "tri", country_name)) %>%
  dplyr::select(-geo,-country,-name)


##################--------------------------------------------------------------
# 4.3 google time preference
##################--------------------------------------------------------------

# the data is downloaded from google trends

# run the next junk only for downloading ggtrends data once, the here used data was downloaded on the 14th of July of 2020
# note that given the number of loops it is possible that Google interrupts the downloading process

'
# get countries for which google trends are avaiable
gtrends_countries <- countries %>% dplyr::select(country_code,sub_code,name) %>%
  mutate(sub_code = as.character(sub_code),
         name  = tolower(name)) %>%
  filter(is.na(sub_code),
         !str_detect(name, " = "))%>% dplyr::select(-sub_code)  %>% distinct()

# number of countries
n_google <- length(gtrends_countries$country_code)

# create data frame and name it
table_google <- as.data.frame(matrix(data = NA, nrow = n_google, ncol = 16))
colnames(table_google) <- c("geo","name",
                            "a_2011","a_2013",
                            "b_2012","b_2014",
                            "c_2013","c_2015",
                            "d_2014","d_2016",
                            "e_2015","e_2017",
                            "f_2016","f_2018",
                            "g_2017","g_2019")


# create a loop to download and prepare data per country
for (google in 1:n_google) {
  cur_country_code <-  as.character(gtrends_countries[google,1])
  cur_country <-  as.character(gtrends_countries[google,2])
  print(label_percent()(google/n_google))

  Sys.sleep(10)
  testing <- try(gtrends(c("2017","2019"),tz=0,gprop = "web",geo = cur_country_code, time = "2012-01-08 2018-12-24"),silent = TRUE)
  if (class(testing)[1] == "gtrends") {

    # download data
    Sys.sleep(10)
    f1a  <- gtrends(c("2011","2013"),tz=0,gprop = "web",geo = cur_country_code,time = "2012-01-08 2012-12-24") # for year 2012
    Sys.sleep(10)
    f1b  <- gtrends(c("2012","2014"),tz=0,gprop = "web",geo = cur_country_code,time = "2013-01-08 2013-12-24") # for year 2013
    Sys.sleep(10)
    f1c  <- gtrends(c("2013","2015"),tz=0,gprop = "web",geo = cur_country_code,time = "2014-01-08 2014-12-24") # for year 2014
    Sys.sleep(10)
    f1d  <- gtrends(c("2014","2016"),tz=0,gprop = "web",geo = cur_country_code,time = "2015-01-08 2015-12-24") # for year 2015
    Sys.sleep(10)
    f1e  <- gtrends(c("2015","2017"),tz=0,gprop = "web",geo = cur_country_code,time = "2016-01-08 2016-12-24") # for year 2016
    Sys.sleep(10)
    f1f  <- gtrends(c("2016","2018"),tz=0,gprop = "web",geo = cur_country_code,time = "2017-01-08 2017-12-24") # for year 2017
    Sys.sleep(10)
    f1g  <- gtrends(c("2017","2019"),tz=0,gprop = "web",geo = cur_country_code,time = "2018-01-08 2018-12-24") # for year 2018


    # extract data and fill in information
    table_google[google,1]  <- cur_country_code
    table_google[google,2]  <- cur_country
    table_google[google,3]  <- if (!is.null(f1a$interest_over_time) ) { (f1a$interest_over_time %>% group_by(keyword) %>% summarise(mean(hits)))[1,2]}else{NA}
    table_google[google,4]  <- if (!is.null(f1a$interest_over_time) ) { (f1a$interest_over_time %>% group_by(keyword) %>% summarise(mean(hits)))[2,2]}else{NA}
    table_google[google,5]  <- if (!is.null(f1b$interest_over_time) ) { (f1b$interest_over_time %>% group_by(keyword) %>% summarise(mean(hits)))[1,2]}else{NA}
    table_google[google,6]  <- if (!is.null(f1b$interest_over_time) ) { (f1b$interest_over_time %>% group_by(keyword) %>% summarise(mean(hits)))[2,2]}else{NA}
    table_google[google,7]  <- if (!is.null(f1c$interest_over_time) ) { (f1c$interest_over_time %>% group_by(keyword) %>% summarise(mean(hits)))[1,2]}else{NA}
    table_google[google,8]  <- if (!is.null(f1c$interest_over_time) ) { (f1c$interest_over_time %>% group_by(keyword) %>% summarise(mean(hits)))[2,2]}else{NA}
    table_google[google,9]  <- if (!is.null(f1d$interest_over_time) ) { (f1d$interest_over_time %>% group_by(keyword) %>% summarise(mean(hits)))[1,2]}else{NA}
    table_google[google,10] <- if (!is.null(f1d$interest_over_time) ) { (f1d$interest_over_time %>% group_by(keyword) %>% summarise(mean(hits)))[2,2]}else{NA}
    table_google[google,11] <- if (!is.null(f1e$interest_over_time) ) { (f1e$interest_over_time %>% group_by(keyword) %>% summarise(mean(hits)))[1,2]}else{NA}
    table_google[google,12] <- if (!is.null(f1e$interest_over_time) ) { (f1e$interest_over_time %>% group_by(keyword) %>% summarise(mean(hits)))[2,2]}else{NA}
    table_google[google,13] <- if (!is.null(f1f$interest_over_time) ) { (f1f$interest_over_time %>% group_by(keyword) %>% summarise(mean(hits)))[1,2]}else{NA}
    table_google[google,14] <- if (!is.null(f1f$interest_over_time) ) { (f1f$interest_over_time %>% group_by(keyword) %>% summarise(mean(hits)))[2,2]}else{NA}
    table_google[google,15] <- if (!is.null(f1g$interest_over_time) ) { (f1g$interest_over_time %>% group_by(keyword) %>% summarise(mean(hits)))[1,2]}else{NA}
    table_google[google,16] <- if (!is.null(f1g$interest_over_time) ) { (f1g$interest_over_time %>% group_by(keyword) %>% summarise(mean(hits)))[2,2]}else{NA}

    if (google == 1) {dat_google_save <- bind_rows(if (!is.null(f1a$interest_over_time) ) { f1a$interest_over_time %>% mutate(hits = as.numeric(as.character(hits)))},
                                                   if (!is.null(f1b$interest_over_time) ) { f1b$interest_over_time %>% mutate(hits = as.numeric(as.character(hits)))},
                                                   if (!is.null(f1c$interest_over_time) ) { f1c$interest_over_time %>% mutate(hits = as.numeric(as.character(hits)))},
                                                   if (!is.null(f1d$interest_over_time) ) { f1d$interest_over_time %>% mutate(hits = as.numeric(as.character(hits)))},
                                                   if (!is.null(f1e$interest_over_time) ) { f1e$interest_over_time %>% mutate(hits = as.numeric(as.character(hits)))},
                                                   if (!is.null(f1f$interest_over_time) ) { f1f$interest_over_time %>% mutate(hits = as.numeric(as.character(hits)))},
                                                   if (!is.null(f1g$interest_over_time) ) { f1g$interest_over_time %>% mutate(hits = as.numeric(as.character(hits)))})} else {

                                                     dat_google_save_aux <-bind_rows(if (!is.null(f1a$interest_over_time) ) { f1a$interest_over_time %>% mutate(hits = as.numeric(as.character(hits)))},
                                                                                     if (!is.null(f1b$interest_over_time) ) { f1b$interest_over_time %>% mutate(hits = as.numeric(as.character(hits)))},
                                                                                     if (!is.null(f1c$interest_over_time) ) { f1c$interest_over_time %>% mutate(hits = as.numeric(as.character(hits)))},
                                                                                     if (!is.null(f1d$interest_over_time) ) { f1d$interest_over_time %>% mutate(hits = as.numeric(as.character(hits)))},
                                                                                     if (!is.null(f1e$interest_over_time) ) { f1e$interest_over_time %>% mutate(hits = as.numeric(as.character(hits)))},
                                                                                     if (!is.null(f1f$interest_over_time) ) { f1f$interest_over_time %>% mutate(hits = as.numeric(as.character(hits)))},
                                                                                     if (!is.null(f1g$interest_over_time) ) { f1g$interest_over_time %>% mutate(hits = as.numeric(as.character(hits)))})

                                                     dat_google_save <- bind_rows(dat_google_save,dat_google_save_aux)

                                                   }

  }
}

# save google trends data
setwd(cd_input_data) # set working directory
# write.csv(dat_google_save, file = "dat_google_save_14072020_per_country_until_country_allyear.csv")
# write.csv(table_google, file = "dat_google_save_14072020_per_country_summary_until_country_allyear.csv")
'

# read google trends data
setwd(cd_input_data) # set working directory
dat_google_save <- read.csv(file = "dat_google_save_14072020_per_country_until_country_allyear.csv", stringsAsFactors = F) %>% dplyr::select(-X)
table_google    <- read.csv(file = "dat_google_save_14072020_per_country_summary_until_country_allyear.csv", stringsAsFactors = F) %>% dplyr::select(-X)



# create dataset with countries that had in more than 95% of the time values unequal to zero
dat_google_save_aux2 <- dat_google_save %>% group_by(geo) %>%
  mutate(zeros = sum(hits == 0, na.rm = T),
         all_n = sum(hits > -2, na.rm = T),
         share_non_zero = 1-zeros/all_n)  %>%
  filter(share_non_zero>0.95) %>%
  mutate(keep_dummy = 1) %>%
  dplyr::select(geo,keep_dummy) %>%
  distinct()

# discard counties from the main dataset that had less than 95% of the time values unequal to zero
table_google_2 <- left_join(table_google,dat_google_save_aux2, by = "geo") %>% filter(keep_dummy == 1, name != "st helena")


# combine google trends data of different years and calculate yearly google time preference
f1 <- table_google_2 %>% #
  mutate(tp2012 = a_2013/a_2011,
         tp2013 = b_2014/b_2012,
         tp2014 = c_2015/c_2013,
         tp2015 = d_2016/d_2014,
         tp2016 = e_2017/e_2015,
         tp2017 = f_2018/f_2016,
         tp2018 = g_2019/g_2017)

# how many NAs?
f1$na_count <- apply(f1[, c("tp2012","tp2013","tp2014","tp2015","tp2016","tp2017","tp2018")], 1, function(x) sum(is.na(x)))

# delete countries with NAs in more than three measures
f2 <- f1 %>% filter(na_count<4) %>% dplyr::select(geo, name, tp2012:tp2018) %>%
  pivot_longer(-c(geo, name), names_to = "tp", values_to = "value") %>%
  mutate(year = as.numeric(str_sub(tp,4,8)),
         id = paste0(geo,tp),
         n_geo = nchar(name))%>%
  arrange(n_geo)

# delete duplicates
f2$duplicate <- !duplicated(f2$id)
f2 <- f2 %>% filter(duplicate == T) %>%
  dplyr::select(-n_geo,-duplicate)



# check and delete observations using cook’s distance (see also https://datascienceplus.com/outlier-detection-and-treatment-with-r/)
mod <- lm(value ~ factor(geo)+year, data=f2)
cooksd <- cooks.distance(mod)
plot(cooksd, pch="*", cex=2, main="Influential Obs by Cooks distance")  # plot cook's distance
abline(h = 4*mean(cooksd, na.rm=T), col="red")  # add cutoff line
text(x=1:length(cooksd)+1, y=cooksd, labels=ifelse(cooksd>4*mean(cooksd, na.rm=T),names(cooksd),""), col="red")  # add labels
influential <- as.numeric(names(cooksd)[(cooksd > 4*mean(cooksd, na.rm=T))]) # influential row numbers
f2_aux <- f2[influential, ] %>% mutate(delete_dummy = 1) %>% ungroup() %>% dplyr::select(id,delete_dummy)
f3 <- left_join(f2,f2_aux, by = "id") %>% group_by(geo) %>%
  mutate(value2 = ifelse(is.na(delete_dummy),value,NA),
         value3 =  na.interpolation(value2),
         val_norm = value3/value3[year==2016][1L])  # 2016 google changed their data collection algorithm



# compute google time preference from 2012 to 2018 and 2016 to 2018. the latter one is mostly used.
f4 <- f3 %>%
  mutate(tp_google_1 = mean(value3,na.rm = T)) %>%
  filter(year > 2015) %>%
  mutate(tp_google_2 = mean(value3,na.rm = T)) %>%
  dplyr::select(geo,name,tp_google_1,tp_google_2) %>% distinct() %>%
  mutate(geo2 = tolower(geo)) %>% ungroup() %>% dplyr::select(-geo)


# add country code information
f4 <- left_join(f4,code_info %>% mutate(geo2 = iso2c),by = "geo2") %>%
  mutate(country_name = ifelse(name == "namibia", "Namibia", country_name),
         iso2c = ifelse(name == "namibia", "na", iso2c),
         iso3c = ifelse(name == "namibia", "nam", iso3c),
         wb_code = ifelse(name == "namibia", "nam", wb_code),
         polity_code = ifelse(name == "namibia", "nam", polity_code)) %>%
  dplyr::select(-geo2,-name)


##################--------------------------------------------------------------
# 4.4 combine environmental policy performance, public environmental priority,
#     and time preference data
##################--------------------------------------------------------------

s1 <-
  full_join(e1 %>% dplyr::select(-c(country_name,iso3c:polity_code)),
            p1 %>% dplyr::select(-c(country_name,iso3c:polity_code)), by = "iso2c") %>%
  full_join(k1 %>% dplyr::select(-c(country_name,iso3c:polity_code)), by = "iso2c") %>%
  full_join(h1 %>% dplyr::select(-c(country_name,iso3c:polity_code)), by = "iso2c")
s2 <- full_join(s1,f4 %>% dplyr::select(-c(country_name,iso3c:polity_code)), by = "iso2c")



################################################################################
# 5. macro data
################################################################################

##################--------------------------------------------------------------
# 5.1 economic indicators and country information
##################--------------------------------------------------------------

# most economic indicators and country information is taken from world development indicators
# (world bank group 2019)

# download data using WDI
w1 <- WDI(country = "all",indicator = c("NY.GDP.PCAP.KD","SP.POP.TOTL", "IT.NET.USER.ZS","NE.TRD.GNFS.ZS",
                                        "SP.RUR.TOTL.ZS","EN.ATM.PM25.MC.ZS","AG.LND.AGRI.ZS"),
          start = 2010, end = 2019, extra = TRUE)
# note:
## gdp per capita (constant 2010 US$) --> NY.GDP.PCAP.KD
## population, total --> SP.POP.TOTL
## individuals using the Internet (% of population) --> IT.NET.USER.ZS
## trade (% of GDP) --> NE.TRD.GNFS.ZS
## rural population (% of total population) --> SP.RUR.TOTL.ZS
## pM2.5 air pollution, population exposed to levels exceeding WHO guideline value (% of total) --> EN.ATM.PM25.MC.ZS
## agricultural land (% of land area) --> AG.LND.AGRI.ZS
## automatically includes countries' coordinates

# delete aggregated regions
w2 <- w1 %>%  mutate(region = ifelse(region == "NA",NA,region)) %>% filter(!income == "Aggregates",!lending == "Aggregates")


# compute averages of timer variable economic indicators
w2 <- w2 %>% group_by(country) %>%
  mutate(gdp_overt = mean(NY.GDP.PCAP.KD, na.rm = T),      # mean gdp per capita over time.
         trade_overt = mean(NE.TRD.GNFS.ZS, na.rm = T),    # mean trade over time
         rural_overt = mean(SP.RUR.TOTL.ZS, na.rm = T),    # mean rural population
         air_overt   = mean(EN.ATM.PM25.MC.ZS, na.rm = T), # mean air pollution
         agri_overt  = mean(AG.LND.AGRI.ZS, na.rm = T),    # mean agricultural land
         pop_overt = mean(SP.POP.TOTL, na.rm = T),         # mean population index over time.
         net_overt = mean(IT.NET.USER.ZS, na.rm = T)) %>%  # compute mean internet users index over time.
  ungroup() %>%
  dplyr::select(-c(NY.GDP.PCAP.KD,NE.TRD.GNFS.ZS,SP.RUR.TOTL.ZS,EN.ATM.PM25.MC.ZS,AG.LND.AGRI.ZS,SP.POP.TOTL,IT.NET.USER.ZS,year)) %>% # only maintain average values
  distinct() # drop duplicates

# fill empty cells with NAs
w2[w2=="NaN"]  <- NA

# calculate number of internet users in a country
w2 <- w2 %>% mutate(internet_user = pop_overt*net_overt/100)


# add gdp for Taiwan Province of China. info taken from https://www.imf.org/external/datamapper/NGDPDPC@WEO/TWN
taiwan <- data.frame(
  year = 2010:2019,
  taiwan_gdp_current = c(19181.358, 20838.589, 21256.361, 21945.46, 22844.316, 22752.991, 23070.733, 25061.624, 25780.407, 25873.367), # GDP per capita, current prices (U.S. dollars per capita)
  taiwan_conversion = c(15.808, 15.151, 15.081, 15.042, 15.25, 15.476, 15.776, 15.73, 15.25, 15.02),   # Implied PPP conversion rate (National currency per international dollar)
  population_taiwan = c(23162123, 23224912, 23315822, 23373517, 23433753, 23492074, 23539816, 23571227, 23588932, 23603121)) # total population per year  https://eng.stat.gov.tw/mp.asp?mp=5
taiwan <- taiwan %>%
  mutate(price_deflator_2010 = taiwan_conversion/15.808,
         taiwan_gdp_constant_2010 = taiwan_gdp_current * price_deflator_2010)

# compute gdp per capital and population mean
taiwan_constant_mean <- mean(taiwan$taiwan_gdp_constant_2010)
taiwan_population <- mean(taiwan$population_taiwan)

w2x <- w2 # create dataset, which is used later for imputing values

# add country code information
w2 <- left_join(w2 %>%  mutate(geo = tolower(iso3c)), code_info %>% mutate(geo = wb_code),by = "geo", suffix = c(".old", "")) %>%
  mutate(country_name = ifelse(country == "Micronesia, Fed. Sts.", "Micronesia", country_name),
         iso2c = ifelse(country == "Micronesia, Fed. Sts.", "fm", iso2c),
         iso3c = ifelse(country == "Micronesia, Fed. Sts.", "fsm", iso3c),

         country_name = ifelse(country == "Kosovo", "Kosovo", country_name),
         iso2c = ifelse(country == "Kosovo", "xk", iso2c),
         iso3c = ifelse(country == "Kosovo", "xkx", iso3c),

         country_name = ifelse(country == "Channel Islands", "Channel Islands", country_name),
         iso2c = ifelse(country == "Channel Islands", "jg", iso2c),
         iso3c = ifelse(country == "Channel Islands", "chi", iso3c),

         country_name = ifelse(country == "St. Martin (French part)", "St. Martin (French part)", country_name),
         iso2c = ifelse(country == "St. Martin (French part)", "mf", iso2c),
         iso3c = ifelse(country == "St. Martin (French part)", "maf", iso3c)) %>%
  dplyr::select(-iso2c.old,-country, - iso3c.old, - capital, -geo)


# load island information:
setwd(cd_input_data) # set working directory
i1 <- read_csv("island_data.csv")  %>%
  mutate(island = 1) %>% # add island information as dummy
  dplyr::select(-pop2019) # delete redundant information

# add country code information
i1 <- left_join(i1, code_info %>% mutate(name = country_name),by = "name") %>%
  mutate(country_name = ifelse(name == "Micronesia", "Micronesia", country_name),
         iso2c = ifelse(name == "Micronesia", "fm", iso2c),
         iso3c = ifelse(name == "Micronesia", "fsm", iso3c),

         country_name = ifelse(name == "Antigua and Barbuda", "Antigua and Barbuda", country_name),
         iso2c = ifelse(name == "Antigua and Barbuda", "ag", iso2c),
         iso3c = ifelse(name == "Antigua and Barbuda", "atg", iso3c),

         country_name = ifelse(name == "Saint Kitts and Nevis", "Saint Kitts and Nevis", country_name),
         iso2c = ifelse(name == "Saint Kitts and Nevis", "kn", iso2c),
         iso3c = ifelse(name == "Saint Kitts and Nevis", "kna", iso3c),

         country_name = ifelse(name == "Saint Lucia", "Saint Lucia", country_name),
         iso2c = ifelse(name == "Saint Lucia", "lc", iso2c),
         iso3c = ifelse(name == "Saint Lucia", "lca", iso3c),

         country_name = ifelse(name == "Saint Vincent and the Grenadines", "Saint Vincent and the Grenadines", country_name),
         iso2c = ifelse(name == "Saint Vincent and the Grenadines", "vc", iso2c),
         iso3c = ifelse(name == "Saint Vincent and the Grenadines", "vct", iso3c),

         country_name = ifelse(name == "Sao Tome and Principe", "Sao Tome and Principe", country_name),
         iso2c = ifelse(name == "Sao Tome and Principe", "st", iso2c),
         iso3c = ifelse(name == "Sao Tome and Principe", "stp", iso3c),

         country_name = ifelse(name == "Trinidad and Tobago", "Trinidad and Tobago", country_name),
         iso2c = ifelse(name == "Trinidad and Tobago", "tt", iso2c),
         iso3c = ifelse(name == "Trinidad and Tobago", "tto", iso3c)) %>%
  dplyr::select(-name)


##################--------------------------------------------------------------
# 5.2 democratization index
##################--------------------------------------------------------------

# democratization index is taken from the polity iv dataset (marshall et al. 2019).

# read the democratization index
setwd(cd_input_data) # set working directory
a1 <- read.csv2("polity_iv_data.csv", stringsAsFactors = F) %>%
  dplyr::select(year,scode,country,polity2) %>%
  filter(year > 2009 & year < 2020) %>% # restrict time period from 2010 to 2019
  group_by(scode) %>%
  mutate(polity2_overt = mean(polity2, na.rm = T), # compute average value of index
         polity2_overt = ifelse(polity2_overt == "NaN",NA, polity2_overt),
         name = tolower(country),
         geo = tolower(scode)) %>%
  dplyr::select(-year,-polity2) %>% distinct() %>% ungroup()


# add country code information
a1 <- left_join(a1, code_info %>% mutate(geo = polity_code),by = "geo") %>%
  mutate(country_name = ifelse(name == "sudan", "Sudan", country_name),
         iso2c = ifelse(name == "sudan", "sd", iso2c),
         iso3c = ifelse(name == "sudan", "sdn", iso3c),

         country_name = ifelse(name == "kosovo", "Kosovo", country_name),
         iso2c = ifelse(name == "kosovo", "xk", iso2c),
         iso3c = ifelse(name == "kosovo", "xkx", iso3c)) %>%
  dplyr::select(-scode,-country, -name, -geo)


##################--------------------------------------------------------------
# 5.3 soviet history
##################--------------------------------------------------------------

# add information if country has a former soviet history. taken from acemoglu et al 2019.
setwd(cd_input_data) # set working directory
sov1 <- read.csv("former_soviet_data.csv", stringsAsFactors = F) %>%
  mutate(geo = tolower(wbcode)) %>%
  dplyr::select(-country_name,-wbcode)

# add country code information
sov1$geo[sov1$geo=="rom"] <- "rou"
sov1 <- left_join(sov1, code_info %>% mutate(geo = iso3c),by = "geo") %>%
  dplyr::select(-geo)


##################--------------------------------------------------------------
# 5.4 education
##################--------------------------------------------------------------

# add information about schooling. taken from friedman et al (2020)

# location of pdf file
location <- 'https://static-content.springer.com/esm/art%3A10.1038%2Fs41586-020-2198-8/MediaObjects/41586_2020_2198_MOESM1_ESM.pdf'

# extract the table
out <- extract_tables(location)
out_aux1 <- out[[3]][-(1:2),-2] # first page
out_aux2 <- do.call(rbind,out[4:16] ) # other relevant pages
out2 <- as.data.frame(rbind(out_aux1,out_aux2)) # combine pages
colnames(out2) <- c("country","year","male","female","gender_gap","AID","mean","prop_0","prop_6","prop_12","prop_15")
out2 <- out2 %>%
  filter(year == "2018") %>%
  dplyr::select(country,prop_12) %>%
  mutate(name = tolower(country),
         name = case_when(name == "bosnia and herzegovina" ~ "bosnia & herzegovina", TRUE ~ name),
         name = case_when(name == "antigua and barbuda" ~ "antigua & barbuda", TRUE ~ name),
         name = case_when(name == "sao tome and principe" ~ "sao tome & principe", TRUE ~ name),
         name = case_when(name == "trinidad and tobago" ~ "trinidad & tobago", TRUE ~ name),
         name = case_when(name == "czech republic" ~ "czechia", TRUE ~ name),
         name = case_when(name == "the bahamas" ~ "bahamas", TRUE ~ name),
         name = case_when(name == "macedonia" ~ "north macedonia", TRUE ~ name),
         name = case_when(name == "myanmar" ~ "myanmar (burma)", TRUE ~ name),
         name = case_when(name == "saint lucia" ~ "st. lucia", TRUE ~ name),
         name = case_when(name == "saint vincent and the grenadines" ~ "st. vincent & grenadines", TRUE ~ name),
         name = case_when(name == "the gambia" ~ "gambia", TRUE ~ name),
         name = case_when(name == "virgin islands, u.s." ~ "u.s. virgin islands", TRUE ~ name),
         name = case_when(name == "swaziland" ~ "eswatini", TRUE ~ name),
         name = case_when(name == "congo" ~ "congo - brazzaville", TRUE ~ name),
         name = case_when(name == "democratic republic of the congo" ~ "congo - kinshasa", TRUE ~ name),
         name = case_when(name == "palestine" ~ "palestinian territories", TRUE ~ name))


# add country code information
out2 <- left_join(out2, code_info %>% mutate(name = tolower(country_name)),by = "name") %>%
  mutate(country_name = ifelse(name == "federated states of micronesia", "Micronesia", country_name),
         iso2c = ifelse(name == "federated states of micronesia", "fm", iso2c),
         iso3c = ifelse(name == "federated states of micronesia", "fsm", iso3c),
         country_name = ifelse(name == "sao tome & principe", "Sao Tome & Principe", country_name),
         iso2c = ifelse(name == "sao tome & principe", "st", iso2c),
         iso3c = ifelse(name == "sao tome & principe", "stp", iso3c),
         country_name = ifelse(str_detect(name, "ivoire"), "ivoire", country_name),
         iso2c = ifelse(str_detect(name, "ivoire"), "ci", iso2c),
         iso3c = ifelse(str_detect(name, "ivoire"), "civ", iso3c)) %>%
  dplyr::select(-country, -name)


################################################################################
# 6. combine, impute, clean, and save data
################################################################################

# add all dataset together
s3 <- full_join(s2,w2 %>% dplyr::select(-c(country_name,iso3c:polity_code)), by = "iso2c") %>%
  full_join(i1 %>% dplyr::select(-c(country_name,iso3c:polity_code)), by = "iso2c") %>%
  mutate(island = ifelse(is.na(island), 0, island))    # adjust island dummy

s4 <- full_join(s3,a1 %>% dplyr::select(-c(country_name,iso3c:polity_code)), by = "iso2c")

s4 <- full_join(s4,sov1 %>% dplyr::select(-c(country_name,iso3c:polity_code)), by = "iso2c") %>%
  mutate(sov1 = case_when(sov1 == 1 ~ 1, TRUE ~ 0))

s4 <- full_join(s4,out2 %>% dplyr::select(-c(country_name,iso3c:polity_code)), by = "iso2c") %>%
  left_join(code_info, by = "iso2c")


# clean, rename dataset and add taiwan information
d1 <- s4 %>%
  dplyr::select(country_name, iso2c, iso3c:polity_code,overall_mean,EPI:tp_google_2,gdp_overt:prop_12,region,income,latitude, -rural_overt) %>%   # order data
  rename(pub_prio_2 = overall_mean,
         gov_epp = EPI,
         gov_climate = CCH,
         gov_biodiversity = BDH,
         tp_gps = patience,
         tp_hofstede = ltowvs,
         tp_google_1 = tp_google_1,
         tp_google_2 = tp_google_2,
         gdp_per = gdp_overt,
         trade = trade_overt,
         air = air_overt,
         agri_share = agri_overt,
         population = pop_overt,
         polity2 = polity2_overt,
         soviet_hist = sov1,
         education = prop_12,
         internet_share= net_overt) %>%
  mutate(gdp_per = ifelse(country_name == "Taiwan", taiwan_constant_mean, gdp_per),
         latitude = ifelse(country_name == "Taiwan", 25.105497, latitude),
         population = ifelse(country_name == "Taiwan", taiwan_population, population),
         region = ifelse(country_name == "Taiwan", 2, region),
         income = ifelse(country_name == "Taiwan", "High income", as.character(income)),
         latitude = ifelse(iso3c == "pse", 31.9474, latitude)) %>%
  filter(!is.na(gdp_per),!is.na(polity2)) %>%
  mutate(latitude = as.numeric(as.character(latitude)))


# only keep data for which one level of environmental policy performance is available
d1 <- left_join(d1, WVS_Wave_joint_1 %>% dplyr::select(N_env_vs_econ_share_env_ln,iso3c), by = "iso3c") %>%
  filter(!is.na(gov_epp) | !is.na(gov_climate) | !is.na(gov_biodiversity)) %>%
  dplyr::rename(pub_prio_1_ln = N_env_vs_econ_share_env_ln)


# impute mean values based on region and income level
impute_values <- w2x %>%
  group_by(region,income) %>%
  summarise(mean_internet_share = mean(internet_user, na.rm = T),
            mean_trade = mean(trade_overt, na.rm = T),
            mean_air = mean(air_overt, na.rm = T),
            mean_agri_share = mean(agri_overt, na.rm = T)) %>% ungroup()
impute_values <- impute_values %>%
  mutate(mean_trade = ifelse(region == 5 & income == "Low income",as.numeric((impute_values %>% filter(region == 7, income == "Low income"))$mean_trade),mean_trade))


# continue cleaning the data
d2 <- left_join(d1 %>% mutate(id = paste0(region,income)),
                impute_values %>% mutate(id = paste0(region,income)) %>% dplyr::select(-region, -income), by = "id" ) %>%
  mutate(internet_share = ifelse(is.na(internet_share), mean_internet_share, internet_share),
         trade = ifelse(is.na(trade), mean_trade, trade),
         air = ifelse(is.na(air), mean_air, air),
         agri_share = ifelse(is.na(agri_share), mean_agri_share, agri_share)) %>%
  # show if we have some environmental policy performance and/or tp information  and reduce dataset
  mutate(epp_at_least_one = ifelse(!is.na(gov_epp), 1,
                                                 ifelse(!is.na(gov_climate), 1,
                                                        ifelse(!is.na(gov_biodiversity), 1, 0))),
         tp_at_least_one = ifelse(!is.na(tp_gps), 1,
                                   ifelse(!is.na(tp_hofstede), 1,
                                          ifelse(!is.na(tp_google_2), 1,0))),
         at_least_one = epp_at_least_one * tp_at_least_one) %>%
  filter(at_least_one >  0) %>%
  dplyr::select(-c(epp_at_least_one:at_least_one)) %>%
  mutate(latitude_abs = abs(as.numeric(as.character(latitude))))


# create log values
d2 <- d2 %>%
  mutate(pub_prio_2_ln = log(pub_prio_2 + 1),
         gov_epp_ln = log(gov_epp),
         gov_climate_ln = log(gov_climate),
         gov_biodiversity_ln = log(gov_biodiversity),
         tp_gps_ln = log(tp_gps+1),
         tp_hofstede_ln = log(tp_hofstede),
         tp_google_2_ln = log(tp_google_2),
         gdp_per_ln = log(gdp_per),
         polity2_ln = log(polity2+11),
         latitude_abs_ln = log(latitude_abs),
         education_ln = log(as.numeric(as.character(education))),
         internet_share_ln = log(internet_share),
         population_ln = log(population),
         trade_ln = log(trade),
         air_ln = log(air),
         agri_share_ln = log(agri_share))




# data to create figure s9
f3_aux <- left_join(f3 %>% mutate(geo = tolower(geo)),code_info %>% mutate(geo = iso2c),by = "geo") %>%
  mutate(country_name = ifelse(name == "namibia", "Namibia", country_name),
         iso2c = ifelse(name == "namibia", "na", iso2c),
         iso3c = ifelse(name == "namibia", "nam", iso3c),
         wb_code = ifelse(name == "namibia", "nam", wb_code),
         polity_code = ifelse(name == "namibia", "nam", polity_code)) %>% ungroup() %>%
  dplyr::select(-geo,-name)
f3_aux2 <- semi_join(f3_aux ,
                     d2 %>% dplyr::select(iso2c,tp_google_2) %>% filter(!is.na(tp_google_2)), by = "iso2c")



# save data used for analysis
setwd(cd_input_data)
# save(d2, file="epp_tp_data_main.RData")
# save(d1, file="epp_tp_data_availability.RData")
# save(f3_aux2, file="epp_tp_data_figure_s9.RData")
