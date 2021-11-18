################################################################################
# r code related to                                                            #
# "Global Relationships between Time Preference and Environmental Policy       #
# Performance" by Sergei Schaub published in Environmental Science & Policy    #
# -----------------------------------------------------------------------------#
# 2021, ETH Zurich                                                             #
# developed by Sergei Schaub                                                   #
# -----------------------------------------------------------------------------#
# specification charts                                                         #
################################################################################



# table of contents:
# 1. load data and prepare data
# 2. specification of variables, datasets, names, etc 
# 3. run analysis and create parts of the specification charts
# 4. assembling of specification charts
# 5. save figures


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
# 2. specification of variables, datasets, names, etc 
################################################################################

# vector of different dependent variables
dependent_vector <- c("gov_epp_ln","gov_climate_ln","gov_biodiversity_ln")

# vector indicating entire sample or sub-sample analysis
data_vector <- c("main_entire", "main_sub", "main_entire", "main_sub", "main_entire", "main_sub")

# vector specifying name of analysis
data_nice_vector <- c("Main Analysis", "Main Analysis",
                      "Sensitivity Analysis I", "Sensitivity Analysis I",
                      "Sensitivity Analysis II", "Sensitivity Analysis II")

# vector indicating sample
sample <- c("Entire Sample", "Sub-Sample", "Entire Sample", "Sub-Sample", "Entire Sample", "Sub-Sample")

# vector including main independent variables of model with controls
independent_var_main_vector <- c("tp_gps_ln + gdp_per_ln","tp_gps_ln + gdp_per_ln",
                                 "tp_hofstede_ln + gdp_per_ln","tp_hofstede_ln + gdp_per_ln",
                                 "tp_google_2_ln + gdp_per_ln","tp_google_2_ln + gdp_per_ln")

# vector including independent variables of model without controls
independent_var_alone_vector <- c("tp_gps_ln","tp_gps_ln",
                                  "tp_hofstede_ln","tp_hofstede_ln",
                                  "tp_google_2_ln","tp_google_2_ln")

# number of dependent variables
n_dependet <- length(dependent_vector)

# number of analyses
n_data <- length(data_vector)


################################################################################
# 3. run analysis and create parts of the specification charts
################################################################################

# define lists to store results
list_figures_entire <- list()
list_figures_sub <- list()
list_figures_spec <- list()


for (s in 1:n_data) {

  # define variables of current loop
  cur_data_name <- data_vector[s]
  cur_data_name_nice <- data_nice_vector[s]
  cur_sample_name <- sample[s]
  independent_var_main <- independent_var_main_vector[s]
  independent_var_alone <- independent_var_alone_vector[s]

  # specify and bootstrap data
  if (cur_data_name == "main_entire") {
    
    if (independent_var_alone == "tp_gps_ln") {
      set.seed(123)
      data_boot <- bootstraps(d2_boot_econ %>% as_tibble(), times = 1000, apparent = FALSE) }

    if (independent_var_alone == "tp_hofstede_ln") {
      set.seed(123)
      data_boot <- bootstraps(d2_boot_thrift %>% as_tibble(), times = 1000, apparent = FALSE) }

    if (independent_var_alone == "tp_google_2_ln") {
      set.seed(123)
      data_boot <- bootstraps(d2_boot_google %>% as_tibble(), times = 1000, apparent = FALSE) }

      

      } else if (cur_data_name == "main_sub") {
                 set.seed(123)
                 data_boot <- bootstraps(d2_boot_sub %>% as_tibble(), times = 1000, apparent = FALSE)

                 }


  for (d in 1:n_dependet) {

    dependent_var <- dependent_vector[d]
    print(dependent_var)


    # vectors of variables
    dependent_var_other_change <- c("polity2_ln","education_ln","latitude_abs_ln","internet_share_ln")
    dependent_var_other_on_off <- c("trade_ln","air_ln","agri_share_ln","population_ln","factor(soviet_hist)")

    # create formulas and dataframe including the formulas
    aux1 <- as.data.frame("")
    colnames(aux1) <- "formulas"
    aux2 <- as.data.frame("gdp_per_ln") %>% unite("formulas",  remove = T, sep = " + ")
    colnames(aux2) <- "formulas"
    aux1 <- bind_rows(aux1,aux2)
    aux1  <- bind_rows(aux1, combinations(4,1,dependent_var_other_change) %>% as.data.frame() %>% unite("formulas",  remove = T, sep = " + "))
    aux1  <- bind_rows(aux1, combinations(4,2,dependent_var_other_change) %>% as.data.frame() %>% unite("formulas",  remove = T, sep = " + "))
    aux1  <- bind_rows(aux1,combinations(4,3,dependent_var_other_change) %>% as.data.frame() %>% unite("formulas",  remove = T, sep = " + "))
    aux1  <- bind_rows(aux1,combinations(4,4,dependent_var_other_change) %>% as.data.frame() %>% unite("formulas",  remove = T, sep = " + "))

    com_others <- 2^length(dependent_var_other_change)+2 # number of models

    sum_table <- matrix( nrow = com_others, ncol = 7) %>% as.data.frame()
    colnames(sum_table) <- c("Model","Estimate","Conf_Low_99","Conf_High_99","Conf_Low_95","Conf_High_95","Formula")


    # run different models
    for (z in 1:com_others) {

      if (z == 1) { # model only with time preference

        cur_formula <- as.formula(paste(dependent_var,"~", paste(independent_var_alone)))
        #print(cur_formula)

        # set function and data for bootstrapping
        lm_est <- function(split, ...) {
          lm(cur_formula, data = analysis(split)) %>%
            tidy()}

        # run multiple models
        cur_lm_boot <- data_boot %>% mutate(results = purrr::map(splits, lm_est))


      } else if (z==2) { # model only with time preference and gdp

        cur_formula <- as.formula(paste(dependent_var,"~", paste(independent_var_alone, "+"),paste(aux1[z,1])))
        #print(cur_formula)

        # set function and data for bootstrapping
        lm_est <- function(split, ...) {
          lm(cur_formula, data = analysis(split)) %>%
            tidy()}

        # run multiple models
        cur_lm_boot <- data_boot %>% mutate(results = purrr::map(splits, lm_est))

      } else if (z<com_others) { # all other models except the model with all controls

        cur_formula <- as.formula(paste(dependent_var,"~", paste(independent_var_main, "+"),paste(aux1[z,1])))
        #print(cur_formula)


        # set function and data for bootstrapping
        lm_est <- function(split, ...) {
          lm(cur_formula, data = analysis(split)) %>%
            tidy()}

        # run multiple models
        cur_lm_boot <- data_boot %>% mutate(results = purrr::map(splits, lm_est))

      } else { # model with all controls

        cur_formula <- as.formula(paste(dependent_var,"~", paste(independent_var_main, "+"),paste(aux1[z-1,1]),paste("+"),
                                        paste(matrix(dependent_var_other_on_off, nrow = 1) %>% as.data.frame %>% unite("formulas",  remove = T, sep = " + "))  ))
        #print(cur_formula)


        # set function and data for bootstrapping
        lm_est <- function(split, ...) {
          lm(cur_formula, data = analysis(split)) %>%
            tidy()}

        # run multiple models
        cur_lm_boot <- data_boot %>% mutate(results = purrr::map(splits, lm_est))
      }


      # extract information from models
      cur_model95 <- int_pctl(cur_lm_boot, results, alpha = 0.05) %>% filter(str_detect(term, "tp_"))
      cur_model99 <- int_pctl(cur_lm_boot, results, alpha = 0.01) %>% filter(str_detect(term, "tp_"))
      sum_table[z,1] <- paste0("Model_",z)
      sum_table[z,2] <- cur_model99[1,3]
      sum_table[z,3] <- cur_model99[1,2]
      sum_table[z,4] <- cur_model99[1,4]
      sum_table[z,5] <- cur_model95[1,2]
      sum_table[z,6] <- cur_model95[1,4]
      sum_table[z,7] <- toString(cur_formula)
    }

    # summarizing table
    sum_table <- sum_table %>%
      mutate('GDP per\nCapita (log)' = as.integer(as.logical(grepl("gdp_per_ln",sum_table$Formula, fixed = TRUE))),
             GDP = as.integer(as.logical(grepl("gdp_per_ln",sum_table$Formula, fixed = TRUE))),
             'Democracy\nIndex (log)' = as.integer(as.logical(grepl("polity2_ln",sum_table$Formula, fixed = TRUE))),
             Democracy = as.integer(as.logical(grepl("polity2_ln",sum_table$Formula, fixed = TRUE))),
             'Education +12 years \n(age 25-29; log)' = as.integer(as.logical(grepl("education_ln",sum_table$Formula, fixed = TRUE))),
             Schooling  = as.integer(as.logical(grepl("education_ln",sum_table$Formula, fixed = TRUE))),
             'Internet Users\n(%; log)' = as.integer(as.logical(grepl("internet_share_ln",sum_table$Formula, fixed = TRUE))),
             Internet  = as.integer(as.logical(grepl("internet_share_ln",sum_table$Formula, fixed = TRUE))),
             'Latitude\n(absolute; log)' = as.integer(as.logical(grepl("latitude_abs_ln",sum_table$Formula, fixed = TRUE))),
             Latitude = as.integer(as.logical(grepl("latitude_abs_ln",sum_table$Formula, fixed = TRUE))),
             'Population\n(log)' = as.integer(as.logical(grepl("population_ln",sum_table$Formula, fixed = TRUE))),
             Population = as.integer(as.logical(grepl("population_ln",sum_table$Formula, fixed = TRUE))),
             'Trade\n(% of GDP; log)' = as.integer(as.logical(grepl("trade_ln",sum_table$Formula, fixed = TRUE))),
             Trade = as.integer(as.logical(grepl("trade_ln",sum_table$Formula, fixed = TRUE))),
             'Air pollution\n(log)' = as.integer(as.logical(grepl("air_ln",sum_table$Formula, fixed = TRUE))),
             Air = as.integer(as.logical(grepl("air_ln",sum_table$Formula, fixed = TRUE))),
             'Agricultural land\n(% of land area; log)' = as.integer(as.logical(grepl("agri_share_ln",sum_table$Formula, fixed = TRUE))),
             Agricultural = as.integer(as.logical(grepl("agri_share_ln",sum_table$Formula, fixed = TRUE))),
             'Soviet History\n(yes)' = as.integer(as.logical(grepl("factor(soviet_hist)",sum_table$Formula, fixed = TRUE))),
             Soviet = as.integer(as.logical(grepl("factor(soviet_hist)",sum_table$Formula, fixed = TRUE))))

    # define time preference index
    if(independent_var_alone == "tp_gps_ln"){text_aux <- "Economic"
    } else if (independent_var_alone == "tp_hofstede_ln") {text_aux <- "Thrift"
    } else if (independent_var_alone == "tp_google_2_ln") {text_aux <- "Google"
    } else {text_aux <- "Error"}


    theme_set(theme_bw()) # set main design for ggplot2

    # extract info about number of observations
    if (is.odd(s)) {
    N_info_aux <- dplyr::select(d2,contains(paste(independent_var_alone))) %>% filter(!is.na(.[,1]))
    colnames(N_info_aux) <- "aux1"
    N_info <- length(N_info_aux$aux1)

    # set limits of plots
    upper_limit <- 1; lower_limit <- -1 ;upper_break <- 1;lower_break <- -1; steps_break = 0.25

    # plot
    chart_a_1 <- sum_table %>% ggplot(aes(x=reorder(Model, Estimate)  , y=Estimate)) +
      geom_hline(yintercept = 0, color=c("#525252")) +
      geom_errorbar (data = sum_table, aes(x=reorder(Model, Estimate),ymin = Conf_Low_99, ymax = Conf_High_99, colour = "99% Confidence Interval"), size = 2,width=0, position=position_dodge(width=0.9))+
      geom_errorbar (data = sum_table, aes(x=reorder(Model, Estimate),ymin = Conf_Low_95, ymax = Conf_High_95, colour = "95% Confidence Interval"), size = 2,width=0, position=position_dodge(width=0.9))+
      geom_point(aes(shape = "Point Esimate"),size = 3)+
      theme(legend.position = c(0.02,(upper_limit-lower_limit)/(3/0.15)),
            legend.justification = c(0, 1),
            legend.title=element_blank(),
            legend.direction = "horizontal",
            legend.margin = margin(0, 0, 0, 0),
            legend.spacing.y = unit(-0, "cm"),
            legend.spacing.x = unit(2, "pt"),
            panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
            axis.title.x=element_blank(),
            axis.text.x=element_blank(),
            axis.ticks.x=element_blank())+
      scale_color_manual(values = c("#3182bd","#9ecae1","#deebf7"))+
      scale_y_continuous(name = paste0("Row 1: Coefficient of ", text_aux, "\nTime Preference (log)\nEnitre Sample (N = ", N_info,")"),
                         limits = c(lower_limit,upper_limit), breaks = c(seq(lower_break,upper_break,steps_break)))+
      scale_x_discrete(limits=c('Model_1','Model_2','Model_3','Model_4','Model_5','Model_6',
                                'Model_7','Model_8','Model_9','Model_10','Model_11','Model_12',
                                'Model_13','Model_14','Model_15','Model_16','Model_17','Model_18')) # if not specified the model is sorted by size of the estimate
    # chart_a_1

    # store results
    list_figures_entire[[length(list_figures_entire)+1]] <- chart_a_1
    }


    if (is.even(s)) {

      # extract info about number of observations
      N_info <- 52

      # set limits of plots
      upper_limit <- 1; lower_limit <- -1 ;upper_break <- 1;lower_break <- -1; steps_break = 0.25

      # plot
      chart_a_2 <- sum_table %>% ggplot(aes(x=reorder(Model, Estimate)  , y=Estimate)) +
        geom_hline(yintercept = 0, color=c("#525252")) +
        geom_errorbar (data = sum_table, aes(x=reorder(Model, Estimate),ymin = Conf_Low_99, ymax = Conf_High_99, colour = "99% Confidence Interval"), size = 2,width=0, position=position_dodge(width=0.9))+
        geom_errorbar (data = sum_table, aes(x=reorder(Model, Estimate),ymin = Conf_Low_95, ymax = Conf_High_95, colour = "95% Confidence Interval"), size = 2,width=0, position=position_dodge(width=0.9))+
        geom_point(aes(shape = "Point Esimate"),size = 3)+
        theme(legend.position = c(0.02,(upper_limit-lower_limit)/(3/0.15)),
              legend.title=element_blank(),
              legend.direction = "vertical",
              legend.margin = margin(0, 0, 0, 0),
              legend.justification = "left",
              legend.spacing.y = unit(-0, "cm"),
              legend.spacing.x = unit(2, "pt"),
              panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
              axis.title.x=element_blank(),
              axis.text.x=element_blank(),
              axis.ticks.x=element_blank())+
        scale_color_manual(values = c("#3182bd","#9ecae1","#deebf7"))+
        scale_y_continuous(name = paste0("Row 2: Coefficient of ", text_aux, "\nTime Preference (log)\nSub-Sample (N = ", N_info,")"),
                           limits = c(lower_limit,upper_limit), breaks = c(seq(lower_break,upper_break,steps_break)))+
        scale_x_discrete(limits=c('Model_1','Model_2','Model_3','Model_4','Model_5','Model_6',
                                  'Model_7','Model_8','Model_9','Model_10','Model_11','Model_12',
                                  'Model_13','Model_14','Model_15','Model_16','Model_17','Model_18')) # if not specified the model is sorted by size of the estimate
      # chart_a_2

      # store results
      list_figures_sub[[length(list_figures_sub)+1]] <- chart_a_2


      # auxiliary information about the models in the main analysis
      sum_table <- sum_table %>% mutate(part_of_main = ifelse(Model == "Model_1", 5,
                                                              ifelse(Model == "Model_12", 5,0)))

      # plot coefficients used in a model
    chart_b  <- sum_table %>% ggplot(aes(x=reorder(Model, Estimate)  , y=Estimate))+
      geom_point(data = sum_table, aes(x=reorder(Model, Estimate),y = 'GDP per\nCapita (log)',colour = as.factor(part_of_main + GDP), size = as.factor(part_of_main),shape = as.factor(part_of_main),fill= as.factor(GDP)))+
      geom_point(data = sum_table, aes(x=reorder(Model, Estimate),y = 'Democracy\nIndex (log)',colour = as.factor(part_of_main + Democracy), size = as.factor(part_of_main),shape = as.factor(part_of_main),fill= as.factor(Democracy)))+
      geom_point(data = sum_table, aes(x=reorder(Model, Estimate),y = 'Latitude\n(absolute; log)',colour = as.factor(part_of_main + Latitude), size = as.factor(part_of_main),shape = as.factor(part_of_main),fill= as.factor(Latitude)))+
      geom_point(data = sum_table, aes(x=reorder(Model, Estimate),y = 'Education +12 years \n(age 25-29; log)',colour = as.factor(part_of_main +Schooling), size = as.factor(part_of_main),shape = as.factor(part_of_main),fill= as.factor(Schooling)))+
      geom_point(data = sum_table, aes(x=reorder(Model, Estimate),y = 'Internet Users\n(%; log)',colour = as.factor(part_of_main + Internet), size = as.factor(part_of_main),shape = as.factor(part_of_main),fill= as.factor(Internet)))+
      geom_point(data = sum_table, aes(x=reorder(Model, Estimate),y = 'Population\n(log)',colour = as.factor(part_of_main + Population), size = as.factor(part_of_main),shape = as.factor(part_of_main),fill= as.factor(Population)))+
      geom_point(data = sum_table, aes(x=reorder(Model, Estimate),y = 'Trade\n(% of GDP; log)',colour = as.factor(part_of_main +Trade), size = as.factor(part_of_main),shape = as.factor(part_of_main),fill= as.factor(Trade)))+
      geom_point(data = sum_table, aes(x=reorder(Model, Estimate),y = 'Air pollution\n(log)',colour = as.factor(part_of_main + Air), size = as.factor(part_of_main),shape = as.factor(part_of_main),fill= as.factor(Air)))+
      geom_point(data = sum_table, aes(x=reorder(Model, Estimate),y = 'Agricultural land\n(% of land area; log)',colour = as.factor(part_of_main + Agricultural), size = as.factor(part_of_main),shape = as.factor(part_of_main),fill= as.factor(Agricultural)))+
      geom_point(data = sum_table, aes(x=reorder(Model, Estimate),y = 'Soviet History\n(yes)',colour = as.factor(part_of_main + Soviet), size = as.factor(part_of_main),shape = as.factor(part_of_main),fill= as.factor(Soviet)))+
      theme(legend.position = "none",
            panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
            axis.title.x=element_blank(),
            axis.text.x=element_blank(),
            axis.ticks.x=element_blank())+
      scale_y_discrete(limits=c('Soviet History\n(yes)',
                                # 'Island\n(yes)',
                                'Agricultural land\n(% of land area; log)','Air pollution\n(log)','Trade\n(% of GDP; log)',
                                'Population\n(log)','Internet Users\n(%; log)','Education +12 years \n(age 25-29; log)','Latitude\n(absolute; log)','Democracy\nIndex (log)','GDP per\nCapita (log)'),
                       name = "Row 3: Control\nVariables included")+
      scale_color_manual(values=c("#d9d9d9", "#000000", "#d95f0e", "#d95f0e"))+
      scale_fill_manual(values=c("#d9d9d9", "#000000"))+
      scale_shape_manual(values=c(22,21))+
      scale_size_manual(values=c(3,3.5)) +
      scale_x_discrete(limits=c('Model_1','Model_2','Model_3','Model_4','Model_5','Model_6',
                                'Model_7','Model_8','Model_9','Model_10','Model_11','Model_12',
                                'Model_13','Model_14','Model_15','Model_16','Model_17','Model_18')) # if not specified the model is sorted by size of the estimate
    # chart_b

    # store results
    list_figures_spec[[length(list_figures_spec)+1]] <- chart_b


    }
  }

}



################################################################################
# 4. assembling of specification charts
################################################################################

##################--------------------------------------------------------------
# 4.1 environmental policy performance overall
##################--------------------------------------------------------------

# economic time preference
p_aux1 <- ggarrange(list_figures_entire[[1]]+
                                     labs(title = "A) Economic Time Preference &", subtitle = "Environmental Policy\nPerformance Overall")+
                                     theme(plot.title = element_text(size=14),plot.subtitle = element_text(size=14),
                                           plot.margin = unit(c(0,1.25,0,0), "cm"),
                                           legend.position = "none"
                                     ),
                                   list_figures_sub[[1]]+theme(legend.position = "none",plot.margin = unit(c(0,1.25,0,0), "cm")),
                                   list_figures_spec[[1]]+theme(legend.position = "none",plot.margin = unit(c(0,1.25,0,0), "cm")),
                                   ncol = 1, nrow = 3,
                                   align = "hv")
# p_aux1

# thrift time preference
p_aux2 <- ggarrange(list_figures_entire[[4]]+
                                     labs(title = "A) Thrift Time Preference &", subtitle = "Environmental Policy\nPerformance Overall")+
                                     theme(plot.title = element_text(size=14),plot.subtitle = element_text(size=14),
                                           legend.position = "none",plot.margin = unit(c(0,1.25,0,0), "cm")),
                                   list_figures_sub[[4]]+theme(legend.position = "none",plot.margin = unit(c(0,1.25,0,0), "cm")),
                                   list_figures_spec[[4]]+theme(legend.position = "none",plot.margin = unit(c(0,1.25,0,0), "cm")),
                                   ncol = 1, nrow = 3,
                                   align = "hv")
# p_aux2

# google time preference
p_aux3 <- ggarrange(list_figures_entire[[7]]+
                      labs(title = "A) Google Time Preference &", subtitle = "Environmental Policy\nPerformance Overall")+
                      theme(plot.title = element_text(size=14),plot.subtitle = element_text(size=14),
                            legend.position = "none",
                            plot.margin = unit(c(0,1.25,0,0), "cm")),
                    list_figures_sub[[7]]+theme(legend.position = "none",plot.margin = unit(c(0,1.25,0,0), "cm")),
                    list_figures_spec[[7]]+theme(legend.position = "none",plot.margin = unit(c(0,1.25,0,0), "cm")),
                    ncol = 1, nrow = 3,
                    align = "hv")
# p_aux3

# store figures
plot_gov1_main <- p_aux1
plot_gov1_alt1 <- p_aux2
plot_gov1_alt2 <- p_aux3


##################--------------------------------------------------------------
# 4.2 climate policy performance
##################--------------------------------------------------------------

# economic time preference
p_aux1 <- ggarrange(list_figures_entire[[2]]+
                                     labs(title = "B) Economic Time Preference &", subtitle = "Climate Policy\nPerformance")+
                                     theme(plot.title = element_text(size=14),plot.subtitle = element_text(size=14),
                                           plot.margin = unit(c(0,1.25,0,0), "cm"),
                                           legend.position = "none"
                                     ),
                                   list_figures_sub[[2]]+theme(legend.position = "none",plot.margin = unit(c(0,1.25,0,0), "cm")),
                                   list_figures_spec[[2]]+theme(legend.position = "none",plot.margin = unit(c(0,1.25,0,0), "cm")),
                                   ncol = 1, nrow = 3,
                                   align = "hv")
# p_aux1

# thrift time preference
p_aux2 <- ggarrange(list_figures_entire[[5]]+
                                     labs(title = "B) Thrift Time Preference &", subtitle = "Climate Policy\nPerformance")+
                                     theme(plot.title = element_text(size=14),plot.subtitle = element_text(size=14),
                                           legend.position = "none",plot.margin = unit(c(0,1.25,0,0), "cm")),
                                   list_figures_sub[[5]]+theme(legend.position = "none",plot.margin = unit(c(0,1.25,0,0), "cm")),
                                   list_figures_spec[[5]]+theme(legend.position = "none",plot.margin = unit(c(0,1.25,0,0), "cm")),
                                   ncol = 1, nrow = 3,
                                   align = "hv")
# p_aux2

# google time preference
p_aux3 <- ggarrange(list_figures_entire[[8]]+
                      labs(title = "B) Google Time Preference &", subtitle = "Climate Policy\nPerformance")+
                      theme(plot.title = element_text(size=14),plot.subtitle = element_text(size=14),
                            legend.position = "none",
                            plot.margin = unit(c(0,1.25,0,0), "cm")),
                    list_figures_sub[[8]]+theme(legend.position = "none",plot.margin = unit(c(0,1.25,0,0), "cm")),
                    list_figures_spec[[8]]+theme(legend.position = "none",plot.margin = unit(c(0,1.25,0,0), "cm")),
                    ncol = 1, nrow = 3,
                    align = "hv")
# p_aux3

# store figures
plot_cc_main <- p_aux1
plot_cc_alt1 <- p_aux2
plot_cc_alt2 <- p_aux3


##################--------------------------------------------------------------
# 4.3 biodiversity policy performance
##################--------------------------------------------------------------

# economic time preference
p_aux1 <- ggarrange(list_figures_entire[[3]]+
                                     labs(title = "C) Economic Time Preference &", subtitle = "Biodiversity Policy\nPerformance")+
                                     theme(plot.title = element_text(size=14),plot.subtitle = element_text(size=14),
                                           plot.margin = unit(c(0,1.25,0,0), "cm"),
                                           legend.position = "none"
                                     ),
                                   list_figures_sub[[3]]+theme(legend.position = "none",plot.margin = unit(c(0,1.25,0,0), "cm")),
                                   list_figures_spec[[3]]+theme(legend.position = "none",plot.margin = unit(c(0,1.25,0,0), "cm")),
                                   ncol = 1, nrow = 3,
                                   align = "hv")
# p_aux1

# thrift time preference
p_aux2 <- ggarrange(list_figures_entire[[6]]+
                                     labs(title = "C) Thrift Time Preference &", subtitle = "Biodiversity Policy\nPerformance")+
                                     theme(plot.title = element_text(size=14),plot.subtitle = element_text(size=14),
                                           legend.position = "none",plot.margin = unit(c(0,1.25,0,0), "cm")),
                                   list_figures_sub[[6]]+theme(legend.position = "none",plot.margin = unit(c(0,1.25,0,0), "cm")),
                                   list_figures_spec[[6]]+theme(legend.position = "none",plot.margin = unit(c(0,1.25,0,0), "cm")),
                                   ncol = 1, nrow = 3,
                                   align = "hv")
# p_aux2

# google time preference
p_aux3 <- ggarrange(list_figures_entire[[9]]+
                      labs(title = "C) Google Time Preference &", subtitle = "Biodiversity Policy\nPerformance")+
                      theme(plot.title = element_text(size=14),plot.subtitle = element_text(size=14),
                            legend.position = "none",
                            plot.margin = unit(c(0,1.25,0,0), "cm")),
                    list_figures_sub[[9]]+theme(legend.position = "none",plot.margin = unit(c(0,1.25,0,0), "cm")),
                    list_figures_spec[[9]]+theme(legend.position = "none",plot.margin = unit(c(0,1.25,0,0), "cm")),
                    ncol = 1, nrow = 3,
                    align = "hv")
# p_aux3

# store figures
plot_bd_main <- p_aux1
plot_bd_alt1 <- p_aux2
plot_bd_alt2 <- p_aux3


################################################################################
# 5. save figures
################################################################################

setwd(cd_results) # set working directory
# save
ggsave(plot_gov1_main + plot_cc_main + plot_bd_main, filename = "v1_figure_s2.pdf", width=23/5*3.5, height=8.5*3/2)
ggsave(plot_gov1_alt1 + plot_cc_alt1 + plot_bd_alt1, filename = "v1_figure_s4.pdf", width=23/5*3.5, height=8.5*3/2)
ggsave(plot_gov1_alt2 + plot_cc_alt2 + plot_bd_alt2, filename = "v1_figure_s5.pdf", width=23/5*3.5, height=8.5*3/2)

