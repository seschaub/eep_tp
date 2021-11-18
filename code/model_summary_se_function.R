model_summary_se <- function(model, with_stars) {

  if (with_stars) {
    tidy(model) %>%
      mutate(stars = ifelse(p.value < 0.001,"***",
                            ifelse(p.value < 0.01,"**",
                                   ifelse(p.value < 0.05,"*","")))) %>%
      dplyr::select(term,estimate,stars) %>%
      add_column(as.data.frame(confint(model)) %>% dplyr::rename(conf_low95 = 1, conf_high95 = 2)) %>%
      as.data.frame() %>%
      mutate(info = paste0(round(estimate,2), " [",round(conf_low95,2),
                           " to ",round(conf_high95,2),"]", stars)) %>%
      dplyr::select(term,info) %>%
      dplyr::rename(Variable = 1, `Estimate (95%-confidence interval)` = 2)
  } else
  {
    tidy(model) %>%
      mutate(stars = ifelse(p.value < 0.001,"***",
                            ifelse(p.value < 0.01,"**",
                                   ifelse(p.value < 0.05,"*","")))) %>%
      dplyr::select(term,estimate,stars) %>%
      add_column(as.data.frame(confint(model)) %>% dplyr::rename(conf_low95 = 1, conf_high95 = 2)) %>%
      as.data.frame() %>%
      mutate(info = paste0(round(estimate,2), " [",round(conf_low95,2),
                           " to ",round(conf_high95,2),"]")) %>%
      dplyr::select(term,info) %>%
      dplyr::rename(Variable = 1, `Estimate (95%-confidence interval)` = 2)

  }
}
