# Functions for Stats. Analyses

# 1. Baseline TWFE --------------------------------------------------------

anti_pro_baseline_DID <- function(df,   outcome="anti_log_sum", treatment = "incident_window_re", 
                                  fe="id", more_controls = NULL, # use text for additional covariates
                                  cluster="id", zerocov=FALSE) {
  
  if (zerocov==TRUE){
    formula_str <- paste0(outcome , " ~ ", treatment, "|", fe, " + time") 
  } else if (!is.null(more_controls)){
    formula_str <- paste0(outcome , " ~ ", treatment, 
                          " + bachelor + black + white + unemployment + log_income + rep_incumbent_before + gun_ratio_pct+",
                          # we checked pct-RDmargin-before as well for robustness check
                          more_controls, "|", fe, " + time") 
  } else {
    formula_str <- paste0(outcome, " ~ ", treatment, 
                          " + bachelor + black + white + unemployment + log_income + rep_incumbent_before + gun_ratio_pct|",
                          fe, " + time") 
  }
  
  formula_obj <- as.formula(formula_str)
  fixest::feols(formula_obj, data = df, cluster = cluster)
}


# Multicollinearity
# for fixstats, see also here
# https://lrberge.github.io/fixest/reference/fitstat_register.html

# This also shows which variable has an issue with which variable
# see https://lrberge.github.io/fixest/reference/collinearity.html

# extract excluded var. info from one model
# Use this function for multiple models
extract_collin_vars <- function(model) {
  if (inherits(model, "fixest")) {
    if (!is.null(model$collin.var) && length(model$collin.var) > 0) {
      paste(model$collin.var, collapse = ", ")
    } else {
      "None"
    }
  } else {
    "Unknown"
  }
}

# from multiple models
create_collin_row <- function(models) {
  info <- sapply(models, extract_collin_vars) # already defined above
  names(info) <- paste0("mod", seq_along(models))  
  
  # create df
  results <- data.frame(
    raw = "Variables Excluded (Multicollinearity)",
    stringsAsFactors = FALSE  
    )
  
  for (i in seq_along(models)) {
    name <- paste0("mod", i)
    results[[name]] <- info[i]
  }
  return(results)
}


gt_table <- function(model, # not results; list of feols models
                     # even when using one model, have to make it list form
                     coef_mapping,  gof_mapping,
                     add_row_data = NULL,
                     title, subtitle, source_note = ss_sourcenote, collin=TRUE) {
  
  # First, get multicollin info
  collin_row <- create_collin_row(model)
  
  # add to add_row_data
  if (collin==TRUE & !is.null(add_row_data)) {
    add_row_data <- rbind(add_row_data, collin_row)
  } else if (collin==TRUE){
    add_row_data <- collin_row
  }
  
  table_result <- modelsummary(model, coef_map = coef_mapping,
                               estimate = "{estimate} ({std.error})",
                               statistic = "p = {p.value}",
                               stars = FALSE, 
                               # title should be added later together with subtitle
                               gof_map = gof_mapping,
                               fmt = "%.3f", # three decimal points
                               add_rows = add_row_data,
                               output = "gt") %>% 
    
    gt::tab_header(title = title, subtitle = subtitle)
  
  if (!is.null(source_note)) {
    table_result <- table_result %>%
      gt::tab_source_note(source_note = gt::md(source_note))
    # gt::md is necessary to input html tags and add break line.
  }
  return(table_result)
}


loo_importance <- function(df, outcome="anti_log_sum") {
  
  if (outcome=="anti_log_sum"){
    full_model <- fixest::feols(anti_log_sum ~ incident_window_re + bachelor + black + white + unemployment +
                                  log_income + rep_incumbent_before + gun_ratio_pct| id+time, data = df, cluster = "id")
    
  } else if (outcome=="pro_log_sum"){
    full_model <- fixest::feols(pro_log_sum ~ incident_window_re + bachelor + black + white + unemployment +
                                  log_income + rep_incumbent_before + gun_ratio_pct| id+time, data = df, cluster = "id")
    
  }
  
  full_r2 <- r2(full_model, type = "wr2") #with-in R2
  
  variables <- c("incident_window_re", "bachelor", "black", "white", "unemployment", 
                 "log_income", "rep_incumbent_before", "gun_ratio_pct")
  
  importance_df <- data.frame()
  
  for(var in variables) {
    
    remaining_vars <- variables[variables != var]
    
    if(length(remaining_vars) > 0) {
      if (outcome == "anti_log_sum") {
        reduced_formula <- paste("anti_log_sum ~", 
                                 paste(remaining_vars, collapse = " + "),
                                 "| id + time")
      } else if (outcome == "pro_log_sum") {
        reduced_formula <- paste("pro_log_sum ~", 
                                 paste(remaining_vars, collapse = " + "),
                                 "| id + time")
      }
    } else { # i.e., remaining_var is null.
      if (outcome == "anti_log_sum") {
        reduced_formula <- "anti_log_sum ~ 1 | id + time"
      } else if (outcome == "pro_log_sum") {
        reduced_formula <- "pro_log_sum ~ 1 | id + time"
      }
    }
    
    reduced_model <- feols(as.formula(reduced_formula), data = df, cluster = "id")
    reduced_r2 <- r2(reduced_model, type = "wr2")
    
    importance <- full_r2 - reduced_r2
    
    importance_df <- rbind(importance_df, 
                           data.frame(variable = var, 
                                      importance = importance,
                                      full_r2 = full_r2,
                                      reduced_r2 = reduced_r2,
                                      stringsAsFactors = FALSE))
  }
  
  importance_df$relative_importance <- 
    importance_df$importance / sum(importance_df$importance) # relative importance compared with all the other var.
  
  return(importance_df[order(-importance_df$importance), ]) # not indispensable, but ordered by importance
}


data_baseline_fatal <- function(df # use a dataset without any filtering; instead of data_s_2000, use data_s
                                ){
  df <- as.data.frame(df) %>%
    filter(abs(pct_RDmargin_before)<=5) %>% 
    
    mutate(incident_fatal_zero = ifelse(incident >= 1 & fatalities == 0, 1, 0)) %>% 
    mutate(incident_fatal_1over = ifelse(incident >= 1 & fatalities >= 1, 1, 0)) %>% 
    mutate(incident_fatal_1 = ifelse(incident >= 1 & fatalities == 1, 1, 0)) %>% 
    mutate(incident_fatal_2over = ifelse(incident >= 1 & fatalities >= 2, 1, 0)) %>% 
    
    mutate(anti_don_number = ifelse(anti_pac_donations_dollar>=10000, anti_don_number, 0),
    anti_log_sum = ifelse(anti_pac_donations_dollar>=10000, anti_log_sum, 0),
    pro_don_number = ifelse(pro_pac_donations_dollar>=10000, pro_don_number, 0),
    pro_log_sum = ifelse(pro_pac_donations_dollar>=10000, pro_log_sum, 0)
    ) %>% 
    
    arrange(id, time) %>%
    group_by(id) %>%
    mutate(last_incident_time_re = ifelse(incident == 1, time, NA)) %>% # specify the last incident time
    fill(last_incident_time_re, .direction = "down") %>%
    mutate(incident_window_re = if_else(!is.na(last_incident_time_re) & (time - last_incident_time_re <= 24), 1, 0),
           incident_window_re = as.integer(incident_window_re)) %>%
    
    # Next, fatal incidents
    mutate(last_incident_time_re_fatal_zero = ifelse(incident_fatal_zero == 1, time, NA)) %>% # specify the last incident time
    fill(last_incident_time_re_fatal_zero, .direction = "down") %>%
    mutate(incident_window_re_fatal_zero = if_else(!is.na(last_incident_time_re_fatal_zero) & (time - last_incident_time_re_fatal_zero <= 24), 1, 0),
           incident_window_re_fatal_zero = as.integer(incident_window_re_fatal_zero)) %>%
    
    mutate(last_incident_time_re_fatal_1over = ifelse(incident_fatal_1over == 1, time, NA)) %>% # specify the last incident time
    fill(last_incident_time_re_fatal_1over, .direction = "down") %>%
    mutate(incident_window_re_fatal_1over = if_else(!is.na(last_incident_time_re_fatal_1over) & (time - last_incident_time_re_fatal_1over <= 24), 1, 0),
           incident_window_re_fatal_1over = as.integer(incident_window_re_fatal_1over)) %>%
    
    mutate(last_incident_time_re_fatal_1 = ifelse(incident_fatal_1 == 1, time, NA)) %>% # specify the last incident time
    fill(last_incident_time_re_fatal_1, .direction = "down") %>%
    mutate(incident_window_re_fatal_1 = if_else(!is.na(last_incident_time_re_fatal_1) & (time - last_incident_time_re_fatal_1 <= 24), 1, 0),
           incident_window_re_fatal_1 = as.integer(incident_window_re_fatal_1)) %>%
    
    mutate(last_incident_time_re_fatal_2over = ifelse(incident_fatal_2over == 1, time, NA)) %>% # specify the last incident time
    fill(last_incident_time_re_fatal_2over, .direction = "down") %>%
    mutate(incident_window_re_fatal_2over = if_else(!is.na(last_incident_time_re_fatal_2over) & (time - last_incident_time_re_fatal_2over <= 24), 1, 0),
           incident_window_re_fatal_2over = as.integer(incident_window_re_fatal_2over)) %>%
    
    
    ungroup() %>% 
    filter(year>=2000)
  return(df)
}


baseline_DID_nonHoR <- function(df, outcome="anti_log_sum", treatment = "incident_window_re",
                             more_controls = NULL, cluster='state_num') {
  if (!is.null(more_controls)){
    formula_str <- paste0(outcome  , " ~ ", treatment, 
                          " + bachelor + black + white + unemployment + log_income+ gun_ratio_pct ",
                          more_controls, "| state_num + time") 
  } else {
    formula_str <- paste0(outcome, " ~ ", treatment, 
                          " + bachelor + black + white + unemployment + log_income+ gun_ratio_pct| state_num+ time") 
  }
  formula_obj <- as.formula(formula_str)
  feols(formula_obj, data = df, cluster=cluster)
}



clustering_TWFE <- function(df, vcov="hc0", # used both clustering and, if no clustering, vcov
                            clustering_var=FALSE, PACtype="anti" # or "pro"
                            ){
  
  if (clustering_var==FALSE & PACtype=="anti"){
    result <- fixest::feols(anti_log_sum ~incident_window_re+ bachelor + black + white + unemployment + log_income + rep_incumbent_before 
                            + gun_ratio_pct 
                            | id + time,  data = df, vcov = vcov)
  } else if (clustering_var==TRUE & PACtype=="anti"){
    
    vcov=as.formula(paste0("~",vcov)) # use it for clustering
    
    result <- fixest::feols(anti_log_sum ~incident_window_re+ bachelor + black + white + unemployment + log_income + rep_incumbent_before 
                            + gun_ratio_pct 
                            | id + time,  data = df, vcov = vcov)
  } else if (clustering_var==FALSE & PACtype=="pro"){
    result <- fixest::feols(pro_log_sum ~incident_window_re+ bachelor + black + white + unemployment + log_income + rep_incumbent_before 
                            + gun_ratio_pct 
                            | id + time,  data = df, vcov = vcov)
  } else if (clustering_var==TRUE & PACtype=="pro"){
    
    vcov=as.formula(paste0("~",vcov)) # use it for clustering
    result <- fixest::feols(pro_log_sum ~incident_window_re+ bachelor + black + white + unemployment + log_income + rep_incumbent_before 
                            + gun_ratio_pct 
                            | id + time,  data = df, vcov = vcov)
  }
  
  return(result)
}




diff_month <- function(df, # a whole non-filtered dataset should be used
                       month_list = c(2,3,4,8,12, 23),
                       outcome= "anti_log_sum", month_cov=FALSE, # add months_to as a covariate
                       
                       labels=  c("2 months","3 months", "4 months","8 months", '12 months',  "All"),
                       
                       title="Baseline DID by Different Thresholds on Incident Timing and Treatment Period",
                       subtitle =  "GVP PACs", 
                       
                       footnote =  "Based on basic filtering and covariates", 
                       ylab = "Effect on Logged Dollar Amount", # since this function flipped axes, it should be horizontal
                       fatal=TRUE, # only focusing on fatal cases
                       voting_margin=5
) {
  
  # preparation of for loop
  month_list <- month_list[!is.null(month_list)]  # remove NULL
  
  # data wrangling
  if (fatal==TRUE){
    df <- data_margin_baseline(df, voting_margin)
  } else if (fatal==FALSE){
    df <- as.data.frame(df) %>%
      filter(abs(pct_RDmargin_before)<=5) %>% 
      
      # removed fatal restriction
      mutate(anti_don_number = ifelse(anti_pac_donations_dollar>=10000, anti_don_number, 0),
             anti_log_sum = ifelse(anti_pac_donations_dollar>=10000, anti_log_sum, 0),
             pro_don_number = ifelse(pro_pac_donations_dollar>=10000, pro_don_number, 0),
             pro_log_sum = ifelse(pro_pac_donations_dollar>=10000, pro_log_sum, 0)) %>% 
      
      arrange(id, time) %>%
      group_by(id) %>%
      
      mutate(last_incident_time_re = ifelse(incident == 1, time, NA)) %>% # specify the last incident time
      fill(last_incident_time_re, .direction = "down") %>%
      mutate(incident_window_re = if_else(!is.na(last_incident_time_re) & (time - last_incident_time_re <= 24), 1, 0),
             incident_window_re = as.integer(incident_window_re)) %>%
      
      ungroup() %>% 
      filter(year>=2000)
    
  }
  
  # for results
  results_list <- list()
  
  for (i in seq_along(month_list)) {
    current_month <- month_list[i]
    
     # short, but election timing is not considered and treatment period can go beyond election
      df <- df %>%
        mutate(incident_month = ifelse(incident >= 1 & months_to_election <= current_month, 1, 0)) %>% 
        arrange(id, time) %>%
        group_by(id) %>%
        
        mutate(last_incident_time_re_month = ifelse(incident_month == 1, time, NA)) %>% # specify the last incident time
        fill(last_incident_time_re_month, .direction = "down") %>%
        
        # instead of 24 months, use current month
       mutate(incident_window_re_month = if_else(!is.na(last_incident_time_re_month) & (time - last_incident_time_re_month <= current_month), 1, 0),
               incident_window_re_month = as.integer(incident_window_re_month), # have to use the same treatment var. name as period case
               #for reference; further treatments 
               incident_window_re_others= ifelse(incident_window_re==1 & incident_window_re_month==0, 1, 0))
      
    
    
    # current month is very large, it should be differently addressed because there is no further treatment
    if (current_month>=23 &outcome=="anti_log_sum"){
      did_month_model <-  anti_pro_baseline_DID(df, treatment = "incident_window_re_month",
                                                more_controls = "months_to_election")
      # no incident_window_re_others
      
    } else if (current_month>=23 &outcome=="anti_don_number"){
      did_month_model <-  anti_pro_baseline_DID(df, outcome = "anti_don_number", treatment = "incident_window_re_month",
                                                more_controls = "months_to_election")
    } else if (current_month>=23 &outcome=="pro_log_sum"){
      did_month_model <-  anti_pro_baseline_DID(df, outcome = "pro_log_sum", treatment = "incident_window_re_month",
                                                more_controls = "months_to_election")
    } else if (current_month>=23 &outcome=="pro_don_number"){
      did_month_model <-  anti_pro_baseline_DID(df, outcome = "pro_don_number", treatment = "incident_window_re_month",
                                                more_controls = "months_to_election")
    } else if (outcome=="anti_log_sum"& month_cov == TRUE){
      did_month_model <-  anti_pro_baseline_DID(df, treatment = "incident_window_re_month",
                                                more_controls = " incident_window_re_others+months_to_election")
     
    } else if (outcome=="anti_don_number"& month_cov == TRUE){
      did_month_model <-  anti_pro_baseline_DID(df, outcome = "anti_don_number",
                                                treatment = "incident_window_re_month",
                                                more_controls = " incident_window_re_others+months_to_election")
     
    } else if (outcome=="anti_log_sum"& month_cov == FALSE){
      did_month_model <-  anti_pro_baseline_DID(df, treatment = "incident_window_re_month",
                                                more_controls = " incident_window_re_others")
    } else if (outcome=="anti_don_number" & month_cov== FALSE){
      did_month_model <-  anti_pro_baseline_DID(df, outcome = "anti_don_number",
                                                treatment = "incident_window_re_month",
                                                more_controls = " incident_window_re_others")

    } else if (outcome=="pro_log_sum"& month_cov == TRUE){
      did_month_model <-  anti_pro_baseline_DID(df, outcome = "pro_log_sum",
                                                treatment = "incident_window_re_month",
                                                more_controls = " incident_window_re_others+months_to_election")
    } else if (outcome=="pro_don_number"& month_cov == TRUE){
      did_month_model <-  anti_pro_baseline_DID(df, outcome = "pro_don_number",
                                                treatment = "incident_window_re_month",
                                                more_controls = " incident_window_re_others+months_to_election")
    } else if (outcome=="pro_log_sum"& month_cov == FALSE){
      did_month_model <-  anti_pro_baseline_DID(df, outcome = "pro_log_sum",
                                                treatment = "incident_window_re_month",
                                                more_controls = " incident_window_re_others")
    } else if (outcome=="pro_don_number" & month_cov== FALSE){
      did_month_model <-  anti_pro_baseline_DID(df, outcome = "pro_don_number",
                                                treatment = "incident_window_re_month",
                                                more_controls = " incident_window_re_others")
      
    } 
    
    # Extracting part of results
    # do not use the name of ..._i, unless it should be dynamic within for loop 
    did_month_coef <- tidy(did_month_model) %>%
      filter(term == "incident_window_re_month")  # have to be the same var. name in either case
    
    # safeguard for multicollinearity
    if(nrow(did_month_coef) == 0) {
      next
    }
    
    did_month_coef <-did_month_coef %>% 
      mutate(n_treated = sum(df[["incident_window_re_month"]], na.rm = TRUE),
             threshold_name = paste0(i, "-month"), # this is its level for the time being,
             # so no need to specify otherwise
             original_index = i  # for the following error handling
      )
    results_list[[i]] <- did_month_coef
  }
  
  # Just in case where all patterns show multi-collinearity
  if(length(results_list) == 0) {
    stop("No results due to multicollinearity")
  }
  
  # dfs are better than lists
  results_df <- bind_rows(results_list) %>% 
    # error handling
    mutate(threshold_name = labels[original_index]) %>% # skip error months, and avoid
    # the entire stopping
    
    mutate(threshold_name = factor(threshold_name,
                                   levels = labels,  # not levels, because I used lables as levels above
                                   labels = labels)) %>% 
    # c("Immediate\n(<= 2m)","Close\n(<= 3m)", "Medium\n(<= 6m)", "Distant\n(<= 12m)", 'All')
    
    # no need to make this kable, since I do not make output here
    dplyr::select(threshold_name, estimate, std.error,p.value,  n_treated) %>% 
    # have to specify for geom errorbar
    mutate(
      conf.low = estimate - 1.96 * std.error,
      conf.high = estimate + 1.96 * std.error)
  
  plot <- results_df%>%
    ggplot(aes( # first, x and y axes are opposite
      x = threshold_name, y = estimate
      # , ymin = conf.low, ymax = conf.high
    )) +
    
    # pointrage = point + error bar, but you cannnot specify width
    # geom_pointrange() +
    
    geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.4) +
    geom_point(aes(size = n_treated)  # size of n.obs does not matter here, since that
               # means all the months since 2000. n.treated matters.
    ) +
    
    geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
    coord_flip() + # for better visualization
    scale_size_continuous(name = "# of treated months",
                          guide = guide_legend(title.position = "top")) +
    labs(
      title = title,
      subtitle=subtitle,
      x = "Month to the Next Election",
      y = ylab,
      caption = footnote) +
    theme_minimal()+
    theme(plot.title = element_text(size=10, hjust = 0.5, face = "bold"),
          plot.subtitle = element_text(size=10, hjust = 0.5),
          axis.text.y = element_text(size = 12, colour = "black"), # no need to make ytitle bold
          plot.caption = element_text(size=10,hjust = 0.5) # 0 might look odd
    )
  
  # output both plot and estimate results
  return(list(plot, results_df))
}

diff_month_results <- function(df, # original data
                               month_threshold=2,
                               start_year=2000){
df_month <- data_margin_baseline(df) %>%
  mutate(incident_month = ifelse(incident >= 1 & months_to_election <= month_threshold, 1, 0)) %>% 
  # filter here, since the main object of the filter is incident, not window
  dplyr::filter(year>=start_year) %>% 
  arrange(id, time) %>%
  group_by(id) %>%
  
  mutate(last_incident_time_re_month = ifelse(incident_month == 1, time, NA)) %>% # specify the last incident time
  fill(last_incident_time_re_month, .direction = "down") %>%
  mutate(incident_window_re_month = if_else(!is.na(last_incident_time_re_month) & (time - last_incident_time_re_month <= month_threshold), 1, 0),
         incident_window_re_month = as.integer(incident_window_re_month), # have to use the same treatment var. name as period case
         #for reference; further treatments 
         incident_window_re_others= ifelse(incident_window_re==1 & incident_window_re_month==0, 1, 0))
return(df_month)
}


diff_month_strategic <- function(df, # a whole non-filtered dataset should be used
                       month_list = c(2,3,4,8,12, 23),
                       outcome= "anti_log_sum", 
                       
                       labels=  c("2 months","3 months", "4 months","8 months", '12 months',  "All"),
                       
                       title="Baseline DID by Different Thresholds on Incident Timing and Treatment Period",
                       subtitle =  "GVP PACs", 
                       
                       footnote =  "Considering strategic interaction; Based on basic filtering and covariates", 
                       ylab = "Effect on Logged Dollar Amount" # since this function flipped axes, it should be horizontal
                       ) {
  
  # preparation of for loop
  month_list <- month_list[!is.null(month_list)]  # remove NULL
  
  df <- data_margin_baseline(df)

  # for results
  results_list <- list()
  
  for (i in seq_along(month_list)) {
    current_month <- month_list[i]
    
    df <- df %>%
        mutate(incident_month = ifelse(incident >= 1 & months_to_election <= current_month, 1, 0)) %>% 
        arrange(id, time) %>%
        group_by(id) %>%
        
        mutate(last_incident_time_re_month = ifelse(incident_month == 1, time, NA)) %>% # specify the last incident time
        fill(last_incident_time_re_month, .direction = "down") %>%
        
        # instead of 24 months, use current month
        mutate(incident_window_re_month = if_else(!is.na(last_incident_time_re_month) & (time - last_incident_time_re_month <= current_month), 1, 0),
               incident_window_re_month = as.integer(incident_window_re_month), # have to use the same treatment var. name as period case
               #for reference; further treatments 
               incident_window_re_others= ifelse(incident_window_re==1 & incident_window_re_month==0, 1, 0))
      
    
    # if current month is very large, it should be differently addressed because there is no further treatment
    if (current_month>=23 &outcome=="anti_log_sum"){
      did_month_model <-  anti_pro_baseline_DID(df, treatment = "incident_window_re_month",
                                                more_controls = "pro_logsum_3month_prior")
      # no incident_window_re_others
      
    } else if (current_month>=23 &outcome=="anti_don_number"){
      did_month_model <-  anti_pro_baseline_DID(df, outcome = "anti_don_number", treatment = "incident_window_re_month",
                                                more_controls = "pro_logsum_3month_prior")
    } else if (current_month>=23 &outcome=="pro_log_sum"){
      did_month_model <-  anti_pro_baseline_DID(df, outcome = "pro_log_sum", treatment = "incident_window_re_month",
                                                more_controls = "anti_logsum_3month_prior")
    } else if (current_month>=23 &outcome=="pro_don_number"){
      did_month_model <-  anti_pro_baseline_DID(df, outcome = "pro_don_number", treatment = "incident_window_re_month",
                                                more_controls = "anti_logsum_3month_prior")
    } else if (outcome=="anti_log_sum"){
      did_month_model <-  anti_pro_baseline_DID(df, treatment = "incident_window_re_month",
                                                more_controls = " incident_window_re_others+pro_logsum_3month_prior")
      
    } else if (outcome=="anti_don_number"){
      did_month_model <-  anti_pro_baseline_DID(df, outcome = "anti_don_number",
                                                treatment = "incident_window_re_month",
                                                more_controls = " incident_window_re_others+pro_logsum_3month_prior")
      
    } else if (outcome=="pro_log_sum"){
      did_month_model <-  anti_pro_baseline_DID(df, outcome = "pro_log_sum",
                                                treatment = "incident_window_re_month",
                                                more_controls = " incident_window_re_others+anti_logsum_3month_prior")
    } else if (outcome=="pro_don_number"){
      did_month_model <-  anti_pro_baseline_DID(df, outcome = "pro_don_number",
                                                treatment = "incident_window_re_month",
                                                more_controls = " incident_window_re_others+anti_logsum_3month_prior")
    } 
    
    # Extracting part of results
    # do not use the name of ..._i, unless it should be dynamic within for loop 
    did_month_coef <- tidy(did_month_model) %>%
      filter(term == "incident_window_re_month")  # have to be the same var. name in either case
    
    # safeguard for multicollinearity
    if(nrow(did_month_coef) == 0) {
      next
    }
    
    did_month_coef <-did_month_coef %>% 
      mutate(n_treated = sum(df[["incident_window_re_month"]], na.rm = TRUE),
             threshold_name = paste0(i, "-month"), # this is its level for the time being,
             # so no need to specify otherwise
             original_index = i  # for the following error handling
      )
    results_list[[i]] <- did_month_coef
  }
  
  # Just in case where all patterns show multi-collinearity
  if(length(results_list) == 0) {
    stop("No results due to multicollinearity")
  }
  
  # dfs are better than lists
  results_df <- bind_rows(results_list) %>% 
    # error handling
    mutate(threshold_name = labels[original_index]) %>% # skip error months, and avoid
    # the entire stopping
    
    mutate(threshold_name = factor(threshold_name,
                                   levels = labels,  # not levels, because I used lables as levels above
                                   labels = labels)) %>% 
    # c("Immediate\n(<= 2m)","Close\n(<= 3m)", "Medium\n(<= 6m)", "Distant\n(<= 12m)", 'All')
    
    # no need to make this kable, since I do not make output here
    dplyr::select(threshold_name, estimate, std.error,p.value,  n_treated) %>% 
    # have to specify for geom errorbar
    mutate(
      conf.low = estimate - 1.96 * std.error,
      conf.high = estimate + 1.96 * std.error)
  
  plot <- results_df%>%
    ggplot(aes( # first, x and y axes are opposite
      x = threshold_name, y = estimate
      # , ymin = conf.low, ymax = conf.high
    )) +
    
    # pointrage = point + error bar, but you cannnot specify width
    # geom_pointrange() +
    
    geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.4) +
    geom_point(aes(size = n_treated)  # size of n.obs does not matter here, since that
               # means all the months since 2000. n.treated matters.
    ) +
    
    geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
    coord_flip() + # for better visualization
    scale_size_continuous(name = "# of treated months",
                          guide = guide_legend(title.position = "top")) +
    labs(
      title = title,
      subtitle=subtitle,
      x = "Month to the Next Election",
      y = ylab,
      caption = footnote) +
    theme_minimal()+
    theme(plot.title = element_text(size=10, hjust = 0.5, face = "bold"),
          plot.subtitle = element_text(size=10, hjust = 0.5),
          axis.text = element_text(size = 12, colour = "black"),
          axis.title = element_text(size = 12, colour = "black"),# no need to make ytitle bold
          plot.caption = element_text(size=12,hjust = 0.5) # 0 might look odd
    )
  
  # output both plot and estimate results
  return(list(plot, results_df))
}


diff_month_double_plot <- function(main_results, # specify only numerical results
                                       second_results,
                                   main_label="main", second_label="sec",
                                   main_color="lightblue", second_color="lightcoral",
                                       title = "School Shooting vs Mass Shooting",
                                       subtitle = "Different Timing to Election; HoR", 
                                       footnote = "Based on basic filtering and covariates", 
                                       ylab = "Effect on Logged Dollar Amount"
) {
  
  combined_df <- bind_rows(
    main_results  %>% mutate(Type = main_label),
    second_results %>% mutate(Type = second_label)
  ) %>%
    mutate(Type = factor(Type, levels = c(main_label, second_label)))
  
  plot <- combined_df %>%
    ggplot(aes(x = threshold_name, # based on numerical input
               y = estimate, color = Type, # use diff colors
               shape = Type)) +
    
    # have to dodge
    geom_errorbar(aes(ymin = conf.low, ymax = conf.high), 
                  width = 0.4, 
                  position = position_dodge(width = 0.4),
                  linewidth = 0.8) +
    
    # geom_point(aes(size = n_treated), 
    #            position = position_dodge(width = 0.4)) +
    
    
    geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
    
    coord_flip() +
    
    scale_color_manual(
      values = setNames(c(main_color, second_color),
                        c(main_label, second_label))
    ) +
    scale_shape_manual(values = c(main_label = 16, 
                                  second_label = 17)) +
    
    # scale_size_continuous(name = "# of treated months",
    #                       # range = c(2, 6), # if you would like to transform acutal size
    #                       guide = guide_legend(title.position = "top")) +
    
    labs(
      title = title,
      subtitle = subtitle,
      x = "Months to the Next Election",
      y = ylab,
      caption = footnote,
      color = "Type",
      shape = "Type"
    ) +
    
    theme_minimal() +
    theme(
      plot.title = element_text(size = 12, hjust = 0.5, face = "bold"),
      plot.subtitle = element_text(size = 10, hjust = 0.5),
      axis.text.y = element_text(size = 10, colour = "black"),
      axis.text.x = element_text(size = 10),
      axis.title = element_text(size = 11),
      plot.caption = element_text(size = 10, hjust = 0.5),
      legend.title = element_text(size = 10),
      legend.text = element_text(size = 10),
      legend.position = "bottom",
      legend.box = "horizontal"
    ) +
    
    guides(
      color = guide_legend(override.aes = list(size = 4), ncol = 1),
      shape = guide_legend(override.aes = list(size = 4), ncol = 1),
      size = guide_legend(title.position = "top", nrow = 1)
    )
  
  return(plot)
}


diff_month_double_plot <- function(main_results, # specify only numerical results
                                       second_results,
                                   main_label="main", second_label="sec",
                                   main_color="lightblue", second_color="lightcoral",
                                       title = "School Shooting vs Mass Shooting",
                                       subtitle = "Different Timing to Election; HoR", 
                                       footnote = "Based on basic filtering and covariates", 
                                       ylab = "Effect on Logged Dollar Amount"
) {
  
  combined_df <- bind_rows(
    main_results  %>% mutate(Type = main_label),
    second_results %>% mutate(Type = second_label)
  ) %>%
    mutate(Type = factor(Type, levels = c(main_label, second_label)))
  
  plot <- combined_df %>%
    ggplot(aes(x = threshold_name, # based on numerical input
               y = estimate, color = Type, # use diff colors
               shape = Type)) +
    
    # have to dodge
    geom_errorbar(aes(ymin = conf.low, ymax = conf.high), 
                  width = 0.4, 
                  position = position_dodge(width = 0.4),
                  linewidth = 0.8) +
    
    # geom_point(aes(size = n_treated), 
    #            position = position_dodge(width = 0.4)) +
    
    
    geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
    
    coord_flip() +
    
    scale_color_manual(
      values = setNames(c(main_color, second_color),
                        c(main_label, second_label))
    ) +
    # scale_shape_manual(values = c(main_label = 16, 
    #                               second_label = 17)) +
    
    # scale_size_continuous(name = "# of treated months",
    #                       # range = c(2, 6), # if you would like to transform acutal size
    #                       guide = guide_legend(title.position = "top")) +
    
    labs(
      title = title,
      subtitle = subtitle,
      x = "Months to the Next Election",
      y = ylab,
      caption = footnote,
      color = "Type",
      shape = "Type"
    ) +
    
    theme_minimal() +
    theme(
      plot.title = element_text(size = 12, hjust = 0.5, face = "bold"),
      plot.subtitle = element_text(size = 10, hjust = 0.5),
      axis.text.y = element_text(size = 10, colour = "black"),
      axis.text.x = element_text(size = 10),
      axis.title = element_text(size = 11),
      plot.caption = element_text(size = 10, hjust = 0.5),
      legend.title = element_text(size = 10),
      legend.text = element_text(size = 10),
      legend.position = "bottom",
      legend.box = "horizontal"
    ) +
    
    guides(
      color = guide_legend(override.aes = list(size = 4), ncol = 1),
      shape = guide_legend(override.aes = list(size = 4), ncol = 1),
      size = guide_legend(title.position = "top", nrow = 1)
    )
  
  return(plot)
}



diff_month_four_plot <- function(main_results, # specify only numerical results
                                   second_results, third_results, fourth_results,
                                   main_label="main", second_label="sec",
                                 third_label="third", fourth_label="fourth",
                                   main_color="lightblue", second_color="lightcoral",
                                 third_color="lightgreen", fourth_color="mediumpurple" ,
                                   title = "School Shooting vs Mass Shooting",
                                   subtitle = "Different Timing to Election; HoR", 
                                   footnote = "Based on basic filtering and covariates", 
                                   ylab = "Effect on Logged Dollar Amount"
) {
  
  combined_df <- bind_rows(
    main_results  %>% mutate(Type = main_label),
    second_results %>% mutate(Type = second_label),
    third_results %>% mutate(Type = third_label),
    fourth_results %>% mutate(Type = fourth_label)
  ) %>%
    mutate(Type = factor(Type, levels = c(main_label, second_label, third_label, fourth_label)))
  
  plot <- combined_df %>%
    ggplot(aes(x = threshold_name, # based on numerical input
               y = estimate, color = Type, # use diff colors
               shape = Type)) +
    
    # have to dodge
    geom_errorbar(aes(ymin = conf.low, ymax = conf.high), 
                  width = 0.4, 
                  position = position_dodge(width = 0.4),
                  linewidth = 0.8) +
    
    # geom_point(aes(size = n_treated), 
    #            position = position_dodge(width = 0.4)) +
    
    
    geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
    
    coord_flip() +
    
    scale_color_manual(
      values = setNames(c(main_color, second_color, third_color, fourth_color),
                        c(main_label, second_label, third_label, fourth_label))
    ) +

    
    # scale_size_continuous(name = "# of treated months",
    #                       # range = c(2, 6), # if you would like to transform acutal size
    #                       guide = guide_legend(title.position = "top")) +
    
    labs(
      title = title,
      subtitle = subtitle,
      x = "Months to the Next Election",
      y = ylab,
      caption = footnote,
      color = "Type",
      shape = "Type"
    ) +
    
    theme_minimal() +
    theme(
      plot.title = element_text(size = 12, hjust = 0.5, face = "bold"),
      plot.subtitle = element_text(size = 10, hjust = 0.5),
      axis.text.y = element_text(size = 10, colour = "black"),
      axis.text.x = element_text(size = 10),
      axis.title = element_text(size = 11),
      plot.caption = element_text(size = 10, hjust = 0.5),
      legend.title = element_text(size = 10),
      legend.text = element_text(size = 10),
      legend.position = "bottom",
      legend.box = "horizontal"
    ) +
    
    guides(
      shape = "none",
      color = guide_legend(nrow = 2, ncol = 2, byrow = TRUE,
                           override.aes = list(size = 4))
    )
  
  
  return(plot)
}


month_to_TWFE <- function(df, # unfiltered data; within this function, data_month_to function is used
                          treated_range,
                          PACtype = "anti", # or "pro"
                          ab_dollar=FALSE,  # absolute dollar, instead of log
                          compare="all" # or treatment
                          # all means using only one variable; "treatment" here means to focus on the additional impact
){
  
  if (PACtype=="anti"){
    # compared with other treatment units
    str_withintreatment <- paste0("anti_log_sum ~ incident_window_re_", treated_range, "others+bachelor + black + white + unemployment + log_income + rep_incumbent_before+ gun_ratio_pct+",
                                  "incident_window_re_", treated_range, "|id + time")
    
    str_all <- paste0("anti_log_sum ~ incident_window_re_", treated_range, "+bachelor + black + white + unemployment + log_income + rep_incumbent_before+ gun_ratio_pct|id + time")
    
    str_absdollar <- paste0("anti_sum_donations ~ incident_window_re+bachelor + black + white + unemployment + log_income + rep_incumbent_before+ gun_ratio_pct+ ",
                            "incident_window_re_",
                            treated_range, "|id + time")
  } else if (PACtype=="pro"){
    str_withintreatment <- paste0("pro_log_sum ~ incident_window_re_", treated_range, "others+bachelor + black + white + unemployment + log_income + rep_incumbent_before+ gun_ratio_pct+",
                                  "incident_window_re_", treated_range, "|id + time")
    
    str_all <- paste0("pro_log_sum ~ incident_window_re_", treated_range, "+bachelor + black + white + unemployment + log_income + rep_incumbent_before+ gun_ratio_pct|id + time")
    
    str_absdollar <- paste0("pro_sum_donations ~ incident_window_re+bachelor + black + white + unemployment + log_income + rep_incumbent_before+ gun_ratio_pct+ ",
                            "incident_window_re_",
                            treated_range, "|id + time")
  }
  
  
  if (compare=="all") {
    result <- fixest::feols(as.formula(str_all),data=data_month_to(df, treated_range),cluster="id")
  }
  else if (ab_dollar==FALSE & compare=="treatment"){  
    result <- fixest::feols(as.formula(str_withintreatment),data=data_month_to(df, treated_range),cluster="id")
  } else if (ab_dollar==TRUE){
    result <- fixest::feols(as.formula(str_absdollar),data=data_month_to(df, treated_range),cluster="id")
  }
  return(result)
}


ife_s_static <- function(df, outcome="anti_log_sum", 
                         # r=c(0,2),
                         r=c(0),
                         parallel=TRUE){
  
  str <- paste(outcome, "~ incident_window_re+bachelor + black + white + unemployment  + log_income + rep_incumbent_before+ gun_ratio_pct")
  
  result <- fect(as.formula(str) ,data = df, index = c("id","time"),
                 force = "two-way", method = "ife", CV = TRUE,
                 r = r,
                 se = TRUE, nboots = 200, seed=123, na.rm = TRUE
                 , parallel = parallel)
  return(result)
}



state_trend_DID <- function(df, outcome="anti_log_sum", treatment = "incident_window_re", 
                            quadratic = TRUE, covariate=TRUE,unitFE="state", cluster="state") {
  
  df$time_squared <- (df$time-120)^2 # starts in Jan. 2000, instead of Jan. 1990
  
  if (quadratic == TRUE & covariate==TRUE){
    formula_str <- paste0(outcome, " ~ ", treatment,
                          " + bachelor + black + white + unemployment + log_income + rep_incumbent_before + gun_ratio_pct + i(state, time)+ i(state, time_squared)|", unitFE, "+time") 
  } else if (quadratic == FALSE& covariate==TRUE) {
    formula_str <- paste0(outcome, " ~ ", treatment, 
                          " + bachelor + black + white + unemployment + log_income + rep_incumbent_before + gun_ratio_pct +i(state, time)|", unitFE, "+time")
  } else if (quadratic == TRUE & covariate==FALSE){
    formula_str <- paste0(outcome, " ~ ", treatment, "+ i(state, time)+ i(state, time_squared)|", unitFE, "+time") 
  } else {
    formula_str <- paste0(outcome, " ~ ", treatment, "+ i(state, time)|", unitFE, "+time") 
  }
  
  formula_obj <- as.formula(formula_str)
  fixest::feols(formula_obj, data = df, cluster = reformulate(cluster))
}


simple_pre_event <- function(df, outcome="anti_log_sum_mva_logav5", past=-18){
  str <- paste(outcome, "~i(time_to_event, ref = -1, keep =", past, ":-1)+ bachelor + black + white + unemployment + log_income + rep_incumbent_before + gun_ratio_pct| id + time")
  # time to event is a dummy variable for each period for event study; 0 means the month of an event
  # ref means baseline
  # can remove far away points by using keep
  fixest::feols(as.formula(str),data = df,cluster = "id") %>% 
    fixest::iplot(ref.line = -1, # use vline
                  xlab = "Month Relative to Incidents",
                  ylab = "Logged Dollar Amount",
                  # main = "Simple Event Study (School Shooting; HoR)",
                  main = "",
                  xlim     = c(past, -1)
    )
  
}



placebo_fe <- function(df, treatment, outcome="anti_log_sum", title, 
                       ylab="Logged Dollar Amount",
                       parallel=TRUE, start=-12,placebo_start=-6,
                       end=-1, # the end of xlim
                       ylim=c(-0.5, 0.75) # for logdol outcome. For number, have to be adjusted
){
  
  str <- paste(outcome, "~", treatment, "+ bachelor + black + white + unemployment + log_income + rep_incumbent_before + gun_ratio_pct")
  
  result <- fect(as.formula(str), data = df, 
                 index = c("id", "time"), # id cannot be state_num
                 force = "two-way", 
                 parallel = parallel, 
                 se = TRUE, 
                 CV = 0, # cross validation will not be done.
                 nboots = 200, placeboTest = TRUE, placebo.period = c(placebo_start, end), 
                 na.rm=TRUE, # to avoid error
                 seed = 123  # for reproducibility
  )
  
  plot <- plot(result, cex.text = 0.9, # legend texts might look so small
               stats = c("placebo.p","equiv.p"),
               ylab = ylab, 
               xlab = "Time Relative to Incidents",
               # type = "equiv", # If you want to show lines for TOST
               # bound = "min",  # Boundary based on maximum pre-treatment residual
               main = title, xlim=c(start,end), 
               ylim=ylim,
               # type= "box",  # not good visualization
               count = FALSE # you can remove the bar plot at the bottom of the plot
  )
  return(plot)
}



## 1.1 Propensity Score ----------------------------------------------------


ps_matching_shooting <- function(df, ratio=1000, time = "Year", 
                                 replace = TRUE, caliper=0.2) {
  
  # for matching, made new time-related variable
  df <- df %>%
    mutate(decade = floor(year / 10) * 10, # decade
           pres_term = floor((year -1) / 4) * 4, # presidency
           hor_term = floor((year -1) / 2) * 2) # HoR
  
  # first, matching

  if (time == "Year"){  # too granular???
    formula.str <- "incident_window_re~ bachelor + black + white + unemployment + 
      log_income + rep_incumbent_before+ gun_ratio_pct + year"
  } else if (time == "Decade") {
    formula.str <- "incident_window_re~ bachelor + black + white + unemployment + 
      log_income + rep_incumbent_before+ gun_ratio_pct  + decade"
  } else if (time == "Presidency") {
    formula.str <- "incident_window_re~ bachelor + black + white + unemployment + 
      log_income + rep_incumbent_before+ gun_ratio_pct  + pres_term"
  } else if (time == "House"){
    formula.str <- "incident_window_re~ bachelor + black + white + unemployment + 
      log_income + rep_incumbent_before+ gun_ratio_pct +hor_term"
  } else if (time == "None"){
    formula.str <- "incident_window_re~ bachelor + black + white + unemployment + 
      log_income + rep_incumbent_before+ gun_ratio_pct "
  }  
  
  # matchit is implementing matching
  m.out <- matchit(as.formula(formula.str), 
                   data = df, 
                   method = "nearest",  # If this is exact, you cannot use ratio.
                   ratio = ratio, # 1 means 1 treatment and 1 control
                   replace = replace, # whether to use the same unit several times. TRUE means to allow this.
                   caliper = caliper)
  
  matched_data <- match.data(m.out)
  
  return(list(matched_data = matched_data, match_obj = m.out))
}


check_balance_shooting <- function(match_obj,ratio=1000, time = "Year",
                                   replace = TRUE, caliper=0.2,
                                   balance_title = "Distributional Balance Based on Distance and Its Before/after Comparison",
                                   balance_subtitle=NULL,
                                   love_title    = "Love Plot for Covariate Balance",
                                   love_subtitle=NULL
) {
  print(summary(match_obj)) # to check improved data
  
  balance_plot <- bal.plot(match_obj, 
                           var.name = "distance", 
                           which = "both", # show both before and after matching
                           colors = c("lightcoral", "skyblue"))+
    labs(
      # title    = "Distributional Balance Based on Distance and Its Before/after Comparison",
      # subtitle = paste("Ratio:", ratio, " ", time, "-basis, Caliper: ",caliper)
      
      title=balance_title, subtitle=balance_subtitle
    )+
    theme(
      plot.title    = element_text(face = "bold", hjust = 0.5, size = 14),    
      plot.subtitle = element_text(hjust = 0.5, size = 14)  
    )
  print(balance_plot)
  
  # plot of standarized mean gap
  love_plot <- love.plot(match_obj, 
                         vars = c("bachelor", "black", "white", "unemployment", 
                                  "log_income", "rep_incumbent_before",
                                  "gun_ratio_pct", "pres_term"),
                         var.names = c(
                           bachelor            = "Bachelor’s degree (%)",
                           black               = "Black (%)",
                           white               = "White (%)",
                           unemployment        = "Unemployment (%)",
                           log_income          = "Log Median Income",
                           rep_incumbent_before= "Rep. Incumbency",
                           gun_ratio_pct       = "Firearm Possession Ratio (%)",
                           pres_term           = "President's Term"
                         ),
                         binary = "std", # standarize binary var. as well by using std
                         thresholds = c(0.1, 0.25), # thresholds for good and bad balance
                         # less than 0.1 would be great. over 0.2 is not good.
                         abs = TRUE,  # to show absolute value. True is practice
                         size=5.5 # point size is a bit small. 5 seems too big? 4 might be too small??
  )+
    labs(
      # title    = "Love Plot for Covariate Balance",
      #   subtitle = paste("Ratio:", ratio, " ", time, "-basis, Caliper: ",caliper)
      title=love_title,subtitle=love_subtitle
    )+
    theme(
      text = element_text(size = 15),
      plot.title    = element_text(face = "bold", hjust = 0.5),    
      plot.subtitle = element_text(hjust = 0.5)  
    )
  
  print(love_plot)
  return(list(balance_plot=balance_plot, love_plot=love_plot))
}


ps_outcome_analysis_shooting <- function(matched_data, PACtype="anti") { # or "pro" 
  
  if (PACtype=='anti'){
    result_dollar <- fixest::feols(anti_log_sum~ incident_window_re+ bachelor + black + white + 
    unemployment + log_income + rep_incumbent_before+ gun_ratio_pct | id + time, 
                            data = matched_data, cluster="id",
                            weights = ~weights)  # add weights; unique point, different from ususal TWFE function
    result_number <- fixest::feols(anti_don_number~ incident_window_re+ bachelor + black + white + 
                                     unemployment + log_income + rep_incumbent_before+ gun_ratio_pct | id + time, 
                                   data = matched_data, cluster="id",
                                   weights = ~weights) 
  } else if (PACtype=="pro"){
      result_dollar <- fixest::feols(pro_log_sum~ incident_window_re+ bachelor + black + white + 
                                       unemployment + log_income + rep_incumbent_before+ gun_ratio_pct | id + time, 
                                     data = matched_data, cluster="id",
                                     weights = ~weights)  # add weights; unique point, different from ususal TWFE function
      result_number <- fixest::feols(pro_don_number~ incident_window_re+ bachelor + black + white + 
                                       unemployment + log_income + rep_incumbent_before+ gun_ratio_pct | id + time, 
                                     data = matched_data, cluster="id",
                                     weights = ~weights) 
    
  }
  
  return(list(result_dollar,result_number))
}


shooting_output <- function(df, PACtype= "anti",
                            ratio=1000, # should use a lot of data
                            time = "Year", replace = TRUE,
                            caliper=0.2,
                            balance_title= "Distributional Balance Based on Distance and Its Before/after Comparison",
                            balance_subtitle=NULL,
                            love_title= "Love Plot for Covariate Balance",love_subtitle=NULL
){
  matching <- ps_matching_shooting(df, ratio, time, replace, caliper) # function for matching
  # these arguments also should be included in arguments of this plot function
  
  # ps_matching function has two outputs
  matched_data <- matching$matched_data
  balance <- check_balance_shooting(matching$match_obj,
                                    ratio, time, caliper,
                                    balance_title,balance_subtitle,
                                    love_title,love_subtitle)
  
  if (PACtype=="anti"){
    effects_dollar <- ps_outcome_analysis_shooting(matched_data, "anti")[[1]]
    effects_number <- ps_outcome_analysis_shooting(matched_data, "anti")[[2]]
    
    psmodels_list <- list(
      "Dollar Amount; Gun Control" = effects_dollar,
      "Number Count; Gun Control" = effects_number)
    
  } else if (PACtype=="pro") {
    effects_dollar <- ps_outcome_analysis_shooting(matched_data, "pro")[[1]]
    effects_number <- ps_outcome_analysis_shooting(matched_data, "pro")[[2]]
    
    psmodels_list <- list(
      "Dollar Amount; Pro-Gun" = effects_dollar,
      "Number Count; Pro-Gun" = effects_number)
  }
  
  
  table <- etable(psmodels_list, title = "PS Matching Results (Monthly Data)")
  
  # simple result output is the following, but it cannot be a product of this function
  # tableprint <- print(table)
  
  return(list("balance"=balance, "psmodels_list"=psmodels_list, "table"=table))
}






## 1.2 Misc - including non-functions --------------------------------------

# For simple visualization
coef_map_single <- c(
  "incident_window_re" = "Treatment")


coef_map_GVP_basic <- c(
  "incident_window_re" = "Treatment",
  "pro_logsum_3month_prior"="Prior gun lobby contributions (3m)",
  "pro_logsum_6month_prior"="Prior gun lobby contributions (6m)",
  "anti_logsum_3month_prior" = "Prior GVP contributions (3m)",
  "anti_logsum_6month_prior" = "Prior GVP contributions (6m)")

coef_map_gun_basic <- c(
  "incident_window_re" = "Treatment",
  "anti_logsum_3month_prior" = "Prior GVP contributions (3m)",
  "anti_logsum_6month_prior" = "Prior GVP contributions (6m)",
  "pro_logsum_3month_prior"="Prior gun lobby contributions (3m)",
  "pro_logsum_6month_prior"="Prior gun lobby contributions (6m)")

gof_map = c("nobs", "r.squared", "adj.r.squared")
# Or you just use c("nobs") to show only numbers
# If you want to check what info might be included
# modelsummary::gof_map


ss_sourcenote <- 
  "Baseline covariates include ratios of bachelor holders, of black, and of white; unemployment rate, logged median income,
<br>incumbency in the previous HoR election, estimated household firearm possession rate, unless removed due to multicollinearity."

# for state-wide datasets; no incumbency
state_sourcenote <- 
  "Baseline covariates include ratios of bachelor holders, of black, and of white; unemployment rate, logged income, estimated household firearm possession rate, unless removed due to multicollinearity"




# 2. Other Analysis -------------------------------------------------------


# 2.1 DCDH ----------------------------------------------------------------



DCDH_event <- function(df, outcome="anti_log_sum", fe="id", time="time", 
                       n_effect = 12, 
                       n_placebo = 6, 
                       # controls should be atomic char or vector of char
                       controls= c("bachelor", "black", "white", "unemployment", "log_income", "rep_incumbent_before",
                                   "gun_ratio_pct"),
                       cluster="id",
                       treatment="incident_window_re" # to use for grouped dataset
) {
  
  # double-check if this is a df, compatible with this package more
  df <- as.data.frame(df)
  
  result <- did_multiplegt_dyn(
    df = df, outcome = outcome, group = fe, # district or state-level
    # state is a character, so when trying state-fe, you have to use "state_num"
    
    time = time,
    treatment = treatment, 
    effects = n_effect,  # no. of post-treatment periods
    placebo = n_placebo, # no. of pre-treatment periods
    cluster = cluster, 
    controls = controls  
    
    # This function is not compatible with the following parallel processing
    # parallel = TRUE
  )
  
  return(result)
}


plot_DCDH <- function(result, n_effect_CI=12, n_placebo_CI=6, 
                      n_effect_point=12, n_placebo_point=6,
                      title=NULL, subtitle=NULL, 
                      ylab="Logged Dollar Amount", footnote=NULL){
  
  plot <- result$plot
  plot$layers[[1]] <- NULL # first, remove the plot's line layer (1st layer)
  
  # Changing windows based on lead and lag
  
  # CI
  plot$layers[[1]]$aes_params$colour <- c(rep("skyblue", n_effect_CI), rep("lightcoral", n_placebo_CI)) 
  plot$layers[[1]]$aes_params$linewidth <- 1  # 0.7 might be too thin
  
  # point
  plot$layers[[2]]$aes_params$colour <- c(rep("skyblue", n_effect_point+1), rep("salmon", n_placebo_point))
  plot$layers[[2]]$aes_params$size <- 1.4
  
  plot <- plot + geom_hline(yintercept = 0, color = "darkgray", linewidth=0.9) +
    geom_line(aes(group = 1), linewidth = 0.8, color="black")+  #0.9 is too thick; big gap between 0.7 and 0.8
    geom_vline(xintercept = 0, linetype = "dotted", color = "darkgray", linewidth = 1.1) +
    # Add the reference line
    
    # scale_x_continuous(breaks=seq(-6,12,1)) + # Adjust the x axis ticks if needed
    
    xlab("Relative Time to Treatment Changes")+
    labs(title=title, subtitle=subtitle)+
    
    ylab(ylab) +
    labs(caption = footnote)
  theme(plot.title = element_text(size = 10, hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(size = 9.5, hjust = 0.5),
        plot.caption = element_text(size = 9, hjust = 0),
        
        panel.grid.major = element_blank(), # Clean the background
        panel.grid.minor = element_blank(), 
        panel.background = element_blank(),
        
        axis.line = element_line(colour = "black"),
        axis.title.x = element_text(size = 9),
        axis.title.y = element_text(size = 9)
  )
  
  return(plot)  
}



tidy.did_multiplegt_dyn <- function(x, ...) {
  # for did_multiplegt_dyn object, specify function for broom::tidy
  df <- x$results$ATE
  df <- cbind(df, x$results$p_jointeffects)
  df <- cbind(df, x$results$p_jointplacebo)
  tibble::tibble(
    term      = "incident_window_re",   estimate  = df[1, "Estimate"],
    std.error = df[1, "SE"], p.value   = df[1, 9], placebo=df[1,10]
  )
}

# glance, not tidy
# to output summary of model
glance.did_multiplegt_dyn <- function(x, ...) {
  tibble::tibble(
    nobs        = nrow(x$plot$data),
    # nobs = NA_integer_,  # when num is not used
    p_placebo   = x$results$p_jointplacebo
  )
}



# 2.1.1 Multiple Plot -----------------------------------------------------

DCDH_multiple_plot <- function(dollar3, dollar4, dollar6,
                               num3,num4,num6, ylim_dol=c(-1.95, 2.2),
                               ylim_num=c(-0.3, 0.23),
                               type="both" # or dollar or num
                               ){
  
DCDH_multiple <- list(
  "3_months"    = dollar3,
  "4_months"    = dollar4,
  "6_months"    = dollar6
)

DCDH_all <- imap_dfr(DCDH_multiple, ~ {
  .x$plot$data %>%
    mutate(
      model  = .y,
      window = ifelse(Time < 0, "pre-treatment", "post-treatment"), 
      # Time (capital letter) is defined at DCDH result objects
      mw     = paste(.y, window, sep = "_")  # model window
    )
})

pre_palette <- c(
  "3_months"   = "#DE2D26",
  "4_months" = "#FB6A4A",
  "6_months"   = "#EF3B2C"
 )
post_palette <- c(
  "3_months"  = "#66C2A5",
  "4_months"   = "#4EB3D3",
  "6_months"      = "#08519C"
)

# integrate color and names
col_vals <- c(
  setNames(pre_palette,  paste(names(pre_palette), "pre-treatment", sep = "_")),
  setNames(post_palette, paste(names(post_palette), "post-treatment", sep = "_"))
)

DCDH_mplot_dollar <- ggplot(DCDH_all, aes(x     = Time, y     = Estimate,
                                   color = mw,   # model window
                                   
                                   # two patterns of shape
                                   shape = window  # shape is diff between pre and post
                                   # shape = model  # shape is diff among models
)) +
  # CI
  geom_errorbar(aes(ymin = LB.CI, ymax = UB.CI),
                width    = 0.1,
                size     = 0.8,
                position = position_dodge(width = 0.6)) +
  # Point
  geom_point(position = position_dodge(width = 0.5), size = 2.5) +
  # 0 dashed line
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_vline(xintercept = 0, linetype = "dotted") +
  
  # color
  # have to specify sequence
  scale_color_manual(
    name   = NULL,
    values = col_vals,
    # sequence
    breaks = c(  # sequence is not horizontal, but vertical!!!
      "3_months_pre-treatment",
      "3_months_post-treatment",
      "4_months_pre-treatment",
      "4_months_post-treatment",
      "6_months_pre-treatment",
      "6_months_post-treatment"
    ),
    labels = c(
      "3 months; Pre",
      "3 months; Post",
      "4 months; Pre",
      "4 months; Post", 
      "6 months; Pre",
      "6 months; Post"      
    ), 
    guide = guide_legend(
      ncol = 3, 
      byrow = FALSE,
      override.aes = list(
        shape    = NA,       # remove points
        linetype = "solid",  
        size     = 1         
      )
    )
  ) +
  
  # shape legend, but no need
  # scale_shape_manual(
  #   name   = NULL,
  #   values = c("3_months" = 16, "4_months" = 17, "6_months" = 18),
  #   breaks = c("3_months", "4_months", "6_months"),
  #   labels = c("3 Months", "4 Months", "6 Months")
  # )
  
  scale_shape_manual(
    name   = NULL,
    # depending on pre/post
    values = c("pre-treatment" = 16, "post-treatment" = 17),
    labels = c("Pre‐treatment", "Post‐treatment")
    
    # depending on models
    # values = c("3_months" = 16, "4_months" = 17, "6_months" = 18),
    # labels = c("3 Months", "4 Months", "6 Months")
  ) +
  
  # if you want to remove legend for shapes or colors
  guides(shape = "none") +
  # guides(color = "none") +
  
  labs(
    # title = "DCDH Event Study",
    # subtitle = "Different Moving Avg. Ranges",
    title="", subtitle="",
    x     = "Relative Time to Treatment Changes",
    y     = "Logged Dollar Amount"
  ) +
  theme_minimal(base_size = 13, base_family = "TNR") +
  theme(
    legend.position   = "bottom",
    legend.key.width   = unit(1.5, "lines"),    # length of each key display
    legend.spacing.x   = unit(0.07, "cm"),       # space between each item
    legend.box.spacing = unit(0.07, "cm"),       # space between legend and plot body
    legend.box.margin = margin(t = 0, r = 0, b = 0, l = 0),
    legend.margin     = margin(t = 0, r = 0, b = 0, l = 0),
    legend.text       = element_text(size = 11.5
                                     # , face = "bold"
                                     ,family = "TNR"
    ),
    plot.title        = element_text(face = "bold", size = 14, hjust = 0.5),
    plot.subtitle        = element_text(size = 9, hjust = 0.5),
    axis.title.x = element_text(family = "TNR"),
    axis.title.y = element_text(family = "TNR"),
    axis.text = element_text(family = "TNR")
  )


# number

DCDH_multiple_number <- list(
  "3_months"    = num3,
  "4_months"    = num4,
  "6_months"    = num6
)

DCDH_all_number <- imap_dfr(DCDH_multiple_number, ~ {
  .x$plot$data %>%
    mutate(
      model  = .y,
      window = ifelse(Time < 0, "pre-treatment", "post-treatment"), 
      # Time (capital letter) is defined at DCDH result objects
      mw     = paste(.y, window, sep = "_")  # model window
    )
})

# color vectors are the same

DCDH_mplot_number <- ggplot(DCDH_all_number, aes(x     = Time, y     = Estimate,
                                                 color = mw,   # model window
                                                 
                                                 shape = window 
)) +
  # CI
  geom_errorbar(aes(ymin = LB.CI, ymax = UB.CI),
                width    = 0.1,
                size     = 0.8,
                position = position_dodge(width = 0.6)) +
  # Point
  geom_point(position = position_dodge(width = 0.5), size = 2.5) +
  # 0 dashed line
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_vline(xintercept = 0, linetype = "dotted") +
  
  # color
  # have to specify sequence
  scale_color_manual(
    name   = NULL,
    values = col_vals,
    # sequence
    breaks = c(  # sequence is not horizontal, but vertical!!!
      "3_months_pre-treatment",
      "3_months_post-treatment",
      "4_months_pre-treatment",
      "4_months_post-treatment",
      "6_months_pre-treatment",
      "6_months_post-treatment"
    ),
    labels = c(
      "3 months; Pre",
      "3 months; Post",
      "4 months; Pre",
      "4 months; Post", 
      "6 months; Pre",
      "6 months; Post"      
    ), 
    guide = guide_legend(
      ncol = 3, 
      byrow = FALSE,
      override.aes = list(
        shape    = NA,       # remove points
        linetype = "solid",  
        size     = 1         
      )
    )
  ) +
  
  scale_shape_manual(
    name   = NULL,
    # depending on pre/post
    values = c("pre-treatment" = 16, "post-treatment" = 17),
    labels = c("Pre‐treatment", "Post‐treatment")
    
    # depending on models
    # values = c("3_months" = 16, "4_months" = 17, "6_months" = 18),
    # labels = c("3 Months", "4 Months", "6 Months")
  ) +
  
  # if you want to remove legend for shapes or colors
  guides(shape = "none") +
  # guides(color = "none") +
  
  labs(
    # title = "DCDH Event Study",
    # subtitle = "Different Moving Avg. Ranges",
    title="", subtitle="",
    x     = "Relative Time to Treatment Changes",
    y     = "Number Count"
  ) +
  theme_minimal(base_size = 11, base_family = "TNR") +
  theme(
    legend.position   = "bottom",
    legend.key.width   = unit(1.5, "lines"),    # length of each key display
    legend.spacing.x   = unit(0.07, "cm"),       # space between each item
    legend.box.spacing = unit(0.07, "cm"),       # space between legend and plot body
    legend.box.margin = margin(t = 0, r = 0, b = 0, l = 0),
    legend.margin     = margin(t = 0, r = 0, b = 0, l = 0),
    legend.text       = element_text(size = 11.5
                                     # , face = "bold"
                                     ,family = "TNR"
    ),
    plot.title        = element_text(face = "bold", size = 14, hjust = 0.5),
    plot.subtitle        = element_text(size = 9, hjust = 0.5)
  )


## Combined (dollar and number) plot

DCDH_all_revised <- DCDH_all %>%
  # if you want to rename model → method. Three methods plot use method.
  # rename(method = model) %>%                
  mutate(
    Outcome = factor(
      "Logged Dollar Amount",
      levels = "Logged Dollar Amount",
      labels = "Effect on Logged Dollar Amount"
    )
  )

DCDH_all_revised_number <- DCDH_all_number %>%
  # rename(method = model) %>%                # model → method
  mutate(
    Outcome = factor(
      "Number Count",
      levels = "Number Count",
      labels = "Effect on Number Count"
    )
  )


combined_DCDH_mplot <- bind_rows(DCDH_all_revised, DCDH_all_revised_number)


mplot_box_DCDH <- ggplot(combined_DCDH_mplot,
                         aes(x = Time, y = Estimate,
                             color = paste(model, window, sep = "_"),
                             shape = window)) +
  
  geom_errorbar(aes(ymin = LB.CI, ymax = UB.CI),
                width    = 0.1, size = 0.8,
                position = position_dodge(0.6)) +
  
  geom_point(position = position_dodge(0.5), size = 2.5) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_vline(xintercept = 0, linetype = "dotted") +
  
  # use ggh4x, for better facet locations
  facet_wrap2(~ Outcome,
              ncol    = 1,
              scales  = "free_y",  # for different y scales at different plots
              strip.position = "top") +  # strip is for grey shaded titles
  
  # use different y scales for diff. plots
  facetted_pos_scales(
    y = list(
      Outcome == "Effect on Logged Dollar Amount"    ~ scale_y_continuous(limits = ylim_dol),
      Outcome == "Effect on Number Count"   ~ scale_y_continuous(limits = ylim_num)
    )
  ) +
  
  # x lim is the same
  coord_cartesian(xlim = c(-6, 6))+
  
  scale_color_manual(
    name   = NULL,
    values = col_vals,
    # sequence
    breaks = c(  # sequence is not horizontal, but vertical!!!
      "3_months_pre-treatment",
      "3_months_post-treatment",
      "4_months_pre-treatment",
      "4_months_post-treatment",
      "6_months_pre-treatment",
      "6_months_post-treatment"
    ),
    labels = c(
      "3 months; Pre",
      "3 months; Post",
      "4 months; Pre",
      "4 months; Post", 
      "6 months; Pre",
      "6 months; Post"      
    ), 
    guide = guide_legend(
      ncol = 3, 
      byrow = FALSE,
      override.aes = list(
        shape    = NA,       # remove points
        linetype = "solid",  
        size     = 1         
      )
    )
  ) +
  scale_shape_manual(
    name   = NULL,
    # depending on pre/post
    values = c("pre-treatment" = 16, "post-treatment" = 17),
    labels = c("Pre‐treatment", "Post‐treatment")
  ) +
  
  # no need of shape legend
  guides(shape = "none") +
  
  labs(
    x = "Relative Time to Treatment Changes",
    y = NULL
  ) +
  
  theme_minimal(base_size = 11, base_family = "TNR") +
  theme(
    
    strip.background = element_rect(fill = "grey70", color = "black"),
    strip.text       = element_text(size = 11.5, family = "TNR"),
    strip.placement  = "outside",
    
    
    panel.background = element_rect(fill = "white", color = NA),
    panel.border     = element_rect(color = "black", fill = NA, size = 0.5),
    
    # space to avoid duplication
    panel.spacing    = unit(0.4, "cm"),
    
    # margin
    plot.margin      = unit(c(0.3, 0.3, 0.3, 0.3), "cm"),
    legend.position  = "bottom",
    legend.key.width = unit(1.5, "lines"),
    legend.spacing.x = unit(0.07, "cm"),
    legend.box.margin= margin(0,0,0,0),
    legend.text      = element_text(size = 11.5),
    
    axis.line        = element_line(color = "black"),
    axis.ticks       = element_line(color = "black"),
    axis.text        = element_text(color = "black"),
    axis.text.x      = element_text(),
    axis.ticks.x     = element_line(),
    axis.text.y      = element_text(),
    axis.ticks.y     = element_line()
  )


if (type=="both"){
  mplot_box_DCDH
  
} else if (type=="dollar"){
  DCDH_mplot_dollar
  
} else if (type=="num"){
  DCDH_mplot_number
}
}


# 2.2 Imputation ----------------------------------------------------------

model_fect <- function(df, outcome="anti_log_sum", treatment="incident_window_re",
                       X=c("bachelor","black","white","unemployment", "log_income",
                            "rep_incumbent_before", "gun_ratio_pct"), 
                       parallel=TRUE){
  result <- fect(Y = outcome, D = treatment, 
                 X= X, data = df,   method = "fe", 
                 index = c("id","time"),
                 se = TRUE,   na.rm = TRUE, vartype="parametric", # instead of normal bootstrap
                 parallel = parallel,  # parallel processing
                 seed = 123, force = "two-way")
  return(result)
}

imputation_plot <- function(df, main="Imputation Method (Basic Filtering)",
                            xlim= c(-6, 12), 
                            ylab="Logged Dollar Amount"){
  plot <- plot(df,   main = main,
               xlim = xlim,
               xlab="Relative Time to Treatment Changes",
               # clim = c(-1, 1),
               ylab = ylab,
               plot.ci = "0.95",  theme.bw = TRUE, cex.main=0.7,
               # type = "box", 
               cex.lab = 0.7,   count = FALSE)  # to remove the bar plot at the bottom
  return(plot)
}


combined_imputation_plot <- function(list, linewidth=1.3, end=12,
                                     ylim_low=-1, ylim_high=1.5, size=15){
  tibble <- imap_dfr(list, ~ {
    fect_obj <- .x
    tibble(
      Time     = fect_obj$time,
      Estimate = fect_obj$att,
      LB.CI    = fect_obj$att.bound[,1],
      UB.CI    = fect_obj$att.bound[,2]
    ) %>%
      mutate(
        model  = .y,
        window = ifelse(Time < 0, "pre-treatment", "post-treatment"),
        mw     = paste(.y, window, sep = "_")
      )
  })
  
  pre_palette_impmva <- c(
    "3_months" = "#FC9272",
    "4_months" = "#FB6A4A",
    "6_months" = "#EF3B2C"
  )
  post_palette_impmva <- c(
    "3_months" = "#66C2A5",
    "4_months" = "#4EB3D3",
    "6_months" = "#08519C"
  )
  col_vals <- c(
    setNames(pre_palette_impmva,  paste(names(pre_palette_impmva),  "pre-treatment", sep = "_")),
    setNames(post_palette_impmva, paste(names(post_palette_impmva), "post-treatment", sep = "_"))
  )
  
  
  plot <- ggplot(tibble, aes(x = Time, y = Estimate, color = mw, shape = window)) +
    geom_errorbar(aes(ymin = LB.CI, ymax = UB.CI),
                  width = 0.1, size = linewidth,
                  position = position_dodge(width = 0.6)) +
    geom_point(position = position_dodge(width = 0.5), size = 2.5) +
    geom_hline(yintercept = 0, linetype = "dashed") +
    geom_vline(xintercept = 0, linetype = "dotted") +
    
    coord_cartesian(xlim = c(-6, end), # scope of time
                    ylim = c(ylim_low, ylim_high)) + # have to adjust it
    
    
    scale_color_manual(
      name   = NULL,
      values = col_vals,
      breaks = c(
        "3_months_pre-treatment", "3_months_post-treatment",
        "4_months_pre-treatment", "4_months_post-treatment",
        "6_months_pre-treatment", "6_months_post-treatment"
      ),
      labels = c(
        "3 months; Pre", "3 months; Post",
        "4 months; Pre", "4 months; Post",
        "6 months; Pre", "6 months; Post"
      )
    ) +
    scale_shape_manual(
      name   = NULL,
      values = c("pre-treatment" = 16, "post-treatment" = 17),
      labels = c("Pre‐treatment", "Post‐treatment")
    ) +
    
    # no need of shape legend
    guides(shape = "none") +
    
    
    labs(
      x = "Relative Time to Treatment Changes",
      # y = "Estimate (ATT)"
      y = "Logged Dollar Amount"
    ) +
    theme_minimal(base_size = size, base_family = "TNR") +
    theme(
      legend.position   = "bottom",
      # legend.key.width  = unit(1.5, "lines"),
      # legend.spacing.x  = unit(0.07, "cm"),
      legend.box.margin = margin(t = 0, r = 0, b = 0, l = 0),
      legend.text       = element_text(family = "TNR"),
      # plot.title        = element_text(face = "bold", size = 14, hjust = 0.5),
      # plot.subtitle     = element_text(size = 9, hjust = 0.5)
      # axis.title        = element_text(family = "TNR"),
      # axis.text         = element_text(family = "TNR")
    )
  return(plot)
}



## 2.3 Panel Match ---------------------------------------------------------

panel_match_plot <- function(df, outcome = "anti_log_sum", match_on_outcome=TRUE,
                             period_end=6, matching_outcome="anti_log_sum", ylim=c(-0.5, 2.5),
                             ylim_pre=c(-1.5,0.5), numeric_result=FALSE
                             ){
  panelm <- PanelData(panel.data = df,unit.id = "id",
                                      # this should be integer or numeric
                                      time.id = "time",
                                      treatment = "incident_window_re",
                                      outcome = outcome)
  

# several matching patterns
# first, similar to propensity matching

if (match_on_outcome==FALSE){
  PM.results <- PanelMatch(panel.data = panelm, 
                           lag = period_end,
                           # Matched treated and control observations share an identical treatment history from t-lag to t-1, where t is the time of treatment.
                           refinement.method = "ps.match",
                           match.missing = TRUE,
                           # If set to FALSE, missing data is not permitted in the lag window of the treatment variable in either treated or control units.
                           covs.formula = ~ bachelor+black+white+unemployment+log_income
                           +rep_incumbent_before+gun_ratio_pct,
                           size.match = 10, qoi = "att",
                           lead = 0:6,
                           forbid.treatment.reversal = FALSE,
                           placebo.test = TRUE  # have to specify here, because the existence of future placebo test will affect matching patterns here.
  )
  
} else if (match_on_outcome==TRUE){
  # next, outcome is included into matching criteria
  # moving avg or simple outcome should be included??
  
  str <- paste0(
    "~bachelor+black+white+unemployment+log_income+rep_incumbent_before+gun_ratio_pct+",
    " I(lag(", matching_outcome, ", 1:", period_end, "))"
  )
  
  PM.results <- PanelMatch(panel.data = panelm,  lag = period_end,
                                                      refinement.method = "ps.match",
                                                      
                                                      # you can also choose "CBPS.match" (covariate balanced propensity score matching)
                                                      # or, "ps.weight" (propensity score weighting).
                                                      
                                                      match.missing = TRUE,
                                                      covs.formula = as.formula(str),
                                                      
                                                      size.match = 10, qoi = "att", # qoi is quantity of interest
                                                      lead = 0:6,
                                                      forbid.treatment.reversal = FALSE,
                                                      placebo.test = TRUE)
  
}


PE.results <- PanelEstimate(sets = PM.results, panel.data = panelm,se.method = "bootstrap")

# print
if (numeric_result==FALSE){
  post_plot <- plot(PE.results, ylim=ylim, 
                    # main = 'Panel Match DID'
                    main="")
  
  pre_plot <- placebo_test(pm.obj = PM.results,panel.data = panelm,
                           number.iterations = 100, 
                           plot = TRUE, # for plot
                           se.method = "bootstrap"
                           , ylim=ylim_pre,
                           # main = 'Panel Match DID (pre-trend placebo test)'
                           main=""
  )
  return(list(post_plot, pre_plot))
} else if (numeric_result==TRUE){
  pre_numeric <- placebo_test(pm.obj = PM.results, panel.data = panelm,
                                               number.iterations = 100, 
                                               plot = FALSE,  # for estimates
                                               se.method = "bootstrap"
                                               , ylim=c( -2.5,1),
                                               main="")
return(list(PE.results,pre_numeric))
  
}


}


# 3 Multiple Method Visualization -----------------------------------------

multiple_hetero_dollar_plot <- function(DCDH_list, imputation_list,panel_post,
                                panel_pre, ylim = c(-2, 2)){

# Imputation (FECT)
df_mplot_imp <- tibble(
  Time     = imputation_list$time,
  Estimate = imputation_list$att,
  LB.CI    = imputation_list$att.bound[,1],
  UB.CI    = imputation_list$att.bound[,2]
) %>%
  mutate(
    method = "Imputation",
    window = ifelse(Time < 0, "Pre", "Post")
  )

# Simple TWFE Event FECT
# df_mplot_simple <- tibble(
#   Time     = simple_event_fect$time,
#   Estimate = simple_event_fect$att,
#   LB.CI    = simple_event_fect$att.bound[,1],
#   UB.CI    = simple_event_fect$att.bound[,2]
# ) %>%
#   mutate(
#     method = "TWFE",
#     window = ifelse(Time < 0, "Pre", "Post")
#   )
# 
# # too similar to imputation method??
# df_mplot_imp
# df_mplot_simple


# Panel Match Pre
# pm_mplot_pre <- pre_s_panelmatch_outcomeplus

df_mplot_pmpre <- tibble(
  Time     = as.numeric(gsub("t", "", names(panel_pre$estimates))),
  Estimate = unname(panel_pre$estimates),
  SE       = unname(panel_pre$standard.errors)
) %>%
  mutate(
    LB.CI   = Estimate - qnorm(0.975)*SE,
    UB.CI   = Estimate + qnorm(0.975)*SE,
    method  = "PanelMatch",
    window  = "Pre"
  ) %>%
  dplyr::select(-SE)

# Then, we have to add a ref. point at t=-1.

df_mplot_pmpre_added <- df_mplot_pmpre %>%
  add_row(
    Time     = -1,
    Estimate = 0,
    LB.CI    = 0,
    UB.CI    = 0,
    method   = "PanelMatch",
    window   = "Pre"
  ) %>%
  arrange(Time)

# df_mplot_pmpre_added 

# PanelMatch Post
# pm_mplot_post <- PE.results_s_baseline_outcomeplus

df_mplot_pmpost <- tibble(
  Time     = as.numeric(gsub("t", "", names(panel_post$estimate))),
  Estimate = unname(panel_post$estimate),
  SE       = unname(panel_post$standard.error)
) %>%
  mutate(
    LB.CI   = Estimate - qnorm(0.975)*SE,
    UB.CI   = Estimate + qnorm(0.975)*SE,
    method  = "PanelMatch",
    window  = "Post"
  ) %>%
  dplyr::select(-SE)

# DCDH
df_mplot_DCDH <- tibble(
  Time     = DCDH_list$plot$data$Time,
  Estimate = DCDH_list$plot$data$Estimate,
  LB.CI    = DCDH_list$plot$data$LB.CI,
  UB.CI    = DCDH_list$plot$data$UB.CI
) %>%
  mutate(
    method = "DCDH",
    window = ifelse(Time < 0, "Pre", "Post")
  )


all_df_mplot <- bind_rows(df_mplot_DCDH, df_mplot_imp, df_mplot_pmpre_added, df_mplot_pmpost)
all_nopmpre_df_mplot <- bind_rows(df_mplot_DCDH, df_mplot_imp, 
                                      # df_mplot_pmpre_added, 
                                      df_mplot_pmpost)
three_df_mplot <- bind_rows(df_mplot_imp, 
                                df_mplot_pmpre_added,
                                df_mplot_pmpost)



  ## color
  
  palette_vals <- c(
    "DCDH_Post" = "#3182BD",
    "DCDH_Pre" = "#DE2D26",
    "Imputation_Post"   = "#66C2A5",
    "Imputation_Pre"    = "#FC9272",
    "PanelMatch_Post"   = "#08519C" 
  )
  
  palette_vals_all <- c(
    "DCDH_Pre" = "#DE2D26",
    "DCDH_Post" = "#3182BD",
    "Imputation_Pre"    = "#FC9272",
    "Imputation_Post"   = "#66C2A5",
    "PanelMatch_Pre"    = "#EF3B2C",
    "PanelMatch_Post"   = "#08519C"
  )
  
  
  
  palette_vals_three <- c(
    "Imputation_Post"   = "#66C2A5",
    "Imputation_Pre"    = "#DE2D26",
    "PanelMatch_Post"   = "#08519C",
    "PanelMatch_Pre"    = "#FB6A4A"
  )
  
  # potential additional colors
  # "#A50F15"
  # "#4292C6"
  # "#CB181D"
  # "#6BAED6"
  # "#FCBBA1"
  # "#9ECAE1"
  
  
  # 5 patterns excluding panel-pre
  
  all_nopmpre_mplot <- ggplot(all_nopmpre_df_mplot, aes(x = Time, y = Estimate,
                                                        color = paste(method, window, sep = "_"),
                                                        shape = window)) +
    geom_errorbar(aes(ymin = LB.CI, ymax = UB.CI),
                  width    = 0.1,
                  size     = 0.8,
                  position = position_dodge(width = 0.6)) +
    geom_point(position = position_dodge(width = 0.5), size = 2.5) +
    geom_hline(yintercept = 0, linetype = "dashed") +
    geom_vline(xintercept = 0, linetype = "dotted") +
    
    coord_cartesian(xlim = c(-6, 6), ylim = ylim) +
    
    scale_color_manual(
      name   = NULL,
      values = palette_vals,
      breaks = names(palette_vals),
      labels = c(
        "DCDH; Post",  "DCDH; Pre",
        "Imputation; Post",  "Imputation; Pre",
        # "TWFE; Post","TWFE; Pre",
        # "PanelMatch; Pre",         
        "PanelMatch; Post"
      ),
      guide = guide_legend(ncol = 3, byrow = FALSE, # 3 columns
                           override.aes = list(
                             shape    = NA,       # remove points
                             linetype = "solid",  # just showing solid line
                             size     = 1         # 
                           ))
    ) +
    
    scale_shape_manual(
      name   = NULL,
      values = c("Pre" = 16, "Post" = 17),
      labels = c("Pre‐treatment", "Post‐treatment")
    ) +
    
    # choose legend
    guides(shape = "none") +
    
    labs(
      title = "Three Methods (Excluding Pre-treatment PanelMatch)",
      x = "Relative Time to Treatment Changes",
      y = "Logged Dollar Amount"
    ) +
    theme_minimal(base_size = 11
                  # , base_family = "TNR"
    ) +
    theme(
      legend.position   = "bottom",
      legend.key.width  = unit(1.5, "lines"),
      legend.spacing.x  = unit(0.07, "cm"),
      legend.box.margin = margin(0,0,0,0),
      legend.text       = element_text(size = 11),
      plot.title = element_text(size=11, hjust = 0.5)
      # axis.title        = element_text(family = "TNR"),
      # axis.text         = element_text(family = "TNR")
    )
  
  
  
  # all 6 patterns
  
  
  all_mplot <- ggplot(all_df_mplot, aes(x = Time, y = Estimate,
                                        color = paste(method, window, sep = "_"),
                                        shape = window)) +
    geom_errorbar(aes(ymin = LB.CI, ymax = UB.CI),
                  width    = 0.1,
                  size     = 0.8,
                  position = position_dodge(width = 0.6)) +
    geom_point(position = position_dodge(width = 0.5), size = 2.5) +
    geom_hline(yintercept = 0, linetype = "dashed") +
    geom_vline(xintercept = 0, linetype = "dotted") +
    
    coord_cartesian(xlim = c(-6, 6), ylim = ylim) +
    
    scale_color_manual(
      name   = NULL,
      values = palette_vals_all,
      breaks = names(palette_vals_all),
      labels = c(
        "DCDH; Pre",  "DCDH; Post",
        "Imputation; Pre",  "Imputation; Post",
        # "TWFE; Post","TWFE; Pre",
        "PanelMatch; Pre",
        "PanelMatch; Post"
      ),
      guide = guide_legend(ncol = 3, byrow = FALSE,
                           override.aes = list(
                             shape    = NA,       # remove points
                             linetype = "solid",  # just showing solid line
                             size     = 1         # 
                           ))
    ) +
    scale_shape_manual(
      name   = NULL,
      values = c("Pre" = 16, "Post" = 17),
      labels = c("Pre‐treatment", "Post‐treatment")
    ) +
    
    # choose legend
    guides(shape = "none") +
    
    labs(
      # title = "Three Methods",
      title="",
      x = "Relative Time to Treatment Changes",
      y = "Logged Dollar Amount"
    ) +
    theme_minimal(base_size = 11
                  , base_family = "TNR"
    ) +
    theme(
      legend.position   = "bottom",
      legend.key.width  = unit(1.5, "lines"),
      legend.spacing.x  = unit(0.07, "cm"),
      legend.box.margin = margin(0,0,0,0),
      legend.text       = element_text(size = 11.5),
      plot.title = element_text(size=11, hjust = 0.5)
      # axis.title        = element_text(family = "TNR"),
      # axis.text         = element_text(family = "TNR")
    )
  
  # scale_x_continuous(limits = c(-6, 6)) +
  # scale_y_continuous(limits = c(-2.05, 2))
  
  return(list(all_nopmpre_mplot, all_mplot))
} 


multiple_hetero_number_plot <- function(DCDH_list, imputation_list,panel_post,
                                       panel_pre, ylim = c(-2, 2)){
  


# Imputation (FECT)
df_mplot_imp_number <- tibble(
  Time     = imputation_list$time,
  Estimate = imputation_list$att,
  LB.CI    = imputation_list$att.bound[,1],
  UB.CI    = imputation_list$att.bound[,2]
) %>%
  mutate(
    method = "Imputation",
    window = ifelse(Time < 0, "Pre", "Post")
  )

# Panel Match Pre

df_mplot_pmpre_number <- tibble(
  Time     = as.numeric(gsub("t", "", names(panel_pre$estimates))),
  Estimate = unname(panel_pre$estimates),
  SE       = unname(panel_pre$standard.errors)
) %>%
  mutate(
    LB.CI   = Estimate - qnorm(0.975)*SE,
    UB.CI   = Estimate + qnorm(0.975)*SE,
    method  = "PanelMatch",
    window  = "Pre"
  ) %>%
  dplyr::select(-SE)

# Then, we have to add a ref. point at t=-1.

df_mplot_pmpre_added_number <- df_mplot_pmpre_number %>%
  add_row(
    Time     = -1,
    Estimate = 0,
    LB.CI    = 0,
    UB.CI    = 0,
    method   = "PanelMatch",
    window   = "Pre"
  ) %>%
  arrange(Time)

# PanelMatch Post

df_mplot_pmpost_number <- tibble(
  Time     = as.numeric(gsub("t", "", names(panel_post$estimate))),
  Estimate = unname(panel_post$estimate),
  SE       = unname(panel_post$standard.error)
) %>%
  mutate(
    LB.CI   = Estimate - qnorm(0.975)*SE,
    UB.CI   = Estimate + qnorm(0.975)*SE,
    method  = "PanelMatch",
    window  = "Post"
  ) %>%
  dplyr::select(-SE)

# DCDH
df_mplot_DCDH_number <- tibble(
  Time     = DCDH_list$plot$data$Time,
  Estimate = DCDH_list$plot$data$Estimate,
  LB.CI    = DCDH_list$plot$data$LB.CI,
  UB.CI    = DCDH_list$plot$data$UB.CI
) %>%
  mutate(
    method = "DCDH",
    window = ifelse(Time < 0, "Pre", "Post")
  )


all_df_mplot_number <- bind_rows(df_mplot_DCDH_number, df_mplot_imp_number, df_mplot_pmpre_added_number, df_mplot_pmpost_number)


# color vector has the same name as the dollar outcome

## color

palette_vals <- c(
  "DCDH_Post" = "#3182BD",
  "DCDH_Pre" = "#DE2D26",
  "Imputation_Post"   = "#66C2A5",
  "Imputation_Pre"    = "#FC9272",
  "PanelMatch_Post"   = "#08519C"
)

palette_vals_all <- c(
  "DCDH_Pre" = "#DE2D26",
  "DCDH_Post" = "#3182BD",
  "Imputation_Pre"    = "#FC9272",
  "Imputation_Post"   = "#66C2A5",
  "PanelMatch_Pre"    = "#EF3B2C",
  "PanelMatch_Post"   = "#08519C"
)



palette_vals_three <- c(
  "Imputation_Post"   = "#66C2A5",
  "Imputation_Pre"    = "#DE2D26",
  "PanelMatch_Post"   = "#08519C",
  "PanelMatch_Pre"    = "#FB6A4A"
)

# potential additional colors
# "#A50F15"
# "#4292C6"
# "#CB181D"
# "#6BAED6"
# "#FCBBA1"
# "#9ECAE1"


all_mplot_number <- ggplot(all_df_mplot_number, aes(x = Time, y = Estimate,
                                                    color = paste(method, window, sep = "_"),
                                                    shape = window)) +
  geom_errorbar(aes(ymin = LB.CI, ymax = UB.CI),
                width    = 0.1,
                size     = 0.8,
                position = position_dodge(width = 0.6)) +
  geom_point(position = position_dodge(width = 0.5), size = 2.5) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_vline(xintercept = 0, linetype = "dotted") +
  
  coord_cartesian(xlim = c(-6, 6), ylim = ylim) +
  
  scale_color_manual(
    name   = NULL,
    values = palette_vals_all,
    breaks = names(palette_vals_all),
    labels = c(
      "DCDH; Pre",  "DCDH; Post",
      "Imputation; Pre",  "Imputation; Post",
      # "TWFE; Post","TWFE; Pre",
      "PanelMatch; Pre",
      "PanelMatch; Post"
    ),
    guide = guide_legend(ncol = 3, byrow = FALSE,
                         override.aes = list(
                           shape    = NA,       # remove points
                           linetype = "solid",  # just showing solid line
                           size     = 1         # 
                         ))
  ) +
  scale_shape_manual(
    name   = NULL,
    values = c("Pre" = 16, "Post" = 17),
    labels = c("Pre‐treatment", "Post‐treatment")
  ) +
  
  # choose legend
  guides(shape = "none") +
  
  labs(
    # title = "Number Outcome",
    title="",
    x = "Relative Time to Treatment Changes",
    y = "Number Count"
  ) +
  theme_minimal(base_size = 11
                , base_family = "TNR"
  ) +
  theme(
    legend.position   = "bottom",
    legend.key.width  = unit(1.5, "lines"),
    legend.spacing.x  = unit(0.07, "cm"),
    legend.box.margin = margin(0,0,0,0),
    legend.text       = element_text(size = 11.5),
    plot.title = element_text(size=11, hjust = 0.5)
  )

return(all_mplot_number)
}




combined_multiple_hetero_plot <- function(DCDH_list, imputation_list,panel_post,
                                       panel_pre, ylim = c(-2, 2),
                                       DCDH_list_num, imputation_list_num,panel_post_num,
                                       panel_pre_num, ylim_num = c(-0.2, 0.2) ){
  
  ## color
  
  palette_vals <- c(
   "DCDH_Pre" = "#DE2D26",
   "DCDH_Post" = "#3182BD",
    "Imputation_Pre"    = "#FFDA03",
    "Imputation_Post"   = "#4daf4a", 
    "PanelMatch_Post"   = "#8b4ac8"
  )
  
 
  
  palette_vals_all <- c(
    "DCDH_Pre" = "#d00000",
    "DCDH_Post" = "#45afff",
    "Imputation_Pre"    = "#e1aa00",
    "Imputation_Post"   = "#4daf4a",
    "PanelMatch_Pre"    = "#fe9e9e",
    "PanelMatch_Post"   = "#8300ff"
  )
  
  
  
  palette_vals_three <- c(

    "Imputation_Pre"    = "#DE2D26",
    "Imputation_Post"   = "#66C2A5",
    "PanelMatch_Pre"    = "#FB6A4A",
    "PanelMatch_Post"   = "#08519C"
  )
  
  
  # Imputation (FECT)
  df_mplot_imp <- tibble(
    Time     = imputation_list$time,
    Estimate = imputation_list$att,
    LB.CI    = imputation_list$att.bound[,1],
    UB.CI    = imputation_list$att.bound[,2]
  ) %>%
    mutate(
      method = "Imputation",
      window = ifelse(Time < 0, "Pre", "Post")
    )
  
  
  df_mplot_pmpre <- tibble(
    Time     = as.numeric(gsub("t", "", names(panel_pre$estimates))),
    Estimate = unname(panel_pre$estimates),
    SE       = unname(panel_pre$standard.errors)
  ) %>%
    mutate(
      LB.CI   = Estimate - qnorm(0.975)*SE,
      UB.CI   = Estimate + qnorm(0.975)*SE,
      method  = "PanelMatch",
      window  = "Pre"
    ) %>%
    dplyr::select(-SE)
  
  # Then, we have to add a ref. point at t=-1.
  
  df_mplot_pmpre_added <- df_mplot_pmpre %>%
    add_row(
      Time     = -1,
      Estimate = 0,
      LB.CI    = 0,
      UB.CI    = 0,
      method   = "PanelMatch",
      window   = "Pre"
    ) %>%
    arrange(Time)
  
  # df_mplot_pmpre_added 
  
  # PanelMatch Post
  # pm_mplot_post <- PE.results_s_baseline_outcomeplus
  
  df_mplot_pmpost <- tibble(
    Time     = as.numeric(gsub("t", "", names(panel_post$estimate))),
    Estimate = unname(panel_post$estimate),
    SE       = unname(panel_post$standard.error)
  ) %>%
    mutate(
      LB.CI   = Estimate - qnorm(0.975)*SE,
      UB.CI   = Estimate + qnorm(0.975)*SE,
      method  = "PanelMatch",
      window  = "Post"
    ) %>%
    dplyr::select(-SE)
  
  # DCDH
  df_mplot_DCDH <- tibble(
    Time     = DCDH_list$plot$data$Time,
    Estimate = DCDH_list$plot$data$Estimate,
    LB.CI    = DCDH_list$plot$data$LB.CI,
    UB.CI    = DCDH_list$plot$data$UB.CI
  ) %>%
    mutate(
      method = "DCDH",
      window = ifelse(Time < 0, "Pre", "Post")
    )
  
  
  all_df_mplot <- bind_rows(df_mplot_DCDH, df_mplot_imp, df_mplot_pmpre_added, df_mplot_pmpost)
  # all_nopmpre_df_mplot <- bind_rows(df_mplot_DCDH, df_mplot_imp, 
  #                                   # df_mplot_pmpre_added, 
  #                                   df_mplot_pmpost)
  # three_df_mplot <- bind_rows(df_mplot_imp, 
  #                             df_mplot_pmpre_added,
  #                             df_mplot_pmpost)
  
  
  # then, number outcome
  
  
  # Imputation (FECT)
  df_mplot_imp_number <- tibble(
    Time     = imputation_list_num$time,
    Estimate = imputation_list_num$att,
    LB.CI    = imputation_list_num$att.bound[,1],
    UB.CI    = imputation_list_num$att.bound[,2]
  ) %>%
    mutate(
      method = "Imputation",
      window = ifelse(Time < 0, "Pre", "Post")
    )
  
  # Panel Match Pre
  
  df_mplot_pmpre_number <- tibble(
    Time     = as.numeric(gsub("t", "", names(panel_pre_num$estimates))),
    Estimate = unname(panel_pre_num$estimates),
    SE       = unname(panel_pre_num$standard.errors)
  ) %>%
    mutate(
      LB.CI   = Estimate - qnorm(0.975)*SE,
      UB.CI   = Estimate + qnorm(0.975)*SE,
      method  = "PanelMatch",
      window  = "Pre"
    ) %>%
    dplyr::select(-SE)
  
  # Then, we have to add a ref. point at t=-1.
  
  df_mplot_pmpre_added_number <- df_mplot_pmpre_number %>%
    add_row(
      Time     = -1,
      Estimate = 0,
      LB.CI    = 0,
      UB.CI    = 0,
      method   = "PanelMatch",
      window   = "Pre"
    ) %>%
    arrange(Time)
  
  # PanelMatch Post
  
  df_mplot_pmpost_number <- tibble(
    Time     = as.numeric(gsub("t", "", names(panel_post_num$estimate))),
    Estimate = unname(panel_post_num$estimate),
    SE       = unname(panel_post_num$standard.error)
  ) %>%
    mutate(
      LB.CI   = Estimate - qnorm(0.975)*SE,
      UB.CI   = Estimate + qnorm(0.975)*SE,
      method  = "PanelMatch",
      window  = "Post"
    ) %>%
    dplyr::select(-SE)
  
  # DCDH
  df_mplot_DCDH_number <- tibble(
    Time     = DCDH_list_num$plot$data$Time,
    Estimate = DCDH_list_num$plot$data$Estimate,
    LB.CI    = DCDH_list_num$plot$data$LB.CI,
    UB.CI    = DCDH_list_num$plot$data$UB.CI
  ) %>%
    mutate(
      method = "DCDH",
      window = ifelse(Time < 0, "Pre", "Post")
    )
  
  
  all_df_mplot_number <- bind_rows(df_mplot_DCDH_number, df_mplot_imp_number, df_mplot_pmpre_added_number, df_mplot_pmpost_number)
  
  
 # Lastly, combined
  # prepare datasets
  df_mplot_dollar <- all_df_mplot %>%
    mutate(
      Outcome = factor(
        "Logged Dollar Amount",
        levels = "Logged Dollar Amount",
        labels = "Effect on Logged Dollar Amount"
      )
    )
  
  df_mplot_number <- all_df_mplot_number %>%
    mutate(
      Outcome = factor(
        "Number Count",
        levels = "Number Count",
        labels = "Effect on Number Count"
      )
    )
  

combined_mplot_df <- bind_rows(df_mplot_dollar, df_mplot_number)

mplot_box_style <- ggplot(combined_mplot_df,
                          aes(x = Time, y = Estimate,
                              color = paste(method, window, sep = "_"),
                              shape = window)) +
  
  geom_errorbar(aes(ymin = LB.CI, ymax = UB.CI),
                width    = 0.1, size = 0.8,
                position = position_dodge(0.6)) +
  
  geom_point(position = position_dodge(0.5), size = 2.5) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_vline(xintercept = 0, linetype = "dotted") +
  
  # use ggh4x, for better facet locations
  facet_wrap2(~ Outcome,
              ncol    = 1,
              scales  = "free_y",  # for different y scales at different plots
              strip.position = "top") +  # strip is for grey shaded titles
  
  # use different y scales for diff. plots
  facetted_pos_scales(
    y = list(
      Outcome == "Effect on Logged Dollar Amount"    ~ scale_y_continuous(limits = ylim),
      Outcome == "Effect on Number Count"   ~ scale_y_continuous(limits = ylim_num)
    )
  ) +
  
  # x lim is the same
  coord_cartesian(xlim = c(-6, 6))+
  
  scale_color_manual(
    name   = NULL,
    values = palette_vals_all,
    breaks = names(palette_vals_all),
    labels = c(
      "DCDH; Pre",  "DCDH; Post",
      "Imputation; Pre",  "Imputation; Post",
      "Panel Match; Pre",  "Panel Match; Post"
    )
  ) +
  scale_shape_manual(
    name   = NULL,
    values = c("Pre" = 16, "Post" = 17),
    labels = c("Pre‐treatment", "Post‐treatment")
  ) +
  # no need of shape legend
  guides(shape = "none") +
  
  labs(
    x = "Relative Time to Treatment Changes",
    y = NULL
  ) +
  
  theme_minimal(base_size = 14, base_family = "TNR") +
  theme(
    
    strip.background = element_rect(fill = "grey70", color = "black"),
    strip.text       = element_text(size = 11.5, family = "TNR"),
    strip.placement  = "outside",
    
    
    panel.background = element_rect(fill = "white", color = NA),
    panel.border     = element_rect(color = "black", fill = NA, size = 0.5),
    
    # space to avoid duplication
    panel.spacing    = unit(0.4, "cm"),
    
    # margin
    plot.margin      = unit(c(0.3, 0.3, 0.3, 0.3), "cm"),
    legend.position  = "bottom",
    legend.key.width = unit(1.5, "lines"),
    legend.spacing.x = unit(0.07, "cm"),
    legend.box.margin= margin(0,0,0,0),
    legend.text      = element_text(size = 11.5),
    
    axis.line        = element_line(color = "black"),
    axis.ticks       = element_line(color = "black"),
    axis.text        = element_text(color = "black"),
    axis.text.x      = element_text(),
    axis.ticks.x     = element_line(),
    axis.text.y      = element_text(),
    axis.ticks.y     = element_line()
  )

return(mplot_box_style)
}


# 4. Semi-parametric ------------------------------------------------------

estimate_local_SSimpact <- function(df, outcomes) {
  Ktri <- function(u) pmax(0, 1 - abs(u))
  kernel_margin <- df$pct_RDmargin_before
  
  # Silverman's ROT; multiplied by 1.25 or 1.5
  kernel_h <- 1.06 * sd(kernel_margin, na.rm = TRUE) * sum(!is.na(kernel_margin))^(-1/5) * 1.5
  
  grid_margin <- seq(
    quantile(kernel_margin, 0.05, na.rm = TRUE),
    quantile(kernel_margin, 0.95, na.rm = TRUE),
    length.out = 60
  )
  
  estimate_one <- function(margin, outcome) {
    
    weight <- Ktri((df$pct_RDmargin_before - margin) / kernel_h)
    
    if (sum(weight > 0, na.rm = TRUE) < 50) {
      return(tibble(
        outcome  = outcome, margin = margin, n_eff = sum(weight > 0),
        term     = "incident_window_re",
        estimate = NA_real_, se = NA_real_
      ))
    }
    
    idx <- which(weight > 0 &  !is.na(df[[outcome]]) &
                   !is.na(df$incident_window_re))
    n_eff <- length(idx)
    
    if (n_eff < 50) {
      return(tibble(
        outcome  = outcome, margin = margin, n_eff = n_eff,
        term     = "incident_window_re",
        estimate = NA_real_, se = NA_real_
      ))
    }
    
    y_loc <- df[[outcome]][idx]
    if (sum(!is.na(y_loc)) < 2 || sd(y_loc, na.rm = TRUE) == 0) {
      return(tibble(
        outcome  = outcome, margin = margin, n_eff = n_eff,
        term     = "incident_window_re",
        estimate = NA_real_, se = NA_real_
      ))
    }
    
    df$kernel_weight <- weight
    
    fml <- as.formula(paste0(
      outcome, " ~ incident_window_re + ",
      "bachelor + black + white + unemployment + log_income + rep_incumbent_before + gun_ratio_pct | id + time"
    ))
    
    fit <- fixest::feols(
      fml, data = df,
      weights = ~ kernel_weight,
      vcov = ~ id # or id + time  
    )
    
    get_coef <- function(fit, name) if (name %in% names(coef(fit))) unname(coef(fit)[name]) else NA_real_
    get_se   <- function(fit, name) if (name %in% names(se(fit)))   unname(se(fit)[name])   else NA_real_
    
    tibble(
      outcome  = outcome,
      margin   = margin,
      n_eff    = sum(weight > 0),
      term     = "incident_window_re",
      estimate = get_coef(fit, "incident_window_re"),
      se       = get_se(fit, "incident_window_re")
    )
  }
  
  map_dfr(grid_margin, function(margin) map_dfr(outcomes, ~estimate_one(margin, .x)))
}


