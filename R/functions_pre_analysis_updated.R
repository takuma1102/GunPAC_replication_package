# Functions before TWFE analysis


# 1. Data Wrangling -------------------------------------------------------

checkNA_first <- function(df) {
  # this might apply to USS data
  df <- df %>%
    drop_na(bachelor, white, black, Asian, otherrace,income,unemployment
            # num_total_before, num_total_next # It is ok to have NAs for these columns due to re-districtions.
    )
  return(df)
}

state_separate <- function(df) {
  if(!"state" %in% colnames(df)) {
    df <- df %>%
      separate(district, into = c("state", "within_state_district"), sep = "-", remove = FALSE)
    # remove should be false to keep the original column
  } else if("district" %in% colnames(df)){
    df <- df %>%
      separate(district, into = c(NA, "within_state_district"), sep = "-", remove = FALSE)
    # need to create within-state variable
  }
  return(df)
}

combine_progun_data <- function (anti_data, pro_data){
  combined_data <- anti_data %>%
    left_join(pro_data %>% 
                dplyr::select(district, year, month, pro_pac_donations_dollar,
                        pro_total_donations_number), 
      by = c("district","year","month")
    ) 
} 


check_percentage_numeric <- function(df){
  target_cols <- c("pct_margin", "pct_democratic", "pct_republican", 
                   "pct_independent", "pct_other", "bachelor", "income", 
                   "white", "black", "Asian", "otherrace", "unemployment")
  
  df <- df %>%
    mutate(across(any_of(target_cols), as.numeric))
  return(df)
}

new_var_HoR <- function(df){
  df <- as.data.frame(df) %>% 
    mutate(time = (year - 1990) * 12 + month, 
           # since the whole dataset contains data after Jan. 1990;
           # time 1 is set at Jan. 1990
           time = as.integer(time),  # integer is more convenient than numeric
           
           # for potential control variables
           even_year = ifelse(year %% 2 == 0, 1, 0),
           after_parkland = ifelse(year>=2018, 1, 0), 
           year_factor = as.factor(year),
           
           total_victim=fatalities+injured,
           
           # creating a dummy variable
           incident = ifelse(incident_count>0, 1, 0),  
           
           # negative amount of contributions means refund to PACs
           pro_refund = ifelse(pro_pac_donations_dollar<0, 1, 0),
           anti_refund = ifelse(anti_pac_donations_dollar<0, 1, 0),
           life_refund = ifelse(life_pac_donations_dollar<0, 1, 0),
           choice_refund = ifelse(choice_pac_donations_dollar<0, 1, 0),
           
           # remove data when refund=1
           pro_don_number = ifelse(pro_refund == 1, 0, pro_total_donations_number), 
           # converting to dummy variable
           pro_don_number = ifelse(pro_don_number > 0, 1, 0), 
           
           anti_don_number = ifelse(anti_refund == 1, 0, anti_total_donations_number),
           anti_don_number = ifelse(anti_don_number > 0, 1, 0),
           
           life_don_number = ifelse(life_refund == 1, 0, life_total_donations_number),  
           life_don_number = ifelse(life_don_number > 0, 1, 0), 
           
           choice_don_number = ifelse(choice_refund == 1, 0, choice_total_donations_number),  
           choice_don_number = ifelse(choice_don_number > 0, 1, 0),
           
           # remove contribution dollar data when refund=1
           pro_sum_donations  = ifelse(pro_refund == 1, 0, pro_pac_donations_dollar) ,   
           anti_sum_donations  = ifelse(anti_refund == 1, 0, anti_pac_donations_dollar) ,
           life_sum_donations  = ifelse(life_refund == 1, 0, life_pac_donations_dollar) ,
           choice_sum_donations  = ifelse(choice_refund == 1, 0, choice_pac_donations_dollar) ,
           
           pro_log_sum = log1p(pro_sum_donations), # Note that monthly contribution dollar might be zero.
           anti_log_sum = log1p(anti_sum_donations),
           life_log_sum = log1p(life_sum_donations),
           choice_log_sum = log1p(choice_sum_donations),
           
           id = as.integer(as.factor(district)),
           log_income = log(income),
           
           pct_dem_before = 100 * num_dem_before / num_total_before,
           # it is ok to produce NA when we have 0 for total number, because it will be removed anyway
           pct_rep_before = 100 * num_rep_before / num_total_before,
           
           pct_RDmargin_before = pct_rep_before - pct_dem_before,  # from GOP viewpoints
          
           # Incumbency
           rep_incumbent_before =  as.integer(ifelse(rep_rank_before == 1, 1, 0)),
           rep_incumbent_next =  as.integer(ifelse(rep_rank_next == 1, 1, 0)),
           
           # next election data, especially for sections to estimate the impact on the next election
           pct_dem_next = 100 * num_dem_next / num_total_next,
           pct_rep_next = 100 * num_rep_next / num_total_next,
           pct_RDmargin_next = pct_rep_next - pct_dem_next,
           
           # difference election outcomes between the previous and the next election
           pct_rep_change = pct_rep_next - pct_rep_before,
           pct_RDmargin_change = pct_RDmargin_next-pct_RDmargin_before,
           incumbent_change = rep_incumbent_next-rep_incumbent_before,
           
           election_year_next_factor = as.factor(election_year_next), # for future grouping
           
           # change state into numeric
           state_num = as.numeric(as.factor(state)),
           
           # the original month_to_election ranges 1 to 24, but will be converted here to "0 to 23" for the consistency
           # with other datasets
           months_to_election = months_to_election-1,
           
           competitive_factor = ifelse(abs(pct_RDmargin_before)<=5, 1, 0),
           
           # for potential control variable
           president_rep = if_else((year >= 1989 & year <= 1992) | 
                                     (year >= 2001 & year <= 2008) | 
                                     (year >= 2017 & year <= 2020), 1, 0),
           
           gun_ratio_pct = 100 * gun_ratio
           # to maintain consistency with other percentage variables
    )%>% 
    
    # Next, create treatment variables
    arrange(id, time) %>%
    group_by(id) %>%
    mutate(history = cummax(incident)) %>%  # dummy showing if incident already has happened
    mutate(first = if (any(history == 1)) min(time[history == 1]) else 0) %>% # It should be 0, not NA or Inf
    # first year for incidents at a given district
    
    # allow to come back to control units after some months of an incident
    mutate(last_incident_time = ifelse(incident == 1, time, NA)) %>% # specify the last incident time
    fill(last_incident_time, .direction = "down") %>%
    mutate(incident_window = if_else(!is.na(last_incident_time) & (time - last_incident_time <= 24), 1, 0),
           incident_window = as.integer(incident_window), # integer is better than numeric
           # for event study in post-treatment periods
           time_since_event = if_else(!is.na(last_incident_time) & (time - last_incident_time <= 24), time - last_incident_time, NA)
           
           # 0 time_since_event means the month of incidents.
    ) %>%
    
    # for event study; mainly for pre-treatment, to avoid double-hat issues
    mutate(
      # the same as last_incident_time, but after defining this, filled in the opposite direction
      next_incident_time = ifelse(incident == 1, time, NA)) %>%
    fill(next_incident_time, .direction = "up") %>%
    
    mutate(time_to_event = case_when(
      # if you want to set this for after incident, but be careful with avoiding double-hat issues
      # !is.na(last_incident_time) ~ time - last_incident_time,
      
      # before incident, it should be negative for event study
      # is.na(last_incident_time) & 
      !is.na(next_incident_time) ~ time - next_incident_time,
      
      # 0 time_to_event means the month of the incident.
      # NA for never-treated and others
      TRUE ~ NA_real_
    )) %>%
    
    # for event study - pre-treatment
    mutate(incident_before0= ifelse(time_to_event==0, 1, 0), # the month of incidents
           incident_before1= ifelse(time_to_event==-1, 1, 0),
           incident_before2= ifelse(time_to_event==-2, 1, 0),
           incident_before3= ifelse(time_to_event==-3, 1, 0),
           incident_before4= ifelse(time_to_event==-4, 1, 0),
           incident_before5= ifelse(time_to_event==-5, 1, 0),
           incident_before6= ifelse(time_to_event==-6, 1, 0),
           incident_after1= ifelse(time_since_event==1, 1, 0),
           incident_after2= ifelse(time_since_event==2, 1, 0),
           incident_after3= ifelse(time_since_event==3, 1, 0),
           incident_after4= ifelse(time_since_event==4, 1, 0),
           incident_after5= ifelse(time_since_event==5, 1, 0),
           incident_after6= ifelse(time_since_event==6, 1, 0),
           incident_after7= ifelse(time_since_event==7, 1, 0),
           incident_after8= ifelse(time_since_event==8, 1, 0),
           incident_after9= ifelse(time_since_event==9, 1, 0),
           incident_after10= ifelse(time_since_event==10, 1, 0),
           incident_after11= ifelse(time_since_event==11, 1, 0),
           incident_after12= ifelse(time_since_event==12, 1, 0)
    ) %>% 
    
    ungroup()
  
  # movign avg.
  # Main use case is for DCDH
  # in the case of DID, backward moving avg., which does not use future info, will be appropriate.
  # LOESS might be an option, but would be inappropriate as outcome. Rather, it is for visualization
  
  df <- df %>% 
    group_by(id) %>%               # have to group by district, which is necessary
    arrange(time) %>%              # sort based on time
    mutate(
      pro_sum_donations_mva1 = slide_dbl(pro_sum_donations, .before   = 1, # from x months before; 3 might be practice for monthly data??
                                         .after    = 0, .f        = mean,
                                         .complete = FALSE  # compute even if there are not enough months
      ),
      pro_sum_donations_mva2 = slide_dbl(pro_sum_donations, .before   = 2, .after    = 0,
                                         .f        = mean, .complete = FALSE),
      pro_sum_donations_mva3 = slide_dbl(pro_sum_donations, .before   = 3, .after    = 0,
                                         .f        = mean, .complete = FALSE),
      pro_sum_donations_mva5 = slide_dbl(pro_sum_donations, .before   = 5, .after    = 0,
                                         .f        = mean, .complete = FALSE),
      
      # log form after summing might be reasonable
      # avlog means average's log
      pro_log_sum_mva_avlog1 = log1p(pro_sum_donations_mva1),
      pro_log_sum_mva_avlog2 = log1p(pro_sum_donations_mva2),
      pro_log_sum_mva_avlog3 = log1p(pro_sum_donations_mva3),
      pro_log_sum_mva_avlog5 = log1p(pro_sum_donations_mva5),
      
      # The following patterns might make less sense, but we will use these
      # logav means log's average
      pro_log_sum_mva_logav1 = slide_dbl(pro_log_sum, .before = 1,.after  = 0, .f      = mean, .complete = FALSE),
      pro_log_sum_mva_logav2 = slide_dbl(pro_log_sum, .before = 2,.after  = 0, .f      = mean, .complete = FALSE),
      pro_log_sum_mva_logav3 = slide_dbl(pro_log_sum, .before = 3,.after  = 0, .f      = mean, .complete = FALSE),
      pro_log_sum_mva_logav5 = slide_dbl(pro_log_sum, .before = 5,.after  = 0, .f      = mean, .complete = FALSE),
      
      # number outcome 
      pro_don_number_mva_5 = slide_dbl(pro_don_number, .before = 5,.after  = 0, .f      = mean, .complete = FALSE),
      pro_don_number_mva_3 = slide_dbl(pro_don_number, .before = 3,.after  = 0, .f      = mean, .complete = FALSE), 
      pro_don_number_mva_2 = slide_dbl(pro_don_number, .before = 2,.after  = 0, .f      = mean, .complete = FALSE)) %>% 
    
    mutate(
      anti_sum_donations_mva1 = slide_dbl(anti_sum_donations, .before   = 1, # from x months before; 3 might be practice for monthly data??
                                          .after    = 0, .f        = mean,
                                          .complete = FALSE  # compute even if there are not enough months
      ),
      anti_sum_donations_mva2 = slide_dbl(anti_sum_donations, .before   = 2, .after    = 0,
                                          .f        = mean, .complete = FALSE),
      anti_sum_donations_mva3 = slide_dbl(anti_sum_donations, .before   = 3, .after    = 0,
                                          .f        = mean, .complete = FALSE),
      anti_sum_donations_mva5 = slide_dbl(anti_sum_donations, .before   = 5, .after    = 0,
                                          .f        = mean, .complete = FALSE),
      
      # log form after summing might be reasonable
      # avlog means average's log
      anti_log_sum_mva_avlog1 = log1p(anti_sum_donations_mva1),
      anti_log_sum_mva_avlog2 = log1p(anti_sum_donations_mva2),
      anti_log_sum_mva_avlog3 = log1p(anti_sum_donations_mva3),
      anti_log_sum_mva_avlog5 = log1p(anti_sum_donations_mva5),
      
      # The following patterns might make less sense, but we will use these
      # logav means log's average
      anti_log_sum_mva_logav1 = slide_dbl(anti_log_sum, .before = 1,.after  = 0, .f      = mean, .complete = FALSE),
      anti_log_sum_mva_logav2 = slide_dbl(anti_log_sum, .before = 2,.after  = 0, .f      = mean, .complete = FALSE),
      anti_log_sum_mva_logav3 = slide_dbl(anti_log_sum, .before = 3,.after  = 0, .f      = mean, .complete = FALSE),
      anti_log_sum_mva_logav5 = slide_dbl(anti_log_sum, .before = 5,.after  = 0, .f      = mean, .complete = FALSE),
      
      # number outcome 
      anti_don_number_mva_5 = slide_dbl(anti_don_number, .before = 5,.after  = 0, .f      = mean, .complete = FALSE),
      anti_don_number_mva_3 = slide_dbl(anti_don_number, .before = 3,.after  = 0, .f      = mean, .complete = FALSE), 
      anti_don_number_mva_2 = slide_dbl(anti_don_number, .before = 2,.after  = 0, .f      = mean, .complete = FALSE)) %>% 
    
    
    ungroup()
  
  return(df)
}



mutual_history <- function(df){
  df %>%
    group_by(id, election_year_next) %>%
    arrange(time) %>%
    mutate(
      # Check if there's any incident in this election cycle
      cycle_has_incident = max(incident, na.rm = TRUE),
      
      # Find the earliest incident time in this cycle
      cycle_incident_time = ifelse(cycle_has_incident >= 1, 
                                   min(time[incident == 1], na.rm = TRUE), 
                                   NA_real_)
    ) %>%
    mutate(
      # sum of prior donations
      # FYI, we used log's average for the PAC paper
      pro_nonlogsum_incident_prior = case_when(
        cycle_has_incident == 1 & (time >= cycle_incident_time) ~ 
          cumsum(ifelse(time >= cycle_incident_time, pro_sum_donations, 0)),
        TRUE ~ 0), # TRUE means else if, or otherwise
      anti_nonlogsum_incident_prior = case_when(
        cycle_has_incident == 1 & (time >= cycle_incident_time) ~ 
          cumsum(ifelse(time >= cycle_incident_time, anti_sum_donations, 0)),
        TRUE ~ 0),
      
      # post-treatment variable, so avoid using
      pro_logsum_incident_prior = case_when(
        cycle_has_incident == 1 & (time >= cycle_incident_time) ~ 
          cumsum(ifelse(time >= cycle_incident_time, pro_log_sum, 0)),
        TRUE ~ 0), # TRUE means else if, or otherwise
      anti_logsum_incident_prior = case_when(
        cycle_has_incident == 1 & (time >= cycle_incident_time) ~ 
          cumsum(ifelse(time >= cycle_incident_time, anti_log_sum, 0)),
        TRUE ~ 0),
      
      # Simple 3-month rolling sum within election cycle (regardless of incident)
      pro_sum_3month_prior = map_dbl(seq_along(time), ~{  # dbl returns number
        current_time <- time[.x] #[.x] is now
        three_months_ago <- current_time - 3  
        
        sum(pro_sum_donations[time < current_time & time >= three_months_ago], na.rm = TRUE)
      }),
      
      anti_sum_3month_prior = map_dbl(seq_along(time), ~{
        current_time <- time[.x]
        three_months_ago <- current_time - 3
        
        sum(anti_sum_donations[time < current_time & time >= three_months_ago], na.rm = TRUE)
      }),
      # FYI, we used log's average for the PAC paper, 
      # so this should be the main pattern??
      pro_logsum_3month_prior = map_dbl(seq_along(time), ~{
        current_time <- time[.x]
        three_months_ago <- current_time - 3
        
        sum(pro_log_sum[time < current_time & time >= three_months_ago], na.rm = TRUE)
      }),
      
      anti_logsum_3month_prior = map_dbl(seq_along(time), ~{
        current_time <- time[.x]
        three_months_ago <- current_time - 3
        
        sum(anti_log_sum[time < current_time & time >= three_months_ago], na.rm = TRUE)
      }),
      
      # also, present pattern, especially for the impact on the next election
      pro_logsum_3month_present = map_dbl(seq_along(time), ~{
        current_time <- time[.x]
        three_months_ago <- current_time - 2 # this name of three is ok.
        
        sum(pro_log_sum[time <= current_time & time >= three_months_ago], na.rm = TRUE)
      }),
      
      anti_logsum_3month_present = map_dbl(seq_along(time), ~{
        current_time <- time[.x]
        three_months_ago <- current_time - 2
        
        sum(anti_log_sum[time <= current_time & time >= three_months_ago], na.rm = TRUE)
      }),
      
      
      # next 6 months
      pro_sum_6month_prior = map_dbl(seq_along(time), ~{  # dbl returns number
        current_time <- time[.x] #[.x] is now
        six_months_ago <- current_time - 6  
        
        sum(pro_sum_donations[time < current_time & time >= six_months_ago], na.rm = TRUE)
      }),
      
      anti_sum_6month_prior = map_dbl(seq_along(time), ~{
        current_time <- time[.x]
        six_months_ago <- current_time - 6
        
        sum(anti_sum_donations[time < current_time & time >= six_months_ago], na.rm = TRUE)
      }),
      
      pro_logsum_6month_prior = map_dbl(seq_along(time), ~{
        current_time <- time[.x]
        six_months_ago <- current_time - 6
        
        sum(pro_log_sum[time < current_time & time >= six_months_ago], na.rm = TRUE)
      }),
      
      anti_logsum_6month_prior = map_dbl(seq_along(time), ~{
        current_time <- time[.x]
        six_months_ago <- current_time - 6
        
        sum(anti_log_sum[time < current_time & time >= six_months_ago], na.rm = TRUE)
      }),
      
      # present pattern
      pro_logsum_6month_present = map_dbl(seq_along(time), ~{
        current_time <- time[.x]
        six_months_ago <- current_time - 5
        
        sum(pro_log_sum[time <= current_time & time >= six_months_ago], na.rm = TRUE)
      }),
      
      anti_logsum_6month_present = map_dbl(seq_along(time), ~{
        current_time <- time[.x]
        six_months_ago <- current_time - 5
        
        sum(anti_log_sum[time <= current_time & time >= six_months_ago], na.rm = TRUE)
      })
      
    ) %>%
    ungroup()
  # dplyr::select(-cycle_has_incident, -cycle_incident_time) # we can leave these in. No harm
}



new_var_Sen <- function(df){
  df <- as.data.frame(df) %>% 
    mutate(time = (year - 1990) * 12 + month,
           time = as.integer(time),  
           even_year = ifelse(year %% 2 == 0, 1, 0),
           
           incident = ifelse(incident_count>0, 1, 0),  
           pro_refund = ifelse(pro_pac_donations_dollar<0, 1, 0),  
           anti_refund = ifelse(anti_pac_donations_dollar<0, 1, 0),  
           
           pro_don_number = ifelse(pro_refund == 1, 0, pro_total_donations_number),  
           pro_don_number = ifelse(pro_don_number > 0, 1, 0), 
           
           anti_don_number = ifelse(anti_refund == 1, 0, anti_total_donations_number),  
           anti_don_number = ifelse(anti_don_number > 0, 1, 0), 
           
           pro_sum_donations  = ifelse(pro_refund == 1, 0, pro_pac_donations_dollar) ,   # remove data when refund=1
           pro_log_sum = log1p(pro_sum_donations), #ok to use, since I removed negative amount of donations
           
           anti_sum_donations  = ifelse(anti_refund == 1, 0, anti_pac_donations_dollar) ,   # remove data when refund=1
           anti_log_sum = log1p(anti_sum_donations), 
           
           # state-by-state analysis; unique to Senate data
           id = as.integer(as.factor(fips)),
           log_income = log(income),
           
           # No need at all for state_num, but just for consistency...
           state_num = as.numeric(as.factor(state)),
           
           
           president_rep = if_else((year >= 1989 & year <= 1992) | 
                                     (year >= 2001 & year <= 2008) | 
                                     (year >= 2017 & year <= 2020), 1, 0),
           
           gun_ratio_pct = 100 * gun_ratio  # again, be careful about %
           
    )%>% 
    
    arrange(id, time) %>%
    group_by(id) %>%
    mutate(history = cummax(incident)) %>%  # dummy showing if incident already has happened
    mutate(first = if (any(history == 1)) min(time[history == 1]) else 0) %>% # It should be 0, not NA or Inf, when using callaway
    # first year for incidents at a given district
    
    # allow to come back to control units after 60 or some months of an incident
    mutate(last_incident_time = ifelse(incident == 1, time, NA)) %>% # specify the last incident time
    fill(last_incident_time, .direction = "down") %>%
    mutate(incident_window = if_else(!is.na(last_incident_time) & (time - last_incident_time <= 24), 1, 0),
           incident_window = as.integer(incident_window) # integer is better than numeric
    ) %>%
    ungroup()
  return(df)
}


new_var_state <- function(df){
  df <- df %>% 
    mutate(time = (year - 1990) * 12 + month,
           time = as.integer(time),  
           even_year = ifelse(year %% 2 == 0, 1, 0),
           
           incident = ifelse(incident_count>0, 1, 0),  # make it dummy
           pro_refund = ifelse(pro_pac_donations_dollar<0, 1, 0),
           anti_refund = ifelse(anti_pac_donations_dollar<0, 1, 0),
           
           # Net means GOP minus DEM, irrelevant to pro- or anti-
           # but anti- doesn't have this data now.
           pro_refund_net = ifelse(pro_pac_donations_dollar_net<0, 1, 0),
           
           pro_don_number = ifelse(pro_refund == 1, 0, pro_total_donations_number),
           pro_don_number = ifelse(pro_don_number > 0, 1, 0), 
           anti_don_number = ifelse(anti_refund == 1, 0, anti_total_donations_number),
           anti_don_number = ifelse(anti_don_number > 0, 1, 0), 
           
           pro_don_number_net = ifelse(pro_refund_net == 1, 0, pro_total_donations_number_net),  
           pro_don_number_net = ifelse(pro_don_number_net > 0, 1, 0),
           
           pro_sum_donations  = ifelse(pro_refund == 1, 0, pro_pac_donations_dollar) ,   # remove data when refund=1
           pro_log_sum = log1p(pro_sum_donations),
           anti_sum_donations  = ifelse(anti_refund == 1, 0, anti_pac_donations_dollar) ,   # remove data when refund=1
           anti_log_sum = log1p(anti_sum_donations),
           
           pro_sum_donations_net  = ifelse(pro_refund_net == 1, 0, pro_pac_donations_dollar_net) ,   # remove data when refund=1
           pro_log_sum_net = log1p(pro_sum_donations_net),
           
           # state-by-state analysis
           id = as.integer(as.factor(fips)),
           log_income = log(income),
           
           # No need at all for state_num, but just for consistency...
           state_num = as.numeric(as.factor(state)),
           # months_to_election_inv = 1/months_to_election,
           president_rep = if_else((year >= 1989 & year <= 1992) | 
                                     (year >= 2001 & year <= 2008) | 
                                     (year >= 2017 & year <= 2020), 1, 0),
           gun_ratio_pct = 100 * gun_ratio  # again, be careful about %
           
           
    )%>% 
    
    arrange(id, time) %>%
    group_by(id) %>%
    mutate(history = cummax(incident)) %>%  # dummy showing if incident already has happened
    mutate(first = if (any(history == 1)) min(time[history == 1]) else 0) %>% # It should be 0, not NA or Inf
    # first year for incidents at a given district
    
    # allow to come back to control units after 60 or some months of an incident
    mutate(last_incident_time = ifelse(incident == 1, time, NA)) %>% # specify the last incident time
    fill(last_incident_time, .direction = "down") %>%
    mutate(incident_window = if_else(!is.na(last_incident_time) & (time - last_incident_time <= 24), 1, 0),
           incident_window = as.integer(incident_window) # integer is better than numeric
    ) %>%
    ungroup()
  return(df)
}


checkNA_second_HoR <- function(df){
  df <- df %>%
    drop_na(id, time, incident_window, pct_RDmargin_before,log_income
            # Do not include "last_incident_time"
    )
  return(df)
} 


checkNA_second_nonHoR <- function(df){
  df <- df %>%
    drop_na(id, time, incident_window,log_income)
            # no "pct_RDmargin_before"
  return(df)
} 



# 2. Create Treatment Variable and Filter Data ----------------------------

## 2.1 Basic for HoR ---------------------------------------------------------------

# Whole dataset; minimum filter; unfiltered in terms of voting margin
# no restriction on minimum threshold of contributions; including non-fatal cases

data_treatment_name <- function(df, start_year=2000){
  df <- as.data.frame(df)  %>%
    mutate(pro_don_number_raw=pro_don_number,
           pro_log_sum_raw=pro_log_sum,
           anti_don_number_raw=anti_don_number,
           anti_log_sum_raw=anti_log_sum,
           anti_sum_donations_raw=anti_sum_donations,
           pro_sum_donations_raw=pro_sum_donations
           ) %>% 
    
    arrange(id, time) %>%
    group_by(id) %>%
    
    mutate(last_incident_time_re = ifelse(incident == 1, time, NA)) %>% # specify the last incident time
    fill(last_incident_time_re, .direction = "down") %>%
    mutate(incident_window_re = if_else(!is.na(last_incident_time_re) & (time - last_incident_time_re <= 24), 1, 0),
           incident_window_re = as.integer(incident_window_re)) %>%
    ungroup() %>% 
    
    filter(year>=start_year)
  # to produce treatment variable from 2000 and use prior data for that, year filtering
  # should come after setting treatment variables.
  return(df)
} 
  
# Next, baseline function
data_margin_baseline <- function(df,  # should use a raw data, instead of minimum filtered dataset
                                 voting_margin=5,
                                 exclude_fatality=0 # be careful; this is not the minimum threshold
                                 ){
  df <- as.data.frame(df) %>% 
    filter(abs(pct_RDmargin_before)<=voting_margin) %>% 
    # should come this first, unlike other filtering
    mutate(
      # incident is a dummy variable; incident_count is a raw variable
           incident = ifelse(incident >= 1 &fatalities>exclude_fatality, 1, 0), # we have to re-define incident window and history!
           
           pro_don_number_raw=pro_don_number,
           pro_log_sum_raw=pro_log_sum,
           anti_don_number_raw=anti_don_number,
           anti_log_sum_raw=anti_log_sum,
           anti_sum_donations_raw=anti_sum_donations,
           pro_sum_donations_raw=pro_sum_donations,
           
           pro_don_number = ifelse(pro_pac_donations_dollar>=10000, pro_don_number, 0),
           pro_log_sum = ifelse(pro_pac_donations_dollar>=10000, pro_log_sum, 0),
           anti_don_number = ifelse(anti_pac_donations_dollar>=10000, anti_don_number, 0),
           anti_log_sum = ifelse(anti_pac_donations_dollar>=10000, anti_log_sum, 0),
           pro_sum_donations = ifelse(pro_pac_donations_dollar>=10000, pro_sum_donations_raw, 0),
           anti_sum_donations = ifelse(anti_pac_donations_dollar>=10000, anti_sum_donations_raw, 0)
    ) %>% 
    
    arrange(id, time) %>%
    group_by(id) %>%
    
    # first treatment var.
    mutate(last_incident_time_re = ifelse(incident == 1, time, NA)) %>% # specify the last incident time
    fill(last_incident_time_re, .direction = "down") %>%
    mutate(incident_window_re = if_else(!is.na(last_incident_time_re) & (time - last_incident_time_re <= 24), 1, 0),
           incident_window_re = as.integer(incident_window_re)) %>%
    ungroup() %>% 
    
    filter(year>=2000) 
    
  return(df)
}

data_flexible <- function(df, voting_margin=5, # if you want to use assymmetry voting margin, use another function
                          fatality_threshold=1, # It can be zero; not excluded fatality
                          minimum_dol=10000,  year_start=2000, month_scope=24,
                          month_to = 23,  incumbent = "All", # or R or D
                          year_end=2024){
  
  if (incumbent=="R"){
    df <- as.data.frame(df) %>%
      filter(rep_incumbent_before==1) 
  } else if (incumbent=="D"){
    df <- as.data.frame(df) %>%
      filter(rep_incumbent_before==0) 
  } else if (incumbent=="All"){
    df <- as.data.frame(df)
  }
  
  df <- as.data.frame(df) %>%
    filter(abs(pct_RDmargin_before)<=voting_margin) %>% 
    mutate(incident = ifelse(incident >= 1 & fatalities>=fatality_threshold & months_to_election <= month_to,
                             1, 0),
           
           pro_don_number_raw=pro_don_number,
           pro_log_sum_raw=pro_log_sum,
           anti_don_number_raw=anti_don_number,
           anti_log_sum_raw=anti_log_sum,,
           anti_sum_donations_raw=anti_sum_donations,
           pro_sum_donations_raw=pro_sum_donations,
           
           pro_don_number = ifelse(pro_pac_donations_dollar>=minimum_dol, pro_don_number, 0),
           pro_log_sum = ifelse(pro_pac_donations_dollar>=minimum_dol, pro_log_sum, 0),
           anti_don_number = ifelse(anti_pac_donations_dollar>=minimum_dol, anti_don_number, 0),
           anti_log_sum = ifelse(anti_pac_donations_dollar>=minimum_dol, anti_log_sum, 0),
           pro_sum_donations = ifelse(pro_pac_donations_dollar>=10000, pro_sum_donations_raw, 0),
           anti_sum_donations = ifelse(anti_pac_donations_dollar>=10000, anti_sum_donations_raw, 0)) %>% 
  
    arrange(id, time) %>%
    group_by(id) %>%
    
    mutate(last_incident_time_re = ifelse(incident == 1, time, NA)) %>% # specify the last incident time
    fill(last_incident_time_re, .direction = "down") %>%
    mutate(incident_window_re = if_else(!is.na(last_incident_time_re) & (time - last_incident_time_re <= month_scope), 1, 0),
           incident_window_re = as.integer(incident_window_re)) %>%
    ungroup() %>% 
    
    filter(year>=year_start) %>% 
    filter(year<=year_end) 
    # symmetrical voting margin threshold
  return(df)
}


data_voting_filter <- function(df, voting_min=-5, voting_max=5){
  df <- as.data.frame(df) %>%
    # Do not use abs().
    filter(pct_RDmargin_before<=voting_max) %>% 
    filter(pct_RDmargin_before>=voting_min) %>% 
    
    mutate(incident = ifelse(incident >= 1 &fatalities>0, 1, 0),
           
           pro_don_number_raw=pro_don_number,
           pro_log_sum_raw=pro_log_sum,
           anti_don_number_raw=anti_don_number,
           anti_log_sum_raw=anti_log_sum,,
           anti_sum_donations_raw=anti_sum_donations,
           pro_sum_donations_raw=pro_sum_donations,
           
           anti_don_number = ifelse(anti_pac_donations_dollar>=10000, anti_don_number, 0),
           anti_log_sum = ifelse(anti_pac_donations_dollar>=10000, anti_log_sum, 0),
           pro_don_number = ifelse(pro_pac_donations_dollar>=10000, pro_don_number, 0),
           pro_log_sum = ifelse(pro_pac_donations_dollar>=10000, pro_log_sum, 0),
           pro_sum_donations = ifelse(pro_pac_donations_dollar>=10000, pro_sum_donations_raw, 0),
           anti_sum_donations = ifelse(anti_pac_donations_dollar>=10000, anti_sum_donations_raw, 0)) %>% 
    
    arrange(id, time) %>%
    group_by(id) %>%
    
    mutate(last_incident_time_re = ifelse(incident == 1, time, NA)) %>% # specify the last incident time
    fill(last_incident_time_re, .direction = "down") %>%
    mutate(incident_window_re = if_else(!is.na(last_incident_time_re) & (time - last_incident_time_re <= 24), 1, 0),
           incident_window_re = as.integer(incident_window_re)) %>%
    
    ungroup() %>% 
    filter(year>=2000) 
  return(df)
}

data_nonfatal_unfiltered <- function(df, start_year=2000){
  df <- as.data.frame(df)  %>%
    filter(year>=start_year) %>% 
    mutate(incident = ifelse(incident >= 1 &fatalities==0, 1, 0))
  # have to use the name of incident here, in a manner consistent with the existing function,
  # to use another function in a consistent way
  return(df)
}

data_noncompetitive_unfiltered <- function(df, voting_lower_threshold=5){
  df <- as.data.frame(df) %>%
    filter(pct_RDmargin_before>=voting_lower_threshold) %>% 
    
    mutate(incident = ifelse(incident >= 1 &fatalities>0, 1, 0),
           
           pro_don_number_raw=pro_don_number,
           pro_log_sum_raw=pro_log_sum,
           anti_don_number_raw=anti_don_number,
           anti_log_sum_raw=anti_log_sum,,
           anti_sum_donations_raw=anti_sum_donations,
           pro_sum_donations_raw=pro_sum_donations,
           
           anti_don_number = ifelse(anti_pac_donations_dollar>=10000, anti_don_number, 0),
           anti_log_sum = ifelse(anti_pac_donations_dollar>=10000, anti_log_sum, 0),
           pro_don_number = ifelse(pro_pac_donations_dollar>=10000, pro_don_number, 0),
           pro_log_sum = ifelse(pro_pac_donations_dollar>=10000, pro_log_sum, 0),
           pro_sum_donations = ifelse(pro_pac_donations_dollar>=10000, pro_sum_donations_raw, 0),
           anti_sum_donations = ifelse(anti_pac_donations_dollar>=10000, anti_sum_donations_raw, 0)) %>% 
    
    arrange(id, time) %>%
    group_by(id) %>%
    
    mutate(last_incident_time_re = ifelse(incident == 1, time, NA)) %>% # specify the last incident time
    fill(last_incident_time_re, .direction = "down") %>%
    mutate(incident_window_re = if_else(!is.na(last_incident_time_re) & (time - last_incident_time_re <= 24), 1, 0),
           incident_window_re = as.integer(incident_window_re)) %>%
    
    ungroup() %>% 
    filter(year>=2000) 
  return(df)
}

data_withinstate <- function(df, voting_margin_first=100, # two thresholds
                             # first, the one about incidents, to include shooting incidents taking place in non-competitive districts
                             voting_margin_second=5, # second, for districts, scope of analysis, used in the last phase
                             # fatality_threshold=1,
                             # need not to be set, since other fatality threshold, 5 and 10 are 
                             # specified through other treatment variables within this function
                             minimum_dol=10000, year_threshold=2000){
  
  df <- as.data.frame(df) %>%
    filter(abs(pct_RDmargin_before)<=voting_margin_first) %>% # default is 100 to cover many shooting incidents 
    mutate(incident = ifelse(incident >= 1 & fatalities>=1, 1, 0)) %>%
    
    # for many fatality cases
    # 5 and more
    mutate(incident5am = ifelse(incident >= 1 & fatalities>=5, 1, 0)) %>%
    # include those who not only killed, but also injured
    mutate(incident5vam = ifelse(incident >= 1 & total_victim>=5, 1, 0)) %>%
    
    # 1 to 4
    mutate(incident1_4 = ifelse(incident >= 1 & fatalities<5, 1, 0)) %>%
    mutate(incident1_4v = ifelse(incident >= 1 & total_victim<5, 1, 0)) %>%
    
    # 10 and more
    mutate(incident10am = ifelse(incident >= 1 & fatalities>=10, 1, 0)) %>%
    # including injured
    mutate(incident10vam = ifelse(incident >= 1 & total_victim>=10, 1, 0)) %>%
    
    # 5-9
    mutate(incident5_9 = ifelse(incident5am == 1 & fatalities<10, 1, 0)) %>%
    mutate(incident5_9v = ifelse(incident5vam == 1 & total_victim<10, 1, 0)) %>%
    
    mutate(pro_don_number_raw=pro_don_number,
           pro_log_sum_raw=pro_log_sum,
           anti_don_number_raw=anti_don_number,
           anti_log_sum_raw=anti_log_sum,,
           anti_sum_donations_raw=anti_sum_donations,
           pro_sum_donations_raw=pro_sum_donations,
           
           pro_don_number = ifelse(pro_pac_donations_dollar>=minimum_dol, pro_don_number, 0),
           pro_log_sum = ifelse(pro_pac_donations_dollar>=minimum_dol, pro_log_sum, 0),
           anti_don_number = ifelse(anti_pac_donations_dollar>=minimum_dol, anti_don_number, 0),
           anti_log_sum = ifelse(anti_pac_donations_dollar>=minimum_dol, anti_log_sum, 0),
           pro_sum_donations = ifelse(pro_pac_donations_dollar>=10000, pro_sum_donations_raw, 0),
           anti_sum_donations = ifelse(anti_pac_donations_dollar>=10000, anti_sum_donations_raw, 0)) %>% 
  
    group_by(state, time) %>% # not grouped by id
    mutate(state_incident = as.integer(any(incident == 1))) %>%
    
    mutate(state_incident5am = as.integer(any(incident5am == 1))) %>%
    mutate(state_incident5vam = as.integer(any(incident5vam == 1))) %>%
    
    mutate(state_incident1_4 = as.integer(any(incident1_4 == 1))) %>%
    mutate(state_incident1_4v = as.integer(any(incident1_4v == 1))) %>%
    
    mutate(state_incident10am = as.integer(any(incident10am == 1))) %>%
    mutate(state_incident10vam = as.integer(any(incident10vam == 1))) %>%
    
    mutate(state_incident5_9 = as.integer(any(incident5_9 == 1))) %>%
    mutate(state_incident5_9v = as.integer(any(incident5_9v == 1))) %>%
    ungroup() %>% 

    arrange(id, time) %>%
    group_by(id) %>%
    
    # First, basic treatment var.
    mutate(last_incident_time_re = ifelse(incident == 1, time, NA)) %>% # specify the last incident time
    fill(last_incident_time_re, .direction = "down") %>%
    mutate(incident_window_re = if_else(!is.na(last_incident_time_re) & (time - last_incident_time_re <= 24), 1, 0),
           incident_window_re = as.integer(incident_window_re)) %>%
    
    # Next, state-related incidents
    # excluding treated district!!
    mutate(last_incident_time_re_state = ifelse(state_incident == 1, time, NA)) %>% # specify the last incident time
    fill(last_incident_time_re_state, .direction = "down") %>%
    mutate(incident_window_re_state = if_else(!is.na(last_incident_time_re_state) & (time - last_incident_time_re_state <= 24) & (incident_window_re == 0), 1, 0),
           incident_window_re_state = as.integer(incident_window_re_state)) %>%
    
    mutate(last_incident_time_re_state5am = ifelse(state_incident5am == 1, time, NA)) %>% # specify the last incident time
    fill(last_incident_time_re_state5am, .direction = "down") %>%
    mutate(incident_window_re_state5am = if_else(!is.na(last_incident_time_re_state5am) & (time - last_incident_time_re_state5am <= 24)& (incident_window_re == 0), 1, 0),
           incident_window_re_state5am = as.integer(incident_window_re_state5am)) %>%
    
    mutate(last_incident_time_re_state10am = ifelse(state_incident10am == 1, time, NA)) %>% # specify the last incident time
    fill(last_incident_time_re_state10am, .direction = "down") %>%
    mutate(incident_window_re_state10am = if_else(!is.na(last_incident_time_re_state10am) & (time - last_incident_time_re_state10am <= 24)& (incident_window_re == 0), 1, 0),
           incident_window_re_state10am = as.integer(incident_window_re_state10am)) %>%
    
    mutate(last_incident_time_re_state5vam = ifelse(state_incident5vam == 1, time, NA)) %>% # specify the last incident time
    fill(last_incident_time_re_state5vam, .direction = "down") %>%
    mutate(incident_window_re_state5vam = if_else(!is.na(last_incident_time_re_state5vam) & (time - last_incident_time_re_state5vam <= 24)& (incident_window_re == 0), 1, 0),
           incident_window_re_state5vam = as.integer(incident_window_re_state5vam)) %>%
    
    mutate(last_incident_time_re_state10vam = ifelse(state_incident10vam == 1, time, NA)) %>% # specify the last incident time
    fill(last_incident_time_re_state10vam, .direction = "down") %>%
    mutate(incident_window_re_state10vam = if_else(!is.na(last_incident_time_re_state10vam) & (time - last_incident_time_re_state10vam <= 24)& (incident_window_re == 0), 1, 0),
           incident_window_re_state10vam = as.integer(incident_window_re_state10vam)) %>%
    
    mutate(last_incident_time_re_state1_4 = ifelse(state_incident1_4 == 1, time, NA)) %>% # specify the last incident time
    fill(last_incident_time_re_state1_4, .direction = "down") %>%
    mutate(incident_window_re_state1_4 = if_else(!is.na(last_incident_time_re_state1_4) & (time - last_incident_time_re_state1_4 <= 24)& (incident_window_re == 0), 1, 0),
           incident_window_re_state1_4 = as.integer(incident_window_re_state1_4)) %>%
    
    mutate(last_incident_time_re_state1_4v = ifelse(state_incident1_4v == 1, time, NA)) %>% # specify the last incident time
    fill(last_incident_time_re_state1_4v, .direction = "down") %>%
    mutate(incident_window_re_state1_4v = if_else(!is.na(last_incident_time_re_state1_4v) & (time - last_incident_time_re_state1_4v <= 24)& (incident_window_re == 0), 1, 0),
           incident_window_re_state1_4v = as.integer(incident_window_re_state1_4v)) %>%
    
    
    mutate(last_incident_time_re_state5_9 = ifelse(state_incident5_9 == 1, time, NA)) %>% # specify the last incident time
    fill(last_incident_time_re_state5_9, .direction = "down") %>%
    mutate(incident_window_re_state5_9 = if_else(!is.na(last_incident_time_re_state5_9) & (time - last_incident_time_re_state5_9 <= 24)& (incident_window_re == 0), 1, 0),
           incident_window_re_state5_9 = as.integer(incident_window_re_state5_9)) %>%
    
    mutate(last_incident_time_re_state5_9v = ifelse(state_incident5_9v == 1, time, NA)) %>% # specify the last incident time
    fill(last_incident_time_re_state5_9v, .direction = "down") %>%
    mutate(incident_window_re_state5_9v = if_else(!is.na(last_incident_time_re_state5_9v) & (time - last_incident_time_re_state5_9v <= 24)& (incident_window_re == 0), 1, 0),
           incident_window_re_state5_9v = as.integer(incident_window_re_state5_9v)) %>%
    
    ungroup() %>% 
    filter(year>=year_threshold) %>% 
    
    # second-threshold of voting margin for districts
    filter(abs(pct_RDmargin_before)<=voting_margin_second) 
  return(df)
}



## 2.2 Non_HoR -------------------------------------------------------------

data_treatment_name_nonHoR <- function(df, start_year=2000){
  df <- as.data.frame(df)  %>%
    mutate(incident_window_re = incident_window,
           pro_don_number_raw=pro_don_number,
           pro_log_sum_raw=pro_log_sum,
           anti_don_number_raw=anti_don_number,
           anti_log_sum_raw=anti_log_sum,,
           anti_sum_donations_raw=anti_sum_donations,
           pro_sum_donations_raw=pro_sum_donations) %>%  # change the name for consistency with other functions and datasets
    filter(year>=start_year) 
  return(df)
}

data_baseline_nonHoR <- function(df # No voting_margin
){
  df <- as.data.frame(df) %>%
    mutate(incident = ifelse(fatalities>0, 1, 0)) %>% 
    mutate(pro_don_number_raw=pro_don_number,
           pro_log_sum_raw=pro_log_sum,
           anti_don_number_raw=anti_don_number,
           anti_log_sum_raw=anti_log_sum,,
           anti_sum_donations_raw=anti_sum_donations,
           pro_sum_donations_raw=pro_sum_donations,
           
           pro_don_number = ifelse(pro_pac_donations_dollar>=10000, pro_don_number, 0),
           pro_log_sum = ifelse(pro_pac_donations_dollar>=10000, pro_log_sum, 0),
           anti_don_number = ifelse(anti_pac_donations_dollar>=10000, anti_don_number, 0),
           anti_log_sum = ifelse(anti_pac_donations_dollar>=10000, anti_log_sum, 0),
           pro_sum_donations = ifelse(pro_pac_donations_dollar>=10000, pro_sum_donations_raw, 0),
           anti_sum_donations = ifelse(anti_pac_donations_dollar>=10000, anti_sum_donations_raw, 0)) %>% 
    
    arrange(id, time) %>%
    group_by(id) %>%
    mutate(last_incident_time_re = ifelse(incident == 1, time, NA)) %>% # specify the last incident time
    fill(last_incident_time_re, .direction = "down") %>%
    mutate(incident_window_re = if_else(!is.na(last_incident_time_re) & (time - last_incident_time_re <= 24), 1, 0),
           incident_window_re = as.integer(incident_window_re)
    ) %>%
    ungroup() %>% 
    filter(year>=2000)
  return(df)
}

data_restriction_nonHoR <- function(df, fatality_threshold=1, # It cannot be zero
                                 minimum_dol=10000, year_threshold=2000, 
                                 month_scope=24){
  df <- as.data.frame(df) %>%
    mutate(incident = ifelse(fatalities>=fatality_threshold, 1, 0)) %>% 
    mutate(pro_don_number_raw=pro_don_number,
           pro_log_sum_raw=pro_log_sum,
           anti_don_number_raw=anti_don_number,
           anti_log_sum_raw=anti_log_sum,,
           anti_sum_donations_raw=anti_sum_donations,
           pro_sum_donations_raw=pro_sum_donations,
           
           pro_don_number = ifelse(pro_pac_donations_dollar>=minimum_dol, pro_don_number, 0),
           pro_log_sum = ifelse(pro_pac_donations_dollar>=minimum_dol, pro_log_sum, 0),
           anti_don_number = ifelse(anti_pac_donations_dollar>=minimum_dol, anti_don_number, 0),
           anti_log_sum = ifelse(anti_pac_donations_dollar>=minimum_dol, anti_log_sum, 0),
           pro_sum_donations = ifelse(pro_pac_donations_dollar>=10000, pro_sum_donations_raw, 0),
           anti_sum_donations = ifelse(anti_pac_donations_dollar>=10000, anti_sum_donations_raw, 0)) %>% 
    
    arrange(id, time) %>%
    group_by(id) %>%
    mutate(last_incident_time_re = ifelse(incident == 1, time, NA)) %>% # specify the last incident time
    fill(last_incident_time_re, .direction = "down") %>%
    mutate(incident_window_re = if_else(!is.na(last_incident_time_re) & (time - last_incident_time_re <= month_scope), 1, 0),
           incident_window_re = as.integer(incident_window_re)
    ) %>%
    ungroup() %>% 
    filter(year>=year_threshold)
  return(df)
}

# no net data for GVP
data_baseline_statenet <- function(df){
  df <- as.data.frame(df) %>%
    mutate(incident = ifelse(fatalities>0, 1, 0)) %>% # we have to re-define incident window and history!
    mutate(pro_don_number_raw=pro_don_number,
           pro_log_sum_raw=pro_log_sum,
           anti_don_number_raw=anti_don_number,
           anti_log_sum_raw=anti_log_sum,,
           anti_sum_donations_raw=anti_sum_donations,
           pro_sum_donations_raw=pro_sum_donations,
           
           pro_don_number_net = ifelse(pro_pac_donations_dollar_net>=10000, pro_don_number_net, 0),
           pro_log_sum_net = ifelse(pro_pac_donations_dollar_net>=10000, pro_log_sum_net, 0)) %>% 
           # no net for gun control side
           
    arrange(id, time) %>%
    group_by(id) %>%
    mutate(last_incident_time_re = ifelse(incident == 1, time, NA)) %>% # specify the last incident time
    fill(last_incident_time_re, .direction = "down") %>%
    mutate(incident_window_re = if_else(!is.na(last_incident_time_re) & (time - last_incident_time_re <= 24), 1, 0),
           incident_window_re = as.integer(incident_window_re)
    ) %>%
    ungroup() %>% 
    filter(year>=2000)
  return(df)
}


## 2.3 Timing to Elections -------------------------------------------------


# Used for timing analysis
# Note that this will filter out non-competitive districts

data_month_to <- function(df, # Note that we have to use non-filtered data here
                         treated_range=24, 
                         margin_threshold=5 # When you do not need to filter out anything, it should be 100.
                         ){
  
  df <- df %>%
    filter(abs(pct_RDmargin_before)<=margin_threshold) %>% 
    mutate(incident = ifelse(incident >= 1 & fatalities>=1, 1, 0)) %>% # we have to re-define incident window and history!
    
    mutate(incident2 = ifelse(incident >= 1 & months_to_election <= 2, 1, 0)) %>%
    mutate(incident3 = ifelse(incident >= 1 & months_to_election <= 3, 1, 0)) %>%
    mutate(incident4 = ifelse(incident >= 1 & months_to_election <= 4, 1, 0)) %>%
    mutate(incident6 = ifelse(incident >= 1 & months_to_election <= 6, 1, 0)) %>%
    mutate(incident8 = ifelse(incident >= 1 & months_to_election <= 8, 1, 0)) %>%
    mutate(incident12 = ifelse(incident >= 1 & months_to_election <= 12, 1, 0)) %>%
    
    mutate(pro_don_number_raw=pro_don_number,
           pro_log_sum_raw=pro_log_sum,
           anti_don_number_raw=anti_don_number,
           anti_log_sum_raw=anti_log_sum,,
           anti_sum_donations_raw=anti_sum_donations,
           pro_sum_donations_raw=pro_sum_donations,
           
           pro_don_number = ifelse(pro_pac_donations_dollar>=10000, pro_don_number, 0),
           pro_log_sum = ifelse(pro_pac_donations_dollar>=10000, pro_log_sum, 0),
           anti_don_number = ifelse(anti_pac_donations_dollar>=10000, anti_don_number, 0),
           anti_log_sum = ifelse(anti_pac_donations_dollar>=10000, anti_log_sum, 0),
           pro_sum_donations = ifelse(pro_pac_donations_dollar>=10000, pro_sum_donations_raw, 0),
           anti_sum_donations = ifelse(anti_pac_donations_dollar>=10000, anti_sum_donations_raw, 0)
    )
  
  
  df <- df %>% 
    arrange(id, time) %>%
    group_by(id) %>%
    
    # First, basic treatment var.
    mutate(last_incident_time_re = ifelse(incident == 1, time, NA)) %>% # specify the last incident time
    fill(last_incident_time_re, .direction = "down") %>%
    
    # treatment range can be changed from 24
    mutate(incident_window_re = if_else(!is.na(last_incident_time_re) & (time - last_incident_time_re <= treated_range), 1, 0),
           incident_window_re = as.integer(incident_window_re)) %>%
    
    # Next, months to election
    # Three patterns of treatment period???
    
    # First, treatment period equals to threshold
    
    mutate(last_incident_time_re_2 = ifelse(incident2 == 1, time, NA)) %>% # specify the last incident time
    fill(last_incident_time_re_2, .direction = "down") %>%
    mutate(incident_window_re_2 = if_else(!is.na(last_incident_time_re_2) & (time - last_incident_time_re_2 <= 2), 1, 0),
           incident_window_re_2 = as.integer(incident_window_re_2)) %>%
    
    mutate(last_incident_time_re_3 = ifelse(incident3 == 1, time, NA)) %>% # specify the last incident time
    fill(last_incident_time_re_3, .direction = "down") %>%
    mutate(incident_window_re_3 = if_else(!is.na(last_incident_time_re_3) & (time - last_incident_time_re_3 <= 3), 1, 0),
           incident_window_re_3 = as.integer(incident_window_re_3)) %>%
    
    mutate(last_incident_time_re_4 = ifelse(incident4 == 1, time, NA)) %>% # specify the last incident time
    fill(last_incident_time_re_4, .direction = "down") %>%
    mutate(incident_window_re_4 = if_else(!is.na(last_incident_time_re_4) & (time - last_incident_time_re_4 <= 4), 1, 0),
           incident_window_re_4 = as.integer(incident_window_re_4)) %>%
    
    mutate(last_incident_time_re_6 = ifelse(incident6 == 1, time, NA)) %>% # specify the last incident time
    fill(last_incident_time_re_6, .direction = "down") %>%
    mutate(incident_window_re_6 = if_else(!is.na(last_incident_time_re_6) & (time - last_incident_time_re_6 <= 6), 1, 0),
           incident_window_re_6 = as.integer(incident_window_re_6)) %>%
    
    
    mutate(last_incident_time_re_8 = ifelse(incident8 == 1, time, NA)) %>% # specify the last incident time
    fill(last_incident_time_re_8, .direction = "down") %>%
    mutate(incident_window_re_8 = if_else(!is.na(last_incident_time_re_8) & (time - last_incident_time_re_8 <= 8), 1, 0),
           incident_window_re_8 = as.integer(incident_window_re_8)) %>%
    
    
    mutate(last_incident_time_re_12 = ifelse(incident12 == 1, time, NA)) %>% # specify the last incident time
    fill(last_incident_time_re_12, .direction = "down") %>%
    mutate(incident_window_re_12 = if_else(!is.na(last_incident_time_re_12) & (time - last_incident_time_re_12 <= 12), 1, 0),
           incident_window_re_12 = as.integer(incident_window_re_12)) %>%
    
    #for reference treatment
    mutate(incident_window_re_2others= ifelse(incident_window_re==1 & incident_window_re_2==0, 1, 0),
           incident_window_re_3others= ifelse(incident_window_re==1 & incident_window_re_3==0, 1, 0),
           incident_window_re_4others= ifelse(incident_window_re==1 & incident_window_re_4==0, 1, 0),
           incident_window_re_6others= ifelse(incident_window_re==1 & incident_window_re_6==0, 1, 0),
           incident_window_re_8others= ifelse(incident_window_re==1 & incident_window_re_8==0, 1, 0),
           incident_window_re_12others= ifelse(incident_window_re==1 & incident_window_re_12==0, 1, 0)) %>% 
    
    
    # Second, cutoff is the next election
    mutate(last_incident_time_re_2election = ifelse(incident2 == 1, time, NA)) %>% # specify the last incident time
    fill(last_incident_time_re_2election, .direction = "down") %>%
    mutate(incident_window_re_2election = if_else(!is.na(last_incident_time_re_2election) & (time - last_incident_time_re_2election <= 2) & (months_to_election<=2), 1, 0),
           incident_window_re_2election = as.integer(incident_window_re_2election)) %>%
    
    mutate(last_incident_time_re_3election = ifelse(incident3 == 1, time, NA)) %>% # specify the last incident time
    fill(last_incident_time_re_3election, .direction = "down") %>%
    mutate(incident_window_re_3election = if_else(!is.na(last_incident_time_re_3election) & (time - last_incident_time_re_3election <= 3)& (months_to_election<=3), 1, 0),
           incident_window_re_3election = as.integer(incident_window_re_3election)) %>%
    
    mutate(last_incident_time_re_4election = ifelse(incident4 == 1, time, NA)) %>% # specify the last incident time
    fill(last_incident_time_re_4election, .direction = "down") %>%
    mutate(incident_window_re_4election = if_else(!is.na(last_incident_time_re_4election) & (time - last_incident_time_re_4election <= 4)& (months_to_election<=4), 1, 0),
           incident_window_re_4election = as.integer(incident_window_re_4election)) %>%
    
    mutate(last_incident_time_re_6election = ifelse(incident6 == 1, time, NA)) %>% # specify the last incident time
    fill(last_incident_time_re_6election, .direction = "down") %>%
    mutate(incident_window_re_6election = if_else(!is.na(last_incident_time_re_6election) & (time - last_incident_time_re_6election <= 6)& (months_to_election<=6), 1, 0),
           incident_window_re_6election = as.integer(incident_window_re_6election)) %>%
    
    
    mutate(last_incident_time_re_8election = ifelse(incident8 == 1, time, NA)) %>% # specify the last incident time
    fill(last_incident_time_re_8election, .direction = "down") %>%
    mutate(incident_window_re_8election = if_else(!is.na(last_incident_time_re_8election) & (time - last_incident_time_re_8election <= 8)& (months_to_election<=8), 1, 0),
           incident_window_re_8election = as.integer(incident_window_re_8election)) %>%
    
    
    mutate(last_incident_time_re_12election = ifelse(incident12 == 1, time, NA)) %>% # specify the last incident time
    fill(last_incident_time_re_12election, .direction = "down") %>%
    mutate(incident_window_re_12election = if_else(!is.na(last_incident_time_re_12election) & (time - last_incident_time_re_12election <= 12)& (months_to_election<=12), 1, 0),
           incident_window_re_12election = as.integer(incident_window_re_12election)) %>%
    
    #for reference treatment
    mutate(incident_window_re_2election_others= ifelse(incident_window_re==1 & incident_window_re_2election==0, 1, 0),
           incident_window_re_3election_others= ifelse(incident_window_re==1 & incident_window_re_3election==0, 1, 0),
           incident_window_re_4election_others= ifelse(incident_window_re==1 & incident_window_re_4election==0, 1, 0),
           incident_window_re_6election_others= ifelse(incident_window_re==1 & incident_window_re_6election==0, 1, 0),
           incident_window_re_8election_others= ifelse(incident_window_re==1 & incident_window_re_8election==0, 1, 0),
           incident_window_re_12election_others= ifelse(incident_window_re==1 & incident_window_re_12election==0, 1, 0)) %>% 
    
    # Lastly, 24 months period
    mutate(last_incident_time_re_2whole = ifelse(incident2 == 1, time, NA)) %>% # specify the last incident time
    fill(last_incident_time_re_2whole, .direction = "down") %>%
    mutate(incident_window_re_2whole = if_else(!is.na(last_incident_time_re_2whole) & (time - last_incident_time_re_2whole <= 24) , 1, 0),
           incident_window_re_2whole = as.integer(incident_window_re_2whole)) %>%
    
    mutate(last_incident_time_re_3whole = ifelse(incident3 == 1, time, NA)) %>% # specify the last incident time
    fill(last_incident_time_re_3whole, .direction = "down") %>%
    mutate(incident_window_re_3whole = if_else(!is.na(last_incident_time_re_3whole) & (time - last_incident_time_re_3whole <= 24), 1, 0),
           incident_window_re_3whole = as.integer(incident_window_re_3whole)) %>%
    
    mutate(last_incident_time_re_4whole = ifelse(incident4 == 1, time, NA)) %>% # specify the last incident time
    fill(last_incident_time_re_4whole, .direction = "down") %>%
    mutate(incident_window_re_4whole = if_else(!is.na(last_incident_time_re_4whole) & (time - last_incident_time_re_4whole <= 24), 1, 0),
           incident_window_re_4whole = as.integer(incident_window_re_4whole)) %>%
    
    mutate(last_incident_time_re_6whole = ifelse(incident6 == 1, time, NA)) %>% # specify the last incident time
    fill(last_incident_time_re_6whole, .direction = "down") %>%
    mutate(incident_window_re_6whole = if_else(!is.na(last_incident_time_re_6whole) & (time - last_incident_time_re_6whole <= 24), 1, 0),
           incident_window_re_6whole = as.integer(incident_window_re_6whole)) %>%
    
    
    mutate(last_incident_time_re_8whole = ifelse(incident8 == 1, time, NA)) %>% # specify the last incident time
    fill(last_incident_time_re_8whole, .direction = "down") %>%
    mutate(incident_window_re_8whole = if_else(!is.na(last_incident_time_re_8whole) & (time - last_incident_time_re_8whole <= 24), 1, 0),
           incident_window_re_8whole = as.integer(incident_window_re_8whole)) %>%
    
    
    mutate(last_incident_time_re_12whole = ifelse(incident12 == 1, time, NA)) %>% # specify the last incident time
    fill(last_incident_time_re_12whole, .direction = "down") %>%
    mutate(incident_window_re_12whole = if_else(!is.na(last_incident_time_re_12whole) & (time - last_incident_time_re_12whole <= 24), 1, 0),
           incident_window_re_12whole = as.integer(incident_window_re_12whole)) %>%
    
    #for reference treatment
    mutate(incident_window_re_2whole_others= ifelse(incident_window_re==1 & incident_window_re_2whole==0, 1, 0),
           incident_window_re_3whole_others= ifelse(incident_window_re==1 & incident_window_re_3whole==0, 1, 0),
           incident_window_re_4whole_others= ifelse(incident_window_re==1 & incident_window_re_4whole==0, 1, 0),
           incident_window_re_6whole_others= ifelse(incident_window_re==1 & incident_window_re_6whole==0, 1, 0),
           incident_window_re_8whole_others= ifelse(incident_window_re==1 & incident_window_re_8whole==0, 1, 0),
           incident_window_re_12whole_others= ifelse(incident_window_re==1 & incident_window_re_12whole==0, 1, 0)) %>% 
    
    ungroup() %>% 
    filter(year>=2000)
  return(df)
}


## 2.4 Misc. ---------------------------------------------------------------

save_processed_dataset <- function(data, filename) {
  output_dir <- here(
    # "gunPAC",
    "PNAS_data", "processed_data")
  
  # RDS
  rds_filepath <- here(output_dir, paste0(filename, ".rds"))
  saveRDS(data, rds_filepath)
  
  # csv
  csv_filepath <- here(output_dir, paste0(filename, ".csv"))
  write.csv(data, csv_filepath, row.names = FALSE)
}


skip_data_wrangling <- function(skip = TRUE){
  
  if (skip == TRUE){
    processed_dir <- here(
      # "gunPAC",
      "PNAS_data", "processed_data")
    rds_files <- list.files(processed_dir, pattern = "\\.rds$")
    csv_files <- list.files(processed_dir, pattern = "\\.csv$")
    
    for (file in rds_files) {
      rds_var_name <- tools::file_path_sans_ext(file)
      assign(rds_var_name, readRDS(here(processed_dir, file)), envir = .GlobalEnv)
    }
    
    for (file in csv_files) {
      csv_var_name <- tools::file_path_sans_ext(file)
      assign(csv_var_name, read.csv(here(processed_dir, file)), envir = .GlobalEnv)
    }
    
  }
}


# 3. Descriptive Stats ----------------------------------------------------



# for top-right part of the scatter part
cor_fun_colored <- function(data, mapping, ...) {
  x   <- GGally::eval_data_col(data, mapping$x)
  y   <- GGally::eval_data_col(data, mapping$y)
  grp <- GGally::eval_data_col(data, mapping$colour)
  
  # overall pooled correlation
  pooled_corr <- stats::cor(x, y, use = "complete.obs")
  
  # per-group correlations
  cors <- tapply(seq_along(grp), grp, function(idx) {
    stats::cor(x[idx], y[idx], use = "complete.obs")
  })
  
  # Combine into a single data frame
  labels <- c("Corr", names(cors))
  values <- c(pooled_corr, as.numeric(cors))
  colors <- c("black", "Control", "Treated")
  
  # evenly spaced y positions from top to bottom
  y_positions <- seq(0.8, 0.2, length.out = length(labels))
  
  df <- data.frame(
    grp  = labels,
    corr = values,
    lx   = 0.5,
    ly   = y_positions,
    col  = colors
  )
  
  ggplot(df, aes(x = lx, y = ly,
                 label = paste0(grp, ": ", sprintf("%.3f", corr)),
                 colour = col, fontface = "plain")) +
    geom_text(size = 8, lineheight = 0.9) +
    scale_colour_manual(values = c("Treated" = "lightblue",
                                   "Control" = "lightcoral",
                                   "black"   = "black"), guide = "none") +
    xlim(0, 1) + ylim(0, 1) +
    theme_void()
}


violin_balance_plot_race <- function(df, title="Covariate Balance",
                                     subtitle = "Treatment vs Control Group", linewidth=0.7 
                                     ) {
  df_long <- df %>%
    # First, limiting to race info
    dplyr::select(incident_window_re,
                  white, black, Asian, otherrace) %>%
    pivot_longer(
      cols = -incident_window_re,
      names_to = "variable",
      values_to = "value"
    ) %>%
    mutate(variable = factor(variable, 
                             levels = c("white", "black", "Asian", "otherrace"),
                             labels = c("White (%)", "Black (%)", "Asian (%)", "Other races (%)")),
          incident_window_re = factor(incident_window_re, 
                                       levels = c(0, 1), 
                                       labels = c("Control", "Treated")))
  
  plot <- ggplot(df_long, aes(x = incident_window_re, y = value, fill = incident_window_re)) +
    geom_violin(alpha = 0.6, linewidth = linewidth ) +
    facet_wrap(~ variable, scales = "free_y", ncol = 2) +
    scale_fill_manual(
      name = "District Group",
      values = c("Control" = "lightcoral", "Treated" = "lightblue")) +
      # values = c("Control" = "lightcoral", "Treatment" = "lightblue")) + # different color pattern
    labs(
      # title = paste("Covariate Balance:", title_char),
      title=title,
      subtitle = subtitle,
      caption = "N.B.: Circle dots represent mean values for each group."
    ) +
    theme_minimal() +
    theme(plot.title = element_text(face = "bold", hjust = 0.5, size = 12),
          plot.subtitle= element_text(hjust = 0.5, size = 10),
          plot.caption=element_text(size = 12))+
    stat_summary(fun = "mean",geom = "point",shape = 19, # 23 is diamond
                 size = 2, 
                 color = "black"
                 ,  fill = "black" # if you want to use another color
    )+
    stat_summary(
      fun = function(x) mean(x, na.rm = TRUE),
      geom = "text",
      aes(label = sprintf("%.1f%%", after_stat(y))),  #  ..y.. is not recommended yet.
      vjust = -1,
      hjust = -0.3
    )+
    theme(
      legend.position = "bottom",
      legend.text = element_text(size = 13),
      legend.title = element_text(face = "bold", size = 13),
      axis.text.x = element_text(size = 12),
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      strip.background = element_rect(fill = "white"),
      strip.text = element_text(face = "bold", size = 14)
    )
  
  return(plot)
}


violin_balance_plot_nonrace <- function(df, var, # choose 1 or 2
                                        title = "Covariate Balance",
                                        subtitle = "Treatment vs Control Group", linewidth=0.7
) {
  
  if (var==1){
    df <- df %>%
      dplyr::select(
        incident_window_re,unemployment, log_income) 
    # bachelor,gun_ratio_pct, pct_RDmargin_before
  } else if (var==2){
    df <- df %>%
      dplyr::select(
        incident_window_re, 
        bachelor,gun_ratio_pct, pct_RDmargin_before)
    
  } 
  
  df_long <- df %>% 
    pivot_longer(cols = -incident_window_re,
                 names_to = "variable",
                 values_to = "value"
    )
  
  if (var==1){
    df_long <- df_long %>% 
      mutate(variable = factor(variable,
                               levels = c("unemployment", "log_income"),
                               labels = c("Unemployment (%)","Log Median Income")
      ))
    
  } else if (var==2){
    df_long <- df_long %>% 
      mutate(variable = factor(variable,
                               levels = c("bachelor","gun_ratio_pct","pct_RDmargin_before"),
                               labels = c("Bachelor's Degree (%)",  "Estimated Firearm Possesion (%)",
                                          "Rep. Margin (%)")))
  }
  
  
  df_long$incident_window_re <- factor(df_long$incident_window_re, 
                                       levels = c(0, 1), 
                                       labels = c("Control", "Treatment"))
  
  plot <- ggplot(df_long, aes(x = incident_window_re, y = value, fill = incident_window_re)) +
    geom_violin(alpha = 0.7, linewidth=linewidth) +
    facet_wrap(~ variable, scales = "free_y", ncol = 2) +
    scale_fill_manual(
      name = "District Group",
      values = c("Control" = "lightcoral", "Treatment" = "lightblue")) +
    labs(
      # title = paste("Covariate Balance:", title_char),
      title = title, subtitle = subtitle,
      # subtitle = "Treatment vs Control Group"
      caption = " N.B.: Circle dots represent mean values for each group."
    ) +
    theme_minimal() +
    theme(plot.title = element_text(face = "bold", hjust = 0.5, size = 12),
          plot.subtitle= element_text(hjust = 0.5, size = 10),
          plot.caption=element_text(size = 12))+
    stat_summary(fun = "mean",geom = "point",shape = 19,size = 2, 
                 color = "black"
                 ,  fill = "black" # if you want to use another color
    )+
    stat_summary(
      fun = function(x) mean(x, na.rm = TRUE),
      geom = "text",
      aes(label = sprintf("%.1f", after_stat(y))), 
      vjust = -1
      # hjust = -0.3 # maybe, not needed here
    )+
    theme(
      legend.position = "bottom",
      legend.text = element_text(size = 13),
      legend.title = element_text(face = "bold", size = 13),
      axis.text.x = element_text(size = 12),
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      strip.background = element_rect(fill = "white"),
      strip.text = element_text(face = "bold", size = 14)
    )
  
  return(plot)
}


desc_table_2yr <- function(df, title){
  df <- as.data.frame(df) %>%
    dplyr::select(incident_window_re, bachelor,
                  white, black, Asian, otherrace,  
                  unemployment, log_income, gun_ratio_pct, pct_RDmargin_before, 
                  rep_incumbent_before
                  # , president_rep
                  ) %>%
    mutate(rep_incumbent_before=factor(rep_incumbent_before, levels = c(0,1),
                                       labels=c("No", "Yes"))
           # president_rep=factor(president_rep,  levels = c(0,1),
           #                      labels=c("Democrat", "Republican"))
           ) %>% 
    
    tbl_summary(  
      by = incident_window_re,  # Compare treatment vs control groups
      statistic = list(all_continuous() ~ "{mean} ({sd})", 
                       rep_incumbent_before ~ "{n} ({p}%)"
                       # president_rep ~ "{n} ({p}%)"
                       ), 
      type = list(rep_incumbent_before ~ "categorical"
                  # president_rep ~ "categorical"
                  ),
      
      # missing = "always", # if you like
      missing = "ifany",
      
      # adjust displayed digit
      digits = list(
        all_continuous()     ~ c(1, 1),  # first decimal for mean and sd
        rep_incumbent_before ~ 0       # integer for categorical variables
        # president_rep        ~ 0
      ),
      
      # value = list(
      #   rep_incumbent_before ~ c("0" = "No", "1" = "Yes"),
      #   president_rep ~ c("0" = "Democratic", "1" = "Republican")
      # ),
      
      label = list(
        bachelor ~ "Ratio of Bachelor's Degree Holders (%)",
        white ~ "White Population (%)",
        black ~ "Black Population (%)",
        Asian ~ "Asian Population (%)",
        otherrace ~ "Other Races' Population (%)",
        unemployment ~ "Unemployment rate (%)",
        log_income ~ "Median Annual Income (Log $)",
        gun_ratio_pct ~"Estimated Household Firearm Possesion (%)",
        pct_RDmargin_before ~ "Margin for Rep. Candidates at the Latest Election (%)",
        # rep_incumbent_before ~ "Republican Incumbency (1 = Yes, 0 = No)" ,
        # president_rep ~ "US Presidency (1 = Rep., 0 = Dem.)"
        rep_incumbent_before ~ "Republican Incumbency"
        # president_rep ~ "US Presidency"
      )
    ) %>%
    
    modify_header(label = "**Variable**"
    ) %>% 
    
    # add spanning header
    modify_spanning_header(c("stat_1", "stat_2") ~ "**Shooting Incident (0 = Control, 1 = Treated)**") %>%
    bold_labels() %>% 
    
    modify_caption(paste0("**Descriptive Statistics Table: ", title, "**")) %>%
    
    modify_footnote(everything() ~ NA) %>% # remove the existing footnote
    modify_source_note(
      "Values for continuous variables are presented as 'mean (standard deviation).'",
      text_interpret = "md"  # md is markdown
    ) %>%  
    modify_table_styling(columns = everything(), align = "center")
  
  return(df)
}


panelview_function <- function(df, outcome="anti_log_sum", # the main focus is on treatment
                               treatment="incident_window_re",title, 
                               show=NULL, # when showing examples, specify, like, c(1:50)
                               competitive=TRUE # Different legend
){
  formula_str <- paste(outcome, "~",  treatment) # you can add cov to check its NA value
  
  if (competitive==TRUE){
    plot <- panelview(as.formula(formula_str), 
                      data = df, index = c("id","time"),xlab = "Month", ylab = "District",
                      # axis.lab.gap = 5, 
                      background = "white", 
                      color = c("lightcoral", "lightblue", "gray40"),
                      legend.labs = c("Control", "Treated", "Filtered Out (Non-competitive districts)"), 
                      main=title, axis.lab="off", show.id = show, cex.main = 8, 
                      gridOff = TRUE  # automatically true when dealing with large size data
                      # to maintain consistency between full and baseline
    )
  } else if (competitive == FALSE){
    plot <- panelview(as.formula(formula_str), 
                      data = df, index = c("id","time"),xlab = "Month", ylab = "District",
                      # axis.lab.gap = 5, 
                      background = "white", 
                      color = c("lightcoral", "lightblue", "gray40"),
                      legend.labs = c("Control", "Treated", "Filtered Out"), # using different legend
                      main=title, axis.lab="off", show.id = show, cex.main = 8, 
                      gridOff = TRUE  # automatically true when dealing with large size data
                      # to maintain consistency between full and baseline
    )
  }
  
  return(plot)
}


log10_sum_plot <- function(df, title="Distribution of Log Dollar Amount of Monthly Contributions",
                           subtitle,
                           pattern="anti", # or "pro"
                           filtered=TRUE, # baseline filtering or not
                           change_color=FALSE # for main body
) {
  
  if(pattern == "anti" & filtered==FALSE){
    outcome = df$anti_sum_donations_raw # logged later
  } else if (pattern == "pro" & filtered==FALSE){
    outcome = df$pro_sum_donations_raw
  } else if(pattern == "anti" & filtered==TRUE){
    outcome = df$anti_sum_donations # logged later
  } else if (pattern == "pro" & filtered==TRUE){
    outcome = df$pro_sum_donations
  }
  
  log10_1p <- function(x) ifelse(is.finite(x) & x > 0, log10(x), 0)
  
  df_trimmed <-df %>%  
    mutate(log10_sum_positive = log10_1p(outcome)) %>% 
    filter(log10_sum_positive>0) # only focusing on positive-value contributions, not including 0.
  
  mean_val <- mean(df_trimmed$log10_sum_positive, na.rm = TRUE)
  # median_val <- median(df_trimmed$log10_sum_positive, na.rm = TRUE) # not reflected here
  
  if (change_color==TRUE){
    plot <- ggplot(df_trimmed, aes(x = log10_sum_positive)) +
      geom_histogram(
        binwidth = log10(10000) - log10(5000), boundary= log10(5000), fill = "salmon", color = "black", alpha = 0.7) 
    
  } else if (change_color==FALSE){
    plot <- ggplot(df_trimmed, aes(x = log10_sum_positive)) +
      geom_histogram(
        binwidth = log10(10000) - log10(5000), boundary= log10(5000), fill = "turquoise", color = "black", alpha = 0.7)   
  }
  
  plot <- plot+
    geom_vline(xintercept = mean_val, color = "darkgreen", linetype = "dashed", linewidth = 1) +
    # geom_vline(xintercept = median_val, color = "darkgreen", linetype = "dashed", linewidth = 1) +
    
    geom_vline(xintercept = log10(5000), color = "blue", linetype = "dotted", linewidth = 1) +
    geom_vline(xintercept = log10(10000), color = "red", linetype = "dotted", linewidth = 1) +
    
    annotate("text", x = mean_val, y = 0, 
             label = sprintf("Mean: %.1f", mean_val), 
             color = "darkgreen", vjust = -12, hjust = -0.1, fontface = "bold", size=5) +
    # annotate("text", x = median_val, y = 0, 
    #          label = sprintf("Median: %.1f", median_val), 
    #          color = "darkgreen", vjust = -4.5, hjust = -0.32, fontface = "bold") +
    
    annotate("text", x = log10(5000), y = 0, 
             label = "$5,000", 
             color = "blue", vjust = -10, hjust = -0.2, fontface = "bold", size=5) +
    annotate("text", x = log10(10000), y = 0, 
             label = "$10,000", 
             color = "red", vjust = -8, hjust = -0.2, fontface = "bold", size=5) +
    
    scale_x_continuous(
      breaks = seq(0, 7, by = 1)
      # second x axis will not be needed, since it is self-evident
      # sec.axis = sec_axis(
      #   ~ .,
      #   breaks = c(log10(5000), log10(10000)),
      #   labels = c("$5,000", "$10,000"))
    ) +
    labs(
      title = title,
      subtitle = subtitle,
      x = "Logged Amount (Base 10)",
      y = "Frequency") +
    theme_minimal() +
    theme(
      plot.title = element_text(face = "bold", hjust = 0.5, size = 12),
      plot.subtitle = element_text(hjust = 0.5, size = 10),
      # axis.title = element_text(face = "bold"),
      panel.grid.minor = element_blank()
    )
  return(plot)
}


log10_sum_density_both <- function(df, title="Density Plot of Log Dollar Amount of Contributions",
                                   subtitle, mean=TRUE, # display mean or not
                                   filtered=TRUE
                                   ) {
  
  log10_1p <- function(x) ifelse(is.finite(x) & x > 0, log10(x), 0)
  
  # Prepare distinct datasets for anti and pro gun lobbies
  if (filtered==TRUE){
    df_anti <- df %>%  
      mutate(log10_sum_positive = log10_1p(anti_sum_donations),
             type = "Anti-gun") %>% 
      filter(log10_sum_positive > 0)
    
    df_pro <- df %>%  
      mutate(log10_sum_positive = log10_1p(pro_sum_donations),
             type = "Pro-gun") %>% 
      filter(log10_sum_positive > 0)
  } else if (filtered==FALSE){
    df_anti <- df %>%  
      mutate(log10_sum_positive = log10_1p(anti_sum_donations_raw), # different unfiltered variable
             type = "Anti-gun") %>% 
      filter(log10_sum_positive > 0)
    
    df_pro <- df %>%  
      mutate(log10_sum_positive = log10_1p(pro_sum_donations_raw),
             type = "Pro-gun") %>% 
      filter(log10_sum_positive > 0)
  }
  
 
  
  df_combined <- bind_rows(df_anti, df_pro) # containing two types for the same variable
  
  # Calculate means
  mean_anti <- mean(df_anti$log10_sum_positive, na.rm = TRUE)
  mean_anti_nonlog <- 10^mean_anti
  mean_pro <- mean(df_pro$log10_sum_positive, na.rm = TRUE)
  mean_pro_nonlog <- 10^mean_pro
  
  plot <- ggplot(df_combined, aes(x = log10_sum_positive, fill = type, color = type)) +
    geom_density(alpha = 0.7, linewidth = 0.7) +
    
    scale_fill_manual(values = c("Anti-gun" = "turquoise", "Pro-gun" = "salmon")) + # content
    scale_color_manual(values = c("Anti-gun" = "gray10", "Pro-gun" = "gray10"))  # edge
    
  
    # Mean lines
  if (mean==TRUE){
    plot <- plot+
      geom_vline(xintercept = mean_anti, color = "turquoise", linetype = "dashed", linewidth = 0.75) +
      geom_vline(xintercept = mean_pro, color = "salmon", linetype = "dashed", linewidth = 0.75) +
      # Annotations
      annotate("text", x = mean_anti, y = 1.5, 
               label = sprintf("Mean (Anti-gun): %.1f", mean_anti_nonlog, " (USD)"), 
               color = "turquoise", vjust = -1, hjust = -0.1, fontface = "bold", size = 4) +
      annotate("text", x = mean_pro, y = 1.5, 
               label = sprintf("Mean (Pro-gun): %.1f", mean_pro_nonlog, " (USD)"), 
               color = "salmon", vjust = -2.5, hjust = -0.1, fontface = "bold", size = 4) 
  }
    
    plot <- plot+
    scale_x_continuous(breaks = seq(0, 8, by = 1)) +
    labs(
      title = title,
      subtitle = subtitle,
      x = "Logged Amount (Base 10)",
      y = "Density",
      fill = "PAC Type",
      color = "PAC Type") +
    theme_minimal() +
    theme(
      plot.title = element_text(face = "bold", hjust = 0.5, size = 12),
      plot.subtitle = element_text(hjust = 0.5, size = 10),
      panel.grid.minor = element_blank(),
      legend.position = "top"
    )
  return(plot)
}




anti_pro_geo_var_plot <- function(df, focus_var, title, subtitle) {
  df <- df %>%
    mutate(state_abbr = state) %>%
    group_by(state_abbr) %>%
    summarise(
      anti_state_log_sum = sum(anti_log_sum, na.rm = TRUE),
      pro_state_log_sum = sum(pro_log_sum, na.rm = TRUE),
      anti_state_sum = sum(anti_sum_donations, na.rm = TRUE),
      pro_state_sum = sum(pro_sum_donations, na.rm = TRUE),
      anti_state_don_number = sum(anti_don_number, na.rm = TRUE),
      pro_state_don_number = sum(pro_don_number, na.rm = TRUE),
      
      anti_state_log_sum_raw = sum(anti_log_sum_raw, na.rm = TRUE),
      pro_state_log_sum_raw = sum(pro_log_sum_raw, na.rm = TRUE),
      anti_state_sum_raw = sum(anti_sum_donations_raw, na.rm = TRUE),
      pro_state_sum_raw = sum(pro_sum_donations_raw, na.rm = TRUE),
      anti_state_don_number_raw = sum(anti_don_number_raw, na.rm = TRUE),
      pro_state_don_number_raw = sum(pro_don_number_raw, na.rm = TRUE),
      
      state_incident_count = sum(incident_count, na.rm = TRUE),
      # incident is a dummy variable, while incident_count is multiple-valued
      .groups = 'drop'
    )
  
  # get u.s. state info
  us_states <- map_data("state")
  
  # mapping state names and two-letter acronyms
  state_abbr <- data.frame(
    state_name = tolower(state.name),
    state_abbr = state.abb,
    stringsAsFactors = FALSE
  )
  
  # get center locations of each state and add abb
  state_centers <- data.frame(
    state_name = tolower(state.name),
    long = state.center$x,
    lat = state.center$y,
    stringsAsFactors = FALSE
  ) %>%
    left_join(state_abbr, by = "state_name")
  
  # merge data with center locations through abbr.
  data_map <- state_centers %>%
    left_join(df, by = "state_abbr")
  
  # Depending on outcome patterns
  if (focus_var == "anti_dollar") {
    size_var <- "anti_state_sum" # _raw is ok, but to make this function versatile for both unfiltered and 
    # filtered data, this filtered variable is used here.
    color_val <- "turquoise"
    legend_title <- "Gun Control PAC; Total Amount ($)"
    use_comma <- TRUE
    
  } else if (focus_var == "pro_dollar") {
    size_var <- "pro_state_sum"
    color_val <- "salmon"
    legend_title <- "Gun Lobby PAC; Total Amount ($)"
    use_comma <- TRUE
    
  } else if (focus_var == "anti_number") {
    size_var <- "anti_state_don_number"
    color_val <- "turquoise"
    legend_title <- "Gun Control PAC; Number Count"
    use_comma <- FALSE
    
  } else if (focus_var == "pro_number") {
    size_var <- "pro_state_don_number"
    color_val <- "salmon"
    legend_title <- "Gun Lobby PAC; Number Count"
    use_comma <- FALSE
    
  } else if (focus_var == "shooting"){
    size_var <- "state_incident_count"  #use count, instead of dummy variable
    color_val <- "limegreen"
    legend_title <- "School Shooting Occurrence"
    use_comma <- FALSE
  } else {
    stop("Invalid parameter.")
  }
  
  map_plot <- ggplot() +
    geom_polygon(data = us_states,
                 aes(x = long, y = lat, group = group),
                 fill = "white", color = "gray50") +
    # circle plot 
    geom_point(data = data_map,
               aes_string(x = "long", y = "lat", size = size_var), 
               color = color_val, alpha = 0.8) +
    # show abbr.
    geom_text(data = data_map,
              aes(x = long, y = lat, label = state_abbr),
              size = 2.5, vjust = 2.0) +
    coord_fixed(1.3) + # this ratio looks natural for US maps
    theme_minimal() +
    labs(title = title,
         subtitle = subtitle,
         x = "Long.", y = "Lat.") +
    theme(legend.position = "right",
          legend.title = element_text(size = 13),
          panel.grid = element_blank(),
          plot.title = element_text(face = "bold", size = 12, hjust = 0.5),
          plot.subtitle = element_text(size = 8, hjust = 0.5),
          axis.text = element_blank(),
          axis.title = element_blank(),
          axis.ticks = element_blank())
  
  # CONSOLIDATED SCALE LOGIC - Only ONE scale_size_continuous call
  
  if (focus_var %in% c("anti_dollar", "pro_dollar")) {
    # Calculate shared breaks for dollar amounts
    max_val <- max(c(df$anti_state_sum, df$pro_state_sum), na.rm = TRUE)
    min_val <- min(c(df$anti_state_sum[df$anti_state_sum>0], df$pro_state_sum[df$pro_state_sum>0]), na.rm = TRUE)
    
    # Create more breaks for a more informative legend by using scales package
    breaks <- scales::breaks_extended(n = 5)(c(min_val, max_val)) # n=5 means 3 (5-2) breaks within extremes.
    
    map_plot <- map_plot + scale_size_continuous(
      name = legend_title, 
      labels = scales::label_dollar(scale = 1/1000000, suffix = "M"), # using million as M.
      breaks = breaks,
      limits = c(min_val, max_val),
      range = c(0.5, 18)  # Increased upper range for more distinction
    ) +
      guides(size = guide_legend(override.aes = list(color = color_val, alpha = 0.8)))
    
  } else if (focus_var %in% "anti_number") {
    # Calculate shared breaks for numbers
    max_val <- max(df$anti_state_don_number, na.rm = TRUE)
    breaks <- pretty(c(1, max_val), n = 4)
    
    map_plot <- map_plot + scale_size_continuous(
      name = legend_title,
      breaks = breaks,
      limits = c(1, max_val),
      range = c(1, 10)
    )
  }  else if (focus_var %in% "pro_number") {
    # Calculate shared breaks for numbers
    max_val <- max(df$pro_state_don_number, na.rm = TRUE)
    breaks <- pretty(c(1, max_val), n = 4)
    
    map_plot <- map_plot + scale_size_continuous(
      name = legend_title,
      breaks = breaks,
      limits = c(1, max_val),
      range = c(1, 10)
    )
  } else {
    # For shooting incidents
    max_val <- max(df$state_incident_count, na.rm = TRUE)
    min_val <- min(df$state_incident_count[df$state_incident_count > 0], na.rm = TRUE)
    breaks <- pretty(c(min_val, max_val), n = 4)
    
    map_plot <- map_plot + scale_size_continuous(
      name = legend_title,
      breaks = breaks,
      limits = c(min_val, max_val),
      range = c(1, 10)
    )
  }
  
  return(map_plot)
}



anti_pro_combined_geo_plot <- function(df, title, subtitle, year="before",
                                       PAClegend=TRUE) {
  df <- df %>%
    mutate(state_abbr = state) %>%
    group_by(state_abbr) %>%
    summarise(
      anti_state_sum = sum(anti_sum_donations, na.rm = TRUE),
      pro_state_sum = sum(pro_sum_donations, na.rm = TRUE),
      anti_state_don_number = sum(anti_don_number, na.rm = TRUE),
      pro_state_don_number = sum(pro_don_number, na.rm = TRUE),
      
      anti_state_sum_raw = sum(anti_sum_donations_raw, na.rm = TRUE),
      pro_state_sum_raw = sum(pro_sum_donations_raw, na.rm = TRUE),
      anti_state_don_number_raw = sum(anti_don_number_raw, na.rm = TRUE),
      pro_state_don_number_raw = sum(pro_don_number_raw, na.rm = TRUE),
      .groups = 'drop'
    )
  
  # get u.s. state info
  us_states <- map_data("state")
  
  # mapping state names and two-letter acronyms
  state_abbr <- data.frame(
    state_name = tolower(state.name),
    state_abbr = state.abb,
    stringsAsFactors = FALSE
  )
  
  # get center locations of each state and add abb
  state_centers <- data.frame(
    state_name = tolower(state.name),
    long = state.center$x,
    lat = state.center$y,
    stringsAsFactors = FALSE
  ) %>%
    left_join(state_abbr, by = "state_name")
  
  # merge data with center locations through abbr.
  data_map <- state_centers %>%
    left_join(df, by = "state_abbr")
  
  # Create offset positions for side-by-side circles
  offset <- 1  # Adjust this to control horizontal separation
  # difference between two circles
  
  data_map_anti <- data_map %>%
    mutate(
      long_offset = long - offset, # change longtitudinal with offset
      circle_type = "Anti-gun",
      amount = anti_state_sum
    )
  
  data_map_pro <- data_map %>%
    mutate(
      long_offset = long + offset,
      circle_type = "Pro-gun", 
      amount = pro_state_sum
    )
  
  # Combine both datasets
  data_combined <- bind_rows(data_map_anti, data_map_pro)
  
  # Calculate shared scale limits
  max_val <- max(c(df$anti_state_sum, df$pro_state_sum), na.rm = TRUE)
  min_val <- min(c(df$anti_state_sum[df$anti_state_sum>0], df$pro_state_sum[df$pro_state_sum>0]), na.rm = TRUE)
  
  # Create more breaks for a more informative legend by using scales package
  breaks <- pretty(c(min_val, max_val), n = 4)
  # breaks <- scales::breaks_extended(n = 5)(c(min_val, max_val)) # n=5 means 3 (5-2) breaks within extremes.
  
  
  
  # max_val <- max(c(df$anti_state_sum, df$pro_state_sum), na.rm = TRUE)
  # min_val <- min(c(df$anti_state_sum[df$anti_state_sum > 0], 
  #                  df$pro_state_sum[df$pro_state_sum > 0]), na.rm = TRUE)
  
  map_plot <- ggplot() +
    geom_polygon(data = us_states,
                 aes(x = long, y = lat, group = group),
                 fill = "white", color = "gray50") +
    # Combined circles with color mapping
    geom_point(data = data_combined,
               aes(x = long_offset, y = lat, size = amount, color = circle_type),
               alpha = 0.7) +
    # State abbreviations in the center
    geom_text(data = data_map,
              aes(x = long, # this is center
                  y = lat, label = state_abbr),
              size = 2.8, vjust = 1.8) +
    # Manual color scale
    scale_color_manual(
      name = "PAC Type",
      values = c("Anti-gun" = "turquoise", "Pro-gun" = "salmon"),
      breaks = c("Anti-gun", "Pro-gun")  
    ) 
  
  # Size scale
  
  if (year=="before"){
    map_plot <- map_plot+
      # Size scale
      scale_size_continuous(
        name = "Dollar Amount; 2000-2017",
        # labels = scales::comma,
        labels = scales::label_dollar(scale = 1/1000000, suffix = "M"), # using million as M.
        breaks = breaks,
        limits = c(min_val, max_val),
        range = c(2, 10))  # Slightly smaller since we have two circles  
  } else if (year == "after"){
    map_plot <- map_plot+
      scale_size_continuous(
        name = "Dollar Amount; 2018-2024",
        # labels = scales::comma,
        labels = scales::label_dollar(scale = 1/1000000, suffix = "M"), # using million as M.
        breaks = breaks,
        limits = c(min_val, max_val),
        range = c(2, 10)) 
  }
  
  
  if (PAClegend==FALSE){
    map_plot <- map_plot +
      guides(color = "none")
  }
  
  map_plot <- map_plot+
    coord_fixed(1.3) +
    theme_minimal() +
    labs(title = title,
         subtitle = subtitle,
         x = "Long.", y = "Lat.") +
    theme(
      legend.position = "right",
      panel.grid = element_blank(),
      plot.title = element_text(face = "bold", size = 12, hjust = 0.5),
      plot.subtitle = element_text(size = 8, hjust = 0.5),
      axis.text = element_blank(),
      axis.title = element_blank(),
      axis.ticks = element_blank()
    )
  
  return(map_plot)
}





time_to_YMD <- function(time) {
  # time starts 1990/01
  year <- 1990 + floor((time - 1) / 12)
  month <- ((time - 1) %% 12) + 1
  # Just in case, added /DD info for placeholder is also added
  return(as.Date(paste0(year, "-", month, "-01")))
}


dollarcount_timeseries_nation_antipro <- function(df, title = "Nationwide Anti- v. Pro-gun Contributions",
                                             subtitle = "Annual Data", legend=TRUE,
                                             outcome="log_dollar" # or "abs_dollar" or "number"
                                               ) {
  
  # first, aggregate to nation-wide data
  
  if (outcome== "log_dollar"){
    df <- df %>%
      group_by(year) %>% 
      summarise(anti_national_sum = log1p(sum(anti_sum_donations_raw, na.rm = TRUE)),
                pro_national_sum = log1p(sum(pro_sum_donations_raw, na.rm = TRUE)))
    # no need to ungroup
    
    
  } else if (outcome == "abs_dollar"){
    df <- df %>%
      group_by(year) %>% 
      summarise(anti_national_sum = sum(anti_sum_donations_raw, na.rm = TRUE),
                pro_national_sum = sum(pro_sum_donations_raw, na.rm = TRUE))
    
  } else if (outcome == "number"){
    df <- df %>%
      group_by(year) %>% 
      summarise(anti_national_sum = sum(anti_don_number_raw, na.rm = TRUE),
                pro_national_sum = sum(pro_don_number_raw, na.rm = TRUE))
    
  }
  

  # to highlight HoR election years
  # not already defined; the previous one is within a function
  min_year <- min(df$year)
  max_year <- max(df$year)
  first_even_year <- ifelse(min_year %% 2 == 0, min_year, min_year + 1)
  even_years_seq <- data.frame(year = seq(first_even_year, max_year, by = 2))
  
  # plot
  # First, x-axis
  plot <- ggplot(df, aes(x = year)) +
    
    # for first y axis, pro-gun lobby
    # turquoise and salmon might not be conspicuous enough, so blue and red might be better
    geom_line(aes(y = pro_national_sum, color = "salmon"), linetype = "solid", linewidth=1.2) +
    geom_point(aes(y = pro_national_sum, color = "salmon")) +
    
    # To avoid too many elements, avoid smooth
    # geom_smooth(aes(y = national_sum), method = "loess", se = FALSE,
    #             color = "orange", linetype = "dashed") +
    
    # election years
    geom_vline(data = even_years_seq, aes(xintercept = year),
               color = "lightgray",  linewidth = 0.3, linetype="dashed") +
    scale_x_continuous(breaks = even_years_seq$year) +
    
    # Next, for anti-gun PAC 
    geom_line(data = df,  aes(y = anti_national_sum, color = "turquoise"),linetype = "solid",linewidth=1.2) +
    geom_point(data = df, aes(y = anti_national_sum, color = "turquoise")) +
    
    
    scale_color_manual(
      name = "PAC Type",  #title of legend
      values = c("salmon" = "salmon", "turquoise" = "turquoise"),  # assign colors
      labels = c("salmon" = "Pro-gun", "turquoise" = "Anti-gun")
    ) +
    
    theme_minimal() +
    labs(
      # title = "Nationwide Pro-gun Contributions",
      title=title,
      subtitle = subtitle,
      x = "Year (Dotted: HoR Election Years)"
    ) +
    theme(
      legend.position = "bottom",
      legend.title = element_text(face = "bold", size = 13),
      legend.text = element_text(size = 13),
      
      axis.title.y = element_text(color = "black", face = "bold", size = 13),
      axis.text.y = element_text(color = "black", size = 11),
      axis.title.x = element_text(margin = margin(t = 15)), 
      
      
      plot.title = element_text(face = "bold", size = 12, hjust = 0.5),
      plot.subtitle = element_text(hjust = 0.5, size= 10),
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank(),
      axis.text.x = element_text(angle = 45, hjust = 1) # tilting leads to better visual
    )
  
  if(legend==FALSE){
    plot <- plot+
      theme(legend.position = "none")
  }
  
  if (outcome == "log_dollar"){
    plot <- plot +
      scale_y_continuous(
        name = "Logged Dollar Amount")  # y axis depending on outcome patterns
    
  } else if (outcome == "abs_dollar"){
    plot <- plot+
      scale_y_continuous(
        name = "Dollar Amount",
        labels = scales::comma) 
    
  } else if (outcome == "number"){
    plot <- plot+
      scale_y_continuous(
        name = "Contribution Count",
        labels = scales::comma) 
    
  }
  
  
  return(plot)
}


monthly_timeseries_nation <- function(df, title = "Nationwide Anti- v. Pro-gun Contributions",
                                                  subtitle = "Monthly Data", legend=TRUE,
                                                  outcome="log_dollar" # or "abs_dollar" or "number"
) {
   
  df <- df %>%
    mutate(date = time_to_YMD(time)) %>%  #date is YYYY/MM/DD form
    group_by(date)
  
  if (outcome== "log_dollar"){
    df <- df %>% 
      # too many zeros, so focus on positive values
      summarise(anti_national_sum = log1p(sum(anti_sum_donations_raw, na.rm = TRUE)),
                pro_national_sum = log1p(sum(pro_sum_donations_raw, na.rm = TRUE)))
    # no need to ungroup
    
    
  } else if (outcome == "abs_dollar"){
    df <- df %>% 
      summarise(anti_national_sum = sum(anti_sum_donations_raw, na.rm = TRUE),
                pro_national_sum = sum(pro_sum_donations_raw, na.rm = TRUE))
    
  } else if (outcome == "number"){
    df <- df %>% 
      summarise(anti_national_sum = sum(anti_don_number_raw, na.rm = TRUE),
                pro_national_sum = sum(pro_don_number_raw, na.rm = TRUE))
    
  }
  
  # highlight HoR elections
  min_year <- year(min(df$date))
  max_year <- year(max(df$date))
  
  first_even_year <- ceiling(min_year / 2) * 2
  HoR_years_seq <- seq(min_year, max_year, by = 2)
  
  # Convert to dates 
  HoR_dates <- as.Date(paste0(HoR_years_seq, "-11-01")) # since they are monthly data 
  
  
  # if you want to highlight Presidential election years (every 4 years) in monthly plots, not every 2 years
  # min_year <- year(min(df$date))
  # max_year <- year(max(df$date))
  # first_pres_year <- ceiling(min_year / 4) * 4
  # pres_years_seq <- seq(min_year, max_year, by = 4)
  # pres_dates <- as.Date(paste0(pres_years_seq, "-11-01")) # since they are monthly data 
  
  # plot
  # First, x-axis
  plot <- ggplot(df, aes(x = date)) + # monthly
    
    # for first y axis, pro-gun lobby
    
    # for all values, including zero
    # geom_line(data = df,  aes(y = pro_national_sum, color = "salmon"),
    # linetype = "solid",linewidth=1.2) +
    
    # for positive values
    geom_line(data = df %>% filter(pro_national_sum > 0),  
              aes(y = pro_national_sum, color = "salmon"), 
              linetype = "solid", linewidth=1.2) +
    geom_point(aes(y = pro_national_sum, color = "salmon")) +
    
    # presidential election years
    geom_vline(xintercept = HoR_dates,
               color = "lightgray", linewidth = 0.3, linetype="dashed") +
    
    scale_x_date(
      date_labels = "%Y/%m", # YYYY/MM format
      date_breaks = "2 year" 
    ) +
    
    # Next, for anti-gun PAC 
    
    # for all values, including zero
    # geom_line(data = df,  aes(y = anti_national_sum, color = "turquoise"),
    # linetype = "solid",linewidth=1.2) +
    
    # for positive values
    geom_line(data = df %>% filter(anti_national_sum > 0),  
              aes(y = anti_national_sum, color = "turquoise"), 
              linetype = "solid", linewidth=1.2) +
    geom_point(data = df, aes(y = anti_national_sum, color = "turquoise")) +
    
    
    scale_color_manual(
      name = "PAC Type",  #title of legend
      values = c("salmon" = "salmon", "turquoise" = "turquoise"),  # assign colors
      labels = c("salmon" = "Pro-gun", "turquoise" = "Anti-gun")
    ) +
    
    theme_minimal() +
    labs(
      # title = "Nationwide Pro-gun Contributions",
      title=title,
      subtitle = subtitle,
      x = "YYYY/MM (Dotted: HoR Election Months)"
    ) +
    theme(
      legend.position = "bottom",
      legend.title = element_text(face = "bold", size = 13),
      legend.text = element_text(size = 13),
      
      axis.title.y = element_text(color = "black", face = "bold", size = 13, margin = margin(t = 15)),
      axis.text.y = element_text(color = "black", size = 11),
      axis.title.x = element_text(margin = margin(t = 15)), 

      plot.title = element_text(face = "bold", size = 12, hjust = 0.5),
      plot.subtitle = element_text(hjust = 0.5, size= 10),
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank(),
      axis.text.x = element_text(angle = 45, hjust = 1) # tilting leads to better visual
    )
  
  if(legend==FALSE){
    plot <- plot+
      theme(legend.position = "none")
  }
  
  if (outcome == "log_dollar"){
    plot <- plot +
      scale_y_continuous(
        name = "Logged Dollar Amount")  # y axis depending on outcome patterns
    
  } else if (outcome == "abs_dollar"){
    plot <- plot+
      scale_y_continuous(
        name = "Dollar Amount",
        labels = scales::comma) 
    
  } else if (outcome == "number"){
    plot <- plot+
      scale_y_continuous(
        name = "Contribution Count",
        labels = scales::comma) 
  }
  return(plot)
}


antipro_donation_incident_double_whole_plot <- function(df, outcome="log_dollar", # log_dollar or number or nonlog_dollar
                                                  title="School Shootings and Anti-gun Contributions",
                                                  subtitle, PACtype="anti" # or "pro"
) {
  
  df <- df %>%
    group_by(year) %>%
    summarise(anti_national_log_sum = log1p(sum(anti_sum_donations_raw, na.rm = TRUE)),
              anti_national_nonlog_sum = sum(anti_sum_donations_raw, na.rm = TRUE),
              anti_national_don_number = sum(anti_don_number_raw, na.rm = TRUE),
              pro_national_log_sum = log1p(sum(pro_sum_donations_raw, na.rm = TRUE)),
              pro_national_nonlog_sum = sum(pro_sum_donations_raw, na.rm = TRUE),
              pro_national_don_number = sum(pro_don_number_raw, na.rm = TRUE),
              national_incident_count = sum(incident_count, na.rm = TRUE)
    )
  
  if (outcome == "log_dollar" & PACtype=="anti"){
    df <- df %>% 
      mutate(national_sum_outcome=anti_national_log_sum)
  } else if (outcome == "nonlog_dollar"& PACtype=="anti"){
    df <- df %>% 
      mutate(national_sum_outcome=anti_national_nonlog_sum)
  } else if (outcome == "log_dollar" & PACtype=="pro"){
    df <- df %>% 
      mutate(national_sum_outcome=pro_national_log_sum)
  } else if (outcome == "nonlog_dollar"& PACtype=="pro"){
    df <- df %>% 
      mutate(national_sum_outcome=pro_national_nonlog_sum)
  }
  
  # outcome patterns
  if (outcome == "log_dollar" | outcome == "nonlog_dollar"){
    y_scale_factor <- max(df$national_sum_outcome) / max(df$national_incident_count)
    df$national_incident_count_revised <- df$national_incident_count * y_scale_factor
  } else if (outcome == "number"){
    y_scale_factor <- max(df$national_don_number) / max(df$national_incident_count)
    df$national_incident_count_revised <- df$national_incident_count * y_scale_factor
  } else {
    stop("Invalid outcome parameter.")
  }
  
  # too messy??
  min_year <- min(df$year)
  max_year <- max(df$year)
  first_even_year <- ifelse(min_year %% 2 == 0, min_year, min_year + 1)
  even_years_seq <- data.frame(year = seq(first_even_year, max_year, by = 2))
  
  # Create the plot
  plot <- ggplot(df, aes(x = year)) 
  
  # Outcome patterns
  # you cannot connect if and plus
  if (PACtype=="anti" & (outcome == "log_dollar" | outcome == "nonlog_dollar")){
    plot <- plot+
      geom_line(aes(y = national_sum_outcome, color = "turquoise"), linetype = "solid", linewidth = 1.2)+
      geom_point(aes(y = national_sum_outcome, color = "turquoise")) 
  } else if (PACtype=="anti" & outcome == "number"){
    plot <- plot+
      geom_line(aes(y = national_don_number, color = "turquoise"), linetype = "solid", linewidth = 1.2)+
      geom_point(aes(y = national_don_number, color = "turquoise")) 
  } else if (PACtype=="pro" & (outcome == "log_dollar" | outcome == "nonlog_dollar")){
    plot <- plot+
      geom_line(aes(y = national_sum_outcome, color = "salmon"), linetype = "solid", linewidth = 1.2)+
      geom_point(aes(y = national_sum_outcome, color = "salmon")) 
  } else if (PACtype=="pro" & outcome == "number"){
    plot <- plot+
      geom_line(aes(y = national_don_number, color = "salmon"), linetype = "solid", linewidth = 1.2)+
      geom_point(aes(y = national_don_number, color = "salmon")) 
  }
  
  
  plot <- plot+
    
    # Incident count line
    geom_line(aes(y = national_incident_count_revised, color = "limegreen"), linetype = "solid", linewidth = 1.2) +
    geom_point(aes(y = national_incident_count_revised, color = "limegreen")) +
    
    # election years are too messy?? 
    geom_vline(data = even_years_seq, aes(xintercept = year),
               color = "gray50", linetype = "dashed", linewidth = 0.6) +
    scale_x_continuous(breaks = even_years_seq$year) 
  
  # Dual Y-axis settings
  if (outcome == "log_dollar"){
    plot <- plot+
      scale_y_continuous(name = "Dollar Amount (logged)",
                         sec.axis = sec_axis(~ . / y_scale_factor, name = "Shooting Occurrence"))
  } else if (outcome == "nonlog_dollar"){
    plot <- plot+
      scale_y_continuous(name = "Dollar Amount (Non-logged)",
                         sec.axis = sec_axis(~ . / y_scale_factor, name = "Shooting Occurrence"))
  } else if (outcome == "number"){
    plot <- plot+
      scale_y_continuous(name = "Contribution Number Count",
                         sec.axis = sec_axis(~ . / y_scale_factor, name = "Shooting Occurrence"))
  } 
  
  
  # Color settings
  if (outcome == "log_dollar" & PACtype=="anti"){
    plot <- plot+
      scale_color_manual(name = "Legend",
                         values = c("turquoise" = "turquoise", "limegreen" = "limegreen"),
                         labels = c("turquoise" = "Logged Dollar Amount", "limegreen" = "Shooting Case Count")
      )
  } else if (outcome == "nonlog_dollar"& PACtype=="anti"){
    plot <- plot+
      scale_color_manual(name = "Legend",
                         values = c("turquoise" = "turquoise", "limegreen" = "limegreen"),
                         labels = c("turquoise" = "Dollar Amount (Non-log)", "limegreen" = "Shooting Case Count")
      )
  } else if (outcome == "number"& PACtype=="anti"){
    plot <- plot+
      scale_color_manual(name = "Legend",
                         values = c("turquoise" = "turquoise", "limegreen" = "limegreen"),
                         labels = c("turquoise" = "Contribution Number Count", "limegreen" = "Shooting Case Count")
      ) 
  } else if (outcome == "log_dollar" & PACtype=="pro"){
    plot <- plot+
      scale_color_manual(name = "Legend",
                         values = c("salmon" = "salmon", "limegreen" = "limegreen"),
                         labels = c("salmon" = "Logged Dollar Amount", "limegreen" = "Shooting Case Count")
      )
  } else if (outcome == "nonlog_dollar"& PACtype=="pro"){
    plot <- plot+
      scale_color_manual(name = "Legend",
                         values = c("salmon" = "salmon", "limegreen" = "limegreen"),
                         labels = c("salmon" = "Dollar Amount (Non-log)", "limegreen" = "Shooting Case Count")
      )
  } else if (outcome == "number"& PACtype=="pro"){
    plot <- plot+
      scale_color_manual(name = "Legend",
                         values = c("salmon" = "salmon", "limegreen" = "limegreen"),
                         labels = c("salmon" = "Contribution Number Count", "limegreen" = "Shooting Case Count")
      ) 
  }
  
  
  plot <- plot+
    theme_minimal() +
    labs(
      # be careful about too lengthy titles
      # title = paste("Nationwide Contribution",title_outcome,"and Shooting Count"),
      
      # title = paste("Nationwide Contribution",title_outcome,"and Shooting Count"),
      title=title,
      subtitle = subtitle,
      x = "Year"
    ) +
    theme(
      legend.position = "bottom",
      legend.title = element_text(face = "bold", size = 14),
      legend.text = element_text(size = 14),
      axis.title.y.right = element_text(color = "limegreen", face = "bold"),
      axis.text.y.right = element_text(color = "limegreen", size = 10),
      axis.title.x = element_text(face = "bold", size = 12),
      axis.text.x = element_text(size = 12),
      # moved to if clause below
      # axis.title.y.left = element_text(color = "black", face = "bold"),
      # axis.text.y.left = element_text(color = "black", size = 6),
      
      plot.title = element_text(face = "bold", size = 10, hjust = 0.5),
      plot.subtitle = element_text(size = 10, hjust = 0.5)
    )
  
  if (PACtype=="anti"&(outcome == "log_dollar" | outcome == "nonlog_dollar")){
    plot <- plot+
      theme(axis.title.y.left = element_text(color = "turquoise", face = "bold"),
            axis.text.y.left = element_text(color = "turquoise", size = 10))
    
  } else if (PACtype=="anti"&outcome == "number"){
    plot <- plot+
      theme(axis.title.y.left = element_text(color = "turquoise", face = "bold"),
            axis.text.y.left = element_text(color = "turquoise", size = 10))
    
  } else if (PACtype=="pro"&(outcome == "log_dollar" | outcome == "nonlog_dollar")){
    plot <- plot+
      theme(axis.title.y.left = element_text(color = "salmon", face = "bold"),
            axis.text.y.left = element_text(color = "salmon", size = 10))
    
  } else if (PACtype=="pro"&outcome == "number"){
    plot <- plot+
      theme(axis.title.y.left = element_text(color = "salmon", face = "bold"),
            axis.text.y.left = element_text(color = "salmon", size = 10))
    
  } 
  
  
  return(plot)
}

# for competitive
antipro_donation_incident_double_comp_plot <- function(df, outcome="log_dollar", # log_dollar or number or nonlog_dollar
                                              title="School Shootings and Anti-gun Contributions",
                                              subtitle, PACtype="anti" # or "pro"
                                              ) {
  
  df <- df %>%
    group_by(year) %>%
    summarise(anti_national_log_sum = log1p(sum(anti_sum_donations, na.rm = TRUE)),
              anti_national_nonlog_sum = sum(anti_sum_donations, na.rm = TRUE),
              anti_national_don_number = sum(anti_don_number, na.rm = TRUE),
              pro_national_log_sum = log1p(sum(pro_sum_donations, na.rm = TRUE)),
              pro_national_nonlog_sum = sum(pro_sum_donations, na.rm = TRUE),
              pro_national_don_number = sum(pro_don_number, na.rm = TRUE),
              national_incident_count = sum(incident_count, na.rm = TRUE)
    )
  
  if (outcome == "log_dollar" & PACtype=="anti"){
    df <- df %>% 
      mutate(national_sum_outcome=anti_national_log_sum)
  } else if (outcome == "nonlog_dollar"& PACtype=="anti"){
    df <- df %>% 
      mutate(national_sum_outcome=anti_national_nonlog_sum)
  } else if (outcome == "log_dollar" & PACtype=="pro"){
    df <- df %>% 
      mutate(national_sum_outcome=pro_national_log_sum)
  } else if (outcome == "nonlog_dollar"& PACtype=="pro"){
    df <- df %>% 
      mutate(national_sum_outcome=pro_national_nonlog_sum)
  }
  
  # outcome patterns
  if (outcome == "log_dollar" | outcome == "nonlog_dollar"){
    y_scale_factor <- max(df$national_sum_outcome) / max(df$national_incident_count)
    df$national_incident_count_revised <- df$national_incident_count * y_scale_factor
  } else if (outcome == "number"){
    y_scale_factor <- max(df$national_don_number) / max(df$national_incident_count)
    df$national_incident_count_revised <- df$national_incident_count * y_scale_factor
  } else {
    stop("Invalid outcome parameter.")
  }
  
  # too messy??
  min_year <- min(df$year)
  max_year <- max(df$year)
  first_even_year <- ifelse(min_year %% 2 == 0, min_year, min_year + 1)
  even_years_seq <- data.frame(year = seq(first_even_year, max_year, by = 2))
  
  # Create the plot
  plot <- ggplot(df, aes(x = year)) 
    
    # Outcome patterns
    # you cannot connect if and plus
    if (PACtype=="anti" & (outcome == "log_dollar" | outcome == "nonlog_dollar")){
      plot <- plot+
        geom_line(aes(y = national_sum_outcome, color = "turquoise"), linetype = "solid", linewidth = 1.2)+
        geom_point(aes(y = national_sum_outcome, color = "turquoise")) 
    } else if (PACtype=="anti" & outcome == "number"){
      plot <- plot+
        geom_line(aes(y = national_don_number, color = "turquoise"), linetype = "solid", linewidth = 1.2)+
        geom_point(aes(y = national_don_number, color = "turquoise")) 
    } else if (PACtype=="pro" & (outcome == "log_dollar" | outcome == "nonlog_dollar")){
      plot <- plot+
        geom_line(aes(y = national_sum_outcome, color = "salmon"), linetype = "solid", linewidth = 1.2)+
        geom_point(aes(y = national_sum_outcome, color = "salmon")) 
    } else if (PACtype=="pro" & outcome == "number"){
      plot <- plot+
        geom_line(aes(y = national_don_number, color = "salmon"), linetype = "solid", linewidth = 1.2)+
        geom_point(aes(y = national_don_number, color = "salmon")) 
    }
  
  
  plot <- plot+
    
    # Incident count line
    geom_line(aes(y = national_incident_count_revised, color = "limegreen"), linetype = "solid", linewidth = 1.2) +
    geom_point(aes(y = national_incident_count_revised, color = "limegreen")) +
    
    # election years are too messy?? 
    geom_vline(data = even_years_seq, aes(xintercept = year),
               color = "gray50", linetype = "dashed", linewidth = 0.6) +
    scale_x_continuous(breaks = even_years_seq$year) 
    
    # Dual Y-axis settings
    if (outcome == "log_dollar"){
      plot <- plot+
        scale_y_continuous(name = "Dollar Amount (logged)",
                         sec.axis = sec_axis(~ . / y_scale_factor, name = "Shooting Occurrence"))
    } else if (outcome == "nonlog_dollar"){
      plot <- plot+
        scale_y_continuous(name = "Dollar Amount (Non-logged)",
                         sec.axis = sec_axis(~ . / y_scale_factor, name = "Shooting Occurrence"))
    } else if (outcome == "number"){
      plot <- plot+
        scale_y_continuous(name = "Contribution Number Count",
                         sec.axis = sec_axis(~ . / y_scale_factor, name = "Shooting Occurrence"))
    } 
  

    # Color settings
    if (outcome == "log_dollar" & PACtype=="anti"){
      plot <- plot+
        scale_color_manual(name = "Legend",
                         values = c("turquoise" = "turquoise", "limegreen" = "limegreen"),
                         labels = c("turquoise" = "Logged Dollar Amount", "limegreen" = "Shooting Case Count")
      )
    } else if (outcome == "nonlog_dollar"& PACtype=="anti"){
      plot <- plot+
        scale_color_manual(name = "Legend",
                         values = c("turquoise" = "turquoise", "limegreen" = "limegreen"),
                         labels = c("turquoise" = "Dollar Amount (Non-log)", "limegreen" = "Shooting Case Count")
      )
    } else if (outcome == "number"& PACtype=="anti"){
      plot <- plot+
        scale_color_manual(name = "Legend",
                         values = c("turquoise" = "turquoise", "limegreen" = "limegreen"),
                         labels = c("turquoise" = "Contribution Number Count", "limegreen" = "Shooting Case Count")
      ) 
    } else if (outcome == "log_dollar" & PACtype=="pro"){
      plot <- plot+
        scale_color_manual(name = "Legend",
                         values = c("salmon" = "salmon", "limegreen" = "limegreen"),
                         labels = c("salmon" = "Logged Dollar Amount", "limegreen" = "Shooting Case Count")
      )
    } else if (outcome == "nonlog_dollar"& PACtype=="pro"){
      plot <- plot+
        scale_color_manual(name = "Legend",
                         values = c("salmon" = "salmon", "limegreen" = "limegreen"),
                         labels = c("salmon" = "Dollar Amount (Non-log)", "limegreen" = "Shooting Case Count")
      )
    } else if (outcome == "number"& PACtype=="pro"){
      plot <- plot+
        scale_color_manual(name = "Legend",
                         values = c("salmon" = "salmon", "limegreen" = "limegreen"),
                         labels = c("salmon" = "Contribution Number Count", "limegreen" = "Shooting Case Count")
      ) 
    }
  
  
  plot <- plot+
    theme_minimal() +
    labs(
      # be careful about too lengthy titles
      # title = paste("Nationwide Contribution",title_outcome,"and Shooting Count"),
      
      # title = paste("Nationwide Contribution",title_outcome,"and Shooting Count"),
      title=title,
      subtitle = subtitle,
      x = "Year"
    ) +
    theme(
      legend.position = "bottom",
      legend.title = element_text(face = "bold", size = 14),
      legend.text = element_text(size = 14),
      axis.title.y.right = element_text(color = "limegreen", face = "bold"),
      axis.text.y.right = element_text(color = "limegreen", size = 10),
      axis.title.x = element_text(face = "bold", size = 12),
      axis.text.x = element_text(size = 12),
      # moved to if clause below
      # axis.title.y.left = element_text(color = "black", face = "bold"),
      # axis.text.y.left = element_text(color = "black", size = 6),
      
      plot.title = element_text(face = "bold", size = 10, hjust = 0.5),
      plot.subtitle = element_text(size = 10, hjust = 0.5)
    )
  
  if (PACtype=="anti"&(outcome == "log_dollar" | outcome == "nonlog_dollar")){
    plot <- plot+
      theme(axis.title.y.left = element_text(color = "turquoise", face = "bold"),
            axis.text.y.left = element_text(color = "turquoise", size = 10))
    
  } else if (PACtype=="anti"&outcome == "number"){
    plot <- plot+
      theme(axis.title.y.left = element_text(color = "turquoise", face = "bold"),
            axis.text.y.left = element_text(color = "turquoise", size = 10))
    
  } else if (PACtype=="pro"&(outcome == "log_dollar" | outcome == "nonlog_dollar")){
    plot <- plot+
      theme(axis.title.y.left = element_text(color = "salmon", face = "bold"),
            axis.text.y.left = element_text(color = "salmon", size = 10))
    
  } else if (PACtype=="pro"&outcome == "number"){
    plot <- plot+
      theme(axis.title.y.left = element_text(color = "salmon", face = "bold"),
            axis.text.y.left = element_text(color = "salmon", size = 10))
    
  } 
  
  
  return(plot)
}


# for competitive
donation_incident_double_comp_cycle_plot <- function(df, outcome="log_dollar", # log_dollar or number or nonlog_dollar
                                                       title="School Shootings and Anti-gun Contributions",
                                                       subtitle, PACtype="anti" # or "pro"
) {
  
  df <- df %>%
    group_by(election_year_next) %>%
    summarise(anti_national_log_sum = log1p(sum(anti_sum_donations, na.rm = TRUE)),
              anti_national_nonlog_sum = sum(anti_sum_donations, na.rm = TRUE),
              anti_national_don_number = sum(anti_don_number, na.rm = TRUE),
              pro_national_log_sum = log1p(sum(pro_sum_donations, na.rm = TRUE)),
              pro_national_nonlog_sum = sum(pro_sum_donations, na.rm = TRUE),
              pro_national_don_number = sum(pro_don_number, na.rm = TRUE),
              national_incident_count = sum(incident_count, na.rm = TRUE)
    )
  
  if (outcome == "log_dollar" & PACtype=="anti"){
    df <- df %>% 
      mutate(national_sum_outcome=anti_national_log_sum)
  } else if (outcome == "nonlog_dollar"& PACtype=="anti"){
    df <- df %>% 
      mutate(national_sum_outcome=anti_national_nonlog_sum)
  } else if (outcome == "log_dollar" & PACtype=="pro"){
    df <- df %>% 
      mutate(national_sum_outcome=pro_national_log_sum)
  } else if (outcome == "nonlog_dollar"& PACtype=="pro"){
    df <- df %>% 
      mutate(national_sum_outcome=pro_national_nonlog_sum)
  }
  
  # outcome patterns
  if (outcome == "log_dollar" | outcome == "nonlog_dollar"){
    y_scale_factor <- max(df$national_sum_outcome) / max(df$national_incident_count)
    df$national_incident_count_revised <- df$national_incident_count * y_scale_factor
  } else if (outcome == "number"){
    y_scale_factor <- max(df$national_don_number) / max(df$national_incident_count)
    df$national_incident_count_revised <- df$national_incident_count * y_scale_factor
  } else {
    stop("Invalid outcome parameter.")
  }
  
  # too messy??
  min_year <- min(df$election_year_next)
  max_year <- max(df$election_year_next)
  first_even_year <- ifelse(min_year %% 2 == 0, min_year, min_year + 1)
  even_years_seq <- data.frame(election_year_next = seq(first_even_year, max_year, by = 2))
  
  # Create the plot
  plot <- ggplot(df, aes(x = election_year_next)) 
  
  # Outcome patterns
  # you cannot connect if and plus
  if (PACtype=="anti" & (outcome == "log_dollar" | outcome == "nonlog_dollar")){
    plot <- plot+
      geom_line(aes(y = national_sum_outcome, color = "turquoise"), linetype = "solid", linewidth = 1.2)+
      geom_point(aes(y = national_sum_outcome, color = "turquoise")) 
  } else if (PACtype=="anti" & outcome == "number"){
    plot <- plot+
      geom_line(aes(y = national_don_number, color = "turquoise"), linetype = "solid", linewidth = 1.2)+
      geom_point(aes(y = national_don_number, color = "turquoise")) 
  } else if (PACtype=="pro" & (outcome == "log_dollar" | outcome == "nonlog_dollar")){
    plot <- plot+
      geom_line(aes(y = national_sum_outcome, color = "salmon"), linetype = "solid", linewidth = 1.2)+
      geom_point(aes(y = national_sum_outcome, color = "salmon")) 
  } else if (PACtype=="pro" & outcome == "number"){
    plot <- plot+
      geom_line(aes(y = national_don_number, color = "salmon"), linetype = "solid", linewidth = 1.2)+
      geom_point(aes(y = national_don_number, color = "salmon")) 
  }
  
  
  plot <- plot+
    
    # Incident count line
    geom_line(aes(y = national_incident_count_revised, color = "limegreen"), linetype = "solid", linewidth = 1.2) +
    geom_point(aes(y = national_incident_count_revised, color = "limegreen")) +
    
    # election years are too messy?? 
    geom_vline(data = even_years_seq, aes(xintercept = election_year_next),
               color = "gray50", linetype = "dashed", linewidth = 0.5) +
    scale_x_continuous(breaks = even_years_seq$election_year_next) 
  
  # Dual Y-axis settings
  if (outcome == "log_dollar"){
    plot <- plot+
      scale_y_continuous(name = "Dollar Amount (logged)",
                         sec.axis = sec_axis(~ . / y_scale_factor, name = "Shooting Occurrence"))
  } else if (outcome == "nonlog_dollar"){
    plot <- plot+
      scale_y_continuous(name = "Dollar Amount (Non-logged)",
                         sec.axis = sec_axis(~ . / y_scale_factor, name = "Shooting Occurrence"))
  } else if (outcome == "number"){
    plot <- plot+
      scale_y_continuous(name = "Contribution Number Count",
                         sec.axis = sec_axis(~ . / y_scale_factor, name = "Shooting Occurrence"))
  } 
  
  
  # Color settings
  if (outcome == "log_dollar" & PACtype=="anti"){
    plot <- plot+
      scale_color_manual(name = "Legend",
                         values = c("turquoise" = "turquoise", "limegreen" = "limegreen"),
                         labels = c("turquoise" = "Logged Dollar Amount", "limegreen" = "Shooting Case Count")
      )
  } else if (outcome == "nonlog_dollar"& PACtype=="anti"){
    plot <- plot+
      scale_color_manual(name = "Legend",
                         values = c("turquoise" = "turquoise", "limegreen" = "limegreen"),
                         labels = c("turquoise" = "Dollar Amount (Non-log)", "limegreen" = "Shooting Case Count")
      )
  } else if (outcome == "number"& PACtype=="anti"){
    plot <- plot+
      scale_color_manual(name = "Legend",
                         values = c("turquoise" = "turquoise", "limegreen" = "limegreen"),
                         labels = c("turquoise" = "Contribution Number Count", "limegreen" = "Shooting Case Count")
      ) 
  } else if (outcome == "log_dollar" & PACtype=="pro"){
    plot <- plot+
      scale_color_manual(name = "Legend",
                         values = c("salmon" = "salmon", "limegreen" = "limegreen"),
                         labels = c("salmon" = "Logged Dollar Amount", "limegreen" = "Shooting Case Count")
      )
  } else if (outcome == "nonlog_dollar"& PACtype=="pro"){
    plot <- plot+
      scale_color_manual(name = "Legend",
                         values = c("salmon" = "salmon", "limegreen" = "limegreen"),
                         labels = c("salmon" = "Dollar Amount (Non-log)", "limegreen" = "Shooting Case Count")
      )
  } else if (outcome == "number"& PACtype=="pro"){
    plot <- plot+
      scale_color_manual(name = "Legend",
                         values = c("salmon" = "salmon", "limegreen" = "limegreen"),
                         labels = c("salmon" = "Contribution Number Count", "limegreen" = "Shooting Case Count")
      ) 
  }
  
  
  plot <- plot+
    theme_minimal() +
    labs(
      # be careful about too lengthy titles
      # title = paste("Nationwide Contribution",title_outcome,"and Shooting Count"),
      
      # title = paste("Nationwide Contribution",title_outcome,"and Shooting Count"),
      title=title,
      subtitle = subtitle,
      x = "House Election Year"
    ) +
    theme(
      legend.position = "bottom",
      legend.title = element_text(face = "bold", size = 14),
      legend.text = element_text(size = 14),
      axis.title.y.right = element_text(color = "limegreen", face = "bold"),
      axis.text.y.right = element_text(color = "limegreen", size = 10),
      axis.title.x = element_text(face = "bold", size = 12),
      axis.text.x = element_text(size = 12),
      # moved to if clause below
      # axis.title.y.left = element_text(color = "black", face = "bold"),
      # axis.text.y.left = element_text(color = "black", size = 6),
      
      plot.title = element_text(face = "bold", size = 10, hjust = 0.5),
      plot.subtitle = element_text(size = 10, hjust = 0.5)
    )
  
  if (PACtype=="anti"&(outcome == "log_dollar" | outcome == "nonlog_dollar")){
    plot <- plot+
      theme(axis.title.y.left = element_text(color = "turquoise", face = "bold"),
            axis.text.y.left = element_text(color = "turquoise", size = 10))
    
  } else if (PACtype=="anti"&outcome == "number"){
    plot <- plot+
      theme(axis.title.y.left = element_text(color = "turquoise", face = "bold"),
            axis.text.y.left = element_text(color = "turquoise", size = 10))
    
  } else if (PACtype=="pro"&(outcome == "log_dollar" | outcome == "nonlog_dollar")){
    plot <- plot+
      theme(axis.title.y.left = element_text(color = "salmon", face = "bold"),
            axis.text.y.left = element_text(color = "salmon", size = 10))
    
  } else if (PACtype=="pro"&outcome == "number"){
    plot <- plot+
      theme(axis.title.y.left = element_text(color = "salmon", face = "bold"),
            axis.text.y.left = element_text(color = "salmon", size = 10))
    
  } 
  
  
  return(plot)
}


three_mass_plot <- function(df_list, dataset_names, 
                            GVAdata, # diff from other two datasets
                            colors=c("turquoise","salmon"),
                            title = "Mass Shooting Based on Three Different Databases",
                            subtitle) {
  
  # get GVA data
  GVA_data <- GVAdata %>%
    group_by(year) %>%
    summarise(
      incident_count = sum(incident_count, na.rm = TRUE)
    )
  
  # get data from MJ and VPP datasets
  incident_count_data <- purrr::map2_dfr(df_list, dataset_names, function(df, name) {
    df %>%
      group_by(year) %>%
      summarise(
        incident_count = sum(incident_count, na.rm = TRUE),
        dataset = name
      )
  })
  
  dataset_colors <- colors  # flexible as to the number of colors
  names(dataset_colors) <- dataset_names
  
  # two patterns of scaling
  y_scale_dollar <- max(GVA_data$incident_count, na.rm = TRUE) / max(incident_count_data$incident_count, na.rm = TRUE)
  y_scale_number <- max(GVA_data$incident_count, na.rm = TRUE) / max(incident_count_data$incident_count, na.rm = TRUE)
  
  plot <- ggplot() +
    # incident : first y axis
    geom_line(data = incident_count_data, 
              aes(x = year, y = incident_count, color = dataset, group = dataset),
              linewidth = 1) +
    geom_point(data = incident_count_data, 
               aes(x = year, y = incident_count, color = dataset, group = dataset),
               size = 1.8) +  # Good visualization for thin lines and big points
    
    scale_color_manual(values = dataset_colors)+
    geom_line(data = GVA_data,
              aes(x = year, y = incident_count / y_scale_dollar),
              color = "darkgreen", linewidth = 1) +
    geom_point(data = GVA_data,
               aes(x = year, y = incident_count / y_scale_dollar),
               color = "darkgreen", size = 1.8, shape = 17) +
    
    # label for different outcome patterns
    scale_y_continuous(
      name = "Incident Count at MJ/VPP Data Set",
      sec.axis = sec_axis(~ . * y_scale_dollar, name = "Incident Count at GVA Data Set")
    ) +
    labs(title = title, subtitle = subtitle,
         x = "Year", color = "Underlying Mass Shooting Datasets") +
    
    theme_minimal() +
    theme(
      legend.position = "bottom",
      plot.title = element_text(face = "bold", hjust = 0.5, size = 12),
      plot.subtitle = element_text(hjust = 0.5, size = 10),
      axis.title.y.left = element_text(color = "black", face = "bold"),
      axis.title.y.right = element_text(color = "darkgreen", face = "bold"),
      axis.text.y.right = element_text(color = "darkgreen"),
      panel.grid.minor = element_blank()
    ) 
  
  return(plot)
}


weighted_margin_timeseries_whole <- function(df, 
                                       title = "Weighted Average of Voting Margin by Gun Contributions",
                                       subtitle = "Weighted by Contribution Dollar Amounts; Unfiltered Data",
                                       log=FALSE
                                       ) {
  
  # Calculate weighted averages by year
  weighted_data <- df %>%
    group_by(year) %>%
    summarise(
      pro_weighted_margin_nonlog = weighted.mean(abs(pct_RDmargin_before), 
                                          w = pro_sum_donations_raw, 
                                          na.rm = TRUE),
      anti_weighted_margin_nonlog = weighted.mean(abs(pct_RDmargin_before), 
                                           w = anti_sum_donations_raw, 
                                           na.rm = TRUE),
      pro_weighted_margin_log = weighted.mean(abs(pct_RDmargin_before), 
                                          w = pro_log_sum_raw, 
                                          na.rm = TRUE),
      anti_weighted_margin_log = weighted.mean(abs(pct_RDmargin_before), 
                                           w = anti_log_sum_raw, 
                                           na.rm = TRUE),
      # Total donations for reference
      total_pro_nonlog = sum(pro_sum_donations_raw, na.rm = TRUE),
      total_anti_nonlog = sum(anti_sum_donations_raw, na.rm = TRUE),
      total_pro_log = sum(pro_log_sum_raw, na.rm = TRUE),
      total_anti_log = sum(anti_log_sum_raw, na.rm = TRUE)
    ) %>%
    # Remove years with no donations (would result in NaN)
    filter(!is.nan(pro_weighted_margin_nonlog) & !is.nan(anti_weighted_margin_nonlog))
  
  if (log==FALSE){
  weighted_data <- weighted_data %>% 
    mutate(pro_weighted_margin = pro_weighted_margin_nonlog,
              anti_weighted_margin = anti_weighted_margin_nonlog)
  } else if (log ==TRUE){
    weighted_data <- weighted_data %>% 
      mutate(pro_weighted_margin = pro_weighted_margin_log,
                anti_weighted_margin = anti_weighted_margin_log)
  }
  
  # Create election year reference lines
  even_years_seq <- data.frame(year = seq(2000, 2024, by = 2))
  
  # Create the plot
  
  plot <- ggplot(weighted_data, aes(x = year)) +
    
    # Pro-gun line (salmon)
    geom_line(aes(y = pro_weighted_margin, color = "pro_gun"), 
              linewidth = 1.2, linetype = "solid") +
    geom_point(aes(y = pro_weighted_margin, color = "pro_gun"), 
               size = 1.5) +
    
    # Anti-gun line (turquoise)  
    geom_line(aes(y = anti_weighted_margin, color = "anti_gun"), 
              linewidth = 1.2, linetype = "solid") +
    geom_point(aes(y = anti_weighted_margin, color = "anti_gun"), 
               size = 1.5) +
    
    # Add election year reference lines
    geom_vline(data = even_years_seq, aes(xintercept = year),
               color = "darkgrey", linewidth = 0.5, linetype = "dashed") +
    
    # Color scheme
    scale_color_manual(
      name = "PAC Type",
      values = c("pro_gun" = "salmon", "anti_gun" = "turquoise"),
      labels = c("pro_gun" = "Pro-gun", "anti_gun" = "GVP")
    ) +
    
    # Scales
    scale_x_continuous(breaks = even_years_seq$year)
  
  if (log ==FALSE){
    plot <- plot +
      scale_y_continuous(
        name = "Weighted Avg. of Voting Margin (%)"
        # name = "Avg. of Voting Margin Weighted by Contribution Dollar (%)" # too long??
        # labels = function(x) paste0(ifelse(x >= 0, "+", ""), x) # only when using non-absolute value and show positive or negative
      ) 
  } else if (log == TRUE) {
    plot <- plot + 
      scale_y_continuous(
        name = "Avg. of Voting Margin Weighted by Logged Dollar (%)"
      ) 
    
  }
    
  plot <- plot+
    # Theme and labels
    theme_minimal() +
    labs(
      title = title,
      subtitle = subtitle,
      x = "Year (Dotted: HoR Election Years)",
      caption = "Note: Weighted by contribution dollar amounts" # have to change for log version
    ) +
    
    theme(
      legend.position = "bottom",
      # legend.title = element_text(face = "bold"),
      legend.text = element_text(size = 13),
      
      plot.title = element_text(face = "bold", size = 12, hjust = 0.5),
      plot.subtitle = element_text(hjust = 0.5, size = 10),
      plot.caption = element_text(size = 11, hjust = 0),
      
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank(),
      axis.text.x = element_text(angle = 45, hjust = 1, size = 11),
      axis.title.x = element_text(size =12),
      axis.title.y = element_text(size =12),
      axis.text.y = element_text(size =11),
      
      panel.background = element_rect(fill = "white", color = NA),
      plot.background = element_rect(fill = "white", color = NA)
    )
  
  return(plot)
}



contribution_scatter_by_margin_whole <- function(df, title = "Total Gun-related PAC Contributions by Voting Margin",
                                           subtitle = "Pro-gun + GVP Contributions; Data Aggregated by Election Cycle",
                                           outcome = "log_dollar_total", # total or pro or anti or absolute dollar or number
                                           y_label="Logged Dollar Amount", smooth=FALSE, ylim=4) {
  
  # Calculate total donations
  plot_data <- df %>%
    mutate(total_donations_raw = pro_sum_donations_raw + anti_sum_donations_raw,
           total_number_raw = pro_don_number_raw + anti_don_number_raw) %>%
    group_by(election_year_next, id) %>%
    summarise(
      district_total_donations = sum(total_donations_raw, na.rm = TRUE),
      district_pro_donations = sum(pro_sum_donations_raw, na.rm = TRUE),
      district_anti_donations = sum(anti_sum_donations_raw, na.rm = TRUE),
      
      district_total_log_sum = log1p(district_total_donations),
      district_pro_log_sum = log1p(district_pro_donations),
      district_anti_log_sum = log1p(district_anti_donations),
      
      district_don_number = sum(total_number_raw, na.rm = TRUE),
      pct_RDmargin_before = mean(pct_RDmargin_before, na.rm = TRUE),
      any_incident = if_else(sum(incident, na.rm = TRUE) > 0, 1L, 0L),
      treatment = factor(any_incident, levels = c(0, 1), labels = c("No Shooting", "With Shooting")),
      .groups = "drop"
    )
  
  # Create the plot
  
  if (outcome=="log_dollar_total"){
    plot <- ggplot(plot_data, aes(x = pct_RDmargin_before, y = district_total_log_sum, color=treatment)) 
  } else if (outcome=="log_dollar_pro"){
    plot <- ggplot(plot_data, aes(x = pct_RDmargin_before, y = district_pro_log_sum, color=treatment)) 
  }  else if (outcome=="log_dollar_anti"){
    plot <- ggplot(plot_data, aes(x = pct_RDmargin_before, y = district_anti_log_sum, color=treatment)) 
  } 
  else if (outcome=="dollar"){
    plot <- ggplot(plot_data, aes(x = pct_RDmargin_before, y = district_total_donations, color=treatment)) 
  } else if (outcome=="number"){
    plot <- ggplot(plot_data, aes(x = pct_RDmargin_before, y = district_don_number, color=treatment))  
  }
  
  plot <- plot+ 
    # Scatter plot with transparency
    geom_point(alpha = 0.7, 
               size = 2.1) +
    scale_color_manual(
      name = "District",
      values = c("No Shooting" = "lightcoral", "With Shooting" = "lightblue"),
      guide = guide_legend(override.aes = list(alpha = 1))
    ) 
  
  if (smooth=="TRUE"){
    plot <- plot+
      geom_smooth(method = "loess", se = FALSE, alpha=1,
                  # color = "darkred",
                  linewidth = 1) 
    
  }
  
  plot <- plot+
    
    # Add vertical lines at competitive district boundaries
    geom_vline(xintercept = 5, color = "gray20", linewidth = 1.1, linetype = "dashed") +
    geom_vline(xintercept = -5, color = "gray20", linewidth = 1.1, linetype = "dashed") +
    geom_vline(xintercept = 0, color = "gray30", linewidth = 0.4, linetype = "solid") +
    
    
    scale_x_continuous(
      name = "Voting Margin in the Previous House Election(%)",
      breaks = seq(-100, 100, by = 10)
    ) 
  
  if (outcome == "dollar") {
    plot <- plot + 
      scale_y_continuous(
        name = y_label,
        labels = scales::comma_format(scale = 1, big.mark = ",")
      )
  } else {
    plot <- plot + 
      scale_y_continuous(name = y_label)
  }
  
  plot <- plot+
    # Theme and labels
    theme_minimal() +
    labs(
      title = title,
      subtitle = subtitle,
      caption = "Note: Dots within vertical dashed lines indicate competitive districts (±5%)"
    ) +
    coord_cartesian(xlim = c(-100,100), 
                    ylim=c(ylim,NA))+
    theme(
      plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
      plot.subtitle = element_text(hjust = 0.5, size = 12),
      plot.caption = element_text(size = 11, hjust = 0),
      
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank(),
      legend.position = "bottom",
      legend.text = element_text(size = 14),
      legend.title = element_text(size = 14, face = "bold"),
      axis.text.x = element_text(angle = 45, hjust = 1),
      axis.title.y = element_text(size = 13),
      axis.title.x = element_text(size = 13)
      
    )
  
  return(plot)
}


contribution_scatter_by_margin_comp <- function(df, title = "Total Gun-related PAC Contributions by Voting Margin",
                                           subtitle = "Pro-gun + GVP Contributions; Data Aggregated by Election Cycle",
                                           outcome = "log_dollar_total", # total or pro or anti or absolute dollar or number
                                           y_label="Logged Dollar Amount", smooth=FALSE, ylim=4) {
  
  # Calculate total donations
  plot_data <- df %>%
    mutate(total_donations = pro_sum_donations + anti_sum_donations,
           total_number = pro_don_number + anti_don_number) %>%
    group_by(election_year_next, id) %>%
    summarise(
      district_total_donations = sum(total_donations, na.rm = TRUE),
      district_pro_donations = sum(pro_sum_donations, na.rm = TRUE),
      district_anti_donations = sum(anti_sum_donations, na.rm = TRUE),
      
      district_total_log_sum = log1p(district_total_donations),
      district_pro_log_sum = log1p(district_pro_donations),
      district_anti_log_sum = log1p(district_anti_donations),
      
      district_don_number = sum(total_number, na.rm = TRUE),
      pct_RDmargin_before = mean(pct_RDmargin_before, na.rm = TRUE),
      any_incident = if_else(sum(incident, na.rm = TRUE) > 0, 1L, 0L),
      treatment = factor(any_incident, levels = c(0, 1), labels = c("No Shooting", "With Shooting")),
      .groups = "drop"
    )
  
  # Create the plot
  
  if (outcome=="log_dollar_total"){
    plot <- ggplot(plot_data, aes(x = pct_RDmargin_before, y = district_total_log_sum, color=treatment)) 
  } else if (outcome=="log_dollar_pro"){
    plot <- ggplot(plot_data, aes(x = pct_RDmargin_before, y = district_pro_log_sum, color=treatment)) 
  }  else if (outcome=="log_dollar_anti"){
    plot <- ggplot(plot_data, aes(x = pct_RDmargin_before, y = district_anti_log_sum, color=treatment)) 
  } 
  else if (outcome=="dollar"){
    plot <- ggplot(plot_data, aes(x = pct_RDmargin_before, y = district_total_donations, color=treatment)) 
  } else if (outcome=="number"){
    plot <- ggplot(plot_data, aes(x = pct_RDmargin_before, y = district_don_number, color=treatment))  
  }
  
  plot <- plot+ 
    # Scatter plot with transparency
    geom_point(alpha = 0.7, 
               size = 2.1) +
    scale_color_manual(
      name = "District",
      values = c("No Shooting" = "lightcoral", "With Shooting" = "lightblue"),
      guide = guide_legend(override.aes = list(alpha = 1))
    ) 
  
  if (smooth=="TRUE"){
    plot <- plot+
      geom_smooth(method = "loess", se = FALSE, alpha=1,
                  # color = "darkred",
                  linewidth = 1) 
    
  }
  
  plot <- plot+
    
    # Add vertical lines at competitive district boundaries
    geom_vline(xintercept = 5, color = "gray20", linewidth = 1.1, linetype = "dashed") +
    geom_vline(xintercept = -5, color = "gray20", linewidth = 1.1, linetype = "dashed") +
    geom_vline(xintercept = 0, color = "gray30", linewidth = 0.4, linetype = "solid") +
    
    
    scale_x_continuous(
      name = "Voting Margin in the Previous House Election(%)",
      breaks = seq(-100, 100, by = 10)
    ) 
  
  if (outcome == "dollar") {
    plot <- plot + 
      scale_y_continuous(
        name = y_label,
        labels = scales::comma_format(scale = 1, big.mark = ",")
      )
  } else {
    plot <- plot + 
      scale_y_continuous(name = y_label)
  }
  
  plot <- plot+
    # Theme and labels
    theme_minimal() +
    labs(
      title = title,
      subtitle = subtitle,
      caption = "Note: Dots within vertical dashed lines indicate competitive districts (±5%)"
    ) +
    coord_cartesian(xlim = c(-100,100), 
                    ylim=c(ylim,NA))+
    theme(
      plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
      plot.subtitle = element_text(hjust = 0.5, size = 12),
      plot.caption = element_text(size = 11, hjust = 0),
      
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank(),
      legend.position = "bottom",
      legend.text = element_text(size = 14),
      legend.title = element_text(size = 14, face = "bold"),
      axis.text.x = element_text(angle = 45, hjust = 1),
      axis.title.y = element_text(size = 13),
      axis.title.x = element_text(size = 13)
      
    )
  
  return(plot)
}

