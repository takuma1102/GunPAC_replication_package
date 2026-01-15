# TWFE Analyses

# 0. Preparation -------------------------------------------------------------

if (!require("here")) install.packages("here")
library(here)

source(here(
  # "gunPAC",
  "PNAS_script", "01_packages.R"))
source(here(
  # "gunPAC",
  "PNAS_R", "functions_pre_analysis_updated.R"))
# source(here(
#   # "gunPAC",
#   "PNAS_script", "02_data_wrangling.R")) # can skip this
# skip_data_wrangling(TRUE) # can skip this as well later
# source(here(
#   # "gunPAC",
#   "PNAS_script", "03_descriptive_stats_updated.R"))
source(here(
  # "gunPAC",
  "PNAS_R", "functions_analysis_updated.R"))


# 1 Baseline TWFE  ----------------------------------------------------------------

# Assume homogeneity
# Use anti_pro_baseline_DID function for analysis
# Use gt_table function for gt visualization

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


## 1.0 No history ----------------------------------------------------


anti_SS_base_twfe_dol <- anti_SS_base_twfe_dol_nohistory <- anti_pro_baseline_DID(data_s_margin_baseline)

pro_SS_base_twfe_dol <- pro_SS_base_twfe_dol_nohistory <- anti_pro_baseline_DID(data_s_margin_baseline, 
                                                        outcome = "pro_log_sum")

# number outcome
anti_SS_base_twfe_dol_num <- anti_SS_base_twfe_dol_nohistory_num <- anti_pro_baseline_DID(data_s_margin_baseline,
                                                             outcome = "anti_don_number")
pro_SS_base_twfe_dol_num <- pro_SS_base_twfe_dol_nohistory_num <- anti_pro_baseline_DID(data_s_margin_baseline, 
                                                            outcome = "pro_don_number")

# etable(anti_SS_base_twfe_dol, pro_SS_base_twfe_dol, anti_SS_base_twfe_dol_num, pro_SS_base_twfe_dol_num)

### 1.0.1 Sharpley Decomposition -----------------------------

# Use loo_importance function

importance_ranking_anti <- loo_importance(data_s_margin_baseline)
importance_ranking_pro <- loo_importance(data_s_margin_baseline, outcome="pro_log_sum")

# Visualization
importance_ranking_anti <- importance_ranking_anti %>% 
  dplyr::select(variable, relative_importance) %>%
  mutate(PAC_type = "Gun Control PAC",
         relative_importance=100 * relative_importance)

importance_ranking_pro <- importance_ranking_pro %>% 
  dplyr::select(variable, relative_importance) %>%
  mutate(PAC_type = "Gun Lobby PAC",
         relative_importance=100 * relative_importance)

combined_importance <- rbind(importance_ranking_pro, importance_ranking_anti) %>% 
  mutate(variable = case_when(
    variable == "incident_window_re" ~ "Fatal School Shooting",
    variable == "black" ~ "Black Population",
    variable == "gun_ratio_pct" ~ "Firearm Possession Rate",
    variable == "rep_incumbent_before" ~ "Republican Incumbency",
    variable == "bachelor" ~ "Bachelor's Degree",
    variable == "log_income" ~ "Log Median Income",
    variable == "unemployment" ~ "Unemployment Rate",
    variable == "white" ~ "White Population",
    TRUE ~ variable
  ))

importance_table <- combined_importance %>%
  pivot_wider(names_from = variable, 
              values_from = relative_importance)

gt_importance <- importance_table %>%
  gt() %>%
  tab_header(
    title = "Relative Importance of Variables by PAC Type",
    subtitle = "Reduced Within-R-Squared Through Leave-One-Out Cross-Validation"
  ) %>%
  cols_label(
    PAC_type = "PAC Type"
  ) %>%
  fmt_number(
    columns = -PAC_type,
    decimals = 2
  ) %>%
  opt_table_font(font = "Times New Roman") %>%
  tab_options(
    table.font.size = 12,
    heading.title.font.size = 14,
    heading.subtitle.font.size = 12
  )

gt_importance

## 1.1 Post-treatment var. and other side history --------------------------------------------

# 3-month history of the other side as a post-treatment variable
anti_SS_base_twfe_dol_3m <- anti_pro_baseline_DID(data_s_margin_baseline,
                                               more_controls = "pro_logsum_3month_prior")
pro_SS_base_twfe_dol_3m <- anti_pro_baseline_DID(data_s_margin_baseline, outcome="pro_log_sum" ,
                                              more_controls = "anti_logsum_3month_prior")

anti_SS_base_twfe_dol_3m_num <- anti_pro_baseline_DID(data_s_margin_baseline,
                                                      outcome = "anti_don_number",
                                                  more_controls = "pro_logsum_3month_prior")
pro_SS_base_twfe_dol_3m_num <- anti_pro_baseline_DID(data_s_margin_baseline, outcome="pro_don_number" ,
                                                 more_controls = "anti_logsum_3month_prior")


# 6 month version
anti_SS_base_twfe_dol_6m <- anti_pro_baseline_DID(data_s_margin_baseline,
                                                  more_controls = "pro_logsum_6month_prior")
pro_SS_base_twfe_dol_6m <- anti_pro_baseline_DID(data_s_margin_baseline,outcome="pro_log_sum",
                                                 more_controls = "anti_logsum_6month_prior")
anti_SS_base_twfe_dol_6m_num <- anti_pro_baseline_DID(data_s_margin_baseline,outcome = "anti_don_number",
                                                  more_controls = "pro_logsum_6month_prior")
pro_SS_base_twfe_dol_6m_num <- anti_pro_baseline_DID(data_s_margin_baseline,outcome="pro_don_number",
                                                 more_controls = "anti_logsum_6month_prior")
## 1.2  Both sides history --------------------------------------------

# next, using both prior history variables
anti_SS_base_twfe_dol_both <- anti_pro_baseline_DID(data_s_margin_baseline,
                                                    more_controls = "pro_logsum_3month_prior + anti_logsum_3month_prior")

anti_SS_base_twfe_dol_bothnum <- anti_pro_baseline_DID(data_s_margin_baseline, outcome = "anti_don_number",
                                                    more_controls = "pro_logsum_3month_prior + anti_logsum_3month_prior")

# gun lobby
pro_SS_base_twfe_dol_both <- anti_pro_baseline_DID(data_s_margin_baseline, outcome="pro_log_sum", 
                                                   more_controls = "pro_logsum_3month_prior + anti_logsum_3month_prior")

pro_SS_base_twfe_dol_bothnum <- anti_pro_baseline_DID(data_s_margin_baseline, outcome="pro_don_number", 
                                                   more_controls = "pro_logsum_3month_prior + anti_logsum_3month_prior")


## 1.3 Visualization ----------------------------------------------------------

GVP_basic_list <- list("Basic"=anti_SS_base_twfe_dol_nohistory,
                       "With Pro-gun Contribution (3m)"=anti_SS_base_twfe_dol_3m, 
                       "With Pro-gun Contribution (6m)"=anti_SS_base_twfe_dol_6m,
                       "With Both-Side Contribution (3m)" = anti_SS_base_twfe_dol_both,
                       "Number Outcome"=anti_SS_base_twfe_dol_nohistory_num,
                       "With Pro-gun Contribution (3m); Number Outcome"=anti_SS_base_twfe_dol_3m_num, 
                       "With Pro-gun Contribution (6m); Number Outcome"=anti_SS_base_twfe_dol_6m_num,
                       "With Both-Side Contribution (3m); Number Outcome" = anti_SS_base_twfe_dol_bothnum)

gun_basic_list <- list("Basic"=pro_SS_base_twfe_dol_nohistory,
                       "With Anti-gun Contribution (3m)"=pro_SS_base_twfe_dol_3m, 
                       "With Anti-gun Contribution (6m)"=pro_SS_base_twfe_dol_6m,
                       "With Both-Side Contribution (3m)" = pro_SS_base_twfe_dol_both,
                       "Number Outcome"=pro_SS_base_twfe_dol_nohistory_num,
                       "With Anti-gun Contribution (3m); Number Outcome"=pro_SS_base_twfe_dol_3m_num, 
                       "With Anti-gun Contribution (6m); Number Outcome"=pro_SS_base_twfe_dol_6m_num,
                       "With Both-Side Contribution (3m); Number Outcome" = pro_SS_base_twfe_dol_bothnum)


gof_add_strategic <- data.frame(
  raw = c("Outcome", "Opponent Side Contributions", "Same Side Contributions"),
  mod1 = c("Log Dollar", "No", "No" ), 
  mod2 = c("Log Dollar","Yes (3m)", "No"),
  mod3 = c("Log Dollar","Yes (6m)", "No"),
  mod4 = c("Log Dollar","Yes (3m)", "Yes"),
  mod5 = c("Number", "No", "No"),  
  mod6 = c("Number", "Yes (3m)", "No"),
  mod7 = c("Number","Yes (6m)", "No"),
  mod8 = c("Number","Yes (3m)", "Yes"),
  stringsAsFactors = FALSE
)

# gof_add_basicGVPgun <- data.frame(
#   raw = c("Outcome", "Prior gun lobby contributions", "After Parkland (Dummy)", "Prior GVP contributions"),
#   mod1 = c("Log Dollar", "No", "No", "No"), 
#   mod2 = c("Log Dollar", "No", "No", "No"),
#   mod3 = c("Log Dollar","Yes (3m)", "No", "No"),
#   mod4 = c("Log Dollar","Yes (3m)", "No", "No"),
#   mod5 = c("Log Dollar","Yes (3m)", "Yes", "Yes (3m)"),
#   mod6 = c("Log Dollar","Yes (3m)", "Yes", "Yes (3m)"),
#   stringsAsFactors = FALSE
# )


GVP_str_TWFE <- gt_table(GVP_basic_list, coef_map_GVP_basic,  c("nobs"),
                           gof_add_strategic,"Baseline GVP TWFE; Strategic Patterns",
                           "SS Dataset; HoR") %>% print()

gun_str_TWFE <- gt_table(gun_basic_list, coef_map_gun_basic,  c("nobs"),
                                  gof_add_strategic,"Baseline Gun Lobby TWFE; Strategic Patterns",
                                  "SS Dataset; HoR") %>% print()



# 2. Robustness -----------------------------------------------------------



## 2.1 financial variations -----------------------------------------------------

data_fin1 <- data_flexible(data_s, minimum_dol = 5000)

anti_SS_covfin0 <- anti_pro_baseline_DID(data_s_margin_baseline)
anti_SS_covfin1 <- anti_pro_baseline_DID(data_fin1)
anti_SS_covfin0_num <- anti_pro_baseline_DID(data_s_margin_baseline, outcome = "anti_don_number")
anti_SS_covfin1_num <- anti_pro_baseline_DID(data_fin1, outcome = "anti_don_number")

# strategic
anti_SS_covfin0_str <- anti_pro_baseline_DID(data_s_margin_baseline, more_controls = "pro_logsum_3month_prior")
anti_SS_covfin1_str <- anti_pro_baseline_DID(data_fin1, more_controls = "pro_logsum_3month_prior")
anti_SS_covfin0_num_str <- anti_pro_baseline_DID(data_s_margin_baseline, outcome = "anti_don_number", more_controls = "pro_logsum_3month_prior")
anti_SS_covfin1_num_str <- anti_pro_baseline_DID(data_fin1, outcome = "anti_don_number", more_controls = "pro_logsum_3month_prior")

# pro gun
pro_SS_covfin0 <- anti_pro_baseline_DID(data_s_margin_baseline, outcome = "pro_log_sum")
pro_SS_covfin1 <- anti_pro_baseline_DID(data_fin1, outcome = "pro_log_sum")
pro_SS_covfin0_num <- anti_pro_baseline_DID(data_s_margin_baseline, outcome = "pro_don_number")
pro_SS_covfin1_num <- anti_pro_baseline_DID(data_fin1, outcome = "pro_don_number")

pro_SS_covfin0_str <- anti_pro_baseline_DID(data_s_margin_baseline, outcome = "pro_log_sum", more_controls = "anti_logsum_3month_prior")
pro_SS_covfin1_str <- anti_pro_baseline_DID(data_fin1, outcome = "pro_log_sum", more_controls = "anti_logsum_3month_prior")
pro_SS_covfin0_num_str <- anti_pro_baseline_DID(data_s_margin_baseline, outcome = "pro_don_number", more_controls = "anti_logsum_3month_prior")
pro_SS_covfin1_num_str <- anti_pro_baseline_DID(data_fin1, outcome = "pro_don_number", more_controls = "anti_logsum_3month_prior")


anti_covfin_results <- list(anti_SS_covfin0,anti_SS_covfin1, 
                                anti_SS_covfin0_num,anti_SS_covfin1_num)
anti_covfinstr_results <- list(anti_SS_covfin0_str,anti_SS_covfin1_str, 
                            anti_SS_covfin0_num_str,anti_SS_covfin1_num_str)

pro_covfin_results <- list(pro_SS_covfin0,pro_SS_covfin1, 
                            pro_SS_covfin0_num,pro_SS_covfin1_num)
pro_covfinstr_results <- list(pro_SS_covfin0_str,pro_SS_covfin1_str, 
                               pro_SS_covfin0_num_str,pro_SS_covfin1_num_str)

names(anti_covfin_results) <- names(pro_covfin_results) <- c("Basic Pattern (10k)", "5k","Number Outcome",
                                                                     "Number Outcome (5k)")

names(anti_covfinstr_results) <- names(pro_covfinstr_results) <- c("Basic Pattern (10k); Strategic",
                                                                   "5k; Strategic",
                                                                   "Number Outcome; Strategic",
                                                             "Number Outcome (5k); Strategic")

gof_add_cov_fin <- data.frame(
  raw = c("Outcome", "Minimum Threshold", "Strategicness"),
  mod1 = c("Logged Dollar", "10k", "No"),
  mod2 = c("Logged Dollar", "5k", "No"),
  mod3 = c("Number Count", "10k", "No"),
  mod4 = c("Number Count", "5k", "No"),
  stringsAsFactors = FALSE
)

gof_add_cov_finstr <- data.frame(
  raw = c("Outcome", "Minimum Threshold", "Strategicness"),
  mod1 = c("Logged Dollar", "10k", "Yes"),
  mod2 = c("Logged Dollar", "5k", "Yes"),
  mod3 = c("Number Count", "10k", "Yes"),
  mod4 = c("Number Count", "5k", "Yes"),
  stringsAsFactors = FALSE
)


anti_SS_covfin_results <- gt_table(anti_covfin_results, coef_map_single,  c("nobs"),
                                   gof_add_cov_fin,"Different Minimum Contribution Threshold",
                                   "GVP; SS Dataset; No Strategicness") %>% print()

pro_SS_covfin_results <- gt_table(pro_covfin_results, coef_map_single,  c("nobs"),
                                  gof_add_cov_fin,"Different Minimum Contribution Threshold",
                                  "Gun Lobby; SS Dataset; No Strategicness") %>% print()


anti_SS_covfinstr_results <- gt_table(anti_covfinstr_results, coef_map_single,  c("nobs"),
                                   gof_add_cov_finstr,"Different Minimum Contribution Threshold",
                                   "GVP; SS Dataset; Strategicness") %>% print()

pro_SS_covfinstr_results <- gt_table(pro_covfinstr_results, coef_map_single,  c("nobs"),
                                  gof_add_cov_finstr,"Different Minimum Contribution Threshold",
                                  "Gun Lobby; SS Dataset; Strategicness") %>% print()


## 2.2 Politics-related Covariates -----------------------------------------

anti_SS_covpol1 <- anti_pro_baseline_DID(data_s_margin_baseline, more_controls ="pct_RDmargin_before")
anti_SS_covpol2 <- anti_pro_baseline_DID(data_s_margin_baseline, more_controls ="abs(pct_RDmargin_before)")
anti_SS_covpol3 <- anti_pro_baseline_DID(data_s_margin_baseline, more_controls ="president_rep")

pro_SS_covpol1 <- anti_pro_baseline_DID(data_s_margin_baseline, more_controls ="pct_RDmargin_before", outcome = "pro_log_sum")
pro_SS_covpol2 <- anti_pro_baseline_DID(data_s_margin_baseline, more_controls ="abs(pct_RDmargin_before)", outcome = "pro_log_sum")
pro_SS_covpol3 <- anti_pro_baseline_DID(data_s_margin_baseline, more_controls ="president_rep", outcome = "pro_log_sum")


anti_covpol_dol_list <- list(anti_SS_base_twfe_dol,anti_SS_covpol1, 
                                anti_SS_covpol2,anti_SS_covpol3)
pro_covpol_dol_list <- list(pro_SS_base_twfe_dol,pro_SS_covpol1, 
                                pro_SS_covpol2,pro_SS_covpol3)

names(anti_covpol_dol_list) <- names(pro_covpol_dol_list) <- c("Basic Pattern", "Voting margin","Absolute voting margin",
                               "Presidency term")

gof_add_cov_pol <- data.frame(
  raw = c("Outcome", "Voting Marin Covariate", "Presidency Covariate"),
  mod1 = c("Logged Dollar", "No", "No"),
  mod2 = c("Logged Dollar", "Yes", "No"),
  mod3 = c("Logged Dollar", "Yes (Abs.)", "No"),
  mod4 = c("Logged Dollar", "No", "Yes"),
  stringsAsFactors = FALSE
)

coef_map_pol <- c(
  "incident_window_re" = "Treatment",
  "pct_RDmargin_before"="Voting Margin",
  "abs(pct_RDmargin_before)" = "Absolute Voting Margin",
  "president_rep"="Presidency Term")

anti_SS_covpol_results <- gt_table(anti_covpol_dol_list, coef_map_pol,  c("nobs"),
                              gof_add_cov_pol,"Baseline DID Comparison",
                              "GVP; SS Dataset; Politics-related Covariates") %>% print()

pro_SS_covpol_results <- gt_table(pro_covpol_dol_list, coef_map_pol,  c("nobs"),
                                   gof_add_cov_pol,"Baseline DID Comparison",
                                   "Gun Lobby; SS Dataset; Politics-related Covariates") %>% print()


## 2.3 Catalyst events -----------------------------------------

# First, Bruen
data_s_margin_baseline$bruen <- 
  data_s_margin_baseline$year>=2022

# about interaction with treatment
anti_SS_cov_bruenint <- anti_pro_baseline_DID(data_s_margin_baseline, more_controls="bruen*incident_window_re")

pro_SS_cov_bruenint <- anti_pro_baseline_DID(data_s_margin_baseline, more_controls="bruen*incident_window_re",
                                              outcome = "pro_log_sum")

# Next, Bruen

data_s_margin_baseline$blm <- 
  data_s_margin_baseline$year>=2020&
  data_s_margin_baseline$year<=2021

# data_s_margin_baseline$blm_state <- 
#   data_s_margin_baseline$year >= 2020 & 
#   data_s_margin_baseline$year <= 2021 & 
#   data_s_margin_baseline$state == "MN"

anti_SS_cov_blmint <- anti_pro_baseline_DID(data_s_margin_baseline, more_controls="blm*incident_window_re")
pro_SS_cov_blmint <- anti_pro_baseline_DID(data_s_margin_baseline, more_controls="blm*incident_window_re",
                                            outcome = "pro_log_sum")

# anti_SS_cov_blmstate <- anti_pro_baseline_DID(data_s_margin_baseline, 
#                                               more_controls="blm_state*incident_window_re")
# 
# 
# pro_SS_cov_blmstate <- anti_pro_baseline_DID(data_s_margin_baseline, 
#                                               more_controls="blm_state*incident_window_re",
#                                                outcome = "pro_log_sum")


# visualization
anti_cov_catalyst_list <- list(anti_SS_base_twfe_dol,
                            anti_SS_cov_bruenint,anti_SS_cov_blmint)

pro_cov_catalyst_list <- list(pro_SS_base_twfe_dol,
                               pro_SS_cov_bruenint,pro_SS_cov_blmint)


names(anti_cov_catalyst_list) <- names(pro_cov_catalyst_list) <- c("Basic Pattern", 
                                                             "Interaction with Bruen",
                                                             "Interaction with BLM")

gof_add_cov_catalyst <- data.frame(
  raw = c( "Interaction with Bruen Dummy", "Interaction with BLM Dummy"),
  mod1 = c( "No", "No"),
  mod2 = c( "Yes", "No"),
  mod3 = c( "No", "Yes"),
  stringsAsFactors = FALSE
)

# check coef name
lapply(anti_cov_catalyst_list, \(m) names(stats::coef(m)))

coef_map_catalyst <- c(
  "incident_window_re" = "Treatment",
  "bruenTRUE" = "Bruen Dummy (>=2022)",
  "incident_window_re:bruenTRUE"="Interaction with Bruen Dummy",
  "blmTRUE" = "BLM Dummy (2020-21)",
  "incident_window_re:blmTRUE"="Interaction with BLM Dummy")

anti_SS_cov_catalyst_results <- gt_table(anti_cov_catalyst_list, coef_map_catalyst,  c("nobs"),
                                      gof_add_cov_catalyst,"Baseline DID Comparison",
                                      "GVP; Interaction with Bruen/BLM") %>% print()

pro_SS_cov_catalyst_results <- gt_table(pro_cov_catalyst_list, coef_map_catalyst,  c("nobs"),
                                         gof_add_cov_catalyst,"Baseline DID Comparison",
                                         "Gun Lobby; Interaction with Bruen/BLM") %>% print()



## 2.4 State-level-FE and other FE settings -----------------------------------------------

# be careful with clustering unit as well

# gun control PAC
anti_SS_base_twfe_dol <- anti_pro_baseline_DID(data_s_margin_baseline)
anti_SS_base_twfe_dol_fe <- anti_pro_baseline_DID(data_s_margin_baseline,  
                                                  fe="state_num", cluster = "state_num")
anti_SS_base_twfe_num <- anti_pro_baseline_DID(data_s_margin_baseline, outcome="anti_don_number")
anti_SS_base_twfe_num_fe <- anti_pro_baseline_DID(data_s_margin_baseline, outcome="anti_don_number", 
                                                  fe="state_num", cluster = "state_num")


# zero covariates
anti_SS_base_twfe_dol_zerocov <- fixest::feols(anti_log_sum~incident_window_re| id+time,
                                          data = data_s_margin_baseline,cluster = "id")
anti_SS_base_twfe_dol_zerocov_num <- fixest::feols(anti_don_number~incident_window_re | id+time,
                                              data = data_s_margin_baseline,cluster = "id")
anti_SS_base_twfe_dol_zerocov_state <- fixest::feols(anti_log_sum~incident_window_re| state_num+time,
                                               data = data_s_margin_baseline,cluster = "state_num")
anti_SS_base_twfe_dol_zerocov_num_state <- fixest::feols(anti_don_number~incident_window_re | state_num+time,
                                                   data = data_s_margin_baseline,cluster = "state_num")


# gun lobby
pro_SS_base_twfe_dol <- anti_pro_baseline_DID(data_s_margin_baseline, outcome = "pro_log_sum")
pro_SS_base_twfe_dol_fe <- anti_pro_baseline_DID(data_s_margin_baseline, outcome = "pro_log_sum",
                                                 fe="state_num", cluster = "state_num")
pro_SS_base_twfe_num <- anti_pro_baseline_DID(data_s_margin_baseline, outcome="pro_don_number")
pro_SS_base_twfe_num_fe <- anti_pro_baseline_DID(data_s_margin_baseline, outcome="pro_don_number",
                                                 fe="state_num", cluster = "state_num")

# zero covariates
pro_SS_base_twfe_dol_zerocov <- fixest::feols(pro_log_sum~incident_window_re| id+time,
                                               data = data_s_margin_baseline,cluster = "id")
pro_SS_base_twfe_dol_zerocov_num <- fixest::feols(pro_don_number~incident_window_re | id+time,
                                                   data = data_s_margin_baseline,cluster = "id")
pro_SS_base_twfe_dol_zerocov_state <- fixest::feols(pro_log_sum~incident_window_re| state_num+time,
                                              data = data_s_margin_baseline,cluster = "state_num")
pro_SS_base_twfe_dol_zerocov_num_state <- fixest::feols(pro_don_number~incident_window_re | state_num+time,
                                                  data = data_s_margin_baseline,cluster = "state_num")


# List
anti_ss_state_FE <- list(
  "Baseline" = anti_SS_base_twfe_dol,
  "State FE" = anti_SS_base_twfe_dol_fe,
  "Number Outcome" = anti_SS_base_twfe_num,
  "State FE; Number Outcome" = anti_SS_base_twfe_num_fe
) 

pro_ss_state_FE <- list(
  "Baseline" = pro_SS_base_twfe_dol,
  "State FE" = pro_SS_base_twfe_dol_fe,
  "Number Outcome" = pro_SS_base_twfe_num,
  "State FE; Number Outcome" = pro_SS_base_twfe_num_fe
) 

anti_zerocov_list <- list(
"Zero Covariate" = anti_SS_base_twfe_dol_zerocov,
"Zero Covariate; State FE" = anti_SS_base_twfe_dol_zerocov_state,
"Zero Covariate; Number Outcome" = anti_SS_base_twfe_dol_zerocov_num,
"Zero Covariate; Number Outcome; State FE" = anti_SS_base_twfe_dol_zerocov_num_state)

pro_zerocov_list <- list(
  "Zero Covariate" = pro_SS_base_twfe_dol_zerocov,
  "Zero Covariate; State FE" = pro_SS_base_twfe_dol_zerocov_state,
  "Zero Covariate; Number Outcome" = pro_SS_base_twfe_dol_zerocov_num,
  "Zero Covariate; Number Outcome; State FE" = pro_SS_base_twfe_dol_zerocov_num_state)



# Add row
gof_add_statefe <- data.frame(
  raw = c("Unit FE and Clustering","Outcome"),
  mod1 = c("District","Logged Dollar"),
  mod2 = c("State","Logged Dollar"),
  mod3 = c("District","Number"),
  mod4 = c("State","Number"),
  stringsAsFactors = FALSE
)


# Plot
anti_ss_state_FEplot <- gt_table(anti_ss_state_FE, coef_map_single,c("nobs"),
                            gof_add_statefe,"State FE Patterns",
                            "GVP: SS Dataset: HoR") %>% print()

pro_ss_state_FEplot <- gt_table(pro_ss_state_FE, coef_map_single,c("nobs"),
                                 gof_add_statefe,"State FE Patterns",
                                 "Gun Lobby: SS Dataset: HoR")%>% print()


anti_ss_zerocovplot <- gt_table(anti_zerocov_list, coef_map_single,c("nobs"),
                                gof_add_statefe,"No Cov Patterns",
                                 "GVP: SS Dataset: HoR", 
                                source_note = "No covariates are included.")%>% print()


pro_ss_zerocovplot <- gt_table(pro_zerocov_list, coef_map_single,c("nobs"),
                               gof_add_statefe,"No Cov Patterns",
                                "Gun Lobby: SS Dataset: HoR", 
                               source_note = "No covariates are included.")%>% print()


# 3 Different voting margin -----------------------------------------

# use data_voting_filter function

anti_ss_votingmargin <- list(
  # "~ -20" = anti_pro_baseline_DID(data_voting_filter(data_s_2000_filtered, -100, -20)),
  "-20 ~ -10" = anti_pro_baseline_DID(data_voting_filter(data_s_2000_filtered, -20, -10)),
  "-10 ~ -5" = anti_pro_baseline_DID(data_voting_filter(data_s_2000_filtered, -10, -5)),
  "-5 ~ 5 (Basic Filtering)" = anti_pro_baseline_DID(data_voting_filter(data_s_2000_filtered, -5, 5)),
  "5 ~ 10" = anti_pro_baseline_DID(data_voting_filter(data_s_2000_filtered, 5, 10)),
  "10 ~ 20" = anti_pro_baseline_DID(data_voting_filter(data_s_2000_filtered, 10, 20))
  # "20 ~" = anti_pro_baseline_DID(data_voting_filter(data_s_2000_filtered, 20, 100))
) 

pro_ss_votingmargin <- list(
  # "~ -20" = anti_pro_baseline_DID(data_voting_filter(data_s_2000_filtered, -100, -20), outcome = "pro_log_sum"),
  "-20 ~ -10" = anti_pro_baseline_DID(data_voting_filter(data_s_2000_filtered, -20, -10), outcome = "pro_log_sum"),
  "-10 ~ -5" = anti_pro_baseline_DID(data_voting_filter(data_s_2000_filtered, -10, -5), outcome = "pro_log_sum"),
  "-5 ~ 5 (Basic Filtering)" = anti_pro_baseline_DID(data_voting_filter(data_s_2000_filtered, -5, 5), outcome = "pro_log_sum"),
  "5 ~ 10" = anti_pro_baseline_DID(data_voting_filter(data_s_2000_filtered, 5, 10), outcome = "pro_log_sum"),
  "10 ~ 20" = anti_pro_baseline_DID(data_voting_filter(data_s_2000_filtered, 10, 20), outcome = "pro_log_sum")
  # "20 ~" = anti_pro_baseline_DID(data_voting_filter(data_s_2000_filtered, 20, 100), outcome = "pro_log_sum")
) 

# Add row
gof_add_votingmargin <- data.frame(
  raw = c("Voting Margin at the previous HoR election"),
  # mod1 = c("~ -20"),
  mod1 = c("-20 ~ -10"),
  mod2 = c("-10 ~ -5"),
  mod3 = c("-5 ~ 5"),
  mod4 = c("5 ~ 10"),
  mod5 = c("10 ~ 20"),
  # mod6 = c("20 ~"),
  stringsAsFactors = FALSE
)

# Plot
anti_ss_votingmargin_plot <- gt_table(anti_ss_votingmargin, coef_map_single,c("nobs"),
                                 gof_add_votingmargin,"Baseline DID with Different Voting Margin",
                                 "GVP PACs; HoR") %>%   print()

pro_ss_votingmargin_plot <- gt_table(pro_ss_votingmargin, coef_map_single,c("nobs"),
                                      gof_add_votingmargin,"Baseline DID with Different Voting Margin",
                                      "Gun Lobby PACs; HoR") %>%   print()


## 3.1 Strategic Pattern ---------------------------------------------

# took into account the other side's prior contribution

anti_ss_votingmargin_strategic <- list(
  "-20 ~ -10" = anti_pro_baseline_DID(data_voting_filter(data_s_2000_filtered, -20, -10), more_controls = "pro_logsum_3month_prior"),
  "-10 ~ -5" = anti_pro_baseline_DID(data_voting_filter(data_s_2000_filtered, -10, -5), more_controls = "pro_logsum_3month_prior"),
  "-5 ~ 5 (Basic Filtering)" = anti_pro_baseline_DID(data_voting_filter(data_s_2000_filtered, -5, 5), more_controls = "pro_logsum_3month_prior"),
  "5 ~ 10" = anti_pro_baseline_DID(data_voting_filter(data_s_2000_filtered, 5, 10), more_controls = "pro_logsum_3month_prior"),
  "10 ~ 20" = anti_pro_baseline_DID(data_voting_filter(data_s_2000_filtered, 10, 20), more_controls = "pro_logsum_3month_prior")
  ) 

pro_ss_votingmargin_strategic <- list(
  "-20 ~ -10" = anti_pro_baseline_DID(data_voting_filter(data_s_2000_filtered, -20, -10), outcome = "pro_log_sum",
                                      more_controls = "anti_logsum_3month_prior"),
  "-10 ~ -5" = anti_pro_baseline_DID(data_voting_filter(data_s_2000_filtered, -10, -5), outcome = "pro_log_sum",
more_controls = "anti_logsum_3month_prior"),
  "-5 ~ 5 (Basic Filtering)" = anti_pro_baseline_DID(data_voting_filter(data_s_2000_filtered, -5, 5), outcome = "pro_log_sum",
more_controls = "anti_logsum_3month_prior"),
  "5 ~ 10" = anti_pro_baseline_DID(data_voting_filter(data_s_2000_filtered, 5, 10), outcome = "pro_log_sum",
more_controls = "anti_logsum_3month_prior"),
  "10 ~ 20" = anti_pro_baseline_DID(data_voting_filter(data_s_2000_filtered, 10, 20), outcome = "pro_log_sum",
more_controls = "anti_logsum_3month_prior")
) 


anti_ss_votingmargin_strategic_plot <- gt_table(anti_ss_votingmargin_strategic, 
                                           coef_map_GVP_basic ,c("nobs"),
                                           gof_add_votingmargin,"Baseline DID with Different Voting Margin",
                                           "GVP PACs; Strategic Pattern") %>% print()

pro_ss_votingmargin_strategic_plot <- gt_table(pro_ss_votingmargin_strategic, 
                                                coef_map_gun_basic ,c("nobs"),
                                                gof_add_votingmargin,"Baseline DID with Different Voting Margin",
                                                "Gun Lobby PACs; Strategic Pattern") %>% print()


# 4. Different month to elections -------------------------------------

## 4.0 Unused function -----------------------------------------------------


# unlike the other function, diff_month, here is a function of using one unified
# treatment var. and only focus on treatment meeting a specified threshold (timing) 

# Either would be ok to make a long function or separate functions for each process
# When it comes to plot, it is so similar to other sections.

# diff_month_stratified <- function(df, # Here, I will use the whole dataset, not filtered ones
#                                   month_list = c(2,3,4,8,12, 23),
#                                   outcome= "anti_log_sum", 
#                                   month_cov=FALSE, # add months_to as cov
#                                   # no need to specify levels since I specify this within this func.
#                                   # levels,
#                                   labels=  c("2 months","3 months", "4 months","8 months", '12 months',  "All"),
#                                   title="Baseline DID by Different Thresholds on Incident Timing and Treatment Period",
#                                   subtitle =  "GVP Contributions", 
#                                   footnote =  "Based on basic filtering and covariates", 
#                                   ylab = "Effect on Logged Dollar Amount", # since fliped, it should be horizontal
#                                   fatal = TRUE, # only focusing on fatal cases
#                                   voting_margin = 5, 
#                                   treat_period="short" # other two options: election and 24
# ) {
#   
#   # preparation of for loop
#   month_list <- month_list[!is.null(month_list)]  # remove NULL
#   
#   # data wrangling
#   # df <- data_margin_baseline(df)
#   
#   if (fatal==TRUE){
#     df <- data_margin_baseline(df, voting_margin)
#   } else if (fatal==FALSE){
#     df <- as.data.frame(df) %>%
#       filter(abs(pct_RDmargin_before)<=5) %>% 
#       
#       # removed fatal restriction
#       
#       mutate(anti_don_number = ifelse(anti_pac_donations_dollar>=10000, anti_don_number, 0),
#              pro_don_number = ifelse(pro_pac_donations_dollar>=10000, pro_don_number, 0),
#              anti_log_sum = ifelse(anti_pac_donations_dollar>=10000, anti_log_sum, 0),
#              pro_log_sum = ifelse(pro_pac_donations_dollar>=10000, pro_log_sum, 0)) %>% 
#       
#       arrange(id, time) %>%
#       group_by(id) %>%
#       
#       mutate(last_incident_time_re = ifelse(incident == 1, time, NA)) %>% # specify the last incident time
#       fill(last_incident_time_re, .direction = "down") %>%
#       mutate(incident_window_re = if_else(!is.na(last_incident_time_re) & (time - last_incident_time_re <= 24), 1, 0),
#              incident_window_re = as.integer(incident_window_re)) %>%
#       
#       ungroup() %>% 
#       filter(year>=2000)
#     
#   }
#   
#   # for results
#   results_list <- list()
#   
#   for (i in seq_along(month_list)) {
#     current_month <- month_list[i]
#     
#     if (treat_period=="short"){
#       df <- df %>%
#         mutate(incident_month = ifelse(incident >= 1 & months_to_election <= current_month, 1, 0)) %>% 
#         arrange(id, time) %>%
#         group_by(id) %>%
#         
#         mutate(last_incident_time_re_month = ifelse(incident_month == 1, time, NA)) %>% # specify the last incident time
#         fill(last_incident_time_re_month, .direction = "down") %>%
#         
#         # instead of 24 months, use current month
#         # mutate(incident_window_re = if_else(!is.na(last_incident_time_re) & (time - last_incident_time_re <= 24), 1, 0),
#         mutate(incident_window_re_month = if_else(!is.na(last_incident_time_re_month) & (time - last_incident_time_re_month <= current_month), 1, 0),
#                incident_window_re_month = as.integer(incident_window_re_month)) # have to use the same treatment var. name as period case
#       
#     } else if (treat_period=="election"){
#       df <- df %>%
#         mutate(incident_month = ifelse(incident >= 1 & months_to_election <= current_month, 1, 0)) %>% 
#         arrange(id, time) %>%
#         group_by(id) %>%
#         
#         mutate(last_incident_time_re_month = ifelse(incident_month == 1, time, NA)) %>% # specify the last incident time
#         fill(last_incident_time_re_month, .direction = "down") %>%
#         
#         # instead of 24 months, use current month
#         # mutate(incident_window_re = if_else(!is.na(last_incident_time_re) & (time - last_incident_time_re <= 24), 1, 0),
#         mutate(incident_window_re_month = if_else(!is.na(last_incident_time_re_month) & (time - last_incident_time_re_month <= current_month) &(months_to_election <=current_month+1), 1, 0),
#                incident_window_re_month = as.integer(incident_window_re_month)) # have to use the same treatment var. name as period case
#       
#     } else if (treat_period=="24"){
#       df <- df %>%
#         mutate(incident_month = ifelse(incident >= 1 & months_to_election <= current_month, 1, 0)) %>% 
#         arrange(id, time) %>%
#         group_by(id) %>%
#         
#         mutate(last_incident_time_re_month = ifelse(incident_month == 1, time, NA)) %>% # specify the last incident time
#         fill(last_incident_time_re_month, .direction = "down") %>%
#         
#         # instead of 24 months, use current month
#         # mutate(incident_window_re = if_else(!is.na(last_incident_time_re) & (time - last_incident_time_re <= 24), 1, 0),
#         mutate(incident_window_re_month = if_else(!is.na(last_incident_time_re_month) & (time - last_incident_time_re_month <= 24), 1, 0),
#                incident_window_re_month = as.integer(incident_window_re_month)) # have to use the same treatment var. name as period case
#       
#       
#     }
#     
#     # anti_pro_baseline_DID(df,outcome) assumes incident_window_re as treatment, so we have to specify a regression here.
#     # before, multi collin happened, and I used year dummy instead of time.
#     
#     if (outcome=="anti_log_sum"& month_cov == TRUE){
#       did_month_model <-  fixest::feols(anti_log_sum~incident_window_re_month+ bachelor + black + 
#                                           white + unemployment + log_income + rep_incumbent_before 
#                                         +months_to_election # needed???
#                                         + gun_ratio_pct 
#                                         | id+ time, data = df, cluster = "id")
#     } else if (outcome=="anti_don_number"& month_cov == TRUE){
#       did_month_model <-  fixest::feols(anti_don_number~incident_window_re_month+ bachelor + black + 
#                                           white + unemployment + log_income + rep_incumbent_before 
#                                         +months_to_election
#                                         + gun_ratio_pct 
#                                         | id+ time, data = df, cluster = "id")
#     } else if (outcome=="anti_log_sum"& month_cov == FALSE){
#       did_month_model <-  fixest::feols(anti_log_sum~incident_window_re_month+ bachelor + black + 
#                                           white + unemployment + log_income + rep_incumbent_before 
#                                         + gun_ratio_pct 
#                                         | id+ time, data = df, cluster = "id")
#     } else if (outcome=="anti_don_number" & month_cov== FALSE){
#       did_month_model <-  fixest::feols(anti_don_number~incident_window_re_month+ bachelor + black + 
#                                           white + unemployment + log_income + rep_incumbent_before 
#                                         + gun_ratio_pct 
#                                         | id+ time, data = df, cluster = "id")
#     }  else if (outcome=="pro_log_sum"& month_cov == TRUE){
#       did_month_model <-  fixest::feols(pro_log_sum~incident_window_re_month+ bachelor + black + 
#                                           white + unemployment + log_income + rep_incumbent_before 
#                                         +months_to_election # needed???
#                                         + gun_ratio_pct 
#                                         | id+ time, data = df, cluster = "id")
#     } else if (outcome=="pro_don_number"& month_cov == TRUE){
#       did_month_model <-  fixest::feols(pro_don_number~incident_window_re_month+ bachelor + black + 
#                                           white + unemployment + log_income + rep_incumbent_before 
#                                         +months_to_election
#                                         + gun_ratio_pct 
#                                         | id+ time, data = df, cluster = "id")
#     } else if (outcome=="pro_log_sum"& month_cov == FALSE){
#       did_month_model <-  fixest::feols(pro_log_sum~incident_window_re_month+ bachelor + black + 
#                                           white + unemployment + log_income + rep_incumbent_before 
#                                         + gun_ratio_pct 
#                                         | id+ time, data = df, cluster = "id")
#     } else if (outcome=="pro_don_number" & month_cov== FALSE){
#       did_month_model <-  fixest::feols(pro_don_number~incident_window_re_month+ bachelor + black + 
#                                           white + unemployment + log_income + rep_incumbent_before 
#                                         + gun_ratio_pct 
#                                         | id+ time, data = df, cluster = "id")
#     }
#     
#     # Extracting part of results
#     # do not use the name of ..._i, unless it should be dynamic within for loop 
#     did_month_coef <- tidy(did_month_model) %>%
#       filter(term == "incident_window_re_month")  # have to be the same var. name in either case
#     
#     # safeguard for multicollin
#     if(nrow(did_month_coef) == 0) {
#       next
#     }
#     
#     did_month_coef <-did_month_coef %>% 
#       mutate(n_treated = sum(df[["incident_window_re_month"]], na.rm = TRUE),
#              threshold_name = paste0(i, "-month"), # this is its level for the time being,
#              # so no need to specify otherwise
#              original_index = i  # for the following error handling
#       )
#     results_list[[i]] <- did_month_coef
#   }
#   
#   # Just in case where all patterns show multi-collinearity
#   if(length(results_list) == 0) {
#     stop("No results due to multicollinearity")
#   }
#   
#   # dfs are better than lists
#   results_df <- bind_rows(results_list) %>% 
#     # error handling
#     mutate(threshold_name = labels[original_index]) %>% # skip error months, and avoid
#     # the entire stopping
#     
#     mutate(threshold_name = factor(threshold_name,
#                                    levels = labels,  # not levels, because I used lables as levels above
#                                    labels = labels)) %>% 
#     # c("Immediate\n(<= 2m)","Close\n(<= 3m)", "Medium\n(<= 6m)", "Distant\n(<= 12m)", 'All')
#     
#     # no need to make this kable, since I do not make output here
#     dplyr::select(threshold_name, estimate, std.error,p.value,  n_treated) %>% 
#     # have to specify for geom errorbar
#     mutate(
#       conf.low = estimate - 1.96 * std.error,
#       conf.high = estimate + 1.96 * std.error)
#   
#   plot <- results_df%>%
#     ggplot(aes( # first, x and y axes are opposite
#       x = threshold_name, y = estimate
#       # , ymin = conf.low, ymax = conf.high
#     )) +
#     
#     # pointrage = point + error bar, but you cannnot specify width
#     # geom_pointrange() +
#     
#     geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.4) +
#     geom_point(aes(size = n_treated)  # size of n.obs does not matter here, since that
#                # means all the months since 2000. n.treated matters.
#     ) +
#     
#     geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
#     coord_flip() + # for better visualization
#     scale_size_continuous(name = "# of treated months",
#                           guide = guide_legend(title.position = "top")) +
#     labs(
#       # title="Baseline DID by Different Thresholds on Incident Timing and Treatment Period",
#       title = title,
#       subtitle=subtitle,
#       x = "Month to the Next Election",
#       y = ylab,
#       caption = footnote) +
#     theme_minimal()+
#     theme(plot.title = element_text(size=10, hjust = 0.5, face = "bold"),
#           plot.subtitle = element_text(size=10, hjust = 0.5),
#           axis.text.y = element_text(size = 10, colour = "black"), # no need to make ytitle bold
#           plot.caption = element_text(size=8,hjust = 0.5) # 0 might look odd
#     )
#   
#   # return(plot)
#   
#   # when needing bpth plot and estimate results
#   return(list(plot, results_df))
# }
# 
# diff_month_stratified(anti_pro_data_s)[1]
# # pro-gun
# diff_month_stratified(anti_pro_data_s, outcome = "pro_log_sum",
#                       subtitle = "Gun Lobby Contributions")[1]



## 4.1 Analysis ------------------------------------------------------------

# Three treatment period patterns
# treatment period: equal to threshold
anti_did_month_baseline_short <- diff_month(data_s, # use the whole untrimmed data
                                       title=NULL,subtitle=NULL, 
                                       treat_period="short"
                                       # subtitle = "GVP PACs; Dollar Outcome; Threshold Period"
)
anti_did_month_baseline_short[[1]]
anti_did_month_baseline_short[[2]]

pro_did_month_baseline_short <- diff_month(data_s, outcome = "pro_log_sum",title=NULL,subtitle=NULL, 
                                            treat_period="short")
pro_did_month_baseline_short[[1]]
pro_did_month_baseline_short[[2]]


# limiting to after parkland
# datasets already defined are already filtered in terms of other aspects than years.
data_s_201824 <- data_s %>% 
  filter(year>=2018)
data_s_200017 <- data_s %>% 
  filter(year<2018)

anti_did_month_baseline_short_201824 <- diff_month(data_s_201824,title=NULL,subtitle=NULL, 
                                                   treat_period="short")
anti_did_month_baseline_short_201824[[1]]
anti_did_month_baseline_short_201824[[2]]
# threshold_name estimate std.error  p.value n_treated conf.low conf.high
# <fct>             <dbl>     <dbl>    <dbl>     <int>    <dbl>     <dbl>
#   1 2 months          3.76      0.281 2.46e-23         5    3.21      4.31 
# 2 3 months          3.15      0.227 2.89e-24         6    2.70      3.59 
# 3 4 months          2.72      0.192 8.58e-25         7    2.34      3.09 
# 4 8 months          0.433     0.388 2.68e- 1        39   -0.328     1.19 
# 5 12 months         0.175     0.280 5.34e- 1        73   -0.374     0.723
# 6 All               0.197     0.162 2.27e- 1       138   -0.120     0.514
anti_did_month_baseline_short_200017 <- diff_month(data_s_200017,,title=NULL,subtitle=NULL, 
                                                   treat_period="short")
anti_did_month_baseline_short_200017[[1]]



### 4.1.1 Detail of result composition ------------------------------------------------------

# First, prep of datasets
# Use diff_month_results

monthto_df2 <- diff_month_results(data_s, # original data
                               month_threshold=2,  start_year=2000)
monthto_df6 <- diff_month_results(data_s, # original data
                                  month_threshold=6,  start_year=2000)
monthto_df12 <- diff_month_results(data_s, # original data
                                  month_threshold=12,  start_year=2000)
monthto_df23 <- diff_month_results(data_s, # original data
                                  month_threshold=23,  start_year=2000)

# after Parkland
monthto_df2_1824 <- diff_month_results(data_s,month_threshold=2,  start_year=2018)
monthto_df6_1824 <- diff_month_results(data_s,month_threshold=6,  start_year=2018)
monthto_df12_1824 <- diff_month_results(data_s,month_threshold=12,  start_year=2018)
monthto_df23_1824 <- diff_month_results(data_s,month_threshold=23,  start_year=2018)

# Analysis 
did_monthto_anti2_1824 <-  anti_pro_baseline_DID(monthto_df2_1824,
                                             treatment = "incident_window_re_month"
                                             # ,outcome = "anti_sum_donations"
                                             )
did_monthto_anti6_1824 <-  anti_pro_baseline_DID(monthto_df6_1824, treatment = "incident_window_re_month")
did_monthto_anti12_1824 <-  anti_pro_baseline_DID(monthto_df12_1824, treatment = "incident_window_re_month")
did_monthto_anti23_1824 <-  anti_pro_baseline_DID(monthto_df23_1824, treatment = "incident_window_re_month")


did_monthto_pro2 <-  anti_pro_baseline_DID(monthto_df2,treatment = "incident_window_re_month",outcome = "pro_log_sum")
did_monthto_pro6 <-  anti_pro_baseline_DID(monthto_df6,treatment = "incident_window_re_month",outcome = "pro_log_sum")
did_monthto_pro12 <-  anti_pro_baseline_DID(monthto_df12,treatment = "incident_window_re_month",outcome = "pro_log_sum")
did_monthto_pro23 <-  anti_pro_baseline_DID(monthto_df23,treatment = "incident_window_re_month",outcome = "pro_log_sum")

did_monthto_anti2 <-  anti_pro_baseline_DID(monthto_df2,treatment = "incident_window_re_month")
did_monthto_anti6 <-  anti_pro_baseline_DID(monthto_df6,treatment = "incident_window_re_month")
did_monthto_anti12 <-  anti_pro_baseline_DID(monthto_df12,treatment = "incident_window_re_month")
did_monthto_anti23 <-  anti_pro_baseline_DID(monthto_df23,treatment = "incident_window_re_month")



### 4.1.2 distribution of large-size donations ------------------------------------------------------


#### 4.1.2.1 Dataset ---------------------------------------------------------


actual_control2 <- monthto_df2 %>% 
  dplyr::filter(incident_window_re_month==0)
actual_control6 <- monthto_df6 %>% 
  dplyr::filter(incident_window_re_month==0)
actual_control12 <- monthto_df12 %>% 
  dplyr::filter(incident_window_re_month==0)
actual_control23 <- monthto_df23 %>% 
  dplyr::filter(incident_window_re_month==0)

actual_control2_1824 <- monthto_df2_1824 %>% 
  dplyr::filter(incident_window_re_month==0)
actual_control6_1824 <- monthto_df6_1824 %>% 
  dplyr::filter(incident_window_re_month==0)
actual_control12_1824 <- monthto_df12_1824 %>% 
  dplyr::filter(incident_window_re_month==0)
actual_control23_1824 <- monthto_df23_1824 %>% 
  dplyr::filter(incident_window_re_month==0)

actual_treat2 <- monthto_df2 %>% 
  dplyr::filter(incident_window_re_month==1)
actual_treat6 <- monthto_df6 %>% 
  dplyr::filter(incident_window_re_month==1)
actual_treat12 <- monthto_df12 %>% 
  dplyr::filter(incident_window_re_month==1)
actual_treat23 <- monthto_df23 %>% 
  dplyr::filter(incident_window_re_month==1)

actual_treat2_1824 <- monthto_df2_1824 %>% 
  dplyr::filter(incident_window_re_month==1)
actual_treat6_1824 <- monthto_df6_1824 %>% 
  dplyr::filter(incident_window_re_month==1)
actual_treat12_1824 <- monthto_df12_1824 %>% 
  dplyr::filter(incident_window_re_month==1)
actual_treat23_1824 <- monthto_df23_1824 %>% 
  dplyr::filter(incident_window_re_month==1)


#### 4.1.2.2 Distribution ----------------------------------------------------


table(actual_control2$anti_log_sum)
table(actual_control6$anti_log_sum)
table(actual_control12$anti_log_sum)
table(actual_control23$anti_log_sum)

table(actual_treat2$anti_log_sum)
table(actual_treat6$anti_log_sum)
table(actual_treat12$anti_log_sum)
table(actual_treat23$anti_log_sum)

table(actual_control2_1824$anti_log_sum)
table(actual_control6_1824$anti_log_sum)
table(actual_control12_1824$anti_log_sum)
table(actual_control23_1824$anti_log_sum)

table(actual_treat2_1824$anti_log_sum)
table(actual_treat6_1824$anti_log_sum)
table(actual_treat12_1824$anti_log_sum)
table(actual_treat23_1824$anti_log_sum)


table(actual_control2$pro_log_sum)
table(actual_control6$pro_log_sum)
table(actual_control12$pro_log_sum)
table(actual_control23$pro_log_sum)

table(actual_treat2$pro_log_sum)
table(actual_treat6$pro_log_sum)
table(actual_treat12$pro_log_sum)
table(actual_treat23$pro_log_sum)


count2_anti <- sum(actual_control2$anti_log_sum > 1, na.rm = TRUE) %>% print()
count6_anti <- sum(actual_control6$anti_log_sum > 1, na.rm = TRUE) %>% print()
count12_anti <- sum(actual_control12$anti_log_sum > 1, na.rm = TRUE) %>% print()
count23_anti <- sum(actual_control23$anti_log_sum > 1, na.rm = TRUE) %>% print()
count2_anti_1824 <- sum(actual_control2_1824$anti_log_sum > 1, na.rm = TRUE) %>% print()
count6_anti_1824 <- sum(actual_control6_1824$anti_log_sum > 1, na.rm = TRUE) %>% print()
count12_anti_1824 <- sum(actual_control12_1824$anti_log_sum > 1, na.rm = TRUE) %>% print()
count23_anti_1824 <- sum(actual_control23_1824$anti_log_sum > 1, na.rm = TRUE) %>% print()

count2_pro <- sum(actual_control2$pro_log_sum > 1, na.rm = TRUE) %>% print()
count6_pro <- sum(actual_control6$pro_log_sum > 1, na.rm = TRUE) %>% print()
count12_pro <- sum(actual_control12$pro_log_sum > 1, na.rm = TRUE) %>% print()
count23_pro <- sum(actual_control23$pro_log_sum > 1, na.rm = TRUE) %>% print()
# count2_pro_1824 <- sum(actual_control2_1824$pro_log_sum > 1, na.rm = TRUE) %>% print()
# count6_pro_1824 <- sum(actual_control6_1824$pro_log_sum > 1, na.rm = TRUE) %>% print()
# count12_pro_1824 <- sum(actual_control12_1824$pro_log_sum > 1, na.rm = TRUE) %>% print()
# count23_pro_1824 <- sum(actual_control23_1824$pro_log_sum > 1, na.rm = TRUE) %>% print()


count2all_anti <- sum(actual_control2$anti_log_sum > -1, na.rm = TRUE) %>% print()
count6all_anti <- sum(actual_control6$anti_log_sum > -1, na.rm = TRUE) %>% print()
count12all_anti <- sum(actual_control12$anti_log_sum > -1, na.rm = TRUE) %>% print()
count23all_anti <- sum(actual_control23$anti_log_sum > -1, na.rm = TRUE) %>% print()
count2all_anti_1824 <- sum(actual_control2_1824$anti_log_sum > -1, na.rm = TRUE) %>% print()
count6all_anti_1824 <- sum(actual_control6_1824$anti_log_sum > -1, na.rm = TRUE) %>% print()
count12all_anti_1824 <- sum(actual_control12_1824$anti_log_sum > -1, na.rm = TRUE) %>% print()
count23all_anti_1824 <- sum(actual_control23_1824$anti_log_sum > -1, na.rm = TRUE) %>% print()



contri_p_pro <- mean(actual_control2$pro_log_sum > 1, na.rm = TRUE) %>% print()

p2_anti <- mean(actual_control2$anti_log_sum > 1, na.rm = TRUE) %>% print()
p2_anti <- mean(actual_control2$anti_log_sum > 1, na.rm = TRUE) %>% print()




## 4.2 (Removed) -----------------------------------------------------------



## 4.3 Month-to-election as a covariate ----------------------------------------

# using basic treatment period, which is equal to threshold

# add a covariate
anti_did_month_baseline_cov <- diff_month(data_s,month_cov=TRUE,
                                     footnote =  "Based on basic filtering; basic covariates and month-to-election covariate",
                                      title=NULL,subtitle=NULL)
                                     # subtitle = "School Shooting; Dollar Outcome; Months to election covariate"
                         
anti_did_month_baseline_cov[[1]]

pro_did_month_baseline_cov <- diff_month(data_s,month_cov=TRUE, outcome = "pro_log_sum",
                                          footnote =  "Based on basic filtering; basic covariates and month-to-election covariate",
                                          title=NULL,subtitle=NULL)
pro_did_month_baseline_cov[[1]]

# number outcome; ylab has to be changed
anti_did_month_baseline_number <- diff_month(data_s,outcome="anti_don_number",
                                        title=NULL,subtitle=NULL,
                                        ylab = "Effect on Number Count")
anti_did_month_baseline_number[[1]]


## 4.4 Other mass shooting datasets --------------------------------------------------------------

anti_did_month_MJ <- diff_month(data_m, title = NULL, subtitle = NULL
                           # title="MJ Dataset: Different Thresholds on Incident Timing and Treatment Period",
                           # subtitle =  "Mass Shooting; HoR; Dollar Outcome"
)
pro_did_month_MJ <- diff_month(data_m, outcome = "pro_log_sum",
                               title = NULL, subtitle = NULL)


anti_did_month_VPP <- diff_month(data_v,title = NULL, subtitle=NULL
                            # title="VPP Dataset: Different Thresholds on Incident Timing and Treatment Period",
                            # subtitle =  "Mass Shooting; HoR; Dollar Outcome"
)

pro_did_month_VPP <- diff_month(data_v, outcome = "pro_log_sum",
                               title = NULL, subtitle = NULL)

anti_did_month_GVA <- diff_month(data_g,title=NULL, subtitle=NULL
                            #      title="GVA Dataset: Different Thresholds on Incident Timing and Treatment Period",
                            # subtitle =  "Mass Shooting; HoR; Dollar Outcome"
)

pro_did_month_GVA <- diff_month(data_g, outcome = "pro_log_sum",
                                title = NULL, subtitle = NULL)

## 4.5 non-fatal ----------------------------------------------------

# Use data_nonfatal_unfiltered function for almost-no filter and focusing on non-fatal cases
# we need a new treatment set
data_s_nonfatal_unfiltered <- data_nonfatal_unfiltered(data_s)
data_s_nonfatal_unfiltered1824 <- data_s_nonfatal_unfiltered %>% 
  filter(year>=2018)

data_s_nonfatal_unfiltered0017 <- data_s_nonfatal_unfiltered %>% 
  filter(year<2018)

# do not need any more data wrangling function, since diff_month function requires minimum filtered dataset

# check non-fatal incident case numbers
table(data_s_nonfatal_unfiltered$incident)
table(data_s_2000$incident)

# around 80% of cases are non-fatal.

anti_did_month_nonfatal_1824 <- diff_month(data_s_nonfatal_unfiltered1824, # use the whole untrimmed data
                                 title = NULL, subtitle=NULL,
                                 # title="Different Thresholds on Incident Timing and Treatment Period",
                                 # subtitle =  "Non-fatal School Shooting; Dollar Outcome", 
                                 fatal = FALSE
)

anti_did_month_nonfatal_0017 <- diff_month(data_s_nonfatal_unfiltered0017, # use the whole untrimmed data
                                           title = NULL, subtitle=NULL,
                                           # title="Different Thresholds on Incident Timing and Treatment Period",
                                           # subtitle =  "Non-fatal School Shooting; Dollar Outcome", 
                                           fatal = FALSE
)

pro_did_month_nonfatal <- diff_month(data_s_nonfatal_unfiltered, outcome = "pro_log_sum",
                                      title = NULL, subtitle=NULL,
                                      fatal = FALSE
)



## 4.6 non-competitive districts ------------------------------------

# Use data_noncompetitive_unfiltered function
# Differentiate non-competitive v. all

data_s_noncompetitive_unfiltered <- data_noncompetitive_unfiltered(data_s)

data_s_noncompetitive_unfiltered_1824 <- data_s_noncompetitive_unfiltered %>% 
  filter(year>=2018)

data_s_noncompetitive_unfiltered_0017 <- data_s_noncompetitive_unfiltered %>% 
  filter(year<2018)

anti_did_month_noncompetitive <- diff_month(data_s_noncompetitive_unfiltered, 
                                      title = NULL, subtitle=NULL,
                                     #   title="Different Thresholds on Incident Timing and Treatment Period",
                                     # subtitle =  "All Districts Including Non-competitive Districts; Dollar Outcome", 
                                     voting_margin = 100
)

anti_did_month_noncompetitive_1824 <- diff_month(data_s_noncompetitive_unfiltered_1824, 
                                            title = NULL, subtitle=NULL,
                                            #   title="Different Thresholds on Incident Timing and Treatment Period",
                                            # subtitle =  "All Districts Including Non-competitive Districts; Dollar Outcome", 
                                            voting_margin = 100
)

anti_did_month_noncompetitive_0017 <- diff_month(data_s_noncompetitive_unfiltered_0017, 
                                                title = NULL, subtitle=NULL,
                                                #   title="Different Thresholds on Incident Timing and Treatment Period",
                                                # subtitle =  "All Districts Including Non-competitive Districts; Dollar Outcome", 
                                                voting_margin = 100
)


pro_did_month_noncompetitive <- diff_month(data_s_noncompetitive_unfiltered, 
                                           outcome = "pro_log_sum",
                                            title = NULL, subtitle=NULL,voting_margin = 100
)


## 4.7 Strategic Patterns --------------------------------------------------
# Use diff_month_strategic function


anti_did_month_strategic <- diff_month_strategic(data_s, title = NULL, subtitle=NULL)
# anti_did_month_strategic[[1]]

anti_did_month_strategic_0017 <- diff_month_strategic(data_s_200017, title = NULL, subtitle=NULL)
# anti_did_month_strategic_0017[[1]]

anti_did_month_strategic_1824 <- diff_month_strategic(data_s_201824, title = NULL, subtitle=NULL)
# anti_did_month_strategic_1824[[1]]

anti_did_month_strategic_num <- diff_month_strategic(data_s, outcome = "anti_don_number",
                                           title = NULL, subtitle=NULL, ylab = "Effect on Number Count")
# anti_did_month_strategic_num[[1]]

anti_did_month_strategic_1824num <- diff_month_strategic(data_s_201824, outcome = "anti_don_number",
                                                     title = NULL, subtitle=NULL, ylab = "Effect on Number Count")
# anti_did_month_strategic_1824num[[1]]

anti_did_month_strategic_0017num <- diff_month_strategic(data_s_200017, outcome = "anti_don_number",
                                                         title = NULL, subtitle=NULL, ylab = "Effect on Number Count")
# anti_did_month_strategic_0017num[[1]]



pro_did_month_strategic <- diff_month_strategic(data_s, outcome = "pro_log_sum",
                                      title = NULL, subtitle=NULL)
# pro_did_month_strategic[[1]]

pro_did_month_strategic_num <- diff_month_strategic(data_s, outcome = "pro_don_number",
                                           title = NULL, subtitle=NULL, ylab = "Effect on Number Count")
# pro_did_month_strategic_num[[1]]

## 4.8 Visualization -----------------------------------------------------------------

### 4.8.1 Double Plot -----------------------------------------------------

# Use diff_month_double_plot function
# no dot size

  
anti_SS_fatal_non_plot_1824 <- diff_month_double_plot(
  main_results = anti_did_month_baseline_short_201824[[2]], 
  second_results = anti_did_month_nonfatal_1824[[2]], title=NULL, subtitle = NULL,
  main_label = "Fatal School Shooting", second_label = "Non-fatal School Shooting") %>% print()

anti_SS_fatal_non_plot_0017 <- diff_month_double_plot(
  main_results = anti_did_month_baseline_short_200017[[2]], 
  second_results = anti_did_month_nonfatal_0017[[2]], title=NULL, subtitle = NULL,
  main_label = "Fatal", second_label = "Non-fatal") %>% print()

pro_SS_fatal_non_plot <- diff_month_double_plot(
  main_results = pro_did_month_baseline_short[[2]], 
  second_results = pro_did_month_nonfatal[[2]], title=NULL, subtitle = NULL,
  main_label = "Fatal School Shooting", second_label = "Non-fatal School Shooting") %>% print()


anti_SS_comp_0017_plot <- diff_month_double_plot(
  main_results = anti_did_month_baseline_short_200017[[2]], 
  second_results = anti_did_month_noncompetitive_0017[[2]], title=NULL, subtitle = NULL,
  main_label = "Competitive Districts", second_label = "Non-competitive Districts") %>% print()


### 4.8.2 all 4 plots ------------------------------------------------

# Use diff_month_four_plot function

pro_SS_allmass_plot <- diff_month_four_plot(
  pro_did_month_baseline_short[[2]], pro_did_month_MJ[[2]],
  pro_did_month_VPP[[2]],pro_did_month_GVA[[2]],
  main_label = "School Shooting", second_label = "MJ", third_label = "VPP",
  fourth_label = "GVA", title = NULL, subtitle = NULL) %>% print()

anti_SS_allmass_plot <- diff_month_four_plot(
  anti_did_month_baseline_short[[2]], anti_did_month_MJ[[2]],
  anti_did_month_VPP[[2]],anti_did_month_GVA[[2]],
  main_label = "School Shooting", second_label = "MJ", third_label = "VPP",
  fourth_label = "GVA", title = NULL, subtitle = NULL) %>% print()


### 4.8.3 Revised Plots with Two by Two Results -------

# treated (lightblue)
estimates_month_to_left_1 <- rev(pro_did_month_baseline_short[[2]]$estimate)
estimates_month_to_right_1 <- rev(anti_did_month_baseline_short_201824[[2]]$estimate)
treatednumber_left_1 <- rev(pro_did_month_baseline_short[[2]]$n_treated)
treatednumber_right_1 <- rev(anti_did_month_baseline_short_201824[[2]]$n_treated)
std_month_to_left_1 <- rev(pro_did_month_baseline_short[[2]]$std.error)
std_month_to_right_1 <- rev(anti_did_month_baseline_short_201824[[2]]$std.error)

# control (lightcoral)
estimates_month_to_left_2 <- rev(pro_did_month_noncompetitive[[2]]$estimate)
estimates_month_to_right_2 <- rev(anti_did_month_noncompetitive_1824[[2]]$estimate)
treatednumber_left_2 <- rev(pro_did_month_noncompetitive[[2]]$n_treated)
treatednumber_right_2 <- rev(anti_did_month_noncompetitive_1824[[2]]$n_treated)
std_month_to_left_2 <- rev(pro_did_month_noncompetitive[[2]]$std.error)
std_month_to_right_2 <- rev(anti_did_month_noncompetitive_1824[[2]]$std.error)

# left result
df_wide_month_to_left <- data.frame(
  horizon = c("All","12 months","8 months","4 months","3 months","2 months"),
  est_1 = estimates_month_to_left_1,
  se_1= std_month_to_left_1,
  est_2 = estimates_month_to_left_2,
  se_2 = std_month_to_left_2,
  treated_months_1 = treatednumber_left_1,
  treated_months_2 = treatednumber_left_2,
  outcome = "Pro-Gun PACs"
)

# Right result
df_wide_month_to_right <- data.frame(
  horizon = c("All","12 months","8 months","4 months","3 months","2 months"),
  est_1 = estimates_month_to_right_1,
  se_1= std_month_to_right_1,
  est_2 = estimates_month_to_right_2,
  se_2 = std_month_to_right_2,
  treated_months_1 = treatednumber_right_1,
  treated_months_2 = treatednumber_right_2,
  outcome = "Gun Control PACs (2018-2024)" 
)


df_wide_month_to_combined <- rbind(df_wide_month_to_left, df_wide_month_to_right)

df_long_month_to_combined <- df_wide_month_to_combined %>% 
  pivot_longer(cols = c(est_1, est_2), names_to = "condition_key", values_to = "estimate") |>
  mutate(
    se = ifelse(condition_key == "est_1", se_1, se_2),
    treated_months = ifelse(condition_key == "est_1", treated_months_1, treated_months_2),
    condition = ifelse(condition_key == "est_1", "Competitive", "Non-Competitive"), 
    horizon = factor(horizon, levels = rev(unique(horizon))),
    outcome = factor(outcome, levels = c("Pro-Gun PACs",  "Gun Control PACs (2018-2024)"))
  ) |>
  dplyr::select(horizon, estimate, se, treated_months, outcome, condition)


theme_pnas_monthto <- function(base_size = 13, base_family = "TNR") {
  theme_classic(base_size = base_size, base_family = base_family) %+replace%
    theme(
      strip.background   = element_rect(fill = "grey70", colour = "black", linewidth = .8),
      strip.text         = element_text(face = "plain", size = base_size - 3,
                                        margin = margin(t = 1.5, b = 1.5)),
      panel.border       = element_rect(colour = "black", fill = NA, linewidth = .7),
      panel.grid.major.x = element_line(colour = "grey80", linetype = "dotted", linewidth = .5),
      panel.grid.major.y = element_blank(),
      axis.ticks         = element_blank(),
      legend.position    = "right",
      legend.title       = element_text(size = base_size - 1),
      legend.text        = element_text(size = base_size - 2)
    )
}

plot_monthto_PNAS_combined <- ggplot(df_long_month_to_combined, aes(x = estimate, y = horizon, color = condition)) +
  geom_vline(xintercept = 0, colour = "grey45", linetype = "dashed", linewidth = .7) +
  geom_point(aes(size = treated_months), shape = 16, position = position_dodge(width = 0.3)) +
  geom_errorbarh(aes(xmin = estimate - 1.96 * se,
                     xmax = estimate + 1.96 * se),
                 height = .15, linewidth = 1.3, , position = position_dodge(width = 0.3)) +
  scale_color_manual(
    values = c("Competitive" = "lightblue", "Non-Competitive" = "lightcoral"),
    name = "District Type"
  ) +
  scale_size_continuous(
    range  = c(2, 6),
    breaks = c(50, 100, 150, 200, 250),
    name   = "# treated units"
  ) +
  facet_wrap(~ outcome, ncol = 2, scales = "free_x") +
  labs(x = "Effect on Logged Dollar Amount", y = "Months to the Next Election") +
  theme_pnas_monthto()

plot_monthto_PNAS_combined

# ggsave transformed format
# ggsave("plot_month_to_PNAS_combined.png", plot = plot_month_to_PNAS_combined,
#        width = 14, height = 12,
#        dpi = 1600,  
#        bg = "white",
#        type = "cairo")


# # old plot??
# plot_month_to_PNAS_combined <- ggplot(df_long_month_to_combined, 
#                                       aes(x = estimate, y = horizon, color = condition)) +
#   geom_vline(xintercept = 0, colour = "grey80", linetype = "dashed", linewidth = .7) +
#   geom_point(aes(size = treated_months), shape = 16, position = position_dodge(width = 0.3)) +
#   geom_errorbarh(aes(xmin = estimate - 1.96 * se, xmax = estimate + 1.96 * se),
#                  height = .15, linewidth = 1.3, position = position_dodge(width = 0.3)) +
#   scale_color_manual(
#     values = c("Treated" = "lightblue", "Control" = "lightcoral"),
#     name = "Condition"
#   ) +
#   scale_size_continuous(
#     range = c(2, 6),
#     breaks = c(50, 100, 150, 200, 250),
#     name = "# treated units"
#   ) +
#   facet_wrap(~ outcome, ncol = 2, scales = "free_x") +
#   labs(x = "Effect on Logged Dollar Amount", y = "Months to the Next Election") +
#   theme_minimal(base_size = 20, base_family = "TNR") +
#   theme(strip.background = element_rect(fill = "gray80", color = "gray80"),  
#         strip.text = element_text(face = "bold", color = "black", margin = margin(t = 1.5, b = 1.5)),
#         panel.border = element_blank(),  
#         panel.grid.major.x = element_line(colour = "gray80", linetype = "dotted", linewidth = .3),
#         panel.grid.major.y = element_blank(),
#         panel.grid.minor = element_blank(),
#         axis.ticks = element_blank(),
#         legend.position = "right"
#       ) 




## 4.9 Non-Grouped Combined Data Analysis ------------------------

# use data_month_to function to create dataset for month_to_TWFE function
# data_month_to function is used within month_to_TWFE function

# sample
etable(month_to_TWFE(data_s, "2"), month_to_TWFE(data_s,"2", "pro"))

# plot
anti_ss_month_to <- list(
  "<= 4 Month" = month_to_TWFE(data_s,"4"), # compare all
  "<= 3 Month" = month_to_TWFE(data_s,"3"),
  "<= 2 Month" = month_to_TWFE(data_s,"2"),
  "<= 4 or > 4 Month" = month_to_TWFE(data_s,"4", compare="treatment"), # to gauge additional impact
  "<= 3 or > 3 Month" = month_to_TWFE(data_s,"3", compare="treatment"),
  "<= 2 or > 2 Month" = month_to_TWFE(data_s,"2", compare="treatment")
) 

pro_ss_month_to <- list(
  "<= 4 Month" = month_to_TWFE(data_s,"4", PACtype = "pro"), # compare all
  "<= 3 Month" = month_to_TWFE(data_s,"3", PACtype = "pro"),
  "<= 2 Month" = month_to_TWFE(data_s,"2", PACtype = "pro"),
  "<= 4 or > 4 Month" = month_to_TWFE(data_s,"4", compare="treatment", PACtype = "pro"), # to gauge additional impact
  "<= 3 or > 3 Month" = month_to_TWFE(data_s,"3", compare="treatment", PACtype = "pro"),
  "<= 2 or > 2 Month" = month_to_TWFE(data_s,"2", compare="treatment", PACtype = "pro")
) 

coef_map_month_to <- c(
  # "incident_window_re" = "Shooting at any timing",
  
  "incident_window_re_4" = "Shooting within 4 months or closer to the next election",
  "incident_window_re_3" = "Shooting within 3 months or closer to the next election",
  "incident_window_re_2" = "Shooting within 2 months or closer to the next election",
  "incident_window_re_2others" = "Farther incidents",
  "incident_window_re_3others" = "Farther incidents",
  "incident_window_re_4others" = "Farther incidents")

# Add row
gof_add_month_to <- data.frame(
  raw = c("Treatment restrictions on Months to the next Election", "Control Group"),
  mod1 = c("4", "All other observations"),
  mod2 = c("3", "All other observations"),
  mod3 = c("2", "All other observations"),
  mod4 = c("4", "Farther incidents"),
  mod5 = c("3", "Farther incidents"),
  mod6 = c("2", "Farther incidents"),
  stringsAsFactors = FALSE
)

# Plot
anti_ss_month_to_plot <- gt_table(anti_ss_month_to, coef_map_month_to,c("nobs"),
                             gof_add_month_to,"Baseline DID with Different Timing to the Next Election",
                             "GVP; HoR") %>% print()


pro_ss_month_to_plot <- gt_table(pro_ss_month_to, coef_map_month_to,c("nobs"),
                                  gof_add_month_to,"Baseline DID with Different Timing to the Next Election",
                                  "Gun Lobby; HoR") %>% print()

## 4.10 Other datasets ------------------------------------------------

# need to remove random shootingness

# first, MJ dataset
mj_month_to <- list(
  "<= 4 or > 4 Month; GVP" = month_to_TWFE(data_m,"4", compare="treatment"),
  "<= 3 or > 3 Month; GVP" = month_to_TWFE(data_m,"3", compare="treatment"),
  "<= 2 or > 2 Month; GVP" = month_to_TWFE(data_m,"2", compare="treatment"),
  "<= 4 or > 4 Month; Gun Lobby" = month_to_TWFE(data_m,"4", compare="treatment", PACtype = "pro"),
  "<= 3 or > 3 Month; Gun Lobby" = month_to_TWFE(data_m,"3", compare="treatment", PACtype = "pro"),
  "<= 2 or > 2 Month; Gun Lobby" = month_to_TWFE(data_m,"2", compare="treatment", PACtype = "pro")
) 

mj_month_to_plot <- gt_table(mj_month_to, coef_map_month_to,c("nobs"),
                             gof_add_month_to,"MJ Dataset: Different Timing to the Next Election",
                             "Mass Shooting (MJ Dataset); GVP and Gun Lobby PAC") %>% print()

# Multi-collin
vpp_month_to <- list(
  "<= 4 or > 4 Month; GVP" = month_to_TWFE(data_v,"4", compare="treatment"),
  "<= 3 or > 3 Month; GVP" = month_to_TWFE(data_v,"3", compare="treatment"),
  "<= 2 or > 2 Month; GVP" = month_to_TWFE(data_v,"2", compare="treatment"),
  "<= 4 or > 4 Month; Gun Lobby" = month_to_TWFE(data_v,"4", compare="treatment", PACtype = "pro"),
  "<= 3 or > 3 Month; Gun Lobby" = month_to_TWFE(data_v,"3", compare="treatment", PACtype = "pro"),
  "<= 2 or > 2 Month; Gun Lobby" = month_to_TWFE(data_v,"2", compare="treatment", PACtype = "pro")
) 

vpp_month_to_plot <- gt_table(vpp_month_to, coef_map_month_to,c("nobs"),
                             gof_add_month_to,"VPP Dataset: Different Timing to the Next Election",
                             "Mass Shooting; GVP and Gun Lobby PAC") %>% print()
# Multi-collin
gva_month_to <- list(
  "<= 4 Month; GVP" = month_to_TWFE(data_v,"4"),
  "<= 3 Month; GVP" = month_to_TWFE(data_v,"3"),
  "<= 2 Month; GVP" = month_to_TWFE(data_v,"2"),
  "<= 4 Month; Gun Lobby" = month_to_TWFE(data_v,"4", PACtype = "pro"),
  "<= 3 Month; Gun Lobby" = month_to_TWFE(data_v,"3", PACtype = "pro"),
  "<= 2 Month; Gun Lobby" = month_to_TWFE(data_v,"2", PACtype = "pro")
) 

# gva_month_to_plot <- gt_table(gva_month_to, coef_map_month_to,c("nobs"),
#                               gof_add_month_to,"GVA Dataset: Different Timing to the Next Election",
#                               "Mass Shooting; GVP and Gun Lobby PAC") %>% print()


# 5 Fatalities (0/1/2) --------------------------------------------------------

# Comparing with impacts of shooting without killed victims

# Use data_baseline_fatal function for data wrangling; be noted to use whole unfiltered dataset, not s_2000.
data_s_fatal <- data_baseline_fatal(data_s) # not data_s_2000

# TWFE models
anti_TWFE_fatal_1over <- anti_pro_baseline_DID(data_s_fatal, treatment="incident_window_re_fatal_1over",
                                               more_controls = "incident_window_re_fatal_zero")
anti_TWFE_fatal_1over_number <- anti_pro_baseline_DID(data_s_fatal, outcome="anti_don_number",
                                                      treatment="incident_window_re_fatal_1over",
                                                      more_controls = "incident_window_re_fatal_zero")
anti_TWFE_fatal_2over <- anti_pro_baseline_DID(data_s_fatal, treatment="incident_window_re_fatal_zero",
                                           more_controls = "incident_window_re_fatal_1+ incident_window_re_fatal_2over")

anti_TWFE_fatal_2over_number <- anti_pro_baseline_DID(data_s_fatal, outcome="anti_don_number",
                                                  treatment="incident_window_re_fatal_zero",
                                                  more_controls = "incident_window_re_fatal_1+incident_window_re_fatal_2over")

# gun lobby
pro_TWFE_fatal_1over <- anti_pro_baseline_DID(data_s_fatal, 
                                          outcome = "pro_log_sum",
                                          treatment="incident_window_re_fatal_1over",
                                          more_controls = "incident_window_re_fatal_zero")
pro_TWFE_fatal_1over_number <- anti_pro_baseline_DID(data_s_fatal, outcome="pro_don_number",
                                                     treatment="incident_window_re_fatal_1over",
                                                     more_controls = "incident_window_re_fatal_zero")
pro_TWFE_fatal_2over <- anti_pro_baseline_DID(data_s_fatal,  
                                          outcome = "pro_log_sum",
                                          treatment="incident_window_re_fatal_zero",
                                           more_controls = "incident_window_re_fatal_1+ incident_window_re_fatal_2over")

pro_TWFE_fatal_2over_number <- anti_pro_baseline_DID(data_s_fatal, outcome="pro_don_number",
                                                  treatment="incident_window_re_fatal_zero",
                                                  more_controls = "incident_window_re_fatal_1+incident_window_re_fatal_2over")



## 5.1 plot ---------------------------------------------------

# List
anti_ss_fatal <- list(
  "0 or >=1" = anti_TWFE_fatal_1over,
  "0 or >=1 (Number outcome)" = anti_TWFE_fatal_1over_number,
  "0, 1, or >= 2" = anti_TWFE_fatal_2over,
  "0, 1, or >= 2 (Number outcome)" = anti_TWFE_fatal_2over_number) 

pro_ss_fatal <- list(
  "0 or >=1" = pro_TWFE_fatal_1over,
  "0 or >=1 (Number outcome)" = pro_TWFE_fatal_1over_number,
  "0, 1, or >= 2" = pro_TWFE_fatal_2over,
  "0, 1, or >= 2 (Number outcome)" = pro_TWFE_fatal_2over_number) 

coef_map_fatal <- c(
  "incident_window_re_fatal_zero" = "Incidents with no killed",
  "incident_window_re_fatal_1over" = "Incidents with at least 1 killed",
  "incident_window_re_fatal_1" = "Incidents with exactly 1 killed",
  "incident_window_re_fatal_2over"="Incidents with 2 or over killed")

# Add row
gof_add_fatal <- data.frame(
  raw = c("Fatality Dummy","Outcome"),
  mod1 = c("0 or >=1","Dollar"),
  mod2 = c("0 or >=1","Number"),
  mod3 = c("0, 1, or >=2","Dollar"),
  mod4 = c("0, 1, or >=2","Number"),
  stringsAsFactors = FALSE
)

# Plot
anti_ss_fatal_plot <- gt_table(anti_ss_fatal, coef_map_fatal,c("nobs"),
                          gof_add_fatal,"Fatalities at Baseline DID",
                          "GVP; School Shooting; HoR") %>% print()


pro_ss_fatal_plot <- gt_table(pro_ss_fatal, coef_map_fatal,c("nobs"),
                               gof_add_fatal,"Fatalities at Baseline DID",
                               "Gun Lobby; School Shooting; HoR") %>% print()


# 6. Different month scope ---------------------------------------------

# Use data_flexible function, use unfiltered dataset, instead of minimal filtering

range12 <- data_flexible(data_s, month_scope=12)
range24 <- data_flexible(data_s, month_scope=24)
range36 <- data_flexible(data_s, month_scope=36)
range48 <- data_flexible(data_s, month_scope=48)
range60 <- data_flexible(data_s, month_scope=60)

# treatment range
trange_list <- list(range12, range24, range36, range48, range60)
anti_trange_results <- purrr::map(trange_list, ~anti_pro_baseline_DID(.))
pro_trange_results <- purrr::map(trange_list, ~anti_pro_baseline_DID(., outcome = "pro_log_sum"))

names(anti_trange_results) <- names(pro_trange_results) <- c("1 Year","2 Years (Basic Pattern)","3 Years","4 Years","5 Years") 

gof_add_range <- data.frame(
  raw = c("Treatment Range"),
  mod1 = c("1 year"),
  mod2 = c("2 years"),
  mod3 = c("3 years"),
  mod4 = c("4 years"),
  mod5 = c("5 years"),
  stringsAsFactors = FALSE
)

anti_trange_result <- gt_table(anti_trange_results, # results of baseline DID
                          coef_map_single,c("nobs"),gof_add_range, 
                          "Different Treated Ranges at Baseline DID", 
                          "GVP; School Shooting; HoR") %>% print()

pro_trange_result <- gt_table(pro_trange_results, 
                               coef_map_single,c("nobs"),gof_add_range, 
                               "Different Treated Ranges at Baseline DID", 
                               "Gun Lobby; School Shooting; HoR") %>% print()


# 7 Mass Shooting 3 datasets -----------------------------------------

# check dataset
table(data_g_margin_baseline$fatalities)

anti_VPP_base_twfe <- anti_pro_baseline_DID(data_v_margin_baseline)
anti_VPP_base_twfe_num <- anti_pro_baseline_DID(data_v_margin_baseline, outcome="anti_don_number")

anti_MJ_base_twfe <- anti_pro_baseline_DID(data_m_margin_baseline)
anti_MJ_base_twfe_num <- anti_pro_baseline_DID(data_m_margin_baseline, outcome="anti_don_number")

anti_GVA_base_twfe <- anti_pro_baseline_DID(data_g_margin_baseline)
anti_GVA_base_twfe_num <- anti_pro_baseline_DID(data_g_margin_baseline, outcome="anti_don_number")

pro_VPP_base_twfe <- anti_pro_baseline_DID(data_v_margin_baseline, outcome="pro_log_sum")
pro_VPP_base_twfe_num <- anti_pro_baseline_DID(data_v_margin_baseline, outcome="pro_don_number")

pro_MJ_base_twfe <- anti_pro_baseline_DID(data_m_margin_baseline, outcome="pro_log_sum")
pro_MJ_base_twfe_num <- anti_pro_baseline_DID(data_m_margin_baseline, outcome="pro_don_number")

pro_GVA_base_twfe <- anti_pro_baseline_DID(data_g_margin_baseline, outcome="pro_log_sum")
pro_GVA_base_twfe_num <- anti_pro_baseline_DID(data_g_margin_baseline, outcome="pro_don_number")

anti_mass_shooting_list <- list(
  "VPP; Baseline DID"=anti_VPP_base_twfe,
  "VPP; Number Outcome"=anti_VPP_base_twfe_num,
  "MJ; Baseline DID"=anti_MJ_base_twfe,
  "MJ; Number Outcome"=anti_MJ_base_twfe_num,
  "GVA; Baseline DID"=anti_GVA_base_twfe,
  "GVA; Number Outcome"=anti_GVA_base_twfe_num)

pro_mass_shooting_list <- list(
  "VPP; Baseline DID"=pro_VPP_base_twfe,
  "VPP; Number Outcome"=pro_VPP_base_twfe_num,
  "MJ; Baseline DID"=pro_MJ_base_twfe,
  "MJ; Number Outcome"=pro_MJ_base_twfe_num,
  "GVA; Baseline DID"=pro_GVA_base_twfe,
  "GVA; Number Outcome"=pro_GVA_base_twfe_num)

gof_add_mass <- data.frame(
  raw = c("Dataset","Outcome"),
  mod1 = c("VPP", "Dollar"),
  mod2 = c("VPP","Number"),
  mod3 = c("MJ", "Dollar"),
  mod4 = c("MJ","Number"),
  mod5 = c("GVA", "Dollar"),
  mod6 = c("GVA","Number"),
  stringsAsFactors = FALSE
)

anti_mass_table <- gt_table(anti_mass_shooting_list, coef_map_single,c("nobs"),
                       gof_add_mass,"Mass Shooting Baseline DID Comparison",
                       "VPP/MJ/GVA Datasets; GVP") %>% print()

pro_mass_table <- gt_table(pro_mass_shooting_list, coef_map_single,c("nobs"),
                            gof_add_mass,"Mass Shooting Baseline DID Comparison",
                            "VPP/MJ/GVA Datasets; Gun Lobby") %>% print()

# 8 State-wide election  -----------------------------------------------------------

# Due to gap of covariates, such as voting margin, use baseline_DID_nonHoR function

anti_USS_base_twfe_dol <- baseline_DID_nonHoR(data_snt_baseline)
anti_gov_base_twfe_dol <- baseline_DID_nonHoR(data_gov_baseline)
anti_atg_base_twfe_dol <- baseline_DID_nonHoR(data_atg_baseline)

anti_USS_base_twfe_num <- baseline_DID_nonHoR(data_snt_baseline, outcome = "anti_don_number")
anti_gov_base_twfe_num <- baseline_DID_nonHoR(data_gov_baseline, outcome = "anti_don_number")
anti_atg_base_twfe_num <- baseline_DID_nonHoR(data_atg_baseline, outcome = "anti_don_number")

pro_USS_base_twfe_dol <- baseline_DID_nonHoR(data_snt_baseline, outcome = "pro_log_sum")
pro_gov_base_twfe_dol <- baseline_DID_nonHoR(data_gov_baseline, outcome = "pro_log_sum")
pro_atg_base_twfe_dol <- baseline_DID_nonHoR(data_atg_baseline, outcome = "pro_log_sum")

pro_USS_base_twfe_num <- baseline_DID_nonHoR(data_snt_baseline, outcome = "pro_don_number")
pro_gov_base_twfe_num <- baseline_DID_nonHoR(data_gov_baseline, outcome = "pro_don_number")
pro_atg_base_twfe_num <- baseline_DID_nonHoR(data_atg_baseline, outcome = "pro_don_number")



anti_state_list <- list(
  "US Senator; Baseline"=anti_USS_base_twfe_dol,
  "US Senator; Number Outcome"=anti_USS_base_twfe_num,
  "Govenor; Baseline"=anti_gov_base_twfe_dol,
  "Govenor outcome: number"=anti_gov_base_twfe_num,
  "State Attorney General; Baseline"= anti_atg_base_twfe_dol,
  "State Attorney General; Number Outcome"=anti_atg_base_twfe_num)


pro_state_list <- list(
  "US Senator; Baseline"=pro_USS_base_twfe_dol,
  "US Senator; Number Outcome"=pro_USS_base_twfe_num,
  "Govenor; Baseline"=pro_gov_base_twfe_dol,
  "Govenor outcome: number"=pro_gov_base_twfe_num,
  "State Attorney General; Baseline"= pro_atg_base_twfe_dol,
  "State Attorney General; Number Outcome"=pro_atg_base_twfe_num)



gof_add_state <- data.frame(
  raw = c("Recipient","Outcome"),
  mod1 = c("US Senator", "Logged Dollar"),
  mod2 = c("US Senator","Number"),
  mod3 = c("Govenor","Logged Dollar"),
  mod4 = c("Govenor","Number"),
  mod5 = c("State Attorney General", "Logged Dollar"),
  mod6 = c("State Attorney General", "Number"),
  stringsAsFactors = FALSE
)

anti_state_table <- gt_table(anti_state_list, coef_map_single,c("nobs"),
                            gof_add_state,"Baseline DID for State-wide Elected Offices; Gun Control PAC",
                            "US Senators and State Offices; All Districts", 
                            source_note = state_sourcenote) %>% print()

pro_state_table <- gt_table(pro_state_list, coef_map_single,c("nobs"),
                             gof_add_state,"Baseline DID for State-wide Elected Offices; Gun Lobby PAC",
                             "US Senators and State Offices; All Districts", 
                             source_note = state_sourcenote) %>% print()



# 9 pre-trend and placebo test -----------------------------------------------------

# check help(fect)
# for plot function, see below
# https://www.rdocumentation.org/packages/fect/versions/1.0.0/topics/plot.fect

# TOST here uses 90% CI.

## 9.1 Simple event study for pre-trend ---------------------------------------------

# Prior to placebo, wanted to try simple event study for pre-treatment period
# Use simple_pre_event function

simple_pre_event(data_s_margin_baseline,
                      outcome="anti_log_sum_mva_logav5", past=-12)

simple_pre_event(data_s_margin_baseline,
                      outcome="pro_log_sum_mva_logav5", past=-12)


## 9.2 placebo test ---------------------------------------------------

# Use placebo_fe function

placebo_fe(data_s_margin_baseline, 
           "incident_before0",  # the month of incidents
           outcome ="anti_log_sum_mva_logav5",
           title = ""
           , parallel=TRUE  # for laptop analysis
           , ylim = c(-0.2, 0.4), start=-12, placebo_start = -6, end = -1
)


placebo_fe(data_s_margin_baseline, 
           "incident_before0",  # the month of incidents
           outcome ="pro_log_sum_mva_logav5",
           title = ""
           , parallel=TRUE  # for laptop analysis
           , ylim = c(-0.45, 0.65), start=-12, placebo_start = -6, end = -1
)


# number outcome
placebo_fe(data_s_margin_baseline, 
           "incident_before0",  
           outcome ="anti_don_number_mva_5",
           title = ""
           , ylim = c(-0.03, 0.05), start=-12, placebo_start = -6, end = -1,
           ylab = "Number Count"
)

placebo_fe(data_s_margin_baseline, 
           "incident_before0",  
           outcome ="pro_don_number_mva_5",
           title = ""
           , ylim = c(-0.07, 0.1), start=-12, placebo_start = -6, end = -1,
           ylab = "Number Count"
)


# 10 State-unique trend ------------------------------------------

# Use state_trend_DID function
# Default; quadratic; state-FE; state clustering; state interaction for state-specific time trend; with cov

anti_state_trend_s <- state_trend_DID(data_s_margin_baseline) 
pro_state_trend_s <- state_trend_DID(data_s_margin_baseline, outcome = "pro_log_sum") 

anti_state_trend_s_noc <- state_trend_DID(data_s_margin_baseline, covariate = FALSE) 
pro_state_trend_s_noc <- state_trend_DID(data_s_margin_baseline, outcome = "pro_log_sum", covariate = FALSE) 

anti_state_trend_s_lin <- state_trend_DID(data_s_margin_baseline, quadratic = FALSE) 
pro_state_trend_s_lin <- state_trend_DID(data_s_margin_baseline, outcome = "pro_log_sum", quadratic = FALSE)

anti_state_trend_s_idFE <- state_trend_DID(data_s_margin_baseline, unitFE = "state_num") 
pro_state_trend_s_idFE <- state_trend_DID(data_s_margin_baseline, outcome = "pro_log_sum", unitFE = "state_num") 

anti_state_trend_s_num <- state_trend_DID(data_s_margin_baseline, outcome = "anti_don_number") 
pro_state_trend_s_num <- state_trend_DID(data_s_margin_baseline, outcome = "pro_don_number") 



trend_sourcenote <- "Includes interaction terms between states and squared time (or time)."

anti_trend_state_list <- list("Baseline (State Interaction)"=anti_state_trend_s,
                         "Number Outcome"=anti_state_trend_s_num,
                         "No Covariates"=anti_state_trend_s_noc,
                         "Linear Interaction"=anti_state_trend_s_lin,
                         "District Unit FE" = anti_state_trend_s_idFE) 

pro_trend_state_list <- list("Baseline (State Interaction)"=pro_state_trend_s,
                              "Number Outcome"=pro_state_trend_s_num,
                              "No Covariates"=pro_state_trend_s_noc,
                              "Linear Interaction"=pro_state_trend_s_lin,
                              "District Unit FE" = pro_state_trend_s_idFE) 


gof_add_trend <- data.frame(
  raw = c("Outcome", "Covariate", "Time Trend per Unit", "Unit FE","Clustering"),
  mod1 = c("Log Dollar","Yes", "Quadratic", "State", "State"),
  mod2 = c("Number","Yes", "Quadratic", "State", "State"),
  mod3 = c("Log Dollar","No", "Quadratic", "State", "State"),
  mod4 = c("Log Dollar","Yes", "Linear", "State", "State"),
  mod5 = c("Log Dollar","Yes", "Quadratic", "District", "State"),
  stringsAsFactors = FALSE
)

anti_state_trend <- gt_table(anti_trend_state_list,coef_map_single,  c("nobs"),
         gof_add_trend, "DID Result (Unique Time Trend per State)",
         "Gun Control PAC; School Shooting; HoR",
         source_note = trend_sourcenote) %>% print()

gt_table(pro_trend_state_list,coef_map_single,  c("nobs"),
         gof_add_trend, "DID Result (Unique Time Trend per State)",
         "Pro-gun PAC; School Shooting; HoR",
         source_note = trend_sourcenote)



# 11. (Removed) -----------------------------------------------------------



# 12 Interactive FE ---------------------------------------------------

# see https://yiqingxu.org/packages/fect/04-gsynth.html#comparing-w-ifect-mc
# Again, month to election (and id) should be removed

# It takes time to excute; when not used, comment out

## 12.1 static ---------------------------------------------------------

# Use ife_s_static function; use r=c(0) after the first trial; at first, try r=c(0,2)
# r refers to potential numbers of unobserved confounders
# MSPE is used for criterion to choose r.

# anti_ife <- ife_s_static(data_s_margin_baseline, parallel = FALSE
#                          # , r=c(0,2)
#                          )
# r = 0; sigma2 = 0.62908; IC = 0.10376; PC = 0.58933; MSPE = 0.58906
# r = 1; sigma2 = 0.26563; IC = -0.20137; PC = 0.31151; MSPE = 0.62149
# r = 2; sigma2 = 0.11144; IC = -0.51523; PC = 0.15701; MSPE = 0.69383

# anti_num_ife <- ife_s_static(data_s_margin_baseline, "anti_don_number",
#                              parallel = TRUE
#                              # , r=c(0,2)
#                              )
# r = 0; sigma2 = 0.00441; IC = -4.85767; PC = 0.00413; MSPE = 0.00341
# r = 1; sigma2 = 0.00217; IC = -5.01040; PC = 0.00254; MSPE = 0.00443
# r = 2; sigma2 = 0.00094; IC = -5.29504; PC = 0.00132; MSPE = 0.00417

# pro_ife <- ife_s_static(data_s_margin_baseline, "pro_log_sum",
#              parallel = TRUE
#              # , r=c(0,2)
#              )
# r = 0; sigma2 = 1.22501; IC = 0.77019; PC = 1.14759; MSPE = 1.01796
# r = 1; sigma2 = 0.61678; IC = 0.64104; PC = 0.72332; MSPE = 1.09900
# r = 2; sigma2 = 0.35087; IC = 0.63171; PC = 0.49434; MSPE = 1.82587

# pro_num_ife <- ife_s_static(data_s_margin_baseline, "pro_don_number",
#              parallel = TRUE
#              # , r=c(0,2)
#              )
# r = 0; sigma2 = 0.01213; IC = -3.84473; PC = 0.01136; MSPE = 0.01058
# r = 1; sigma2 = 0.00619; IC = -3.96080; PC = 0.00726; MSPE = 0.01083
# r = 2; sigma2 = 0.00354; IC = -3.96512; PC = 0.00498; MSPE = 0.01681


# ife_static_list <- list("GVP; Dollar Outcome" = anti_ife,
#                         "GVP; Number Outcome"= anti_num_ife,
#                         "Gun Lobby; Dollar Outcome" = pro_ife,
#                         "Gun Lobby; Number Outcome"= pro_num_ife)



## 12.2 gt and modelsummary plot -----------------------------------------------------------

# first, check structure
# ife_static_list[[1]]
# str(ife_static_list[[1]])
# ife_static_list[[1]]$est.avg

# extract ATT info, est.avg
ife_att_tbl <- imap_dfr(ife_static_list, ~ {
  att <- .x$est.avg
  tibble(  Outcome = .y,
           term    = "incident_window_re",  estimate = att[1, "ATT.avg"], se       = att[1, "S.E."],
           lower    = att[1, "CI.lower"],  upper    = att[1, "CI.upper"],  p.value  = att[1, "p.value"] )
})

# for modelsummary readable
# register into S3 class
setOldClass("fect")

tidy.fect <- function(x, ...) {
  df <- x$est.avg
  tibble::tibble(
    term      = "incident_window_re",   estimate  = df[1, "ATT.avg"],
    std.error = df[1, "S.E."], p.value   = df[1, "p.value"]
  )
}
# into broom
registerS3method("tidy", "fect", tidy.fect, envir = asNamespace("broom"))

# too long???
# gof_add_ife <- data.frame(
#   raw = c("Outcome", "PAC Type", "Optimal # of Unobserved Covariates", "MSPE"),
#   mod1 = c("Dollar", "Gun Control", "0", "0.58906 (0.62149 for one confounder)"),
#   mod2 = c("Number", "Gun Control","0", "0.00341 (0.004430 for one confounder)"),
#   mod3 = c("Dollar", "Gun Lobby","0", "1.01796 (1.099000 for one confounder)"),
#   mod4 = c("Number", "Gun Lobby","0", "0.01058 (0.01083 for one confounder)"),
#   stringsAsFactors = FALSE
# )

gof_add_ife <- data.frame(
  raw = c("Outcome", "PAC Type", "Optimal # of Unobserved Covariates", "MSPE"),
  mod1 = c("Dollar", "Gun Control", "0", "0.59 (0.62 for one confounder)"),
  mod2 = c("Number", "Gun Control","0", "0.0034 (0.0044 for one confounder)"),
  mod3 = c("Dollar", "Gun Lobby","0", "1.02 (1.10 for one confounder)"),
  mod4 = c("Number", "Gun Lobby","0", "0.0106 (0.0108 for one confounder)"),
  stringsAsFactors = FALSE
)

IFE_static_results <- gt_table(
  model           = ife_static_list,
  coef_mapping    = coef_map_single,
  gof_mapping     = c("nobs"),
  add_row_data    = gof_add_ife,
  title           = "Two-Way Interactive FE DID",
  subtitle        = "School Shooting; HoR; Gun Control and Gun Lobby PACs",
  source_note     = ss_sourcenote,
  collin = FALSE  # did not work
) %>% print()


## 12.3 event study ----------------------------------------------------


anti_ife_s_event <- fect(anti_log_sum ~
                      # incident_window_re
                      incident_before0 # the month of incidents; already confirmed at datasets
                    + bachelor + black + white + unemployment+ log_income + rep_incumbent_before + gun_ratio_pct
                    ,data = data_s_margin_baseline, index = c("id","time"),
                    force = "two-way", method = "ife", CV = TRUE,
                    r = c(0), se = TRUE, nboots = 200, seed=123, na.rm = TRUE
                    , parallel = TRUE
)


plot(anti_ife_s_event, count = FALSE,xlim=c(-18, -1), xlab = "Month Relative to Incidents",
     ylab = "Logged Dollar Amount",
     # main="Interactive FE (School Shooting; HoR; With Covariates)"
     main=''
)

pro_ife_s_event <- fect(pro_log_sum ~
                           incident_before0 # the month of incidents; already confirmed at datasets
                         + bachelor + black + white + unemployment+ log_income + rep_incumbent_before + gun_ratio_pct
                         ,data = data_s_margin_baseline, index = c("id","time"),
                         force = "two-way", method = "ife", CV = TRUE,
                         r = c(0), se = TRUE, nboots = 200, seed=123, na.rm = TRUE
                         , parallel = TRUE
)

plot(pro_ife_s_event, count = FALSE,xlim=c(-18, -1), xlab = "Month Relative to Incidents",
     ylab = "Logged Dollar Amount",
     # main="Interactive FE (School Shooting; HoR; With Covariates)"
     main=''
)


anti_ife_s_event_noc <- fect(anti_log_sum ~ incident_before0
                        # +incident_before2+incident_before3+incident_before4+incident_before5+incident_before6
                        ,data = data_s_margin_baseline, index = c("id","time"),
                        force = "two-way", method = "ife", CV = TRUE,
                        r = c(0), se = TRUE, nboots = 200, seed=123, na.rm = TRUE
                        , parallel = TRUE
)

plot(anti_ife_s_event_noc, count = FALSE,xlim=c(-18, -1), xlab = "Month Relative to Incidents",
     ylab = "Logged Dollar Amount",
     # main="Interactive FE (School Shooting; HoR; No Covariates)"
     main=""
)

# no covariate pattern
anti_ife_s_event_noc <- fect(anti_log_sum ~ incident_before0
                             ,data = data_s_margin_baseline, index = c("id","time"),
                             force = "two-way", method = "ife", CV = TRUE,
                             r = c(0), se = TRUE, nboots = 200, seed=123, na.rm = TRUE
                             , parallel = TRUE
)

plot(anti_ife_s_event_noc, count = FALSE,xlim=c(-18, -1), xlab = "Month Relative to Incidents",
     ylab = "Logged Dollar Amount",
     # main="Interactive FE (School Shooting; HoR; No Covariates)"
     main=""
)

pro_ife_s_event_noc <- fect(pro_log_sum ~ incident_before0
                             ,data = data_s_margin_baseline, index = c("id","time"),
                             force = "two-way", method = "ife", CV = TRUE,
                             r = c(0), se = TRUE, nboots = 200, seed=123, na.rm = TRUE
                             , parallel = TRUE
)

plot(pro_ife_s_event_noc, count = FALSE,xlim=c(-18, -1), xlab = "Month Relative to Incidents",
     ylab = "Logged Dollar Amount",
     # main="Interactive FE (School Shooting; HoR; No Covariates)"
     main=""
)



# 13. Many fatalities and within-state impact ---------------------------------------------------

# Use data_withinstate function, using group by state
# Already excluded purely treated districts from treated variables

data_s_withinstate <- data_withinstate(data_s) %>% 
  # also have to exclude purely-treated districts from dataset
  filter(incident_window_re==0)

# 1 and more
# use incident_window_re_state variable for treatment
anti_many_fatal_within_1state <- anti_pro_baseline_DID(data_s_withinstate,
                                                 outcome = "anti_log_sum",
                                                 treatment = " incident_window_re_state",
                                                 cluster = "state_num") 

anti_many_fatal_within_1id <- anti_pro_baseline_DID(data_s_withinstate,
                                                       outcome = "anti_log_sum",
                                                       treatment = " incident_window_re_state")  

pro_many_fatal_within_1state <- anti_pro_baseline_DID(data_s_withinstate,
                                                       outcome = "pro_log_sum",
                                                       treatment = " incident_window_re_state",
                                                       cluster = "state_num") 

pro_many_fatal_within_1id <- anti_pro_baseline_DID(data_s_withinstate,
                                                    outcome = "pro_log_sum",
                                                    treatment = " incident_window_re_state") 

# 5 or more fatalities; use incident_window_re_state5am and incident_window_re_state1_4
anti_many_fatal_within_5state <- anti_pro_baseline_DID(data_s_withinstate,
                                                       outcome = "anti_log_sum",
                                                       treatment = "incident_window_re_state5am",
                                                       more_controls = "incident_window_re_state1_4",
                                                       cluster = "state_num") 

anti_many_fatal_within_5id <- anti_pro_baseline_DID(data_s_withinstate,
                                                    outcome = "anti_log_sum",
                                                    treatment = " incident_window_re_state5am",
                                                    more_controls = "incident_window_re_state1_4")  

pro_many_fatal_within_5state <- anti_pro_baseline_DID(data_s_withinstate,
                                                      outcome = "pro_log_sum",
                                                      treatment = " incident_window_re_state5am",
                                                      more_controls = "incident_window_re_state1_4",
                                                      cluster = "state_num") 

pro_many_fatal_within_5id <- anti_pro_baseline_DID(data_s_withinstate,
                                                   outcome = "pro_log_sum",
                                                   treatment = " incident_window_re_state5am",
                                                   more_controls = "incident_window_re_state1_4") 

# 10 or more fatalities; use incident_window_re_state10am and incident_window_re_state1_4 + 5_9
anti_many_fatal_within_10state <- anti_pro_baseline_DID(data_s_withinstate,
                                                       outcome = "anti_log_sum",
                                                       treatment = "incident_window_re_state10am",
                                                       more_controls = "incident_window_re_state1_4+incident_window_re_state5_9",
                                                       cluster = "state_num") 

anti_many_fatal_within_10id <- anti_pro_baseline_DID(data_s_withinstate,
                                                    outcome = "anti_log_sum",
                                                    treatment = " incident_window_re_state10am",
                                                    more_controls = "incident_window_re_state1_4+incident_window_re_state5_9")  

pro_many_fatal_within_10state <- anti_pro_baseline_DID(data_s_withinstate,
                                                      outcome = "pro_log_sum",
                                                      treatment = " incident_window_re_state10am",
                                                      more_controls = "incident_window_re_state1_4+incident_window_re_state5_9",
                                                      cluster = "state_num") 

pro_many_fatal_within_10id <- anti_pro_baseline_DID(data_s_withinstate,
                                                   outcome = "pro_log_sum",
                                                   treatment = " incident_window_re_state10am",
                                                   more_controls = "incident_window_re_state1_4+incident_window_re_state5_9") 



anti_withinstate_list <- list(
  '>=1 Fatality'= anti_many_fatal_within_1id, 
  '>=1 Fatality; State Clustering'=anti_many_fatal_within_1state, 
  '>=5 Fatalities'=anti_many_fatal_within_5id,
  '>=5 Fatalities; State Clustering'=anti_many_fatal_within_5state, 
  '>=10 Fatalities'=anti_many_fatal_within_10id,
  '>=10 Fatalities; State Clustering'=anti_many_fatal_within_10state)

pro_withinstate_list <- list(
  '>=1 Fatality'=pro_many_fatal_within_1id, 
  '>=1 Fatality; State Clustering'=pro_many_fatal_within_1state, 
  '>=5 Fatalities'=pro_many_fatal_within_5id,
  '>=5 Fatalities; State Clustering'=pro_many_fatal_within_5state, 
  '>=10 Fatalities'=pro_many_fatal_within_10id,
  '>=10 Fatalities; State Clustering'=pro_many_fatal_within_10state)


coef_map_withinstate <- c(
  "incident_window_re_state" = ">=1 fatality",
  "incident_window_re_state1_4" = "1~4 fatalities",
  "incident_window_re_state5am" = ">=5 fatalities",
  "incident_window_re_state5_9" = "5~9 fatalities",
  "incident_window_re_state10am" = ">=10 fatalities")
# "incident_window_re_state1_4v" = "1~4 victims",
# "incident_window_re_state5vam" = ">=5 victims",
# "incident_window_re_state5_9v" = "5~9 victims",
# "incident_window_re_state10vam" = ">=10 victims")
# 
# coef_map_withinstatev <- c(
#   "incident_window_re_state" = ">=1 victim",
#   "incident_window_re_state1_4v" = "1~4 victims",
#   "incident_window_re_state5vam" = ">=5 victims",
#   "incident_window_re_state5_9v" = "5~9 victims",
#   "incident_window_re_state10vam" = ">=10 victims")

gof_add_withinstate <- data.frame(
  raw = c("Fatality Range", "Clustering Unit"),
  mod1 = c("1-", "District"),  
  mod2 = c("1-", "State"),
  mod3 = c("1-4, 5-", "District"),
  mod4 = c("1-4, 5-", "State"),
  mod5 = c("1-4, 5-9, 10-", "District"),
  mod6 = c("1-4, 5-9, 10-", "State"),
  stringsAsFactors = FALSE
)


anti_withinstate_plot <- gt_table(anti_withinstate_list,coef_map_withinstate,  c("nobs"),
                                  title = "State-wide Impact of Many-fatalities Cases; Gun Control PAC", 
                                  subtitle = "School Shooting; Competitive Districts" ) %>% print()
                             

pro_withinstate_plot <- gt_table(pro_withinstate_list,coef_map_withinstate,  c("nobs"),
                                 title = "State-wide Impact of Many-fatalities Cases; Pro-gun PAC", 
                                 subtitle = "School Shooting; Competitive Districts" ) %>% print()











withinstate_plotv <- gt_table(withinstate_listv,coef_map_withinstatev,  c("nobs"),
                              NULL,
                              "State-wide Impact of Many-victims Cases", "School Shooting; HoR", 
                              source_note = ss_sourcenote, collin=TRUE) 

withinstate_plot
withinstate_plotv







withinstate_listv <- list(
  '>=1 Victim (and >= 1 fatality)'=many_fatal_within1, 
  '>=5 Victims (and >= 1 fatality)'=many_fatal_within5v,
  '>=10 Victims (and >= 1 fatality)'=many_fatal_within10v)

# many_fatal_within10only <- fixest::feols(log_sum~
#                                        incident_window_re_state+
#                                        # incident_window_re+
#                                        # incident_window_re_state5+
#                                        # incident_window_re_state5v+
#                                        incident_window_re_state10+
#                                        # incident_window_re_state10v+
#                                        bachelor + black + white
#                                      + unemployment + log_income + rep_incumbent_before + gun_ratio_pct
#                                      + incident_window_re_random
#                                      |id + time, data = anti_pro_data_s_withinstate5
#                                      , cluster = "id"
#                                      # , cluster = "state"
# )

many_fatal_within5v <- fixest::feols(log_sum~
                                       incident_window_re_state1_4v+
                                       # incident_window_re+
                                       # incident_window_re_state5+
                                       incident_window_re_state5vam+
                                       bachelor + black + white
                                     + unemployment + log_income + rep_incumbent_before + gun_ratio_pct 
                                     # + incident_window_re_random
                                     |id + time, data = anti_pro_data_s_withinstate5
                                     , cluster = "id"
)

many_fatal_within10v <- fixest::feols(log_sum~
                                        incident_window_re_state1_4v+
                                        # incident_window_re+
                                        # incident_window_re_state5+
                                        incident_window_re_state5_9v+
                                        # incident_window_re_state10+
                                        incident_window_re_state10vam+
                                        bachelor + black + white
                                      + unemployment + log_income + rep_incumbent_before + gun_ratio_pct
                                      # + incident_window_re_random
                                      |id + time, data = anti_pro_data_s_withinstate5
                                      , cluster = "id"
)



# 14. clustering ----------------------------------------------

# Be noted that "cluster = NULL" does not work.
# Use clustering_TWFE function

anti_SS_base_twfe_dol_nocl <- clustering_TWFE(data_s_margin_baseline, "iid") 
anti_SS_base_twfe_dol_idfe <- clustering_TWFE(data_s_margin_baseline, "id",TRUE)
anti_SS_base_twfe_dol_statefe <- clustering_TWFE(data_s_margin_baseline, "state",TRUE)
anti_SS_base_twfe_dol_idtime <- clustering_TWFE(data_s_margin_baseline, "id+time", TRUE) 
anti_SS_base_twfe_dol_statetime <- clustering_TWFE(data_s_margin_baseline, "state+time", TRUE)

pro_SS_base_twfe_dol_nocl <- clustering_TWFE(data_s_margin_baseline, "iid", PACtype = "pro")
pro_SS_base_twfe_dol_idfe <- clustering_TWFE(data_s_margin_baseline, "id",TRUE, PACtype = "pro")
pro_SS_base_twfe_dol_statefe <- clustering_TWFE(data_s_margin_baseline, "state",TRUE, PACtype = "pro")
pro_SS_base_twfe_dol_idtime <- clustering_TWFE(data_s_margin_baseline, "id+time", TRUE, PACtype = "pro")
pro_SS_base_twfe_dol_statetime <- clustering_TWFE(data_s_margin_baseline, "state+time", TRUE, PACtype = "pro")



anti_cluster_dol_results <- list("No clutering"=anti_SS_base_twfe_dol_nocl,
                                  "District (Baseline)" = anti_SS_base_twfe_dol_idfe,
                                  "State"=anti_SS_base_twfe_dol_statefe,
                                 "District+month" = anti_SS_base_twfe_dol_idtime,
                                  "State+month"=anti_SS_base_twfe_dol_statetime)

pro_cluster_dol_results <- list("No clutering"=pro_SS_base_twfe_dol_nocl,
                                 "District (Baseline)" = pro_SS_base_twfe_dol_idfe,
                                 "State"=pro_SS_base_twfe_dol_statefe,
                                "District+month" = pro_SS_base_twfe_dol_idtime,
                                 "State+month"=pro_SS_base_twfe_dol_statetime)

gof_add_cluster <- data.frame(
  raw = c("Clustering", "Fixed Effect"),
  mod1 = c("No", "District+month"), 
  mod2 = c("District", "District+month"),
  mod3 = c("State", "District+month"),
  mod4 = c("District+month", "District+month"),
  mod5 = c("State+month", "District+month"),
  stringsAsFactors = FALSE
)

anti_SS_cluster_results <- gt_table(anti_cluster_dol_results, coef_map_single,  c("nobs"),
                                     gof_add_cluster,  title="Clustering Pattern",
                                    subtitle = "Gun Control PAC; Baseline DID; 2000--2024") %>%  print()


pro_SS_cluster_results <- gt_table(pro_cluster_dol_results, coef_map_single,  c("nobs"),
                                    gof_add_cluster,  title="Clustering Pattern",
                                    subtitle = "Gun Lobby PAC; Baseline DID; 2000--2024") %>%  print()




# 15. Propensity Score ----------------------------------------------------

# Use ps_matching_shooting function for matching
# Use check_balance_shooting function for balance check
# Use ps_outcome_analysis_shooting function for outcome analysis; only for the below output analysis
# Lastly, use shooting_output function for output


## 15.1 Gun Control --------------------------------------------------------

# baseline
anti_ps_month_1000_pt <- shooting_output(data_s_margin_baseline, 
                                    ratio=1000,  time = "Presidency",
                                    balance_title = NULL, love_title = NULL)
anti_ps_month_1000_pt[[3]]
# Good balance
anti_ps_month_1000_pt[[1]] # this is for Appendix


# more matched units
anti_ps_month_2000_pt <- shooting_output(data_s_margin_baseline, 
                                    ratio=2000,  time = "Presidency")
anti_ps_month_2000_pt[[3]]

anti_ps_month_500_pt <- shooting_output(data_s_margin_baseline, 
                                         ratio=500,  time = "Presidency")
anti_ps_month_500_pt[[3]]



## 15.2 Pro-Gun ------------------------------------------------------------

# baseline
pro_ps_month_1000_pt <- shooting_output(data_s_margin_baseline, PACtype = "pro",
                                        ratio=1000,  time = "Presidency",
                                        balance_title = NULL, love_title = NULL)
pro_ps_month_1000_pt[[3]]

# more matched units
pro_ps_month_2000_pt <- shooting_output(data_s_margin_baseline, PACtype = "pro",
                                        ratio=2000,  time = "Presidency")
pro_ps_month_2000_pt[[3]]

pro_ps_month_500_pt <- shooting_output(data_s_margin_baseline, PACtype = "pro",
                                       ratio=500,  time = "Presidency")
pro_ps_month_500_pt[[3]]



# prepare a list
pro_ps_month_list <- list()
anti_ps_month_list <- list()

pro_ps_month_list_name <- c("Baseline; Pro-Gun", "Number Outcome; Pro-Gun", 
                        "2000 Matching; Pro-Gun", "2000 Matching (Number); Pro-Gun",
                        "500 Matching; Pro-Gun", "500 Matching (Number); Pro-Gun")

anti_ps_month_list_name <- c( "Baseline; Gun Control", "Number Outcome; Gun Control", 
                        "2000 Matching; Gun Control", "2000 Matching (Number); Gun Control",
                        "500 Matching; Gun Control", "500 Matching (Number); Gun Control")

pro_ps_month_result <- list(pro_ps_month_1000_pt[[2]],
                             pro_ps_month_2000_pt[[2]],
                             pro_ps_month_500_pt[[2]])

anti_ps_month_result <- list(anti_ps_month_1000_pt[[2]],
                             anti_ps_month_2000_pt[[2]],
                             anti_ps_month_500_pt[[2]])


ps_month_list_counter <- 1
for (i in 1:length(pro_ps_month_result)) {
  pro_ps_month_list[[ps_month_list_counter]] <- pro_ps_month_result[[i]][[1]]  # Dollar Amount
  pro_ps_month_list[[ps_month_list_counter+1]] <- pro_ps_month_result[[i]][[2]]  # Number Count
  ps_month_list_counter <- ps_month_list_counter + 2
}

ps_month_list_counter <- 1
for (i in 1:length(anti_ps_month_result)) {
  anti_ps_month_list[[ps_month_list_counter]] <- anti_ps_month_result[[i]][[1]]  # Dollar Amount
  anti_ps_month_list[[ps_month_list_counter+1]] <- anti_ps_month_result[[i]][[2]]  # Number Count
  ps_month_list_counter <- ps_month_list_counter + 2
}

names(pro_ps_month_list) <- pro_ps_month_list_name
names(anti_ps_month_list) <- anti_ps_month_list_name


# Add row
gof_add_psshooting <- data.frame(
  raw = c("Outcome", "Control # per one treated unit"),
  mod1 = c("Dollar", "1000"),
  mod2 = c("Number",  "1000"),
  mod3= c("Dollar",  "2000"),
  mod4= c("Number",   "2000"),
  mod5= c("Dollar","500"),
  mod6= c("Number", "500"),
  stringsAsFactors = FALSE
)
  

ps_sourcenote <- 
  "   1. Baseline matching: Treated-control ratio: 1:1000, Nearest matching; Allowing multiple time use of the same unit; Using multiple units as control units; time-related variables for matching: presidential term, caliper: 0.2;
<br>2. Baseline covariates include ratios of bachelor holders, of black, and of white; unemployment rate, logged income,
<br>incumbency in the previous election, the number of months to its next HoR election, and estimated housefold firearm possession rate, unless removed due to multi-collinearity,
<br>3. Outcome regression: TWFE using district- and month-level fixed effects"

gt_table(pro_ps_month_list,coef_map_single,  c("nobs"),
         gof_add_psshooting, title="Propensity Score Matching Results",
         "SS baseline Dataset; Pro-Gun",
         source_note = ps_sourcenote) 

gt_table(anti_ps_month_list,coef_map_single,  c("nobs"),
         gof_add_psshooting, title="Propensity Score Matching Results",
         "SS baseline Dataset; Gun Control",
         source_note = ps_sourcenote) 



