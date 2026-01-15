# Data Wrangling and Create Treatment Variables

# 0. Packages and Functions -------------------------------------------------------------

if (!require("here")) install.packages("here")
library(here)

source(here(
  # "gunPAC",
  "PNAS_script", "01_packages.R"))
source(here(
  # "gunPAC",
  "PNAS_R", "functions_pre_analysis_updated.R"))

# 1. Read data files -------------------------------------------------------------

# HoR
# NEED TO CHANGE NAMES OF OBJECTS AND DATASETS THEMSELVES!
data_s <- anti_data_s <- data.table::fread(here(
  # "gunPAC",
  "PNAS_data", "raw_data","FINAL_merged_GVPpac_panel_with_PROLIFE_and_PROCHOICE_FIXED_july16.csv"))
pro_data_s <- data.table::fread(here(
  # "gunPAC",
  "PNAS_data", "raw_data","SS_panel_wElections_FINAL_with_NEXT_UPDATED_wHFR_july16.csv"))

data_v <- anti_data_v <- data.table::fread(here(
  # "gunPAC",
  "PNAS_data", "raw_data","VPP_July22_with_GVP_PROLIFE_PROCHOICE.csv"))
data_g <- anti_data_g <- data.table::fread(here(
  # "gunPAC",
  "PNAS_data", "raw_data","GVA_MERGED_with_GVP_PROLIFE_PROCHOICE_july25.csv"))
data_m <- anti_data_m <- data.table::fread(here(
  # "gunPAC",
  "PNAS_data", "raw_data","MJ_July22_with_GVP_PROLIFE_PROCHOICE.csv"))


# US Senates
data_snt <- data.table::fread(here(
  # "gunPAC",
  "PNAS_data", "raw_data","Senate_SS_panel_withprogun_withGVP_wHFR_july23.csv"))

# State Offices
data_gov <- data.table::fread(here(
  # "gunPAC",
  "PNAS_data", "raw_data","gov_SS_merged_panel_with_covariates_wHFR_wGVP.csv"))
data_atg <- data.table::fread(here(
  # "gunPAC",
  "PNAS_data", "raw_data","AG_SS_merged_panel_with_covariates_wHFR_wGVP.csv"))


## 1.1 Check -----------------------------------------------------------

# Check if uploaded datasets have unappropriate or unintentional characters

data_list <- list(data_s, data_m, data_g, data_v, data_snt, data_gov, data_atg)
# purrr::map(data_list, glimpse)


## 1.2 Select Columns ----------------------------------------------------

# Select columns used for executing scripts
# Note: set refund and margin variables, instead of using variables included in datasets

### 1.2.1 HoR -------------------------------------------------------------

# school shooting
data_s <- data_s %>% 
  dplyr::select (1:4,6:8, # to total_wounded
                 11, 12, 14, #unique_pacs
                 17, 19,
                 # 21, 22,# election dates
                 24:30, #bachelor-unemployed
                 32, # total vote
                 33:34,#rank info(33-34) is important
                 # 38, 39, # no percentage
                 42,43,
                 # just for the time being, almost no next-related info
                 45,# election year next
                 47:49,57,58,60, # gun ratio
                 61,62,64:66,68)

# MJ dataset
data_m <- data_m %>% 
  dplyr::select(1:3, # no date
                5:7, 9,10, # - incident count 
                14, 16, 18:24, # to unemployment
                26:28, 36,37,39, # next election
                41:43, 51, 52, # no state info?
                55:57, 59:61, 63:65, 67)  


# VPP
data_v <- data_v %>% 
  dplyr::select(1:6, # - to wounded
                9,10,13:19, # to unemployed
                23,25, 27:29, # to reprank
                37, 38, 40, 42:44, # to reprank next
                52,53, 56:58, 60:62,64:66, 68)  


# for GVA
data_g <- data_g %>% 
  dplyr::select(1:3, 5:7, 9:10, 15:21, # - unemployment
                23,25, # timing in relation to election
                27:29, 37,38, # dem and rep vote before,
                40, 42:44, #rep.rank.next 
                52:53, 56:58, 60:62,64:66, 68)


### 1.2.2 US Senate and State Offices -------------------------------------------------------

# US Sen.
data_snt <- data_snt %>% 
  dplyr::select (1:7, # to total_wounded
                 9,  11, 12,   # no each senator info (13)
                 14:19, 21:24)

# State Offices
glimpse(data_gov)
data_gov <- data_gov %>% 
  dplyr::select (1:7, # to total_wounded
                 9,  11, 12,   # to donation count
                 13:18, # rep and dem; that said, this is only for gun lobby, and no counterparts at GVP
                 19:24,26:29)

data_atg <- data_atg %>% 
  dplyr::select (1:7, # to total_wounded
                 9,  11, 12,   # to donation count
                 13:18, # rep and dem
                 19:24,26, # no State info; duplication
                 28:30)

## 1.3. Set Column Names -------------------------------------------------------

### 1.3.1 HoR -----------------------------------------------------------

# Note that this dataset does not contain information on gun lobby PAC contribution.
colnames(data_s) <- c("district", "year", "month", 
                      "state", # pre-prepared; not included in some datasets
                      "incident_count", 
                      #different from "incident," a binary variable which will bet set up later
                      "fatalities", "injured",
                      "anti_pac_donations_dollar", # dollar variable for gun safety PACs
                      "anti_total_donations_number", # number
                      "anti_unique_pacs",
                       # "refund",  # not used; set later
                      "months_to_election", "months_since_election",
                      # "some_college",  # bachelor is used instead
                      "bachelor","income", "white","black","Asian", 
                      "otherrace", # Not the same as Hispanic
                      "unemployment", "num_total_before", "dem_rank_before", "rep_rank_before",
                      # "pct_RDmargin_before", # not used; set later
                       "num_dem_before","num_rep_before", # be careful with percentage (%) and ratio
                      "election_year_next", "num_total_next","dem_rank_next", "rep_rank_next",
                           # "pct_RDmargin_next", # not used here
                           "num_dem_next","num_rep_next", "gun_ratio",
                      
                      # no pro gun lobby data in this dataset
                           "life_pac_donations_dollar", "life_total_donations_number",
                           "life_unique_pacs","choice_pac_donations_dollar", "choice_total_donations_number",
                           "choice_unique_pacs")

colnames(data_m) <- c("district", "year", "month", # no state
                           "incident_count","fatalities", "injured",
                           "pro_pac_donations_dollar", "pro_total_donations_number",  
                           "months_to_election", "months_since_election",
                           "bachelor","income",
                           "white","black","Asian",  "otherrace", "unemployment",
                           "num_total_before","dem_rank_before", "rep_rank_before",
                           "num_dem_before","num_rep_before", "election_year_next",
                           
                           "num_total_next","dem_rank_next", "rep_rank_next",
                           "num_dem_next","num_rep_next", "gun_ratio",
                           "anti_pac_donations_dollar", "anti_total_donations_number","anti_unique_pacs",
                           "life_pac_donations_dollar", "life_total_donations_number","life_unique_pacs",
                           "choice_pac_donations_dollar", "choice_total_donations_number","choice_unique_pacs"
)

colnames(data_v) <- c("district", "year", "month", # no state
                           "incident_count", "fatalities", "injured",
                           "pro_pac_donations_dollar", "pro_total_donations_number",
                           "bachelor","income","white","black","Asian", 
                           "otherrace", "unemployment", "months_to_election", "months_since_election",
                           "num_total_before", "dem_rank_before", "rep_rank_before",
                            "num_dem_before","num_rep_before", "election_year_next",
                           "num_total_next","dem_rank_next", "rep_rank_next",
                           "num_dem_next","num_rep_next", "gun_ratio",
                           "anti_pac_donations_dollar", "anti_total_donations_number","anti_unique_pacs",
                           "life_pac_donations_dollar", "life_total_donations_number","life_unique_pacs",
                           "choice_pac_donations_dollar", "choice_total_donations_number","choice_unique_pacs"
)

colnames(data_g) <- c("district", "year", "month", # no state
                           "incident_count", "fatalities", "injured",
                           "pro_pac_donations_dollar", "pro_total_donations_number",
                           "bachelor","income",
                           "white","black","Asian",  "otherrace",   "unemployment",
                           "months_to_election", "months_since_election",
                          "num_total_before","dem_rank_before", "rep_rank_before",
                           "num_dem_before","num_rep_before","election_year_next",
                           
                           "num_total_next","dem_rank_next", "rep_rank_next",
                           "num_dem_next","num_rep_next", "gun_ratio",
                           "anti_pac_donations_dollar", "anti_total_donations_number","anti_unique_pacs",
                           "life_pac_donations_dollar", "life_total_donations_number","life_unique_pacs",
                           "choice_pac_donations_dollar", "choice_total_donations_number","choice_unique_pacs"
)


### 1.3.2 US Senator and State Offices ----------------------------------------------------

colnames(data_snt) <- c("abbr","fips",  "state", "year", "month", 
                        "fatalities", "injured",
                        "incident_count","pro_pac_donations_dollar", "pro_total_donations_number",
                        "income",  "white","black","Asian",   "otherrace",   "unemployment", "bachelor", 
                        "gun_ratio",  "anti_pac_donations_dollar", "anti_total_donations_number")

colnames(data_gov) <- c("abbr","fips",  "state", "year", "month", 
                        "fatalities", "injured",
                        "incident_count","pro_pac_donations_dollar", "pro_total_donations_number",
                        
                        # only gun lobby has breakdowns of Rep and Dem
                        "pro_pac_donations_dollar_rep", "pro_total_donations_number_rep",
                        # contributions for Rep. is different from gun lobby contributions.
                        "pro_pac_donations_dollar_dem", "pro_total_donations_number_dem",
                        
                        # net means GOP minus DEM
                        "pro_pac_donations_dollar_net", "pro_total_donations_number_net",
                        "income",
                        "white","black","Asian", "otherrace",   "unemployment", "bachelor",
                        "gun_ratio", "anti_pac_donations_dollar", "anti_total_donations_number")

colnames(data_atg) <- c("abbr","fips",  "state", "year", "month", 
                        "fatalities", "injured",
                        "incident_count","pro_pac_donations_dollar", "pro_total_donations_number",
                        "pro_pac_donations_dollar_rep", "pro_total_donations_number_rep",
                        "pro_pac_donations_dollar_dem", "pro_total_donations_number_dem",
                        "pro_pac_donations_dollar_net", "pro_total_donations_number_net",
                        "income",
                        "white","black","Asian", "otherrace",   "unemployment", "bachelor",
                        "gun_ratio", "anti_pac_donations_dollar", "anti_total_donations_number")


## 1.4 check NAs (1st)-------------------------------------------------------

# Checking NA at original variables first, before creating more variables.
# use checkNA_first function

data_list <- list(data_s, data_m, data_g, data_v, data_snt, data_gov, data_atg)
names(data_list) <- c("data_s", "data_m", "data_g", "data_v", "data_snt", "data_gov", "data_atg")
data_list <- purrr::map(data_list, checkNA_first)

# map will be applied later
# list2env(data_list, envir = .GlobalEnv)

# For newly created variables, check NA later

## 1.5 Extracting state information ------------------------------------------------------

# For datasets not including "state" variable, extracting state info from district column
# use state_separate function
# Even when a dataset contains "state" variable, this function should be applied to
# create within-state district variable.
 

# data_list <- list(data_s, data_m, data_g, data_v, data_snt, data_gov, data_atg)
# names(data_list) <- c("data_s", "data_m", "data_g", "data_v", "data_snt", "data_gov", "data_atg")
data_list <- purrr::map(data_list, state_separate)
list2env(data_list, envir = .GlobalEnv)

## 1.6 Combined with pro-gun PAC dataset-----------------------------------------

# School shooting data does not contain information on pro gun lobby PAC.

pro_data_s <- pro_data_s %>% 
  rename(district = congressional_district,
         pro_pac_donations_dollar=total_pac_donations,
         pro_total_donations_number =total_number_of_donations)

# restore pre-combined dataset for subsequent checking
anti_data_s <- data_s

# use combine_progun_data function
data_s <- combine_progun_data(anti_data_s, pro_data_s)


### 1.6.1 Check combined dataset -------------------------------------------

# glimpse(data_s)

# Check that there are no NAs at contributions data.
table(!is.na(data_s$pro_pac_donations_dollar))
table(!is.na(data_s$anti_pac_donations_dollar))

summary(data_s$pro_pac_donations_dollar)
summary(data_s$anti_pac_donations_dollar)

# Check duplication and void
nrow(anti_data_s)
nrow(pro_data_s)
nrow(data_s)

#### 1.6.6.1 Check missing data further ---------------------------------------------------

missing_in_pro <- anti_data_s %>%
  anti_join(pro_data_s, by = c("district", "year", "month"))

missing_in_anti <- pro_data_s %>%
  anti_join(anti_data_s, by = c("district", "year", "month"))

nrow(missing_in_pro) # return zero
nrow(missing_in_anti) # return zero

#### 1.6.6.2 Duplication within the same dataset ----------------------------

pro_duplicates <- pro_data_s %>%
  group_by(district, year, month) %>%
  summarise(count = n(), .groups = 'drop') %>%  # have to ungroup
  filter(count > 1) # >1 means duplication within the dataset

nrow(pro_duplicates) # zero

# next, check duplication in GVP dataset
anti_duplicates <- anti_data_s %>%
  group_by(district, year, month) %>%
  summarise(count = n(), .groups = 'drop') %>%
  filter(count > 1)

nrow(anti_duplicates) # zero

# Lastly, check the combined dataset
anti_pro_duplicates <- data_s %>%
  group_by(district, year, month) %>%
  summarise(count = n(), .groups = 'drop') %>%
  filter(count > 1)

nrow(anti_pro_duplicates) # zero


## 1.7 Percentage and numeric ----------------------------------------------

# check if variables with percentage unit are numeric
# The concern here is the case where "%" is included and converted to characters.
# use check_percentage_numeric function

data_list <- list(data_s, data_m, data_g, data_v, data_snt, data_gov, data_atg)
names(data_list) <- c("data_s", "data_m", "data_g", "data_v", "data_snt", "data_gov", "data_atg")
data_list <- purrr::map(data_list, check_percentage_numeric)
list2env(data_list, envir = .GlobalEnv)


# 2. Create new variables -----------------------------------------------------


## 2.1 For HoR Dataset -----------------------------------------------------


# create basic variables
# use new_var_HoR function

HoR_data_list <- list(data_s, data_m, data_g, data_v)
names(HoR_data_list) <- c("data_s", "data_m", "data_g", "data_v")
HoR_data_list <- purrr::map(HoR_data_list, new_var_HoR)

# Then, create contributions during relevant periods (especially, within election cycle) for each side
# This assumes that contributions for HoR candidates are made for the sake of HoR elections.
# use mutual_history function

HoR_data_list <- purrr::map(HoR_data_list, mutual_history)
list2env(HoR_data_list, envir = .GlobalEnv)


## 2.2 For Senate Data -----------------------------------------------------

# use new_var_Sen function
data_snt <- new_var_Sen(data_snt)


## 2.3 State Offices ----------------------------------------------------------

# Note that datasets on state offices have data on pro-gun contributions to both GOP and DEM
# candidates, as well as on "net" contribution amount, which means GOP minus DEM.
# But, this is only the case for gun lobby PACs.

# use new_var_state function

data_gov <- new_var_state(data_gov)
data_atg <- new_var_state(data_atg)

## 2.4 Check new datasets -------------------------------------------------

# Check unintentional characters or integers

# data_list <- list(data_s, data_m, data_g, data_v, data_snt, data_gov, data_atg)
# data_list %>% 
#   purrr::map(glimpse) %>% 
#   purrr::map(nrow)

### 2.4.1 Check NAs (second time)-------------------------------------------------------

# Do not include "last_incident_time." That NA is needed to calculate incident_window.

# Use checkNA_second_HoR and checkNA_second_nonHoR functions
# Should use different functions, since only HoR has pct_RDmargin_before data

# First, HoR
HoR_data_list <- list(data_s, data_m, data_g, data_v)
names(HoR_data_list) <- c("data_s", "data_m", "data_g", "data_v")
HoR_data_list <- purrr::map(HoR_data_list, checkNA_second_HoR)
list2env(HoR_data_list, envir = .GlobalEnv)

# Then, non-HoR
data_snt <- checkNA_second_nonHoR(data_snt)
data_gov <- checkNA_second_nonHoR(data_gov)
data_atg <- checkNA_second_nonHoR(data_atg)



# 3. Create Treatment Variable and Filter Data ----------------------------

## 3.1 Basic Functions for HoR -------------------------------------------------------------

### 3.1.1 Whole data  -----------------------------------------------

# Use data_treatment_name function for whole datasets, unfiltered in terms of voting margin
# Do not forget to differentiate raw data and the data with minimized filtering by data_treatment_name function

HoR_data_list <- list(data_s, data_m, data_g, data_v)
                      # , data_snt, data_gov, data_atg
names(HoR_data_list) <- c("data_s", "data_m", "data_g", "data_v")
                      #, "data_snt", "data_gov", "data_atg"
data_list_2000 <- purrr::map(HoR_data_list, data_treatment_name) %>%
  purrr::set_names(c("data_s_2000", "data_m_2000", "data_g_2000", "data_v_2000"))
                    # , "data_snt_2000","data_gov_2000", "data_atg_2000"
list2env(data_list_2000, envir = .GlobalEnv)



### 3.1.2 Baseline ----------------------------------------------------------

# Next, baseline limitation, of our largest interest
# use data_margin_baseline function and raw data for input (instead of minimum filtered dataset)
# this can apply to mass shooting datasets

data_s_margin_baseline <- data_margin_baseline(data_s) 
data_m_margin_baseline <- data_margin_baseline(data_m) 
data_v_margin_baseline <- data_margin_baseline(data_v) 
data_g_margin_baseline <- data_margin_baseline(data_g) 

data_s_2000_filtered <- data_margin_baseline(data_s, 100)
data_m_2000_filtered <- data_margin_baseline(data_m, 100)
data_v_2000_filtered <- data_margin_baseline(data_v, 100)
data_g_2000_filtered <- data_margin_baseline(data_g, 100)


### 3.1.3 Other settings ----------------------------------------------------

# Next, add more flexible function, named data_flexible
# only for symmetrical voting margin threshold

# used for before and after Parkland, which took place in 2018

data_s_whole_2000_2017 <- data_flexible(data_s,100,# margin
                                                0, # fatality
                                                1, year_start=2000, year_end=2017)
data_s_whole_2018_2024 <- data_flexible(data_s,100,0,1,year_start=2018, year_end=2024)


data_s_wholefiltered_2000_2017 <- data_flexible(data_s,100,# margin
                                        1, # fatality
                                        10000, year_start=2000, year_end=2017)
data_s_wholefiltered_2018_2024 <- data_flexible(data_s,100,1,10000,year_start=2018, year_end=2024)

# baseline filtering
data_s_baseline_2000_2017 <- data_flexible(data_s, year_start=2000, year_end=2017)
data_s_baseline_2018_2024 <- data_flexible(data_s, year_start=2018, year_end=2024)

# Then, data_voting_filter function for assymetric voting margin


# Lastly, data_withinstate function to estimate within-state impact



## 3.2 Non-HoR-----------------------------------------------------------------

# Different functions are defined, since we do not have margin data for non-HoR datasets
# data_treatment_name_nonHoR function
# The same function as the HoR version, since the HoR version also does not have margin threshold,
# but defined just for consistency.

data_snt_2000 <- data_treatment_name_nonHoR(data_snt) 
data_gov_2000 <- data_treatment_name_nonHoR(data_gov)
data_atg_2000 <- data_treatment_name_nonHoR(data_atg)

# Next, data_baseline_nonHoR function for baseline limitation 
# Note that the number of arguments is different from the baseline HoR function
data_snt_baseline <- data_baseline_nonHoR(data_snt) 
data_gov_baseline <- data_baseline_nonHoR(data_gov) 
data_atg_baseline <- data_baseline_nonHoR(data_atg) 

# Then, more flexible function, data_restriction_nonHoR function


# for state offices
# in principle, the same function can apply as US Senate

# data_baseline_statenet for baseline setting; it is tailored to use net-donations.
# Note that this "net" variable is only for gun lobby contributions.
data_govnet_baseline <- data_baseline_statenet(data_gov)
data_atgnet_baseline <- data_baseline_statenet(data_atg)


## 3.3 Time to election -------------------------------------------------

# data_month_to function used for timing analysis
# Note that, under de-fault setting, this will filter out non-competitive districts

# Just in case, it would be great to avoid changing original data
# Instead, make new datasets specific for this purpose

data_s_month_to_whole <- data_month_to(data_s, 100)
data_m_month_to_whole <- data_month_to(data_m, 100)
data_v_month_to_whole <- data_month_to(data_v, 100)
data_g_month_to_whole <- data_month_to(data_g, 100)

data_s_month_to_baseline <- data_month_to(data_s) # default margin is 5.
data_m_month_to_baseline <- data_month_to(data_m)
data_v_month_to_baseline <- data_month_to(data_v)
data_g_month_to_baseline <- data_month_to(data_g)


# 3.4 Save processed datasets ----------------------------------------

# use save_processed_dataset function

save_processed_dataset(data_s, "data_s")
save_processed_dataset(data_m, "data_m")
save_processed_dataset(data_g, "data_g")
save_processed_dataset(data_v, "data_v")

save_processed_dataset(data_s_2000, "data_s_2000")
save_processed_dataset(data_m_2000, "data_m_2000")
save_processed_dataset(data_g_2000, "data_g_2000")
save_processed_dataset(data_v_2000, "data_v_2000")

save_processed_dataset(data_s_2000_filtered, "data_s_2000_filtered")
save_processed_dataset(data_m_2000_filtered, "data_m_2000_filtered")
save_processed_dataset(data_g_2000_filtered, "data_g_2000_filtered")
save_processed_dataset(data_v_2000_filtered, "data_v_2000_filtered")

save_processed_dataset(data_s_margin_baseline, "data_s_margin_baseline")
save_processed_dataset(data_m_margin_baseline, "data_m_margin_baseline")
save_processed_dataset(data_v_margin_baseline, "data_v_margin_baseline")
save_processed_dataset(data_g_margin_baseline, "data_g_margin_baseline")

save_processed_dataset(data_snt_baseline, "data_snt_baseline")
save_processed_dataset(data_gov_baseline, "data_gov_baseline")
save_processed_dataset(data_atg_baseline, "data_atg_baseline")

save_processed_dataset(data_s_month_to_whole, "data_s_month_to_whole")
save_processed_dataset(data_m_month_to_whole, "data_m_month_to_whole")
save_processed_dataset(data_v_month_to_whole, "data_v_month_to_whole")
save_processed_dataset(data_g_month_to_whole, "data_g_month_to_whole")

save_processed_dataset(data_s_month_to_baseline, "data_s_month_to_baseline")
save_processed_dataset(data_m_month_to_baseline, "data_m_month_to_baseline")
save_processed_dataset(data_v_month_to_baseline, "data_v_month_to_baseline")
save_processed_dataset(data_g_month_to_baseline, "data_g_month_to_baseline")

save_processed_dataset(data_s_whole_2000_2017, "data_s_whole_2000_2017")
save_processed_dataset(data_s_whole_2018_2024, "data_s_whole_2018_2024")
save_processed_dataset(data_s_wholefiltered_2000_2017, "data_s_wholefiltered_2000_2017")
save_processed_dataset(data_s_wholefiltered_2018_2024, "data_s_wholefiltered_2018_2024")
save_processed_dataset(data_s_baseline_2000_2017, "data_s_baseline_2000_2017")
save_processed_dataset(data_s_baseline_2018_2024, "data_s_baseline_2018_2024")

 


