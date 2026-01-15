# Other Analyses

# 0. Preparation -------------------------------------------------------------

if (!require("here")) install.packages("here")
library(here)

source(here(
  # "gunPAC",
  "PNAS_script", "01_packages.R"))
source(here(
  # "gunPAC",
  "PNAS_R", "functions_pre_analysis.R"))

# source(here(
#   # "gunPAC",
#   "PNAS_script", "02_data_wrangling.R")) # can skip this
# skip_data_wrangling(TRUE) # can skip this as well later

source(here(
  # "gunPAC",
  "PNAS_script", "03_descriptive_stats.R"))
source(here(
  # "gunPAC",
  "PNAS_R", "functions_analysis.R"))
source(here(
  # "gunPAC",
  "PNAS_script", "04_baseline_TWFE.R"))

# 1 DCDH---------------------------------------------------------------------

# Using DIDmultiplegtDYN package
# have to update and check what is the latest ver.

# This is not usig bootstrap, unlike another old package
# see here; https://github.com/chaisemartinPackages/did_multiplegt_dyn

# Use DCDH_event function, different from a plot function, since results may have no CI data, and that
# will cause errors for plots

# check results also by print, not only by plot


## 1.1 Visualization-----------------------------------------------------------------

# Use plot_DCDH function


# See https://asjadnaqvi.github.io/DiD/docs/code_r/07_did_multiplegt_dyn_r/
# If some CIs are missing in results, you have to adjust numbers of rep color.
# Error when no CI results


## 1.2 Output for static tables ------------------------------------------------------------

# 24 months for static analysis
anti_DCDH_s_baseline24 <- DCDH_event(data_s_margin_baseline
                                     , n_effect = 24) # consistency with baseline DID

pro_DCDH_s_baseline24 <- DCDH_event(data_s_margin_baseline, outcome = "pro_log_sum", n_effect = 24) 

# Number outcome
anti_DCDH_s_num24 <- DCDH_event(data_s_margin_baseline, outcome = "anti_don_number", n_effect = 24)
pro_DCDH_s_num24 <- DCDH_event(data_s_margin_baseline, outcome = "pro_don_number", n_effect = 24)


### 1.2.1 (suspended) static modelsummary plot ---------------------------------------------

# jointplacebo is not usual p-value.
anti_DCDH_s_baseline24$results


# later, will be incorporated to results based on imputation analysis

anti_DCDH_static_list <- list("Dollar Outcome; Gun Control"=anti_DCDH_s_baseline24,
                         "Number Outcome; Gun Control"=anti_DCDH_s_num24) # these names are reflected in the plot

pro_DCDH_static_list <- list("Dollar Outcome; Pro-Gun"=pro_DCDH_s_baseline24,
                              "Number Outcome; Pro-Gun"=pro_DCDH_s_num24)

# register into S3 class
setOldClass("DCDH") # convert S3 to S4


# register both tidy and glance for modelsummary
registerS3method("tidy",   "did_multiplegt_dyn", tidy.did_multiplegt_dyn,   envir = asNamespace("broom"))
registerS3method("glance", "did_multiplegt_dyn", glance.did_multiplegt_dyn, envir = asNamespace("broom"))
# into broom
# registerS3method("tidy", "DCDH", tidy.DCDH, envir = asNamespace("broom"))

anti_gof_add_DCDH <- data.frame(
  raw = c("Placebo Test's p-value", "Outcome"),
  mod1 = c(sprintf("%.3f", anti_DCDH_s_baseline24$results$p_jointplacebo), "Dollar"),
  mod2 = c( sprintf("%.3f", anti_DCDH_s_num24$results$p_jointplacebo), "Number"),
  stringsAsFactors = FALSE
)

pro_gof_add_DCDH <- data.frame(
  raw = c("Placebo Test's p-value", "Outcome"),
  mod1 = c(sprintf("%.3f", pro_DCDH_s_baseline24$results$p_jointplacebo), "Dollar"),
  mod2 = c( sprintf("%.3f", pro_DCDH_s_num24$results$p_jointplacebo), "Number"),
  stringsAsFactors = FALSE
)


anti_DCDH_static_results <- gt_table(anti_DCDH_static_list,coef_map_single,
                                c("nobs"),anti_gof_add_DCDH,
                                "DCDH Static Analysis","School Shooting; Gun Control",
                                source_note = ss_sourcenote, collin = FALSE) %>% print()


pro_DCDH_static_results <- gt_table(pro_DCDH_static_list,coef_map_single,
                                     c("nobs"),pro_gof_add_DCDH,
                                     "DCDH Static Analysis","School Shooting; Pro-Gun",
                                     source_note = ss_sourcenote, collin = FALSE) %>% print()


## 1.3 Event Study ------------------------------------------------------

# Do not forget to comment out unnecessary lines

anti_DCDH_s_mva_logav5_cov <- DCDH_event(data_s_margin_baseline, outcome="anti_log_sum_mva_logav5", n_effect = 12)
pro_DCDH_s_mva_logav5_cov <- DCDH_event(data_s_margin_baseline, outcome="pro_log_sum_mva_logav5", n_effect = 12)

anti_DCDH_s_mva_5_covnum <- DCDH_event(data_s_margin_baseline, "anti_don_number_mva_5", n_effect = 6)
pro_DCDH_s_mva_5_covnum <- DCDH_event(data_s_margin_baseline, "pro_don_number_mva_5", n_effect = 6)


# Other periods
anti_DCDH_s_mva_logav2_cov <- DCDH_event(data_s_margin_baseline, "anti_log_sum_mva_logav2", n_effect = 6)
anti_DCDH_s_mva_logav3_cov <- DCDH_event(data_s_margin_baseline, "anti_log_sum_mva_logav3", n_effect = 6)

anti_DCDH_s_mva_logav2_covnum <- DCDH_event(data_s_margin_baseline, "anti_don_number_mva_2", n_effect = 6)
anti_DCDH_s_mva_logav3_covnum <- DCDH_event(data_s_margin_baseline, "anti_don_number_mva_3", n_effect = 6)

pro_DCDH_s_mva_logav2_cov <- DCDH_event(data_s_margin_baseline, "pro_log_sum_mva_logav2", n_effect = 6)
pro_DCDH_s_mva_logav3_cov <- DCDH_event(data_s_margin_baseline, "pro_log_sum_mva_logav3", n_effect = 6)

pro_DCDH_s_mva_logav2_covnum <- DCDH_event(data_s_margin_baseline, "pro_don_number_mva_2", n_effect = 6)
pro_DCDH_s_mva_logav3_covnum <- DCDH_event(data_s_margin_baseline, "pro_don_number_mva_3", n_effect = 6)



# without cov
anti_DCDH_s_mva_logav5_nocov <- DCDH_event(data_s_margin_baseline, "anti_log_sum_mva_logav5", 
                                           controls = NULL)
pro_DCDH_s_mva_logav5_nocov <- DCDH_event(data_s_margin_baseline, "pro_log_sum_mva_logav5", 
                                           controls = NULL)


# focusing on after parkland

data_s_margin_baseline_1824 <- data_s_margin_baseline %>% 
  filter(year>=2018)

anti_DCDH_s_mva_logav5_cov_1824 <- DCDH_event(data_s_margin_baseline_1824, outcome="anti_log_sum_mva_logav5", n_effect = 12)
pro_DCDH_s_mva_logav5_cov_1824 <- DCDH_event(data_s_margin_baseline_1824, outcome="pro_log_sum_mva_logav5", n_effect = 12)

anti_DCDH_s_mva_5_covnum_1824 <- DCDH_event(data_s_margin_baseline_1824, "anti_don_number_mva_5", n_effect = 6)
pro_DCDH_s_mva_5_covnum_1824 <- DCDH_event(data_s_margin_baseline_1824, "pro_don_number_mva_5", n_effect = 6)


anti_DCDH_s_mva_logav2_cov_1824 <- DCDH_event(data_s_margin_baseline_1824, "anti_log_sum_mva_logav2", n_effect = 6)
anti_DCDH_s_mva_logav3_cov_1824 <- DCDH_event(data_s_margin_baseline_1824, "anti_log_sum_mva_logav3", n_effect = 6)

anti_DCDH_s_mva_logav2_covnum_1824 <- DCDH_event(data_s_margin_baseline_1824, "anti_don_number_mva_2", n_effect = 6)
anti_DCDH_s_mva_logav3_covnum_1824 <- DCDH_event(data_s_margin_baseline_1824, "anti_don_number_mva_3", n_effect = 6)

pro_DCDH_s_mva_logav2_cov_1824 <- DCDH_event(data_s_margin_baseline_1824, "pro_log_sum_mva_logav2", n_effect = 6)
pro_DCDH_s_mva_logav3_cov_1824 <- DCDH_event(data_s_margin_baseline_1824, "pro_log_sum_mva_logav3", n_effect = 6)

pro_DCDH_s_mva_logav2_covnum_1824 <- DCDH_event(data_s_margin_baseline_1824, "pro_don_number_mva_2", n_effect = 6)
pro_DCDH_s_mva_logav3_covnum_1824 <- DCDH_event(data_s_margin_baseline_1824, "pro_don_number_mva_3", n_effect = 6)


anti_DCDH_s_mva_5_covnum_1824 <- DCDH_event(data_s_margin_baseline_1824, "anti_don_number_mva_5", n_effect = 6)
pro_DCDH_s_mva_5_covnum_1824 <- DCDH_event(data_s_margin_baseline_1824, "pro_don_number_mva_5", n_effect = 6)




# Number Outcome

anti_DCDH_s_mva_2_covnum <- DCDH_event(data_s_margin_baseline, "anti_don_number_mva_2", n_effect = 6)
anti_DCDH_s_mva_3_covnum <- DCDH_event(data_s_margin_baseline, "anti_don_number_mva_3", n_effect = 6)
anti_DCDH_s_mva_2_covnum24 <- DCDH_event(data_s_margin_baseline, "anti_don_number_mva_2", n_effect = 24)
anti_DCDH_s_mva_3_covnum24 <- DCDH_event(data_s_margin_baseline, "anti_don_number_mva_3", n_effect = 24)

pro_DCDH_s_mva_2_covnum <- DCDH_event(data_s_margin_baseline, "pro_don_number_mva_2", n_effect = 6)
pro_DCDH_s_mva_3_covnum <- DCDH_event(data_s_margin_baseline, "pro_don_number_mva_3", n_effect = 6)
pro_DCDH_s_mva_2_covnum24 <- DCDH_event(data_s_margin_baseline, "pro_don_number_mva_2", n_effect = 24)
pro_DCDH_s_mva_3_covnum24 <- DCDH_event(data_s_margin_baseline, "pro_don_number_mva_3", n_effect = 24)


## 1.4 Output --------------------------------------------------------

# default is n=12


plot_DCDH(anti_DCDH_s_mva_logav5_nocov, 
          # n_effect_CI = 24, n_effect_point = 24,
          title = "",
          subtitle = "")

plot_DCDH(pro_DCDH_s_mva_logav5_nocov, 
          title = "",
          subtitle = "")

plot_DCDH(anti_DCDH_s_mva_5_covnum,
          n_effect_CI = 6, n_effect_point = 6,
          title = "",
          subtitle = "")

plot_DCDH(pro_DCDH_s_mva_5_covnum,
          n_effect_CI = 6, n_effect_point = 6,
          title = "",
          subtitle = "")

plot_DCDH(anti_DCDH_s_baseline24, 
          title = "",
          subtitle = "")

plot_DCDH(pro_DCDH_s_mva_logav5_cov,
          title = "",
          subtitle = ""
          # n_effect_CI = 6, n_effect_point = 6
          )


plot_DCDH(pro_DCDH_s_mva_logav2_cov,
          title = "",
          subtitle = "",
          n_effect_CI = 6, n_effect_point = 6
)

# plot_DCDH(DCDH_s_mva_avlog5_cov, title_char="Basic Filtering",
#           subtitle="School Shooting; HoR; Moving Avg. (Avg. of Log); All covariates")


plot_DCDH(DCDH_s_mva_logav5_nocov,
          # n_effect_CI=6,
          # n_effect_point=6,
          # title="DCDH; Basic Filtering",
          # subtitle="School Shooting; Moving Avg. (Avg. of log); All Covariates"
          title = "", subtitle = ""
)

plot_DCDH(DCDH_s_mva_5_covnum12,
          # n_effect_CI=6,
          # n_effect_point=6,
          # title="DCDH; Basic Filtering",
          # subtitle="School Shooting; Moving Avg. (Avg. of log); All Covariates"
          title = "", subtitle = "", ylab = "Number Count"  # have to change ylab!!
)


plot_DCDH(DCDH_s_mva_logav5_cov,
          n_effect_CI=6,
          n_effect_point=6,
          # title="DCDH; Basic Filtering",
          # subtitle="School Shooting; Moving Avg. (Avg. of log); All Covariates"
          title = "", subtitle = ""
)

plot_DCDH(DCDH_s_mva_5_covnum, title_char="Basic Filtering",
          subtitle="School Shooting; Moving Avg. (Number); All Covariates",
          ylab="Number Count")

plot_DCDH(DCDH_s_mva_logav5_nocov, title_char="Basic Filtering",
          subtitle="School Shooting; Moving Avg. (Log of Avg.); No Covariates")

plot_DCDH(DCDH_s_mva_logav2_cov, title_char="Basic Filtering",
          subtitle="School Shooting; Moving Avg. (3 months); All Covariates")

plot_DCDH(DCDH_s_mva_logav3_cov, title_char="Basic Filtering",
          subtitle="School Shooting; Moving Avg. (4 months); All Covariates")


## 1.5 Multiple Output (for dollar, and three moving avg patterns) -----------------------------------------------

# Use DCDH_multiple_plot function

DCDH_multiple_plot(anti_DCDH_s_mva_logav2_cov,
                   anti_DCDH_s_mva_logav3_cov,
                   anti_DCDH_s_mva_logav5_cov,
                   anti_DCDH_s_mva_2_covnum,
                   anti_DCDH_s_mva_3_covnum,
                   anti_DCDH_s_mva_5_covnum,
                   ylim_dol = c(-0.8, 1.6),ylim_num = c(-0.1,0.2))


DCDH_multiple_plot(pro_DCDH_s_mva_logav2_cov,
                   pro_DCDH_s_mva_logav3_cov,
                   pro_DCDH_s_mva_logav5_cov,
                   pro_DCDH_s_mva_2_covnum,
                   pro_DCDH_s_mva_3_covnum,
                   pro_DCDH_s_mva_5_covnum,
                   ylim_dol = c(-2.5, 2),ylim_num = c(-0.35,0.22))


DCDH_multiple_plot(anti_DCDH_s_mva_logav2_cov_1824,
                   anti_DCDH_s_mva_logav3_cov_1824,
                   anti_DCDH_s_mva_logav5_cov_1824,
                   anti_DCDH_s_mva_logav2_covnum_1824,
                   anti_DCDH_s_mva_logav3_covnum_1824,
                   anti_DCDH_s_mva_5_covnum_1824,
                   ylim_dol = c(-1.3, 2),ylim_num = c(-0.2,0.28))





# 2. Imputation Method ----------------------------------------------------

# Use model_fect function
# see here https://rdrr.io/cran/fect/man/fect.html


# for plot, use imputation_plot function
# https://yiqingxu.org/packages/fect/03-plots.html


## 2.1 static combined plot (modelsummary) ---------------------------------------------

pro_DCDH_s_baseline24$results

pro_DCDH_s_baseline_mplot <- list("estimate" = pro_DCDH_s_baseline24$results$ATE[1, "Estimate"],
                              "se"       = pro_DCDH_s_baseline24$results$ATE[1, "SE"],
                              "lower"   = pro_DCDH_s_baseline24$results$ATE[1, "LB CI"],
                              "upper"    = pro_DCDH_s_baseline24$results$ATE[1, "UB CI"],
                              "p.value"  = pro_DCDH_s_baseline24$results$p_jointeffects,
                              "placebo" = pro_DCDH_s_baseline24$results$p_jointplacebo)

pro_DCDH_s_num_mplot <- list("estimate" = pro_DCDH_s_num24$results$ATE[1, "Estimate"],
                         "se"       = pro_DCDH_s_num24$results$ATE[1, "SE"],
                         "lower"    = pro_DCDH_s_num24$results$ATE[1, "LB CI"],
                         "upper"    = pro_DCDH_s_num24$results$ATE[1, "UB CI"],
                         "p.value"  = pro_DCDH_s_num24$results$p_jointeffects,
                         "placebo" =pro_DCDH_s_num24$results$p_jointplacebo)


pro_imp_s_baseline_mplot <- list("estimate" = pro_fect_s$est.avg[1, "ATT.avg"],
                             "se"       = pro_fect_s$est.avg[1, "S.E."],
                             "lower"    = pro_fect_s$est.avg[1, "CI.lower"],
                             "upper"    = pro_fect_s$est.avg[1, "CI.upper"],
                             "p.value"  = pro_fect_s$est.avg[1, "p.value"],
                             "placebo" =pro_fect_s$test.out$tost.equiv.p)

pro_imp_s_num_mplot <- list("estimate" = pro_fect_s_num$est.avg[1, "ATT.avg"],
                        "se"       = pro_fect_s_num$est.avg[1, "S.E."],
                        "lower"    = pro_fect_s_num$est.avg[1, "CI.lower"],
                        "upper"    = pro_fect_s_num$est.avg[1, "CI.upper"],
                        "p.value"  = pro_fect_s_num$est.avg[1, "p.value"],
                        "placebo" =pro_fect_s_num$test.out$tost.equiv.p)


anti_imp_s_baseline_mplot <- list("estimate" = anti_fect_s$est.avg[1, "ATT.avg"],
                                 "se"       = anti_fect_s$est.avg[1, "S.E."],
                                 "lower"    = anti_fect_s$est.avg[1, "CI.lower"],
                                 "upper"    = anti_fect_s$est.avg[1, "CI.upper"],
                                 "p.value"  = anti_fect_s$est.avg[1, "p.value"],
                                 "placebo" =anti_fect_s$test.out$tost.equiv.p)

anti_imp_s_num_mplot <- list("estimate" = anti_fect_s_num$est.avg[1, "ATT.avg"],
                            "se"       = anti_fect_s_num$est.avg[1, "S.E."],
                            "lower"    = anti_fect_s_num$est.avg[1, "CI.lower"],
                            "upper"    = anti_fect_s_num$est.avg[1, "CI.upper"],
                            "p.value"  = anti_fect_s_num$est.avg[1, "p.value"],
                            "placebo" =anti_fect_s_num$test.out$tost.equiv.p)

hetero_static_list <- list("DCDH; Dollar Outcome"=pro_DCDH_s_baseline_mplot,
                           "DCDH; Number Outcome"=pro_DCDH_s_num_mplot,
                           "Imputation; Dollar Outcome; Pro-Gun" = pro_imp_s_baseline_mplot,
                           "Imputation; Number Outcome; Pro-Gun" = pro_imp_s_num_mplot,
                           "Imputation; Dollar Outcome; Anti-Gun" = anti_imp_s_baseline_mplot,
                           "Imputation; Number Outcome; Anti-Gun" = anti_imp_s_num_mplot
                           )

# set class
# class is important, because modelsummary uses broom::tidy() and broom::glance(),
# both of which use S3.

hetero_static_list <- lapply(hetero_static_list, function(x) {
  class(x) <- "hetero"
  x
})

# for modelsummary readable
# register into S3 class
# S3 is lighter and simple than S4. S3 is old
setOldClass("hetero") # conver S3 to S4

### 2.1.1 static plot --------------------------------------------------------

# If you want to avoid using function and manually extract ATT info
# hetero_att_tbl <- imap_dfr(hetero_static_list, ~ {  # for imap_dfr function, .x is content, and .y is name 
#   # iterate and output of rbind dataframe
#   # att <- .x$results$ATE
#   # pvalue <- .x$results$p_jointeffects
#   # placebo <- .x$results$p_jointplacebo
#   tibble( Outcome = .y, # add name (.y) to this var.
#           term    = "incident_window_re",  estimate = "estimate", se       = "se",
#           lower    = "lower",  upper    = "upper",  p.value  = "p.value",
#           placebo = "placebo")
# })



tidy.hetero <- function(x, ...) {
  tibble::tibble(
    term      = "incident_window_re",
    estimate  = x$estimate,
    std.error = x$se,
    p.value   = x$p.value,
    placebo   = x$placebo
  )
}

# register
registerS3method("tidy",   "hetero", tidy.hetero,   envir = asNamespace("broom"))

# glance, not tidy
# glace is for gof
glance.hetero <- function(x, ...) {
  # gof for gt-table
  tibble::tibble(
    nobs        = nrow(x$plot$data),
    # nobs = NA_integer_  # when not need to show nobs
  )
}

registerS3method("glance", "hetero", glance.hetero, envir = asNamespace("broom"))

# when pre-treatment testing results are needed
# gof_add_hetero <- data.frame(
#   raw = c("Placebo Test's p-value", "Equivalence Test's p-value", "Model", "Outcome"),
#   mod1 = c(sprintf("%.3f", DCDH_s_baseline24$results$p_jointplacebo), "","DCDH", "Dollar"),
#   mod2 = c( sprintf("%.3f", DCDH_s_num24$results$p_jointplacebo), "","DCDH","Number"),
#   mod3 = c("", sprintf("%.3f", fect_s_base$test.out$tost.equiv.p), "Imputation","Dollar"),
#   mod4 = c("", sprintf("%.3f", fect_s_number$test.out$tost.equiv.p), "Imputation","Number"),
#   stringsAsFactors = FALSE
# )

# when not
gof_add_hetero <- data.frame(
  raw = c( "Model", "Outcome", "PAC Type"),
  mod1 = c("DCDH", "Dollar", "Pro-Gun"),
  mod2 = c("DCDH","Number", "Pro-Gun"),
  mod3 = c( "Imputation","Dollar", "Pro-Gun"),
  mod4 = c( "Imputation","Number", "Pro-Gun"),
  mod5 = c( "Imputation","Number", "Gun Control"),
  mod6 = c( "Imputation","Number", "Gun Control"),
  stringsAsFactors = FALSE
)

hetero_static_results <- gt_table(hetero_static_list,coef_map_single, 
                                  c("nobs"),gof_add_hetero, 
                                  "Cumulative Impact Based on the Heterogeneity Assumption","DCDH and Imputation Method; School Shooting; HoR",
                                  source_note = ss_sourcenote, collin = FALSE) %>% print()




## 2.2 Event study ------------------------------------------------------

anti_fect_s <- model_fect(data_s_margin_baseline, "anti_log_sum")
pro_fect_s <- model_fect(data_s_margin_baseline, "pro_log_sum")

# number
anti_fect_s_num <- model_fect(data_s_margin_baseline, "anti_don_number")
pro_fect_s_num <- model_fect(data_s_margin_baseline, "pro_don_number")

imputation_plot(anti_fect_s, main='',xlim= c(-6, 12))
imputation_plot(pro_fect_s, main='',xlim= c(-6, 12))

# moving avg.
anti_fect_s_mva_logav5 <- model_fect(data_s_margin_baseline, "anti_log_sum_mva_logav5")
anti_fect_s_mva_logav5_nocov <- model_fect(data_s_margin_baseline, "anti_log_sum_mva_logav5", X=NULL)
pro_fect_s_mva_logav5 <- model_fect(data_s_margin_baseline, "pro_log_sum_mva_logav5")
pro_fect_s_mva_logav5_nocov <- model_fect(data_s_margin_baseline, "pro_log_sum_mva_logav5", X=NULL)

imputation_plot(anti_fect_s_mva_logav5, main='',xlim= c(-6, 12))
imputation_plot(anti_fect_s_mva_logav5_nocov, main='',xlim= c(-6, 12))
imputation_plot(pro_fect_s_mva_logav5, main='',xlim= c(-6, 12))
imputation_plot(pro_fect_s_mva_logav5_nocov, main='',xlim= c(-6, 12))


# after parkland
# have to exclude gun ratio
anti_fect_s_mva_logav5_1824 <- model_fect(data_s_margin_baseline_1824, "anti_log_sum_mva_logav5",
                                          X=c("bachelor","black","white","unemployment", "log_income",
                                              "rep_incumbent_before")) 
imputation_plot(anti_fect_s_mva_logav5_1824, main='',xlim= c(-6, 12))

anti_fect_s_mva_logav3_1824 <- model_fect(data_s_margin_baseline_1824, "anti_log_sum_mva_logav3",
                                          X=c("bachelor","black","white","unemployment", "log_income",
                                              "rep_incumbent_before"))
imputation_plot(anti_fect_s_mva_logav3_1824, main='',xlim= c(-6, 12))

anti_fect_s_mva_logav2_1824 <- model_fect(data_s_margin_baseline_1824, "anti_log_sum_mva_logav2",
                                          X=c("bachelor","black","white","unemployment", "log_income",
                                              "rep_incumbent_before"))
imputation_plot(anti_fect_s_mva_logav2_1824, main='',xlim= c(-6, 12))

# number outcome
anti_fect_s_mva_num5 <- model_fect(data_s_margin_baseline, "anti_don_number_mva_5")
pro_fect_s_mva_num5 <- model_fect(data_s_margin_baseline, "pro_don_number_mva_5")

anti_fect_s_mva_num5_1824 <- model_fect(data_s_margin_baseline_1824, "anti_don_number_mva_5",
                                        X=c("bachelor","black","white","unemployment", "log_income",
                                            "rep_incumbent_before"))

imputation_plot(pro_fect_s_mva_num5, main=''
                , ylab = "Number Count") # have to adjust this for number outcome
imputation_plot(anti_fect_s_mva_num5, main='', ylab = "Number Count")


# other ranges
anti_fect_s_mva_logav2 <- model_fect(data_s_margin_baseline, "anti_log_sum_mva_logav2")
pro_fect_s_mva_logav2 <- model_fect(data_s_margin_baseline, "pro_log_sum_mva_logav2")

imputation_plot(anti_fect_s_mva_logav2, main='',xlim= c(-6, 12))
imputation_plot(pro_fect_s_mva_logav2, main='',xlim= c(-6, 12))

anti_fect_s_mva_logav3 <- model_fect(data_s_margin_baseline, "anti_log_sum_mva_logav3")
pro_fect_s_mva_logav3 <- model_fect(data_s_margin_baseline, "pro_log_sum_mva_logav3")

imputation_plot(anti_fect_s_mva_logav3, main='',xlim= c(-6, 12))
imputation_plot(pro_fect_s_mva_logav3, main='',xlim= c(-6, 12))



## 2.3 combined plots --------------------------------------------------

# Use combined_imputation_plot function

pro_fect_mva_multiple <- list(
  "3_months" = pro_fect_s_mva_logav2, 
  "4_months" = pro_fect_s_mva_logav3,
  "6_months" = pro_fect_s_mva_logav5
)

anti_fect_mva_multiple <- list(
  "3_months" = anti_fect_s_mva_logav2, 
  "4_months" = anti_fect_s_mva_logav3,
  "6_months" = anti_fect_s_mva_logav5
)
  
pro_imputation_combined <- combined_imputation_plot(pro_fect_mva_multiple,
                                                    linewidth=2, end=6,
                                       ylim_low=-1.2, ylim_high=1.7) 

anti_imputation_combined <- combined_imputation_plot(anti_fect_mva_multiple,
                                                    linewidth=2, end=6,
                                                    ylim_low=-0.9, ylim_high=1.4) 

ggsave("pro_imputation_combined.png", plot= pro_imputation_combined,
       width = 14, height = 11,
       dpi = 1600,
       bg = "white",   type = "cairo")

ggsave("anti_imputation_combined.png", plot= anti_imputation_combined,
       width = 14, height = 11,
       dpi = 1600,
       bg = "white",   type = "cairo")




# 3 Imai 2023 (PanelMatch) ---------------------------------------------

# see https://github.com/insongkim/PanelMatch/tree/version3

# error of unit.id, but it can be solved by this.
# tibble is not compatible with this package.
# see https://github.com/insongkim/PanelMatch/issues/130

data_s_margin_baseline.df <- as.data.frame(data_s_margin_baseline)
data_s_margin_baseline_1824.df <- as.data.frame(data_s_margin_baseline_1824)


# Use panel_match_plot function; both for post- and pre-plot
# do not forget to use data_s_margin_baseline.df

anti_panel <- panel_match_plot(data_s_margin_baseline.df, outcome = "anti_log_sum_mva_logav5",
                               match_on_outcome=TRUE,
                             period_end=6, matching_outcome="anti_log_sum", ylim=c(-0.3, 2),
                             ylim_pre = c(-1,0.6), numeric_result = FALSE
)

# for numeric results

anti_panel_numeric <- panel_match_plot(data_s_margin_baseline.df, outcome = "anti_log_sum_mva_logav5",
                               match_on_outcome=TRUE,
                               period_end=6, matching_outcome="anti_log_sum", ylim=c(-0.3, 2),
                               ylim_pre = c(-1,0.6), numeric_result = TRUE)

anti_panel_numeric_1824 <- panel_match_plot(data_s_margin_baseline_1824.df, outcome = "anti_log_sum_mva_logav5",
                                       match_on_outcome=TRUE,
                                       period_end=6, matching_outcome="anti_log_sum", ylim=c(-0.3, 2),
                                       ylim_pre = c(-1,0.6), numeric_result = TRUE)


# pro-gun
pro_panel <- panel_match_plot(data_s_margin_baseline.df, outcome = "pro_log_sum_mva_logav5",
                               match_on_outcome=TRUE,
                               period_end=6, matching_outcome="pro_log_sum", ylim=c(-0.3, 2.5),
                               ylim_pre = c(-2.4,0.6)
)

pro_panel_numeric <- panel_match_plot(data_s_margin_baseline.df, outcome = "pro_log_sum_mva_logav5",
                                       match_on_outcome=TRUE,
                                       period_end=6, matching_outcome="pro_log_sum", ylim=c(-0.3, 2),
                                       ylim_pre = c(-1,0.6), numeric_result = TRUE)


## 3.1 Number Outcome ------------------------------------------------------

anti_panel_num <- panel_match_plot(data_s_margin_baseline.df, outcome = "anti_don_number_mva_5",
                               match_on_outcome=TRUE,
                               period_end=6, matching_outcome="anti_don_number", ylim=c(-0.05, 0.2),
                               ylim_pre = c(-0.15,0.06), numeric_result = FALSE
)

# for numeric results

anti_panel_numeric_num <- panel_match_plot(data_s_margin_baseline.df, outcome = "anti_don_number_mva_5",
                                       match_on_outcome=TRUE,
                                       period_end=6, matching_outcome="anti_don_number", ylim=c(-0.05, 0.2),
                                       ylim_pre = c(-0.15,0.06), numeric_result = TRUE)

anti_panel_numeric_num_1824 <- panel_match_plot(data_s_margin_baseline_1824.df, outcome = "anti_don_number_mva_5",
                                           match_on_outcome=TRUE,
                                           period_end=6, matching_outcome="anti_don_number", ylim=c(-0.05, 0.2),
                                           ylim_pre = c(-0.15,0.06), numeric_result = TRUE)

anti_panel_numeric_num_1824_3 <- panel_match_plot(data_s_margin_baseline_1824.df, outcome = "anti_don_number_mva_2",
                                                match_on_outcome=TRUE,
                                                period_end=6, matching_outcome="anti_don_number", ylim=c(-0.05, 0.2),
                                                ylim_pre = c(-0.15,0.06), numeric_result = TRUE)
anti_panel_numeric_num_1824_4 <- panel_match_plot(data_s_margin_baseline_1824.df, outcome = "anti_don_number_mva_3",
                                                match_on_outcome=TRUE,
                                                period_end=6, matching_outcome="anti_don_number", ylim=c(-0.05, 0.2),
                                                ylim_pre = c(-0.15,0.06), numeric_result = TRUE)


# pro-gun
pro_panel_num <- panel_match_plot(data_s_margin_baseline.df, outcome = "pro_don_number_mva_5",
                              match_on_outcome=TRUE,
                              period_end=6, matching_outcome="pro_don_number", ylim=c(-0.3, 2.5),
                              ylim_pre = c(-2.4,0.6)
)

pro_panel_numeric_num <- panel_match_plot(data_s_margin_baseline.df, outcome = "pro_don_number_mva_5",
                                      match_on_outcome=TRUE,
                                      period_end=6, matching_outcome="pro_don_number", ylim=c(-0.1, 0.2),
                                      ylim_pre = c(-1,0.6), numeric_result = TRUE)

pro_panel_numeric_num3 <- panel_match_plot(data_s_margin_baseline.df, outcome = "pro_don_number_mva_2",
                                          match_on_outcome=TRUE,
                                          period_end=6, matching_outcome="pro_don_number", ylim=c(-0.1, 0.2),
                                          ylim_pre = c(-1,0.6), numeric_result = TRUE)

pro_panel_numeric_num4 <- panel_match_plot(data_s_margin_baseline.df, outcome = "pro_don_number_mva_3",
                                           match_on_outcome=TRUE,
                                           period_end=6, matching_outcome="pro_don_number", ylim=c(-0.1, 0.2),
                                           ylim_pre = c(-1,0.6), numeric_result = TRUE)



# 4. (Removed) ------------------------------------------------------------


# 5. (Removed) ------------------------------------------------------------


# 6. Removed --------------------------------------------------------------


# 7. Removed --------------------------------------------------------------


# 7A. multiple methods' plot ----------------------------------------------

# Use multiple_hetero_dollar_plot and multiple_hetero_number_plot function

multiple_hetero_dollar_plot(anti_DCDH_s_mva_logav5_cov, anti_fect_s_mva_logav5,
                    anti_panel_numeric[[1]], anti_panel_numeric[[2]],
                    ylim = c(-1,1))

multiple_hetero_dollar_plot(pro_DCDH_s_mva_logav5_cov, pro_fect_s_mva_logav5,
                    pro_panel_numeric[[1]], pro_panel_numeric[[2]],
                    ylim = c(-1.5,2))

multiple_hetero_number_plot(anti_DCDH_s_mva_5_covnum, anti_fect_s_mva_num5,
                           anti_panel_numeric_num[[1]], anti_panel_numeric_num[[2]],
                           ylim = c(-0.12,0.16))

multiple_hetero_number_plot(pro_DCDH_s_mva_5_covnum, pro_fect_s_mva_num5,
                           pro_panel_numeric_num[[1]], pro_panel_numeric_num[[2]],
                           ylim = c(-0.2,0.2))





## 7A.0 Combined Plot -------------------------------------------------------

# Use combined_multiple_hetero_plot function

combined_multiple_hetero_plot(anti_DCDH_s_mva_logav5_cov, anti_fect_s_mva_logav5,
                           anti_panel_numeric[[1]], anti_panel_numeric[[2]],
                           ylim = c(-0.8,1.35),anti_DCDH_s_mva_5_covnum, anti_fect_s_mva_num5,
                           anti_panel_numeric_num[[1]], anti_panel_numeric_num[[2]],
                           ylim_num = c(-0.12,0.16))

combined_multiple_hetero_plot(pro_DCDH_s_mva_logav5_cov, pro_fect_s_mva_logav5,
                             pro_panel_numeric[[1]], pro_panel_numeric[[2]],
                             ylim = c(-2.1,2.3),pro_DCDH_s_mva_5_covnum, pro_fect_s_mva_num5,
                             pro_panel_numeric_num[[1]], pro_panel_numeric_num[[2]],
                             ylim_num = c(-0.24,0.28))

combined_multiple_hetero_plot(anti_DCDH_s_mva_logav5_cov_1824, anti_fect_s_mva_logav5_1824,
                             anti_panel_numeric_1824[[1]], anti_panel_numeric_1824[[2]],
                             ylim = c(-1.4,2.4),anti_DCDH_s_mva_5_covnum_1824, anti_fect_s_mva_num5_1824,
                             anti_panel_numeric_num_1824[[1]], anti_panel_numeric_num_1824[[2]],
                             ylim_num = c(-0.21,0.31))


## 7A.1 Make lists ----------------------------------------------------------

# Imputation (FECT)
pro_df_mplot_imp <- tibble(
  Time     = pro_fect_s_mva_logav5$time,
  Estimate = pro_fect_s_mva_logav5$att,
  LB.CI    = pro_fect_s_mva_logav5$att.bound[,1],
  UB.CI    = pro_fect_s_mva_logav5$att.bound[,2]
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

pro_df_mplot_pmpre <- tibble(
  Time     = as.numeric(gsub("t", "", names(pro_panel_numeric[[2]]$estimates))),
  Estimate = unname(pro_panel_numeric[[2]]$estimates),
  SE       = unname(pro_panel_numeric[[2]]$standard.errors)
) %>%
  mutate(
    LB.CI   = Estimate - qnorm(0.975)*SE,
    UB.CI   = Estimate + qnorm(0.975)*SE,
    method  = "PanelMatch",
    window  = "Pre"
  ) %>%
  dplyr::select(-SE)

# Then, we have to add a ref. point at t=-1.

pro_df_mplot_pmpre_added <- pro_df_mplot_pmpre %>%
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

pro_df_mplot_pmpost <- tibble(
  Time     = as.numeric(gsub("t", "", names(pro_panel_numeric[[1]]$estimate))),
  Estimate = unname(pro_panel_numeric[[1]]$estimate),
  SE       = unname(pro_panel_numeric[[1]]$standard.error)
) %>%
  mutate(
    LB.CI   = Estimate - qnorm(0.975)*SE,
    UB.CI   = Estimate + qnorm(0.975)*SE,
    method  = "PanelMatch",
    window  = "Post"
  ) %>%
  dplyr::select(-SE)

# DCDH
pro_df_mplot_DCDH <- tibble(
  Time     = pro_DCDH_s_mva_logav5_cov$plot$data$Time,
  Estimate = pro_DCDH_s_mva_logav5_cov$plot$data$Estimate,
  LB.CI    = pro_DCDH_s_mva_logav5_cov$plot$data$LB.CI,
  UB.CI    = pro_DCDH_s_mva_logav5_cov$plot$data$UB.CI
) %>%
  mutate(
    method = "DCDH",
    window = ifelse(Time < 0, "Pre", "Post")
  )


pro_all_df_mplot <- bind_rows(pro_df_mplot_DCDH, pro_df_mplot_imp, pro_df_mplot_pmpre_added, pro_df_mplot_pmpost)
pro_all_nopmpre_df_mplot <- bind_rows(pro_df_mplot_DCDH, pro_df_mplot_imp, 
                                  # df_mplot_pmpre_added, 
                                  pro_df_mplot_pmpost)
pro_three_df_mplot <- bind_rows(pro_df_mplot_imp, 
                                pro_df_mplot_pmpre_added,
                                pro_df_mplot_pmpost)

## 7A.2 color -------------------------------------------------------------

palette_vals_all <- c(
  "DCDH_Pre" = "#d00000",
  "DCDH_Post" = "#45afff",
  "Imputation_Pre"    = "#e1aa00",
  "Imputation_Post"   = "#4daf4a",
  "PanelMatch_Pre"    = "#fe9e9e",
  "PanelMatch_Post"   = "#8300ff"
)


## 7A.3 plot --------------------------------------------------------------


### 7A.3.1 5 patterns -------------------------------------------------------


pro_all_nopmpre_mplot <- ggplot(pro_all_nopmpre_df_mplot, aes(x = Time, y = Estimate,
                                                      color = paste(method, window, sep = "_"),
                                                      shape = window)) +
  geom_errorbar(aes(ymin = LB.CI, ymax = UB.CI),
                width    = 0.1,
                size     = 0.8,
                position = position_dodge(width = 0.6)) +
  geom_point(position = position_dodge(width = 0.5), size = 2.5) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_vline(xintercept = 0, linetype = "dotted") +
  
  coord_cartesian(xlim = c(-6, 6), ylim = c(-1.1, 2)) +
  
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
    title = "Three Methods (Excluding Pre-treatment PanelMatch); Pro-Gun",
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

pro_all_nopmpre_mplot


### 7A.3.2 All patterns -----------------------------------------------------


pro_all_mplot <- ggplot(pro_all_df_mplot, aes(x = Time, y = Estimate,
                                      color = paste(method, window, sep = "_"),
                                      shape = window)) +
  geom_errorbar(aes(ymin = LB.CI, ymax = UB.CI),
                width    = 0.1,
                size     = 0.8,
                position = position_dodge(width = 0.6)) +
  geom_point(position = position_dodge(width = 0.5), size = 2.5) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_vline(xintercept = 0, linetype = "dotted") +
  
  coord_cartesian(xlim = c(-6, 6), ylim = c(-2, 2)) +
  
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

pro_all_mplot


