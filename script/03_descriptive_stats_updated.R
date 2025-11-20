# Descriptive Stats

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
# # "gunPAC",
# "PNAS_script", "02_data_wrangling.R")) # can skip this
# skip_data_wrangling(TRUE) # can skip this as well later


# 1 Scatter plot ------------------------------------------------------

data_control_var <- data_s_margin_baseline %>%
  dplyr::select(bachelor,black, white, unemployment, log_income, 
                gun_ratio_pct, incident_window_re) %>% 
  mutate(Treatment=factor(incident_window_re, levels = c(0, 1),
                                          labels = c("Control", "Treated"))) %>% 
  dplyr::select(-incident_window_re) # we need only factor variable

scatter_labels <- c("Bachelor",  "Black", "White",
               "Unemployment", "Income", "Gun Ratio")

# use cor_fun_colored function only for upper-right part

# commented out since it takes time

# scatter_plot <- GGally::ggpairs(data_control_var,
#         columns = which(sapply(data_control_var, is.numeric)), # only choose numeric
#         columnLabels = scatter_labels,
#         # title   = "Scatter Plot (Competitive Districts)",
#         title = "",
#         mapping = aes(colour = Treatment),   # different color by treatment
#         # upper = list(continuous = wrap("cor", size = 2.2, stars = FALSE)), # for top right part, but using only gray color for all
#         upper = list(continuous = cor_fun_colored),
#         lower = list(continuous = wrap("points", alpha = 0.7)), # for bottom left part; no need of smooth
#         legend = 1,
#         diag = list(continuous = wrap("densityDiag", alpha = 0.4))
# )+
#   theme_minimal() +
#   theme(
#     # plot.title = element_text(hjust = 0.5,size  = 14, face  = "bold"),
#     axis.text = element_text(size = 12),
#     strip.text = element_text(size = 14),
#     legend.position = "bottom",
#     legend.title = element_text(size = 14, face = "bold"),
#     legend.text = element_text(size = 14)
#   )+
#   scale_colour_manual(
#     name = "Treatment",
#     labels = c("Treated", "Control"),
#     values = c("Treated" = "lightblue", "Control" = "lightcoral")  # Your custom colors
#   ) %>% print()
# 
# ggsave("scatter_plot.png", plot = scatter_plot,
#        width = 18, height = 14,  dpi = 1500, # 600 is on a fine level
#        units = "in",   bg = "white",  # White background
#        type = "cairo")

## 1.1 Violin Plots of Covariates---------------------------------------------------------

### 1.1.1 Race ----------------------------------------------------------

# Use violin_balance_plot_race function

# violin_balance_plot_race(data_s_2000, "SS Dataset")
violin_race <- violin_balance_plot_race(data_s_margin_baseline, 
                         # "Covariate Balance: School Shooting (Basic Filtering)"
                         NULL,NULL
)

# ggsave("RACE_violinplot.png", plot = violin_race,
#        # width = 10, height = 10,
#        dpi = 1600,
#        # units = "in",
#        bg = "white",    type = "cairo")

### 1.1.2 Non-race info -------------------------------------------------

# incumbency does not need violin plot, since it is categorical and the table is enough
# use violin_balance_plot_nonrace function

# violin_balance_plot_nonrace(data_s_2000, "Whole Districts")
violin_nonrace1 <- violin_balance_plot_nonrace(data_s_margin_baseline, 1, 
                            # "School Shooting (Basic Filtering)"
                            NULL, NULL) %>% print()
violin_nonrace2 <- violin_balance_plot_nonrace(data_s_margin_baseline, 2, 
                            # "School Shooting (Basic Filtering)"
                            NULL,NULL) %>% print()

# ggsave("unemployment_income_violinplot.png", plot = violin_nonrace1,
#        width = 12, height = 10,
#        dpi = 1600,  bg = "white",   type = "cairo")
# 
# ggsave("violin_Bachelor.png", plot = violin_nonrace2,
#        width = 12, height = 10,
#        dpi = 1600,   bg = "white",  type = "cairo")

# 2. Descriptive Table ----------------------------------------------------

# use desc_table_2yr function

# school shooting
desc_table_2yr(data_s_2000, "School Shooting (All Districts)")
desc_table_2yr(data_s_margin_baseline, "School Shooting (Competitive Districts)")

# mass shooting
# desc_table_2yr(data_m_2000, "MJ Dataset (All Districts)")
desc_table_2yr(data_m_margin_baseline, "MJ Dataset (Competitive Districts)")

# desc_table_2yr(data_g_2000, "GVA Dataset (All Districts)")
desc_table_2yr(data_g_margin_baseline, "GVA Dataset (Competitive Districts)")

# desc_table_2yr(data_v_2000, "VPP Dataset (Whole Data Without Filtering)")
desc_table_2yr(data_v_margin_baseline, "VPP Dataset (Competitive Districts)")



# 3 Panel view ------------------------------------------------------

# see https://github.com/fhollenbach/did_compare/blob/main/ComparingDiD.md, and
# https://yiqingxu.org/packages/panelview/panelView_1.1.6.html
# Note that this is panelview, not panelView, which is mentioned as in each website

# for more details, see help("panelview")


# use panelview_function function

# If not needed, comment this out since it takes time
# competitive districts
ss_baseline_treatment <- panelview_function(data_s_margin_baseline,
                                                 # title="Treatment Status (School Shooting; Basic Filtering)"
                                                 title="", competitive = TRUE) %>% print()

# ggsave("panel_view_competitive.png", plot = ss_baseline_treatment,
#        width = 17, height = 12,
#        dpi = 1600,  bg = "white",   type = "cairo")
# 
# whole districts
# It should use a filtered data, since we should focus on fatal school shootings

ss_whole_treatment <- panelview_function(data_s_2000_filtered,
                                         title="", competitive = FALSE) %>% print()
# 
# ggsave("panel_view_whole.png", plot = ss_whole_treatment,
#        width = 17, height = 12,
#        dpi = 1500,  bg = "white",   type = "cairo")


# 4 Contribution ----------------------------------------------------------


## 4.0 Monthly Dollar Contribution's Histogram --------------------------------------------------------

# Focusing on positive values of gun policy contributions
# Use log10_sum_plot function


GLobby_hist_month <- log10_sum_plot(data_s_margin_baseline, NULL,NULL,
                     # subtitle = "Gun Lobby PAC Contributions; Competitive Districts",
                     pattern = 'pro', change_color = TRUE) %>% print() # salmon color for gun lobby

# ggsave("GLobby_hist_month.png", plot = GLobby_hist_month,
#        width = 12, height = 14,
#        dpi = 1500,  bg = "white",   type = "cairo")

# GVP_hist_month <- log10_sum_plot(data_s_margin_baseline, NULL,NULL,
#                # subtitle = "GVP PAC Contributions; competitive districts",
#                pattern = 'anti') %>% print()


# whole, unfiltered districts
log10_sum_plot(data_s_2000,
                     NULL,NULL,
                     # subtitle = "Gun Lobby PAC Contributions; Whole Districts",
                     pattern = 'pro', filtered=FALSE)
# 
log10_sum_plot(data_s_2000,
                NULL,NULL,
                   filtered=FALSE,
               # subtitle = "GVP PAC Contributions; whole",
               pattern = 'anti', change_color = TRUE)


## 4.1 Overlay Density Plot --------------------------------------------

# Use log10_sum_density_both function

# whole districts
log10_sum_density_both(data_s_2000, 
                       # subtitle = "Anti- vs Pro- Gun PAC Contributions; Unfiltered Data",
                       NULL,NULL,
                       mean=TRUE, filtered=FALSE)
                        
# competitive districts
# filtered should be TRUE, which is a default
comp_logsum_density <- log10_sum_density_both(data_s_margin_baseline, NULL,NULL,
                       # subtitle = "Anti- vs Pro- Gun PAC Contributions; Competitive Districts",
                       mean = TRUE) %>% print()

# ggsave("comp_log_density.png", plot = comp_logsum_density,
#        width = 10, height = 10,
#        dpi = 1300,  bg = "white",
#        type = "cairo")


# 5. Geo-distribution -----------------------------------------------------


## 5.0 Donation and Shooting ------------------------------------------

# use anti_pro_geo_var_plot function
# whole districts

anti_geo_dollar <- anti_pro_geo_var_plot(data_s_2000, "anti_dollar",
                                         # "Aggregated Anti-gun Contributions (since 2000)",
                                         # "Unfiltered Data; HoR"
                                         NULL,NULL
)

pro_geo_dollar <- anti_pro_geo_var_plot(data_s_2000, "pro_dollar",
                                        # "Aggregated Pro-gun Contributions (since 2000)",
                                        # "Unfiltered Data; HoR"
                                        NULL,NULL
)

geo_SS <- anti_pro_geo_var_plot(data_s_2000, "shooting",
                                # "Aggregated School Shooting (since 2000)", 
                                # "School Shooting; Whole Data"
                                NULL,NULL
)

geo_dollar_plot <- grid.arrange(anti_geo_dollar, pro_geo_dollar, geo_SS, nrow=3)


# ggsave("geoplot.png", plot = geo_dollar_plot,
#        width = 10, height = 10,
#        dpi = 1300,  bg = "white",
#        type = "cairo")

# number plot
anti_geo_number <- anti_pro_geo_var_plot(data_s_2000, "anti_number",
                                         NULL,NULL
)

pro_geo_number <- anti_pro_geo_var_plot(data_s_2000, "pro_number",
                                        NULL,NULL
)

geo_number_plot <- grid.arrange(anti_geo_number, pro_geo_number, geo_SS, nrow=3) %>% print()

# ggsave("geoplot_number.png", plot = geo_number_plot,
#        width = 12, height = 10,
#        dpi = 1500,    bg = "white",
#        type = "cairo")

## 5.1 Side-by-side circles for Both Wings --------------------------------------------

# Use anti_pro_combined_geo_plot function

# before and after 2018, since GVP changed a lot after 2018

anti_pro_geo_2000_2017 <- anti_pro_combined_geo_plot(
  data_s_whole_2000_2017, NULL, NULL, "before"
  # "Anti-gun vs Pro-gun Contributions by State (2000-2017)",
  # "Unfiltered Data; HoR"
)

anti_pro_geo_2018_2024 <- anti_pro_combined_geo_plot(
  data_s_whole_2018_2024,NULL, NULL, "after", 
  FALSE # only color legend at the first plot
  # "Anti-gun vs Pro-gun Contributions by State (2018-2024)",
  # "Unfiltered Data; HoR"
)

geo_side <- grid.arrange(anti_pro_geo_2000_2017, anti_pro_geo_2018_2024, nrow=2)

# ggsave("geo_side.png", plot = geo_side,
#        width = 12, height = 9,
#        dpi = 1500,
#         bg = "white",   type = "cairo")


# 6 Time series (Only Contribution) --------------------------------------------------------

# Use time_to_YMD function. Time starts with 1 at 1990 Jan.

## 6.1 Nationwide Contribution --------------------------------------------------------

###  6.1.1 Annual Contribution ---------------------------------------------------------

# use dollar_timeseries_nation_antipro function
# this function is used for whole districts, since it uses raw donation variables, instead of filtered variables

annual_both_absdollar <- dollarcount_timeseries_nation_antipro(data_s_2000, NULL,NULL,
                                                          # # "Whole Data", 
                                                          # subtitle = "Annual; Unfiltered Data",
                                                          legend=TRUE, outcome ="abs_dollar") %>% print()
# 
# ggsave("annual_both_absdollar.png", plot = annual_both_absdollar,
#        width = 12, height = 9,
#        dpi = 1500,
#        bg = "white",type = "cairo")

annual_both_number <- dollarcount_timeseries_nation_antipro(data_s_2000, NULL,NULL,
                                                               # # "Whole Data", 
                                                               # subtitle = "Annual; Unfiltered Data",
                                                               legend=TRUE, outcome ="number") %>% print()

# ggsave("annual_both_number.png", plot = annual_both_number,
#        width = 12, height = 9,
#        dpi = 1500,
#        bg = "white",  type = "cairo")

# logged outcome output (unused)
# annual_both_logdollar <- dollarcount_timeseries_nation_antipro(data_s_2000, 
#                                                           # "Whole Data", 
#                                                           subtitle = "Log Form; Annual; Unfiltered Data",
#                                                           legend=TRUE, outcome = "log_dollar")
# annual_both_logdollar


### 6.1.2 Monthly Contribution ----------------------------------------------

# use monthly_timeseries_nation funtion
# also for whole districts

monthly_both_number <- monthly_timeseries_nation(data_s_2000, NULL,NULL,
                                                 # # "Whole Data", 
                                                 # subtitle = "Annual; Unfiltered Data",
                                                 legend=TRUE, outcome ="number") %>% print()

# ggsave("number_monthly_antipro.png", plot = monthly_both_number,
#        dpi = 1500,
#        bg = "white",
#        type = "cairo")

# since too many zero values, logged dollar value would be better.

monthly_both_logdollar <- monthly_timeseries_nation(data_s_2000, NULL,NULL,
                                                    # # "Whole Data", 
                                                    # subtitle = "Annual; Unfiltered Data",
                                                    legend=TRUE, outcome ="log_dollar") %>% print()


# ggsave("monthly_plot_cyclic.png", plot = monthly_both_logdollar,
#        dpi = 1500,
#        bg = "white",
#        type = "cairo")


# 7. Time Series Plot (Treatment and Contribution) -------------------------------------------------------------------

# use antipro_donation_incident_double_whole_plot function
# whole district and log form

anti_shooting_plot <- antipro_donation_incident_double_whole_plot(data_s_2000, "log_dollar", 
                                  # "School Shootings and Anti-gun Contributions",
                                  # "Log form; Unfiltered Data"
                                  NULL,NULL
) %>% print()

# 
# ggsave("GVP_shooting.png", plot = anti_shooting_plot,
#        width = 12, height = 9,
#        dpi = 1500, bg = "white", type = "cairo")

pro_shooting_plot <- antipro_donation_incident_double_whole_plot(data_s_2000, "log_dollar", 
                                                            # "School Shootings and Anti-gun Contributions",
                                                            # "Log form; Unfiltered Data"
                                                            NULL,NULL, "pro"
) %>% print()
 
# ggsave("gun_lobby_shooting.png", plot = pro_shooting_plot,
#        width = 12, height = 9,
#        dpi = 1500,
#        bg = "white",   type = "cairo")


## 7.1 Competitive Districts and fatal shootings ---------------------------

# use antipro_donation_incident_double_comp_plot or donation_incident_double_comp_cycle_plot function
# as opposed to whole_plot function, different filtered variable is used
anti_shooting_comp <- antipro_donation_incident_double_comp_plot(data_s_margin_baseline, "log_dollar", 
                                                            # "School Shootings and Anti-gun Contributions",
                                                            # "Log form; Unfiltered Data"
                                                            NULL,NULL
) %>% print()

anti_shooting_comp_cycle <- donation_incident_double_comp_cycle_plot(data_s_margin_baseline, "log_dollar", 
                                                                 # "School Shootings and Anti-gun Contributions",
                                                                 # "Log form; Unfiltered Data"
                                                                 NULL,NULL
) %>% print()


ggsave("GVP_shooting_comp.png", plot = anti_shooting_comp_cycle,
       width = 12, height = 9,
       dpi = 1500, bg = "white", type = "cairo")

pro_shooting_comp_cycle <- donation_incident_double_comp_cycle_plot(data_s_margin_baseline, "log_dollar", 
                                                           # "School Shootings and Anti-gun Contributions",
                                                           # "Log form; Unfiltered Data"
                                                           NULL,NULL, "pro"
) %>% print()

ggsave("gun_lobby_shooting_comp.png", plot = pro_shooting_comp_cycle,
       width = 12, height = 9,
       dpi = 1500,
       bg = "white",   type = "cairo")



## 7.2 mass shooting plot for three datasets --------------------

# only for mass shooting;  not for contributions or incidents, which sounds messy
# for one y axis, show GVA; for the other y axis, show MJ and VPP

# use three_mass_plot function; be careful with the function's argument
# whole data

mass_doubleplot_three <- three_mass_plot(
  list(data_m_2000, data_v_2000),c("MJ", "VPP"),data_g_2000,
  # subtitle = "Three Datasets; Whole Data"
  title = NULL, subtitle = NULL
)

# ggsave("mass_doubleplot_three.png", plot = mass_doubleplot_three,
#        width = 12, height = 9,
#        dpi = 1500,
#        bg = "white",   type = "cairo")


# 8 Histogram of Competitiveness ----------------------------------------------


mean_margin <- mean(data_s_2000$pct_RDmargin_before, na.rm = TRUE)

comp_hist_plot <- ggplot(data_s_2000, aes(x = pct_RDmargin_before)) +
  geom_histogram(
    binwidth = 5, boundary= -100,
    fill = "lightblue", color = "black", alpha = 0.7) +
  geom_vline(xintercept = mean_margin, color = "gray40", linetype = "dashed", linewidth = 0.8) +
  
  geom_vline(xintercept = 5, color = "tomato",  linewidth = 0.8) +
  geom_vline(xintercept = -5, color = "tomato",  linewidth = 0.8) +
  
  annotate("text", x = -43, y = 6000,
           label = sprintf("Mean: %.1f%%", mean_margin),
           color = "gray40", vjust = -5, hjust = -0.1, fontface = "bold", size=7.5) +
  annotate("text", x = -35, y = 6000, 
           label = "-5% ~ 5%", 
           color = "tomato", vjust = 0, hjust = 0, fontface = "bold", size=7.5) +
  
  scale_x_continuous(
    breaks = seq(-100, 100, by = 20)
    # second x axis will not be needed, since it is self-evident
    # sec.axis = sec_axis(
    #   ~ .,
    #   breaks = c(log10(5000), log10(10000)),
    #   labels = c("$5,000", "$10,000"))
  ) +
  labs(
    # title = "Distribution of Voting Margins for Each District",
    tile=NULL,
    # subtitle = "Difference between Rep. and Dem. Candidates",
    subtitle=NULL,
    x = "Margin (%)",
    y = "") +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5, size = 12),
    plot.subtitle = element_text(hjust = 0.5, size = 10),
    axis.ticks.y = element_blank(),
    axis.text.y = element_blank(),
    axis.text.x = element_text(size=13),
    axis.title.x = element_text(face = "bold", size = 13),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
  )

# ggsave("comp_hist_plot.png", plot = comp_hist_plot,
#        width = 12, height = 9,
#        dpi = 1500,
#        bg = "white",   type = "cairo")



## 8.1 how many competitive districts? ---------------------------------

# first, total number
# 5613
data_s_election <- data_s_2000 %>% 
  filter(months_to_election==0)
dim(data_s_election)

# 350 competitive elections
data_s_competitive_election <- data_s_election %>% 
  filter(abs(pct_RDmargin_before)<=5)
dim(data_s_competitive_election)

350/5613



# 9 Weighted avg of margin ----------------------------------------------

# Use weighted_margin_timeseries_whole function

whole_weighted_margin_plot <- weighted_margin_timeseries_whole(data_s_2000, NULL, NULL, FALSE)

# ggsave("weighted_margin.png", plot = whole_weighted_margin_plot,
#        width = 11, height = 9,
#        dpi = 1500,
#        bg = "white",   type = "cairo")




## 9.1 Scatter Plot of Competitiveness and contributions -----------------------------------

# use contribution_scatter_by_margin_whole function

total_donations_scatter <- contribution_scatter_by_margin_whole(data_s_2000, NULL, NULL)

# ggsave("total_donations_scatter.png", plot = total_donations_scatter,
#        width = 11, height = 9,
#        dpi = 1500,
#        bg = "white",   type = "cairo")


total_donations_scatter_abs <- contribution_scatter_by_margin_whole(data_s_2000,NULL,NULL, 
                                                              outcome = "dollar",
                                                              y_label = "Dollar Value (Non-log)", ylim=10000)

# ggsave("total_donations_scatter_abs.png", plot = total_donations_scatter_abs,
#        width = 11, height = 9,
#        dpi = 1500,
#        bg = "white",   type = "cairo")


