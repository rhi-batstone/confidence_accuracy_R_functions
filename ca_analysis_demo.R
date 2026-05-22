library(tidyverse) 

source("ca_analysis_functions.R")

# Make some fake eywitness data
data <- data.frame(
  scale = sample(0:100, 1000, replace = TRUE),
  condition = sample(c("good", "bad"), 1000, replace = TRUE)) %>%
  mutate(conf_bins = cut(scale,  breaks = c(-Inf, 20, 40, 60, 80, Inf),
                         labels = c("Low", "Medium low", "Medium", "Medium high", "High")),
    prob_acc = case_when(
      conf_bins == "High" ~ 0.85,
      conf_bins == "Medium high" ~ 0.70,  
      conf_bins == "Medium" ~ 0.60,
      conf_bins == "Medium low" ~ 0.45,  
      conf_bins == "Low" ~ 0.35),
    conf_3 = case_when(
      scale < 60 ~ "Low",
      scale < 85 ~ "Medium",
      scale <= 100 ~ "High"),
    decision = ifelse(
      rbinom(n(), size = 1, prob = prob_acc) == 1,
      sample(c("CR", "CID"), n(), replace = TRUE),
      sample(c("TP_FID", "TA_FID", "IR"), n(), replace = TRUE)), 
    choosing = ifelse(decision %in% c("CR", "IR"), 0, 1),
    accuracy = ifelse(decision %in% c("CR", "CID"), 1, 0),
    target_presence = ifelse(decision %in% c("CR", "TA_FID"), "ta", "tp")) %>%
  select(-prob_acc)

# Define a tredoux's E value
te_mean = 5


# Calculate CAC and Calibration -------------------------------------------

cac_cali_data <- compute_cac_cali(data, 
                                  conf_col = scale,
                                  conf_bin_col = conf_bins, 
                                  decision_col = decision, 
                                  # Condition defaults to NULL
                                  condition_col = condition,
                                  # Can be "identification" or "rejection"
                                  choosing = "identification",
                                  te = te_mean)


# Plot CAC  ---------------------------------------------------------------

plot_cac(df = cac_cali_data,
         condition_col = "condition",
         conf_col = "conf_bins", #default
         cac_col = "cac", #default
         se_col = "cac_se", #default
         count_col = "cac_n", #default
         # Defaults to three levels so adding the current confidence levels (In order!)
         levels_conf = c("Low", "Medium low", "Medium", "Medium high", "High"),
         title = "CAC",
         # If FALSE (default) uses base R palette and you can add your own with + scale_colour_manual()
         viridis = TRUE, #default
         max_size = 20 #default
         )

# Or minimum code
plot_cac(cac_cali_data, "condition",
         levels_conf = c("Low", "Medium low", "Medium", "Medium high", "High"),
         title = "CAC")

# Plot Calibration --------------------------------------------------------

plot_calib(df = cac_cali_data, 
           condition = "condition", 
           conf_col = "conf_bins", #default
           cali_col = "cali", #default
           se_col = "cali_se", #default
           count_col = "cali_n", #default
           # Defaults to three levels so adding the current confidence levels (In order!)
           levels_conf = c("Low", "Medium low", "Medium", "Medium high", "High"),
           # If FALSE (default) uses base R palette and you can add your own with + scale_colour_manual()
           title = "Calibration",
           viridis = TRUE,
           max_size = 20)

# Or  
plot_calib(cac_cali_data, "condition",
         levels_conf = c("Low", "Medium low", "Medium", "Medium high", "High"),
         title = "Calibration")

# Generate formatted ICI table --------------------------------------------
## This uses the ICI_noBoot() function and formats it including overlap check

generate_ici_table(cac_cali_data, # CAC stats calculated before
                   cond_col = "condition",
                   conf_col = "conf_bins")


# Calculate calibration statistics ----------------------------------------
ids_only <- data %>% filter(choosing == 1)

CalcCalib(subset(ids_only, condition == "bad"), conf_bins = "conf_bins", e = te_mean)
CalcCalib(subset(ids_only, condition == "good"), conf_bins = "conf_bins", e = te_mean)

CalcOU(subset(ids_only, condition == "bad"), conf_bins = "conf_bins", e = te_mean)
CalcOU(subset(ids_only, condition == "good"), conf_bins = "conf_bins", e = te_mean)

CalcANDI(subset(ids_only, condition == "bad"), conf_bins = "conf_bins", e = te_mean)
CalcANDI(subset(ids_only, condition == "good"), conf_bins = "conf_bins", e = te_mean)


# Plot ROC curves ---------------------------------------------------------

plot_roc(df = data, 
         # You can list the groups you want to use, or pull them from the data
         groups = unique(data$condition), 
         # You can supply different TE's for each group or one repeated by n = # groups
         te_means = c(rep(te_mean, 2)), 
         # Supply the names of these columns if they don't match these defaults
         condition_col = "condition",
         conf_bin_col = "conf_bins",
         accuracy_col = "accuracy",
         choosing_col = "choosing",
         tp_col = "target_presence",
         # Defaults to five levels so update to match (case sensitve) - put in order!
         conf_levels = c("Low", "Medium low", "Medium", "Medium high","High"))

# Or
plot_roc(data, groups = c("good", "bad"), 
         te_means = c(te_mean, te_mean))
