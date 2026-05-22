
# Libraries ---------------------------------------------------------------

library(jtools)


# Helper functions --------------------------------------------------------


# Helper function used within other functions to remove the elading zero. 
drop_zero <- function(x) {sub("^0\\.", ".", format(x, trim = TRUE, scientific = FALSE))}


# Calculate and plot Calibration and CAC curves ---------------------------

# This function takes your data and summarises both the CAC and calibration values, based on confidence bins and conditions. 
# you need to supply:
#            data - the name of the data frame
#        conf_col - The name of your numeric confidence scale column (e.g. conf_col = "conf_3" )
#    conf_bin_col - The name of your confidence bins column (e.g. conf_col = "conf_3" )
#    decision_col - The column with lineup decision. these must be written as: CID, TP_FID, TA_FID, CR, IR (additional columns like IDK are fine)
#   condition_col - The name of your condition column if you have conditions to compare, otherwise you can leave it out. 
#    choosing_col - Is this for "identification" (default) or "rejection"
#              te - Tredoux's E (if you are using mean tredoux's e. It defaults to 6) 



compute_cac_cali <- function(df, 
                             conf_col, 
                             conf_bin_col,
                             decision_col, 
                             condition_col = NULL,
                             choosing = "identification",
                             te = 1) {
  
  # Create identity line
  ## define the relevant cases
  if (choosing == "rejection") {
    include_decisions <- c("CR", "IR")
  } else if (te == 1) {
    include_decisions <- c("TA_FID", "CID")
  } else {
    include_decisions <- c("TP_FID", "TA_FID", "CID")
  }
  
  ## Calculate the mean of the numeric scale per confidence bin depending on type of statistic
  id_line_data <- df %>%
    filter({{decision_col}} %in% include_decisions) %>% 
    group_by({{conf_bin_col}}) %>%
    summarise(
      mean_conf = mean({{conf_col}}, na.rm = TRUE) / 100, 
      .groups = "drop"
    )
  
  # Calculation the confidence accuracy statitsics CAC and Calibration simultaneously (unless for rejections, then only Calibration)
  df <- df %>% 
    group_by({{conf_bin_col}}, {{condition_col}}, {{decision_col}}) %>% 
    count() %>% 
    pivot_wider(names_from = {{decision_col}}, values_from = n)
  
  ## Calculate metrics if rejections
  if (choosing == "rejection") {
    df <- df %>% mutate(
      cali = CR/(CR+IR),
      cali_se = sqrt(cali*(1-cali)/sum(CR, IR)),
      cali_n = CR + IR
    )
  ## Calculate metrics if identifications
  } else {
    df <- df %>% mutate(
      cac = CID/(CID+(TA_FID/te)),
      cac_se = sqrt(cac*(1-cac)/sum(CID, TA_FID)),
      cac_n = CID + TA_FID,
      cali = CID/(CID+(TA_FID+TP_FID)),
      cali_se = sqrt(cali*(1-cali)/sum(CID, TA_FID, TP_FID)),
      cali_n = CID + TA_FID + TP_FID
    )
  }
  
  # Join CA stats with the identity line data
  join_col <- as.character(substitute(conf_bin_col))
  df <- df %>% left_join(id_line_data, by = join_col) %>% 
    rename(conf_bins = {{conf_bin_col}})
  
  return(df)
}


# This function plots CAC curves with ggplot
# You need to supply: 
#          data - the name of the data frame
# condition_col - The name of your condition column if you have conditions to compare, otherwise you can leave it out. 

# Defaults you don't need to supply unless they are different:
#      conf_col - the compute_cac_cali function names this "conf_bins" which is the default 
# 	    cac_col - "cac"
#        se_col - "cac_se"
# 	      title - If you want a title supply this as a string defaults to no title
#     count_col - "cac_n"
#  	levels_conf - c("Low", "Medium", "High")
#      max_size - 20 (relative value of the points for sample size)


plot_cac <- function(df,
                     condition_col = NULL,
                     conf_col = "conf_bins",
                     cac_col = "cac",
                     se_col = "cac_se",
                     count_col = "cac_n",
                     levels_conf = c("Low", "Medium", "High"),
                     title = "",
                     viridis = TRUE,
                     max_size = 20) {

  
  # factor ordering of confidence levels
  df <- df %>%
    mutate(!!conf_col := factor(.data[[conf_col]], levels = levels_conf))
  
  
  # build ggplot aesthetics depending on whether condition_col is supplied
  if (!is.null(condition_col) && condition_col %in% names(df)) {
    mapping <- aes(
      x = .data[[conf_col]],
      y = .data[[cac_col]],
      group = .data[[condition_col]],
      col = .data[[condition_col]],
      size = .data[[count_col]]
    )
    show_line_legend <- TRUE
  } else {
    mapping <- aes(
      x = .data[[conf_col]],
      y = .data[[cac_col]],
      group = 1,
      size = .data[[count_col]]
    )
    show_line_legend <- FALSE
  }
  
  # Plot the CA curves
  p <- ggplot(df, mapping) +
    geom_line(linewidth = 1.6, show.legend = show_line_legend) +
    # Add circles for sample size
    geom_point(alpha = 0.33, colour = "grey") +
    scale_size_area(
      name = "Count",
      max_size = max_size,
      breaks = range(df[[count_col]]),
      labels = range(df[[count_col]])
      ) +
    guides(size = guide_legend(
      override.aes = list(
        alpha = 0.33,
        colour = "grey",
        linetype = "blank",
        shape = 19
      ))) +
    labs(x = "Confidence", y = "Proportion correct") +
    geom_errorbar(
      aes(
        ymin = .data[[cac_col]] - .data[[se_col]],
        ymax = .data[[cac_col]] + .data[[se_col]]
      ),
      width = 0.08,
      linewidth = 1
    ) +
    scale_y_continuous(
      breaks = seq(0, 1, .1),
      labels = drop_zero,
      limits = c(0, 1.1)
    ) +
    labs(
      title = title
    ) 
  
  # Custom colors
  if(viridis == TRUE){
  # Add viridis colours if conditions are present
  if (!is.null(condition_col) && condition_col %in% names(df)) {
    n_cond <- length(unique(df[[condition_col]]))
    p <- p + scale_colour_manual(values = viridis::viridis(n_cond))
  }
  }
  
  p + theme_apa() +
    theme(axis.text = element_text(size = 16),
          axis.title.x= element_text(size = 14), 
          axis.title.y= element_text(size = 14)) +
    geom_line(aes(x = .data[[conf_col]],, y = mean_conf, group = 1), 
              linetype = "dotted", 
              color = "black",
              inherit.aes = FALSE)
  
}

# This function plots CAlibration curves with ggplot
# You need to supply: 
#          data - the name of the data frame
#      conf_col - The name of your confidence bins column (e.g. conf_col = "conf_3")
# condition_col - The name of your condition column if you have conditions to compare, otherwise you can leave it out. 

# Defaults you don't need to supply unless they are different:
# 	    cali_col - "cali"
#        se_col - "cali_se"
# 	      title - If you want a title supply this as a string defaults to no title
#     count_col - "cali_n"
#  	levels_conf - c("Low", "Medium", "High")
#      max_size - 20 (relative value of the points for sample size)
#


plot_calib <- function(df,
                       condition_col = NULL,
                       conf_col = "conf_bins",
                       cali_col = "cali",
                       se_col = "cali_se",
                       count_col = "cali_n",
                       levels_conf = c("Low", "Medium", "High"),
                       title = "",
                       viridis = TRUE,
                       max_size = 20) {
  
  # enforce factor ordering
  df <- df %>%
    mutate(!!conf_col := factor(.data[[conf_col]], levels = levels_conf))
  
  # build aesthetic mapping depending on whether condition_col is supplied
  if (!is.null(condition_col) && condition_col %in% names(df)) {
    mapping <- aes(
      x = .data[[conf_col]],
      y = .data[[cali_col]],
      group = .data[[condition_col]],
      col = .data[[condition_col]],
      size = .data[[count_col]]
    )
    show_line_legend <- TRUE
  } else {
    mapping <- aes(
      x = .data[[conf_col]],
      y = .data[[cali_col]],
      group = 1,
      size = .data[[count_col]]
    )
    show_line_legend <- FALSE
  }
  
  p <- ggplot(df, mapping) +
    geom_line(linewidth = 1.6, show.legend = show_line_legend) +
    geom_point(alpha = 0.33, colour = "grey") +
    scale_size_area(
      name = "Count",
      max_size = max_size,
      breaks = range(df[[count_col]]),
      labels = range(df[[count_col]])
    ) +
    guides(size = guide_legend(
      override.aes = list(
        alpha = 0.33,
        colour = "grey",
        linetype = "blank",
        shape = 19
      )
    )) +
    labs(x = "Confidence", y = "Proportion correct") +
    geom_errorbar(
      aes(
        ymin = .data[[cali_col]] - .data[[se_col]],
        ymax = .data[[cali_col]] + .data[[se_col]]
      ),
      width = 0.08,
      linewidth = 1
    ) +
    scale_y_continuous(
      breaks = seq(0, 1, .1),
      labels = drop_zero,
      limits = c(0, 1)
    ) +
    scale_x_discrete(labels = c(
      low = "Low",
      med = "Medium",
      high = "High"
    )) +
    labs(
      title = title
    )
  
  # Custom colors
  if(viridis == TRUE){
    # Add viridis colours if conditions are present
    if (!is.null(condition_col) && condition_col %in% names(df)) {
      n_cond <- length(unique(df[[condition_col]]))
      p <- p + scale_colour_manual(values = viridis::viridis(n_cond))
    }
  }
  
  p + theme_apa() +
    theme(axis.text = element_text(size = 16),
          axis.title.x= element_text(size = 14), 
          axis.title.y= element_text(size = 14)) +
    geom_line(aes(x = .data[[conf_col]],, y = mean_conf, group = 1), 
              linetype = "dotted", 
              color = "black",
              inherit.aes = FALSE)
  
}


# ICIs -----------------------------------------


ICI_noBoot <- function(prop1, prop2, n1, n2, num_comparisons) {
  SE1 <- sqrt(prop1 * (1 - prop1) / n1)
  SE2 <- sqrt(prop2 * (1 - prop2) / n2)
  
  #E <- sqrt((SE1^2 + SE2^2)) / (SE1 + SE2)
  #z <- abs(qnorm(0.05 / 10))  # adjust 3 to your number of comparisons
  
  #update to E calculation based oN Tryon and Lewis (2008)
  z <- abs(qnorm(0.05 / num_comparisons))
  
  SEdiff = sqrt(SE1^2+SE2^2)
  
  E <- z*SEdiff / (z*SE1 + z*SE2)
  
  Lower1 <- prop1 - z * E * SE1
  Upper1 <- prop1 + z * E * SE1
  Lower2 <- prop2 - z * E * SE2
  Upper2 <- prop2 + z * E * SE2
  
  # Create formatted interval string
  interval_str <- paste0("[", format(round(Lower1, 3), nsmall = 3), ", ", 
                         format(round(Upper1, 3), nsmall = 3), "][",
                         format(round(Lower2, 3), nsmall = 3), ", ",
                         format(round(Upper2, 3), nsmall = 3), "]")
  
  # Check if intervals overlap
  overlap <- !(Upper1 < Lower2 || Upper2 < Lower1)
  
  # Return as a list (can be accessed with $interval and $overlap)
  return(list(interval = interval_str, overlap = overlap))
}




generate_ici_table <- function(df, cond_col, conf_col = "conf_bins") {
  
  # helper function for math
  run_ici_calc <- function(prop1, prop2, n1, n2, num_comparisons) {
    SE1 <- sqrt(prop1 * (1 - prop1) / n1)
    SE2 <- sqrt(prop2 * (1 - prop2) / n2)
    z <- abs(qnorm(0.05 / num_comparisons))
    SEdiff <- sqrt(SE1^2 + SE2^2)
    E <- (z * SEdiff) / (z * SE1 + z * SE2)
    
    Lower1 <- prop1 - z * E * SE1
    Upper1 <- prop1 + z * E * SE1
    Lower2 <- prop2 - z * E * SE2
    Upper2 <- prop2 + z * E * SE2
    
    # Format to 2 digits
    fmt <- function(x) format(round(x, 2), nsmall = 2)
    interval_str <- paste0("[", fmt(Lower1), ", ", fmt(Upper1), "][",
                           fmt(Lower2), ", ", fmt(Upper2), "]")
    
    # Check if overlapping intervals
    overlap <- !(Upper1 < Lower2 || Upper2 < Lower1)
    
    return(list(interval = interval_str, overlap = overlap))
  }
  

  df %>%
    group_by(!!sym(conf_col)) %>%
    group_modify(function(df, key) {
      n_rows <- nrow(df)
      if(n_rows < 2) return(tibble()) 
      
      # Get all pair combinations
      indices <- combn(n_rows, 2, simplify = FALSE)
      num_comparisons <- length(indices)
      
      map_dfr(indices, function(pair) {
        i <- pair[1]
        j <- pair[2]
        
        res <- run_ici_calc(
          df$cac[i], df$cac[j], 
          df$cac_n[i], df$cac_n[j], 
          num_comparisons = num_comparisons
        )
        
        tibble(
          Comparison = paste(df[[cond_col]][i], "vs", df[[cond_col]][j]),
          Intervals  = res$interval,
          Overlap    = res$overlap
        )
      })
    }) %>%
    ungroup()
}


# Calib, OU, ANDI  --------------------------------------------------------


## Calibration -------------------------------------------------------------

CalcCalib = function(df, 
                     conf_bins_col = "conf_3", 
                     conf_col = "scale", 
                     accuracy_col = "accuracy",
                     e = 1,
                     bin_labels = NULL) {
  
  # 1. Auto-detect bins if specific labels aren't provided
  if (is.null(bin_labels)) {
    if (is.factor(df[[conf_bins_col]])) {
      bin_labels <- levels(df[[conf_bins_col]])
    } else {
      bin_labels <- unique(na.omit(df[[conf_bins_col]]))
    }
  }
  
  # 2. Initialize empty vectors to store our calculations
  n_bins <- length(bin_labels)
  Cj <- numeric(n_bins)
  Aj <- numeric(n_bins)
  nj <- numeric(n_bins)
  
  # 3. Loop through each dynamic bin to calculate Cj, Aj, and nj
  for (i in seq_along(bin_labels)) {
    label <- bin_labels[i]
    
    # Create subset for the current bin
    sub_df <- df[df[[conf_bins_col]] == label, ]
    
    # Count the number of cases (nj)
    nj[i] <- nrow(sub_df)
    
    # Only calculate if the bin isn't empty to avoid dividing by zero early
    if (nj[i] > 0) {
      # Calculate Cj (Mean confidence per bin)
      Cj[i] <- mean(sub_df[[conf_col]], na.rm = TRUE) / 100 
      
      # Calculate Aj (Accuracy per bin with Tredoux's e adjustment)
      acc_sum <- sum(sub_df[[accuracy_col]], na.rm = TRUE)
      err_sum <- sum(sub_df[[accuracy_col]] == 0, na.rm = TRUE)
      
      Aj[i] <- acc_sum / (acc_sum + (err_sum / e))
    }
  }
  
  # 4. If a bin had values but they were all NA, the math above yields NaN. Set to 0.
  Cj[is.nan(Cj)] <- 0
  Aj[is.nan(Aj)] <- 0
  
  ## 5. Weighted mean squared error (Calibration Index)
  # Prevent division by zero if the entire dataframe is empty
  total_n <- sum(nj)
  if (total_n == 0) return(NA)
  
  return(sum((Cj - Aj)^2 * nj) / total_n)
}

## Over/Under -------------------------------------------------------------

CalcOU = function(df, 
                  conf_bins_col = "conf_bins", 
                  conf_col = "scale", 
                  accuracy_col = "accuracy", 
                  e = 1,
                  bin_labels = NULL) {
  
  # Auto-detect bins
  if (is.null(bin_labels)) {
    if (is.factor(df[[conf_bins_col]])) {
      bin_labels <- levels(df[[conf_bins_col]])
    } else {
      bin_labels <- unique(na.omit(df[[conf_bins_col]]))
    }
  }
  
  n_bins <- length(bin_labels)
  Cj <- numeric(n_bins)
  Aj <- numeric(n_bins)
  nj <- numeric(n_bins)
  
  # Loop through bins
  for (i in seq_along(bin_labels)) {
    label <- bin_labels[i]
    sub_df <- df[df[[conf_bins_col]] == label, ]
    nj[i] <- nrow(sub_df)
    
    if (nj[i] > 0) {
      # Calculate Cj
      Cj[i] <- mean(sub_df[[conf_col]], na.rm = TRUE) / 100 
      
      # Calculate Aj with Tredoux's e
      acc_sum <- sum(sub_df[[accuracy_col]], na.rm = TRUE)
      err_sum <- sum(sub_df[[accuracy_col]] == 0, na.rm = TRUE)
      Aj[i] <- acc_sum / (acc_sum + (err_sum / e))
    }
  }
  
  Cj[is.nan(Cj)] <- 0
  Aj[is.nan(Aj)] <- 0
  
  total_n <- sum(nj)
  if (total_n == 0) return(NA)
  
  # Calculate O/U
  return(sum((Cj - Aj) * nj) / total_n)
}


## ANDI -------------------------------------------------------------
## Adjusted Normalised Discrimination Index (ANDI) 

CalcANDI = function(df, 
                    conf_bins_col = "conf_3", 
                    conf_col = "scale", 
                    accuracy_col = "accuracy",
                    e = 1, 
                    bin_labels = NULL) {
  
  # Auto-detect bins
  if (is.null(bin_labels)) {
    if (is.factor(df[[conf_bins_col]])) {
      bin_labels <- levels(df[[conf_bins_col]])
    } else {
      bin_labels <- unique(na.omit(df[[conf_bins_col]]))
    }
  }
  
  n_bins <- length(bin_labels)
  J <- n_bins # J is dynamically set to the number of bins
  
  Aj <- numeric(n_bins)
  nj <- numeric(n_bins)
  
  # Trackers for overall accuracy (dmean)
  total_acc <- 0
  total_err <- 0
  
  # Loop through bins
  for (i in seq_along(bin_labels)) {
    label <- bin_labels[i]
    sub_df <- df[df[[conf_bins_col]] == label, ]
    nj[i] <- nrow(sub_df)
    
    if (nj[i] > 0) {
      # Bin-specific accuracy sums
      acc_sum <- sum(sub_df[[accuracy_col]], na.rm = TRUE)
      err_sum <- sum(sub_df[[accuracy_col]] == 0, na.rm = TRUE)
      
      # Calculate Aj for the bin
      Aj[i] <- acc_sum / (acc_sum + (err_sum / e))
      
      # Accumulate global sums for dmean
      total_acc <- total_acc + acc_sum
      total_err <- total_err + err_sum
    }
  }
  
  Aj[is.nan(Aj)] <- 0
  total_n <- sum(nj)
  if (total_n == 0) return(NA)
  
  # Calculate overall weighted accuracy (dmean)
  dmean <- total_acc / (total_acc + (total_err / e))
  if (is.nan(dmean)) dmean <- 0
  
  # ANDI Formulas
  DI <- (1 / total_n) * sum(nj * (Aj - dmean)^2)
  vard <- dmean * (1 - dmean)
  
  # Prevent division by zero if variance is 0
  if (vard == 0) return(NA)
  
  NDI <- DI / vard
  ANDI <- (total_n * NDI - J + 1) / (total_n - J + 1)
  
  return(ANDI)
}


# ROC Curves --------------------------------------------------------------

plot_roc <- function(df, 
                     groups = c("good", "mixed", "poor"), 
                     # Named vector mapping groups to their te_mean
                     te_means = c("a" = 1, "b" = 1), 
                     condition_col = "condition",
                     conf_bin_col = "conf_bins",
                     accuracy_col = "accuracy",
                     choosing_col = "choosing",
                     tp_col = "target_presence",
                     conf_levels = c("Low", "Medium low", "Medium", "Medium high", "High") 
) {
  
  # Filter to selected groups
  df <- df %>% filter(.data[[condition_col]] %in% groups)
  
  # Ensure te_means is properly mapped even if names aren't provided
  if (is.null(names(te_means))) {
    if(length(te_means) != length(groups)) {
      stop("Length of 'te_means' must match the number of 'groups' if names are not provided.")
    }
    te_means <- setNames(te_means, groups)
  }
  
  # Calculate base rates (n_tp, n_ta) per group
  base_rates <- df %>%
    group_by(.data[[condition_col]]) %>%
    summarise(
      n_tp = sum(.data[[tp_col]] %in% c("tp", 1), na.rm = TRUE),
      n_ta = sum(.data[[tp_col]] %in% c("ta", 0), na.rm = TRUE),
      .groups = "drop"
    ) %>%
    rename(condition = !!sym(condition_col))
  
  # Process ROC data for all groups 
  roc_data <- df %>%
    mutate(
      condition = as.character(.data[[condition_col]]),
      conf_bin = factor(.data[[conf_bin_col]], levels = conf_levels)
    ) %>%
    group_by(condition, conf_bin) %>%
    summarise(
      cid = sum(.data[[accuracy_col]] == 1 & .data[[tp_col]] %in% c("tp", 1), na.rm = TRUE),
      fid_raw = sum(.data[[choosing_col]] == 1 & .data[[tp_col]] %in% c("ta", 0), na.rm = TRUE),
      .groups = "drop"
    ) %>%
    # Bring in group-specific base rates and te_means
    left_join(base_rates, by = "condition") %>%
    mutate(
      te_mean = te_means[condition],
      fid = fid_raw / te_mean
    ) %>%
    arrange(condition, conf_bin) %>%
    group_by(condition) %>%
    mutate(
      cid_cum = rev(cumsum(rev(cid))),
      fid_cum = rev(cumsum(rev(fid))),
      cid_rate = cid_cum / n_tp,
      fid_rate = fid_cum / n_ta
    ) %>%
    ungroup()
  
  # Create the (0,0) anchor points for all curves
  zero_points <- roc_data %>%
    group_by(condition) %>%
    slice(1) %>%
    mutate(
      conf_bin = "zero", 
      cid_rate = 0, 
      fid_rate = 0
    ) %>%
    ungroup()
  
  # Convert conf_bin to character in both to avoid factor binding warnings
  roc_data <- roc_data %>% mutate(conf_bin = as.character(conf_bin))
  
  # Combine and Plot
  plot_data <- bind_rows(roc_data, zero_points)
  
  roc_plot <- ggplot(plot_data, aes(x = fid_rate, y = cid_rate, group = condition, col = condition)) +
    geom_point(size = 3) +
    geom_line(linewidth = 2) + 
    geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "grey50") +
    labs(x = "False ID Rate", y = "Correct ID Rate", color = "Condition") +
    scale_colour_manual(values = viridis::viridis(length(groups)))
  
  # Apply theme_apa 
    roc_plot <- roc_plot + theme_apa(y.font.size = 14, x.font.size = 14)
  
  return(roc_plot)
}
