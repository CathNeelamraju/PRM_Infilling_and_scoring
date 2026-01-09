rm(list = ls())

#libraries----------
library(tidyverse)
library(dplyr)

# Load data-----
# Q data
setwd("C:/Users/uqcneela/OneDrive - The University of Queensland/General - Sci SEES Res Reef Catchment Science Partnership/Project 3/Catherine Neelamraju/Infilling/MICE_revised test")
Q_r2_data <- read_csv("compiled_model_r2_summary_Q.csv")
names(Q_r2_data)[1] <- "Site.Code" # Fix name of first column
names(Q_r2_data)#check
pooled_Q_data <- read_csv("all_pooled_pmm_results_Q.csv")# Pooled model data
str(pooled_Q_data) # check

# WL data
WL_r2_data <- read_csv("compiled_model_r2_summary_WL.csv")
names(WL_r2_data)[1] <- "Site.Code" # Fix name of first column
names(WL_r2_data)#check
pooled_WL_data <- read_csv("all_pooled_pmm_results_WL.csv")# Pooled model data
str(pooled_WL_data) # check

# Site-wise correlations
correlation_data <- read_csv("SiteWise_Correlations.csv")
str(correlation_data)

# Site names 
setwd("C:/Users/uqcneela/OneDrive - The University of Queensland/General - Sci SEES Res Reef Catchment Science Partnership/Project 3/Catherine Neelamraju/Infilling")
SiteDat_all <- read.csv("PRM_with all Q and WL_FINAL.csv", header = TRUE)
setwd("C:/Users/uqcneela/OneDrive - The University of Queensland/General - Sci SEES Res Reef Catchment Science Partnership/Project 3/Catherine Neelamraju/Infilling/MICE_revised test")
SiteDat <- unique(SiteDat_all[, 1:4]) # filter to unique site data onlu

# --- Helper scoring functions ---
Q_score_r2 <- function(r2) {
  case_when(
    r2 >= 0.70 ~ 10,
    r2 >= 0.50 ~ 7,
    r2 >= 0.30 ~ 4,
    TRUE       ~ 0
  )
}

score_stability <- function(se_pct, ci_pct) {
  case_when(
    se_pct < 20 & ci_pct < 30 ~ 10,
    (se_pct >= 20 & se_pct < 40) | (ci_pct >= 30 & ci_pct < 60) ~ 7,
    (se_pct >= 40 & se_pct < 60) | (ci_pct >= 60 & ci_pct <= 100) ~ 4,
    se_pct > 60 | ci_pct > 100 ~ 0,
    TRUE ~ NA_real_
  )
}

score_imputation <- function(fmi, riv, lambda) {
  case_when(
    fmi < 0.50 & riv < 1.0 & lambda < 1.0 ~ 10,
    (fmi >= 0.50 & fmi < 0.70) | (riv >= 1.0 & riv < 2.0) ~ 7,
    (fmi >= 0.70 & fmi < 0.90) | (riv >= 2.0 & riv <= 5.0) ~ 4,
    fmi > 0.90 | riv > 5.0 ~ 0,
    TRUE ~ NA_real_
  )
}

# --- Ecological plausibility rules ---
plausibility_rules <- tribble(
  ~term_pattern, ~plausible_min, ~plausible_max, ~allow_negative,
  "Intercept",   0,   80,  FALSE,
  "Discharge_cumecs",  -1,    1,  TRUE,
  "WaterLevel_m", -40, 40, TRUE,
  "Rel.Month", -60,   10,  TRUE
)

get_bounds <- function(term) {
  rule <- plausibility_rules %>%
    filter(str_detect(term, term_pattern)) %>%
    slice(1)
  
  if (nrow(rule) == 0) {
    list(plausible_min = -Inf, plausible_max = Inf, allow_negative = TRUE)
  } else {
    list(
      plausible_min = rule$plausible_min,
      plausible_max = rule$plausible_max,
      allow_negative = rule$allow_negative
    )
  }
}

score_ecological <- function(term, estimate, conf.low, conf.high) {
  bounds <- get_bounds(term)
  min_val <- bounds$plausible_min
  max_val <- bounds$plausible_max
  allow_neg <- bounds$allow_negative
  
  ci_within <- conf.low >= min_val & conf.high <= max_val
  est_within <- estimate >= min_val & estimate <= max_val
  
  case_when(
    ci_within & (allow_neg | estimate >= 0) ~ 10,
    est_within & !ci_within ~ 7,
    !est_within & ci_within ~ 4,
    !est_within & !ci_within ~ 0,
    TRUE ~ NA_real_
  )
}

# --- Wrapper function for scoring workflow ---
score_models <- function(r2_df, pooled_df, model_type) {
  
  # R² scores
  r2_scores <- r2_df %>%
    filter(Target == "Total.PRM") %>%
    mutate(
      R2_used = coalesce(R2_conditional, R2_marginal),
      R2_score = Q_score_r2(R2_used)
    ) %>%
    group_by(Site.Code) %>%
    summarise(R2_score = mean(R2_score, na.rm = TRUE), .groups = "drop")
  
  # Stability scores
  stability_scores <- pooled_df %>%
    filter(Target == "Total.PRM") %>%
    mutate(
      se_pct = 100 * abs(std.error / estimate),
      ci_width = abs(conf.high - conf.low),
      ci_pct = 100 * abs(ci_width / estimate),
      stability_score = score_stability(se_pct, ci_pct)
    ) %>%
    group_by(Site.Code) %>%
    summarise(CoeffStability = mean(stability_score, na.rm = TRUE), .groups = "drop")
  
  # Imputation scores
  imputation_scores <- pooled_df %>%
    filter(Target == "Total.PRM") %>%
    mutate(imputation_score = score_imputation(fmi, riv, lambda)) %>%
    group_by(Site.Code) %>%
    summarise(ImputationQuality = mean(imputation_score, na.rm = TRUE), .groups = "drop")
  
  # Ecological plausibility
  ecological_scores <- pooled_df %>%
    filter(Target == "Total.PRM") %>%
    mutate(
      eco_score = pmap_dbl(list(term, estimate, conf.low, conf.high),
                           ~score_ecological(..1, ..2, ..3, ..4))
    ) %>%
    group_by(Site.Code) %>%
    summarise(Plausibility = mean(eco_score, na.rm = TRUE), .groups = "drop")
  
  # Combine all
  all_scores <- r2_scores %>%
    left_join(stability_scores, by = "Site.Code") %>%
    left_join(imputation_scores, by = "Site.Code") %>%
    left_join(ecological_scores, by = "Site.Code") %>%
    mutate(
      total_score = R2_score + CoeffStability + ImputationQuality + Plausibility,
      Grade = case_when(
        total_score >= 31 ~ "Excellent (Strong fit, stable estimates, reliable imputations)",
        total_score >= 21 ~ "Good (Acceptable fit, some uncertainty but usable)",
        total_score >= 11 ~ "Fair (Weak fit or unstable imputations; caution advised)",
        total_score >= 0  ~ "Poor (Reject model; not suitable for inference)",
        TRUE ~ NA_character_
      ),
      # override: force Poor if R2_score == 0
      Grade = if_else(R2_score == 0, "Poor (forced by R2 score = 0)", Grade),
      Model.Type = model_type
    )
  
  return(all_scores)
}

# --- Apply to both model types ---
names(pooled_Q_data)
discharge_scores   <- score_models(Q_r2_data, pooled_Q_data, "Discharge_cumecs")
waterlevel_scores  <- score_models(WL_r2_data, pooled_WL_data, "WaterLevel_m")

# --- Final combined df ---
final_scores <- bind_rows(discharge_scores, waterlevel_scores) %>%
  arrange(Model.Type, desc(total_score))

print(final_scores)
str(final_scores)

# add site info
# Bind SiteDat to final_scores by Site.Code
final_scores_with_sites <- SiteDat %>%
  left_join(final_scores, by = "Site.Code")

# Inspect result
str(final_scores_with_sites)
print(final_scores_with_sites)

# puch back to main df once QCd
final_scores <- final_scores_with_sites

# Kernel Density --------
# Apply grading rule
correlation_data <- correlation_data %>%
  mutate(
    Grade = if_else(
      pmap_lgl(list(R2_Discharge, R2_WaterLevel, R2_RelMonth),
               ~ any(c(..1, ..2, ..3) > 0.3, na.rm = TRUE)),
      "Poor (Use of simple infilling may perpertuate bias in wet season ave)", "Fair (no evidence of bias in PRM results, simple infilling okay)"
    )
  )

print(correlation_data)
str(correlation_data)
str(SiteDat)

# add site info & clean
# Bind SiteDat to final_scores by Site.Code
correlation_data_with_sites <- SiteDat %>%
  left_join(correlation_data, by = "Site.Name")
correlation_data_with_sites <- correlation_data_with_sites[, -c(5:7)]
correlation_data_with_sites$Model.Type <- "Simple Imputation (KD)"

# Inspect result
str(correlation_data_with_sites)
print(correlation_data_with_sites)

# push back once QCd
correlation_data<- correlation_data_with_sites

# join both scoring dfs
str(final_scores)
str(correlation_data)
combined_scores_df <- bind_rows(final_scores, correlation_data)

# write to file------
write.csv(combined_scores_df, "Model scores and grading.csv", row.names = FALSE)
