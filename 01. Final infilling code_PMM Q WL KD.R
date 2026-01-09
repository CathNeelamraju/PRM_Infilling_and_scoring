rm(list = ls())
# libraries----------
library(ggplot2)
library(leaps)
library(car)
library(s20x)
library(gam)
library(MASS)
library(fitdistrplus)
library(bestNormalize)
library(boot)
library(randomForest)
library(caret)
library(tidyverse)
library(pdp)
library(vip)
library(ggpubr)
library(dplyr)
library(mgcv)
library(gratia)
library(ggplot2)
library(RColorBrewer)
library(ggpmisc)  # for stat_poly_eq
library(VIM)
library(mice)
library(gridExtra)
library(miceadds)

#library(caret)

library(broom)
library(mitools)
library(broom.mixed)
library(performance)
library(lme4)
library(fitdistrplus)
library(furrr)

remove.packages("future.apply")



# read in data --------
setwd("C:/Users/uqcneela/OneDrive - The University of Queensland/General - Sci SEES Res Reef Catchment Science Partnership/Project 3/Catherine Neelamraju/Infilling")
SampleDat <- read.csv("PRM_with all Q and WL_FINAL.csv", header = TRUE)
setwd("C:/Users/uqcneela/OneDrive - The University of Queensland/General - Sci SEES Res Reef Catchment Science Partnership/Project 3/Catherine Neelamraju/Infilling/MICE_revised test")
# > Clean and add vars ---------------
# check formats
{
  str(SampleDat)
  unique(unlist(SampleDat$"Site.Name", use.names = TRUE)) #check
  SampleDat$PRM.Date.Time <- as.POSIXct(SampleDat$PRM.Date.Time, format = "%Y-%m-%d %H:%M", tz = "Australia/Brisbane")
  SampleDat$Discharge.Date.Time <- as.POSIXct(SampleDat$Discharge.Date.Time, format = "%Y-%m-%d %H:%M", tz = "Australia/Brisbane")
  SampleDat$WL.Date.Time <- as.POSIXct(SampleDat$WL.Date.Time, format = "%Y-%m-%d %H:%M", tz = "Australia/Brisbane")
  str(SampleDat) #check
}

{
  SampleDat$Q_Decile_EProb <- as.numeric(gsub("EP", "", SampleDat$Q_Decile_EProb))
  SampleDat$Q_Decile_cumecs <- as.numeric(gsub("D", "", SampleDat$Q_Decile_cumecs))
  SampleDat$WL_Decile_EProb <- as.numeric(gsub("EP", "", SampleDat$WL_Decile_EProb))
  SampleDat$WL_Decile_metres <- as.numeric(gsub("D", "", SampleDat$WL_Decile_metres))
}

unique(unlist(SampleDat$"Timing", use.names = TRUE)) #check

# add Rel.Month
# Create 30-day bins for Rel.Day
SampleDat$Rel.Month <- cut(SampleDat$Rel.Day,
                           breaks = c(0, 30, 60, 90, 120, 150, 182),
                           labels = c("1", "2", "3", "4", "5", "6"),
                           right = TRUE, include.lowest = TRUE)

SampleDat$Sampling.Year <- as.factor(SampleDat$Sampling.Year)
str(SampleDat)

# add source
SampleDat <- SampleDat %>%
  mutate(Source = ifelse(Sampled == "Yes", "Observed", "Imputed"))
unique(unlist(SampleDat$"Source", use.names = TRUE)) #check
str(SampleDat)

# Discharge PMM----------------------
# > filter to Q only---------------
SampleDat_Q <- SampleDat %>%
  filter(!is.na(Discharge_cumecs)) %>%
  droplevels()
unique(unlist(SampleDat_Q$Site.Name, use.names = TRUE)) # check
n_observed <- sum(SampleDat_Q$Source == "Observed", na.rm = TRUE) # count calues

# ADd Date
SampleDat_Q$Date <- as.POSIXct(format(SampleDat_Q$Discharge.Date.Time, "%Y-%m-%d"), format = "%Y-%m-%d")

# Remove uneccesary columns
SampleDat_Q <- SampleDat_Q[, -c(7,9,16,20,22,23:30)]
names(SampleDat_Q)
unique(unlist(SampleDat_Q$"Site.Name", use.names = TRUE)) #check
unique(unlist(SampleDat_Q$"Sampling.Year", use.names = TRUE)) #check
str(SampleDat_Q)

# fix formats
SampleDat_Q$Sampled <- as.factor(SampleDat_Q$Sampled)
SampleDat_Q$Timing <- as.factor(SampleDat_Q$Timing)
SampleDat_Q$Source <- as.factor(SampleDat_Q$Source)

# check for NAs
# Returns a named vector with the count of NAs in each column
colSums(is.na(SampleDat_Q)) # missing values only in PRM columns - as expected

# check correlations
library(s20x)
pairs20x(SampleDat_Q[, c(12,14:17,20,23:24)], main="Discharge Variables MICE_all sites combined")
str(SampleDat_Q)
names(SampleDat_Q)

# check Total.PRM dostribution
SampleDat_Q_filtered <- SampleDat_Q %>%
  filter(Source != "Imputed")
fit.norm <- fitdist(SampleDat_Q_filtered$Total.PRM, "norm")
plot(fit.norm)
rm(SampleDat_Q_filtered)

# > Impute missing Q PAF------------
# Define variable groups so the model knows which things to impute and which to leave

# Install/load required packages
install.packages("future.apply", type = "binary")
library(lme4)
library(future)
library(furrr)
library(mice)
library(broom)
library(performance)
library(dplyr)
library(readr)
library(stringr)
library(purrr)

# Define variables
target_vars    <- c("OtherHerb.PRM", "Fungicide.PRM", "Insecticide.PRM", "PSII.PRM", "Total.PRM")
target_vars    <- target_vars[sapply(SampleDat_Q[target_vars], is.numeric)]
predictor_vars <- c("Discharge_cumecs", "Rel.Month","Sampling.Year")
meta_vars      <- c("Site.Code", "Site.Name", "PRM.Date.Time", 
                    "Discharge.Date.Time", "Source", "Q_RateOfChange", "Q_Cumulative")

# Subset dataframe
impute_data <- SampleDat_Q[, c(target_vars, predictor_vars, meta_vars)]

# Split data by Site.Code
site_list <- split(impute_data, impute_data$Site.Code)

# Storage for site-wise imputations
site_imputations <- list()

# Run MICE with PMM separately for each site
for (site in names(site_list)) {
  site_data <- site_list[[site]]
  
  # Treat Sampling.Year as a factor (random effect style)
  if ("Sampling.Year" %in% names(site_data)) {
    site_data$Sampling.Year <- factor(site_data$Sampling.Year)
  }
  
  # Convert any matrix columns to numeric
  is_matrix_col <- sapply(site_data, is.matrix)
  site_data[is_matrix_col] <- lapply(site_data[is_matrix_col], as.numeric)
  
  # Identify non-imputable columns (POSIXt dates etc.)
  non_imputable_cols <- names(site_data)[sapply(site_data, inherits, "POSIXt")]
  
  # Create predictor matrix
  pred_mat <- make.predictorMatrix(site_data)
  
  # Exclude meta and non-imputable columns
  pred_mat[, c(meta_vars, non_imputable_cols)] <- 0
  
  # Set predictors for target variables
  for (var in target_vars) {
    pred_mat[var, ] <- 0
    pred_mat[var, predictor_vars] <- 1
  }
  
  # Create method vector
  meth_vec <- rep("", ncol(site_data))
  names(meth_vec) <- colnames(site_data)
  meth_vec[target_vars] <- "pmm"
  
  # Run PMM imputations
  set.seed(1234)
  site_imputations[[site]] <- mice(site_data, method = meth_vec, m = 20, predictorMatrix = pred_mat)
}

# Inspect warnings and logged events
warnings()
lapply(site_imputations, function(imp) imp$loggedEvents)

# Storage lists for model fits
model_fits    <- list()
pooled_models <- list()
tidy_results  <- list()
r2_results    <- list()

# Fit models site-wise
for (site in names(site_imputations)) {
  imp_obj <- site_imputations[[site]]
  imp_obj$data$Sampling.Year <- factor(imp_obj$data$Sampling.Year)  # Ensure factor
  
  model_fits[[site]]    <- list()
  pooled_models[[site]] <- list()
  tidy_results[[site]]  <- list()
  r2_results[[site]]    <- list()
  
  for (target in target_vars) {
    # Use only main effects (no interaction)
    main_effects <- paste(predictor_vars, collapse = " + ")
    formula_str <- paste(target, "~", main_effects, "+ (1 | Sampling.Year)")
    
    message("🔄 Fitting model for: ", target, " [Site: ", site, "]")
    
    models <- tryCatch({
      with(imp_obj, lme4::lmer(as.formula(formula_str)))
    }, error = function(e) {
      message("❌ Model fit failed for ", target, " [Site: ", site, "]: ", e$message)
      return(NULL)
    })
    
    # Check singularity
    is_valid_mira <- !is.null(models) && inherits(models, "mira")
    is_singular <- is_valid_mira && all(sapply(models$analyses, isSingular))
    
    # Refit with lm if singular
    if (is_singular) {
      message("⚠️ Singular fit detected for ", target, " [Site: ", site, "]. Refitting without random effect.")
      formula_str <- paste(target, "~", main_effects)
      models <- tryCatch({
        with(imp_obj, stats::lm(as.formula(formula_str)))
      }, error = function(e) {
        message("❌ Refit failed for ", target, " [Site: ", site, "]: ", e$message)
        return(NULL)
      })
    }
    
    # Store model fits
    model_fits[[site]][[target]] <- models
    
    # Pool only if mira
    pooled <- tryCatch({
      if (!is.null(models) && inherits(models, "mira")) {
        pool(models)
      } else {
        message("⚠️ Skipping pooling: model object is not mira for ", target, " [Site: ", site, "]")
        return(NULL)
      }
    }, error = function(e) {
      message("❌ Pooling failed for ", target, " [Site: ", site, "]: ", e$message)
      return(NULL)
    })
    
    pooled_models[[site]][[target]] <- pooled
    
    if (!is.null(pooled)) {
      tidy <- tryCatch({
        broom.mixed::tidy(pooled, conf.int = TRUE)
      }, error = function(e) {
        message("❌ Tidying failed for ", target, " [Site: ", site, "]: ", e$message)
        return(NULL)
      })
      tidy_results[[site]][[target]] <- tidy
      
      # R² from first imputed model if available
      r2 <- tryCatch({
        if (inherits(models, "mira")) {
          performance::r2(models$analyses[[1]])
        } else {
          NULL
        }
      }, error = function(e) {
        message("❌ R² failed for ", target, " [Site: ", site, "]: ", e$message)
        return(NULL)
      })
      r2_results[[site]][[target]] <- r2
      
      # Removed file export — results are now only stored in lists
    }
  }
}


# > Compile imputed + observed dataset------------
completed_sites <- list()
for (site in names(site_imputations)) {
  imp_obj <- site_imputations[[site]]
  completed_data <- complete(imp_obj, action = 1)
  completed_data$Site.Code <- site
  completed_sites[[site]] <- completed_data
}
final_imputed_df <- do.call(rbind, completed_sites)
final_imputed_df <- final_imputed_df[, c(target_vars, predictor_vars, meta_vars)]
write.csv(final_imputed_df, "obs plus imputed data_pmm by site_Q.csv", row.names = FALSE)


# Export imputed df to CSV
tryCatch({
  write.csv(final_imputed_df, "obs plus imputed data_pmm by site_Q.csv", row.names = FALSE)
  message("✅ Final imputed dataset exported: fobs plus imputed data_pmm by site.csv")
}, error = function(e) {
  message("❌ Export failed: ", e$message)
})

warnings()

library(dplyr)
library(purrr)

#> Compile model metadata and R² values ---------
library(purrr)
library(tibble)
model_summary_df <- map_dfr(names(model_fits), function(site) {
  map_dfr(names(model_fits[[site]]), function(target) {
    model_obj <- model_fits[[site]][[target]]
    r2_obj <- r2_results[[site]][[target]]
    
    model_type <- if (inherits(model_obj, "mira")) {
      class(model_obj$analyses[[1]])[1]
    } else {
      NA_character_
    }
    
    r2_marginal <- if (!is.null(r2_obj) && "R2_marginal" %in% names(r2_obj)) r2_obj$R2_marginal else NA_real_
    r2_conditional <- if (!is.null(r2_obj) && "R2_conditional" %in% names(r2_obj)) r2_obj$R2_conditional else NA_real_
    
    tibble(
      Site.Name = site,
      Target = target,
      Model.Type = model_type,
      R2_marginal = r2_marginal,
      R2_conditional = r2_conditional
    )
  })
})

# Export compiled R² summary
write.csv(model_summary_df, "compiled_model_r2_summary_Q.csv", row.names = FALSE)



#> compile pooled diagnostics-------------
library(dplyr)
library(purrr)

# Compile tidy results directly from the list structure
compiled_models <- purrr::map_dfr(names(tidy_results), function(site) {
  purrr::map_dfr(names(tidy_results[[site]]), function(target) {
    df <- tidy_results[[site]][[target]]
    if (!is.null(df)) {
      df %>%
        mutate(Target = target,
               Site.Code = site,
               Source = "Discharge")
    }
  })
})

# Inspect
glimpse(compiled_models)

# Export once, if desired
write_csv(compiled_models, "all_pooled_pmm_results_Q.csv")

#Water Level PMM----------------------
#setwd("C:/Users/uqcneela/OneDrive - The University of Queensland/General - Sci SEES Res Reef Catchment Science Partnership/Project 3/Catherine Neelamraju/Infilling/MICE/WL_model fits")
#unlink("C:/Users/uqcneela/AppData/Local/R/win-library/4.2/00LOCK-parallelly", recursive = TRUE)
#install.packages("future.apply", type = "binary")
library(dplyr)
library(tidyverse)

# > Filter to WL only---------------
names(SampleDat)
SampleDat_WL <- SampleDat %>%
  filter(!is.na(WaterLevel_m), !is.na(WL_RateOfChange)) %>%
  droplevels()

unique(unlist(SampleDat_WL$Site.Name, use.names = TRUE)) # check
n_observed_WL <- sum(SampleDat_WL$Source == "Observed", na.rm = TRUE) # count values

# Add Date
SampleDat_WL$Date <- as.POSIXct(format(SampleDat_WL$WL.Date.Time, "%Y-%m-%d"), format = "%Y-%m-%d")

# Remove unnecessary columns
SampleDat_WL <- SampleDat_WL[, -c(7,9,15:22)]
names(SampleDat_WL)
unique(unlist(SampleDat_WL$Site.Name, use.names = TRUE)) # check
unique(unlist(SampleDat_WL$Sampling.Year, use.names = TRUE)) # check
str(SampleDat_WL)

# Fix formats
SampleDat_WL$Sampled <- as.factor(SampleDat_WL$Sampled)
SampleDat_WL$Timing <- as.factor(SampleDat_WL$Timing)
SampleDat_WL$Source <- as.factor(SampleDat_WL$Source)

# Check for NAs
colSums(is.na(SampleDat_WL)) # missing values only in PRM columns - as expected

# remove offset
SampleDat_WL <- SampleDat_WL[, -c(14)]
SampleDat_WL <- SampleDat_WL[, -c(17:19)]

# Check correlations
SampleDat_WL_filtered <- SampleDat_WL_combined %>%
  filter(Source != "Imputed") %>%
  droplevels()
names(SampleDat_WL_filtered)
str(SampleDat_WL_filtered)
SampleDat_WL_filtered$Rel.Month <- as.numeric(SampleDat_WL_filtered$Rel.Month)
SampleDat_WL_filtered$WL_RateOfChange <- as.numeric(SampleDat_WL_filtered$WL_RateOfChange)
selected_vars_WL <- c("Total.PRM","WaterLevel_m", "WL_Cumulative", "Rel.Month", "WL_RateOfChange")

library(s20x)
library(fitdistrplus)
pairs20x(SampleDat_WL_filtered[, selected_vars_WL], main = "Water Level Variables MICE_all sites combined")
str(SampleDat_WL)
names(SampleDat_WL)

# Check Total.PRM distribution
fit.norm_WL <- fitdist(SampleDat_WL_filtered$Total.PRM, "norm")
plot(fit.norm_WL)
rm(SampleDat_WL_filtered)

# Set up parallel plan
library(future)
library(parallel)  # for detectCores()
library(furrr)
library(mice)
plan(multisession, workers = parallel::detectCores() - 1)

library(future)
library(furrr)
library(mice)
SampleDat_WL_reduced <- SampleDat_WL

# > Impute missing WL PAF---------
# Define variable groups
target_vars_WL    <- c("OtherHerb.PRM", "Fungicide.PRM", "Insecticide.PRM", "PSII.PRM", "Total.PRM")
target_vars_WL    <- target_vars_WL[sapply(SampleDat_WL_reduced[target_vars_WL], is.numeric)]
predictor_vars_WL <- c("WaterLevel_m", "Rel.Month","Sampling.Year")
meta_vars_WL      <- c("Site.Code", "Site.Name", "PRM.Date.Time", "WL.Date.Time", "WL_Cumulative", "Source")

# Subset dataframe
impute_data_WL <- SampleDat_WL_reduced[, c(target_vars_WL, predictor_vars_WL, meta_vars_WL)]

# Split data by Site.Code
site_list_WL <- split(impute_data_WL, impute_data_WL$Site.Code)

# Parallel site-wise imputation with reproducible RNG
plan(multisession, workers = parallel::detectCores() - 1)

site_imputations_WL <- future_map(
  site_list_WL,
  function(site_data_WL) {
    # Ensure Sampling.Year is treated as a factor
    if ("Sampling.Year" %in% names(site_data_WL)) {
      site_data_WL$Sampling.Year <- factor(site_data_WL$Sampling.Year)
    }
    
    # Convert matrix columns to numeric
    is_matrix_col_WL <- sapply(site_data_WL, is.matrix)
    site_data_WL[is_matrix_col_WL] <- lapply(site_data_WL[is_matrix_col_WL], as.numeric)
    
    # Identify POSIXct columns
    non_imputable_cols <- names(site_data_WL)[sapply(site_data_WL, inherits, "POSIXt")]
    
    # Build predictor matrix
    pred_mat_WL <- make.predictorMatrix(site_data_WL)
    pred_mat_WL[, c(meta_vars_WL, non_imputable_cols)] <- 0
    for (var in target_vars_WL) {
      pred_mat_WL[var, ] <- 0
      pred_mat_WL[var, predictor_vars_WL] <- 1
    }
    
    # Build method vector
    method_vec_WL <- rep("", ncol(site_data_WL))
    names(method_vec_WL) <- colnames(site_data_WL)
    method_vec_WL[target_vars_WL] <- "pmm"
    
    # Run mice
    mice(site_data_WL, method = method_vec_WL, m = 20, predictorMatrix = pred_mat_WL)
  },
  .options = furrr::furrr_options(seed = TRUE)
)

# Check for warnings
warnings()
lapply(site_imputations_WL, function(imp) imp$loggedEvents)
site_data_WL$m

site <- site_target_pairs$site[1]
target <- site_target_pairs$target[1]
imp_obj <- site_imputations_WL[[site]]
formula_str <- paste(target, "~", paste(predictor_vars_WL, collapse = " + "), "+ (1 | Sampling.Year)")

models <- with(imp_obj, lmer(as.formula(formula_str)))
class(models)

library(lme4)
library(broom.mixed)
library(mice)
library(performance)

#initialise empty lists before the loop
model_fits_WL    <- list()
pooled_models_WL <- list()
tidy_results_WL  <- list()
r2_results_WL    <- list()

for (site_WL in names(site_imputations_WL)) {
  imp_obj_WL <- site_imputations_WL[[site_WL]]
  imp_obj_WL$data$Sampling.Year <- factor(imp_obj_WL$data$Sampling.Year)  # Ensure factor
  
  model_fits_WL[[site_WL]]    <- list()
  pooled_models_WL[[site_WL]] <- list()
  tidy_results_WL[[site_WL]]  <- list()
  r2_results_WL[[site_WL]]    <- list()
  
  for (target_WL in target_vars_WL) {
    fixed_effects_WL <- paste(predictor_vars_WL, collapse = " + ")
    formula_str_WL   <- paste(target_WL, "~", fixed_effects_WL, "+ (1 | Sampling.Year)")
    
    message("🔄 Fitting model for: ", target_WL, " [Site: ", site_WL, "]")
    
    models_WL <- tryCatch({
      with(imp_obj_WL, lmer(as.formula(formula_str_WL)))
    }, error = function(e) {
      message("❌ Model fit failed for ", target_WL, " [Site: ", site_WL, "]: ", e$message)
      return(NULL)
    })
    
    # Check if model is mira and not singular
    is_valid_mira <- !is.null(models_WL) && inherits(models_WL, "mira")
    is_singular <- is_valid_mira && all(sapply(models_WL$analyses, isSingular))
    
    # Refit with lm if singular
    if (is_singular) {
      message("⚠️ Singular fit detected for ", target_WL, " [Site: ", site_WL, "]. Refitting without random effect.")
      formula_str_WL <- paste(target_WL, "~", fixed_effects_WL)
      models_WL <- tryCatch({
        with(imp_obj_WL, lm(as.formula(formula_str_WL)))
      }, error = function(e) {
        message("❌ Refit failed for ", target_WL, " [Site: ", site_WL, "]: ", e$message)
        return(NULL)
      })
    }
    
    model_fits_WL[[site_WL]][[target_WL]] <- models_WL
    
    # Pool only if mira
    pooled_WL <- tryCatch({
      if (!is.null(models_WL) && inherits(models_WL, "mira")) {
        pool(models_WL)
      } else {
        message("⚠️ Skipping pooling: model object is not mira for ", target_WL, " [Site: ", site_WL, "]")
        return(NULL)
      }
    }, error = function(e) {
      message("❌ Pooling failed for ", target_WL, " [Site: ", site_WL, "]: ", e$message)
      return(NULL)
    })
    
    pooled_models_WL[[site_WL]][[target_WL]] <- pooled_WL
    
    if (!is.null(pooled_WL)) {
      tidy_WL <- tryCatch(tidy(pooled_WL, conf.int = TRUE), error = function(e) {
        message("❌ Tidying failed for ", target_WL, " [Site: ", site_WL, "]: ", e$message)
        return(NULL)
      })
      tidy_results_WL[[site_WL]][[target_WL]] <- tidy_WL
      
      # R² from first imputed model if available
      r2_WL <- tryCatch({
        if (inherits(models_WL, "mira")) {
          performance::r2(models_WL$analyses[[1]])
        } else {
          NULL
        }
      }, error = function(e) {
        message("❌ R² failed for ", target_WL, " [Site: ", site_WL, "]: ", e$message)
        return(NULL)
      })
      r2_results_WL[[site_WL]][[target_WL]] <- r2_WL
      
      # Removed file export — results are now only stored in lists
    }
  }
}


warnings()



#> Compile imputed + observed data ---------------------
completed_sites_WL <- list()

# Extract completed data (observed + imputed) from each site's mice object
for (site_WL in names(site_imputations_WL)) {
  imp_obj_WL <- site_imputations_WL[[site_WL]]
  
  # Use the first completed dataset (you can change to average or stack if needed)
  completed_data_WL <- complete(imp_obj_WL, action = 1)
  
  # Add site identifier back if needed
  completed_data_WL$Site.Code <- site_WL
  
  completed_sites_WL[[site_WL]] <- completed_data_WL
}

# Combine all sites into one final dataframe
final_imputed_df_WL <- do.call(rbind, completed_sites_WL)

# Optional: reorder columns to match original structure
final_imputed_df_WL <- final_imputed_df_WL[, c(target_vars_WL, predictor_vars_WL, meta_vars_WL)]

# Replace all NAs in Fungicide.PRM with zeros
final_imputed_df_WL$Fungicide.PRM[is.na(final_imputed_df_WL$Fungicide.PRM)] <- 0

# Export to CSV
tryCatch({
  write.csv(final_imputed_df_WL, "obs plus imputed data_pmm by site_WL.csv", row.names = FALSE)
  message("✅ Final imputed dataset exported: obs plus imputed data_pmm by site_WL.csv")
}, error = function(e) {
  message("❌ Export failed: ", e$message)
})


#> Compile model metadata and R² values for WL models -----------
model_summary_WL <- purrr::map_dfr(names(model_fits_WL), function(site_WL) {
  purrr::map_dfr(names(model_fits_WL[[site_WL]]), function(target_WL) {
    model_obj_WL <- model_fits_WL[[site_WL]][[target_WL]]
    r2_obj_WL    <- r2_results_WL[[site_WL]][[target_WL]]
    
    # Identify model type
    model_type_WL <- if (inherits(model_obj_WL, "mira")) {
      class(model_obj_WL$analyses[[1]])[1]
    } else if (!is.null(model_obj_WL)) {
      class(model_obj_WL)[1]
    } else {
      NA_character_
    }
    
    #  extract R² values
    r2_marginal_WL <- if (!is.null(r2_obj_WL) && "R2_marginal" %in% names(r2_obj_WL)) r2_obj_WL$R2_marginal else NA_real_
    r2_conditional_WL <- if (!is.null(r2_obj_WL) && "R2_conditional" %in% names(r2_obj_WL)) r2_obj_WL$R2_conditional else NA_real_
    
    tibble::tibble(
      Site.Name      = site_WL,
      Target         = target_WL,
      Model.Type     = model_type_WL,
      R2_marginal    = r2_marginal_WL,
      R2_conditional = r2_conditional_WL
    )
  })
})

# Preview the summary
glimpse(model_summary_WL)

# Optional: export to CSV
write.csv(model_summary_WL, "compiled_model_r2_summary_WL.csv", row.names = FALSE)


#> compile pooled diagnostics----
# List all result CSVs from model loop
library(dplyr)
library(purrr)

# Compile tidy results directly from the WL list structure
compiled_models_WL <- purrr::map_dfr(names(tidy_results_WL), function(site_WL) {
  purrr::map_dfr(names(tidy_results_WL[[site_WL]]), function(target_WL) {
    df <- tidy_results_WL[[site_WL]][[target_WL]]
    if (!is.null(df)) {
      df %>%
        mutate(Target    = target_WL,
               Site.Code = site_WL,
               Source    = "Water Level")
    }
  })
})

# Inspect
glimpse(compiled_models_WL)

# Export once, if desired
write_csv(compiled_models_WL, "all_pooled_pmm_results_WL.csv")

# Time aware KD imputation -------
str(SampleDat)


# > identify dates missing & dates sampled------------
library(dplyr)
library(lubridate)
library(purrr)
library(tidyr)
library(data.table)

# identify dates sampled (valid PRM)
# convert to data.table
setDT(SampleDat)
str(SampleDat)

# 1) Choose the timestamp to represent "day present in time series" for unsampled rows
SampleDat[
  , ts_for_unsampled := fifelse(
    Site.Name == "Fairydale Drainage at Norton Road",
    WL.Date.Time,
    Discharge.Date.Time
  )
]

# 2) Derive day columns for comparison
SampleDat[
  , `:=`(
    unsampled_day = as.Date(ts_for_unsampled),
    sampled_day   = as.Date(PRM.Date.Time)
  )
]

# 3) Build a lookup of sampled days per site-year (unique calendar days)
sampled_days <- unique(
  SampleDat[!is.na(sampled_day),
            .(Site.Name, Sampling.Year, Day = sampled_day)
  ]
)

# 4) Candidate unsampled days: rows where PRM is NA and we have a valid time-series day
unsampled_candidates <- unique(
  SampleDat[is.na(PRM.Date.Time) & !is.na(unsampled_day),
            .(Site.Name, Sampling.Year, Day = unsampled_day)
  ]
)

# 5) Exclude any candidate day that appears as a sampled day for the same site-year
unsampled_unique <- unsampled_candidates[
  !sampled_days, on = .(Site.Name, Sampling.Year, Day)
][order(Site.Name, Sampling.Year, Day)]

unsampled_unique


# Convert to plain data.frame
MissingDates <- as.data.frame(unsampled_unique)
str(MissingDates)

# Convert numeric MissingDates to Date
MissingDates$MissingDates <- as.Date(MissingDates$MissingDates, origin = "1970-01-01")
names(MissingDates)[3] <- "Date" # Fix name

# Check structure
str(MissingDates)
head(MissingDates)


# > KD MI function--------
library(dplyr)
library(purrr)
library(tibble)

# --- (re-use imputation functions from before) ---
imputation_Kernel <- function(x, n) {
  x <- x[!is.na(x)]
  x <- x[x > 0]
  if (length(x) < 2) return(rep(NA_real_, n))
  density_obj <- stats::density(x, from = min(x, na.rm = TRUE))
  sample(density_obj$x, size = n, replace = TRUE, prob = density_obj$y)
}

impute_missing_days_kernel <- function(df, missing_dates, target_vars) {
  if (!"Date" %in% names(missing_dates)) stop("missing_dates must contain a column called 'Date'.")
  if (!all(c("Site.Name", "Sampling.Year") %in% names(missing_dates))) stop("missing_dates must contain 'Site.Name' and 'Sampling.Year'.")
  if (!"PRM.Date.Time" %in% names(df)) stop("df must contain PRM.Date.Time column.")
  missing_dates <- missing_dates %>%
    mutate(Site.Name = as.character(Site.Name), Sampling.Year = as.character(Sampling.Year))
  df <- df %>%
    mutate(Site.Name = as.character(Site.Name), Sampling.Year = as.character(Sampling.Year))
  df_observed <- df %>% filter(!is.na(PRM.Date.Time))
  for (v in target_vars) {
    if (!v %in% names(df_observed)) stop(paste0("Target variable not found in df: ", v))
    df_observed[[v]] <- as.numeric(df_observed[[v]])
  }
  grouped_missing <- missing_dates %>% group_by(Site.Name, Sampling.Year) %>% group_split()
  imputed_results <- map_dfr(grouped_missing, function(group_df) {
    site <- unique(group_df$Site.Name); year <- unique(group_df$Sampling.Year)
    if (length(site) != 1 || length(year) != 1) stop("Each group must have exactly one Site.Name and one Sampling.Year")
    obs_group <- df_observed %>% filter(Site.Name == site, Sampling.Year == year)
    n_missing <- nrow(group_df)
    imputed_values <- map_dfc(target_vars, function(var) {
      observed_vals <- obs_group[[var]]
      observed_vals <- observed_vals[!is.na(observed_vals) & observed_vals > 0]
      if (length(observed_vals) < 2) {
        warning_msg <- paste0(
          "Insufficient positive observed values for imputation: Site.Name = '",
          site, "', Sampling.Year = '", year, "', variable = '", var,
          "'. Imputed values set to NA for these missing dates. ",
          "Please inspect SampleDat_Q and MissingDates for data issues."
        )
        warning(warning_msg)
        return(tibble(!!var := rep(NA_real_, n_missing)))
      }
      tibble(!!var := imputation_Kernel(observed_vals, n_missing))
    })
    group_df %>% select(Site.Name, Sampling.Year, Date) %>% bind_cols(imputed_values) %>% mutate(Source = "Imputed", .before = 1)
  })
  desired_cols <- c("Source", "Site.Name", "Sampling.Year", "Date", target_vars)
  imputed_results %>% select(any_of(desired_cols))
}

#> apply to Q and WL dfs ----------
# --- parameters ---
target_vars <- c("OtherHerb.PRM", "Fungicide.PRM", "Insecticide.PRM", "PSII.PRM", "Total.PRM")
excluded_site <- "Fairydale Drainage at Norton Road"

# --- 1) run on SampleDat_Q / MissingDates excluding the Fairydale site ---
SampleDat_Q_noFairy <- SampleDat_Q %>% filter(Site.Name != excluded_site)
MissingDates_noFairy <- MissingDates %>% filter(Site.Name != excluded_site)

imputed_missing_days_Q <- impute_missing_days_kernel(
  df = SampleDat_Q_noFairy,
  missing_dates = MissingDates_noFairy,
  target_vars = target_vars
)

# observed rows (excluding Fairydale) to combine later
observed_Q_noFairy <- SampleDat_Q_noFairy %>% filter(!is.na(PRM.Date.Time))

# --- 2) run on SampleDat_WL / MissingDates for only the Fairydale site ---
# ensure SampleDat_WL exists and has the same structure (PRM.Date.Time and target_vars)
SampleDat_WL_Fairy <- SampleDat_WL %>% filter(Site.Name == excluded_site)
MissingDates_Fairy <- MissingDates %>% filter(Site.Name == excluded_site)

imputed_missing_days_Fairy <- impute_missing_days_kernel(
  df = SampleDat_WL_Fairy,
  missing_dates = MissingDates_Fairy,
  target_vars = target_vars
)

# observed rows for Fairydale from SampleDat_WL (if PRM.Date.Time used there)
observed_WL_Fairy <- SampleDat_WL_Fairy %>% filter(!is.na(PRM.Date.Time))

# --- 3) combine imputed results and observed rows back together ---
imputed_missing_days_all <- bind_rows(imputed_missing_days_Q, imputed_missing_days_Fairy)

observed_all <- bind_rows(
  observed_Q_noFairy,
  observed_WL_Fairy
)

SampleDat_Q_complete <- bind_rows(observed_all, imputed_missing_days_all)

#> inspect imp + obs df-----
na_rows_df <- SampleDat_Q_complete %>%
  filter(if_any(8:12, ~ is.na(.)))
# count NAs in thise columns
na_counts <- colSums(is.na(SampleDat_Q_complete[, 8:12, drop = FALSE]))

na_counts_df <- tibble(
  column = names(na_counts),
  n_NA = as.integer(na_counts)
)

# columns that have any NAs
cols_with_na <- na_counts_df %>% filter(n_NA > 0) %>% pull(column)

# fungicides are the only target with NAs - due to KD being unable to fit a distribution to such low numbers with no variance
# this actually works well because KD was ADDING PAF before by forcing a distribution fit.
# Now replace all Fungicide NAs with zero --> this is appropriate given source (Observed) data was also essentially zero

# > replace NA values in Fungicide.PRM with 0 ------
names(SampleDat_Q_complete)
SampleDat_Q_complete$Fungicide.PRM[is.na(SampleDat_Q_complete$Fungicide.PRM)] <- 0
sum(is.na(SampleDat_Q_complete$Fungicide.PRM))  # check


# export if desired
write.csv(SampleDat_Q_complete, "obs plus imputed data_KD simple imputation.csv", row.names = FALSE)

# Optional: Plot to check-------
library(ggplot2)
library(dplyr)

# prepare data: ensure numeric, remove NA Total.PRM (or adjust to taste)
names(SampleDat_Q_complete)
plot_df <- SampleDat_Q_complete %>%
  mutate(
    OtherHerb.PRM = as.numeric(OtherHerb.PRM),
    Source = as.character(Source),
    Site.Name = as.character(Site.Name)
  ) %>%
  filter(!is.na(Total.PRM))    # remove NA values from the variable being plotted

# density plot
p <- ggplot(plot_df, aes(x = OtherHerb.PRM, fill = Source, colour = Source)) +
  geom_density(alpha = 0.35, size = 0.25) +
  facet_wrap(~ Site.Name, scales = "free") +
  scale_fill_brewer(palette = "Set2", na.value = "grey70") +
  scale_colour_brewer(palette = "Set2", guide = "none") +
  labs(
    x = "Other Herbicide mixture toxicity (% affected fraction)",
    y = "Density",
    fill = "Source"
  ) +
  theme_bw(base_size = 12) +
  theme(
    strip.text = element_text(size = 8.3),
    axis.text = element_text(size = 8),
    legend.position = "bottom"
  )
print(p)
ggsave("density_OH PRM_KD MI.png", p, width = 15, height = 8, dpi = 300)




