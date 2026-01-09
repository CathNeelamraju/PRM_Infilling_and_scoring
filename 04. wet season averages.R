rm(list = ls())

# libraries --------------
library(lubridate)
library(dplyr)
library(tidyr)
library(purrr)
library(readxl)
library(ggplot2)
library(tibble)
library(data.table)
library(plyr)
library(corrplot)
library(fuzzyjoin)
library(openxlsx)
library(stringr)

setwd("C:/Users/uqcneela/OneDrive - The University of Queensland/General - Sci SEES Res Reef Catchment Science Partnership/Project 3/Catherine Neelamraju/Infilling/Shiny app")
QDat_final <- read_csv("QDat_final.csv")
WLDat_final <- read_csv("WLDat_final.csv")
KDDat_final <- read_csv("KDDat_final.csv")

# calculate wet season averages -------
setwd("C:/Users/uqcneela/OneDrive - The University of Queensland/General - Sci SEES Res Reef Catchment Science Partnership/Project 3/Catherine Neelamraju/Infilling/MICE_revised test")

# function to caluclate average wet season PAF
str(QDat_final)
str(WLDat_final)
str(KDDat_final)

dt_avg_paf_wide <- function(dt) {
  setDT(dt)
  out <- dt[, .(PAF = mean(PAF, na.rm = TRUE)),
            by = .(Site.Code, Site.Name, Sampling.Year, Group)]
  dcast(out, Site.Code + Site.Name + Sampling.Year ~ Group, value.var = "PAF")
}

# > apply function-----
names(QDat_final)
names(WLDat_final)
names(KDDat_final)

# Remove all rows with any NA
KDDat_final <- na.omit(KDDat_final)


QDat_avg  <- dt_avg_paf_wide(QDat_final)[, Dataset := "QDat"]
WLDat_avg <- dt_avg_paf_wide(WLDat_final)[, Dataset := "WLDat"]
KDDat_avg <- dt_avg_paf_wide(KDDat_final)[, Dataset := "KDDat"]

combined_avg <- rbindlist(list(QDat_avg, WLDat_avg, KDDat_avg), use.names = TRUE, fill = TRUE)
setcolorder(combined_avg, c("Dataset", "Site.Code", "Site.Name", "Sampling.Year"))

#> clean------
combined_avg <- as.data.frame(combined_avg)
combined_avg_clean <- combined_avg %>%
  filter(!if_all(-c(Dataset, Site.Code, Site.Name, Sampling.Year), is.na))

# Calculate proprtions------
str(combined_avg_clean)

wetseason_props <- setDT(combined_avg_clean)

# ensure numeric for cols 5:8
numcols <- names(wetseason_props)[5:8]
wetseason_props[, (numcols) := lapply(.SD, as.numeric), .SDcols = numcols]

# compute sum and proportions, setting proportions to NA when sum is zero
wetseason_props[, Sum_Groups := rowSums(.SD, na.rm = TRUE), .SDcols = numcols]
wetseason_props[Sum_Groups == 0, paste0(numcols, "_prop") := lapply(numcols, function(x) NA_real_)]
wetseason_props[Sum_Groups != 0, paste0(numcols, "_prop") := lapply(numcols, function(x) get(x) / Sum_Groups)]

str(wetseason_props)

#> calulate adjusted props
library(data.table)
setDT(wetseason_props) # works better in DT

# ensure Total.PRM and prop cols are numeric
prop_cols <- names(wetseason_props)[11:14]
wetseason_props[, (c("Total.PRM", prop_cols)) := lapply(.SD, as.numeric), .SDcols = c("Total.PRM", prop_cols)]

# create adjusted columns named like Fungicide.PRM_adj, etc.
adj_names <- sub("_prop$", "_adj", prop_cols)
wetseason_props[, (adj_names) := Map(function(pcol) {
  # multiply prop column by Total.PRM, keep NA if either is NA
  get(pcol) * Total.PRM
}, prop_cols)]

wetseason_props <- as.data.frame(wetseason_props)
write.csv(wetseason_props, "wet season averages_all models.csv", row.names = FALSE)

#> subset by chosen model
unique(unlist(wetseason_props$Dataset, use.names = TRUE)) # check

# create ref table of final chosen models for each site
selector <- tribble(
  ~Site.Name, ~Site.Code, ~Dataset,
  "Daintree River at Lower Daintree", "1080025", "KDDat",
  "Fitzroy River at Fitzroy River Water", "1300040", "KDDat",
  "Moore Park Drainage at Moore Park Road", "1350051", "KDDat",
  "Fairydale Drainage at Norton Road", "1350052", "WLDat",
  "Welcome Creek at Gooburrum Road", "1350053", "QDat",
  "Burnett River at Quay Street Bridge", "1360106", "QDat",
  "Yellow Waterholes Creek at Dahls Road", "1370040", "QDat",
  "Tully River at Euramo", "113006A", "QDat",
  "Barratta Creek at Northcote", "119101A", "QDat",
  "Pioneer River at Dumbleton Pump Station Headwater", "125013A", "QDat",
  "Sandy Creek at Homebush", "126001A", "QDat"
)

# ensure same types and trim whitespace if needed, then semi_join to keep matching rows
selector <- selector %>%
  mutate(
    Site.Code = as.character(Site.Code),
    Dataset  = as.character(Dataset),
    Site.Name = as.character(Site.Name)
  )

matched_rows <- wetseason_props %>%
  mutate(
    Site.Code = as.character(Site.Code),
    Dataset  = as.character(Dataset),
    Site.Name = as.character(Site.Name)
  ) %>%
  semi_join(selector, by = c("Site.Name", "Site.Code", "Dataset"))

write.csv(matched_rows, "wet season averages_final models.csv", row.names = FALSE)


# > plot proportions (faceted)-----
str(matched_rows)

# prepare data and pivot long
plot_df <- matched_rows %>%
  # ensure numeric for the adjusted columns (cols 15:18)
  mutate(across(15:18, as.numeric)) %>%
  # keep the identifying columns and the 4 adjusted cols
  select(Dataset, Site.Code, Site.Name, Sampling.Year, all_of(names(.)[15:18])) %>%
  pivot_longer(
    cols = 5:8,                   # the four adjusted columns (Fungicide.PRM_adj ... PSII.PRM_adj)
    names_to = "Contribution",
    values_to = "PAF"
  ) %>%
  # make Sampling.Year a factor to preserve ordering (customise levels if needed for plotting)
  mutate(Sampling.Year = factor(Sampling.Year, levels = unique(Sampling.Year)))

# fix Group names
unique(unlist(plot_df$Contribution, use.names = TRUE)) # check
plot_df <- plot_df %>%
  mutate(
    Contribution = recode(
      Contribution,
      "Fungicide.PRM_adj"   = "Fungicides",
      "Insecticide.PRM_adj" = "Insecticides",
      "OtherHerb.PRM_adj"   = "Other Herbicides",
      "PSII.PRM_adj"        = "PSII Herbicides"
    )
  )
unique(unlist(plot_df$Contribution, use.names = TRUE)) # check    


# wrap Site.Name to ~30 characters (adjust width to fit)
plot_df <- plot_df %>%
  mutate(Site.Name_wrapped = str_wrap(Site.Name, width = 33))

ContPlot <- ggplot(plot_df, aes(x = Sampling.Year, y = PAF, fill = Contribution)) +
  geom_col(colour = "grey30", size = 0.2, width = 0.9, alpha = 0.9) +
  facet_wrap(~ Site.Name_wrapped, scales = "free_x") +
  scale_fill_brewer(palette = "Paired", direction = -1, na.value = "grey50") +
  labs(
    x = "Sampling Year",
    y = "Total mixture toxicity (% affected fraction)",
    fill = "Contribution"
  ) +
  theme_bw(base_size = 10) +
  theme(
    axis.text.x = element_text(size = 7.5),
    strip.text = element_text(size = 9),
    panel.spacing = unit(0.7, "lines"),
    legend.position = c(0.95, 0.0001),
    legend.justification = c("right", "bottom"),
    legend.direction = "vertical",
    legend.background = element_rect(fill = alpha("white", 0.90), colour = "white"),
    legend.key = element_rect(fill = "white", colour = NA),
    legend.key.size = unit(9, "mm"),
    legend.text = element_text(size = 9),
    legend.title = element_text(size = 11, hjust = 0)
  ) +
  guides(
    fill = guide_legend(
      title.position = "top",
      title.hjust = 0,
      ncol = 1,
      override.aes = list(size = 3),
      reverse = FALSE
    )
  )
ContPlot


ggsave(filename = "Wet season ave and contributions_grid_freex.png",
       plot = ContPlot,
       width = 10, height = 8, dpi = 300)

#> unfaceted------
plot_df <- plot_df %>%
  mutate(Site.Name_wrapped = str_wrap(Site.Name, width = 17))

ContPlot <- ggplot(plot_df, aes(x = Sampling.Year, y = PAF, fill = Contribution)) +
  geom_col(colour = "black", size = 0.5, width = 0.9, alpha = 0.9) +
    facet_grid(~ Site.Name_wrapped) +
  #geom_hline(yintercept = 5, colour = "red", linetype = "dashed", size = 0.4) + 
  scale_fill_brewer(palette = "Paired", direction = -1, na.value = "grey50") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
  labs(
    x = NULL,
    y = "Total mixture toxicity (% affected fraction)",
    fill = "Contribution"
  ) +
  theme_bw(base_size = 13) +
  theme(
    axis.text.x = element_text(size = 8.5, angle = 45, hjust = 1),
    strip.text = element_text(size = 9.2),
    panel.spacing = unit(0.2, "lines"),
    legend.position = "bottom",
    legend.direction = "horizontal",
    legend.title = element_text(size = 11, hjust = 0.5),
    legend.text = element_text(size = 9),
    legend.key.size = unit(5, "mm"),
    legend.background = element_rect(fill = alpha("white", 0.90), colour = "white"),
    legend.key = element_rect(fill = "white", colour = NA),
    panel.grid.major.x = element_blank(),   # remove vertical  grid lines
    panel.grid.minor.x = element_blank()
  ) +
  guides(
    fill = guide_legend(title.hjust = 0.5,
                        nrow = 1,  # single row
                        override.aes = list(size = 3),
                        reverse = FALSE))
ContPlot


ggsave(filename = "Wet season averages and contributions_wide.tiff",
       plot = ContPlot,
       width = 14, height = 8, dpi = 600)

str(plot_df)



# density plots of obs and imputed data--------
names(WLDat_final)
names(QDat_final)
names(KDDat_final)

WLDat_final$Model <- "PMM (Water Level)"
QDat_final$Model <- "PMM (Discharge)"
KDDat_final$Model <- "Kernel Density"
str(QDat_final)

# keep Observed aside
QDat_observed <- QDat_final %>%
  filter(Source == "Observed")

# bind together
AllDat <- bind_rows(WLDat_final, QDat_final, KDDat_final)
names(AllDat)


# create ref table of final chosen models for each site
selector <- tribble(
  ~Site.Name, ~Site.Code, ~Model,
  "Daintree River at Lower Daintree", "1080025", "Kernel Density",
  "Fitzroy River at Fitzroy River Water", "1300040", "Kernel Density",
  "Moore Park Drainage at Moore Park Road", "1350051", "Kernel Density",
  "Fairydale Drainage at Norton Road", "1350052", "PMM (Water Level)",
  "Welcome Creek at Gooburrum Road", "1350053", "PMM (Discharge)",
  "Burnett River at Quay Street Bridge", "1360106", "PMM (Discharge)",
  "Yellow Waterholes Creek at Dahls Road", "1370040", "PMM (Discharge)",
  "Tully River at Euramo", "113006A", "PMM (Discharge)",
  "Barratta Creek at Northcote", "119101A", "PMM (Discharge)",
  "Pioneer River at Dumbleton Pump Station Headwater", "125013A", "PMM (Discharge)",
  "Sandy Creek at Homebush", "126001A", "PMM (Discharge)"
  )

str(matched_rows)
str(QDat_observed)



matched_rows <- AllDat %>%
  mutate(
    Site.Code = as.character(Site.Code),
    Source  = as.character(Source),
    Site.Name = as.character(Site.Name)
  ) %>%
  semi_join(selector, by = c("Site.Name", "Site.Code", "Model"))

# add Observed bac to df
CombinedDat <- bind_rows(matched_rows, QDat_observed)

# subset to total PAF
str(matched_rows)
subset_CombinedDat <- CombinedDat[CombinedDat$Group == "Total.PRM", ]
unique(unlist(subset_CombinedDat$"Group", use.names = TRUE)) #check
unique(unlist(subset_CombinedDat$"Sampling.Year", use.names = TRUE)) #check


# rename model types
subset_CombinedDat <- subset_CombinedDat %>%
  mutate(Source = case_when(
    Source == "PMM Imputations (Discharge)" ~ "PMM Imputation (Discharge)",
    Source == "PMM Imputations (Water Level)" ~ "PMM Imputation (Water Level)",
    TRUE ~ Source
  ))
unique(unlist(subset_CombinedDat$"Source", use.names = TRUE)) #check

# plot desity
subset_CombinedDat <- subset_CombinedDat %>%
  filter(Sampling.Year == "2023-2024") %>%
  mutate(Site.Name_wrapped = str_wrap(Site.Name, width = 30))

DensPlot <- ggplot(subset_CombinedDat, aes(x = PAF, colour = Source, fill = Source)) +
  geom_density(alpha = 0.3) +
  facet_wrap(~ Site.Name_wrapped, scales = "free") +
  labs(
    x = "Mixture toxicity (% affected fraction)",
    y = "Density",
    colour = "Source",
    fill = "Source"
  ) +
  scale_fill_manual(values = c(
    "Observed" = "grey",
    "PMM Imputation (Water Level)" = "steelblue",
    "PMM Imputation (Discharge)"   = "darkgreen",
    "Simple Imputation (KD)"        = "orange"
  )) +
  scale_colour_manual(values = c(
    "Observed" = "darkgrey",
    "PMM Imputations (Water Level)" = "steelblue",
    "PMM Imputations (Discharge)"   = "darkgreen",
    "Simple Imputation (KD)"        = "orange"
  )) +
  theme_bw(base_size = 11) +
  theme(
    strip.text = element_text(size = 10),
    legend.position = "bottom"
  )
DensPlot

ggsave(filename = "Density_obs v inputed.png",
       plot = DensPlot,
       width = 12, height = 8, dpi = 300)

# plot histogram
subset_CombinedDat <- subset_CombinedDat %>%
  filter(Sampling.Year == "2023-2024") %>%
  mutate(Site.Name_wrapped = str_wrap(Site.Name, width = 30))

HistPlot <- ggplot(subset_CombinedDat, aes(x = PAF, colour = Source, fill = Source)) +
  geom_histogram(alpha = 0.3) +
  facet_wrap(~ Site.Name_wrapped, scales = "free") +
  labs(
    x = "Mixture toxicity (% affected fraction)",
    y = "Count",
    colour = "Source",
    fill = "Source"
  ) +
  scale_fill_manual(values = c(
    "Observed" = "grey",
    "PMM Imputation (Water Level)" = "steelblue",
    "PMM Imputation (Discharge)"   = "darkgreen",
    "Simple Imputation (KD)"        = "orange"
  )) +
  scale_colour_manual(values = c(
    "Observed" = "darkgrey",
    "PMM Imputation (Water Level)" = "steelblue",
    "PMM Imputation (Discharge)"   = "darkgreen",
    "Simple Imputation (KD)"        = "orange"
  )) +
  theme_bw(base_size = 11) +
  theme(
    strip.text = element_text(size = 10),
    legend.position = "bottom"
  )
HistPlot

ggsave(filename = "Histogram_obs v inputed.png",
       plot = HistPlot,
       width = 12, height = 8, dpi = 300)

