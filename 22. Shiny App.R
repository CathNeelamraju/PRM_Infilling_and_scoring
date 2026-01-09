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
library(shiny)

#   > read in data --------
setwd("C:/Users/uqcneela/OneDrive - The University of Queensland/General - Sci SEES Res Reef Catchment Science Partnership/Project 3/Catherine Neelamraju/Infilling/MICE_revised test")
QDat <- read.csv("obs plus imputed data_pmm by site_Q.csv", header = TRUE)
WLDat <- read.csv("obs plus imputed data_pmm by site_WL.csv", header = TRUE)
KDDat <- read.csv("obs plus imputed data_KD simple imputation.csv", header = TRUE)

# reset WD
setwd("C:/Users/uqcneela/OneDrive - The University of Queensland/General - Sci SEES Res Reef Catchment Science Partnership/Project 3/Catherine Neelamraju/Infilling/Shiny app")


# Check and clean Q --------
str(QDat)
QDat <- QDat %>%
  mutate(Discharge.Date.Time = as.POSIXct(Discharge.Date.Time,
                                          format = "%Y-%m-%d %H:%M:%S",
                                          tz = "Australia/Brisbane"))
QDat$Date <- as.Date(QDat$Discharge.Date.Time)# add Date
selected_QDat <- QDat[, c(1:6, 8:10, 13,16)] # select required columns
unique(unlist(selected_QDat$"Source", use.names = TRUE)) #check
selected_QDat$Source <- gsub("^Imputed$", "PMM Imputations (Discharge)", selected_QDat$Source)
unique(unlist(selected_QDat$"Source", use.names = TRUE)) #check
selected_QDat$Fungicide.PRM[is.na(selected_QDat$Fungicide.PRM)] <- 0 # fix Fungicides
str(selected_QDat)


# pivot long by flow type
selected_QDat_long <- selected_QDat %>%
  pivot_longer(
    cols = Discharge_cumecs,
    names_to = "Flow.Type",
    values_to = "Value"
  )

# pivot long by PRM group
str(selected_QDat_long)
QDat_long <- selected_QDat_long %>%
  pivot_longer(
    cols = 1:5,
    names_to = "Group",
    values_to = "PAF"
  )

# remove unecessary dfs
rm(selected_QDat, selected_QDat_long)

# >> SUmmarise Q data ------------
# calcualte ave PAF and max Q for each date (but leave Observed data as is)
# df too large - use dt instead
setDT(QDat_long)

# summarise imputed sources
QDat_summary <- QDat_long[
  Source != "Observed",
  .(
    PAF   = mean(PAF, na.rm = TRUE),
    Value = max(Value, na.rm = TRUE)
  ),
  by = .(Site.Code, Site.Name, Sampling.Year, Group, Flow.Type, Source, Date)
]
QDat_observed <- QDat_long[Source == "Observed"]# keep observed rows as-is
QDat_final <- rbind(QDat_observed, QDat_summary, fill = TRUE) # combine observed + summarised imputed
QDat_final <- QDat_final %>%
  arrange(Site.Name, Group, Date) # sort

QDat_final <- as.data.frame(QDat_final)
unique(unlist(QDat_final$"Flow.Type"))
unique(unlist(QDat_final$"Source"))

# Check and clean WL --------
str(WLDat)
WLDat <- WLDat %>%
  mutate(WL.Date.Time = as.POSIXct(WL.Date.Time,
                                          format = "%Y-%m-%d %H:%M:%S",
                                          tz = "Australia/Brisbane"))
WLDat$Date <- as.Date(WLDat$WL.Date.Time)# add Date
str(WLDat)
selected_WLDat <- WLDat[, c(1:6, 8:10,14:15)] # select required columns
unique(unlist(selected_WLDat$"Source", use.names = TRUE)) #check
selected_WLDat$Source <- gsub("^Imputed$", "PMM Imputations (Water Level)", selected_WLDat$Source)
unique(unlist(selected_WLDat$"Source", use.names = TRUE)) #check
selected_WLDat$Fungicide.PRM[is.na(selected_WLDat$Fungicide.PRM)] <- 0 # fix Fungicides
str(selected_WLDat)

#pivot lonf by flow tpe
selected_WLDat_long <- selected_WLDat %>%
  pivot_longer(
    cols = WaterLevel_m,
    names_to = "Flow.Type",
    values_to = "Value"
  )
unique(unlist(selected_WLDat_long$"Flow.Type", use.names = TRUE)) #check

# pivot long
str(selected_WLDat_long)
WLDat_long <- selected_WLDat_long %>%
  pivot_longer(
    cols = 1:5,
    names_to = "Group",
    values_to = "PAF"
  )
unique(unlist(WLDat_long$"Source", use.names = TRUE)) #check
unique(unlist(WLDat_long$"Group", use.names = TRUE)) #check
unique(unlist(WLDat_long$"Site.Name", use.names = TRUE)) #check

# remove unecessary dfs
rm(selected_WLDat, selected_WLDat_long)

# subset Fairydale observed so it can be bound back to the main df
Fairydale_Observed <- WLDat_long %>%
  filter(Source == "Observed",
         Site.Name == "Fairydale Drainage at Norton Road")
unique(unlist(Fairydale_Observed$"Source", use.names = TRUE)) #check
unique(unlist(Fairydale_Observed$"Site.Name", use.names = TRUE)) #check

# remove sounre = observed
WLDat_long <- WLDat_long %>%
  filter(Source != "Observed")
unique(unlist(WLDat_long$"Source", use.names = TRUE)) #check

combined_WL_long <- bind_rows(Fairydale_Observed, WLDat_long) # bind back together so FAirydale is captured
combined_WL_long %>%
  filter(Source == "Observed") %>%
  summarise(n_sites = n_distinct(Site.Name)) # check

# cross check formats and col names
str(QDat_long)
str(combined_WL_long)
unique(unlist(combined_WL_long$"Group", use.names = TRUE)) #check
unique(unlist(combined_WL_long$"Flow.Type", use.names = TRUE)) #check


# sort
combined_WL_long <- combined_WL_long %>%
  arrange(Site.Name, Sampling.Year, Flow.Type, Group, Date)
str(combined_WL_long)

# >> Summarise Water Level data ------------
# calculate average PAF and max WL for each date (leave Observed data as is)
# df too large - use data.table
setDT(combined_WL_long)

# summarise imputed sources
WLDat_summary <- combined_WL_long[
  Source != "Observed",
  .(
    PAF   = mean(PAF, na.rm = TRUE),
    Value = max(Value, na.rm = TRUE)
  ),
  by = .(Site.Code, Site.Name, Sampling.Year, Group, Flow.Type, Source, Date)
]

# keep observed rows as-is
WLDat_observed <- combined_WL_long[Source == "Observed"]

# combine observed + summarised imputed
WLDat_final <- rbind(WLDat_observed, WLDat_summary, fill = TRUE)

# sort for readability
WLDat_final <- WLDat_final[order(Site.Name, Group, Date)]
WLDat_final <- as.data.frame(WLDat_final)
unique(unlist(WLDat_final$"Flow.Type", use.names = TRUE)) #check
unique(unlist(WLDat_final$"Source", use.names = TRUE)) #check

# Prepare KD MI data---------
# > check and clean------
KDDat <- KDDat[, -c(1:2, 4, 7, 13:20)]# remove columns
names(KDDat) # check
str(KDDat) # check formats

# > add spatial info----
# Site names 
setwd("C:/Users/uqcneela/OneDrive - The University of Queensland/General - Sci SEES Res Reef Catchment Science Partnership/Project 3/Catherine Neelamraju/Infilling")
SiteDat_all <- read.csv("PRM_with all Q and WL_FINAL.csv", header = TRUE)
setwd("C:/Users/uqcneela/OneDrive - The University of Queensland/General - Sci SEES Res Reef Catchment Science Partnership/Project 3/Catherine Neelamraju/Infilling/Shiny app")
SiteDat <- unique(SiteDat_all[, 1:4]) # filter to unique site data onlu
rm(SiteDat_all)
KDDat_with_sites <- SiteDat %>%# Bind SiteDat to final_scores by Site.Code
  left_join(KDDat, by = "Site.Name")

# rename IMputed data
KDDat_with_sites$Source <- gsub("^Imputed$", "Simple Imputation (KD)", KDDat_with_sites$Source)
unique(unlist(KDDat_with_sites$"Source", use.names = TRUE)) #check
KDDat_with_sites$Fungicide.PRM[is.na(KDDat_with_sites$Fungicide.PRM)] <- 0 # fix Fungicides

# fix dates
KDDat_with_sites$Date <- as.Date(KDDat_with_sites$Date, format = "%d/%m/%Y")
str(KDDat_with_sites)

# > pivot long by PAF---------
str(KDDat_with_sites)
KDDat_long <- KDDat_with_sites %>%
  pivot_longer(
    cols = 7:11,
    names_to = "Group",
    values_to = "PAF"
  )
unique(unlist(KDDat_long$"Source", use.names = TRUE)) #check
unique(unlist(KDDat_long$"Group", use.names = TRUE)) #check
unique(unlist(KDDat_long$"Site.Name", use.names = TRUE)) #check

# remove PRM.Date.Time
names(KDDat_long)
KDDat_long <- KDDat_long[, -c(6)]# remove columns
KDDat_long <- KDDat_long[, -c(1:2)]# remove columns

# > add WL data to KD df---------
str(WLDat_final)
setDT(WLDat_final)

WLDat_unique <- unique(
  WLDat_final[, .(Sampling.Year, Site.Code, Site.Name, Date, Flow.Type, Value)]
)

# make sure both are data.table
setDT(WLDat_unique)
setDT(KDDat_long)

# join by Site.Code and Date
KDDat_with_sites_andWL <- KDDat_long[WLDat_unique, on = .(Site.Code, Date)]
KDDat_final <- as.data.frame(KDDat_with_sites_andWL)
names(KDDat_final)
KDDat_final <- KDDat_final[, -c(6:9, 12:13)]# remove columns
names(KDDat_final)

#Bind all data---------
# Reorder columns alphabetically by name
WLDat_final <- as.data.frame(WLDat_final)
QDat_final  <- QDat_final[, sort(names(QDat_final))]
WLDat_final <- WLDat_final[, sort(names(WLDat_final))]
KDDat_final <- KDDat_final[, sort(names(KDDat_final))]

# check they match
names(QDat_final)
names(WLDat_final)
names(KDDat_final)

# bind
Combined_imp_obs_all<- bind_rows(QDat_final, WLDat_final, KDDat_final)
unique(unlist(Combined_imp_obs_all$"Source", use.names = TRUE)) #check

# Quick check
NA_rows <- Combined_imp_obs_all %>% # check SOurce = NA
  filter(is.na(Source))
nrow(NA_rows)   # how many NA rows
head(NA_rows)   # preview
Combined_imp_obs_all <- Combined_imp_obs_all %>%
  filter(!is.na(Source))

# write to file
setwd("C:/Users/uqcneela/OneDrive - The University of Queensland/General - Sci SEES Res Reef Catchment Science Partnership/Project 3/Catherine Neelamraju/Infilling/Shiny app")
write.csv(QDat_final, "QDat_final.csv", row.names = FALSE)
write.csv(WLDat_final, "WLDat_final.csv", row.names = FALSE)
write.csv(KDDat_final, "KDDat_final.csv", row.names = FALSE)
write.csv(Combined_imp_obs_all, "imp_obs_all_06122025.csv", row.names = FALSE)

# Shiny app---------
# UI
library(shiny)
library(ggplot2)
library(dplyr)
library(data.table)
library(readr)

# read in data
setwd("C:/Users/uqcneela/OneDrive - The University of Queensland/General - Sci SEES Res Reef Catchment Science Partnership/Project 3/Catherine Neelamraju/Infilling/Shiny app")
dt_combined <- read_csv("imp_obs_all_06122025.csv")
dt_combined <- as.data.frame(dt_combined)
dt_combined$Site.Name <- as.character(dt_combined$Site.Name)
str(dt_combined)
names(dt_combined)
setDT(dt_combined)

# Use your actual data.frame
#dt_combined <- Combined_imp_obs_all

# UI
ui <- fluidPage(
  titlePanel("Imputed versus observed PAF"),
  sidebarLayout(
    sidebarPanel(
      width = 2,
      selectInput("site", "Select Site Name:",
                  choices = unique(dt_combined$Site.Name),
                  selected = unique(dt_combined$Site.Name)[1],
                  multiple = TRUE),
      selectInput("group", "Select PRM Group:",
                  choices = unique(dt_combined$Group),
                  selected = unique(dt_combined$Group)[1],
                  multiple = TRUE),
      selectInput("source", "Select Source:",
                  choices = unique(dt_combined$Source),
                  selected = unique(dt_combined$Source)[1],
                  multiple = TRUE),
      selectInput("flowtype", "Select Flow Type:",
                  choices = unique(dt_combined$Flow.Type),
                  selected = unique(dt_combined$Flow.Type)[1],
                  multiple = FALSE)
    ),
    mainPanel(
      width = 10,
      plotOutput("pafPlot", height = "800px")
    )
  )
)

# Server
server <- function(input, output) {
  filtered_data <- reactive({
    dt_combined %>%
      filter(
        Site.Name %in% input$site,
        Group %in% input$group,
        Source %in% input$source
      )
  })
  
  output$pafPlot <- renderPlot({
    df <- filtered_data()
    
    # Extract flow values for selected Flow.Type
    flow_df <- df %>%
      filter(Flow.Type == input$flowtype) %>%
      transmute(Date, Source, Site.Name, Sampling.Year, Group,
                Flow.Type, Flow.Value = Value)
    
    # Merge with PAF data
    merged_df <- df %>%
      left_join(flow_df,
                by = c("Date", "Source", "Site.Name", "Sampling.Year", "Group"))
    
    # Scale Flow.Value to match PAF range
    paf_max <- max(merged_df$PAF, na.rm = TRUE)
    flow_max <- max(merged_df$Flow.Value, na.rm = TRUE)
    merged_df <- merged_df %>%
      mutate(Flow.Scaled = Flow.Value * paf_max / flow_max)
    
    # Updated custom color mapping
    source_colors <- c(
      "Observed"                      = "#377eb8",  # blue
      "PMM Imputations (Discharge)"   = "#e41a1c",  # red
      "PMM Imputations (Water Level)" = "#4daf4a",  # green
      "Simple Imputation (KD)"        = "#ff7f00"   # orange
    )
    
    ggplot(merged_df, aes(x = Date)) +
      geom_point(aes(y = PAF, color = Source), alpha = 0.7, size = 2.5) +
      geom_line(
        data = merged_df %>% filter(!is.na(Flow.Scaled)),
        aes(x = Date, y = Flow.Scaled),
        color = "blue",
        linewidth = 0.6
      ) +
      scale_color_manual(values = source_colors) +
      scale_y_continuous(
        name = "Percent affected fraction (%)",
        sec.axis = sec_axis(
          transform = ~ . * flow_max / paf_max,
          name = paste(input$flowtype, "(units)")
        )
      ) +
      labs(
        title = NULL,
        x = "Date",
        color = "Source"
      ) +
      facet_wrap(~ Sampling.Year, ncol = 1, scales = "free") +
      theme_bw(base_size = 16) +
      theme(
        plot.title = element_text(size = 20, face = "bold", color = "black"),
        axis.title.x = element_text(size = 18, color = "black"),
        axis.title.y = element_text(size = 18, color = "black"),
        axis.title.y.right = element_text(size = 18, color = "black"),
        axis.text = element_text(size = 14, color = "black"),
        legend.position = "bottom",
        legend.title = element_text(size = 16, color = "black"),
        legend.text = element_text(size = 14, color = "black"),
        strip.text = element_text(size = 16, face = "bold", color = "black")
      )
  })
}

# Run the app
shinyApp(ui = ui, server = server)


# Additinoal plotting--------
library(stringr)
setwd("C:/Users/uqcneela/OneDrive - The University of Queensland/General - Sci SEES Res Reef Catchment Science Partnership/Project 3/Catherine Neelamraju/Infilling/MICE_revised test")
combined_imp_long <- as.data.frame(dt_combined)
str(combined_imp_long)
unique(unlist(combined_imp_long$"Source", use.names = TRUE)) #check
combined_imp_long <- combined_imp_long %>% # fix source names
  mutate(Source = case_when(
    Source == "PMM Imputations (Discharge)" ~ "PMM Imputation (Discharge)",
    Source == "PMM Imputations (Water Level)" ~ "PMM Imputation (Water Level)",
    TRUE ~ Source
  ))
unique(unlist(combined_imp_long$"Source", use.names = TRUE)) #check
unique(unlist(combined_imp_long$"Group", use.names = TRUE)) #check

Comp_Plot <- combined_imp_long %>%
  filter(!Source %in% c("Validation_Discharge", "Validation_WaterLevel"),
         Group == "Total.PRM") %>%
  mutate(Site.Name = str_wrap(Site.Name, width = 12)) %>%  # Wrap long site names
  ggplot(aes(x = Site.Name, y = PAF, fill = Source)) +
  geom_boxplot(outlier.size = 1) +
  # geom_jitter(position = position_jitter(width = 0.2), size = 1, alpha = 0.5) +
  facet_wrap(~ Sampling.Year, scales = "free_y") +
  stat_summary(fun = mean, geom = "point", shape = 18, size = 1.5, color = "red", position = position_dodge(width = 0.75)) +
  scale_fill_brewer(palette = "Blues") +
  # scale_fill_manual(values = c(
  #   "Observed" = "#377eb8",           # Blue
  #   "Imputed_Discharge" = "#e41a1c",  # Red
  #   "Imputed_WaterLevel" = "#ff7f00"  # Orange
  # )) +
  labs(
    x = NULL,
    y = "Toal mixture toxicity (% affected fraction)",
    fill = "Source"
  ) +
  theme_bw(base_size = 14) +
  scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
  theme(
    legend.position = "top",
    axis.text.x = element_text(size = 8)  # Reduce x-axis font size
  )
Comp_Plot
ggsave(filename = "Cf imp v obs by Source_TPAF.png",
       plot = Comp_Plot,
       width = 15, height = 8, dpi = 300)


# statistical comparison ------------
str(dt_combined)
names(dt_combined)
library(dplyr)
library(tidyr)
library(purrr)
library(broom)
dt_combined_comp <- as.data.frame(dt_combined)
str(dt_combined_comp)

# Filter relevant sources
unique(unlist(dt_combined_comp$"Source", use.names = TRUE)) #check
df_comp <- dt_combined_comp %>%
  dplyr::filter(Source %in% c("PMM Imputations (Discharge)", "PMM Imputations (Water Level)")) %>%
  dplyr::select(Site.Code, Site.Name, Sampling.Year, Group, Source, PAF)
unique(unlist(df_comp$"Source", use.names = TRUE)) #check
str(df_comp)

# Ensure Source is a factor
df_comp$Source <- factor(df_comp$Source)

# Define test function
run_source_test <- function(df) {
  sources <- unique(df$Source)
  
  if (length(sources) == 2) {
    # Drop NAs
    df_clean <- df %>% dplyr::filter(!is.na(PAF))
    
    # Check for constant values
    group_stats <- df_clean %>%
      dplyr::group_by(Source) %>%
      dplyr::summarise(n = dplyr::n(), unique_vals = length(unique(PAF)), .groups = "drop")
    
    if (any(group_stats$n < 2)) {
      return(tibble::tibble(statistic = NA, p.value = NA, method = "Too few observations"))
    }
    if (any(group_stats$unique_vals == 1)) {
      return(tibble::tibble(statistic = NA, p.value = NA, method = "Constant values in one group"))
    }
    
    # Run unpaired Wilcoxon test
    result <- tryCatch(
      wilcox.test(PAF ~ Source, data = df_clean),
      error = function(e) return(NULL)
    )
    
    if (!is.null(result)) {
      return(broom::tidy(result) %>% dplyr::mutate(method = "Wilcoxon unpaired test"))
    } else {
      return(tibble::tibble(statistic = NA, p.value = NA, method = "Wilcoxon test failed"))
    }
  } else {
    return(tibble::tibble(statistic = NA, p.value = NA, method = "Insufficient Source levels"))
  }
}


grouped_tests <- df_comp %>%
  group_by(Site.Code, Site.Name, Sampling.Year, Group) %>%
  nest() %>%
  mutate(test_result = map(data, run_source_test)) %>%
  unnest(test_result)

grouped_tests_clean <- grouped_tests %>%
  dplyr::select(-data) %>%  # remove nested data column
  dplyr::ungroup()          # optional: flatten grouping

# write to file
write.csv(grouped_tests_clean, "unparied Wilcoxon_PMM Source comparison.csv", row.names = FALSE)


library(dplyr)
library(tidyr)

# 1. counts per group-source
DT <- as.data.table(df_comp)
df_comp_groups <- DT[, .(n = .N),
                     by = .(Site.Code, Sampling.Year, Group, Source)]

# create a helper column of 1s and aggregate their sum
df_comp$.__one__ <- 1L

df_comp_groups <- aggregate(.__one__ ~ Site.Code + Sampling.Year + Group + Source,
                            data = df_comp,
                            FUN = sum)

# rename the count column
names(df_comp_groups)[names(df_comp_groups) == ".__one__"] <- "n"

# inspect
df_comp_groups
str(df_comp_groups)
write.csv(df_comp_groups, "comparison of group numbers.csv", row.names = FALSE)

# Comparison of relationships obs vs imputed-----------
setwd("C:/Users/uqcneela/OneDrive - The University of Queensland/General - Sci SEES Res Reef Catchment Science Partnership/Project 3/Catherine Neelamraju/Infilling/MICE_revised test")
Q_sites <- read_csv("obs plus imputed data_pmm by site_Q.csv")
WL_sites <- read_csv("obs plus imputed data_pmm by site_WL.csv")

# remove Q columns & rename flow var
str(Q_sites)
Q_sites_reduced <- Q_sites %>%
  select(-c(1:4, 7:9, 11:12, 14:15)) %>%
  dplyr::rename(Value = Discharge_cumecs)
str(Q_sites_reduced) # check

# subset to Q model sites
sites_to_keep <- c(
  "Tully River at Euramo",
  "Barratta Creek at Northcote",
  "Pioneer River at Dumbleton Pump Station Headwater",
  "Sandy Creek at Homebush",
  "Welcome Creek at Gooburrum Road",
  "Yellow Waterholes Creek at Dahls Road"
)

Q_sites_subset <- Q_sites_reduced %>%
  filter(Site.Name %in% sites_to_keep)
str(Q_sites_subset)
unique(unlist(Q_sites_subset$"Site.Name", use.names = TRUE)) #check

# remove WL columns & rename flow var
WL_sites_reduced <- WL_sites %>%
  select(-c(1:4, 7:9, 11:13)) %>%
  dplyr::rename(Value = WaterLevel_m)
str(WL_sites_reduced) # check

# subset to WL model sites
sites_to_keep <- c(
  "Fairydale Drainage at Norton Road"
)

WL_sites_subset <- WL_sites_reduced %>%
  filter(Site.Name %in% sites_to_keep)
str(WL_sites_subset)
unique(unlist(WL_sites_subset$"Site.Name", use.names = TRUE)) #check

# combine
combined_sites <- bind_rows(Q_sites_subset, WL_sites_subset)
str(combined_sites)
unique(unlist(combined_sites$"Site.Name", use.names = TRUE)) #check
unique(unlist(combined_sites$"Source", use.names = TRUE)) #check


# Scatter plot with regression lines and R2 values
library(ggpmisc)
R2_plot <- ggplot(combined_sites, aes(x = Value, y = Total.PRM, color = Source)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = TRUE) +
  stat_regline_equation(
    aes(label = paste(..eq.label.., ..rr.label.., sep = " ~ ")),
    label.x.npc = 0.7,   
    label.y.npc = 0.95,
    size = 4
  ) +
  theme_bw(base_size = 14) +
  labs(
    x = "Log flow (discharge or water level)",
    y = "Percent affected fraction (%)"
  ) +
  scale_x_log10(
    labels = scales::label_number(),
    breaks = scales::breaks_log(n = 6),
    expand = expansion(mult = c(0.02, 0.12))
  ) +
  facet_wrap(~ Site.Name, scales = "free") +
  coord_cartesian(clip = "off") +
  theme(
    plot.margin = margin(10, 22, 10, 10),
    strip.background = element_rect(fill = "grey95", colour = "black",  size = 0.2),
    legend.box.margin = margin(t = -5, b = 5),
    legend.margin = margin(t = -5, b = 5),
    panel.border = element_rect(colour = "black", fill = NA, size = 0.2)  
  )
R2_plot

setwd("C:/Users/uqcneela/OneDrive - The University of Queensland/General - Sci SEES Res Reef Catchment Science Partnership/Project 3/Catherine Neelamraju/Infilling/MICE_revised test")
ggsave(filename = "flow v PAF_obs and imputed.jpg",
       plot = R2_plot,
       width = 23, height = 16, dpi = 300)

write.csv(matched_rows, "daily PAF obs v impu_final PMM models.csv", row.names = FALSE)
