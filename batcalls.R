setwd("~/WEB lab")
data=read.csv("Fixed_MasterFile.csv")
head(data)

# Load required libraries
library(ggplot2)
library(dplyr)
library(lubridate)
library(tidyr)

# Rename species codes to common names
data <- data %>%
  mutate(Common_Name = case_when(
    AUTO_ID == "EPTFUS" ~ "Big Brown Bat",
    AUTO_ID == "LASBOR" ~ "Eastern Red Bat",
    AUTO_ID == "LASCIN" ~ "Hoary Bat",
    AUTO_ID == "LASNOC" ~ "Silver-haired Bat",
    AUTO_ID == "MYOLEI" ~ "Northern Long-eared Bat",
    AUTO_ID == "MYOLUC" ~ "Little Brown Bat",
    AUTO_ID == "MYOSEP" ~ "Eastern Small-footed Bat",
    AUTO_ID == "NoID"   ~ "Unknown Species",
    AUTO_ID == "PERSUB" ~ "Tricolored Bat",
    TRUE ~ AUTO_ID  # Keep any unmatched values unchanged
  ))

# Convert DATE column to Date format and filter out Unknown Species
data <- data %>%
  mutate(DATE = as.Date(DATE, format="%Y-%m-%d")) %>%
  filter(Common_Name != "Unknown Species")  # Remove Unknown Species

# Step 1: Compute total pulses per species per night for each habitat and township
nightly_totals <- data %>%
  group_by(DATE, Habitat, Township, Common_Name) %>%
  summarise(Total_Pulses_Night = sum(PULSES, na.rm = TRUE), .groups = 'drop')

# Step 2: Compute weekly averages of total nightly calls for each habitat and township
weekly_summary <- nightly_totals %>%
  mutate(Week = floor_date(DATE, unit = "week")) %>%
  group_by(Week, Habitat, Township, Common_Name) %>%
  summarise(Average_Nightly_Calls = mean(Total_Pulses_Night, na.rm = TRUE), .groups = 'drop')

# Group Myotis species together
weekly_summary <- weekly_summary %>%
  mutate(Common_Name = ifelse(Common_Name %in% c("Little Brown Bat", "Northern Long-eared Bat"), 
                              "Myotis spp.", Common_Name))

# Function to apply LOESS smoothing
smooth_loess <- function(df) {
  if (nrow(df) < 3) return(df)  # Avoid errors on small datasets
  loess_fit <- loess(Average_Nightly_Calls ~ as.numeric(Week), data = df, span = 0.4)
  df$Smoothed_Calls <- predict(loess_fit)
  df$Smoothed_Calls <- pmax(df$Smoothed_Calls, 0)  # Ensure no values go below 0
  return(df)
}

# Apply smoothing to each species within each habitat-township
smoothed_data <- weekly_summary %>%
  group_by(Habitat, Township, Common_Name) %>%
  group_modify(~ smooth_loess(.x)) %>%
  ungroup()

# Summarize total calls per species per month for all habitat-township combinations
monthly_totals <- smoothed_data %>%
  mutate(Month = floor_date(Week, "month")) %>%
  group_by(Month, Habitat, Township, Common_Name) %>%
  summarise(Total_Calls = sum(Average_Nightly_Calls, na.rm = TRUE), .groups = 'drop') %>%
  pivot_wider(names_from = Common_Name, values_from = Total_Calls, values_fill = 0)  # Fill missing species with 0

# Create a directory for saving figures
output_dir <- "Bat_Call_Figures"
if (!dir.exists(output_dir)) {
  dir.create(output_dir)
}

# Extract unique habitat-township combinations
habitat_town_combinations <- smoothed_data %>%
  distinct(Habitat, Township) %>%
  arrange(Habitat, Township)

# Filter data for the current habitat-township combination
filtered_data <- smoothed_data %>%
  filter(Habitat == habitat_name, Township == township_name) %>%
  arrange(Week) %>%
  na.omit()  # Remove any rows with missing values

# Check if data exists for this combination
if(nrow(filtered_data) > 0) {
  
  # Generate the multi-panel figure for this habitat-township
  plot <- ggplot(filtered_data, 
                 aes(x = Week, y = pmax(0, Smoothed_Calls), color = Common_Name)) +  # Ensure values are ≥ 0
    geom_line(linewidth = 1.2) +  # Smoothed line
    facet_wrap(~ Common_Name, scales = "free_y") +  # Each species gets its own panel
    labs(title = paste("Average Calls Per Night -", habitat_name, "-", township_name),
         x = "Week",
         y = "Smoothed Calls Per Night",
         color = "Species") +
    theme_light() +  # Force light mode
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  # Define file name
  file_name <- paste0(output_dir, "/Bat_Calls_", gsub(" ", "_", habitat_name), "_", gsub(" ", "_", township_name), ".png")
  
  # Save the plot with a forced white background
  ggsave(filename = file_name, plot = plot, width = 10, height = 7, dpi = 300, bg = "white")
  
} else {
  print(paste("No data available for", habitat_name, "-", township_name))
}


#####Now some myotis stuff

library(ggplot2)
library(dplyr)
library(lubridate)

# Step 1: Merge water-related categories into "Water"
weekly_summary <- weekly_summary %>%
  mutate(Habitat = case_when(
    Habitat %in% c("Little_Water", "Big_Water", "Water") ~ "Water",
    TRUE ~ Habitat  # Keep other habitats unchanged
  ))

# Step 2: Filter for Myotis spp. and aggregate by habitat (ignoring township)
myotis_data <- weekly_summary %>%
  filter(Common_Name == "Myotis spp.") %>%
  group_by(Week, Habitat) %>%
  summarise(
    Mean_Calls = mean(Average_Nightly_Calls, na.rm = TRUE),
    SE_Calls = ifelse(is.na(sd(Average_Nightly_Calls, na.rm = TRUE)), 0, 
                      sd(Average_Nightly_Calls, na.rm = TRUE) / sqrt(n())),  # Avoid NA issues
    .groups = 'drop'
  ) %>%
  na.omit()  # Remove any remaining NA values

# Aggregate Myotis data using SD instead of SE
myotis_data <- weekly_summary %>%
  filter(Common_Name == "Myotis spp.") %>%
  mutate(Habitat = ifelse(Habitat %in% c("Little Water", "Big Water", "Water"), "Water", Habitat)) %>%
  group_by(Week, Habitat) %>%
  summarise(
    Mean_Calls = mean(Average_Nightly_Calls, na.rm = TRUE),
    SD_Calls = sd(Average_Nightly_Calls, na.rm = TRUE),  # Using Standard Deviation
    .groups = 'drop'
  ) %>%
  na.omit()  # Remove any remaining NA values

# Generate the multi-panel plot with SD as error shading
myotis_data <- myotis_data %>%
  mutate(SD_Calls = ifelse(is.na(SD_Calls), 0, SD_Calls))  # Replace NA SDs with 0

ggplot(myotis_data, aes(x = Week, y = Mean_Calls, color = Habitat, fill = Habitat)) +
  geom_ribbon(aes(ymin = pmax(0, Mean_Calls - SD_Calls), ymax = Mean_Calls + SD_Calls), alpha = 0.3) +  # SD error area
  geom_line(linewidth = 1.2) +  # Myotis calls trend line
  facet_wrap(~ Habitat, scales = "free_y") +  # Separate panels per habitat
  labs(title = "Average Calls Per Night for Myotis spp. by Habitat",
       x = "Week",
       y = "Mean Calls Per Night ± SD") +
  theme_light() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none")  # Removes legend if facet labels exist



# Remove zero values before applying log transformation
myotis_data_log <- myotis_data %>%
  filter(Mean_Calls > 0) %>%  # Exclude zero values to avoid log(0) issues
  mutate(
    Log_Mean_Calls = log(Mean_Calls),  # Apply natural log transformation
    Log_SD_Calls = SD_Calls / Mean_Calls  # Convert SD to relative scale
  )

# Plot the log-transformed Myotis calls
ggplot(myotis_data_log, aes(x = Week, y = Log_Mean_Calls, color = Habitat, fill = Habitat)) +
  geom_ribbon(aes(ymin = Log_Mean_Calls - Log_SD_Calls, 
                  ymax = Log_Mean_Calls + Log_SD_Calls), alpha = 0.3) +  # SD error area
  geom_line(linewidth = 1.2) +  # Myotis calls trend line
  facet_wrap(~ Habitat, scales = "free_y") +  # Separate panels per habitat
  labs(title = "Natural Log-Transformed Myotis Acoustic Activity by Habitat",
       x = "Week",
       y = "ln(Mean Calls) ± SD") +
  theme_light() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none")  # Removes legend if facet labels exist



##Included bugs
# Load required libraries
library(ggplot2)
library(dplyr)
library(lubridate)
library(tidyr)
library(zoo)  # For interpolation

insects=read.csv("BugCollection.csv")
insects <- insects %>%
  mutate(Date = as.Date(Date, format = "%Y-%m-%d"),
         Quantity = as.numeric(Quantity))

insects <- insects %>%
  mutate(Date = as.Date(Date, format = "%Y-%m-%d"),
         Quantity = as.numeric(Quantity),
         Habitat = ifelse(Habitat %in% c("Little Water", "Big Water", "Water"), "Water", Habitat))


weekly_insect_summary <- insects %>%
  mutate(Week = floor_date(Date, "week")) %>%
  group_by(Week, Habitat) %>%
  summarise(Total_Insects_Per_Week = sum(Quantity, na.rm = TRUE), .groups = "drop")


full_weeks <- expand.grid(
  Week = seq(min(weekly_insect_summary$Week, na.rm = TRUE), 
             max(weekly_insect_summary$Week, na.rm = TRUE), by = "week"),
  Habitat = unique(weekly_insect_summary$Habitat)
)

weekly_insect_summary <- full_weeks %>%
  left_join(weekly_insect_summary, by = c("Week", "Habitat")) %>%
  group_by(Habitat) %>%
  mutate(Total_Insects_Per_Week = zoo::na.approx(Total_Insects_Per_Week, na.rm = FALSE)) %>%
  ungroup()


ggplot(weekly_insect_summary, aes(x = Week, y = Total_Insects_Per_Week, fill = Habitat)) +
  geom_bar(stat = "identity", position = "dodge", alpha = 0.7) +
  facet_wrap(~ Habitat, scales = "free_y") +
  labs(title = "Total Insect Abundance",
       x = "Week",
       y = "Insect Abundance") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_manual(values = c("Agriculture" = "red", "Forest" = "blue", 
                               "Grass" = "green", "Urban" = "purple", "Water" = "orange"))

##Myotis and bugs

insect_weekly <- insects %>%
  mutate(Date = as.Date(Date, format = "%Y-%m-%d"),
         Week = floor_date(Date, "week"),
         Habitat = case_when(
           Habitat %in% c("Little Water", "Big Water", "Water") ~ "Water",
           TRUE ~ Habitat)) %>%
  group_by(Week, Habitat) %>%
  summarise(Total_Insects_Per_Week = sum(as.numeric(Quantity), na.rm = TRUE),
            .groups = 'drop')

combined_data <- left_join(insect_weekly, myotis_data, by = c("Week", "Habitat"))

# Ensure Myotis spp. is correctly grouped and contains Average_Nightly_Calls
myotis_data <- weekly_summary %>%
  filter(Common_Name == "Myotis spp.") %>%
  group_by(Week, Habitat) %>%
  summarise(
    Mean_Calls = mean(Average_Nightly_Calls, na.rm = TRUE),
    SD_Calls = sd(Average_Nightly_Calls, na.rm = TRUE),  # Use Standard Deviation instead of SE
    .groups = 'drop'
  ) %>%
  na.omit()  # Remove any remaining NA values


# Ensure date format is correct
insects <- insects %>%
  mutate(Date = as.Date(Date, format = "%Y-%m-%d"),
         Quantity = as.numeric(Quantity),
         Habitat = ifelse(Habitat %in% c("Little Water", "Big Water", "Water"), "Water", Habitat))

# Aggregate insect data weekly, summing per habitat
weekly_insect_summary <- insects %>%
  mutate(Week = floor_date(Date, "week")) %>%
  group_by(Week, Habitat) %>%
  summarise(Total_Insects_Per_Week = sum(Quantity, na.rm = TRUE), .groups = "drop")

# Create full weekly sequence for interpolation
full_weeks <- expand.grid(
  Week = seq(min(weekly_insect_summary$Week, na.rm = TRUE), 
             max(weekly_insect_summary$Week, na.rm = TRUE), by = "week"),
  Habitat = unique(weekly_insect_summary$Habitat)
)

# Fill in missing weeks using interpolation
weekly_insect_summary <- full_weeks %>%
  left_join(weekly_insect_summary, by = c("Week", "Habitat")) %>%
  group_by(Habitat) %>%
  mutate(Total_Insects_Per_Week = zoo::na.approx(Total_Insects_Per_Week, na.rm = FALSE)) %>%
  ungroup()

# Process Myotis spp. acoustic data (ensuring Water habitats are merged)
myotis_data <- weekly_summary %>%
  filter(Common_Name == "Myotis spp.") %>%
  mutate(Habitat = ifelse(Habitat %in% c("Little Water", "Big Water", "Water"), "Water", Habitat)) %>%
  group_by(Week, Habitat) %>%
  summarise(
    Mean_Calls = mean(Average_Nightly_Calls, na.rm = TRUE),
    SD_Calls = sd(Average_Nightly_Calls, na.rm = TRUE),  
    .groups = 'drop'
  )

# Merge insect and Myotis datasets
combined_data <- left_join(weekly_insect_summary, myotis_data, by = c("Week", "Habitat"))


###Myotis issue doing calls per night

myotis_data <- weekly_summary %>%
  filter(Common_Name == "Myotis spp.") %>%
  group_by(Week, Habitat) %>%
  summarise(
    Mean_Nightly_Calls = mean(Average_Nightly_Calls, na.rm = TRUE),  # Ensuring the correct variable
    SD_Calls = sd(Average_Nightly_Calls, na.rm = TRUE),  # Standard deviation
    .groups = 'drop'
  ) 


combined_data <- left_join(weekly_insect_summary, myotis_data, by = c("Week", "Habitat"))

ggplot(combined_data, aes(x = Week)) +
  # Insect abundance (left y-axis) as bars
  geom_bar(aes(y = Total_Insects_Per_Week, fill = Habitat), 
           stat = "identity", position = "dodge", alpha = 0.7) +
  
  # Myotis calls (right y-axis) with SD shading (FIXED)
  geom_ribbon(aes(ymin = pmax(0, Mean_Nightly_Calls - SD_Calls), 
                  ymax = Mean_Nightly_Calls + SD_Calls), 
              alpha = 0.3, fill = "black") +  # ✅ Parentheses correctly closed
  
  geom_line(aes(y = Mean_Nightly_Calls), color = "black", linewidth = 1.2) +
  
  # Facet by habitat
  facet_wrap(~ Habitat, scales = "free_y") +
  
  # Labels
  labs(title = "Myotis Acoustic Activity vs. Insect Abundance",
       x = "Week") +
  
  # Explicit y-axis limits
  scale_y_continuous(
    name = "Total Insect Occurrences", 
    sec.axis = sec_axis(~ ., name = "Mean Myotis Calls Per Night ± SD")
  ) +
  
  # Themes and colors
  theme_light() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none") +
  scale_fill_manual(values = c("Agriculture" = "red", "Forest" = "blue", 
                               "Grass" = "green", "Urban" = "purple", "Water" = "orange"))


###########Log calls +insects 
install.packages("scales")  # Run this if you haven't installed it
library(scales)

sec.axis = sec_axis(~ rescale(., from = c(0, 7500), to = c(2, 12)), 
                    name = "Log+1 Mean Myotis Calls Per Night ± SD")


min_log_calls <- min(log1p(combined_data$Mean_Nightly_Calls))  # 1.5
max_log_calls <- max(log1p(combined_data$Mean_Nightly_Calls))  # 6

###MIGHT HAVE DONE IT!!!
scaling_factor <- (7000 - 1000) / (max(log1p(combined_data$Mean_Nightly_Calls)) - min(log1p(combined_data$Mean_Nightly_Calls)))  # Scale to 2-12 range

ggplot(combined_data, aes(x = Week)) +
  # Insect abundance (left y-axis) as bars
  geom_bar(aes(y = Total_Insects_Per_Week, fill = Habitat), 
           stat = "identity", position = "dodge", alpha = 0.7) +
  
  # Myotis calls (right y-axis) with SD shading (log-transformed correctly)
  geom_ribbon(aes(ymin = 2 + scaling_factor * (log1p(pmax(0, Mean_Nightly_Calls - SD_Calls, Mean_Nightly_Calls / 2)) - min(log1p(combined_data$Mean_Nightly_Calls))), 
                  ymax = 2 + scaling_factor * (log1p(Mean_Nightly_Calls + SD_Calls) - min(log1p(combined_data$Mean_Nightly_Calls)))), 
              alpha = 0.3, fill = "black") +  
  
  # Myotis call trend line (log-transformed properly & scaled)
  geom_line(aes(y = 2 + scaling_factor * (log1p(Mean_Nightly_Calls) - min(log1p(combined_data$Mean_Nightly_Calls)))), 
            color = "black", linewidth = 1.2) +
  
  # Facet by habitat
  facet_wrap(~ Habitat, scales = "free_y") +
  
  # Labels
  labs(title = "Myotis Acoustic Activity vs. Insect Abundance",
       x = "Week") +
  
  # Fix y-axes
  scale_y_continuous(
    name = "Total Insect Occurrences", 
    limits = c(0, 7500),  # Fix insect scale
    sec.axis = sec_axis(~ ((. - 2) / scaling_factor) + min(log1p(combined_data$Mean_Nightly_Calls)), 
                        name = "Log+1 Mean Myotis Calls Per Night ± SD")
  ) +
  
  # Themes and colors
  theme_light() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none") +
  scale_fill_manual(values = c("Agriculture" = "red", "Forest" = "blue", 
                               "Grass" = "green", "Urban" = "purple", "Water" = "orange"))

##Test to see if bat calls is related to insect abundance 
##checking normality 

shapiro.test(combined_data$Total_Insects_Per_Week)  # Check normality of insects = not normal
shapiro.test(combined_data$Mean_Nightly_Calls)  # Check normality of bat calls = not normal

cor.test(combined_data$Total_Insects_Per_Week, combined_data$Mean_Nightly_Calls, method = "spearman")

library(mgcv)

gam_model <- gam(Mean_Nightly_Calls ~ s(Total_Insects_Per_Week) + Habitat, family = nb(), data = combined_data)

summary(gam_model)
plot(gam_model)

##look at each habitat 
combined_data$Habitat <- as.factor(combined_data$Habitat)

gam_model_interact <- gam(Mean_Nightly_Calls ~ s(Total_Insects_Per_Week, by = Habitat) + Habitat, 
                          family = nb(), data = combined_data)
summary(gam_model_interact)

ggplot(combined_data, aes(x = Total_Insects_Per_Week, y = Mean_Nightly_Calls, color = Habitat)) +
  geom_point() +
  geom_smooth(method = "gam", formula = y ~ s(x), se = TRUE) +
  facet_wrap(~ Habitat, scales = "free") +
  labs(title = "Bat Calls vs. Insect Abundance by Habitat",
       x = "Weekly Insect Abundance",
       y = "Mean Weekly Bat Calls")

