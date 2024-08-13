#####################################################
# Part Five - Data Visualization & Pattern Extraction
#####################################################

library(ggplot2)
library(dplyr)
library(scales)
library(readr)
library(glue)
library(utils)
library(ggpmisc)

# Call the script preProcess.R to request and download data and preprocess it
result <- source('PreProcess.R')

# Extracting the four dataframes that we receive from preprocess.R
finalData <- result$value$finalData
dailyAverages <- result$value$dailyAverages
monthlyAverages <- result$value$monthlyAverages
yearlyAverages <- result$value$yearlyAverages


yearly_averages_region <- yearlyAverages %>%
  group_by(Year) %>%
  summarise(
    Avg_Temperature = mean(Avg_Temperature, na.rm = TRUE),
    Avg_DNI = mean(Avg_DNI, na.rm = TRUE),
    Avg_Wind_Speed = mean(Avg_Wind_Speed, na.rm = TRUE)
  )



#############################################
# Temperature Section
#############################################

### 
# Yearly Temperature
###


# Convert Avg_Temperature from Celsius to Fahrenheit in the yearlyAverages dataframe
yearlyAverages$Avg_Temperature <- (yearlyAverages$Avg_Temperature * 9/5) + 32

# Get the unique years in the yearlyAverages dataset
years <- unique(yearlyAverages$Year)

# Find the maximum, min, mean, and stdev of the averages
max_temp <- max(yearlyAverages$Avg_Temperature, na.rm = TRUE)
min_temp <- min(yearlyAverages$Avg_Temperature, na.rm = TRUE)
mean_temp <- mean(yearlyAverages$Avg_Temperature, na.rm = TRUE)
stdev_temp <- sd(yearlyAverages$Avg_Temperature, na.rm = TRUE)

# Save summary statistics to a dataframe
summary_stats <- data.frame(
  Metric = c("Max Yearly-Avg Temperature", "Min Yearly-Avg Temperature", "Mean Yearly-Avg Temperature", "Standard Deviation of Yearly-Avgs"),
  Value = c(max_temp, min_temp, mean_temp, stdev_temp)
)

plots_dir <- "Plots"
# Create the directory if it doesn't exist, otherwise empty it
if (!dir.exists(plots_dir)) {
  dir.create(plots_dir)
} else {
  # If the directory already exists, delete all files and folders within it
  files_and_dirs <- list.files(plots_dir, full.names = TRUE)
  unlink(files_and_dirs, recursive = TRUE)
}



# Loop through each year and create a heatmap
for (year in years) {
  # Filter the data for the current year
  yearly_data <- yearlyAverages %>% filter(Year == year)
  
  # Create a heatmap for the current year
  plot <- ggplot(yearly_data, aes(x = Longitude, y = Latitude, fill = Avg_Temperature)) +
    geom_tile() +
    scale_fill_viridis_c(option = "inferno", direction = -1, limits = c(min_temp, max_temp)) +
    labs(title = paste("Yearly Temperature Averages (", year, ")", sep = ""),
         x = "Longitude",
         y = "Latitude",
         fill = "Temperature (°F)") +
    theme_minimal()
  
  # Display the plot
  print(plot)
  
  temperature_dir <- "Plots/Temperature"
  if (!dir.exists(temperature_dir)) {
    dir.create(temperature_dir)
  }
  
  # Construct the file name for saving
  file_name <- paste0("Plots/Temperature/Temperature_Yearly_", year, ".png")
  
  # Save the plot
  ggsave(filename = file_name, plot = plot, width = 8, height = 6)
}

# Calculate the 90th percentile of the Temperature values in the yearlyAverages dataset
temp_90th_percentile_yearly <- quantile(yearlyAverages$Avg_Temperature, 0.90, na.rm = TRUE)

# Filter the dataset to get the coordinates with Temperature values in the 90th percentile or above
high_temp_coordinates_yearly <- yearlyAverages %>%
  filter(Avg_Temperature >= temp_90th_percentile_yearly) %>%
  select(Avg_Temperature, Longitude, Latitude)


# Write high_temp_coordinates_yearly and summary_stats to CSV files
write.csv(high_temp_coordinates_yearly, "Output_Data/TempMAX_coordinates_yearly.csv", row.names = FALSE)
write.csv(summary_stats, "Output_Data/Temp_yearly_summary_stats.csv", row.names = FALSE)

print("Temperature data saved.")


###
# Monthly Temperature
###


# Convert Avg_Temperature from Celsius to Fahrenheit in the monthlyAverages dataframe
monthlyAverages$Avg_Temperature <- (monthlyAverages$Avg_Temperature * 9/5) + 32

# Get the unique months in the monthlyAverages dataset
months <- unique(paste(monthlyAverages$Year, monthlyAverages$Month, sep = "-"))

# Find the maximum, min, mean, and stdev of the averages
max_temp <- max(monthlyAverages$Avg_Temperature, na.rm = TRUE)
min_temp <- min(monthlyAverages$Avg_Temperature, na.rm = TRUE)
mean_temp <- mean(monthlyAverages$Avg_Temperature, na.rm = TRUE)
stdev_temp <- sd(monthlyAverages$Avg_Temperature, na.rm = TRUE)

# Loop through each month and create a heatmap
for (month in months) {
  # Split the month into Year and Month parts
  year_month <- strsplit(month, "-")[[1]]
  year <- as.numeric(year_month[1])
  month_num <- as.numeric(year_month[2])
  
  # Filter the data for the current month
  monthly_data <- monthlyAverages %>% filter(Year == year, Month == month_num)
  
  # Create a heatmap for the current month
  plot <- ggplot(monthly_data, aes(x = Longitude, y = Latitude, fill = Avg_Temperature)) +
    geom_tile() +
    scale_fill_viridis_c(option = "inferno", direction = -1, limits = c(min_temp, max_temp)) +
    labs(title = paste("Monthly Temperature Averages (", year, "-", month_num, ")", sep = ""),
         x = "Longitude",
         y = "Latitude",
         fill = "Temperature (°F)") +
    theme_minimal()
  
  # Display the plot
  print(plot)
  
  # Construct the file name for saving
  file_name <- paste0("Plots/Temperature/Temperature_Monthly_", year, "_", month_num, ".png")
  
  # Save the plot
  ggsave(filename = file_name, plot = plot, width = 8, height = 6)
  
}



#############################################
# Solar Radiation Section
#############################################

### 
# Yearly DNI
###

#Find the maximum, min, mean, and stdev of the averages
max_dni <- max(yearlyAverages$Avg_DNI, na.rm = TRUE)
min_dni <- min(yearlyAverages$Avg_DNI, na.rm = TRUE)
mean_dni <- mean(yearlyAverages$Avg_DNI, na.rm = TRUE)
stdev_dni <- sd(yearlyAverages$Avg_DNI, na.rm = TRUE)

# Loop through each year and create a heatmap
for (year in years) {
  # Filter the data for the current year
  yearly_data <- yearlyAverages %>% filter(Year == year)
  
  # Create a heatmap for the current year
  plot <- ggplot(yearly_data, aes(x = Longitude, y = Latitude, fill = Avg_DNI)) +
    geom_tile() +
    scale_fill_viridis_c(option = "inferno", direction = -1, limits = c(min_dni, max_dni)) +
    labs(title = paste("Yearly Solar DNI Averages (", year, ")", sep = ""),
         x = "Longitude",
         y = "Latitude",
         fill = "DNI") +
    theme_minimal()
  
  # Display the plot
  print(plot)
  
  solar_dir <- "Plots/Solar"
  if (!dir.exists(solar_dir)) {
    dir.create(solar_dir)
  }
  
  # Construct the file name for saving
  file_name <- paste0("Plots/Solar/Solar_Yearly_", year, ".png")
  
  # Save the plot
  ggsave(filename = file_name, plot = plot, width = 8, height = 6)
  
}


# Calculate the 90th percentile of the DNI values in the yearlyAverages dataset
dni_90th_percentile_yearly <- quantile(yearlyAverages$Avg_DNI, 0.90, na.rm = TRUE)

# Filter the dataset to get the coordinates with DNI values in the 90th percentile or above
high_dni_coordinates_yearly <- yearlyAverages %>%
  filter(Avg_DNI >= dni_90th_percentile_yearly) %>%
  select(Avg_DNI, Longitude, Latitude)


# Write high_dni_coordinates_yearly and summary_stats to CSV files
write.csv(high_dni_coordinates_yearly, "Output_Data/SolarMAX_coordinates_yearly.csv", row.names = FALSE)
write.csv(summary_stats, "Output_Data/Solar_yearly_summary_stats.csv", row.names = FALSE)

print("Solar data saved.")


###
# Monthly
###

# Find the maximum, min, mean, and stdev of the averages
max_dni <- max(monthlyAverages$Avg_DNI, na.rm = TRUE)
min_dni <- min(monthlyAverages$Avg_DNI, na.rm = TRUE)
mean_dni <- mean(monthlyAverages$Avg_DNI, na.rm = TRUE)
stdev_dni <- sd(monthlyAverages$Avg_DNI, na.rm = TRUE)

# Loop through each month and create a heatmap
for (month in months) {
  # Split the month into Year and Month parts
  year_month <- strsplit(month, "-")[[1]]
  year <- as.numeric(year_month[1])
  month_num <- as.numeric(year_month[2])
  
  # Filter the data for the current month
  monthly_data <- monthlyAverages %>% filter(Year == year, Month == month_num)
  
  # Create a heatmap for the current month
  plot <- ggplot(monthly_data, aes(x = Longitude, y = Latitude, fill = Avg_DNI)) +
    geom_tile() +
    scale_fill_viridis_c(option = "inferno", direction = -1, limits = c(min_dni, max_dni)) +
    labs(title = paste("Monthly Solar DNI Averages (W/m^2) (", year, "-", month_num, ")", sep = ""),
         x = "Longitude",
         y = "Latitude",
         fill = "DNI") +
    theme_minimal()
  
  # Display the plot
  print(plot)
  
  # Construct the file name for saving
  file_name <- paste0("Plots/Solar/Solar_Monthly_", year, "_", month_num, ".png")
  
  # Save the plot
  ggsave(filename = file_name, plot = plot, width = 8, height = 6)
  
}


#############################################
# Wind Section
#############################################

### 
# Yearly Wind Speeds
###

#Find the maximum, min, mean, and stdev of the averages
max_wind <- max(yearlyAverages$Avg_Wind_Speed, na.rm = TRUE)
min_wind <- min(yearlyAverages$Avg_Wind_Speed, na.rm = TRUE)
mean_wind <- mean(yearlyAverages$Avg_Wind_Speed, na.rm = TRUE)
stdev_wind <- sd(yearlyAverages$Avg_Wind_Speed, na.rm = TRUE)

# Loop through each year and create a heatmap
for (year in years) {
  # Filter the data for the current year
  yearly_data <- yearlyAverages %>% filter(Year == year)
  
  # Create a heatmap for the current year
  plot <- ggplot(yearly_data, aes(x = Longitude, y = Latitude, fill = Avg_Wind_Speed)) +
    geom_tile() +
    scale_fill_viridis_c(option = "viridis", direction = -1, limits = c(min_wind, max_wind)) +
    labs(title = paste("Yearly Wind Speed Averages (", year, ")", sep = ""),
         x = "Longitude",
         y = "Latitude",
         fill = "Wind Speed") +
    theme_minimal()
  
  # Display the plot
  print(plot)
  
  wind_dir <- "Plots/Wind"
  if (!dir.exists(wind_dir)) {
    dir.create(wind_dir)
  }
  
  # Construct the file name for saving
  file_name <- paste0("Plots/Wind/Wind_yearly", year, ".png")
  
  # Save the plot
  ggsave(filename = file_name, plot = plot, width = 8, height = 6)
  
}


# Calculate the 90th percentile of the WIND values in the yearlyAverages dataset
wind_90th_percentile_yearly <- quantile(yearlyAverages$Avg_Wind_Speed, 0.90, na.rm = TRUE)

# Filter the dataset to get the coordinates with WIND values in the 90th percentile or above
high_wind_coordinates_yearly <- yearlyAverages %>%
  filter(Avg_Wind_Speed >= wind_90th_percentile_yearly) %>%
  select(Avg_Wind_Speed, Longitude, Latitude)

# Write high_wind_coordinates_yearly and summary_stats to CSV files
write.csv(high_wind_coordinates_yearly, "Output_Data/WindMAX_coordinates_yearly.csv", row.names = FALSE)
write.csv(summary_stats, "Output_Data/Wind_yearly_summary_stats.csv", row.names = FALSE)

print("Wind data saved")

###
# Monthly Wind
###

# Find the maximum, min, mean, and stdev of the averages
max_wind <- max(monthlyAverages$Avg_Wind_Speed, na.rm = TRUE)
min_wind <- min(monthlyAverages$Avg_Wind_Speed, na.rm = TRUE)
mean_wind <- mean(monthlyAverages$Avg_Wind_Speed, na.rm = TRUE)
stdev_wind <- sd(monthlyAverages$Avg_Wind_Speed, na.rm = TRUE)

# Loop through each month and create a heatmap
for (month in months) {
  # Split the month into Year and Month parts
  year_month <- strsplit(month, "-")[[1]]
  year <- as.numeric(year_month[1])
  month_num <- as.numeric(year_month[2])
  
  # Filter the data for the current month
  monthly_data <- monthlyAverages %>% filter(Year == year, Month == month_num)
  
  # Create a heatmap for the current month
  plot <- ggplot(monthly_data, aes(x = Longitude, y = Latitude, fill = Avg_Wind_Speed)) +
    geom_tile() +
    scale_fill_viridis_c(option = "viridis", direction = -1, limits = c(min_wind, max_wind)) +
    labs(title = paste("Monthly Wind Speed Averages (", year, "-", month_num, ")", sep = ""),
         x = "Longitude",
         y = "Latitude",
         fill = "Wind Speed") +
    theme_minimal()
  
  # Display the plot
  print(plot)
  
  
  # Construct the file name for saving
  file_name <- paste0("Plots/Wind/Wind_Monthly_", year, "_", month_num, ".png")
  
  # Save the plot
  ggsave(filename = file_name, plot = plot, width = 8, height = 6)
}


###############################
#  Plotting sections complete!
###############################


###############################
# Bar Charts Section
###############################

# ### This section was working, but for some reason is not working now.
# # Temperature Bar Chart
# temp_plot <- ggplot(yearly_averages_region, aes(x = as.numeric(Year))) +
#   geom_bar(aes(y = Avg_Temperature), stat = "identity", fill = "red", width = 0.5) +
#   geom_smooth(aes(y = Avg_Temperature, group = 1), method = "lm", se = FALSE, color = "white", linetype = "dotted") +
#   stat_poly_eq(aes(label = after_stat(eq.label), y = Avg_Temperature), 
#                formula = y ~ x, parse = TRUE, size = 3.5, geom = "label", fill = "black", color = "white", 
#                label.x = "center", label.padding = unit(0.2, "lines")) +
#   labs(title = "Yearly Averages: Temperature",
#        x = "Year", y = "Avg Temperature (°F)") +
#   theme_minimal(base_family = "Arial") +
#   theme(
#     plot.background = element_rect(fill = "black", color = "black"),
#     panel.background = element_rect(fill = "black", color = "black"),
#     panel.grid.major = element_line(color = "gray50"),
#     panel.grid.minor = element_line(color = "gray50"),
#     axis.text = element_text(color = "white"),
#     axis.title = element_text(color = "white"),
#     plot.title = element_text(color = "white", hjust = 0.5)
#   )
# 
# # Save the Temperature plot
# ggsave(filename = "Plots/Temperature/Yearly_Temperature_Plot.png", plot = temp_plot, width = 8, height = 6)
# 
# ### This section was working, but for some reason is not working now.
# # Solar (DNI) Bar Chart
# solar_plot <- ggplot(yearly_averages_region, aes(x = as.numeric(Year))) +
#   geom_bar(aes(y = Avg_DNI), stat = "identity", fill = "yellow", width = 0.5) +
#   geom_smooth(aes(y = Avg_DNI, group = 1), method = "lm", se = FALSE, color = "white", linetype = "dotted") +
#   stat_poly_eq(aes(label = after_stat(eq.label), y = Avg_DNI), 
#                formula = y ~ x, parse = TRUE, size = 3.5, geom = "label", fill = "black", color = "white", 
#                label.x = "center", label.padding = unit(0.2, "lines")) +
#   labs(title = "Yearly Averages: Solar (DNI)",
#        x = "Year", y = expression(paste("Avg DNI (W/m"^2,")"))) +
#   theme_minimal(base_family = "Arial") +
#   theme(
#     plot.background = element_rect(fill = "black", color = "black"),
#     panel.background = element_rect(fill = "black", color = "black"),
#     panel.grid.major = element_line(color = "gray50"),
#     panel.grid.minor = element_line(color = "gray50"),
#     axis.text = element_text(color = "white"),
#     axis.title = element_text(color = "white"),
#     plot.title = element_text(color = "white", hjust = 0.5)
#   )
# 
# # Save the Solar plot
# ggsave(filename = "Plots/Solar/Yearly_Solar_Plot.png", plot = solar_plot, width = 8, height = 6)
# 
# 
# ### This section was working, but for some reason is not working now.
# # # Wind Speed Bar Chart
# # wind_plot <- ggplot(yearly_averages_region, aes(x = as.numeric(Year))) +
# #   geom_bar(aes(y = Avg_Wind_Speed), stat = "identity", fill = "lightblue", width = 0.5) +
# #   geom_smooth(aes(y = Avg_Wind_Speed, group = 1), method = "lm", se = FALSE, color = "white", linetype = "dotted") +
# #   stat_poly_eq(aes(label = after_stat(eq.label), y = Avg_Wind_Speed), 
# #                formula = y ~ x, parse = TRUE, size = 3.5, geom = "label", fill = "black", color = "white", 
# #                label.x = "center", label.padding = unit(0.2, "lines")) +
# #   labs(title = "Yearly Averages: Wind Speed",
# #        x = "Year", y = "Avg Wind Speed (m/s)") +
# #   theme_minimal(base_family = "Arial") +
# #   theme(
# #     plot.background = element_rect(fill = "black", color = "black"),
# #     panel.background = element_rect(fill = "black", color = "black"),
# #     panel.grid.major = element_line(color = "gray50"),
# #     panel.grid.minor = element_line(color = "gray50"),
# #     axis.text = element_text(color = "white"),
# #     axis.title = element_text(color = "white"),
# #     plot.title = element_text(color = "white", hjust = 0.5)
# #   )
# # 
# # # Save the Wind Speed Chart
# # ggsave(filename = "Plots/Wind/Yearly_Wind_Plot.png", plot = wind_plot, width = 8, height = 6)
# 
