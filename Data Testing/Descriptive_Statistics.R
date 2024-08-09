# Load required libraries
library(tidyverse)
library(dplyr)    # For data manipulation
library(moments)  # For skewness and kurtosis calculations
library(psych)    # For additional descriptive statistics
library(lubridate) # For date manipulation

# Import data
inflation_data <- read.csv("Data/City_Inflation_Differences.csv")

# Determine the most recent year and filter if necessary
max_year <- max(inflation_data$Year)
inflation_data_filtered <- inflation_data |>
  filter(Year > (max_year - 25))

# Reshape the filtered data
inflation_long <- inflation_data_filtered |>
  pivot_longer(
    cols = c(ends_with("Percentage_Change"), Jan, Feb, Mar, Apr, May, Jun, Jul, Aug, Sep, Oct, Nov, Dec),
    names_to = "Month",
    values_to = "Inflation"
  ) |>
  mutate(
    Type = ifelse(grepl("Percentage_Change$", Month), "Egg_Inflation", "Overall_Inflation"),
    Month = ifelse(Type == "Egg_Inflation", 
                   sub("_Percentage_Change$", "", Month),
                   Month),
    Month = match(Month, month.abb),
    Date = ymd(paste(Year, Month, "01"))
  ) |>
  pivot_wider(
    names_from = Type,
    values_from = Inflation
  ) |>
  arrange(Date) |>
  select(Year, Month, Date, Egg_Inflation, Overall_Inflation)

# Function to calculate all descriptive statistics
get_descriptive_stats <- function(x) {
  x_clean <- na.omit(x)  # Remove NA values for skewness and kurtosis calculations
  c(
    n = length(x),
    missing = sum(is.na(x)),
    mean = mean(x, na.rm = TRUE),
    median = median(x, na.rm = TRUE),
    sd = sd(x, na.rm = TRUE),
    min = min(x, na.rm = TRUE),
    max = max(x, na.rm = TRUE),
    range = diff(range(x, na.rm = TRUE)),
    Q1 = quantile(x, 0.25, na.rm = TRUE),
    Q3 = quantile(x, 0.75, na.rm = TRUE),
    IQR = IQR(x, na.rm = TRUE),
    skewness = skewness(x_clean),  # Use clean data for skewness
    kurtosis = kurtosis(x_clean)   # Use clean data for kurtosis
  )
}

# Calculate descriptive statistics
overall_inflation_stats <- get_descriptive_stats(inflation_long$Overall_Inflation)
egg_inflation_stats <- get_descriptive_stats(inflation_long$Egg_Inflation)

# Combine results into a data frame
descriptive_stats <- data.frame(
  Statistic = names(overall_inflation_stats),
  Overall_Inflation = overall_inflation_stats,
  Egg_Inflation = egg_inflation_stats
)

# Print the results
print(descriptive_stats)

# Calculate correlations
correlation <- cor(inflation_long$Overall_Inflation, inflation_long$Egg_Inflation, use = "complete.obs")
print(paste("Correlation between Overall Inflation and Egg Inflation:", round(correlation, 4)))

# Visual summaries
par(mfrow = c(2, 2))

# Histograms
hist(inflation_long$Overall_Inflation, main = "Histogram of Overall Inflation", xlab = "Overall Inflation")
hist(inflation_long$Egg_Inflation, main = "Histogram of Egg Inflation", xlab = "Egg Inflation Difference")

# Box plots
boxplot(inflation_long$Overall_Inflation, main = "Boxplot of Overall Inflation", ylab = "Overall Inflation")
boxplot(inflation_long$Egg_Inflation, main = "Boxplot of Egg Inflation", ylab = "Egg Inflation")

# Reset plot layout
par(mfrow = c(1, 1))

# Additional summary using psych package
psych_summary <- describe(inflation_long[c("Overall_Inflation", "Egg_Inflation")])
print(psych_summary)

# Reshape the data to long format for easier plotting
inflation_comparison <- inflation_long |>
  select(Date, Overall_Inflation, Egg_Inflation) |>
  pivot_longer(cols = c(Overall_Inflation, Egg_Inflation),
               names_to = "Inflation_Type",
               values_to = "Inflation_Rate")

# Create a named vector for custom colors
custom_colors <- c("Overall_Inflation" = "#EB4B33", "Egg_Inflation" = "#EBC531")

# Create the boxplot with custom colors
boxplot_inflation_comparison <- ggplot(inflation_comparison, aes(x = Inflation_Type, y = Inflation_Rate, fill = Inflation_Type)) +
  geom_boxplot() +
  labs(title = "Distribution of Overall Inflation and Egg Inflation (Last 25 Years)",
       x = "Inflation Type",
       y = "Inflation Rate",
       fill = "Inflation Type") +
  theme(legend.position = "none") +  # Remove legend as it's redundant with x-axis labels
  scale_fill_manual(values = custom_colors)  # Use custom colors

# Display the plot
print(boxplot_inflation_comparison)

# If you want to save the plot
ggsave("inflation_comparison_boxplot.png", boxplot_inflation_comparison, width = 10, height = 6, dpi = 300)
