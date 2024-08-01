# Your existing code with modifications
library(tidyr)
library(dplyr)
library(lubridate)
library(ggplot2)
library(plotly)

#import data
inflation_data <- read.csv("Data/City_Inflation_Differences.csv")

#remove NA values
inflation_data <- inflation_data |>
  na.omit()

# Determine the most recent year and filter if necessary
max_year <- max(inflation_data$Year)
inflation_data_filtered <- inflation_data |>
  filter(Year > (max_year - 25))

# Reshape the filtered data
inflation_long <- inflation_data_filtered |>
  pivot_longer(
    cols = c(ends_with("Percentage_Change"), ends_with("Egg_Inflation_Dif")),
    names_to = c("Month", ".value"),
    names_pattern = "(.+)_(Percentage_Change|Egg_Inflation_Dif)"
  ) |>
  rename(
    Overall_Inflation = Percentage_Change,
    Egg_Inflation_Dif = Egg_Inflation_Dif
  ) |>
  mutate(
    Month = match(Month, month.abb),
    Date = ymd(paste(Year, Month, "01"))
  ) |>
  arrange(Date)


qqplot <- ggplot(inflation_long, aes(sample = Overall_Inflation)) +
  stat_qq() +
  stat_qq_line() +
  theme_minimal() +
  labs(
    title = "Q-Q Plot of Overall Inflation",
    x = "Theoretical Quantiles",
    y = "Sample Quantiles"
  )

# Display the plot
print(qqplot)

histogram <- ggplot(inflation_long, aes(x = Overall_Inflation)) +
  geom_histogram(binwidth = 1, fill = "skyblue", color = "black") +
  geom_vline(aes(xintercept = mean(Overall_Inflation)), 
             color = "red", linetype = "dashed", size = 1) +
  theme_minimal() +
  labs(
    title = "Histogram of Overall Inflation",
    x = "Overall Inflation",
    y = "Frequency"
  )

# Display the plot
print(histogram)
