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

overall_inflation_scatterplot <- ggplot(inflation_long, aes(x = Date, y = Overall_Inflation)) +
  geom_point() +  # Add points
  geom_smooth(method = "lm", se = FALSE, color = "red")

egg_inflation_scatterplot <- ggplot(inflation_long, aes(x = Date, y = Egg_Inflation_Dif)) +
  geom_point() +  # Add points
  geom_smooth(method = "lm", se = FALSE, color = "red")

ggsave("overall_inflation_scatterplot.png", plot = overall_inflation_scatterplot, width = 12, height = 8, dpi = 300)
ggsave("egg_inflation_scatterplot.png", plot = egg_inflation_scatterplot, width = 12, height = 8, dpi = 300)

