# Load required libraries
library(tidyverse)
library(lubridate)
library(plotly)

# Read the CSV file
data <- read.csv("Data/Inflation_Differences.csv", stringsAsFactors = FALSE)

max_year <- max(data$Year)
inflation_data_filtered <- data |>
  filter(Year > (max_year - 4))

# Reshape the data
long_data <- inflation_data_filtered |>
  pivot_longer(
    cols = starts_with("Jan_Egg"):starts_with("Dec_Egg"),
    names_to = "Month",
    values_to = "Egg_Inflation_Dif"
  ) %>%
  mutate(
    Month = str_remove(Month, "_Egg_Inflation_Dif"),
    Date = ymd(paste(Year, Month, "01")),
    Region = case_when(
      MW == 1 ~ "Midwest",
      S == 1 ~ "South",
      NE == 1 ~ "Northeast",
      W == 1 ~ "West"
    )
  ) %>%
  select(Date, Region, Egg_Inflation_Dif) %>%
  filter(!is.na(Egg_Inflation_Dif))

# Create the time series plot
time_series_plot <- ggplot(long_data, aes(x = Date, y = Egg_Inflation_Dif, color = Region)) +
  geom_line() +
  labs(title = "Egg Inflation Difference by Region",
       x = "Date",
       y = "Egg Inflation Difference",
       color = "Region") +
  theme(legend.position = "bottom")

# Create the time series plot with broken lines for NA values
  ggplot(long_data, aes(x = Date, y = Egg_Inflation_Dif, color = Region, group = Region)) +
  geom_line(na.rm = TRUE) +
  labs(title = "Egg Inflation Difference by Region",
       x = "Date",
       y = "Egg Inflation Difference",
       color = "Region") +
  theme(legend.position = "bottom")



# Save the plot
ggsave("egg_inflation_difference_by_region_4years.png", plot = time_series_plot, width = 12, height = 8, dpi = 300)
ggsave("egg_inflation_difference_by_region_4years.png", plot = time_series_gap, width = 12, height = 8, dpi = 300)


time_series_plot_interactive <- ggplotly(time_series_plot)
htmlwidgets::saveWidget(time_series_plot_interactive, "inflation_comparison_interactive.html")
