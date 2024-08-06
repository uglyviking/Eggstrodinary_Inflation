# Your existing code with modifications
library(tidyr)
library(dplyr)
library(lubridate)
library(ggplot2)
library(plotly)
library(tseries)

#import data
inflation_data <- read.csv("Data/City_Inflation_Differences.csv")

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

inflation_long <- inflation_long |>
  select(Year, Overall_Inflation, Egg_Inflation_Dif, Date)

#remove NA values
inflation_long <- inflation_long |>
  na.omit()


# Create separate time series for overall inflation and egg inflation difference
overall_inflation_ts <- ts(inflation_long$Overall_Inflation, 
                           start = c(min(inflation_long$Year), 1), 
                           end = c(max(inflation_long$Year), 12), 
                           frequency = 12)

egg_inflation_diff_ts <- ts(inflation_long$Egg_Inflation_Dif, 
                            start = c(min(inflation_long$Year), 1), 
                            end = c(max(inflation_long$Year), 12), 
                            frequency = 12)

# Augmented Dickey-Fuller test
adf.test(inflation_long$Overall_Inflation)
#data appears stationary

# Plot both time series using ggplot2
time_series_plot_25 <- ggplot(inflation_long, aes(x = Date)) +
  geom_line(aes(y = Overall_Inflation, color = "Overall Inflation")) +
  geom_line(aes(y = Egg_Inflation_Dif, color = "Egg Inflation")) +
  labs(title = "Comparison of Overall Inflation and Egg Inflation (Last 24 Years)", 
       y = "Percentage Change", 
       color = "Measure") +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  scale_color_manual(values = c("Overall Inflation" = "#EB4B33", "Egg Inflation" = "#EBC531")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#save png
ggsave("inflation_timeseries_25.png", plot = time_series_plot_25, width = 12, height = 8, dpi = 300)

# Convert in plotlty and save as an HTML file
#time_series_plot_interactive <- ggplotly(time_series_plot)
#htmlwidgets::saveWidget(time_series_plot_interactive, "inflation_comparison_interactive.html")

#repeat process for 4 years
inflation_data_filtered <- inflation_data |>
  filter(Year > (max_year - 5))

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

inflation_long <- inflation_long |>
  select(Year, Overall_Inflation, Egg_Inflation_Dif, Date)

#remove NA values
inflation_long <- inflation_long |>
  na.omit()


# Create separate time series for overall inflation and egg inflation difference
overall_inflation_ts <- ts(inflation_long$Overall_Inflation, 
                           start = c(min(inflation_long$Year), 1), 
                           end = c(max(inflation_long$Year), 12), 
                           frequency = 12)

egg_inflation_diff_ts <- ts(inflation_long$Egg_Inflation_Dif, 
                            start = c(min(inflation_long$Year), 1), 
                            end = c(max(inflation_long$Year), 12), 
                            frequency = 12)

# Ensure the Date column is properly formatted as Date type
inflation_long$Date <- as.Date(inflation_long$Date)

time_series_plot_5 <- ggplot(inflation_long, aes(x = Date)) +
  geom_line(aes(y = Overall_Inflation, color = "Overall Inflation")) +
  geom_line(aes(y = Egg_Inflation_Dif, color = "Egg Inflation")) +
  labs(title = "Comparison of Overall Inflation and Egg Inflation (Last 5 Years)", 
       y = "Percentage Change", 
       color = "Measure") +
  scale_x_date(
    date_breaks = "3 months",
    date_labels = "%b\n%Y",
    expand = c(0.02, 0)
  ) +
  scale_color_manual(values = c("Overall Inflation" = "#EB4B33", "Egg Inflation" = "#EBC531"))

ggsave("inflation_timeseries_5.png", plot = time_series_plot_5, width = 12, height = 8, dpi = 300)

