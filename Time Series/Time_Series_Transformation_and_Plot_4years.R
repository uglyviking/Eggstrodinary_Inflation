# Your existing code with modifications
library(tidyr)
library(dplyr)
library(lubridate)
library(ggplot2)
library(plotly)
library(tseries)

#import data
inflation_data <- read.csv("Data/City_Inflation_Differences.csv")

#remove NA values
inflation_data <- inflation_data |>
  na.omit()

# Determine the most recent year and filter if necessary
max_year <- max(inflation_data$Year)
inflation_data_filtered <- inflation_data |>
  filter(Year > (max_year - 4))

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
#cannot confirm data is stationary

# Plot both time series using ggplot2
time_series_plot <- ggplot(inflation_long, aes(x = Date)) +
  geom_line(aes(y = Overall_Inflation, color = "Overall Inflation")) +
  geom_line(aes(y = Egg_Inflation_Dif, color = "Egg Inflation Difference")) +
  labs(title = "Comparison of Overall Inflation and Egg Inflation Difference (Last 10 Years)", 
       y = "Percentage Change", 
       color = "Measure") +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


#save png
ggsave("inflation_comparison_hires_4years.png", plot = time_series_plot, width = 12, height = 8, dpi = 300)

# Convert in plotlty and save as an HTML file
#time_series_plot_interactive <- ggplotly(time_series_plot)
#htmlwidgets::saveWidget(time_series_plot_interactive, "inflation_comparison_interactive.html")


