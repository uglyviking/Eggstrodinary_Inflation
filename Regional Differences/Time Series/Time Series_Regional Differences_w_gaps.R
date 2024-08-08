# Load required libraries
library(tidyverse)
library(lubridate)
library(plotly)

# Read the CSV file
region_data <- read.csv("Data/Inflation_Differences.csv", stringsAsFactors = FALSE)


long_data <- region_data |>
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
    Date = ymd(paste(Year, Month, "01")),
    Region = case_when(
      MW == 1 ~ "Midwest",
      S == 1 ~ "South",
      NE == 1 ~ "Northeast",
      W == 1 ~ "West"
    )
  ) |>
  pivot_wider(
    names_from = Type,
    values_from = Inflation
  ) |>
  select(Region, Year, Month, Date, Egg_Inflation, Overall_Inflation)

long_data <- long_data |>
  filter(Date < "2024-07-01")

region_ts_10 <-  ggplot(long_data, aes(x = Date, y = Egg_Inflation, color = Region, group = Region)) +
  geom_line(na.rm = TRUE) +
  labs(title = "Egg Inflation by Region (Last 10 Years)",
       x = "Date",
       y = "Inflation",
       color = "Region") +
  theme(legend.position = "bottom") +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y")

# Save the plot
ggsave("egg_inflation_difference_by_region_gaps_10years.png", plot = region_ts_10, width = 12, height = 8, dpi = 300)

# Repeat for 5 years
max_year <- max(region_data$Year)
inflation_data_filtered <- region_data |>
  filter(Year > (max_year - 5))

long_data <- inflation_data_filtered |>
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
    Date = ymd(paste(Year, Month, "01")),
    Region = case_when(
      MW == 1 ~ "Midwest",
      S == 1 ~ "South",
      NE == 1 ~ "Northeast",
      W == 1 ~ "West"
    )
  ) |>
  pivot_wider(
    names_from = Type,
    values_from = Inflation
  ) |>
  select(Region, Year, Month, Date, Egg_Inflation, Overall_Inflation)

long_data <- long_data |>
  filter(Date < "2024-07-01")

time_series_region_5 <- ggplot(long_data, aes(x = Date, y = Egg_Inflation, color = Region)) +
  geom_line() +
  labs(title = "Egg Inflation by Region (Last 5 Years)",
       x = "Date",
       y = "Inflation",
       color = "Region") +
  theme(legend.position = "bottom")+
  scale_x_date(
    date_breaks = "3 months",
    date_labels = "%b\n%Y",
    expand = c(0.02, 0))

# Save the plot
ggsave("egg_inflation_difference_by_region_5years.png", plot = time_series_region_5, width = 12, height = 8, dpi = 300)



