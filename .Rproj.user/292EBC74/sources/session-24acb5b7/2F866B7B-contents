# Load required libraries
library(tidyverse)
library(lubridate)
library(plotly)

# Read the CSV file
data <- read.csv("Data/Inflation_Differences.csv", stringsAsFactors = FALSE)

max_year <- max(data$Year)
inflation_data_filtered <- data |>
  filter(Year > (max_year - 5))

long_data <- inflation_data_filtered |>
  pivot_longer(
    cols = starts_with("Jan_Egg"):starts_with("Dec_Egg"),
    names_to = "Month",
    values_to = "Egg_Inflation_Dif"
  ) |>
  mutate(
    Month = str_remove(Month, "_Egg_Inflation_Dif"),
    Date = ymd(paste(Year, Month, "01")),
    Region = case_when(
      MW == 1 ~ "Midwest",
      S == 1 ~ "South",
      NE == 1 ~ "Northeast",
      W == 1 ~ "West"
    )
  ) |>
  select(Date, Region, Egg_Inflation_Dif)

region_ts_5 <- ggplot(long_data, aes(x = Date, y = Egg_Inflation_Dif, color = Region, group = Region)) +
  geom_line(na.rm = TRUE) +
  labs(title = "Egg Inflation by Region (Last 5 Years)",
       x = "Date",
       y = "Inflation",
       color = "Region") +
  theme(legend.position = "bottom")+
  scale_x_date(date_breaks = "1 year", date_labels = "%Y")

# Save the plot
ggsave("egg_inflation_difference_by_region_gaps_5.png", plot = region_ts_5, width = 12, height = 8, dpi = 300)





