# Your existing code with modifications
library(tidyr)
library(dplyr)
library(lubridate)
library(ggplot2)
library(plotly)
library(ggimage)

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

# Path to your SVG file
egg_path <- "Assets/egg_solid_yellow.svg"

# Update your ggplot code
egg_scatterplot <- ggplot(inflation_long, aes(x = Date, y = Egg_Inflation_Dif)) +
  geom_image(aes(image = egg_path), size = 0.01, dpi = 300, na.rm = TRUE) +
  geom_smooth(method = "lm", se = FALSE, color = "black", na.rm = TRUE) +
  labs(title = "Egg Inflation Difference Over Time",
       x = "Date",
       y = "Egg Inflation Difference") +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave("scatterplot_egg_inflation.png", plot = egg_scatterplot, width = 12, height = 8, dpi = 300)
