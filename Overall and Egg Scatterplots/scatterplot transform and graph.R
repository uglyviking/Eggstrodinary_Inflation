# Your existing code with modifications
library(tidyr)
library(dplyr)
library(lubridate)
library(ggplot2)


#import data
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

#remove NA values
inflation_long <- inflation_long |>
  na.omit()


overall_inflation_scatterplot <- ggplot(inflation_long, aes(x = Date, y = Overall_Inflation)) +
  geom_point(color = "#EB4B33") + 
  geom_smooth(method = "lm", se = FALSE, color = "black")+
  labs(title = "Overall Inflation Over Time",
       x = "Date",
       y = "Inflation")+
  scale_x_date(date_breaks = "1 year", date_labels = "%Y")
  

egg_inflation_scatterplot <- ggplot(inflation_long, aes(x = Date, y = Egg_Inflation)) +
  geom_point(color = "#EBC531") + 
  geom_smooth(method = "lm", se = FALSE, color = "black")+
  labs(title = "Egg Inflation Over Time",
       x = "Date",
       y = "Inflation")+
  scale_x_date(date_breaks = "1 year", date_labels = "%Y")


overall_inflation_scatterplot
egg_inflation_scatterplot

ggsave("overall_inflation_scatterplot.png", plot = overall_inflation_scatterplot, width = 12, height = 8, dpi = 300)
ggsave("egg_inflation_scatterplot.png", plot = egg_inflation_scatterplot, width = 12, height = 8, dpi = 300)

