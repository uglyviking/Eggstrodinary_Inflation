# Your existing code with modifications
library(tidyr)
library(dplyr)
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


correlation_pearson <- cor.test(inflation_long$Overall_Inflation, inflation_long$Egg_Inflation, method = "pearson")
correlation_spearman <- cor.test(inflation_long$Overall_Inflation, inflation_long$Egg_Inflation, method = "spearman")


print(correlation_pearson)
print(correlation_spearman)


#Repeat for 10 year interval
inflation_data_filtered <- inflation_data |>
  filter(Year > (max_year - 10))

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


correlation_pearson <- cor.test(inflation_long$Overall_Inflation, inflation_long$Egg_Inflation, method = "pearson")
correlation_spearman <- cor.test(inflation_long$Overall_Inflation, inflation_long$Egg_Inflation, method = "spearman")


print(correlation_pearson)
print(correlation_spearman)

#Repeat for 5 year interval 
inflation_data_filtered <- inflation_data |>
  filter(Year > (max_year - 5))

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


correlation_pearson <- cor.test(inflation_long$Overall_Inflation, inflation_long$Egg_Inflation, method = "pearson")
correlation_spearman <- cor.test(inflation_long$Overall_Inflation, inflation_long$Egg_Inflation, method = "spearman")


print(correlation_pearson)
print(correlation_spearman)




