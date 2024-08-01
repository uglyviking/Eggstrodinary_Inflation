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


remove_outliers <- function(x, na.rm = TRUE, ...) {
  qnt <- quantile(x, probs=c(.25, .75), na.rm = na.rm, ...)
  H <- 1.5 * IQR(x, na.rm = na.rm)
  y <- x
  y[x < (qnt[1] - H)] <- NA
  y[x > (qnt[2] + H)] <- NA
  y
}

# Remove outliers from Overall_Inflation
inflation_long_no_outliers <- inflation_long %>%
  mutate(Overall_Inflation_no_outliers = remove_outliers(Overall_Inflation)) %>%
  filter(!is.na(Overall_Inflation_no_outliers))

# Create Q-Q plots
qqplot_original <- ggplot(inflation_long, aes(sample = Overall_Inflation)) +
  stat_qq() + stat_qq_line() + theme_minimal() +
  labs(title = "Q-Q Plot of Original Overall Inflation")

qqplot_no_outliers <- ggplot(inflation_long_no_outliers, aes(sample = Overall_Inflation_no_outliers)) +
  stat_qq() + stat_qq_line() + theme_minimal() +
  labs(title = "Q-Q Plot of Overall Inflation (Outliers Removed)")

# Create histograms
histogram_original <- ggplot(inflation_long, aes(x = Overall_Inflation)) +
  geom_histogram(binwidth = 1, fill = "skyblue", color = "black") +
  geom_vline(aes(xintercept = mean(Overall_Inflation)), color = "red", linetype = "dashed", size = 1) +
  theme_minimal() +
  labs(title = "Histogram of Original Overall Inflation")

histogram_no_outliers <- ggplot(inflation_long_no_outliers, aes(x = Overall_Inflation_no_outliers)) +
  geom_histogram(binwidth = 1, fill = "skyblue", color = "black") +
  geom_vline(aes(xintercept = mean(Overall_Inflation_no_outliers)), color = "red", linetype = "dashed", size = 1) +
  theme_minimal() +
  labs(title = "Histogram of Overall Inflation (Outliers Removed)")

# Display plots
print(qqplot_original)
print(qqplot_no_outliers)
print(histogram_original)
print(histogram_no_outliers)

# Shapiro-Wilk test for normality
shapiro_test_original <- shapiro.test(inflation_long$Overall_Inflation)
shapiro_test_no_outliers <- shapiro.test(inflation_long_no_outliers$Overall_Inflation_no_outliers)

print(shapiro_test_original)
print(shapiro_test_no_outliers)
