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


remove_outliers <- function(x, na.rm = TRUE, ...) {
  qnt <- quantile(x, probs=c(.25, .75), na.rm = na.rm, ...)
  H <- 1.5 * IQR(x, na.rm = na.rm)
  y <- x
  y[x < (qnt[1] - H)] <- NA
  y[x > (qnt[2] + H)] <- NA
  y
}


# Remove outliers from Overall_Inflation
inflation_long_no_outliers <- inflation_long |>
  mutate(Overall_Inflation_no_outliers = remove_outliers(Overall_Inflation)) |>
  filter(!is.na(Overall_Inflation_no_outliers))

# Remove outliers from Egg Inflation
inflation_long__egg_no_outliers <- inflation_long |>
  mutate(Egg_Inflation_no_outliers = remove_outliers(Egg_Inflation)) |>
  filter(!is.na(Egg_Inflation_no_outliers))

# Create Q-Q plots
qqplot_original <- ggplot(inflation_long, aes(sample = Overall_Inflation)) +
  stat_qq(color = "#EB4B33") + stat_qq_line() + 
  labs(title = "Q-Q Plot of Overall Inflation",
       x = "Theoretical Quantiles",
       y = "Sample Quantiles")

qqplot_no_outliers <- ggplot(inflation_long_no_outliers, aes(sample = Overall_Inflation_no_outliers)) +
  stat_qq(color = "#EB4B33") + stat_qq_line() + 
  labs(title = "Q-Q Plot of Overall Inflation (Outliers Removed)",
       x = "Theoretical Quantiles",
       y = "Sample Quantiles")

qqplot_egg_original <- ggplot(inflation_long, aes(sample = Egg_Inflation)) +
  stat_qq(color = "#EBC531") + stat_qq_line() + 
  labs(title = "Q-Q Plot of Egg Inflation",
       x = "Theoretical Quantiles",
       y = "Sample Quantiles")

qqplot_egg_no_outliers <- ggplot(inflation_long__egg_no_outliers, aes(sample = Egg_Inflation_no_outliers)) +
  stat_qq(color = "#EBC531") + stat_qq_line() + 
  labs(title = "Q-Q Plot of Egg Inflation (Outliers Removed)",
       x = "Theoretical Quantiles",
       y = "Sample Quantiles")


# Create histograms
histogram_original <- ggplot(inflation_long, aes(x = Overall_Inflation)) +
  geom_histogram(binwidth = 1, fill = "#EB4B33", color = "black") +
  geom_vline(aes(xintercept = mean(Overall_Inflation)), color = "black", linetype = "dashed", size = 1) +
  labs(title = "Histogram of Overall Inflation",
       x = "Inflation Amount",
       y = "Frequency")

histogram_no_outliers <- ggplot(inflation_long_no_outliers, aes(x = Overall_Inflation_no_outliers)) +
  geom_histogram(binwidth = 1, fill = "#EB4B33", color = "black") +
  geom_vline(aes(xintercept = mean(Overall_Inflation_no_outliers)), color = "black", linetype = "dashed", size = 1) +
  labs(title = "Histogram of Overall Inflation (Outliers Removed)",
       x = "Inflation Amount",
       y = "Frequency")

histogram_egg_original <- ggplot(inflation_long, aes(x = Egg_Inflation)) +
  geom_histogram(binwidth = 1, fill = "#EBC531", color = "black") +
  geom_vline(aes(xintercept = mean(Egg_Inflation)), color = "black", linetype = "dashed", size = 1) +
  labs(title = "Histogram of Egg Inflation",
       x = "Inflation Amount",
       y = "Frequency")

histogram_egg_no_outliers <- ggplot(inflation_long__egg_no_outliers, aes(x = Egg_Inflation_no_outliers)) +
  geom_histogram(binwidth = 1, fill = "#EBC531", color = "black") +
  geom_vline(aes(xintercept = mean(Egg_Inflation_no_outliers)), color = "black", linetype = "dashed", size = 1) +
  labs(title = "Histogram of Egg Inflation (Outliers Removed)",
       x = "Inflation Amount",
       y = "Frequency")

# Display plots
print(qqplot_original)
print(qqplot_no_outliers)
print(histogram_original)
print(histogram_no_outliers)

print(qqplot_egg_original)
print(qqplot_egg_no_outliers)
print(histogram_egg_original)
print(histogram_egg_no_outliers)

# Shapiro-Wilk test for normality
shapiro_test_original <- shapiro.test(inflation_long$Overall_Inflation)
shapiro_test_no_outliers <- shapiro.test(inflation_long_no_outliers$Overall_Inflation_no_outliers)
shapiro_test_egg_original <- shapiro.test(inflation_long$Egg_Inflation)
shapiro_test_egg_no_outliers <- shapiro.test(inflation_long__egg_no_outliers$Egg_Inflation_no_outliers)

print(shapiro_test_original)
print(shapiro_test_no_outliers)
print(shapiro_test_egg_original)
print(shapiro_test_egg_no_outliers)

ggsave("qqplot_inflation.png", plot = qqplot_original, width = 12, height = 8, dpi = 300)
ggsave("qqplot_egg_inflation.png", plot = qqplot_egg_original, width = 12, height = 8, dpi = 300)
ggsave("histogram_inflation.png", plot = histogram_original, width = 12, height = 8, dpi = 300)
ggsave("histogram_egg_inflation.png", plot = histogram_egg_original, width = 12, height = 8, dpi = 300)

