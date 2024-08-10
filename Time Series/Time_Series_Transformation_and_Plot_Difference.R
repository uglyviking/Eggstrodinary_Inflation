library(tidyr)
library(dplyr)
library(lubridate)
library(ggplot2)
library(tseries)

#repeat process for 4 years
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

# Extract egg inflation series
egg_inflation <- ts(inflation_long$Egg_Inflation, frequency = 12)

# Apply first-order differencing
egg_inflation_diff <- diff(egg_inflation, differences = 1)

# Test stationarity of the differenced series
adf_result <- adf.test(egg_inflation_diff)
print(adf_result)

# Add differenced series to the dataframe
inflation_long$Egg_Inflation_Diff <- c(NA, as.vector(egg_inflation_diff))

# If still non-stationary, apply log transformation and then difference
if (adf_result$p.value > 0.05) {
  egg_inflation_log <- log(egg_inflation + 1)  # Adding 1 to handle possible zero values
  egg_inflation_log_diff <- diff(egg_inflation_log, differences = 1)
  adf_result_log <- adf.test(egg_inflation_log_diff)
  print(adf_result_log)
  
  # Add log-differenced series to the dataframe
  inflation_long$Egg_Inflation_Log_Diff <- c(NA, as.vector(egg_inflation_log_diff))
}

# Plot original, differenced, and (if needed) log-differenced series
par(mfrow = c(3, 1))
plot(egg_inflation, main = "Original Egg Inflation")
plot(egg_inflation_diff, main = "Differenced Egg Inflation")
if (exists("egg_inflation_log_diff")) {
  plot(egg_inflation_log_diff, main = "Log-Differenced Egg Inflation")
}

# Create the ggplot
p <- ggplot(inflation_long, aes(x = Date)) +
  geom_line(aes(y = Overall_Inflation, color = "Overall Inflation")) +
  geom_line(aes(y = Egg_Inflation, color = "Original Egg Inflation")) +
  geom_line(aes(y = Egg_Inflation_Diff, color = "Differenced Egg Inflation"))

# Add log-differenced series if it exists
if ("Egg_Inflation_Log_Diff" %in% colnames(inflation_long)) {
  p <- p + geom_line(aes(y = Egg_Inflation_Log_Diff, color = "Log-Differenced Egg Inflation"))
}

# Finalize the plot
p <- p + 
  labs(title = "Comparison of Series (Last 5 Years) With Differenced Egg Inflation", 
       y = "Value", 
       color = "Measure") +
  scale_x_date(date_breaks = "3 months", date_labels = "%b\n%Y", expand = c(0.02, 0)) +
  scale_color_manual(values = c("Overall Inflation" = "#EB4B33", 
                                "Original Egg Inflation" = "#EBC531",
                                "Differenced Egg Inflation" = "#31B6EB",
                                "Log-Differenced Egg Inflation" = "#31EB98")) +
  theme(legend.position = "bottom")

# Display the plot
print(p)

# Save the plot
ggsave("inflation_timeseries_5_difference.png", plot = p, width = 8, height = 5, dpi = 300)

# Print summary statistics
summary(inflation_long[, c("Overall_Inflation", "Egg_Inflation", "Egg_Inflation_Diff")])
if ("Egg_Inflation_Log_Diff" %in% colnames(inflation_long)) {
  summary(inflation_long$Egg_Inflation_Log_Diff)
}

Version 2 of 2




