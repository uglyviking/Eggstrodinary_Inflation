# Load required libraries
library(tidyverse)
library(lubridate)
library(tseries)

# Assuming 'inflation_data' is your original dataset
# Filter data for the last 5 years (adjust as needed)
inflation_data_filtered <- inflation_data %>%
  filter(Year > max(Year) - 5)

# Reshape the data
inflation_long <- inflation_data_filtered %>%
  pivot_longer(
    cols = c(ends_with("Percentage_Change"), Jan:Dec),
    names_to = "Month",
    values_to = "Inflation"
  ) %>%
  mutate(
    Type = if_else(grepl("Percentage_Change$", Month), "Egg_Inflation", "Overall_Inflation"),
    Month = if_else(Type == "Egg_Inflation", sub("_Percentage_Change$", "", Month), Month),
    Month = match(Month, month.abb),
    Date = ymd(paste(Year, Month, "01"))
  ) %>%
  pivot_wider(
    names_from = Type,
    values_from = Inflation
  ) %>%
  arrange(Date) %>%
  select(Date, Egg_Inflation, Overall_Inflation) %>%
  na.omit()

# Print summary of original data to check for negative or zero values
print("Summary of original data:")
print(summary(inflation_long))

# Function to create differenced and log-transformed series
create_transformations <- function(data, column_name) {
  data %>%
    mutate(
      !!paste0(column_name, "_Diff") := c(NA, diff(!!sym(column_name))),
      !!paste0(column_name, "_Log") := log(!!sym(column_name) + abs(min(!!sym(column_name), na.rm = TRUE)) + 1)
    )
}

# Apply transformations
inflation_transformed <- inflation_long %>%
  create_transformations("Egg_Inflation") %>%
  create_transformations("Overall_Inflation")

# Print summary of transformed data
print("Summary of transformed data:")
print(summary(inflation_transformed))

# Create the plot
p <- ggplot(inflation_transformed, aes(x = Date)) +
  geom_line(aes(y = Overall_Inflation, color = "Overall Inflation")) +
  geom_line(aes(y = Egg_Inflation, color = "Egg Inflation")) +
  geom_line(aes(y = Overall_Inflation_Diff, color = "Differenced Overall Inflation")) +
  geom_line(aes(y = Egg_Inflation_Diff, color = "Differenced Egg Inflation")) +
  geom_line(aes(y = Overall_Inflation_Log, color = "Log Overall Inflation")) +
  geom_line(aes(y = Egg_Inflation_Log, color = "Log Egg Inflation")) +
  labs(
    title = "Comparison of Inflation Series (Last 5 Years)",
    y = "Value",
    color = "Measure"
  ) +
  scale_x_date(date_breaks = "6 months", date_labels = "%b %Y", expand = c(0.02, 0)) +
  scale_color_manual(values = c(
    "Overall Inflation" = "#EB4B33",
    "Egg Inflation" = "#EBC531",
    "Differenced Overall Inflation" = "#31B6EB",
    "Differenced Egg Inflation" = "#31EB98",
    "Log Overall Inflation" = "#9B31EB",
    "Log Egg Inflation" = "#EB3193"
  )) +
  theme(
    legend.position = "bottom",
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

# Display the plot
print(p)

# Save the plot
ggsave("comprehensive_inflation_analysis.png", plot = p, width = 8, height = 5, dpi = 300)

# Print summary statistics
summary(inflation_transformed)

# Optionally, perform ADF tests on the series
adf_tests <- list(
  Egg_Inflation = adf.test(inflation_transformed$Egg_Inflation),
  Overall_Inflation = adf.test(inflation_transformed$Overall_Inflation),
  Egg_Inflation_Diff = adf.test(inflation_transformed$Egg_Inflation_Diff, na.action = na.omit),
  Overall_Inflation_Diff = adf.test(inflation_transformed$Overall_Inflation_Diff, na.action = na.omit),
  Egg_Inflation_Log = adf.test(inflation_transformed$Egg_Inflation_Log),
  Overall_Inflation_Log = adf.test(inflation_transformed$Overall_Inflation_Log)
)

# Print ADF test results
print(adf_tests)
