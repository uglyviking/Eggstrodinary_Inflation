# Load required libraries
library(tidyverse)
library(lubridate)
library(car) 
library(agricolae)
library(trend)
library(FSA)
library(ggplot2)
library(gt)
library(webshot2)


# Read the CSV file
region_data <- read.csv("Data/Inflation_Differences.csv", stringsAsFactors = FALSE)

# Reshape the data
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

# Descriptive Statistics
desc_stats <- long_data |>
  group_by(Region) |>
  summarise(
    Mean = mean(Egg_Inflation, na.rm = TRUE),
    Median = median(Egg_Inflation, na.rm = TRUE),
    SD = sd(Egg_Inflation, na.rm = TRUE),
    Min = min(Egg_Inflation, na.rm = TRUE),
    Max = max(Egg_Inflation, na.rm = TRUE)
  )
print(desc_stats)



#Test Normality
shapiro.test(long_data$Egg_Inflation)

# Kruskal-Wallis Test
kruskal.test(Egg_Inflation ~ Region, data = long_data)

#Test for homogeneity of variances (Levene's Test)
leveneTest(Egg_Inflation ~ Region, data = long_data)

#Dunn Test
dunnTest(Egg_Inflation ~ Region, data = long_data, method = "holm")

# Visualization of distributions
boxplot_regions_10 <- ggplot(long_data, aes(x = Region, y = Egg_Inflation, fill = Region)) +
  geom_boxplot() +
  labs(title = "Distribution of Egg Inflation by Region (Last 10 Years)",
       y = "Inflation")

violinplot_10 <- ggplot(long_data, aes(x = Region, y = Egg_Inflation, fill = Region)) +
  geom_violin(trim = FALSE) +
  geom_boxplot(width = 0.1, fill = "white", color = "black") +  # Add a mini boxplot inside
  labs(title = "Violin Plot of Egg Inflation by Region (Last 10 Years)",
       y = "Inflation")


ggsave("egg_inflation_boxplot_10.png", plot = boxplot_regions_10, width = 8, height = 5, dpi = 300)
ggsave("egg_inflation_violinplot_10.png", plot = violinplot_10, width = 8, height = 5, dpi = 300)

# Create the gt table
regional_egg_inflation_table <- desc_stats %>%
  gt() %>%
  fmt_number(columns = c(Mean, Median, SD, Min, Max), decimals = 2) %>%
  tab_header(
    title = "Descriptive Statistics for Regional Egg Inflation",
    subtitle = "Regional Data for 2014-2024"
  ) %>%
  opt_row_striping() %>%
  opt_table_font(font = "Arial") %>%
  cols_label(
    Region = "Region",
    Mean = "Mean",
    Median = "Median",
    SD = "Std. Deviation",
    Min = "Minimum",
    Max = "Maximum"
  )

# Display the table
print(regional_egg_inflation_table)
# Save the table as an HTML file
gtsave(regional_egg_inflation_table, filename = "regional_egg_inflation_table.html")
# Use webshot2 to convert the HTML to a PNG image with 300 DPI
webshot("regional_egg_inflation_table.html", file = "regional_egg_inflation_table.png", zoom = 2, vwidth = 1000, vheight = 800)

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

# Descriptive Statistics
desc_stats <- long_data |>
  group_by(Region) |>
  summarise(
    Mean = mean(Egg_Inflation, na.rm = TRUE),
    Median = median(Egg_Inflation, na.rm = TRUE),
    SD = sd(Egg_Inflation, na.rm = TRUE),
    Min = min(Egg_Inflation, na.rm = TRUE),
    Max = max(Egg_Inflation, na.rm = TRUE)
  )
print(desc_stats)

#Test Normality
shapiro.test(long_data$Egg_Inflation)

# Kruskal-Wallis Test
kruskal.test(Egg_Inflation ~ Region, data = long_data)

#Test for homogeneity of variances (Levene's Test)
leveneTest(Egg_Inflation ~ Region, data = long_data)

#Dunn Test
dunnTest(Egg_Inflation ~ Region, data = long_data, method = "holm")

# Visualization of distributions
boxplot_regions_5 <- ggplot(long_data, aes(x = Region, y = Egg_Inflation, fill = Region)) +
  geom_boxplot() +
  labs(title = "Distribution of Egg Inflation by Region (Last 5 Years)",
       y = "Inflation")

violinplot_5 <- ggplot(long_data, aes(x = Region, y = Egg_Inflation, fill = Region)) +
  geom_violin(trim = FALSE) +
  geom_boxplot(width = 0.1, fill = "white", color = "black") +  # Add a mini boxplot inside
  labs(title = "Violin Plot of Egg Inflation by Region (Last 5 Years)",
       y = "Inflation")


ggsave("egg_inflation_boxplot_5.png", plot = boxplot_regions_5, width = 8, height = 5, dpi = 300)
ggsave("egg_inflation_violinplot_5.png", plot = violinplot_5, width = 8, height = 5, dpi = 300)
