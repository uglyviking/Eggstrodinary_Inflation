# Load required libraries
library(tidyverse)
library(lubridate)
library(car)  # for Levene's test
library(agricolae)  # for Tukey's HSD
library(trend)  # for Mann-Kendall test


# Read the CSV file
data <- read.csv("Data/Inflation_Differences.csv", stringsAsFactors = FALSE)

# Reshape the data
long_data <- data %>%
  pivot_longer(
    cols = starts_with("Jan_Egg"):starts_with("Dec_Egg"),
    names_to = "Month",
    values_to = "Egg_Inflation_Dif"
  ) %>%
  mutate(
    Month = str_remove(Month, "_Egg_Inflation_Dif"),
    Date = ymd(paste(Year, Month, "01")),
    Region = case_when(
      MW == 1 ~ "Midwest",
      S == 1 ~ "South",
      NE == 1 ~ "Northeast",
      W == 1 ~ "West"
    )
  ) %>%
  select(Date, Region, Egg_Inflation_Dif) %>%
  filter(!is.na(Egg_Inflation_Dif))

# 1. Descriptive Statistics
desc_stats <- long_data %>%
  group_by(Region) %>%
  summarise(
    Mean = mean(Egg_Inflation_Dif, na.rm = TRUE),
    Median = median(Egg_Inflation_Dif, na.rm = TRUE),
    SD = sd(Egg_Inflation_Dif, na.rm = TRUE),
    Min = min(Egg_Inflation_Dif, na.rm = TRUE),
    Max = max(Egg_Inflation_Dif, na.rm = TRUE)
  )
print(desc_stats)

# 2. ANOVA
anova_model <- aov(Egg_Inflation_Dif ~ Region, data = long_data)
summary(anova_model)

# 3. Kruskal-Wallis Test
kruskal.test(Egg_Inflation_Dif ~ Region, data = long_data)

# 4. Tukey's HSD
tukey_results <- TukeyHSD(anova_model)
print(tukey_results)

# 5. Pairwise t-tests
pairwise.t.test(long_data$Egg_Inflation_Dif, long_data$Region, p.adjust.method = "bonferroni")

# Mann-Kendall Trend Test for each region
mk_results <- long_data %>%
  group_by(Region) %>%
  summarise(
    MK_test = list(mk.test(Egg_Inflation_Dif, continuity = TRUE))
  ) %>%
  mutate(
    MK_statistic = map_dbl(MK_test, ~ .$statistic),
    MK_p_value = map_dbl(MK_test, ~ .$p.value)
  ) %>%
  select(-MK_test)

print(mk_results)

# 7. Test for homogeneity of variances (Levene's Test)
leveneTest(Egg_Inflation_Dif ~ Region, data = long_data)

# Visualization of distributions
ggplot(long_data, aes(x = Region, y = Egg_Inflation_Dif, fill = Region)) +
  geom_boxplot() +
  labs(title = "Distribution of Egg Inflation by Region (Last 25 Years)",
       y = "Inflation")

violinplot25 <- ggplot(long_data, aes(x = Region, y = Egg_Inflation_Dif, fill = Region)) +
  geom_violin(trim = FALSE) +
  geom_boxplot(width = 0.1, fill = "white", color = "black") +  # Add a mini boxplot inside
  labs(title = "Violin Plot of Egg Inflation by Region (Last 25 Years)",
       y = "Inflation")


ggsave("egg_inflation_boxplot_25.png", width = 10, height = 6, dpi = 300)
ggsave("egg_inflation_violinplot_25.png", plot = violinplot25, width = 10, height = 6, dpi = 300)
