## Results of the Shapiro-Wilk normality tests

### First option with unaltered data set
> print(shapiro_test_original)

	Shapiro-Wilk normality test

data:  inflation_long$Overall_Inflation
W = 0.96145, p-value = 6.232e-07

### Second option with outliers removed
> print(shapiro_test_no_outliers)

	Shapiro-Wilk normality test

data:  inflation_long_no_outliers$Overall_Inflation_no_outliers
W = 0.98683, p-value = 0.01393

---

With both p-values below 0.05 it seems the data isn't necessarily normal but please take a look at the histograms and qq-plots. 