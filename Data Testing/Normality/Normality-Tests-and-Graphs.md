Normality test
================
2024-08-01

## Overall Inflation Plots


![](Normality-Tests-and-Graphs_files/figure-gfm/plots-1.png)<!-- -->


![](Normality-Tests-and-Graphs_files/figure-gfm/plots-2.png)<!-- -->


![](Normality-Tests-and-Graphs_files/figure-gfm/plots-3.png)<!-- -->


![](Normality-Tests-and-Graphs_files/figure-gfm/plots-4.png)<!-- -->



## Egg Inflation Plots

![](Normality-Tests-and-Graphs_files/figure-gfm/plots-5.png)<!-- -->

![](Normality-Tests-and-Graphs_files/figure-gfm/plots-6.png)<!-- -->

![](Normality-Tests-and-Graphs_files/figure-gfm/plots-7.png)<!-- -->

![](Normality-Tests-and-Graphs_files/figure-gfm/plots-8.png)<!-- -->

## Shapiro-Wilk test for normality tests

### Original Overall
    ## 
    ##  Shapiro-Wilk normality test
    ## 
    ## data:  inflation_long$Overall_Inflation
    ## W = 0.96145, p-value = 6.232e-07

### Overall No Outliers

    ## 
    ##  Shapiro-Wilk normality test
    ## 
    ## data:  inflation_long_no_outliers$Overall_Inflation_no_outliers
    ## W = 0.98683, p-value = 0.01393

### Original Eggs
    ## 
    ##  Shapiro-Wilk normality test
    ## 
    ## data:  inflation_long$Egg_Inflation_Dif
    ## W = 0.96084, p-value = 5.161e-07

### Eggs No Ouliers
    ## 
    ##  Shapiro-Wilk normality test
    ## 
    ## data:  inflation_long__egg_no_outliers$Egg_Inflation_Dif_no_outliers
    ## W = 0.99058, p-value = 0.07932