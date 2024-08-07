Normality test
================

## Overall Inflation Plots

![](Normality-Tests-and-Graphs_files/figure-gfm/plots-1.png)<!-- -->![](Normality-Tests-and-Graphs_files/figure-gfm/plots-2.png)<!-- -->![](Normality-Tests-and-Graphs_files/figure-gfm/plots-3.png)<!-- -->![](Normality-Tests-and-Graphs_files/figure-gfm/plots-4.png)<!-- -->

## Egg Inflation Plots

![](Normality-Tests-and-Graphs_files/figure-gfm/egg%20inflation%20plots-1.png)<!-- -->![](Normality-Tests-and-Graphs_files/figure-gfm/egg%20inflation%20plots-2.png)<!-- -->![](Normality-Tests-and-Graphs_files/figure-gfm/egg%20inflation%20plots-3.png)<!-- -->![](Normality-Tests-and-Graphs_files/figure-gfm/egg%20inflation%20plots-4.png)<!-- -->

## Shapiro-Wilk test for normality

### Original Overall

    ## 
    ##  Shapiro-Wilk normality test
    ## 
    ## data:  inflation_long$Overall_Inflation
    ## W = 0.92195, p-value = 2.869e-11

### Original No Outliers

    ## 
    ##  Shapiro-Wilk normality test
    ## 
    ## data:  inflation_long_no_outliers$Overall_Inflation_no_outliers
    ## W = 0.98797, p-value = 0.02248

### Original Egg Inflation

    ## 
    ##  Shapiro-Wilk normality test
    ## 
    ## data:  inflation_long$Egg_Inflation
    ## W = 0.96006, p-value = 3.193e-07

### Egg Inflation No Outliers

    ## 
    ##  Shapiro-Wilk normality test
    ## 
    ## data:  inflation_long__egg_no_outliers$Egg_Inflation_no_outliers
    ## W = 0.98663, p-value = 0.011
