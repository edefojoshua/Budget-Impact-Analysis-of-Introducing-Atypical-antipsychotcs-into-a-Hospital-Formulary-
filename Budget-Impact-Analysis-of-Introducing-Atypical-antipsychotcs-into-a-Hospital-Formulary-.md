Budget Impact Analysis of introducing Atypical antipsychotics
================
Joshua Edefo
2024-08-26

Libraries

``` r
library(ggplot2)
```

    ## Warning: package 'ggplot2' was built under R version 4.3.2

``` r
library(usethis)
```

    ## Warning: package 'usethis' was built under R version 4.3.2

Parameters: Population Size: Mean = 2000, SD = 500 Typical
Antipsychotics (old drug) Cost: Mean = NGN 1000, SD = NGN 250 Atypical
Antipsychotics (new drug) Cost: Mean = NGN 6000, SD = NGN 1500 Currency:
NGN (Nigerian Naira) Time_horizon = 5 years (2016 -2020) Yearly market
share growth = 5%

``` r
#Set the number of simulations

n_simulations <- 10000

# Set the specific years for the time horizon
years <- c(2016, 2017, 2018, 2019, 2020)

# Define input distributions
set.seed(123)  # For reproducibility

# Yearly market share growth
market_share_growth_rate <- 1.05  # 5% growth in market share each year (you can adjust this)

# Initialize lists to store yearly budget impacts
budget_impact_per_year <- list()

for (year_index in 1:length(years)) {
  # Simulate population size for each year
  population_size <- rnorm(n_simulations, mean = 2000, sd = 500)  # Normal distribution for patient #population
  
  # Simulate market share for Drug B (increases yearly)
  market_share_B <- rbeta(n_simulations, shape1 = 3, shape2 = 7) * (market_share_growth_rate ^ (year_index - 1))
  
  # Simulate costs for Drug A (typical antipsychotics) and Drug B (atypical antipsychotics)
  cost_A <- rnorm(n_simulations, mean = 1000, sd = 250)
  cost_B <- rnorm(n_simulations, mean = 6000, sd = 1500)
  # Calculate yearly costs
  cost_old <- population_size * cost_A  # Old scenario (only Drug A)
  cost_new <- population_size * ((1 - market_share_B) * cost_A + market_share_B * cost_B)  # New scenario (Drug A and B)
  
  # Calculate yearly budget impact
  budget_impact <- cost_new - cost_old
  
  # Store the results for each year
  budget_impact_per_year[[year_index]] <- budget_impact
}
```

Budget Impact assessment and plot of the dustribution

``` r
# Summing the budget impact over the specified years
total_budget_impact <- Reduce("+", budget_impact_per_year)

# Plot the distribution of total budget impact over the 5 years
ggplot(data.frame(total_budget_impact), aes(x = total_budget_impact)) +
  geom_histogram(binwidth = 1000000, fill = "skyblue", color = "black") +
  labs(title = "Monte Carlo Simulation of Budget Impact (2016-2020)",
       x = "Total Budget Impact (NGN)",
       y = "Frequency") +
  theme_minimal()
```

![](Budget-Impact-Analysis-of-Introducing-Atypical-antipsychotcs-into-a-Hospital-Formulary-_files/figure-gfm/c-1.png)<!-- -->

The impact estimates

``` r
# Calculate and print overall key statistics for the 5-year period
mean_impact <- mean(total_budget_impact)
ci_lower <- quantile(total_budget_impact, 0.025)
ci_upper <- quantile(total_budget_impact, 0.975)

cat(sprintf("Overall Mean Budget Impact (2016-2020): NGN %.2f\n", mean_impact))
```

    ## Overall Mean Budget Impact (2016-2020): NGN 16625874.15

``` r
cat(sprintf("95%% Confidence Interval: NGN %.2f to NGN %.2f\n", ci_lower, ci_upper))
```

    ## 95% Confidence Interval: NGN 8689708.64 to NGN 27322687.11

``` r
# Print key statistics for each year
for (year_index in 1:length(years)) {
  yearly_mean <- mean(budget_impact_per_year[[year_index]])
  yearly_sd <- sd(budget_impact_per_year[[year_index]])
  yearly_ci_lower <- quantile(budget_impact_per_year[[year_index]], 0.025)
  yearly_ci_upper <- quantile(budget_impact_per_year[[year_index]], 0.975)
  
  cat(sprintf("\nYear %d Budget Impact:\n", years[year_index]))
  cat(sprintf("  Mean: NGN %.2f\n", yearly_mean))
  cat(sprintf("  Standard Deviation: NGN %.2f\n", yearly_sd))
  cat(sprintf("  95%% Confidence Interval: NGN %.2f to NGN %.2f\n", yearly_ci_lower, yearly_ci_upper))
}
```

    ## 
    ## Year 2016 Budget Impact:
    ##   Mean: NGN 2969617.09
    ##   Standard Deviation: NGN 1899980.82
    ##   95% Confidence Interval: NGN 485796.15 to NGN 7679877.13
    ## 
    ## Year 2017 Budget Impact:
    ##   Mean: NGN 3184747.76
    ##   Standard Deviation: NGN 2036246.57
    ##   95% Confidence Interval: NGN 524225.41 to NGN 8268902.63
    ## 
    ## Year 2018 Budget Impact:
    ##   Mean: NGN 3293329.26
    ##   Standard Deviation: NGN 2088343.26
    ##   95% Confidence Interval: NGN 563351.01 to NGN 8536437.66
    ## 
    ## Year 2019 Budget Impact:
    ##   Mean: NGN 3482219.34
    ##   Standard Deviation: NGN 2272126.42
    ##   95% Confidence Interval: NGN 550351.03 to NGN 9255974.26
    ## 
    ## Year 2020 Budget Impact:
    ##   Mean: NGN 3695960.70
    ##   Standard Deviation: NGN 2383510.29
    ##   95% Confidence Interval: NGN 587206.46 to NGN 9590774.81

Results: Year 2016 Budget Impact: Mean: NGN 2969617.09; SD: NGN
1899980.82 Year 2017 Budget Impact: Mean: NGN 3184747.76; SD: NGN
2036246.57 Year 2018 Budget Impact: Mean: NGN 3293329.26, SD: NGN
2088343.26 Year 2019 Budget Impact: Mean: NGN 3482219.34; SD: NGN
2272126.42 Year 2020 Budget Impact: Mean: NGN 3695960.70; SD: NGN
2383510.29

session information

``` r
sessionInfo()
```

    ## R version 4.3.1 (2023-06-16 ucrt)
    ## Platform: x86_64-w64-mingw32/x64 (64-bit)
    ## Running under: Windows 11 x64 (build 22631)
    ## 
    ## Matrix products: default
    ## 
    ## 
    ## locale:
    ## [1] LC_COLLATE=English_United Kingdom.utf8 
    ## [2] LC_CTYPE=English_United Kingdom.utf8   
    ## [3] LC_MONETARY=English_United Kingdom.utf8
    ## [4] LC_NUMERIC=C                           
    ## [5] LC_TIME=English_United Kingdom.utf8    
    ## 
    ## time zone: Europe/London
    ## tzcode source: internal
    ## 
    ## attached base packages:
    ## [1] stats     graphics  grDevices utils     datasets  methods   base     
    ## 
    ## other attached packages:
    ## [1] usethis_2.2.2 ggplot2_3.4.4
    ## 
    ## loaded via a namespace (and not attached):
    ##  [1] vctrs_0.6.3       cli_3.6.1         knitr_1.44        rlang_1.1.1      
    ##  [5] xfun_0.40         purrr_1.0.2       generics_0.1.3    labeling_0.4.3   
    ##  [9] glue_1.6.2        colorspace_2.1-0  htmltools_0.5.6   scales_1.3.0     
    ## [13] fansi_1.0.4       rmarkdown_2.25    grid_4.3.1        evaluate_0.21    
    ## [17] munsell_0.5.0     tibble_3.2.1      fastmap_1.1.1     yaml_2.3.7       
    ## [21] lifecycle_1.0.3   compiler_4.3.1    dplyr_1.1.3       fs_1.6.3         
    ## [25] pkgconfig_2.0.3   rstudioapi_0.15.0 farver_2.1.1      digest_0.6.33    
    ## [29] R6_2.5.1          tidyselect_1.2.0  utf8_1.2.3        pillar_1.9.0     
    ## [33] magrittr_2.0.3    withr_2.5.0       tools_4.3.1       gtable_0.3.4
