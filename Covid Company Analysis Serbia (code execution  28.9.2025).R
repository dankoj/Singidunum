R version 4.5.0 (2025-04-11 ucrt) -- "How About a Twenty-Six"
Copyright (C) 2025 The R Foundation for Statistical Computing
Platform: x86_64-w64-mingw32/x64

R is free software and comes with ABSOLUTELY NO WARRANTY.
You are welcome to redistribute it under certain conditions.
Type 'license()' or 'licence()' for distribution details.

R is a collaborative project with many contributors.
Type 'contributors()' for more information and
'citation()' on how to cite R or R packages in publications.

Type 'demo()' for some demos, 'help()' for on-line help, or
'help.start()' for an HTML browser interface to help.
Type 'q()' to quit R.

> # Financial Resilience of Serbian Enterprises to COVID-19: Panel Data Analysis
> 
> rm(list = ls()) # Clear workspace
> # Version information
> cat("R version:", R.version.string, "\n")
R version: R version 4.5.0 (2025-04-11 ucrt) 
> cat("Analysis date:", Sys.Date(), "\n")
Analysis date: 20359 
> 
> # Load required libraries
> library(plm)       # Panel data econometrics
Warning message:
package ‘plm’ was built under R version 4.5.1 
> library(dplyr)     # Data manipulation

Attaching package: ‘dplyr’

The following objects are masked from ‘package:plm’:

    between, lag, lead

The following objects are masked from ‘package:stats’:

    filter, lag

The following objects are masked from ‘package:base’:

    intersect, setdiff, setequal, union

Warning message:
package ‘dplyr’ was built under R version 4.5.1 
> library(tidyr)     # Data reshaping
Warning message:
package ‘tidyr’ was built under R version 4.5.1 
> library(readxl)    # Excel file reading
Warning message:
package ‘readxl’ was built under R version 4.5.1 
> library(lmtest)    # Robust standard errors
Loading required package: zoo

Attaching package: ‘zoo’

The following objects are masked from ‘package:base’:

    as.Date, as.Date.numeric

Warning message:
package ‘lmtest’ was built under R version 4.5.1 
> library(sandwich)  # HAC standard errors
Warning message:
package ‘sandwich’ was built under R version 4.5.1 
> 
> # Set working directory and load data
> setwd("C:/path/to/your/data")
> data <- read_excel("data.xlsx", sheet = "Data", col_types = c("numeric", "numeric", "text", rep("numeric", 399)))
> # Validate data structure
> cat("\nData dimensions:", dim(data), "\n")

Data dimensions: 35427 402 
> cat("Column names check:", all(c("mb", "year", "name") %in% names(data)), "\n")
Column names check: TRUE 
> cat("Year range:", range(data$year, na.rm = TRUE), "\n")
Year range: 2013 2023 
> 
> EUR <- 117.50  # Average RSD to EUR exchange rate
> 
> # Extract time-invariant firm characteristics (size, ownership, foreign status) --fixed over the analysis period to avoid endogeneity
> df_company <- data %>%
+   filter(year>2017, year<2024, mb!=7777663 ) %>%  #Analise only observed period, and remove specific outlier firm (MERCATA)
+   mutate(
+     ID = mb,
+     Name = name,
+     OperatingRevenue = AOP1001,
+     Assets = AOP59,
+     Employees = AOP9005,
+     Ownership = ifelse(is.na(AOP9002), 2, AOP9002),      # Ownership types: 1=socialistic, 2=private, 3=cooperative, 4=mixed, 5=government, Default to private if missing
+     Foreign = ifelse(is.na(AOP9003), FALSE, AOP9003 > 0),     # Foreign ownership indicator
+     Liquidation = grepl("U STEČAJU|U LIKVIDACIJI", Name, ignore.case = FALSE),     # Check for bankruptcy/liquidation
+     
+     # Determine firm size based on Serbian Law on Accounting (Article 7)
+     # Size categories: 1=Micro, 2=Small, 3=Medium, 4=Large
+     # Classification requires meeting 2 of 3 criteria (employees, revenue, assets)
+     Size = case_when(
+       # Missing data defaults to Medium
+       is.na(Employees) | is.na(OperatingRevenue) | is.na(Assets) ~ 3, # Default to Medium for no data
+       rowSums(cbind(Employees <= 10, OperatingRevenue <= 700*EUR, Assets <= 350*EUR)) >= 2 ~ 1, # Micro: ≤10 employees, ≤700k  revenue, ≤350k EUR assets
+       rowSums(cbind(Employees > 10 & Employees <= 50, OperatingRevenue > 700*EUR & OperatingRevenue <= 8000*EUR, Assets > 350*EUR & Assets <= 4000*EUR)) >= 2 ~ 2, # Small: 11-50 employees, 700k-8M EUR revenue, 350k-4M EUR assets
+       rowSums(cbind(Employees > 50 & Employees <= 250, OperatingRevenue > 8000*EUR & OperatingRevenue <= 40000*EUR, Assets > 4000*EUR & Assets <= 20000*EUR)) >= 2 ~ 3, # Medium: 51-250 employees, 8M-40M EUR revenue, 4M-20M EUR assets   
+       rowSums(cbind(Employees > 250, OperatingRevenue > 40000*EUR, Assets > 20000*EUR)) >= 2 ~ 4, # Large: >250 employees, >40M EUR revenue, >20M EUR assets
+       TRUE ~ 3 # Default to Medium for edge cases
+     )
+   ) %>%
+   group_by(ID) %>% 
+   summarise( # Keep maximum values per firm (in case of changes over time)
+     Size = max(Size, na.rm = TRUE), 
+     Foreign = max(Foreign, na.rm = TRUE), 
+     Ownership = max(Ownership, na.rm = TRUE), 
+     Liquidation = any(Liquidation, na.rm = TRUE),
+     Name = names(sort(table(Name), decreasing = TRUE))[1],
+     .groups = "drop"
+   )
> 
> cat("\nTotal firms:", nrow(df_company), " original firm size distribution:\n")

Total firms: 3312  original firm size distribution:
> size_dist <- df_company %>%
+   count(Size) %>%
+   mutate(
+     Size_Label = c("Micro", "Small", "Medium", "Large")[Size],
+     percentage = round(n/sum(n)*100, 1)
+   )
> print(as.data.frame(size_dist))
  Size    n Size_Label percentage
1    1   42      Micro        1.3
2    2    1      Small        0.0
3    3 2766     Medium       83.5
4    4  503      Large       15.2
> 
> cat("\nFirms in bankruptcy/liquidation:", sum(df_company$Liquidation), "\nSize distribution:\n")

Firms in bankruptcy/liquidation: 34 
Size distribution:
> size_dist <- df_company %>%
+   filter(Liquidation == TRUE) %>%
+   count(Size) %>%
+   mutate(
+     Size_Label = c("Micro", "Small", "Medium", "Large")[Size],
+     percentage = round(n/sum(n)*100, 1)
+   )
> print(as.data.frame(size_dist))
  Size  n Size_Label percentage
1    3 34     Medium        100
> 
> df <- data %>% # Create main panel dataset with financial variables and calculated metrics
+   filter( year > 2017, year < 2024 ) %>%
+   select(mb, year, AOP1001, AOP1025, AOP1026, AOP1043, AOP1055, AOP1064, AOP59) %>%
+   rename(
+     ID = mb,
+     Year = year,
+     OperatingRevenue = AOP1001,
+     TotalRevenue = AOP1043,  # Includes operating + financial + other revenue
+     OperatingIncome = AOP1025,
+     OperatingLoss = AOP1026,
+     NetIncome = AOP1055,
+     NetLoss = AOP1064,    
+     Assets = AOP59
+   ) %>%
+   mutate(
+     OperatingResult = replace_na(OperatingIncome, 0) - replace_na(OperatingLoss, 0),  # Calculate profit metrics
+     NetResult = replace_na(NetIncome, 0) - replace_na(NetLoss, 0),    
+     OperatingMargin = ifelse(is.na(OperatingRevenue) | OperatingRevenue == 0, NA_real_, OperatingResult / OperatingRevenue), # Calculate margins (handling division by zero)
+     NetMargin = ifelse(is.na(OperatingRevenue) | OperatingRevenue == 0, NA_real_,  NetResult / OperatingRevenue),
+     logOperatingRevenue = log(OperatingRevenue), # Log transformations for elasticity interpretation
+     logTotalRevenue = log(TotalRevenue),
+     Covid_period = case_when(Year %in% 2018:2019 ~ 1, Year %in% 2020:2021 ~ 2, Year %in% 2022:2023 ~ 3 ) # Period indicators: 1=Pre-COVID (2018-19), 2=COVID (2020-21), 3=Post-COVID (2022-23)
+   )
> 
> df_merged <- df %>% #Combine panel data with firm characteristics and apply filters
+   left_join(df_company %>% select(ID, Size, Foreign, Ownership, Liquidation), by = "ID") %>%
+   # Apply filters:
+   filter(
+     Liquidation == FALSE,           # Exclude bankrupt firms
+     Size > 2,                       # ONLY Medium and Large firms
+     abs(OperatingMargin) < 1,       # Remove extreme operating margin outliers (>100%)
+     abs(NetMargin) < 1,             # Remove extreme net margin outliers (Op+Net n=213)
+     Ownership != 1,                 # Remove socialistic ownership (n=2)
+   ) %>%
+   mutate(
+     OperatingRevenue_mil = OperatingRevenue / 1000,     # Convert to millions for easier interpretation
+     TotalRevenue_mil = TotalRevenue / 1000,
+     Assets_mil = Assets / 1000,   
+     OperatingMargin_pct = OperatingMargin * 100,     # Convert margins to percentage points
+     NetMargin_pct = NetMargin * 100,  
+     Size = factor(Size, levels = c(3, 2, 1, 4), labels = c("Medium", "Small", "Micro", "Large")), # Medium is baseline (first)
+     Covid_period = factor(Covid_period, levels = c(1, 2, 3),  labels = c("Pre", "Covid", "Post")), # Pre-COVID is baseline
+     Foreign = as.factor(Foreign), Ownership = factor(Ownership, levels = c(2, 1, 3, 4, 5), labels = c("Private", "Socialistic", "Cooperative",  "Mixed", "Government")) # Private is baseline
+   )
> 
> cat("\nDATA QUALITY REPORT")

DATA QUALITY REPORT> cat("\nTotal observations:", nrow(df_merged))

Total observations: 16145> cat("\nUnique firms:", n_distinct(df_merged$ID))

Unique firms: 3020> cat("\nYears covered:", paste(sort(unique(df$Year)), collapse = ", "))

Years covered: 2018, 2019, 2020, 2021, 2022, 2023> 
> cat("\n\nTotal firms in analysis:", n_distinct(df_merged$ID), "\nFirm-Size Distribution in df_merged:\n")


Total firms in analysis: 3020 
Firm-Size Distribution in df_merged:
> size_dist_merged <- df_merged %>%
+   group_by(ID) %>%
+   summarise(Size = first(Size), .groups = "drop") %>%
+   count(Size) %>%
+   mutate(
+     Size_Label = as.character(Size),  # Since Size is already a factor with labels
+     percentage = round(n/sum(n)*100, 1)
+   )
> print(as.data.frame(size_dist_merged))
    Size    n Size_Label percentage
1 Medium 2519     Medium       83.4
2  Large  501      Large       16.6
> 
> cat("\nFirm ownership distribution in df_merged:\n")

Firm ownership distribution in df_merged:
> ownership_dist_merged <- df_merged %>%
+   group_by(ID) %>%
+   summarise(Ownership = first(Ownership), .groups = "drop") %>%
+   count(Ownership) %>%
+   mutate(
+     percentage = round(n/sum(n)*100, 1)
+   ) %>%
+   arrange(desc(n))  # Sort by count, descending
> print(as.data.frame(ownership_dist_merged))
    Ownership    n percentage
1     Private 2593       85.9
2  Government  234        7.7
3 Cooperative   97        3.2
4       Mixed   96        3.2
> 
> cat("\nFirm foreign ownership distribution in df_merged:\n")

Firm foreign ownership distribution in df_merged:
> foreign_dist_merged <- df_merged %>%
+   group_by(ID) %>%
+   summarise(Foreign = first(Foreign), .groups = "drop") %>%
+   count(Foreign) %>%
+   mutate(
+     Foreign_Label = ifelse(Foreign == 1, "Foreign", "Domestic"),
+     percentage = round(n / sum(n) * 100, 1)
+   ) %>%
+   select(Foreign_Label, n, percentage)
> print(as.data.frame(foreign_dist_merged))
  Foreign_Label    n percentage
1      Domestic 2330       77.2
2       Foreign  690       22.8
> 
> cat("\nPanel Balance Check:\n")

Panel Balance Check:
> panel_balance <- df_merged %>%
+   group_by(ID) %>%
+   summarise(n_years = n_distinct(Year), .groups = "drop") %>%
+   count(n_years)
> print(as.data.frame(panel_balance))
  n_years    n
1       1  169
2       2   98
3       3  115
4       4   90
5       5  213
6       6 2335
> cat("\nPerfectly balanced firms (6 years):", 
+     panel_balance$n[panel_balance$n_years == 6], "\n")

Perfectly balanced firms (6 years): 2335 
> 
> cat("\nSummary Statistics by Size:\n")

Summary Statistics by Size:
> size_stats <- df_merged %>%
+   group_by(Size) %>%
+   summarise(
+     n_firms = n_distinct(ID),
+     n_obs = n(),
+     mean_revenue = round(mean(OperatingRevenue_mil, na.rm = TRUE), 2),
+     median_revenue = round(median(OperatingRevenue_mil, na.rm = TRUE), 2),
+     mean_tot_revenue = round(mean(TotalRevenue_mil, na.rm = TRUE), 2),
+     median_tot_revenue = round(median(TotalRevenue_mil, na.rm = TRUE), 2),    
+     mean_assets = round(mean(Assets_mil, na.rm = TRUE), 2),
+     median_assets = round(median(Assets_mil, na.rm = TRUE), 2),    
+     mean_op_margin = round(mean(OperatingMargin_pct, na.rm = TRUE), 2),
+     median_op_margin = round(median(OperatingMargin_pct, na.rm = TRUE), 2),    
+     .groups = "drop"
+   )
> print(as.data.frame(size_stats))
    Size n_firms n_obs mean_revenue median_revenue mean_tot_revenue median_tot_revenue mean_assets median_assets mean_op_margin median_op_margin
1 Medium    2519 13370       903.71         563.75           505.71               3.28      670.99         13.27           6.14             4.35
2  Large     501  2775     12907.19        5941.95          7996.14            1438.61    10501.07       1971.84           4.22             3.86
> 
> cat("\nAssets by Size\n")

Assets by Size
> assets_wide <- df_merged %>%
+   group_by(Year, Size) %>%
+   summarise(
+     assets = paste0(round(mean(Assets_mil, na.rm = TRUE), 1), 
+                     " (", 
+                     round(median(Assets_mil, na.rm = TRUE), 1), 
+                     ") n=",
+                     n()),
+     .groups = "drop"
+   ) %>%
+   pivot_wider(names_from = Size, values_from = assets, values_fill = "N/A")
> print(as.data.frame(assets_wide))
  Year                Medium                  Large
1 2018        4.2 (0) n=2320         36.2 (0) n=425
2 2019        5.9 (0) n=2276         41.2 (0) n=464
3 2020        6.1 (0) n=2232         33.8 (0) n=462
4 2021 1280.8 (637.7) n=2199   18413 (5223.3) n=464
5 2022 1350.8 (679.4) n=2181   20569 (5865.7) n=464
6 2023   1468 (740.8) n=2162 22182.9 (6864.7) n=496
> cat("Note: Values shown as Mean (Median) n=count in million RSD\n")
Note: Values shown as Mean (Median) n=count in million RSD
> 
> # --- Assets by Ownership (2021–2023) ---
> 
> assets_by_ownership <- df_merged %>%
+   filter(Year %in% 2021:2023) %>%
+   group_by(Ownership) %>%
+   summarise(
+     mean_assets = round(mean(Assets_mil, na.rm = TRUE), 1),
+     median_assets = round(median(Assets_mil, na.rm = TRUE), 1),
+     observations = n(),
+     .groups = "drop"
+   ) %>%
+   arrange(desc(mean_assets))
> print(as.data.frame(assets_by_ownership))
    Ownership mean_assets median_assets observations
1       Mixed     19373.0        1467.2          250
2  Government     18584.1         961.1          666
3     Private      2987.2         894.8         6770
4 Cooperative      2266.4        1452.1          280
> 
> 
> ###############################################################################################################
> run_panel_regression <- function(depvar) { # Function to run fixed effects model with robust standard errors
+   cat("\n\n==========================================")
+   cat("\nDEPENDENT VARIABLE:", depvar)
+     
+   df_test <- df_merged %>% filter(!is.na(!!sym(depvar)))   # Filter for valid observations
+   
+     if (depvar %in% c("logOperatingRevenue", "logTotalRevenue", "OperatingRevenue_mil", "TotalRevenue_mil", "Assets_mil")) { 
+     df_test <- df_test %>%  # For revenue/asset variables, ensure positive values
+       filter(!!sym(depvar) > 0)
+   }
+   
+   pdata <- pdata.frame(df_test, index = c("ID", "Year")) # Create panel data structure
+   formula <- as.formula(paste0( depvar, " ~ Covid_period * Size + Covid_period * Foreign + Covid_period * Ownership"  )) # Model specification with interaction terms
+   model <- plm(formula, data = pdata, model = "within") # Estimate fixed effects model
+   model_summary <- summary(model) # Get model summary
+   
+   cat("\nModel Diagnostics:")
+   cat("\n  Observations:", nobs(model))
+   cat("\n  Firms:", length(unique(df_test$ID)))
+   cat("\n  R-squared:", round(model_summary$r.squared["rsq"], 4))
+   cat("\n  Adj R-squared:", round(model_summary$r.squared["adjrsq"], 4))
+   
+   robust_se <- coeftest(model, vcov = function(x) vcovHC(x, method = "arellano", type = "HC1")) # Apply HAC robust standard errors (Arellano method)
+   
+   cat("\n\nResults with Robust Standard Errors:\n")
+   if (grepl("_mil$", depvar)) cat("(Values in RSD millions)\n")
+   if (grepl("_pct$", depvar)) cat("(Values in percentage points)\n")
+   print(robust_se)
+   
+   return(list( # Return results for summary table
+     depvar = depvar,
+     nobs = nobs(model),
+     nfirms = length(unique(df_test$ID)),
+     rsq = model_summary$r.squared["rsq"],
+     adjrsq = model_summary$r.squared["adjrsq"],
+     fstat = model_summary$fstatistic
+   ))
+ }
> 
> dependent_vars <- c( # Define dependent variables
+   "logOperatingRevenue",    # Log operating revenue
+   "OperatingRevenue_mil",   # Operating revenue (millions)
+   "logTotalRevenue",        # Log total revenue
+   "TotalRevenue_mil",       # Total revenue (millions)
+   "Assets_mil",             # Total assets (millions)
+   "OperatingMargin_pct",    # Operating margin (%)
+   "NetMargin_pct"          # Net margin (%)
+ )
> results <- lapply(dependent_vars, run_panel_regression) # Run all regressions


==========================================
DEPENDENT VARIABLE: logOperatingRevenue
Model Diagnostics:
  Observations: 16145
  Firms: 3020
  R-squared: 0.0669
  Adj R-squared: -0.1488

Results with Robust Standard Errors:

t test of coefficients:

                                         Estimate Std. Error t value Pr(>|t|)    
Covid_periodCovid                      -0.0107404  0.0119613 -0.8979  0.36924    
Covid_periodPost                        0.1458617  0.0162349  8.9845  < 2e-16 ***
Covid_periodCovid:SizeLarge             0.1770604  0.0191111  9.2648  < 2e-16 ***
Covid_periodPost:SizeLarge              0.2908068  0.0275183 10.5678  < 2e-16 ***
Covid_periodCovid:Foreign1             -0.0270167  0.0214696 -1.2584  0.20828    
Covid_periodPost:Foreign1              -0.0209064  0.0320653 -0.6520  0.51442    
Covid_periodCovid:OwnershipCooperative  0.0813484  0.0456665  1.7814  0.07488 .  
Covid_periodPost:OwnershipCooperative   0.0642906  0.0699376  0.9193  0.35798    
Covid_periodCovid:OwnershipMixed       -0.0913547  0.0500504 -1.8253  0.06799 .  
Covid_periodPost:OwnershipMixed        -0.1785129  0.0762276 -2.3418  0.01920 *  
Covid_periodCovid:OwnershipGovernment   0.0068866  0.0240276  0.2866  0.77441    
Covid_periodPost:OwnershipGovernment    0.0351534  0.0350801  1.0021  0.31632    
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1



==========================================
DEPENDENT VARIABLE: OperatingRevenue_mil
Model Diagnostics:
  Observations: 16145
  Firms: 3020
  R-squared: 0.0855
  Adj R-squared: -0.1258

Results with Robust Standard Errors:
(Values in RSD millions)

t test of coefficients:

                                        Estimate Std. Error t value  Pr(>|t|)    
Covid_periodCovid                         76.625     39.598  1.9351   0.05300 .  
Covid_periodPost                         -82.141    138.182 -0.5944   0.55223    
Covid_periodCovid:SizeLarge             1244.128    204.154  6.0941 1.132e-09 ***
Covid_periodPost:SizeLarge              5052.994    626.506  8.0654 7.933e-16 ***
Covid_periodCovid:Foreign1              -130.802    118.485 -1.1040   0.26964    
Covid_periodPost:Foreign1                708.246    339.335  2.0872   0.03689 *  
Covid_periodCovid:OwnershipCooperative   -86.185     90.238 -0.9551   0.33955    
Covid_periodPost:OwnershipCooperative    -20.249    171.184 -0.1183   0.90584    
Covid_periodCovid:OwnershipMixed       -1236.644    754.791 -1.6384   0.10136    
Covid_periodPost:OwnershipMixed         2084.976   2679.777  0.7780   0.43656    
Covid_periodCovid:OwnershipGovernment    132.793    212.337  0.6254   0.53173    
Covid_periodPost:OwnershipGovernment    1323.278    865.149  1.5295   0.12616    
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1



==========================================
DEPENDENT VARIABLE: logTotalRevenue
Model Diagnostics:
  Observations: 9077
  Firms: 2835
  R-squared: 0.4867
  Adj R-squared: 0.2522

Results with Robust Standard Errors:

t test of coefficients:

                                        Estimate Std. Error t value  Pr(>|t|)    
Covid_periodCovid                       6.001729   0.224558 26.7269 < 2.2e-16 ***
Covid_periodPost                        6.490324   0.219982 29.5038 < 2.2e-16 ***
Covid_periodCovid:SizeLarge            -0.109183   0.251586 -0.4340 0.6643182    
Covid_periodPost:SizeLarge              0.835203   0.248618  3.3594 0.0007858 ***
Covid_periodCovid:Foreign1             -1.188466   0.253006 -4.6974 2.692e-06 ***
Covid_periodPost:Foreign1              -0.076909   0.248711 -0.3092 0.7571574    
Covid_periodCovid:OwnershipCooperative  1.262070   0.823761  1.5321 0.1255527    
Covid_periodPost:OwnershipCooperative   0.969391   0.775998  1.2492 0.2116321    
Covid_periodCovid:OwnershipMixed       -0.599719   0.738910 -0.8116 0.4170373    
Covid_periodPost:OwnershipMixed        -0.840524   0.794907 -1.0574 0.2903761    
Covid_periodCovid:OwnershipGovernment   0.877351   0.768112  1.1422 0.2534075    
Covid_periodPost:OwnershipGovernment    0.506637   0.774871  0.6538 0.5132423    
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1



==========================================
DEPENDENT VARIABLE: TotalRevenue_mil
Model Diagnostics:
  Observations: 9098
  Firms: 2836
  R-squared: 0.2075
  Adj R-squared: -0.1534

Results with Robust Standard Errors:
(Values in RSD millions)

t test of coefficients:

                                       Estimate Std. Error t value  Pr(>|t|)    
Covid_periodCovid                       -1581.0     1078.0 -1.4666  0.142539    
Covid_periodPost                        -2102.3     1302.7 -1.6137  0.106633    
Covid_periodCovid:SizeLarge              9979.2     1318.8  7.5671 4.369e-14 ***
Covid_periodPost:SizeLarge              16339.2     1894.6  8.6243 < 2.2e-16 ***
Covid_periodCovid:Foreign1               2817.5     1379.7  2.0421  0.041181 *  
Covid_periodPost:Foreign1                4873.1     1849.2  2.6352  0.008429 ** 
Covid_periodCovid:OwnershipCooperative  -1112.9     1470.3 -0.7569  0.449117    
Covid_periodPost:OwnershipCooperative   -1228.8     1585.7 -0.7749  0.438403    
Covid_periodCovid:OwnershipMixed        46086.3    24269.1  1.8990  0.057615 .  
Covid_periodPost:OwnershipMixed         54727.9    29366.3  1.8636  0.062421 .  
Covid_periodCovid:OwnershipGovernment   19043.5    16150.6  1.1791  0.238395    
Covid_periodPost:OwnershipGovernment    20381.4    16945.0  1.2028  0.229100    
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1



==========================================
DEPENDENT VARIABLE: Assets_mil
Model Diagnostics:
  Observations: 9317
  Firms: 2838
  R-squared: 0.0827
  Adj R-squared: -0.3214

Results with Robust Standard Errors:
(Values in RSD millions)

t test of coefficients:

                                       Estimate Std. Error t value  Pr(>|t|)    
Covid_periodCovid                       -3119.0     1503.3 -2.0748 0.0380478 *  
Covid_periodPost                        -4038.3     1912.1 -2.1120 0.0347255 *  
Covid_periodCovid:SizeLarge             16212.2     5246.5  3.0901 0.0020096 ** 
Covid_periodPost:SizeLarge              23530.5     7014.6  3.3545 0.0007996 ***
Covid_periodCovid:Foreign1               6145.8     3746.8  1.6403 0.1009915    
Covid_periodPost:Foreign1                7359.7     4607.7  1.5973 0.1102573    
Covid_periodCovid:OwnershipCooperative   1205.2     1635.8  0.7367 0.4613035    
Covid_periodPost:OwnershipCooperative    1761.5     1787.3  0.9855 0.3243910    
Covid_periodCovid:OwnershipMixed        35386.1    21732.6  1.6282 0.1035209    
Covid_periodPost:OwnershipMixed         48536.3    28966.1  1.6756 0.0938603 .  
Covid_periodCovid:OwnershipGovernment   25315.6    14490.9  1.7470 0.0806854 .  
Covid_periodPost:OwnershipGovernment    31840.5    17744.5  1.7944 0.0727987 .  
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1



==========================================
DEPENDENT VARIABLE: OperatingMargin_pct
Model Diagnostics:
  Observations: 16145
  Firms: 3020
  R-squared: 0.0664
  Adj R-squared: -0.1494

Results with Robust Standard Errors:
(Values in percentage points)

t test of coefficients:

                                        Estimate Std. Error  t value  Pr(>|t|)    
Covid_periodCovid                        0.60234    0.39331   1.5315   0.12567    
Covid_periodPost                         0.64643    0.62479   1.0346   0.30086    
Covid_periodCovid:SizeLarge              4.14211    0.74499   5.5599 2.751e-08 ***
Covid_periodPost:SizeLarge               6.80725    1.31404   5.1804 2.247e-07 ***
Covid_periodCovid:Foreign1               1.28662    0.74588   1.7250   0.08456 .  
Covid_periodPost:Foreign1                0.92859    1.31723   0.7050   0.48085    
Covid_periodCovid:OwnershipCooperative  -0.21523    1.35195  -0.1592   0.87351    
Covid_periodPost:OwnershipCooperative   -3.76014    2.86129  -1.3141   0.18882    
Covid_periodCovid:OwnershipMixed       -12.91103    2.37485  -5.4366 5.529e-08 ***
Covid_periodPost:OwnershipMixed        -23.39451    4.48295  -5.2185 1.831e-07 ***
Covid_periodCovid:OwnershipGovernment  -15.58695    1.23615 -12.6092 < 2.2e-16 ***
Covid_periodPost:OwnershipGovernment   -33.09405    2.11454 -15.6507 < 2.2e-16 ***
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1



==========================================
DEPENDENT VARIABLE: NetMargin_pct
Model Diagnostics:
  Observations: 16145
  Firms: 3020
  R-squared: 0.1121
  Adj R-squared: -0.0931

Results with Robust Standard Errors:
(Values in percentage points)

t test of coefficients:

                                         Estimate Std. Error t value  Pr(>|t|)    
Covid_periodCovid                        5.353476   0.334964 15.9822 < 2.2e-16 ***
Covid_periodPost                        10.777944   0.448553 24.0283 < 2.2e-16 ***
Covid_periodCovid:SizeLarge              0.224544   0.542956  0.4136 0.6792041    
Covid_periodPost:SizeLarge               0.893884   0.854417  1.0462 0.2954917    
Covid_periodCovid:Foreign1               0.282076   0.614299  0.4592 0.6461098    
Covid_periodPost:Foreign1               -0.089172   0.899437 -0.0991 0.9210274    
Covid_periodCovid:OwnershipCooperative  -0.167061   1.287553 -0.1298 0.8967655    
Covid_periodPost:OwnershipCooperative    0.457737   1.791761  0.2555 0.7983659    
Covid_periodCovid:OwnershipMixed        -4.646505   1.754297 -2.6486 0.0080913 ** 
Covid_periodPost:OwnershipMixed        -10.691456   2.471213 -4.3264 1.527e-05 ***
Covid_periodCovid:OwnershipGovernment   -3.222108   0.975962 -3.3015 0.0009644 ***
Covid_periodPost:OwnershipGovernment   -10.065791   1.320709 -7.6215 2.679e-14 ***
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

> 
> cat("\n\n=== SUMMARY OF ALL MODELS ===\n")


=== SUMMARY OF ALL MODELS ===
> summary_table <- data.frame(
+   Variable = sapply(results, function(x) x$depvar),
+   Obs = sapply(results, function(x) x$nobs),
+   Firms = sapply(results, function(x) x$nfirms),
+   R2 = round(sapply(results, function(x) x$rsq), 4),
+   Adj_R2 = round(sapply(results, function(x) x$adjrsq), 4),
+   F_stat = round(sapply(results, function(x) 
+     ifelse(!is.null(x$fstat), x$fstat$statistic, NA)), 2)
+ )
> print(summary_table)
              Variable   Obs Firms     R2  Adj_R2 F_stat
1  logOperatingRevenue 16145  3020 0.0669 -0.1488  78.32
2 OperatingRevenue_mil 16145  3020 0.0855 -0.1258 102.22
3      logTotalRevenue  9077  2835 0.4867  0.2522 492.30
4     TotalRevenue_mil  9098  2836 0.2075 -0.1534 136.40
5           Assets_mil  9317  2838 0.0827 -0.3214  48.58
6  OperatingMargin_pct 16145  3020 0.0664 -0.1494  77.69
7        NetMargin_pct 16145  3020 0.1121 -0.0931 137.99
> 
> # Export key results for paper
> write.csv(summary_table, "model_summary_results.csv", row.names = FALSE)
> 
> cat("\n\nANALYSIS COMPLETE")


ANALYSIS COMPLETE> cat("\nResults saved in 'results' object\n")

Results saved in 'results' obj