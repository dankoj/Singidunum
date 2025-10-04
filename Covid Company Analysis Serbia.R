# Financial Resilience of Serbian Enterprises to COVID-19: Panel Data Analysis

rm(list = ls()) # Clear workspace
# Version information
cat("R version:", R.version.string, "\n")
cat("Analysis date:", Sys.Date(), "\n")

# Load required libraries
library(plm)       # Panel data econometrics
library(dplyr)     # Data manipulation
library(tidyr)     # Data reshaping
library(readxl)    # Excel file reading
library(lmtest)    # Robust standard errors
library(sandwich)  # HAC standard errors

# Set working directory and load data
#setwd("C:/path/to/your/data")
setwd("C:/SYNC/OneDrive - Jugodata/Doktorat/Danko papers/Financial Resilience to the Economic Shock of COVID")
data <- read_excel("data.xlsx", sheet = "Data", col_types = c("numeric", "numeric", "text", rep("numeric", 399)))
# Validate data structure
cat("\nData dimensions:", dim(data), "\n")
cat("Column names check:", all(c("mb", "year", "name") %in% names(data)), "\n")
cat("Year range:", range(data$year, na.rm = TRUE), "\n")

EUR <- 117.50  # Average RSD to EUR exchange rate

# Extract time-invariant firm characteristics (size, ownership, foreign status) --fixed over the analysis period to avoid endogeneity
df_company <- data %>%
  filter(year>2017, year<2024, mb!=7777663 ) %>%  #Analise only observed period, and remove specific outlier firm (MERCATA)
  mutate(
    ID = mb,
    Name = name,
    OperatingRevenue = AOP1001,
    Assets = AOP59,
    Employees = AOP9005,
    Ownership = ifelse(is.na(AOP9002), 2, AOP9002),      # Ownership types: 1=socialistic, 2=private, 3=cooperative, 4=mixed, 5=government, Default to private if missing
    Foreign = ifelse(is.na(AOP9003), FALSE, AOP9003 > 0),     # Foreign ownership indicator
    Liquidation = grepl("U STEČAJU|U LIKVIDACIJI", Name, ignore.case = FALSE),     # Check for bankruptcy/liquidation
    
    # Determine firm size based on Serbian Law on Accounting (Article 7)
    # Size categories: 1=Micro, 2=Small, 3=Medium, 4=Large
    # Classification requires meeting 2 of 3 criteria (employees, revenue, assets)
    Size = case_when(
      # Missing data defaults to Medium
      is.na(Employees) | is.na(OperatingRevenue) | is.na(Assets) ~ 3, # Default to Medium for no data
      rowSums(cbind(Employees <= 10, OperatingRevenue <= 700*EUR, Assets <= 350*EUR)) >= 2 ~ 1, # Micro: ≤10 employees, ≤700k  revenue, ≤350k EUR assets
      rowSums(cbind(Employees > 10 & Employees <= 50, OperatingRevenue > 700*EUR & OperatingRevenue <= 8000*EUR, Assets > 350*EUR & Assets <= 4000*EUR)) >= 2 ~ 2, # Small: 11-50 employees, 700k-8M EUR revenue, 350k-4M EUR assets
      rowSums(cbind(Employees > 50 & Employees <= 250, OperatingRevenue > 8000*EUR & OperatingRevenue <= 40000*EUR, Assets > 4000*EUR & Assets <= 20000*EUR)) >= 2 ~ 3, # Medium: 51-250 employees, 8M-40M EUR revenue, 4M-20M EUR assets   
      rowSums(cbind(Employees > 250, OperatingRevenue > 40000*EUR, Assets > 20000*EUR)) >= 2 ~ 4, # Large: >250 employees, >40M EUR revenue, >20M EUR assets
      TRUE ~ 3 # Default to Medium for edge cases
    )
  ) %>%
  group_by(ID) %>% 
  summarise( # Keep maximum values per firm (in case of changes over time)
    Size = max(Size, na.rm = TRUE), 
    Foreign = max(Foreign, na.rm = TRUE), 
    Ownership = max(Ownership, na.rm = TRUE), 
    Liquidation = any(Liquidation, na.rm = TRUE),
    Name = names(sort(table(Name), decreasing = TRUE))[1],
    .groups = "drop"
  )

cat("\nTotal firms:", nrow(df_company), " original firm size distribution:\n")
size_dist <- df_company %>%
  count(Size) %>%
  mutate(
    Size_Label = c("Micro", "Small", "Medium", "Large")[Size],
    percentage = round(n/sum(n)*100, 1)
  )
print(as.data.frame(size_dist))

cat("\nFirms in bankruptcy/liquidation:", sum(df_company$Liquidation), "\nSize distribution:\n")
size_dist <- df_company %>%
  filter(Liquidation == TRUE) %>%
  count(Size) %>%
  mutate(
    Size_Label = c("Micro", "Small", "Medium", "Large")[Size],
    percentage = round(n/sum(n)*100, 1)
  )
print(as.data.frame(size_dist))

df <- data %>% # Create main panel dataset with financial variables and calculated metrics
  filter( year > 2017, year < 2024 ) %>%
  select(mb, year, AOP1001, AOP1025, AOP1026, AOP1043, AOP1055, AOP1064, AOP59) %>%
  rename(
    ID = mb,
    Year = year,
    OperatingRevenue = AOP1001,
    TotalRevenue = AOP1043,  # Includes operating + financial + other revenue
    OperatingIncome = AOP1025,
    OperatingLoss = AOP1026,
    NetIncome = AOP1055,
    NetLoss = AOP1064,    
    Assets = AOP59
  ) %>%
  mutate(
    OperatingResult = replace_na(OperatingIncome, 0) - replace_na(OperatingLoss, 0),  # Calculate profit metrics
    NetResult = replace_na(NetIncome, 0) - replace_na(NetLoss, 0),    
    OperatingMargin = ifelse(is.na(OperatingRevenue) | OperatingRevenue == 0, NA_real_, OperatingResult / OperatingRevenue), # Calculate margins (handling division by zero)
    NetMargin = ifelse(is.na(OperatingRevenue) | OperatingRevenue == 0, NA_real_,  NetResult / OperatingRevenue),
    logOperatingRevenue = log(OperatingRevenue), # Log transformations for elasticity interpretation
    logTotalRevenue = log(TotalRevenue),
    Covid_period = case_when(Year %in% 2018:2019 ~ 1, Year %in% 2020:2021 ~ 2, Year %in% 2022:2023 ~ 3 ) # Period indicators: 1=Pre-COVID (2018-19), 2=COVID (2020-21), 3=Post-COVID (2022-23)
  )

df_merged <- df %>% #Combine panel data with firm characteristics and apply filters
  left_join(df_company %>% select(ID, Size, Foreign, Ownership, Liquidation), by = "ID") %>%
  # Apply filters:
  filter(
    Liquidation == FALSE,           # Exclude bankrupt firms
    Size > 2,                       # ONLY Medium and Large firms
    abs(OperatingMargin) < 1,       # Remove extreme operating margin outliers (>100%)
    abs(NetMargin) < 1,             # Remove extreme net margin outliers (Op+Net n=213)
    Ownership != 1,                 # Remove socialistic ownership (n=2)
  ) %>%
  mutate(
    OperatingRevenue_mil = OperatingRevenue / 1000,     # Convert to millions for easier interpretation
    TotalRevenue_mil = TotalRevenue / 1000,
    Assets_mil = Assets / 1000,   
    OperatingMargin_pct = OperatingMargin * 100,     # Convert margins to percentage points
    NetMargin_pct = NetMargin * 100,  
    Size = factor(Size, levels = c(3, 2, 1, 4), labels = c("Medium", "Small", "Micro", "Large")), # Medium is baseline (first)
    Covid_period = factor(Covid_period, levels = c(1, 2, 3),  labels = c("Pre", "Covid", "Post")), # Pre-COVID is baseline
    Foreign = as.factor(Foreign), Ownership = factor(Ownership, levels = c(2, 1, 3, 4, 5), labels = c("Private", "Socialistic", "Cooperative",  "Mixed", "Government")) # Private is baseline
  )

cat("\nDATA QUALITY REPORT")
cat("\nTotal observations:", nrow(df_merged))
cat("\nUnique firms:", n_distinct(df_merged$ID))
cat("\nYears covered:", paste(sort(unique(df$Year)), collapse = ", "))

cat("\n\nTotal firms in analysis:", n_distinct(df_merged$ID), "\nFirm-Size Distribution in df_merged:\n")
size_dist_merged <- df_merged %>%
  group_by(ID) %>%
  summarise(Size = first(Size), .groups = "drop") %>%
  count(Size) %>%
  mutate(
    Size_Label = as.character(Size),  # Since Size is already a factor with labels
    percentage = round(n/sum(n)*100, 1)
  )
print(as.data.frame(size_dist_merged))

cat("\nFirm ownership distribution in df_merged:\n")
ownership_dist_merged <- df_merged %>%
  group_by(ID) %>%
  summarise(Ownership = first(Ownership), .groups = "drop") %>%
  count(Ownership) %>%
  mutate(
    percentage = round(n/sum(n)*100, 1)
  ) %>%
  arrange(desc(n))  # Sort by count, descending
print(as.data.frame(ownership_dist_merged))

cat("\nFirm foreign ownership distribution in df_merged:\n")
foreign_dist_merged <- df_merged %>%
  group_by(ID) %>%
  summarise(Foreign = first(Foreign), .groups = "drop") %>%
  count(Foreign) %>%
  mutate(
    Foreign_Label = ifelse(Foreign == 1, "Foreign", "Domestic"),
    percentage = round(n / sum(n) * 100, 1)
  ) %>%
  select(Foreign_Label, n, percentage)
print(as.data.frame(foreign_dist_merged))

cat("\nPanel Balance Check:\n")
panel_balance <- df_merged %>%
  group_by(ID) %>%
  summarise(n_years = n_distinct(Year), .groups = "drop") %>%
  count(n_years)
print(as.data.frame(panel_balance))
cat("\nPerfectly balanced firms (6 years):", 
    panel_balance$n[panel_balance$n_years == 6], "\n")

cat("\nSummary Statistics by Size:\n")
size_stats <- df_merged %>%
  group_by(Size) %>%
  summarise(
    n_firms = n_distinct(ID),
    n_obs = n(),
    mean_revenue = round(mean(OperatingRevenue_mil, na.rm = TRUE), 2),
    median_revenue = round(median(OperatingRevenue_mil, na.rm = TRUE), 2),
    mean_tot_revenue = round(mean(TotalRevenue_mil, na.rm = TRUE), 2),
    median_tot_revenue = round(median(TotalRevenue_mil, na.rm = TRUE), 2),    
    mean_assets = round(mean(Assets_mil, na.rm = TRUE), 2),
    median_assets = round(median(Assets_mil, na.rm = TRUE), 2),    
    mean_op_margin = round(mean(OperatingMargin_pct, na.rm = TRUE), 2),
    median_op_margin = round(median(OperatingMargin_pct, na.rm = TRUE), 2),    
    .groups = "drop"
  )
print(as.data.frame(size_stats))

cat("\nAssets by Size\n")
assets_wide <- df_merged %>%
  group_by(Year, Size) %>%
  summarise(
    assets = paste0(round(mean(Assets_mil, na.rm = TRUE), 1), 
                    " (", 
                    round(median(Assets_mil, na.rm = TRUE), 1), 
                    ") n=",
                    n()),
    .groups = "drop"
  ) %>%
  pivot_wider(names_from = Size, values_from = assets, values_fill = "N/A")
print(as.data.frame(assets_wide))
cat("Note: Values shown as Mean (Median) n=count in million RSD\n")

# --- Assets by Ownership (2021–2023) ---

assets_by_ownership <- df_merged %>%
  filter(Year %in% 2021:2023) %>%
  group_by(Ownership) %>%
  summarise(
    mean_assets = round(mean(Assets_mil, na.rm = TRUE), 1),
    median_assets = round(median(Assets_mil, na.rm = TRUE), 1),
    observations = n(),
    .groups = "drop"
  ) %>%
  arrange(desc(mean_assets))
print(as.data.frame(assets_by_ownership))


###############################################################################################################
run_panel_regression <- function(depvar) { # Function to run fixed effects model with robust standard errors
  cat("\n\n==========================================")
  cat("\nDEPENDENT VARIABLE:", depvar)
    
  df_test <- df_merged %>% filter(!is.na(!!sym(depvar)))   # Filter for valid observations
  
    if (depvar %in% c("logOperatingRevenue", "logTotalRevenue", "OperatingRevenue_mil", "TotalRevenue_mil", "Assets_mil")) { 
    df_test <- df_test %>%  # For revenue/asset variables, ensure positive values
      filter(!!sym(depvar) > 0)
  }
  
  pdata <- pdata.frame(df_test, index = c("ID", "Year")) # Create panel data structure
  formula <- as.formula(paste0( depvar, " ~ Covid_period * Size + Covid_period * Foreign + Covid_period * Ownership"  )) # Model specification with interaction terms
  model <- plm(formula, data = pdata, model = "within") # Estimate fixed effects model
  model_summary <- summary(model) # Get model summary
  
  cat("\nModel Diagnostics:")
  cat("\n  Observations:", nobs(model))
  cat("\n  Firms:", length(unique(df_test$ID)))
  cat("\n  R-squared:", round(model_summary$r.squared["rsq"], 4))
  cat("\n  Adj R-squared:", round(model_summary$r.squared["adjrsq"], 4))
  
  robust_se <- coeftest(model, vcov = function(x) vcovHC(x, method = "arellano", type = "HC1")) # Apply HAC robust standard errors (Arellano method)
  
  cat("\n\nResults with Robust Standard Errors:\n")
  if (grepl("_mil$", depvar)) cat("(Values in RSD millions)\n")
  if (grepl("_pct$", depvar)) cat("(Values in percentage points)\n")
  print(robust_se)
  
  return(list( # Return results for summary table
    depvar = depvar,
    nobs = nobs(model),
    nfirms = length(unique(df_test$ID)),
    rsq = model_summary$r.squared["rsq"],
    adjrsq = model_summary$r.squared["adjrsq"],
    fstat = model_summary$fstatistic
  ))
}

dependent_vars <- c( # Define dependent variables
  "logOperatingRevenue",    # Log operating revenue
  "OperatingRevenue_mil",   # Operating revenue (millions)
  "logTotalRevenue",        # Log total revenue
  "TotalRevenue_mil",       # Total revenue (millions)
  "Assets_mil",             # Total assets (millions)
  "OperatingMargin_pct",    # Operating margin (%)
  "NetMargin_pct"          # Net margin (%)
)
results <- lapply(dependent_vars, run_panel_regression) # Run all regressions

cat("\n\n=== SUMMARY OF ALL MODELS ===\n")
summary_table <- data.frame(
  Variable = sapply(results, function(x) x$depvar),
  Obs = sapply(results, function(x) x$nobs),
  Firms = sapply(results, function(x) x$nfirms),
  R2 = round(sapply(results, function(x) x$rsq), 4),
  Adj_R2 = round(sapply(results, function(x) x$adjrsq), 4),
  F_stat = round(sapply(results, function(x) 
    ifelse(!is.null(x$fstat), x$fstat$statistic, NA)), 2)
)
print(summary_table)

# Export key results for paper
write.csv(summary_table, "model_summary_results.csv", row.names = FALSE)

cat("\n\nANALYSIS COMPLETE")
cat("\nResults saved in 'results' object\n")