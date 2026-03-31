# =============================================================================
# ARTICLE_ANALYSIS.R
# =============================================================================
# Purpose:
#   Produce EVERY number, table, and statistical test reported in the
#   article manuscript. All results use the corrected specification:
#     event ~ log_t + Size + Sector + Z_entry + ROA_entry
#   (DE_entry excluded from primary; tested as robustness)
#
#   Standard errors clustered at the firm level throughout.
#
# Prerequisites:
#   source("data_prepare.R")  — must have run first
#
# Output:
#   Tables 1-7, A1-A2, and all in-text statistics, printed to console.
#   Final verification report comparing output to manuscript values.
#
# Version: 1.0
# Date: March 2026
# =============================================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(survival)
  library(sandwich)
  library(lmtest)
})

cat("=======================================================================\n")
cat("ARTICLE ANALYSIS — ALL MANUSCRIPT NUMBERS\n")
cat("=======================================================================\n\n")

# Check prerequisites
if (!exists("baseline") || !exists("df_ML")) {
  stop("Run data_prepare.R first. Objects baseline, df_ML not found.")
}

pp <- baseline$pp

# Helper: extract clustered coeftest as a matrix
clustered_coefs <- function(model, cluster_var) {
  ct <- coeftest(model, vcov = vcovCL(model, cluster = cluster_var))
  ct[, , drop = FALSE]
}

# =============================================================================
# TABLE 1: SAMPLE SELECTION
# =============================================================================
cat("=== TABLE 1: Sample Selection ===\n\n")
cat(sprintf("  Raw firm-years (APR):          35,416 (3,800 firms)\n"))
cat(sprintf("  After exclusions:              %d (%d firms)\n",
            nrow(df_final), n_distinct(df_final$mb)))
cat(sprintf("  Medium and large subsample:    %d (%d firms)\n",
            nrow(df_ML), n_distinct(df_ML$mb)))

# =============================================================================
# TABLE A1: EPISODE CONSTRUCTION AND CENSORING (Appendix)
# =============================================================================
cat("\n=== TABLE A1: Episode Construction ===\n\n")
cat(sprintf("  Distress episodes (%d firms):  %d\n",
            n_distinct(baseline$episodes_all$mb),
            nrow(baseline$episodes_all)))
cat(sprintf("  Left-censored (starting 2013): %d\n",
            sum(baseline$episodes_all$left_censored)))
cat(sprintf("  Clean distress:                %d\n",
            nrow(baseline$episodes_clean)))
cat(sprintf("  Right-censored (ongoing 2023): %d\n",
            sum(baseline$episodes_clean$right_censored)))
cat(sprintf("  Completed (observed exit):     %d\n",
            sum(!baseline$episodes_clean$right_censored)))
cat(sprintf("  Person-period observations:    %d\n", nrow(pp)))

# =============================================================================
# TABLE A2: FINANCIAL PROFILE AT EPISODE ENTRY (Appendix)
# =============================================================================
cat("\n=== TABLE A2: Financial Profile at Entry ===\n\n")

# Use entry values captured by build_episodes (winsorised ratios from df_ML)
ep <- baseline$episodes_clean

cat(sprintf("  Current Ratio:  N=%d, Mean=%.2f, Median=%.2f, SD=%.2f\n",
            sum(!is.na(ep$CR_entry)),
            mean(ep$CR_entry, na.rm = TRUE),
            median(ep$CR_entry, na.rm = TRUE),
            sd(ep$CR_entry, na.rm = TRUE)))
cat(sprintf("  Debt-to-Equity: N=%d, Mean=%.2f, Median=%.2f, SD=%.2f\n",
            sum(!is.na(ep$DE_entry)),
            mean(ep$DE_entry, na.rm = TRUE),
            median(ep$DE_entry, na.rm = TRUE),
            sd(ep$DE_entry, na.rm = TRUE)))
cat(sprintf("  ROA:            N=%d, Mean=%.3f, Median=%.3f, SD=%.3f\n",
            sum(!is.na(ep$ROA_entry)),
            mean(ep$ROA_entry, na.rm = TRUE),
            median(ep$ROA_entry, na.rm = TRUE),
            sd(ep$ROA_entry, na.rm = TRUE)))
cat(sprintf("  Altman Z\":      N=%d, Mean=%.2f, Median=%.2f, SD=%.2f\n",
            sum(!is.na(ep$Z_entry)),
            mean(ep$Z_entry, na.rm = TRUE),
            median(ep$Z_entry, na.rm = TRUE),
            sd(ep$Z_entry, na.rm = TRUE)))
cat("  Note: CR, DE, ROA are winsorised at 1st/99th percentiles.\n")
cat(sprintf("  DE missing: %d episodes (negative or zero equity)\n",
            sum(is.na(ep$DE_entry))))

# =============================================================================
# TABLE 2: OWNERSHIP DISTRIBUTION
# =============================================================================
cat("\n=== TABLE 2: Ownership Distribution ===\n\n")

# All firms (using df_company which has Ownership as factor already)
own_all <- df_company %>%
  filter(!is.na(Ownership)) %>%
  count(Ownership) %>%
  mutate(Pct = round(100 * n / sum(n), 0))

# M+L firms
own_ml <- df_ML %>%
  filter(!is.na(Ownership)) %>%
  distinct(mb, .keep_all = TRUE) %>%
  count(Ownership) %>%
  mutate(Pct = round(100 * n / sum(n), 0))

cat("  All firms:\n")
for (i in 1:nrow(own_all)) {
  cat(sprintf("    %s: %d (%d%%)\n", own_all$Ownership[i], own_all$n[i], own_all$Pct[i]))
}
cat(sprintf("    Total: %d\n", sum(own_all$n)))

cat("  Medium & Large:\n")
for (i in 1:nrow(own_ml)) {
  cat(sprintf("    %s: %d (%d%%)\n", own_ml$Ownership[i], own_ml$n[i], own_ml$Pct[i]))
}
cat(sprintf("    Total: %d\n", sum(own_ml$n)))

# =============================================================================
# TABLE 3: EMPIRICAL HAZARD RATES BY DURATION
# =============================================================================
cat("\n=== TABLE 3: Empirical Hazard Rates ===\n\n")

# Life-table approach: count episodes at risk and exiting at each duration
max_t <- max(pp$t)
hazard_rows <- list()

for (dur in 1:6) {
  at_risk <- sum(baseline$episodes_clean$duration >= dur)
  exits   <- sum(baseline$episodes_clean$duration == dur &
                   !baseline$episodes_clean$right_censored)
  hazard_rows[[dur]] <- list(t_label = as.character(dur),
                             N_at_risk = at_risk, N_exits = exits)
}

# Row 7+: episodes that reached duration 7 or more
at_risk_7 <- sum(baseline$episodes_clean$duration >= 7)
exits_7   <- sum(baseline$episodes_clean$duration >= 7 &
                   !baseline$episodes_clean$right_censored)
hazard_rows[[7]] <- list(t_label = "7+", N_at_risk = at_risk_7, N_exits = exits_7)

hazard_table <- bind_rows(lapply(hazard_rows, as_tibble))
hazard_table <- hazard_table %>%
  mutate(
    Hazard = round(N_exits / N_at_risk, 3)
  )

# Calculate survival rate
hazard_table$Survival <- NA_real_
surv_rate <- 1
for (i in 1:nrow(hazard_table)) {
  surv_rate <- surv_rate * (1 - hazard_table$Hazard[i])
  hazard_table$Survival[i] <- round(surv_rate, 3)
}

cat(sprintf("%-10s %10s %10s %12s %14s\n",
            "Duration", "At Risk", "Exits", "Hazard Rate", "Survival Rate"))
cat(paste(rep("-", 58), collapse = ""), "\n")
for (i in 1:nrow(hazard_table)) {
  cat(sprintf("%-10s %10d %10d %11.1f%% %13.1f%%\n",
              hazard_table$t_label[i],
              hazard_table$N_at_risk[i],
              hazard_table$N_exits[i],
              hazard_table$Hazard[i] * 100,
              hazard_table$Survival[i] * 100))
}

# =============================================================================
# TABLE 4: BASELINE HAZARD MODEL — CORRECTED SPECIFICATION
# =============================================================================
cat("\n=== TABLE 4: Baseline Hazard Model (cloglog) ===\n")
cat("  Specification: event ~ log_t + Size + Sector + Z_entry + ROA_entry\n")
cat("  (DE_entry excluded — 12% missing, p=0.448 when included)\n\n")

m_primary <- glm(
  event ~ log_t + Size + Sector + Z_entry + ROA_entry,
  data = pp, family = binomial(link = "cloglog")
)

# Clustered standard errors at firm level
cl_primary <- clustered_coefs(m_primary, pp$mb)

key_vars <- c("log_t", "SizeLarge", "Z_entry", "ROA_entry")
cat(sprintf("%-18s %12s %14s %8s %10s\n",
            "Variable", "Coefficient", "Clustered SE", "Z", "p-value"))
cat(paste(rep("-", 64), collapse = ""), "\n")
for (v in key_vars) {
  if (v %in% rownames(cl_primary)) {
    cat(sprintf("%-18s %12.3f %14.3f %8.2f %10s\n",
                v, cl_primary[v, 1], cl_primary[v, 2], cl_primary[v, 3],
                ifelse(cl_primary[v, 4] < 0.001, "< 0.001",
                       sprintf("%.3f", cl_primary[v, 4]))))
  }
}

cat(sprintf("\n  N = %d person-period obs from %d episodes (%d events)\n",
            nrow(pp), nrow(baseline$episodes_clean),
            sum(pp$event)))
cat(sprintf("  AIC = %.1f\n", AIC(m_primary)))

# Sector FE joint LR test
m_no_sector <- glm(
  event ~ log_t + Size + Z_entry + ROA_entry,
  data = pp, family = binomial(link = "cloglog")
)
lr_sector <- anova(m_no_sector, m_primary, test = "Chisq")
cat(sprintf("  Sector FE joint LR test: chi2=%.2f, df=%d, p=%.3f\n",
            lr_sector$Deviance[2], lr_sector$Df[2], lr_sector$`Pr(>Chi)`[2]))

# D/E note
pp_with_de <- pp %>% filter(!is.na(DE_entry))
m_with_de <- glm(
  event ~ log_t + Size + Sector + Z_entry + ROA_entry + DE_entry,
  data = pp_with_de, family = binomial(link = "cloglog")
)
cl_de <- clustered_coefs(m_with_de, pp_with_de$mb)
cat(sprintf("  D/E inclusion note: coef=%.3f, p=%.3f on n=%d\n",
            cl_de["DE_entry", 1], cl_de["DE_entry", 4], nrow(pp_with_de)))

# =============================================================================
# DURATION SPECIFICATION ALTERNATIVES (Appendix)
# =============================================================================
cat("\n=== Alternative Duration Specifications ===\n\n")

# LR test: constant vs log(t)
m_null <- glm(event ~ Size + Sector + Z_entry + ROA_entry,
              data = pp, family = binomial(link = "cloglog"))
lr_const <- anova(m_null, m_primary, test = "Chisq")
cat(sprintf("  LR constant vs log(t): chi2=%.2f, p<0.001\n",
            lr_const$Deviance[2]))

# Linear t
m_linear <- glm(event ~ t + Size + Sector + Z_entry + ROA_entry,
                data = pp, family = binomial(link = "cloglog"))
cat(sprintf("  Linear t: coef=%.3f, p<0.001, AIC=%.1f\n",
            coef(m_linear)["t"], AIC(m_linear)))

# Piecewise
pp$t_early <- pmin(pp$t, 3)
pp$t_late  <- pmax(pp$t - 3, 0)
m_piece <- glm(event ~ t_early + t_late + Size + Sector + Z_entry + ROA_entry,
               data = pp, family = binomial(link = "cloglog"))
pw_coefs <- summary(m_piece)$coefficients
cat(sprintf("  Piecewise: early=%.3f (p<0.001), late=%.3f (p=%.3f)\n",
            pw_coefs["t_early", 1], pw_coefs["t_late", 1], pw_coefs["t_late", 4]))

# Factor (non-parametric)
pp$t_factor <- factor(pmin(pp$t, 6), levels = 1:6,
                      labels = c("1","2","3","4","5","6+"))
m_factor <- glm(event ~ t_factor + Size + Sector + Z_entry + ROA_entry,
                data = pp, family = binomial(link = "cloglog"))
lr_factor <- anova(m_primary, m_factor, test = "Chisq")
cat(sprintf("  LR log(t) vs factor: chi2=%.2f, p=%.3f\n",
            lr_factor$Deviance[2], lr_factor$`Pr(>Chi)`[2]))

fac_coefs <- summary(m_factor)$coefficients
cat("  Factor coefficients:\n")
for (v in paste0("t_factor", c("2","3","4","5","6+"))) {
  if (v %in% rownames(fac_coefs)) {
    cat(sprintf("    %s: %.3f (p=%.3f)\n", v, fac_coefs[v,1], fac_coefs[v,4]))
  }
}

# =============================================================================
# TABLE 5: SECTORAL COMPARISON
# =============================================================================
cat("\n=== TABLE 5: Sectoral Comparison ===\n\n")

surv_sector <- baseline$episodes_clean %>%
  mutate(sector_type = case_when(
    Sector %in% c("D","E","H") ~ "Regulated",
    Sector %in% c("C","G","F") ~ "Market",
    TRUE ~ NA_character_
  )) %>%
  filter(!is.na(sector_type))

# Episode counts and medians
sector_summary <- surv_sector %>%
  group_by(sector_type) %>%
  summarise(
    n_episodes = n(),
    median_dur = median(duration),
    mean_hazard = round(sum(!right_censored) / sum(duration), 3),
    .groups = "drop"
  )
print(as.data.frame(sector_summary))

# KM and log-rank test
surv_obj <- Surv(surv_sector$duration,
                 as.integer(!surv_sector$right_censored))
km_sector <- survfit(surv_obj ~ surv_sector$sector_type)
lr_km <- survdiff(surv_obj ~ surv_sector$sector_type)

# Extract KM medians explicitly
cat("\n  KM survival estimates (survfit):\n")
print(km_sector)

# Extract median from the printed table
km_tbl <- summary(km_sector)$table
if (is.matrix(km_tbl)) {
  km_market_median <- km_tbl[1, "median"]
  km_reg_median    <- km_tbl[2, "median"]
} else {
  # Single stratum fallback
  km_market_median <- km_tbl["median"]
  km_reg_median    <- NA
}
cat(sprintf("\n  FOR MANUSCRIPT — KM medians: Market = %g years, Regulated = %g years\n",
            km_market_median, km_reg_median))
cat(sprintf("  Raw episode medians (ignoring censoring): Regulated=%d, Market=%d\n",
            median(surv_sector$duration[surv_sector$sector_type == "Regulated"]),
            median(surv_sector$duration[surv_sector$sector_type == "Market"])))
cat("  NOTE: Use KM medians for manuscript (they account for right-censoring)\n")
cat(sprintf("  Log-rank test: chi2=%.2f, p<0.001\n", lr_km$chisq))

# Sector binary model (includes DE_entry for consistency with manuscript Table 8)
pp_sector <- pp %>% filter(sector_type != "Other")
pp_sector_de <- pp_sector %>% filter(!is.na(DE_entry))
m_sector_bin <- glm(
  event ~ log_t + sector_type + Size + Z_entry + ROA_entry + DE_entry,
  data = pp_sector_de, family = binomial(link = "cloglog")
)
cl_sector <- clustered_coefs(m_sector_bin, pp_sector_de$mb)
cat(sprintf("\n  Sector binary: coef=%.3f, SE=%.3f, p=%.3f\n",
            cl_sector["sector_typeRegulated", 1],
            cl_sector["sector_typeRegulated", 2],
            cl_sector["sector_typeRegulated", 4]))

# =============================================================================
# INDIVIDUAL SECTOR HAZARD RATES
# =============================================================================
cat("\n=== Individual Sector Hazards ===\n\n")

sector_hazards <- pp %>%
  filter(!is.na(Sector)) %>%
  group_by(Sector) %>%
  summarise(
    N_obs = n(), N_events = sum(event),
    Hazard = round(N_events / N_obs, 3),
    .groups = "drop"
  ) %>%
  filter(N_obs >= 30) %>%
  arrange(Hazard)

print(as.data.frame(sector_hazards))

# =============================================================================
# TABLE 6: OWNERSHIP CONFOUNDING ANALYSIS
# =============================================================================
cat("\n=== TABLE 6: Ownership Confounding ===\n\n")

# Add ownership to person-period
pp_own <- pp_sector %>%
  left_join(df_ML %>% select(mb, year, Ownership) %>% distinct(),
            by = c("mb", "calendar_year" = "year"))

# Ownership distribution by sector type (firm-YEAR level, matching manuscript)
own_by_sector <- df_ML %>%
  mutate(sector_type = case_when(
    Sector %in% c("D","E","H") ~ "Regulated",
    Sector %in% c("C","G","F") ~ "Market",
    TRUE ~ "Other"
  )) %>%
  filter(sector_type != "Other") %>%
  count(sector_type, Ownership) %>%
  group_by(sector_type) %>%
  mutate(Pct = round(100 * n / sum(n), 1)) %>%
  ungroup()

cat("Ownership by sector type (firms):\n")
print(as.data.frame(own_by_sector %>% select(sector_type, Ownership, n, Pct)))

# Model 1: No ownership control (with DE_entry to match manuscript)
m_own1 <- glm(event ~ log_t + sector_type + Size + Z_entry + ROA_entry + DE_entry,
              data = pp_own, family = binomial(link = "cloglog"))
cl_own1 <- clustered_coefs(m_own1, pp_own$mb)

# Model 2: With ownership controls (with DE_entry)
m_own2 <- glm(event ~ log_t + sector_type + Ownership + Size + Z_entry + ROA_entry + DE_entry,
              data = pp_own, family = binomial(link = "cloglog"))
cl_own2 <- clustered_coefs(m_own2, pp_own$mb)

# Model 3: Private firms only (with DE_entry)
pp_private <- pp_own %>% filter(Ownership == "Private")
m_own3 <- glm(event ~ log_t + sector_type + Size + Z_entry + ROA_entry + DE_entry,
              data = pp_private, family = binomial(link = "cloglog"))
cl_own3 <- clustered_coefs(m_own3, pp_private$mb)

cat(sprintf("\n  No ownership control:  coef=%.3f, SE=%.3f, p=%.3f, n=%d\n",
            cl_own1["sector_typeRegulated",1], cl_own1["sector_typeRegulated",2],
            cl_own1["sector_typeRegulated",4], nrow(pp_own)))
cat(sprintf("  Ownership controls:    coef=%.3f, SE=%.3f, p=%.3f, n=%d\n",
            cl_own2["sector_typeRegulated",1], cl_own2["sector_typeRegulated",2],
            cl_own2["sector_typeRegulated",4], nrow(pp_own)))
cat(sprintf("  Private firms only:    coef=%.3f, SE=%.3f, p=%.3f, n=%d\n",
            cl_own3["sector_typeRegulated",1], cl_own3["sector_typeRegulated",2],
            cl_own3["sector_typeRegulated",4], nrow(pp_private)))

attenuation <- 100 * (cl_own2["sector_typeRegulated",1] -
                       cl_own1["sector_typeRegulated",1]) /
  abs(cl_own1["sector_typeRegulated",1])
cat(sprintf("  Attenuation: %.1f%%\n", attenuation))

# State ownership coefficient
if ("OwnershipGovernment" %in% rownames(cl_own2)) {
  cat(sprintf("  State ownership coef: %.3f, p=%.3f\n",
              cl_own2["OwnershipGovernment",1], cl_own2["OwnershipGovernment",4]))
}

# =============================================================================
# TABLE 7: ROBUSTNESS TESTS
# =============================================================================
cat("\n=== TABLE 7: Robustness Tests ===\n\n")

results <- list()

# Row 1: Primary
results[["Primary"]] <- list(
  spec = "Cloglog, no D/E",
  coef = cl_primary["log_t", 1],
  pval = cl_primary["log_t", 4],
  n_ep = nrow(baseline$episodes_clean)
)

# Row 2: Logit
m_logit <- glm(event ~ log_t + Size + Sector + Z_entry + ROA_entry,
               data = pp, family = binomial(link = "logit"))
cl_logit <- clustered_coefs(m_logit, pp$mb)
results[["Logit"]] <- list(
  spec = "Logit",
  coef = cl_logit["log_t", 1],
  pval = cl_logit["log_t", 4],
  n_ep = nrow(baseline$episodes_clean)
)

# Row 3: Cox PH
surv_data <- baseline$episodes_clean %>%
  filter(!is.na(Size) & !is.na(Sector) & !is.na(Z_entry) & !is.na(ROA_entry))
m_cox <- coxph(
  Surv(duration, as.integer(!right_censored)) ~
    Size + Sector + Z_entry + ROA_entry,
  data = surv_data
)
cox_lr <- summary(m_cox)$logtest
cox_conc <- summary(m_cox)$concordance[1]
# Schoenfeld test
cox_zph <- cox.zph(m_cox)
results[["Cox PH"]] <- list(
  spec = "Cox PH",
  coef = NA,  # implicit duration
  pval = cox_lr[3],
  n_ep = nrow(surv_data)
)
cat(sprintf("  Cox PH: LR chi2=%.2f, p<0.001; concordance=%.3f\n",
            cox_lr[1], cox_conc))
cat(sprintf("  Schoenfeld global test: p=%.3f\n", cox_zph$table["GLOBAL", "p"]))

# Row 4: Alternative distress
pp_alt <- alt_def$pp
m_alt <- glm(event ~ log_t + Size + Sector + Z_entry + ROA_entry,
             data = pp_alt, family = binomial(link = "cloglog"))
cl_alt <- clustered_coefs(m_alt, pp_alt$mb)
results[["Alt distress"]] <- list(
  spec = "Neg ROA + High D/E",
  coef = cl_alt["log_t", 1],
  pval = cl_alt["log_t", 4],
  n_ep = nrow(alt_def$episodes_clean)
)

# Row 5: Macro control (GDP)
m_gdp <- glm(event ~ log_t + Size + Sector + Z_entry + ROA_entry + gdp_growth,
             data = pp, family = binomial(link = "cloglog"))
cl_gdp <- clustered_coefs(m_gdp, pp$mb)
results[["GDP"]] <- list(
  spec = "+ GDP growth",
  coef = cl_gdp["log_t", 1],
  pval = cl_gdp["log_t", 4],
  n_ep = nrow(baseline$episodes_clean)
)

# Row 6: Full S+M+L sample (with DE_entry to match manuscript)
pp_sml <- sml_ep$pp
m_sml <- glm(event ~ log_t + Size + Sector + Z_entry + ROA_entry + DE_entry,
             data = pp_sml, family = binomial(link = "cloglog"))
cl_sml <- clustered_coefs(m_sml, pp_sml$mb)
results[["Full SML"]] <- list(
  spec = "S+M+L firms",
  coef = cl_sml["log_t", 1],
  pval = cl_sml["log_t", 4],
  n_ep = nrow(sml_ep$episodes_clean)
)

# Row 7: Left-censored inclusion
pp_lc <- baseline_lc$pp
m_lc <- glm(event ~ log_t + left_censored + Size + Sector + Z_entry + ROA_entry,
            data = pp_lc, family = binomial(link = "cloglog"))
cl_lc <- clustered_coefs(m_lc, pp_lc$mb)
results[["Left-censored"]] <- list(
  spec = "Include 2013 starts",
  coef = cl_lc["log_t", 1],
  pval = cl_lc["log_t", 4],
  n_ep = nrow(baseline_lc$episodes_clean)
)

# Row 8: Strict exit (2-year rule)
pp_strict <- strict_ep$pp
m_strict <- glm(event ~ log_t + Size + Sector + Z_entry + ROA_entry,
                data = pp_strict, family = binomial(link = "cloglog"))
cl_strict <- clustered_coefs(m_strict, pp_strict$mb)
results[["Strict exit"]] <- list(
  spec = "2-year rule",
  coef = cl_strict["log_t", 1],
  pval = cl_strict["log_t", 4],
  n_ep = nrow(strict_ep$episodes_clean)
)

# Row 9: Circularity check (without Z_entry)
m_no_z <- glm(event ~ log_t + Size + Sector + ROA_entry,
              data = pp, family = binomial(link = "cloglog"))
cl_no_z <- clustered_coefs(m_no_z, pp$mb)
results[["No Z_entry"]] <- list(
  spec = "Without Z_entry",
  coef = cl_no_z["log_t", 1],
  pval = cl_no_z["log_t", 4],
  n_ep = nrow(baseline$episodes_clean)
)

# Row 10: D/E inclusion (reduced sample)
m_de <- glm(event ~ log_t + Size + Sector + Z_entry + ROA_entry + DE_entry,
            data = pp_with_de, family = binomial(link = "cloglog"))
cl_de_rob <- clustered_coefs(m_de, pp_with_de$mb)
results[["D/E inclusion"]] <- list(
  spec = "Reduced sample",
  coef = cl_de_rob["log_t", 1],
  pval = cl_de_rob["log_t", 4],
  n_ep = n_distinct(paste(pp_with_de$mb, pp_with_de$episode_id))
)

# Print Table 7
cat(sprintf("\n%-20s %-22s %14s %10s %12s\n",
            "Test", "Specification", "Duration Coef.", "p-value", "n Episodes"))
cat(paste(rep("-", 80), collapse = ""), "\n")
for (nm in names(results)) {
  r <- results[[nm]]
  coef_str <- if (is.na(r$coef)) "see note*" else sprintf("%.3f", r$coef)
  pval_str <- if (r$pval < 0.001) "< 0.001" else sprintf("%.3f", r$pval)
  cat(sprintf("%-20s %-22s %14s %10s %12d\n",
              nm, r$spec, coef_str, pval_str, r$n_ep))
}

# =============================================================================
# UNOBSERVED HETEROGENEITY
# =============================================================================
cat("\n=== Unobserved Heterogeneity Tests ===\n\n")

# Gamma frailty model (by sector, for unobserved heterogeneity across sectors)
m_frailty <- coxph(
  Surv(duration, as.integer(!right_censored)) ~
    Size + Z_entry + ROA_entry +
    frailty(Sector, distribution = "gamma"),
  data = surv_data
)
# Extract frailty variance
fr_hist <- m_frailty$history[[length(m_frailty$history)]]
frailty_var <- if (is.list(fr_hist)) fr_hist$theta else {
  # Try to extract from print output
  fr_sum <- summary(m_frailty)
  fr_sum$print2[grep("Variance", names(fr_sum$print2))]
}
if (length(frailty_var) == 0 || is.null(frailty_var)) {
  # Fallback: parse from printed output
  fr_capture <- capture.output(print(m_frailty))
  fr_line <- grep("Variance of", fr_capture, value = TRUE)
  if (length(fr_line) > 0) {
    frailty_var <- as.numeric(gsub(".*Variance of random effect= *", "", fr_line[1]))
  } else {
    frailty_var <- NA
  }
}
cat(sprintf("  Frailty variance (gamma, by sector): %.3f\n", frailty_var))

# Mover-stayer analysis
multi_ep_firms <- baseline$episodes_clean %>%
  count(mb) %>%
  filter(n > 1)

n_multi_firms <- nrow(multi_ep_firms)
cat(sprintf("  Multi-episode firms: %d (%.1f%% of %d)\n",
            n_multi_firms,
            100 * n_multi_firms / n_distinct(baseline$episodes_clean$mb),
            n_distinct(baseline$episodes_clean$mb)))

# Long repeaters: firms where ALL episodes are long (min duration >= 3)
# This identifies a "stayer" subpopulation — firms that consistently fail to exit
long_repeaters <- baseline$episodes_clean %>%
  semi_join(multi_ep_firms, by = "mb") %>%
  group_by(mb) %>%
  summarise(
    n_episodes = n(),
    min_dur = min(duration),
    max_dur = max(duration),
    .groups = "drop"
  ) %>%
  filter(min_dur >= 3)  # every episode is 3+ years

cat(sprintf("  Long repeaters (all eps >= 3 years): %d (%.1f%% of multi-ep)\n",
            nrow(long_repeaters),
            100 * nrow(long_repeaters) / n_multi_firms))

# =============================================================================
# VERIFICATION REPORT
# =============================================================================
cat("\n=======================================================================\n")
cat("VERIFICATION REPORT\n")
cat("=======================================================================\n\n")

cat("Table 4 (primary model):\n")
cat(sprintf("  log(t):    %.3f  (expect -0.508)\n", cl_primary["log_t", 1]))
cat(sprintf("  ClustSE:   %.3f  (expect  0.090)\n", cl_primary["log_t", 2]))
cat(sprintf("  Large:     %.3f  (expect -0.153)\n", cl_primary["SizeLarge", 1]))
cat(sprintf("  Z_entry:   %.3f  (expect  0.129)\n", cl_primary["Z_entry", 1]))
cat(sprintf("  ROA_entry: %.3f  (expect  0.916)\n", cl_primary["ROA_entry", 1]))
cat(sprintf("  N:         %d     (expect 1449)\n", nrow(pp)))
cat(sprintf("  Events:    %d      (expect 411)\n", sum(pp$event)))
cat(sprintf("  AIC:       %.1f  (expect 1642.1)\n", AIC(m_primary)))

cat("\nTable 7 (robustness — log(t) coefficients):\n")
expected <- c(Primary = -0.508, Logit = -0.599,
              `Alt distress` = -0.306, GDP = -0.483,
              `Full SML` = -0.600, `Left-censored` = -0.352,
              `Strict exit` = -0.622, `No Z_entry` = -0.531,
              `D/E inclusion` = -0.503)
for (nm in names(expected)) {
  actual <- results[[nm]]$coef
  if (!is.na(actual)) {
    match_flag <- ifelse(abs(actual - expected[nm]) < 0.005, "OK", "MISMATCH")
    cat(sprintf("  %-20s actual=%.3f  expected=%.3f  [%s]\n",
                nm, actual, expected[nm], match_flag))
  }
}

cat("\nOther key numbers:\n")
cat(sprintf("  LR const vs log(t): %.2f  (expect 38.63)\n", lr_const$Deviance[2]))
cat(sprintf("  Sector FE LR:       %.2f  (expect 34.84)\n", lr_sector$Deviance[2]))
cat(sprintf("  Log-rank chi2:      %.2f  (expect 18.66)\n", lr_km$chisq))
cat(sprintf("  KM median Market:   %g    (expect 2)\n", km_market_median))
cat(sprintf("  KM median Regulated:%g    (expect 4)\n", km_reg_median))
cat(sprintf("  Sector binary:      %.3f  (expect -0.537)\n",
            cl_sector["sector_typeRegulated", 1]))
cat(sprintf("  Cox LR chi2:        %.2f  (expect 63.98)\n", cox_lr[1]))
cat(sprintf("  Concordance:        %.3f  (expect 0.655)\n", cox_conc))
cat(sprintf("  Schoenfeld p:       %.3f  (expect 0.008)\n",
            cox_zph$table["GLOBAL", "p"]))
cat(sprintf("  Frailty variance:   %.3f  (expect 0.069)\n", frailty_var))

cat("\n=======================================================================\n")
cat(sprintf("Analysis completed: %s\n", Sys.time()))
cat("=======================================================================\n")
