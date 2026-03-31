# =============================================================================
# DATA_PREPARE.R
# =============================================================================
# Purpose:
#   Master data preparation for the article. Sources the general pipeline
#   (01-03), then builds all article-specific analytical objects:
#   - M+L and S+M+L panels with distress indicators
#   - GDP growth rates
#   - Episode construction (baseline + all robustness variants)
#   - Person-period datasets ready for hazard modelling
#
# Prerequisites:
#   - 01_load_data.R, 02_classify_companies.R, 03_prepare_data.R
#   - Data files: data.xlsx, AOP_Mapping.xlsx, MB_Delatnost.xlsx, subsidiaries.xlsx
#
# Required packages:
#   dplyr, tidyr, readxl, skimr, survival, sandwich, lmtest, ggplot2, scales
#
# Output objects:
#   - df_ML:          Medium+Large panel (18,183 firm-years)
#   - df_SML:         Full S+M+L panel (for robustness)
#   - gdp_growth:     GDP growth rates 2013-2023
#   - baseline:       List (episodes_all, episodes_clean, pp)
#   - alt_def:        Alternative distress definition episodes
#   - sml_ep:         S+M+L sample episodes
#   - baseline_lc:    Baseline including left-censored episodes
#   - strict_ep:      Strict 2-year exit rule episodes
#
# Version: 1.0
# Date: March 2026
# =============================================================================

cat("=======================================================================\n")
cat("DATA PREPARATION\n")
cat("=======================================================================\n\n")

# =============================================================================
# STEP 1: SOURCE GENERAL DATA PIPELINE
# =============================================================================
cat("--- Sourcing general pipeline (01-03) ---\n\n")

source("01_load_data.R")
source("02_classify_companies.R")
source("03_prepare_data.R")

cat("\n--- Pipeline complete. df_final available. ---\n\n")

# =============================================================================
# STEP 2: BUILD M+L AND S+M+L PANELS
# =============================================================================
cat("--- Building analysis panels ---\n")

df_ML <- df_final %>%
  filter(Size %in% c("Medium", "Large")) %>%
  mutate(
    in_distress = as.integer(Risk_Zone == "Distress"),
    # Alternative distress: Negative ROA + above-median D/E
    distress_alt = as.integer(
      ROA < 0 & Debt_to_Equity > median(Debt_to_Equity, na.rm = TRUE)
    ),
    distress_alt = ifelse(is.na(distress_alt), 0L, distress_alt)
  ) %>%
  arrange(mb, year)

cat(sprintf("  df_ML: %d firm-years, %d firms\n",
            nrow(df_ML), n_distinct(df_ML$mb)))

df_SML <- df_final %>%
  mutate(
    in_distress = as.integer(Risk_Zone == "Distress")
  ) %>%
  arrange(mb, year)

cat(sprintf("  df_SML: %d firm-years, %d firms\n",
            nrow(df_SML), n_distinct(df_SML$mb)))

# =============================================================================
# STEP 3: GDP GROWTH RATES (World Bank WDI)
# =============================================================================
# Source: World Bank WDI, GDP growth (annual %) — Serbia
# https://data.worldbank.org/indicator/NY.GDP.MKTP.KD.ZG?locations=RS

gdp_growth <- tibble(
  year = 2013:2023,
  gdp_growth = c(2.6, -1.6, 1.8, 3.3, 2.1, 4.5, 4.3, -0.9, 7.5, 2.3, 2.5)
)

cat("  GDP growth rates loaded (2013-2023)\n")

# =============================================================================
# STEP 4: EPISODE CONSTRUCTION FUNCTION
# =============================================================================

build_episodes <- function(df, distress_var = "in_distress", label = "baseline",
                           include_left_censored = FALSE) {
  #' Build distress episodes and person-period dataset
  #'

#' @param df Panel data with firm-year observations
  #' @param distress_var Name of binary distress indicator column
  #' @param label Label for this episode set
  #' @param include_left_censored If TRUE, keep episodes starting in first year
  #' @return List with episodes_all, episodes_clean, pp (person-period)

  ep_raw <- df %>%
    arrange(mb, year) %>%
    group_by(mb) %>%
    mutate(
      d = !!sym(distress_var),
      prev_d = lag(d, default = 0L),
      prev_year = lag(year),
      new_ep = d == 1 & (prev_d == 0 | is.na(prev_year) | year - prev_year > 1),
      episode_id = cumsum(new_ep)
    ) %>%
    ungroup()

  distress_obs <- ep_raw %>% filter(d == 1)

  episodes <- distress_obs %>%
    group_by(mb, episode_id) %>%
    summarise(
      start_year = min(year),
      end_year   = max(year),
      duration   = n(),
      Size       = first(Size),
      Sector     = first(Sector),
      CR_entry   = first(Current_Ratio),
      DE_entry   = first(Debt_to_Equity),
      ROA_entry  = first(ROA),
      Z_entry    = first(Altman_Z_Score),
      .groups    = "drop"
    ) %>%
    mutate(
      left_censored  = start_year == min(df$year),
      right_censored = end_year == max(df$year)
    )

  if (include_left_censored) {
    episodes_clean <- episodes  # keep all
  } else {
    episodes_clean <- episodes %>% filter(!left_censored)
  }

  # Person-period expansion
  pp <- episodes_clean %>%
    rowwise() %>%
    do({
      ep <- .
      tibble(
        mb         = ep$mb,
        episode_id = ep$episode_id,
        t          = 1:ep$duration,
        event      = c(rep(0, ep$duration - 1),
                       ifelse(ep$right_censored, 0, 1)),
        Size       = ep$Size,
        Sector     = ep$Sector,
        CR_entry   = ep$CR_entry,
        DE_entry   = ep$DE_entry,
        ROA_entry  = ep$ROA_entry,
        Z_entry    = ep$Z_entry,
        start_year    = ep$start_year,
        calendar_year = ep$start_year + (1:ep$duration) - 1,
        left_censored = ep$left_censored
      )
    }) %>%
    ungroup() %>%
    mutate(
      log_t = log(t),
      sector_type = case_when(
        Sector %in% c("D", "E", "H") ~ "Regulated",
        Sector %in% c("C", "G", "F") ~ "Market",
        TRUE ~ "Other"
      )
    ) %>%
    left_join(gdp_growth, by = c("calendar_year" = "year"))

  list(
    label          = label,
    episodes_all   = episodes,
    episodes_clean = episodes_clean,
    pp             = pp
  )
}

# =============================================================================
# STEP 5: BUILD ALL EPISODE VARIANTS
# =============================================================================

cat("\n--- Building episode sets ---\n")

# 5.1 Baseline: Z < 1.1, M+L, exclude left-censored
baseline <- build_episodes(df_ML, "in_distress", "baseline")
cat(sprintf("  baseline:     %d episodes (%d all, %d left-cens), %d pp obs\n",
            nrow(baseline$episodes_clean), nrow(baseline$episodes_all),
            sum(baseline$episodes_all$left_censored),
            nrow(baseline$pp)))

# 5.2 Alternative distress: Neg ROA + High D/E
alt_def <- build_episodes(df_ML, "distress_alt", "alt_accounting")
cat(sprintf("  alt_def:      %d episodes, %d pp obs\n",
            nrow(alt_def$episodes_clean), nrow(alt_def$pp)))

# 5.3 Full S+M+L sample
sml_ep <- build_episodes(df_SML, "in_distress", "full_SML")
cat(sprintf("  sml_ep:       %d episodes, %d pp obs\n",
            nrow(sml_ep$episodes_clean), nrow(sml_ep$pp)))

# 5.4 Left-censored inclusion (for robustness)
baseline_lc <- build_episodes(df_ML, "in_distress", "with_left_censored",
                              include_left_censored = TRUE)
cat(sprintf("  baseline_lc:  %d episodes (incl. left-censored), %d pp obs\n",
            nrow(baseline_lc$episodes_clean), nrow(baseline_lc$pp)))

# 5.5 Strict exit: require 2 consecutive years of Z >= 1.1 to count as exit
#     Method: for each completed episode, verify that the firm remains
#     non-distressed for at least 2 years after exit. If the firm falls
#     back into distress in the second year after exit (or has no data),
#     reclassify that exit as censored.
#     This tests whether results are driven by temporary improvements.
#     Keeps same 609 episodes and 1,449 pp obs; only changes some events.
cat("  Building strict-exit episodes...\n")

# For each completed episode, check the 2nd year after exit
completed_eps <- baseline$episodes_clean %>%
  filter(!right_censored) %>%
  select(mb, episode_id, end_year) %>%
  mutate(
    check_year = end_year + 2  # 2nd year after last distress year
    # end_year+1 is already non-distressed (that's why episode ended)
    # We need end_year+2 to ALSO be non-distressed
  )

# Look up distress status in the panel for check_year
panel_lookup <- df_ML %>%
  select(mb, year, in_distress) %>%
  distinct()

exit_check <- completed_eps %>%
  left_join(panel_lookup, by = c("mb", "check_year" = "year")) %>%
  mutate(
    # Exit is non-durable if:
    # (a) firm is back in distress in check_year, OR
    # (b) firm has no data in check_year (can't confirm durable exit)
    non_durable = is.na(in_distress) | in_distress == 1
  )

non_durable_ids <- exit_check %>%
  filter(non_durable) %>%
  select(mb, episode_id) %>%
  mutate(non_durable = TRUE)

cat(sprintf("    Completed episodes: %d\n", nrow(completed_eps)))
cat(sprintf("    Non-durable exits (Z<1.1 or no data at t+2): %d\n",
            nrow(non_durable_ids)))
cat(sprintf("    Durable exits: %d\n",
            nrow(completed_eps) - nrow(non_durable_ids)))

# Create strict person-period: same as baseline but non-durable exits -> censored
strict_pp <- baseline$pp %>%
  left_join(non_durable_ids, by = c("mb", "episode_id")) %>%
  mutate(
    event = ifelse(!is.na(non_durable) & event == 1, 0L, event)
  ) %>%
  select(-non_durable)

strict_ep <- list(
  label = "strict_exit",
  episodes_all = baseline$episodes_all,
  episodes_clean = baseline$episodes_clean,
  pp = strict_pp
)

cat(sprintf("  strict_ep:    %d episodes, %d pp obs, %d events (was %d)\n",
            nrow(baseline$episodes_clean),
            nrow(strict_pp),
            sum(strict_pp$event),
            sum(baseline$pp$event)))

# =============================================================================
# STEP 6: SUMMARY
# =============================================================================

cat("\n--- Sample Selection Summary ---\n\n")
cat(sprintf("  Raw APR data:                35,416 firm-years, 3,800 firms\n"))
cat(sprintf("  After exclusions (df_final): %d firm-years, %d firms\n",
            nrow(df_final), n_distinct(df_final$mb)))
cat(sprintf("  Medium + Large (df_ML):      %d firm-years, %d firms\n",
            nrow(df_ML), n_distinct(df_ML$mb)))
cat(sprintf("  Distress episodes (all):     %d (%d firms)\n",
            nrow(baseline$episodes_all), n_distinct(baseline$episodes_all$mb)))
cat(sprintf("  Left-censored (excluded):    %d (%.1f%%)\n",
            sum(baseline$episodes_all$left_censored),
            100 * mean(baseline$episodes_all$left_censored)))
cat(sprintf("  Clean episodes:              %d (%d firms)\n",
            nrow(baseline$episodes_clean),
            n_distinct(baseline$episodes_clean$mb)))
cat(sprintf("  Right-censored:              %d (%.1f%%)\n",
            sum(baseline$episodes_clean$right_censored),
            100 * mean(baseline$episodes_clean$right_censored)))
cat(sprintf("  Completed (observed exit):   %d\n",
            sum(!baseline$episodes_clean$right_censored)))
cat(sprintf("  Person-period observations:  %d\n",
            nrow(baseline$pp)))

# Multi-episode firms
n_multi <- baseline$episodes_clean %>%
  count(mb) %>%
  filter(n > 1) %>%
  nrow()
cat(sprintf("  Firms with multiple episodes: %d (%.1f%%)\n",
            n_multi,
            100 * n_multi / n_distinct(baseline$episodes_clean$mb)))

cat("\n=======================================================================\n")
cat("DATA PREPARATION COMPLETE\n")
cat("=======================================================================\n")
cat("Objects available: df_ML, df_SML, gdp_growth,\n")
cat("  baseline, alt_def, sml_ep, baseline_lc, strict_ep\n")
cat("Each episode set contains: $episodes_all, $episodes_clean, $pp\n")
