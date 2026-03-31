# =============================================================================
# FIGURES_AND_CHECKS.R
# =============================================================================
# Purpose:
#   Generate all manuscript figures (Figures 1-4) and run diagnostic checks
#   not directly producing manuscript numbers (long episodes, recidivism, etc.)
#
# Prerequisites:
#   source("data_prepare.R")  — must have run first
#
# Output:
#   - output/Fig1_duration_distribution.png
#   - output/Fig2_hazard_rates.png
#   - output/Fig3_KM_sector_type.png
#   - output/Fig4_sector_hazards.png
#   - Diagnostic tables printed to console
#
# Version: 1.0
# Date: March 2026
# =============================================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  library(survival)
})

cat("=======================================================================\n")
cat("FIGURES AND DIAGNOSTIC CHECKS\n")
cat("=======================================================================\n\n")

if (!exists("baseline") || !exists("df_ML")) {
  stop("Run data_prepare.R first.")
}

pp <- baseline$pp

# Output directory
out_dir <- "output"
if (!dir.exists(out_dir)) dir.create(out_dir)

# Common theme
theme_article <- theme_minimal(base_size = 12) +
  theme(
    panel.grid.minor = element_blank(),
    plot.title = element_text(face = "bold", size = 13),
    plot.subtitle = element_text(size = 10, color = "grey40"),
    legend.position = "bottom"
  )

# =============================================================================
# FIGURE 1: DURATION DISTRIBUTION (p.8)
# =============================================================================
cat("--- Figure 1: Duration Distribution ---\n")

dur_dist <- baseline$episodes_clean %>%
  mutate(
    dur_group = case_when(
      duration == 1 ~ "1", duration == 2 ~ "2", duration == 3 ~ "3",
      duration == 4 ~ "4", duration == 5 ~ "5",
      duration %in% 6:7 ~ "6-7", duration >= 8 ~ "8+"
    )
  ) %>%
  count(dur_group) %>%
  mutate(
    dur_group = factor(dur_group, levels = c("1","2","3","4","5","6-7","8+")),
    pct = round(100 * n / sum(n), 1)
  )

fig1 <- ggplot(dur_dist, aes(x = dur_group, y = n)) +
  geom_col(fill = "grey40", alpha = 0.8, width = 0.7) +
  geom_text(aes(label = sprintf("%d\n(%.1f%%)", n, pct)),
            vjust = -0.3, size = 3.5, lineheight = 0.9) +
  labs(
    title = "Distribution of Distress Episode Durations",
    x = "Duration (years)", y = "Number of Episodes"
  ) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.15)),
                     breaks = seq(0, 300, 50)) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 12, face = "bold"),
    axis.text = element_text(size = 10),
    axis.title = element_text(size = 11, face = "bold"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(color = "gray30")
  )

ggsave(file.path(out_dir, "Fig1_duration_distribution.png"), fig1,
       width = 7, height = 5, dpi = 300)
cat("  Saved: Fig1_duration_distribution.png\n")

cat(sprintf("  Mean: %.1f, Median: %.0f, Max: %d, Total: %d episodes\n",
            mean(baseline$episodes_clean$duration),
            median(baseline$episodes_clean$duration),
            max(baseline$episodes_clean$duration),
            nrow(baseline$episodes_clean)))

# =============================================================================
# FIGURE 2: EMPIRICAL HAZARD RATES (p.11)
# =============================================================================
cat("\n--- Figure 2: Empirical Hazard Rates ---\n")

hazard_table <- pp %>%
  mutate(t_group = ifelse(t >= 7, 7, t),
         t_label = ifelse(t >= 7, "8+", as.character(t))) %>%
  group_by(t_group) %>%
  summarise(
    N_at_risk = n(), N_exits = sum(event),
    Hazard = N_exits / N_at_risk,
    .groups = "drop"
  )

# Append row 7 (year 7 separately for the bar chart if needed)
fig2_data <- pp %>%
  mutate(t_display = pmin(t, 8)) %>%
  group_by(t_display) %>%
  summarise(
    N_at_risk = n(), N_exits = sum(event),
    Hazard = N_exits / N_at_risk,
    .groups = "drop"
  ) %>%
  mutate(label = ifelse(t_display == 8, "8+", as.character(t_display)))

fig2 <- ggplot(fig2_data, aes(x = factor(label, levels = c(as.character(1:7), "8+")),
                               y = Hazard)) +
  geom_col(fill = "grey30", alpha = 0.8, width = 0.7) +
  geom_text(aes(label = sprintf("%.1f%%", Hazard * 100)),
            vjust = -0.5, size = 3.5) +
  scale_y_continuous(labels = scales::percent_format(),
                     limits = c(0, 0.45)) +
  labs(
    title = "Empirical Hazard Rates: Probability of Exiting Distress",
    subtitle = "Discrete-time hazard h(t) = P(exit at year t | survived to year t)",
    x = "Years in Distress", y = "Hazard Rate"
  ) +
  theme_article

ggsave(file.path(out_dir, "Fig2_hazard_rates.png"), fig2,
       width = 8, height = 5, dpi = 300)
cat("  Saved: Fig2_hazard_rates.png\n")

# =============================================================================
# FIGURE 3: KAPLAN-MEIER BY SECTOR TYPE (p.13)
# =============================================================================
cat("\n--- Figure 3: KM Survival by Sector Type ---\n")

surv_sector <- baseline$episodes_clean %>%
  mutate(sector_type = case_when(
    Sector %in% c("D","E","H") ~ "Regulated",
    Sector %in% c("C","G","F") ~ "Market",
    TRUE ~ NA_character_
  )) %>%
  filter(!is.na(sector_type))

surv_obj <- Surv(surv_sector$duration,
                 as.integer(!surv_sector$right_censored))
km_fit <- survfit(surv_obj ~ surv_sector$sector_type)
lr_test <- survdiff(surv_obj ~ surv_sector$sector_type)

# Extract KM data for ggplot
km_data <- tibble(
  time   = km_fit$time,
  surv   = km_fit$surv,
  lower  = km_fit$lower,
  upper  = km_fit$upper,
  strata = rep(names(km_fit$strata), times = km_fit$strata)
) %>%
  mutate(
    sector_type = ifelse(grepl("Market", strata),
                         "Market-oriented", "Regulated/Infrastructure")
  ) %>%
  filter(time <= 10)

# Prepend time 0
km_data <- bind_rows(
  tibble(time = 0, surv = 1, lower = 1, upper = 1,
         strata = NA, sector_type = "Market-oriented"),
  tibble(time = 0, surv = 1, lower = 1, upper = 1,
         strata = NA, sector_type = "Regulated/Infrastructure"),
  km_data
)

fig3 <- ggplot(km_data, aes(x = time, y = surv,
                             color = sector_type, linetype = sector_type)) +
  geom_step(linewidth = 1.1) +
  geom_ribbon(aes(ymin = lower, ymax = upper, fill = sector_type),
              stat = "identity", alpha = 0.1, color = NA) +
  geom_hline(yintercept = 0.5, linetype = "dashed", color = "grey50") +
  scale_x_continuous(breaks = 0:10) +
  scale_y_continuous(labels = scales::percent_format(), limits = c(0, 1)) +
  scale_color_manual(values = c("Market-oriented" = "grey20",
                                "Regulated/Infrastructure" = "grey55")) +
  scale_fill_manual(values = c("Market-oriented" = "grey20",
                               "Regulated/Infrastructure" = "grey55")) +
  scale_linetype_manual(values = c("Market-oriented" = "solid",
                                   "Regulated/Infrastructure" = "dashed")) +
  labs(
    title = "Kaplan-Meier Survival by Sector Type",
    subtitle = sprintf("Log-rank test: \u03c7\u00b2 = %.1f, p < 0.001", lr_test$chisq),
    x = "Years in Distress",
    y = "Probability of Remaining in Distress",
    color = "Sector Type", fill = "Sector Type", linetype = "Sector Type"
  ) +
  theme_article

ggsave(file.path(out_dir, "Fig3_KM_sector_type.png"), fig3,
       width = 8, height = 5.5, dpi = 300)
cat("  Saved: Fig3_KM_sector_type.png\n")

# =============================================================================
# FIGURE 4: INDIVIDUAL SECTOR HAZARD RATES (p.13)
# =============================================================================
cat("\n--- Figure 4: Sector-Specific Hazard Rates ---\n")

sector_hazards <- pp %>%
  filter(!is.na(Sector)) %>%
  group_by(Sector) %>%
  summarise(
    N_obs = n(), N_events = sum(event),
    Hazard = N_events / N_obs,
    .groups = "drop"
  ) %>%
  filter(N_obs >= 30) %>%
  mutate(
    sector_type = case_when(
      Sector %in% c("D","E","H") ~ "Regulated",
      Sector %in% c("C","G","F") ~ "Market",
      TRUE ~ "Other"
    ),
    Sector_Label = case_when(
      Sector == "A" ~ "A: Agriculture",
      Sector == "C" ~ "C: Manufacturing",
      Sector == "D" ~ "D: Energy",
      Sector == "E" ~ "E: Water/Waste",
      Sector == "F" ~ "F: Construction",
      Sector == "G" ~ "G: Trade",
      Sector == "H" ~ "H: Transport",
      Sector == "J" ~ "J: IT/Comm",
      Sector == "M" ~ "M: Professional",
      TRUE ~ Sector
    )
  ) %>%
  arrange(Hazard)

fig4 <- ggplot(sector_hazards,
               aes(x = reorder(Sector_Label, Hazard),
                   y = Hazard, fill = sector_type)) +
  geom_col(alpha = 0.85, width = 0.7) +
  geom_text(aes(label = sprintf("%.1f%%", Hazard * 100)),
            hjust = -0.2, size = 3.3) +
  coord_flip() +
  scale_fill_manual(values = c("Market" = "grey30",
                               "Regulated" = "grey60",
                               "Other" = "grey80")) +
  scale_y_continuous(labels = scales::percent_format(),
                     limits = c(0, max(sector_hazards$Hazard) * 1.15)) +
  labs(
    title = "Overall Hazard Rate by Sector",
    subtitle = "P(exit distress in any given year)",
    x = NULL, y = "Hazard Rate",
    fill = "Sector Type"
  ) +
  theme_article

ggsave(file.path(out_dir, "Fig4_sector_hazards.png"), fig4,
       width = 9, height = 5, dpi = 300)
cat("  Saved: Fig4_sector_hazards.png\n")

# =============================================================================
# DIAGNOSTIC: LONG EPISODES (7+ years)
# =============================================================================
cat("\n--- Diagnostic: Long Episodes (7+ years) ---\n\n")

long_eps <- baseline$episodes_clean %>%
  filter(duration >= 7) %>%
  arrange(desc(duration)) %>%
  select(mb, start_year, end_year, duration, Size, Sector,
         right_censored, Z_entry, ROA_entry)

cat(sprintf("  Episodes with duration >= 7 years: %d\n", nrow(long_eps)))
if (nrow(long_eps) > 0) {
  print(as.data.frame(long_eps))
}

# =============================================================================
# DIAGNOSTIC: RECIDIVISM CHECK
# =============================================================================
cat("\n--- Diagnostic: Recidivism ---\n\n")

# Firms that exit distress and re-enter within 2 years
# (minimum possible gap between episodes is 2 by construction)
completed_eps <- baseline$episodes_clean %>%
  filter(!right_censored) %>%
  select(mb, end_year) %>%
  distinct()

all_starts <- baseline$episodes_clean %>%
  select(mb, start_year) %>%
  distinct()

recid <- completed_eps %>%
  inner_join(all_starts, by = "mb", relationship = "many-to-many") %>%
  filter(start_year == end_year + 2) %>%  # gap of exactly 2 = re-enter after 1 year out
  distinct(mb, end_year)

cat(sprintf("  Completed episodes: %d\n", nrow(completed_eps)))
cat(sprintf("  Re-enter after exactly 1 year out (gap=2): %d (%.1f%%)\n",
            nrow(recid),
            100 * nrow(recid) / nrow(completed_eps)))

# =============================================================================
# DIAGNOSTIC: Z-SCORE DISTRIBUTION
# =============================================================================
cat("\n--- Diagnostic: Z-Score Distribution in M+L Panel ---\n\n")

z_dist <- df_ML %>%
  filter(!is.na(Risk_Zone)) %>%
  count(Risk_Zone) %>%
  mutate(Pct = round(100 * n / sum(n), 1))

for (i in 1:nrow(z_dist)) {
  cat(sprintf("  %s: %d (%.1f%%)\n", z_dist$Risk_Zone[i], z_dist$n[i], z_dist$Pct[i]))
}

# =============================================================================
# DIAGNOSTIC: MULTI-EPISODE FIRMS
# =============================================================================
cat("\n--- Diagnostic: Multi-Episode Firms ---\n\n")

ep_per_firm <- baseline$episodes_clean %>%
  count(mb, name = "n_episodes") %>%
  count(n_episodes, name = "n_firms")

for (i in 1:nrow(ep_per_firm)) {
  cat(sprintf("  %d episode(s): %d firms\n",
              ep_per_firm$n_episodes[i], ep_per_firm$n_firms[i]))
}

cat(sprintf("\n  Total firms: %d\n", n_distinct(baseline$episodes_clean$mb)))
cat(sprintf("  Multi-episode: %d (%.1f%%)\n",
            sum(ep_per_firm$n_firms[ep_per_firm$n_episodes > 1]),
            100 * sum(ep_per_firm$n_firms[ep_per_firm$n_episodes > 1]) /
              n_distinct(baseline$episodes_clean$mb)))

# =============================================================================
# DIAGNOSTIC: SMALL FIRM HAZARD COMPARISON (for exclusion justification)
# =============================================================================
cat("\n--- Diagnostic: Small vs M+L Hazard at t=5 ---\n\n")

if (exists("sml_ep")) {
  hazard_by_size <- sml_ep$pp %>%
    filter(t == 5) %>%
    mutate(size_group = ifelse(Size %in% c("Medium", "Large"), "M+L", "Small")) %>%
    group_by(size_group) %>%
    summarise(
      N = n(), exits = sum(event),
      hazard = round(exits / N, 3),
      .groups = "drop"
    )
  print(as.data.frame(hazard_by_size))
  cat("  (Used in Data section to justify small firm exclusion)\n")
}

# =============================================================================
cat("\n=======================================================================\n")
cat("FIGURES AND CHECKS COMPLETE\n")
cat(sprintf("Output directory: %s/\n", out_dir))
cat("  Fig1_duration_distribution.png  (paper Figure 1)\n")
cat("  Fig2_hazard_rates.png           (paper Figure 2)\n")
cat("  Fig3_KM_sector_type.png         (paper Figure 3)\n")
cat("  Fig4_sector_hazards.png         (paper Figure 4)\n")
cat("=======================================================================\n")
