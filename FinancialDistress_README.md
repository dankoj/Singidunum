# Duration Dependence and Sectoral Heterogeneity in Financial Distress

Replication code for:

> Duration Dependence and Sectoral Heterogeneity in Financial Distress: Evidence from Serbian Medium and Large Enterprises, 2013–2023. *The European Journal of Applied Economics*.

## Requirements

### Software

- R 4.3+ (developed on R 4.4.x)

### R Packages

```r
install.packages(c(
  "dplyr", "tidyr", "readxl",   # Data manipulation
  "skimr",                       # Summary statistics
  "survival",                    # Kaplan-Meier, Cox PH, frailty models
  "sandwich", "lmtest",          # Clustered standard errors
  "ggplot2", "scales"            # Figures
))
```

### Data Files

The following files must be placed in the working directory:

| File | Description |
|------|-------------|
| `data.xlsx` | Raw APR financial statement data (2013–2023) |
| `AOP_Mapping.xlsx` | AOP code harmonisation mapping (old ↔ new format) |
| `MB_Delatnost.xlsx` | NACE Rev. 2 sector classification by firm |
| `subsidiaries.xlsx` | Corporate group structure |

Raw data are available at: [MendeleyData link to be added]

## Execution Order

The scripts must be run in sequence. Each script sources or depends on the output of the previous one.

```
data_prepare.R          →  article_analysis.R  →  figures_and_checks.R
(data pipeline +           (all tables and         (Figures 1–4 and
 episode construction)      statistical tests)      diagnostic checks)
```

### Step 1: `data_prepare.R`

Sources the general data pipeline (`01_load_data.R`, `02_classify_companies.R`, `03_prepare_data.R`), then builds M+L and S+M+L analysis panels, distress episodes, and person-period datasets for hazard modelling.

**Output objects:** `df_ML`, `df_SML`, `gdp_growth`, `baseline`, `alt_def`, `sml_ep`, `baseline_lc`, `strict_ep`

### Step 2: `article_analysis.R`

Produces every number, table, and statistical test reported in the manuscript. Includes a verification report at the end comparing computed values against expected manuscript values.

**Output:** Tables 1–7, Tables A1–A2, all in-text statistics (printed to console).

### Step 3: `figures_and_checks.R`

Generates all manuscript figures and runs diagnostic checks (long episodes, recidivism, multi-episode firms, small-firm hazard comparison).

**Output:** `output/Fig1_duration_distribution.png` through `output/Fig4_sector_hazards.png`

## Mapping: Code Output → Manuscript Tables

| Code section | Manuscript |
|---|---|
| Table 1: Sample Selection | Table 1 |
| Table A1: Episode Construction | Table A1 (Appendix) |
| Table A2: Financial Profile at Entry | Table A2 (Appendix) |
| Table 2: Ownership Distribution | Table 2 |
| Table 3: Empirical Hazard Rates | Table 3 |
| Table 4: Baseline Hazard Model | Table 4 |
| Table 5: Sectoral Comparison | Table 5 |
| Table 6: Ownership Confounding | Table 6 |
| Table 7: Robustness Tests | Table 7 |

## Key Design Decisions

- **Distress definition:** Altman Z″ < 1.1 (Altman, 2000), with alternative accounting-based definition as robustness check.
- **Link function:** Complementary log-log (cloglog), corresponding to continuous-time proportional hazards observed at discrete intervals.
- **Standard errors:** Clustered at the firm level throughout (120 firms contribute multiple episodes).
- **Left-censoring:** Episodes starting in 2013 excluded from the primary analysis (included as robustness check).
- **Exit definition:** Baseline requires one year of Z″ ≥ 1.1; strict variant requires two consecutive years.
- **DE_entry excluded** from primary specification (12% missing, p = 0.438 when included); tested as robustness check.

## Licence

Code: MIT. Data: subject to APR terms of use.
