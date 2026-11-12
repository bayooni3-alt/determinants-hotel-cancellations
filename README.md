# Determinants of Hotel Booking Cancellations — Replication Package
**Author:** Adebayo  
**Version:** v8 (final, manuscript-aligned)  
**Date:** 2026-11-12  
**Tested on:** R 4.5.3 (Windows 11 x64)
---
## 1. What this is
Replication package for the manuscript:
> Adebayo (2026). *Determinants of Hotel Booking Cancellations: A Multi-Method Statistical Analysis.* Manuscript under review.
The script fits a multi-method pipeline to a **1,000-row sample** of the Hotel Booking Demand dataset (Antonio, Almeida & Nunes, 2019), yielding **n = 985** after data cleaning. It reproduces every number in the manuscript: non-parametric tests, logistic regression, Firth penalization, machine learning, bootstrap, propensity-score matching, and multilevel modeling.
---
## 2. Repository structure
```text
.
├── README.md
├── hotel_cancellation_analysis.R      # main script (v8)
├── hotel_bookings_full.csv            # OPTIONAL — full Kaggle file (enables sampling)
├── hotel_bookings.csv                 # fallback — pre-subsetted 1,000-row file
├── outputs/                           # auto-created: all tables (CSV + HTML)
└── figures/                           # auto-created: all figures (PNG, 300 dpi)
3. How to reproduce
Prerequisites: R ≥ 4.0. Data: Hotel Booking Demand dataset.
    Place the full Kaggle file as hotel_bookings_full.csv in the project root (preferred — enables the reproducible 1,000-row sample). If unavailable, the script falls back to a pre-subsetted hotel_bookings.csv (no sampling).
    Set the working directory to the project root.
    Run:
source("hotel_cancellation_analysis.R")
    Expect the closing banner ANALYSIS COMPLETE, all tables in outputs/, and all figures in figures/.

Runtime: ~4–6 minutes (multilevel glmer + XGBoost CV dominate).
Reproducibility: set.seed(20250101) at the top guarantees identical output on every run.
4. Table → manuscript mapping
File	Manuscript table
table_01_descriptives	Table 1
table_02_wilcoxon	Table 2
table_03_chisq	Table 3
table_04_cramers_v	Table 4
table_05_logistic_or_key	Table 5
table_06_fit_stats	Table 6
table_06b_hoslem	Table 6 footnote (HL suppressed)
table_07_firth	Table 7
table_08_ml_auc	Table 8
table_09_bootstrap	Table 9
table_10_multilevel	Table 10
table_11_discriminatory_power	Table 11
table_13_headline_findings	Consolidated Headline Findings
appendix_A_psm	Appendix A
5. Key methodological notes
    Quasi-complete separation in deposit_type = Non Refund and required_car_parking_spaces:
        Ordinary maximum-likelihood estimates are not interpretable (exploded ORs, SE > 1,000).
        Firth penalization (Table 7) is the defensible estimate.
        Hosmer–Lemeshow is suppressed — see table_06b_hoslem.csv for the exact rationale.
    Sampling: 1,000-row simple random sample with set.seed(20250101) if hotel_bookings_full.csv is present; otherwise the pre-subsetted file is used as-is.
    Cleaning filters: zero guests, zero nights, non-positive ADR → 1,000 → 997 → 995 → 985.
    Leakage control: reservation_status, reservation_status_date, and arrival_date are dropped before modeling.
Common issues and fixes
Issue	Fix
Place hotel_bookings_full.csv …	Put the data file in the project root
Full Kaggle file not found — using pre-subsetted …	Expected if no full file; update manuscript §4.1 accordingly
fitted probabilities numerically 0 or 1	Expected — separation; the Firth estimate is authoritative
Model is nearly unidentifiable (glmer)	Harmless; already handled by the bobyqa optimizer
SHAP skipped	Cosmetic; not needed for manuscript tables
geom_errorbarh() was deprecated	Cosmetic; forest plot still renders
7. Citation
Manuscript:
    Adebayo (2026). Determinants of Hotel Booking Cancellations: A Multi-Method Statistical Analysis. Manuscript under review.
Dataset:
    Antonio, N., de Almeida, A., & Nunes, L. (2019). Hotel booking demand datasets. Data in Brief, 22, 41–49. https://doi.org/10.1016/j.dib.2018.11.126
8. License
    Code: MIT
    Manuscript: CC-BY 4.0
    Data: original Kaggle license (Hotel Booking Demand dataset)
9. Contact
Adebayo Oni Patrick — patrick.adebayo@funato.edu.ng · Federal University of Agriculture and Technology, Okeho
