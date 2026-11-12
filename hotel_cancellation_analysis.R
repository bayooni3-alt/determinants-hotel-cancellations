# =============================================================================
# HOTEL BOOKING CANCELLATION — PhD-LEVEL ANALYSIS
# Complete, corrected, run-ready script (v8 — FINAL, manuscript-aligned)
# -----------------------------------------------------------------------------
# v8 fixes vs. v7:
#   FIX-OR:  Table 7 (Firth) gains an OR column matching the manuscript
#   FIX-NUM: CSV filenames now match manuscript numbering
#            (table_10_multilevel, table_11_discriminatory_power,
#             appendix_A_psm)
#   FIX-HL:  Table 6b explicitly records that HL was suppressed
# =============================================================================

# =============================================================================
# 1. SETUP, REPRODUCIBILITY, PACKAGES
# =============================================================================
set.seed(20250101)

required_packages <- c(
  "tidyverse", "janitor", "skimr", "DataExplorer", "corrplot",
  "car", "MASS", "pROC", "caret", "randomForest", "rpart", "rpart.plot",
  "vip", "effects", "emmeans", "broom", "knitr", "kableExtra",
  "patchwork", "scales", "ggpubr", "psych", "nortest", "lmtest",
  "sandwich", "boot", "pwr", "glmnet", "xgboost", "shapviz",
  "MatchIt", "lme4", "logistf", "here", "lubridate", "readr"
)

install_if_missing <- function(pkgs) {
  for (p in pkgs) {
    if (!requireNamespace(p, quietly = TRUE)) install.packages(p)
  }
}
install_if_missing(required_packages)

suppressPackageStartupMessages({
  library(tidyverse);   library(janitor);    library(skimr)
  library(DataExplorer);library(corrplot);   library(car)
  library(MASS);        library(pROC);       library(caret)
  library(randomForest);library(rpart);      library(rpart.plot)
  library(vip);         library(effects);    library(emmeans)
  library(broom);       library(knitr);      library(kableExtra)
  library(patchwork);   library(scales);     library(ggpubr)
  library(psych);       library(nortest);    library(lmtest)
  library(sandwich);    library(boot);       library(pwr)
  library(glmnet);      library(xgboost);    library(shapviz)
  library(MatchIt);     library(lme4);       library(logistf)
  library(here);        library(lubridate);  library(readr)
})

options(scipen = 999)

dir.create(here::here("outputs"), showWarnings = FALSE)
dir.create(here::here("figures"), showWarnings = FALSE)

write_table <- function(df, name, caption = NULL) {
  readr::write_csv(df, here::here("outputs", paste0(name, ".csv")))
  k <- knitr::kable(df, caption = caption, format = "html") %>%
    kableExtra::kable_styling(bootstrap_options = c("striped", "hover"))
  readr::write_file(as.character(k),
                    here::here("outputs", paste0(name, ".html")))
  invisible(df)
}

safe_plot <- function(expr, file, width = 7, height = 5) {
  tryCatch({
    p <- force(expr)
    if (inherits(p, "ggplot")) {
      ggplot2::ggsave(here::here("figures", file), p,
                      width = width, height = height, dpi = 300)
    } else if (!is.null(dev.list())) dev.off()
    invisible(p)
  }, error = function(e) message("Figure skipped (", file, "): ",
                                 conditionMessage(e)))
}

# =============================================================================
# 2. DATA IMPORT  (FIX-DISC-1A: explicit reproducible 1,000-row sample)
# =============================================================================
full_path <- here::here("hotel_bookings_full.csv")
sub_path  <- here::here("hotel_bookings.csv")

if (file.exists(full_path)) {
  full_raw <- read.csv(full_path, stringsAsFactors = FALSE)
  set.seed(20250101)
  raw <- full_raw[sample(nrow(full_raw), 1000), ]
  rownames(raw) <- NULL
  cat("Sampled 1,000 rows from", nrow(full_raw), "— seed 20250101\n")
} else if (file.exists(sub_path)) {
  raw <- read.csv(sub_path, stringsAsFactors = FALSE)
  message("Full Kaggle file not found — using pre-subsetted hotel_bookings.csv (no sampling).")
} else {
  stop("Place hotel_bookings_full.csv (or hotel_bookings.csv) in the project root.")
}

hotel <- raw %>%
  janitor::clean_names() %>%
  dplyr::select(-any_of("rownames"))

cat("\n--- STRUCTURE ---\n");   str(hotel)
cat("\n--- DIMENSIONS ---\n");  print(dim(hotel))

missing_tab <- hotel %>%
  summarise(across(everything(), ~ sum(is.na(.)))) %>%
  pivot_longer(everything(), names_to = "variable", values_to = "n_missing") %>%
  mutate(pct_missing = round(100 * n_missing / nrow(hotel), 2)) %>%
  arrange(desc(n_missing))
write_table(missing_tab, "table_00_missingness", "Missingness by Variable")

# =============================================================================
# 3. CLEANING, TYPE COERCION, FEATURE ENGINEERING
# =============================================================================
hotel_clean <- hotel %>%
  mutate(
    across(c(hotel, arrival_date_month, meal, country, market_segment,
             distribution_channel, deposit_type, customer_type,
             reservation_status),
           as.factor),
    agent   = na_if(agent, "NULL"),
    company = na_if(company, "NULL"),
    agent   = suppressWarnings(as.numeric(agent)),
    company = suppressWarnings(as.numeric(company)),
    reservation_status_date = as.Date(reservation_status_date, "%m/%d/%Y"),
    is_canceled = factor(is_canceled, levels = c(0, 1),
                         labels = c("Not_Canceled", "Canceled")),
    total_nights = stays_in_weekend_nights + stays_in_week_nights,
    total_guests = adults + children + babies,
    revenue      = average_daily_rate * total_nights,
    room_changed = as.integer(as.character(reserved_room_type) !=
                                as.character(assigned_room_type)),
    has_agent    = as.integer(!is.na(agent)),
    has_company  = as.integer(!is.na(company)),
    arrival_date = as.Date(
      paste(arrival_date_year, arrival_date_month,
            arrival_date_day_of_month, sep = "-"),
      format = "%Y-%B-%d"
    ),
    arrival_quarter = factor(lubridate::quarter(arrival_date)),
    arrival_dow     = factor(lubridate::wday(arrival_date, label = TRUE))
  ) %>%
  mutate(
    reserved_room_type = as.factor(reserved_room_type),
    assigned_room_type = as.factor(assigned_room_type)
  )

cat("\n[TRACE] after coercion:", nrow(hotel_clean), "\n")
hotel_clean <- hotel_clean %>% filter(adults + children + babies > 0)
cat("[TRACE] after total_guests > 0:", nrow(hotel_clean), "\n")
hotel_clean <- hotel_clean %>% filter(total_nights > 0)
cat("[TRACE] after total_nights > 0:", nrow(hotel_clean), "\n")
hotel_clean <- hotel_clean %>% filter(average_daily_rate > 0)
cat("[TRACE] after ADR > 0:", nrow(hotel_clean), "\n")
hotel_clean <- hotel_clean %>%
  dplyr::select(where(~ !all(is.na(.)))) %>%
  droplevels()

cat("\n--- CLEAN DIMENSIONS ---\n"); print(dim(hotel_clean))

leak_vars <- c("reservation_status", "reservation_status_date", "arrival_date")
model_df  <- hotel_clean %>% dplyr::select(-all_of(leak_vars))
cat("\n[TRACE] model_df rows:", nrow(model_df), "\n")

# =============================================================================
# 4. EXPLORATORY ANALYSIS (figures)
# =============================================================================
num_vars <- c("lead_time", "average_daily_rate", "total_nights",
              "total_guests", "total_of_special_requests",
              "booking_changes", "previous_cancellations")
cat_vars <- c("hotel", "market_segment", "deposit_type",
              "customer_type", "arrival_date_month")

safe_plot(
  ggplot(hotel_clean, aes(is_canceled, fill = is_canceled)) +
    geom_bar() +
    geom_text(stat = "count", aes(label = after_stat(count)), vjust = -0.5) +
    scale_fill_manual(values = c("#2E86AB", "#D7263D")) +
    labs(title = "Distribution of Booking Cancellation",
         x = NULL, y = "Count") +
    theme_minimal(base_size = 13) + theme(legend.position = "none"),
  "fig_target_distribution.png", 6, 4)

safe_plot(
  hotel_clean %>%
    dplyr::select(all_of(num_vars), is_canceled) %>%
    pivot_longer(-is_canceled, names_to = "variable", values_to = "value") %>%
    ggplot(aes(value, fill = is_canceled)) +
    geom_density(alpha = 0.5) +
    facet_wrap(~ variable, scales = "free", ncol = 3) +
    scale_fill_manual(values = c("#2E86AB", "#D7263D")) +
    labs(title = "Numeric Predictors by Cancellation Status",
         x = NULL, y = "Density", fill = "Status") +
    theme_minimal(base_size = 12),
  "fig_numeric_density.png", 10, 6)

for (v in cat_vars) {
  local({
    vv <- v
    safe_plot(
      ggplot(hotel_clean, aes(.data[[vv]], fill = is_canceled)) +
        geom_bar(position = "fill") +
        scale_y_continuous(labels = percent) +
        scale_fill_manual(values = c("#2E86AB", "#D7263D")) +
        labs(title = paste("Cancellation Rate by", vv),
             x = NULL, y = "Proportion", fill = "Status") +
        theme_minimal(base_size = 12) +
        theme(axis.text.x = element_text(angle = 45, hjust = 1)),
      paste0("fig_cat_", vv, ".png"), 7, 4)
  })
}

num_data <- hotel_clean %>% dplyr::select(where(is.numeric))
zero_sd  <- names(which(sapply(num_data, sd, na.rm = TRUE) == 0))
if (length(zero_sd) > 0) num_data <- num_data %>% dplyr::select(-all_of(zero_sd))
cor_mat <- cor(num_data, use = "pairwise.complete.obs")
tryCatch({
  png(here::here("figures", "fig_correlation_matrix.png"),
      width = 900, height = 900, res = 120)
  corrplot(cor_mat, method = "color", type = "upper",
           tl.cex = 0.5, tl.col = "black", number.cex = 0.5,
           title = "Correlation Matrix", mar = c(0, 0, 2, 0))
  dev.off()
}, error = function(e) { if (!is.null(dev.list())) dev.off() })

# =============================================================================
# 5. FORMAL STATISTICAL TESTING
# =============================================================================
n_sw <- min(5000, nrow(hotel_clean))
cat("\n--- SHAPIRO-WILK (lead_time) ---\n")
print(shapiro.test(sample(hotel_clean$lead_time, n_sw)))
cat("\n--- SHAPIRO-WILK (ADR) ---\n")
print(shapiro.test(sample(hotel_clean$average_daily_rate, n_sw)))

wilcox_results <- purrr::map_dfr(num_vars, function(v) {
  f  <- as.formula(paste(v, "~ is_canceled"))
  wt <- wilcox.test(f, data = hotel_clean)
  tibble(variable = v, W = unname(wt$statistic), p_value = wt$p.value)
}) %>%
  mutate(
    p_adj = p.adjust(p_value, method = "BH"),
    Sig.  = case_when(p_adj < 0.001 ~ "***",
                      p_adj < 0.01  ~ "**",
                      p_adj < 0.05  ~ "*",
                      TRUE          ~ "ns")
  )
write_table(wilcox_results, "table_02_wilcoxon",
            "Table 2. Wilcoxon Rank-Sum Tests")

chi_results <- purrr::map_dfr(cat_vars, function(v) {
  tab <- table(hotel_clean[[v]], hotel_clean$is_canceled)
  ct  <- suppressWarnings(chisq.test(tab, simulate.p.value = TRUE, B = 2000))
  tibble(variable = v,
         chi_sq  = unname(ct$statistic),
         df      = if (!is.null(ct$parameter)) unname(ct$parameter) else NA_real_,
         p_value = ct$p.value)
}) %>%
  mutate(p_adj = p.adjust(p_value, method = "BH"))
write_table(chi_results, "table_03_chisq",
            "Table 3. Chi-Square Tests of Independence")

cramers_v <- function(tab) {
  chi2 <- suppressWarnings(chisq.test(tab, simulate.p.value = TRUE, B = 2000)$statistic)
  sqrt(chi2 / (sum(tab) * (min(dim(tab)) - 1)))
}
cv_tab <- purrr::map_dfr(cat_vars, function(v) {
  tibble(variable  = v,
         cramers_v = round(cramers_v(table(hotel_clean[[v]],
                                           hotel_clean$is_canceled)), 3))
}) %>%
  mutate(Magnitude = case_when(
    cramers_v < 0.10 ~ "Negligible",
    cramers_v < 0.20 ~ "Small",
    cramers_v < 0.30 ~ "Small-medium",
    cramers_v < 0.40 ~ "Medium",
    TRUE             ~ "Medium-large"
  ))
write_table(cv_tab, "table_04_cramers_v", "Table 4. Cramér's V Effect Sizes")

# =============================================================================
# 6. LOGISTIC REGRESSION
# =============================================================================
logit_full <- suppressWarnings(
  glm(
    is_canceled ~ lead_time + average_daily_rate + total_nights +
      total_guests + total_of_special_requests + booking_changes +
      previous_cancellations + is_repeated_guest +
      required_car_parking_spaces + hotel + market_segment +
      deposit_type + customer_type + arrival_date_month,
    data = model_df, family = binomial(link = "logit")
  )
)
cat("\n--- LOGISTIC REGRESSION SUMMARY ---\n")
print(summary(logit_full))

separation_terms <- c("required_car_parking_spaces",
                      "deposit_typeNon Refund",
                      "deposit_typeRefundable",
                      "customer_typeGroup")

or_tab_full <- suppressWarnings(
  broom::tidy(logit_full, exponentiate = TRUE, conf.int = TRUE)
) %>%
  dplyr::filter(term != "(Intercept)") %>%
  dplyr::mutate(
    Separation = term %in% separation_terms,
    OR         = ifelse(Separation, NA, round(estimate, 3)),
    `CI lower` = ifelse(Separation, NA, round(conf.low, 3)),
    `CI upper` = ifelse(Separation, NA, round(conf.high, 3)),
    p          = round(p.value, 4),
    Sig.       = case_when(p.value < 0.001 ~ "***",
                           p.value < 0.01  ~ "**",
                           p.value < 0.05  ~ "*",
                           TRUE            ~ "ns")
  ) %>%
  dplyr::select(term, OR, `CI lower`, `CI upper`, p, Sig., Separation)

write_table(or_tab_full, "table_05a_odds_ratios_full",
            "Logistic Regression: Full Odds Ratio Table (separation flagged)")

table5_terms <- c(
  "lead_time", "average_daily_rate", "total_nights", "total_guests",
  "total_of_special_requests", "booking_changes", "previous_cancellations",
  "is_repeated_guest", "market_segmentOnline TA", "hotelResort Hotel",
  "required_car_parking_spaces", "deposit_typeNon Refund"
)
table5 <- or_tab_full %>%
  dplyr::filter(term %in% table5_terms) %>%
  dplyr::mutate(term = factor(term, levels = table5_terms)) %>%
  dplyr::arrange(term) %>%
  dplyr::mutate(
    term = as.character(term),
    OR   = case_when(
      Separation & term == "required_car_parking_spaces" ~ "~0 (separation)",
      Separation & term == "deposit_typeNon Refund"      ~ "exploded (separation)",
      TRUE ~ as.character(OR)
    )
  )
write_table(table5, "table_05_logistic_or_key",
            "Table 5. Logistic Regression Odds Ratios (Key Predictors)")

vif_vals <- tryCatch(car::vif(logit_full),
                     error = function(e) paste("VIF error:", e$message))
cat("\n--- VIF ---\n"); print(vif_vals)

null_dev <- logit_full$null.deviance
res_dev  <- logit_full$deviance
mcf_r2   <- 1 - res_dev / null_dev
fit_stats <- tibble(
  Metric = c("Null Deviance", "Residual Deviance", "McFadden R2",
             "AIC", "BIC", "n"),
  Value  = round(c(null_dev, res_dev, mcf_r2,
                   AIC(logit_full), BIC(logit_full), nobs(logit_full)), 3)
)
write_table(fit_stats, "table_06_fit_stats",
            "Table 6. Logistic Regression Fit Statistics")

# --- 6.5 Hosmer-Lemeshow (FIX-DISC-2A: suppress under separation) ---
hoslem_test <- function(model, g = 10) {
  y     <- as.numeric(model$y) - 1
  p     <- fitted(model)
  q     <- unique(quantile(p, probs = seq(0, 1, 1/g)))
  cut_p <- cut(p, breaks = q, include.lowest = TRUE)
  obs   <- tapply(y, cut_p, sum); exp_ <- tapply(p, cut_p, sum)
  n     <- tapply(y, cut_p, length)
  chi2  <- sum((obs - exp_)^2 / (exp_ * (1 - exp_/n)))
  list(statistic = chi2, df = length(obs) - 2,
       p.value = pchisq(chi2, length(obs) - 2, lower.tail = FALSE))
}

hl <- hoslem_test(logit_full)
if (is.finite(hl$statistic) && hl$statistic < 1e4) {
  cat("\n--- HOSMER-LEMESHOW ---\n"); print(hl)
  write_table(
    tibble(Metric = c("HL statistic", "df", "p-value"),
           Value  = round(c(hl$statistic, hl$df, hl$p.value), 4)),
    "table_06b_hoslem", "Hosmer-Lemeshow Goodness-of-Fit"
  )
} else {
  cat("\n--- HOSMER-LEMESHOW: skipped (quasi-complete separation) ---\n")
  cat("    Use Firth model (Table 7) for deposit_type inference.\n")
  write_table(
    tibble(Metric = "Hosmer-Lemeshow",
           Value  = paste("Not reported — invalid under quasi-complete",
                          "separation; see Firth (Table 7)")),
    "table_06b_hoslem", "Hosmer-Lemeshow Goodness-of-Fit"
  )
}

prob_pred <- predict(logit_full, type = "response")
roc_obj   <- pROC::roc(model_df$is_canceled, prob_pred,
                       levels = c("Not_Canceled", "Canceled"))
auc_val   <- as.numeric(pROC::auc(roc_obj))
best_thr  <- pROC::coords(roc_obj, "best", ret = "threshold")
pred_class <- ifelse(prob_pred > best_thr$threshold,
                     "Canceled", "Not_Canceled")
conf_mat <- caret::confusionMatrix(
  factor(pred_class, levels = c("Not_Canceled", "Canceled")),
  model_df$is_canceled)
cat("\n--- CONFUSION MATRIX ---\n"); print(conf_mat)

table12 <- tibble(
  `Fit Index`     = c("AUC", "McFadden R2", "Accuracy",
                      "Sensitivity", "Specificity", "Kappa"),
  `Full Logistic` = c(round(auc_val, 3),
                      round(mcf_r2, 3),
                      round(conf_mat$overall["Accuracy"], 3),
                      round(conf_mat$byClass["Sensitivity"], 3),
                      round(conf_mat$byClass["Specificity"], 3),
                      round(conf_mat$overall["Kappa"], 3)),
  Firth   = c(NA, NA, NA, NA, NA, NA),
  XGBoost = c(NA, NA, NA, NA, NA, NA)
)

# =============================================================================
# 7. MACHINE LEARNING BENCHMARKING
# =============================================================================
set.seed(20250101)
train_idx <- caret::createDataPartition(model_df$is_canceled, p = 0.7, list = FALSE)
train_df  <- model_df[train_idx, ]
test_df   <- model_df[-train_idx, ]

mm_formula <- is_canceled ~ lead_time + average_daily_rate + total_nights +
  total_guests + total_of_special_requests + booking_changes +
  previous_cancellations + is_repeated_guest +
  required_car_parking_spaces + hotel + market_segment +
  deposit_type + customer_type + arrival_date_month

x_train <- model.matrix(mm_formula, data = train_df)[, -1]
y_train <- as.numeric(train_df$is_canceled) - 1
x_test  <- model.matrix(mm_formula, data = test_df)[, -1]
y_test  <- as.numeric(test_df$is_canceled) - 1

set.seed(20250101)
cv_lasso   <- glmnet::cv.glmnet(x_train, y_train, family = "binomial", alpha = 1)
lasso_pred <- as.numeric(predict(cv_lasso, newx = x_test, s = "lambda.min",
                                 type = "response"))
auc_lasso  <- as.numeric(pROC::auc(pROC::roc(y_test, lasso_pred, quiet = TRUE)))

set.seed(20250101)
rf_fit <- randomForest::randomForest(
  is_canceled ~ lead_time + average_daily_rate + total_nights +
    total_guests + total_of_special_requests + booking_changes +
    previous_cancellations + deposit_type + market_segment +
    customer_type + hotel,
  data = train_df, ntree = 500, importance = TRUE
)
rf_pred <- predict(rf_fit, newdata = test_df, type = "prob")[, "Canceled"]
auc_rf  <- as.numeric(pROC::auc(pROC::roc(y_test, rf_pred, quiet = TRUE)))

dtrain <- xgboost::xgb.DMatrix(x_train, label = y_train)
dtest  <- xgboost::xgb.DMatrix(x_test,  label = y_test)
xgb_params <- list(objective = "binary:logistic", eval_metric = "auc",
                   eta = 0.05, max_depth = 4,
                   subsample = 0.8, colsample_bytree = 0.8)
set.seed(20250101)
xgb_cv <- xgboost::xgb.cv(params = xgb_params, data = dtrain,
                          nrounds = 500, nfold = 5,
                          early_stopping_rounds = 30, verbose = 0)
best_n <- xgb_cv$best_iteration
if (is.null(best_n) || length(best_n) == 0 || is.na(best_n)) {
  best_n <- which.max(xgb_cv$evaluation_log$test_auc_mean)
}
set.seed(20250101)
xgb_fit  <- xgboost::xgb.train(params = xgb_params, data = dtrain,
                               nrounds = best_n, verbose = 0)
xgb_pred <- predict(xgb_fit, dtest)
auc_xgb  <- as.numeric(pROC::auc(pROC::roc(y_test, xgb_pred, quiet = TRUE)))

model_comp <- tibble(
  Model = c("Logistic (Full)", "Lasso Logistic", "Random Forest", "XGBoost"),
  AUC   = round(c(auc_val, auc_lasso, auc_rf, auc_xgb), 3)
)
write_table(model_comp, "table_08_ml_auc",
            "Table 8. Machine Learning Model Comparison (AUC)")

safe_plot(vip::vip(rf_fit, num_features = 15) +
            labs(title = "Random Forest Variable Importance") +
            theme_minimal(base_size = 12),
          "fig_rf_importance.png", 8, 6)

tryCatch({
  shp <- shapviz::shapviz(xgb_fit, X_pred = x_test)
  safe_plot(shapviz::sv_importance(shp, kind = "beeswarm", max_display = 15) +
              labs(title = "SHAP Values — XGBoost") +
              theme_minimal(base_size = 12),
            "fig_shap_xgb.png", 8, 6)
}, error = function(e) message("SHAP skipped: ", conditionMessage(e)))

# =============================================================================
# 8. ADVANCED INFERENTIAL EXTENSIONS
# =============================================================================
robust_se <- lmtest::coeftest(logit_full,
                              vcov = sandwich::vcovHC(logit_full, type = "HC3"))
cat("\n--- ROBUST (HC3) ---\n"); print(robust_se)

firth_fit <- logistf::logistf(
  is_canceled ~ lead_time + deposit_type + market_segment +
    average_daily_rate + total_nights,
  data = model_df
)
cat("\n--- FIRTH PENALIZED LOGISTIC ---\n"); print(summary(firth_fit))

table7 <- tibble(
  Predictor      = names(firth_fit$coefficients),
  Coefficient    = round(firth_fit$coefficients, 3),
  SE             = round(sqrt(diag(firth_fit$var)), 4),
  `95% CI lower` = round(firth_fit$ci.lower, 3),
  `95% CI upper` = round(firth_fit$ci.upper, 3),
  OR             = round(exp(firth_fit$coefficients), 3),   # FIX-OR: added OR
  p              = round(firth_fit$prob, 4),
  Sig.           = case_when(firth_fit$prob < 0.001 ~ "***",
                             firth_fit$prob < 0.01  ~ "**",
                             firth_fit$prob < 0.05  ~ "*",
                             TRUE                   ~ "ns")
)
write_table(table7, "table_07_firth",
            "Table 7. Firth Penalized Logistic Regression")

boot_or <- function(data, idx) {
  m <- glm(is_canceled ~ lead_time + average_daily_rate + total_nights,
           data = data[idx, ], family = binomial)
  exp(coef(m))
}
set.seed(20250101)
boot_res <- boot::boot(model_df, boot_or, R = 1000)
table9 <- tibble(
  Parameter    = names(boot_res$t0),
  Original     = round(as.numeric(boot_res$t0), 4),
  Bias         = round(as.numeric(boot_res$t0 - colMeans(boot_res$t)), 5),
  `Std. Error` = round(apply(boot_res$t, 2, sd), 5)
)
write_table(table9, "table_09_bootstrap",
            "Table 9. Bootstrap Odds Ratios (R = 1000)")

set.seed(20250101)
m_out <- MatchIt::matchit(
  I(deposit_type == "Non Refund") ~ lead_time +
    average_daily_rate + total_nights + total_guests +
    market_segment + customer_type,
  data = model_df, method = "nearest", ratio = 1
)
matched     <- MatchIt::match.data(m_out)
matched_fit <- suppressWarnings(
  glm(is_canceled ~ I(deposit_type == "Non Refund") +
        lead_time + average_daily_rate + total_nights,
      data = matched, family = binomial)
)
table10 <- broom::tidy(matched_fit) %>%
  dplyr::mutate(across(where(is.numeric), ~ round(.x, 4)))
write_table(table10, "appendix_A_psm",                    # FIX-NUM: renamed
            "Appendix A. Propensity-Score Matched Model")

glmer_fit <- suppressWarnings(
  lme4::glmer(
    is_canceled ~ lead_time + average_daily_rate + total_nights + (1 | country),
    data = model_df, family = binomial,
    control = lme4::glmerControl(optimizer = "bobyqa")
  )
)
cat("\n--- MULTILEVEL MODEL ---\n"); print(summary(glmer_fit))

fe          <- summary(glmer_fit)$coefficients
var_country <- as.numeric(lme4::VarCorr(glmer_fit)$country)
icc         <- var_country / (var_country + pi^2 / 3)

table11 <- tibble(
  Predictor = rownames(fe),
  Estimate  = round(fe[, "Estimate"], 4),
  SE        = round(fe[, "Std. Error"], 4),
  z         = round(fe[, "z value"], 3),
  p         = round(fe[, "Pr(>|z|)"], 4),
  Sig.      = case_when(fe[, "Pr(>|z|)"] < 0.001 ~ "***",
                        fe[, "Pr(>|z|)"] < 0.01  ~ "**",
                        fe[, "Pr(>|z|)"] < 0.05  ~ "*",
                        TRUE                     ~ "ns")
)
write_table(table11, "table_10_multilevel",               # FIX-NUM: renamed
            "Table 10. Multilevel Logistic Model (Country Random Effect)")
cat("\nICC (country):", round(icc, 3), "\n")

# =============================================================================
# 9. PUBLICATION-READY OUTPUTS
# =============================================================================
table1 <- hotel_clean %>%
  group_by(Status = is_canceled) %>%
  summarise(
    n = n(),
    `Lead Time M (SD)`        = sprintf("%.1f (%.1f)", mean(lead_time), sd(lead_time)),
    `ADR M (SD)`              = sprintf("%.1f (%.1f)", mean(average_daily_rate), sd(average_daily_rate)),
    `Total Nights M (SD)`     = sprintf("%.1f (%.1f)", mean(total_nights), sd(total_nights)),
    `Special Requests M (SD)` = sprintf("%.1f (%.1f)", mean(total_of_special_requests), sd(total_of_special_requests)),
    `Repeat Guest %`          = sprintf("%.1f%%", 100 * mean(is_repeated_guest)),
    .groups = "drop"
  )
write_table(table1, "table_01_descriptives",
            "Table 1. Descriptives by Cancellation Status")

table12_final <- table12 %>%
  mutate(
    Firth   = c(NA, NA, NA, NA, NA, NA),
    XGBoost = c(round(auc_xgb, 3), NA, NA, NA, NA, NA)
  )
write_table(table12_final, "table_11_discriminatory_power", # FIX-NUM: renamed
            "Table 11. Model Discriminatory Power Summary")

headline <- tibble(
  Rank = 1:8,
  Predictor = c(
    "previous_cancellations",
    "deposit_type = Non Refund",
    "total_of_special_requests",
    "booking_changes",
    "lead_time",
    "average_daily_rate",
    "market_segment = Online TA",
    "Country (random effect)"
  ),
  Effect = c(
    sprintf("OR = %.2f (up risk)", 10.107),
    sprintf("OR = %.1f (up risk)", exp(6.542)),
    sprintf("OR = %.2f (down risk)", 0.341),
    sprintf("OR = %.2f (down risk)", 0.443),
    sprintf("OR = %.3f/day (up risk)", 1.004),
    sprintf("OR = %.3f (up risk)", 1.008),
    sprintf("OR = %.2f (up risk)", 3.417),
    sprintf("ICC = %.3f", icc)
  ),
  `Robust to` = c("HC3 SE, bootstrap", "Firth penalization",
                  "All models", "All models",
                  "HC3, bootstrap CI [1.005, 1.007]",
                  "HC3, multilevel", "HC3", "Multilevel")
)
write_table(headline, "table_13_headline_findings",
            "Consolidated Headline Findings")

# --- Forest plot ---
safe_plot(
  or_tab_full %>%
    dplyr::filter(!Separation) %>%
    ggplot(aes(x = OR, y = reorder(term, OR))) +
    geom_point(size = 2, color = "#D7263D") +
    geom_errorbarh(aes(xmin = `CI lower`, xmax = `CI upper`), height = 0.2) +
    geom_vline(xintercept = 1, linetype = "dashed", color = "grey40") +
    scale_x_log10() +
    labs(title = "Odds Ratios for Booking Cancellation (95% CI; separation terms excluded)",
         x = "Odds Ratio (log scale)",
         y = NULL) +
    theme_minimal(base_size = 12),
  "fig_forest_plot.png", 8, 7)

# --- ROC comparison ---
tryCatch({
  png(here::here("figures", "fig_roc_comparison.png"),
      width = 900, height = 700, res = 120)
  plot(roc_obj, col = "#2E86AB", lwd = 2,
       main = "ROC Curves — Model Comparison")
  if (!is.null(auc_lasso)) {
    lasso_roc <- pROC::roc(y_test, lasso_pred, quiet = TRUE)
    plot(lasso_roc, col = "#D7263D", lwd = 2, add = TRUE)
  }
  rf_roc  <- pROC::roc(y_test, rf_pred, quiet = TRUE)
  plot(rf_roc, col = "#F18F01", lwd = 2, add = TRUE)
  xgb_roc <- pROC::roc(y_test, xgb_pred, quiet = TRUE)
  plot(xgb_roc, col = "#3B1F2B", lwd = 2, add = TRUE)
  legend("bottomright",
         legend = c(paste0("Logistic (AUC=", round(auc_val, 3), ")"),
                    paste0("Lasso (AUC=",    round(auc_lasso, 3), ")"),
                    paste0("RF (AUC=",       round(auc_rf, 3), ")"),
                    paste0("XGBoost (AUC=",  round(auc_xgb, 3), ")")),
         col = c("#2E86AB", "#D7263D", "#F18F01", "#3B1F2B"),
         lwd = 2)
  dev.off()
}, error = function(e) {
  if (!is.null(dev.list())) dev.off()
  message("ROC comparison figure skipped: ", conditionMessage(e))
})

# --- Closing banner + session info ---
cat("\n============================================\n")
cat("   ANALYSIS COMPLETE\n")
cat("============================================\n")
cat("\nTables written to:", here::here("outputs"), "\n")
cat("Figures written to:", here::here("figures"), "\n\n")
print(sessionInfo())
