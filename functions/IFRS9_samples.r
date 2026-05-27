# =============================================================================
#  IFRS 9 / ECB – EXPECTED CREDIT LOSS (ECL) MODELS IN R
#  Bank Risk Department | Full Model Suite
# =============================================================================
#  Sections
#   0.  Libraries & Synthetic Data Generation
#   1.  Stage Classification (SICR – Significant Increase in Credit Risk)
#   2.  PD Model – 12-Month (Logistic Regression)
#   3.  PD Model – Lifetime (Discrete-Time Survival / Hazard)
#   4.  PD Model – Through-the-Cycle (TTC) to Point-in-Time (PIT) Scaling
#   5.  LGD Model (Tobit / Beta Regression)
#   6.  EAD / CCF Model (Credit Conversion Factor)
#   7.  Macro-Economic Scenario Overlay (Forward-Looking Adjustments)
#   8.  ECL Calculation (Stage 1, 2, 3)
#   9.  Model Validation & Backtesting
#  10.  Reporting Summary
# =============================================================================


# =============================================================================
# SECTION 0 – LIBRARIES & SYNTHETIC DATA
# =============================================================================

# Install missing packages automatically
required_pkgs <- c(
  "tidyverse", "survival", "survminer", "betareg", "VGAM",
  "pROC", "ggplot2", "caret", "scales", "glmnet",
  "markovchain", "muhaz", "flexsurv", "knitr", "reshape2"
)
new_pkgs <- required_pkgs[!required_pkgs %in% installed.packages()[, "Package"]]
if (length(new_pkgs)) install.packages(new_pkgs, repos = "https://cloud.r-project.org")

library(tidyverse)
library(survival)
library(betareg)
library(VGAM)
library(pROC)
library(ggplot2)
library(caret)
library(scales)
library(glmnet)
library(markovchain)
library(flexsurv)
library(reshape2)

set.seed(42)
n <- 5000   # number of loan observations

# ── Synthetic Loan Portfolio ──────────────────────────────────────────────────
loan_data <- tibble(
  loan_id          = paste0("L", sprintf("%05d", 1:n)),
  origination_date = sample(seq(as.Date("2018-01-01"),
                                as.Date("2022-12-31"), by = "day"), n, TRUE),
  maturity_years   = sample(c(1, 3, 5, 7, 10, 20, 25), n, TRUE,
                            prob = c(0.05,0.15,0.20,0.20,0.15,0.15,0.10)),
  outstanding_bal  = round(runif(n, 10000, 5000000), 2),
  limit            = round(runif(n, 50000, 6000000), 2),
  product_type     = sample(c("Mortgage","SME","Corporate","Retail","RevCredit"),
                            n, TRUE, prob = c(0.35,0.20,0.15,0.20,0.10)),
  internal_rating  = sample(1:10, n, TRUE,           # 1=best, 10=worst
                            prob = c(0.05,0.10,0.15,0.20,0.18,0.12,
                                     0.08,0.06,0.04,0.02)),
  ltv              = pmin(pmax(rnorm(n, 0.65, 0.20), 0.05), 1.30),
  dti              = pmin(pmax(rnorm(n, 0.35, 0.15), 0.02), 0.95),
  months_on_books  = as.integer(runif(n, 1, 84)),
  dpd_current      = sample(0:180, n, TRUE,
                            prob = c(rep(0.012, 30),
                                     rep(0.006, 60),
                                     rep(0.002, 91))),
  dpd_max_12m      = sample(0:180, n, TRUE,
                            prob = c(rep(0.010, 30),
                                     rep(0.006, 60),
                                     rep(0.003, 91))),
  restructured     = rbinom(n, 1, 0.05),
  watchlist        = rbinom(n, 1, 0.08),
  collateral_value = round(runif(n, 0, 7000000), 2),
  sector           = sample(c("Real_Estate","Manufacturing","Retail_Trade",
                              "Finance","Services","Construction","Other"),
                            n, TRUE)
)

# Macro variables (quarterly snapshots attached to each loan)
loan_data <- loan_data %>%
  mutate(
    gdp_growth      = rnorm(n,  0.020, 0.015),
    unemployment    = pmax(rnorm(n, 0.07, 0.02), 0.02),
    interest_rate   = pmax(rnorm(n, 0.035, 0.015), 0.001),
    house_price_idx = rnorm(n, 1.00, 0.08)
  )

# True default flag (logit-driven for realism)
log_odds <- -3.5 +
  0.35 * loan_data$internal_rating +
  0.80 * (loan_data$dpd_current > 30) +
  0.60 * loan_data$ltv +
  1.20 * loan_data$dti +
  0.40 * loan_data$restructured +
  0.30 * loan_data$watchlist -
  5.00 * loan_data$gdp_growth +
  2.00 * loan_data$unemployment

loan_data$default_12m <- rbinom(n, 1, plogis(log_odds))
cat("Default rate:", round(mean(loan_data$default_12m) * 100, 2), "%\n")

# LGD (loss given default, 0-1 scale)
loan_data$lgd_obs <- ifelse(
  loan_data$default_12m == 1,
  pmin(pmax(rbeta(n, 2, 5) * (0.4 + 0.5 * loan_data$ltv), 0.01), 0.99),
  NA_real_
)

# EAD / CCF (for revolving facilities)
loan_data$ccf_obs <- ifelse(
  loan_data$product_type == "RevCredit",
  pmin(pmax(rbeta(n, 3, 4), 0.01), 0.99),
  NA_real_
)

# Survival-time variable (months to default for time-to-event models)
loan_data$time_to_event <- ifelse(
  loan_data$default_12m == 1,
  pmax(1, round(loan_data$months_on_books * runif(n, 0.1, 1))),
  loan_data$months_on_books
)
loan_data$event <- loan_data$default_12m


# =============================================================================
# SECTION 1 – STAGE CLASSIFICATION (SICR)
# =============================================================================
# IFRS 9 requires classification into:
#   Stage 1 – No significant increase in credit risk (12-month ECL)
#   Stage 2 – Significant increase in credit risk (lifetime ECL)
#   Stage 3 – Credit-impaired / defaulted (lifetime ECL, actual losses)
# ECB guidelines: quantitative (DPD, rating migration) + qualitative triggers
# =============================================================================

classify_stage <- function(df) {
  df %>%
    mutate(
      # ── Qualitative triggers ────────────────────────────────────────────
      q_restructured  = restructured == 1,
      q_watchlist     = watchlist == 1,
      q_dpd_30        = dpd_current > 30,
      q_dpd_90        = dpd_current > 90,    # EBA backstop
      
      # ── Quantitative: rating deterioration ≥ 2 notches (proxy) ─────────
      q_rating_deteri = internal_rating >= 7,
      
      # ── Absolute PD threshold (ECB: > 0.5% is indicative SICR) ─────────
      # (Will be populated after PD model – placeholder here)
      q_pd_threshold  = FALSE,
      
      # ── Stage assignment ─────────────────────────────────────────────────
      stage = case_when(
        q_dpd_90                             ~ 3L,   # defaulted
        q_dpd_30 | q_restructured | q_watchlist |
          q_rating_deteri                    ~ 2L,   # SICR
        TRUE                                 ~ 1L    # performing
      ),
      stage = factor(stage, levels = 1:3,
                     labels = c("Stage 1","Stage 2","Stage 3"))
    )
}

loan_data <- classify_stage(loan_data)

cat("\n── Stage Distribution ──────────────────────────────\n")
print(table(loan_data$stage))

# Stage migration matrix (QoQ or YoY monitoring)
set.seed(123)
loan_data$stage_prev <- sample(
  c("Stage 1","Stage 2","Stage 3"), n, TRUE, prob = c(0.75, 0.20, 0.05)
)

migration_matrix <- table(
  Previous = loan_data$stage_prev,
  Current  = loan_data$stage
)
migration_pct <- round(prop.table(migration_matrix, margin = 1) * 100, 1)
cat("\n── Stage Migration Matrix (%) ───────────────────────\n")
print(migration_pct)

# =============================================================================
# SECTION 2 – PD MODEL: 12-MONTH (LOGISTIC REGRESSION)
# =============================================================================
# Recommended by ECB for short-horizon default probability.
# Methods: Logistic Regression, Regularised (LASSO/Ridge), Scorecard.
# =============================================================================

# ── 2a. Feature preparation ───────────────────────────────────────────────────
pd_df <- loan_data %>%
  filter(stage != "Stage 3") %>%   # exclude already defaulted
  mutate(
    log_bal     = log1p(outstanding_bal),
    log_mob     = log1p(months_on_books),
    dpd_flag30  = as.integer(dpd_current > 30),
    sector_f    = factor(sector),
    product_f   = factor(product_type)
  ) %>%
  select(loan_id, default_12m, internal_rating, ltv, dti, log_bal,
         log_mob, dpd_flag30, restructured, watchlist, gdp_growth,
         unemployment, interest_rate, house_price_idx,
         sector_f, product_f)

# Train / test split (80/20)
train_idx <- createDataPartition(pd_df$default_12m, p = 0.80, list = FALSE)
train_pd  <- pd_df[ train_idx, ]
test_pd   <- pd_df[-train_idx, ]

# ── 2b. Logistic Regression ───────────────────────────────────────────────────
pd_logit <- glm(
  default_12m ~ internal_rating + ltv + dti + log_bal + log_mob +
    dpd_flag30 + restructured + watchlist + gdp_growth + unemployment +
    interest_rate + house_price_idx + sector_f + product_f,
  data   = train_pd,
  family = binomial(link = "logit")
)
summary(pd_logit)

# Predict on test set
test_pd$pd_logit <- predict(pd_logit, newdata = test_pd, type = "response")

# ── 2c. LASSO-regularised logistic regression ─────────────────────────────────
x_vars <- c("internal_rating","ltv","dti","log_bal","log_mob",
            "dpd_flag30","restructured","watchlist","gdp_growth",
            "unemployment","interest_rate","house_price_idx")

x_train <- model.matrix(~ . - 1,
                        data = train_pd %>% select(all_of(x_vars),
                                                   sector_f, product_f))
y_train <- train_pd$default_12m
x_test  <- model.matrix(~ . - 1,
                        data = test_pd %>% select(all_of(x_vars),
                                                  sector_f, product_f))

cv_lasso <- cv.glmnet(x_train, y_train, family = "binomial", alpha = 1)
test_pd$pd_lasso <- as.vector(
  predict(cv_lasso, newx = x_test, s = "lambda.min", type = "response")
)

# ── 2d. Model Performance ─────────────────────────────────────────────────────
roc_logit <- roc(test_pd$default_12m, test_pd$pd_logit, quiet = TRUE)
roc_lasso <- roc(test_pd$default_12m, test_pd$pd_lasso, quiet = TRUE)

cat("\n── 12M PD Model Performance ────────────────────────\n")
cat("Logistic Regression AUC :", round(auc(roc_logit), 4), "\n")
cat("LASSO Logistic AUC      :", round(auc(roc_lasso), 4), "\n")

# Gini coefficient (standard banking metric)
gini_logit <- 2 * auc(roc_logit) - 1
gini_lasso <- 2 * auc(roc_lasso) - 1
cat("Gini – Logistic         :", round(gini_logit, 4), "\n")
cat("Gini – LASSO            :", round(gini_lasso, 4), "\n")

# Hosmer-Lemeshow goodness-of-fit
hl_test <- function(actual, predicted, g = 10) {
  cuts   <- quantile(predicted, probs = seq(0, 1, length.out = g + 1))
  groups <- cut(predicted, breaks = cuts, include.lowest = TRUE)
  obs_1  <- tapply(actual,    groups, sum)
  exp_1  <- tapply(predicted, groups, sum)
  n_grp  <- tapply(actual,    groups, length)
  obs_0  <- n_grp - obs_1
  exp_0  <- n_grp - exp_1
  chi_sq <- sum((obs_1 - exp_1)^2 / exp_1 + (obs_0 - exp_0)^2 / exp_0,
                na.rm = TRUE)
  list(statistic = chi_sq, df = g - 2,
       p.value   = pchisq(chi_sq, df = g - 2, lower.tail = FALSE))
}

hl <- hl_test(test_pd$default_12m, test_pd$pd_logit)
cat("\nHosmer-Lemeshow χ²:", round(hl$statistic, 2),
    "  p-value:", round(hl$p.value, 4), "\n")

# Apply chosen PD back to full dataset (use logistic as primary)
loan_data$pd_12m <- predict(pd_logit, newdata = loan_data %>%
                              mutate(log_bal = log1p(outstanding_bal),
                                     log_mob = log1p(months_on_books),
                                     dpd_flag30 = as.integer(dpd_current > 30),
                                     sector_f = factor(sector),
                                     product_f = factor(product_type)),
                            type = "response")

# Update SICR PD threshold trigger
loan_data <- loan_data %>%
  mutate(
    q_pd_threshold = pd_12m > 0.005,   # ECB indicative threshold
    stage = case_when(
      dpd_current > 90 | default_12m == 1           ~ factor("Stage 3",
                                                             levels = c("Stage 1","Stage 2","Stage 3")),
      dpd_current > 30 | restructured == 1 |
        watchlist == 1 | internal_rating >= 7 |
        q_pd_threshold                              ~ factor("Stage 2",
                                                             levels = c("Stage 1","Stage 2","Stage 3")),
      TRUE                                          ~ factor("Stage 1",
                                                             levels = c("Stage 1","Stage 2","Stage 3"))
    )
  )


# =============================================================================
# SECTION 3 – LIFETIME PD: DISCRETE-TIME HAZARD / SURVIVAL MODEL
# =============================================================================
# ECB/IFRS 9: Stage 2 & 3 require ECL over remaining lifetime.
# Approach: Discrete-time proportional hazard (complementary log-log link).
# Alternative: Cox PH, Weibull AFT, Markov transition matrices.
# =============================================================================

# ── 3a. Expand to loan-period panel (person-month format) ─────────────────────
# Each loan contributes one row per month until default or censoring
panel_df <- loan_data %>%
  slice_sample(n = 800) %>%          # subset for performance
  select(loan_id, time_to_event, event, internal_rating, ltv, dti,
         restructured, watchlist, gdp_growth, unemployment) %>%
  mutate(time_to_event = pmax(time_to_event, 1L))

# Create person-period dataset
person_period <- panel_df %>%
  rowwise() %>%
  do({
    t_max <- .$time_to_event
    tibble(
      loan_id        = .$loan_id,
      period         = 1:t_max,
      event_period   = c(rep(0, t_max - 1), .$event),
      internal_rating = .$internal_rating,
      ltv            = .$ltv,
      dti            = .$dti,
      restructured   = .$restructured,
      watchlist      = .$watchlist,
      gdp_growth     = .$gdp_growth,
      unemployment   = .$unemployment
    )
  }) %>%
  ungroup()

# Add time spline terms (baseline hazard shape)
person_period <- person_period %>%
  mutate(
    log_t  = log(period),
    log_t2 = log(period)^2
  )

# ── 3b. Complementary log-log model (discrete-time PH) ───────────────────────
cloglog_fit <- glm(
  event_period ~ log_t + log_t2 + internal_rating + ltv + dti +
    restructured + watchlist + gdp_growth + unemployment,
  data   = person_period,
  family = binomial(link = "cloglog")
)
summary(cloglog_fit)

# ── 3c. Predict conditional default probability h(t) for each period ──────────
person_period$h_t <- predict(cloglog_fit, type = "response")

# Compute survival S(t) = ∏(1 - h(s)), s=1..t
person_period <- person_period %>%
  group_by(loan_id) %>%
  arrange(period) %>%
  mutate(
    S_t       = cumprod(1 - h_t),          # Survival function
    pd_period = h_t * lag(S_t, default = 1)  # Unconditional PD per period
  ) %>%
  ungroup()

# ── 3d. Lifetime PD (sum of marginal PDs over all future periods) ─────────────
lifetime_pd_tbl <- person_period %>%
  group_by(loan_id) %>%
  summarise(
    lifetime_pd = sum(pd_period, na.rm = TRUE),
    max_period  = max(period),
    .groups     = "drop"
  )

cat("\n── Lifetime PD Summary ─────────────────────────────\n")
print(summary(lifetime_pd_tbl$lifetime_pd))

# Merge back
loan_data <- loan_data %>%
  left_join(lifetime_pd_tbl %>% select(loan_id, lifetime_pd),
            by = "loan_id") %>%
  mutate(lifetime_pd = coalesce(lifetime_pd, pd_12m * maturity_years * 0.8))

# ── 3e. Alternative: Cox Proportional Hazards ─────────────────────────────────
surv_obj <- Surv(panel_df$time_to_event, panel_df$event)
cox_fit  <- coxph(surv_obj ~ internal_rating + ltv + dti +
                    restructured + watchlist + gdp_growth + unemployment,
                  data = panel_df, ties = "efron")
summary(cox_fit)

cat("\nCox Model Concordance:", round(cox_fit$concordance["concordance"], 4), "\n")

# ── 3f. Markov Chain transition matrix approach ───────────────────────────────
# States: Performing (1), Watchlist (2), Default (3), Cured (4)
# Build from stage migrations observed in portfolio

states <- c("Performing","Watchlist","Default","Cured")

# Simplified empirical transition matrix (in production: estimate from data)
trans_matrix <- matrix(
  c(0.92, 0.05, 0.02, 0.01,
    0.20, 0.60, 0.15, 0.05,
    0.00, 0.00, 0.85, 0.15,
    0.60, 0.15, 0.05, 0.20),
  nrow = 4, byrow = TRUE,
  dimnames = list(from = states, to = states)
)

mc_credit <- new("markovchain",
                 states         = states,
                 transitionMatrix = trans_matrix,
                 name           = "Credit State MC")

cat("\n── Markov Chain Steady-State Distribution ──────────\n")
print(steadyStates(mc_credit))

# Lifetime default probability via Markov: P(ever reach Default | start state)
# Power the matrix to maturity horizon
horizon <- 60  # months
mc_power <- trans_matrix
for (i in 2:horizon) mc_power <- mc_power %*% trans_matrix
cat("\nP(Default within 60m | starts Performing) =",
    round(mc_power["Performing","Default"], 4), "\n")


# =============================================================================
# SECTION 4 – TTC TO PIT PD SCALING
# =============================================================================
# ECB requires Point-in-Time PDs (macro-conditional) for ECL.
# TTC PDs (rating-based, cycle-neutral) must be scaled using macro factors.
# Method: Vasicek single-factor model / scaling ratio approach.
# =============================================================================

# ── 4a. Vasicek one-factor model ─────────────────────────────────────────────
# PD_PIT = Φ( (Φ⁻¹(PD_TTC) - √ρ · Z_macro) / √(1-ρ) )
# where Z_macro is a standardised composite macro index, ρ = asset correlation

vasicek_pit <- function(pd_ttc, rho, z_macro) {
  pnorm((qnorm(pd_ttc) - sqrt(rho) * z_macro) / sqrt(1 - rho))
}

# Composite macro index (standardised GDP growth + unemployment)
z_macro_composite <- with(loan_data,
                          scale(gdp_growth)[,1] - scale(unemployment)[,1]
)

# Asset correlation ρ: Basel II formula (retail vs corporate)
# For illustration, use ρ = 0.15 (retail) and ρ = 0.20 (corporate)
loan_data <- loan_data %>%
  mutate(
    rho = ifelse(product_type %in% c("Mortgage","Retail","RevCredit"), 0.15, 0.20),
    pd_ttc = pd_12m,   # treat model PD as TTC baseline
    z_macro = z_macro_composite,
    pd_pit  = vasicek_pit(
      pd_ttc  = pmax(pd_ttc, 1e-6),
      rho     = rho,
      z_macro = z_macro
    ),
    pd_pit = pmin(pmax(pd_pit, 1e-6), 0.9999)
  )

cat("\n── PD TTC vs PIT Comparison ────────────────────────\n")
cat("Mean TTC PD:", round(mean(loan_data$pd_ttc, na.rm=TRUE)*100, 3), "%\n")
cat("Mean PIT PD:", round(mean(loan_data$pd_pit, na.rm=TRUE)*100, 3), "%\n")

# ── 4b. Scalar ratio method (simpler, ECB-accepted) ──────────────────────────
# PD_PIT = PD_TTC × (Observed default rate / Long-run average default rate)
lr_avg_dr <- 0.02  # long-run average default rate
obs_dr     <- mean(loan_data$default_12m)
scalar     <- obs_dr / lr_avg_dr

loan_data <- loan_data %>%
  mutate(pd_pit_scalar = pmin(pd_ttc * scalar, 0.9999))

cat("Scalar method ratio:", round(scalar, 3), "\n")


# =============================================================================
# SECTION 5 – LGD MODEL (LOSS GIVEN DEFAULT)
# =============================================================================
# ECB: Downturn LGD required for IFRS 9 ECL.
# Methods: Tobit (censored regression), Beta regression, OLS on logit-transform.
# LGD = 1 - Recovery Rate; bounded [0,1].
# =============================================================================

lgd_df <- loan_data %>%
  filter(!is.na(lgd_obs)) %>%
  mutate(
    lgd         = pmin(pmax(lgd_obs, 0.001), 0.999),
    log_bal     = log1p(outstanding_bal),
    collat_flag = as.integer(collateral_value > 0),
    collat_ratio = pmin(collateral_value / (outstanding_bal + 1), 5)
  )

# ── 5a. Beta Regression (canonical LGD approach) ─────────────────────────────
beta_lgd <- betareg(
  lgd ~ ltv + dti + log_bal + collat_flag + collat_ratio +
    internal_rating + restructured + gdp_growth + unemployment +
    product_type,
  data = lgd_df
)
summary(beta_lgd)

lgd_df$lgd_hat_beta <- predict(beta_lgd, type = "response")

# ── 5b. Tobit (censored regression) ──────────────────────────────────────────
tobit_lgd <- vglm(
  lgd ~ ltv + dti + log_bal + collat_flag + internal_rating +
    restructured + gdp_growth + unemployment,
  tobit(Lower = 0, Upper = 1),
  data = lgd_df
)
lgd_df$lgd_hat_tobit <- pmin(pmax(fitted(tobit_lgd)[, 1], 0.001), 0.999)

# ── 5c. Downturn LGD adjustment ───────────────────────────────────────────────
# ECB: add economic downturn add-on (typically 10-20% relative increase)
# Downturn identified from worst historical LGD in stress vintage
downturn_add_on <- 0.15   # 15% add-on – calibrate to historical stress period
loan_data <- loan_data %>%
  left_join(
    lgd_df %>% select(loan_id, lgd_hat_beta, lgd_hat_tobit),
    by = "loan_id"
  ) %>%
  mutate(
    lgd_base    = coalesce(lgd_hat_beta,
                           0.45),  # ECB fallback: 45% for unsecured
    lgd_downturn = pmin(lgd_base * (1 + downturn_add_on), 0.99)
  )

cat("\n── LGD Model Performance (RMSE) ────────────────────\n")
cat("Beta Regression RMSE:",
    round(sqrt(mean((lgd_df$lgd - lgd_df$lgd_hat_beta)^2)), 4), "\n")
cat("Tobit RMSE          :",
    round(sqrt(mean((lgd_df$lgd - lgd_df$lgd_hat_tobit)^2)), 4), "\n")


# =============================================================================
# SECTION 6 – EAD / CCF MODEL (EXPOSURE AT DEFAULT)
# =============================================================================
# EAD = Current drawn balance + CCF × Undrawn commitment
# CCF (Credit Conversion Factor): proportion of undrawn that will be drawn
# Most relevant for revolving credit, overdrafts, credit cards, commitments.
# =============================================================================

ead_df <- loan_data %>%
  filter(product_type == "RevCredit") %>%
  mutate(
    undrawn       = pmax(limit - outstanding_bal, 0),
    util_rate     = outstanding_bal / pmax(limit, 1),
    log_undrawn   = log1p(undrawn),
    log_limit     = log1p(limit)
  ) %>%
  filter(!is.na(ccf_obs))

# ── 6a. Beta Regression for CCF ──────────────────────────────────────────────
ccf_beta <- betareg(
  ccf_obs ~ util_rate + log_undrawn + log_limit +
    internal_rating + months_on_books + gdp_growth,
  data = ead_df
)
summary(ccf_beta)

ead_df$ccf_hat <- predict(ccf_beta, type = "response")

# ── 6b. OLS on logit-transformed CCF (alternative) ───────────────────────────
ead_df$ccf_logit <- log(ead_df$ccf_obs / (1 - ead_df$ccf_obs))
ccf_ols <- lm(
  ccf_logit ~ util_rate + log_undrawn + log_limit +
    internal_rating + months_on_books + gdp_growth,
  data = ead_df
)
ead_df$ccf_hat_ols <- plogis(predict(ccf_ols))

# ── 6c. EAD calculation ───────────────────────────────────────────────────────
loan_data <- loan_data %>%
  left_join(
    ead_df %>% select(loan_id, ccf_hat, ccf_hat_ols),
    by = "loan_id"
  ) %>%
  mutate(
    undrawn    = pmax(limit - outstanding_bal, 0),
    ccf        = coalesce(ccf_hat, 0),   # 0 for non-revolving
    ead        = outstanding_bal + ccf * undrawn,
    ead        = pmax(ead, outstanding_bal)  # EAD >= current balance
  )

cat("\n── EAD Summary ─────────────────────────────────────\n")
cat("Total Outstanding  :", formatC(sum(loan_data$outstanding_bal),
                                    format="f", big.mark=",", digits=0), "\n")
cat("Total EAD          :", formatC(sum(loan_data$ead),
                                    format="f", big.mark=",", digits=0), "\n")
cat("EAD / Outstanding  :",
    round(sum(loan_data$ead)/sum(loan_data$outstanding_bal), 4), "\n")


# =============================================================================
# SECTION 7 – MACRO-ECONOMIC SCENARIO OVERLAY
# =============================================================================
# ECB/IFRS 9 requires forward-looking macro adjustments using multiple scenarios.
# Typical: Base, Adverse, Severe Adverse – probability-weighted.
# Satellite models link macro variables to PD/LGD.
# =============================================================================

# ── 7a. Define 3-scenario macro paths (3-year horizon, 12 quarters) ───────────
quarters <- 1:12  # Q1 to Q12 forward

scenarios <- tibble(
  quarter      = rep(quarters, 3),
  scenario     = rep(c("Base","Adverse","Severe_Adverse"), each = 12),
  probability  = rep(c(0.50, 0.30, 0.20), each = 12),
  gdp_growth   = c(
    # Base: gradual recovery
    c(0.025, 0.025, 0.022, 0.022, 0.020, 0.020, 0.019, 0.019,
      0.018, 0.018, 0.017, 0.017),
    # Adverse: slowdown
    c(0.005, 0.003, 0.000, -0.005, -0.005, 0.000, 0.005, 0.008,
      0.010, 0.012, 0.013, 0.014),
    # Severe: deep recession
    c(-0.010, -0.030, -0.040, -0.040, -0.025, -0.010, 0.000,
      0.005, 0.008, 0.010, 0.012, 0.013)
  ),
  unemployment = c(
    c(0.065, 0.063, 0.062, 0.061, 0.060, 0.059, 0.059, 0.058,
      0.058, 0.057, 0.057, 0.056),
    c(0.070, 0.078, 0.085, 0.090, 0.092, 0.090, 0.085, 0.080,
      0.076, 0.073, 0.071, 0.069),
    c(0.075, 0.090, 0.110, 0.130, 0.140, 0.138, 0.130, 0.120,
      0.110, 0.100, 0.090, 0.083)
  )
)

# ── 7b. Satellite model: macro → PD adjustment multiplier ────────────────────
# Estimate from historical data (here: simplified calibrated coefficients)
macro_pd_model <- function(gdp_growth, unemployment,
                           beta_gdp = -8.0, beta_unemp = 5.0,
                           intercept = -4.5) {
  # Returns expected log-odds shift in PD given macro variables
  plogis(intercept + beta_gdp * gdp_growth + beta_unemp * unemployment)
}

scenarios <- scenarios %>%
  mutate(
    pd_macro_factor = macro_pd_model(gdp_growth, unemployment) /
      macro_pd_model(0.02, 0.065)  # normalise to base macro
  )

# ── 7c. Probability-weighted macro-adjusted PD (per period) ──────────────────
scenario_ecl_weights <- scenarios %>%
  group_by(quarter) %>%
  summarise(
    weighted_pd_factor = sum(pd_macro_factor * probability),
    .groups = "drop"
  )

cat("\n── Scenario-Weighted PD Multiplier (by quarter) ────\n")
print(scenario_ecl_weights)

# ── 7d. Non-linearity adjustment (Jensen's inequality correction) ─────────────
# Because E[f(X)] ≠ f(E[X]), a convexity adjustment is applied.
# Simple approach: average PDs across scenarios then apply.
scenarios_wide <- scenarios %>%
  group_by(quarter) %>%
  summarise(
    pd_base        = pd_macro_factor[scenario=="Base"],
    pd_adverse     = pd_macro_factor[scenario=="Adverse"],
    pd_severe      = pd_macro_factor[scenario=="Severe_Adverse"],
    w_base         = probability[scenario=="Base"],
    w_adverse      = probability[scenario=="Adverse"],
    w_severe       = probability[scenario=="Severe_Adverse"],
    pd_weighted    = pd_base*w_base + pd_adverse*w_adverse + pd_severe*w_severe,
    .groups        = "drop"
  )

cat("\n── Probability-Weighted PD Factor (Q1-Q12) ─────────\n")
print(scenarios_wide %>% select(quarter, pd_base, pd_adverse,
                                pd_severe, pd_weighted))


# =============================================================================
# SECTION 8 – ECL CALCULATION (STAGE 1, 2, 3)
# =============================================================================
# ECL = PD × LGD × EAD × DF (discount factor)
# Stage 1: 12-month ECL
# Stage 2: Lifetime ECL
# Stage 3: Lifetime ECL (impaired; PD=1 effectively, best-estimate LGD)
# ECB: use effective interest rate for discounting.
# =============================================================================

# ── 8a. Discount factor (effective interest rate approximation) ───────────────
loan_data <- loan_data %>%
  mutate(
    eir          = pmax(interest_rate + 0.02, 0.02),  # EIR = rate + spread
    df_12m       = 1 / (1 + eir),                      # 1-year discount
    df_lifetime  = (1 - (1 + eir)^(-maturity_years)) / eir  # annuity DF
  )

# ── 8b. Apply scenario-weighted PD multiplier ─────────────────────────────────
# Use Q1 factor for 12m PD, average of Q1-Q12 for lifetime
avg_pw_factor <- mean(scenarios_wide$pd_weighted)
q1_pw_factor  <- scenarios_wide$pd_weighted[1]

loan_data <- loan_data %>%
  mutate(
    pd_pit_fwd_12m  = pmin(pd_pit * q1_pw_factor, 0.9999),
    pd_pit_lifetime = pmin(lifetime_pd * avg_pw_factor, 0.9999)
  )

# ── 8c. ECL per loan ──────────────────────────────────────────────────────────
loan_data <- loan_data %>%
  mutate(
    ecl = case_when(
      stage == "Stage 1" ~
        pd_pit_fwd_12m * lgd_downturn * ead * df_12m,
      
      stage == "Stage 2" ~
        pd_pit_lifetime * lgd_downturn * ead * df_lifetime,
      
      stage == "Stage 3" ~
        1.0 * lgd_downturn * ead * df_lifetime,  # PD=100%; already defaulted
      
      TRUE ~ 0
    ),
    ecl = pmax(ecl, 0)
  )

# ── 8d. Portfolio ECL summary ─────────────────────────────────────────────────
ecl_summary <- loan_data %>%
  group_by(stage) %>%
  summarise(
    n_loans          = n(),
    total_ead        = sum(ead,           na.rm=TRUE),
    total_ecl        = sum(ecl,           na.rm=TRUE),
    avg_pd           = mean(pd_pit_fwd_12m, na.rm=TRUE),
    avg_lgd          = mean(lgd_downturn, na.rm=TRUE),
    coverage_ratio   = total_ecl / total_ead,
    .groups = "drop"
  )

cat("\n── Portfolio ECL by Stage ──────────────────────────\n")
print(ecl_summary %>%
        mutate(
          total_ead      = dollar(total_ead, accuracy=1),
          total_ecl      = dollar(total_ecl, accuracy=1),
          avg_pd         = percent(avg_pd, accuracy=0.01),
          avg_lgd        = percent(avg_lgd, accuracy=0.1),
          coverage_ratio = percent(coverage_ratio, accuracy=0.01)
        ))

cat("\nTotal Portfolio ECL:", dollar(sum(loan_data$ecl, na.rm=TRUE)), "\n")
cat("Total EAD          :", dollar(sum(loan_data$ead, na.rm=TRUE)), "\n")
cat("Portfolio Coverage :",
    percent(sum(loan_data$ecl,na.rm=TRUE)/sum(loan_data$ead,na.rm=TRUE),
            accuracy=0.01), "\n")

# ── 8e. Scenario ECL (for disclosure) ─────────────────────────────────────────
calc_ecl_scenario <- function(pd_multiplier, suffix) {
  loan_data %>%
    mutate(
      pd_scen = pmin(pd_pit * pd_multiplier, 0.9999),
      ecl_scen = case_when(
        stage == "Stage 1" ~ pd_scen * lgd_downturn * ead * df_12m,
        stage == "Stage 2" ~ pmin(pd_scen * maturity_years * 0.8, 0.9999) *
          lgd_downturn * ead * df_lifetime,
        stage == "Stage 3" ~ lgd_downturn * ead * df_lifetime,
        TRUE ~ 0
      )
    ) %>%
    summarise(scenario = suffix,
              ecl_total = sum(pmax(ecl_scen, 0), na.rm=TRUE))
}

ecl_scenarios <- bind_rows(
  calc_ecl_scenario(scenarios_wide$pd_base[1],    "Base"),
  calc_ecl_scenario(scenarios_wide$pd_adverse[1], "Adverse"),
  calc_ecl_scenario(scenarios_wide$pd_severe[1],  "Severe_Adverse")
)

cat("\n── ECL by Macro Scenario ───────────────────────────\n")
print(ecl_scenarios %>% mutate(ecl_total = dollar(ecl_total)))


# =============================================================================
# SECTION 9 – MODEL VALIDATION & BACKTESTING
# =============================================================================
# ECB requires independent model validation covering:
#  (a) Discriminatory power (AUC/Gini)
#  (b) Calibration (Brier score, reliability diagrams)
#  (c) Stability (PSI – Population Stability Index)
#  (d) Backtesting (predicted vs actual default rates)
#  (e) Sensitivity / stress testing
# =============================================================================

# ── 9a. Discriminatory Power ──────────────────────────────────────────────────
val_df <- test_pd %>%
  mutate(
    pd_logit = predict(pd_logit, newdata = test_pd %>%
                         mutate(log_bal=log1p(outstanding_bal),
                                log_mob=log1p(months_on_books),
                                dpd_flag30=as.integer(dpd_current>30),
                                sector_f=factor(sector),
                                product_f=factor(product_type)), type="response"),
    actual   = default_12m
  )

roc_val <- roc(val_df$actual, val_df$pd_logit, quiet = TRUE)
auc_val <- auc(roc_val)
gini_val <- 2 * auc_val - 1

cat("\n── Validation Discriminatory Power ─────────────────\n")
cat("AUC  :", round(auc_val, 4), "\n")
cat("Gini :", round(gini_val, 4), "\n")

# ── 9b. Brier Score (calibration) ─────────────────────────────────────────────
brier_score <- mean((val_df$actual - val_df$pd_logit)^2)
brier_naive <- mean((val_df$actual - mean(val_df$actual))^2)
brier_skill <- 1 - brier_score / brier_naive  # Brier skill score
cat("Brier Score       :", round(brier_score, 5), "\n")
cat("Brier Skill Score :", round(brier_skill, 4), " (higher=better)\n")

# ── 9c. Population Stability Index (PSI) ──────────────────────────────────────
psi_calc <- function(expected, actual, n_bins = 10) {
  bins   <- quantile(expected, probs = seq(0, 1, length.out = n_bins + 1))
  bins[1] <- bins[1] - 1e-9;  bins[length(bins)] <- bins[length(bins)] + 1e-9
  exp_cnt <- as.numeric(table(cut(expected, breaks = bins)))
  act_cnt <- as.numeric(table(cut(actual,   breaks = bins)))
  exp_pct <- exp_cnt / sum(exp_cnt)
  act_pct <- act_cnt / sum(act_cnt)
  # Avoid log(0)
  exp_pct[exp_pct == 0] <- 1e-9
  act_pct[act_pct == 0] <- 1e-9
  psi <- sum((act_pct - exp_pct) * log(act_pct / exp_pct))
  list(psi = psi, exp_pct = exp_pct, act_pct = act_pct)
}

# Simulate a "development vs current" PSI comparison
set.seed(99)
pd_dev  <- predict(pd_logit, newdata = train_pd %>%
                     mutate(log_bal=log1p(outstanding_bal), log_mob=log1p(months_on_books),
                            dpd_flag30=as.integer(dpd_current>30),
                            sector_f=factor(sector), product_f=factor(product_type)),
                   type="response")
pd_val  <- val_df$pd_logit

psi_res <- psi_calc(pd_dev, pd_val)
cat("\nPSI (Development vs Validation) :", round(psi_res$psi, 4),
    ifelse(psi_res$psi < 0.10, " → STABLE",
           ifelse(psi_res$psi < 0.25, " → MONITOR", " → UNSTABLE")), "\n")

# ── 9d. Backtesting: predicted vs actual default rate (vintage analysis) ───────
backtest <- loan_data %>%
  mutate(pd_bucket = cut(pd_12m,
                         breaks = c(0, 0.005, 0.01, 0.02, 0.05, 0.10, 1),
                         labels = c("<0.5%","0.5-1%","1-2%","2-5%","5-10%",">10%"),
                         include.lowest = TRUE)) %>%
  group_by(pd_bucket) %>%
  summarise(
    n            = n(),
    avg_pd_pred  = mean(pd_12m, na.rm=TRUE),
    actual_dr    = mean(default_12m, na.rm=TRUE),
    ratio        = actual_dr / pmax(avg_pd_pred, 1e-8),
    .groups = "drop"
  )

cat("\n── Backtesting: Predicted vs Actual DR ─────────────\n")
print(backtest %>%
        mutate(
          avg_pd_pred = percent(avg_pd_pred, accuracy=0.01),
          actual_dr   = percent(actual_dr,   accuracy=0.01),
          ratio       = round(ratio, 2)
        ))

# ── 9e. KS Statistic ──────────────────────────────────────────────────────────
ks_test <- function(actual, predicted) {
  df <- data.frame(actual, predicted) %>%
    arrange(desc(predicted)) %>%
    mutate(
      cum_bad  = cumsum(actual)    / sum(actual),
      cum_good = cumsum(1-actual)  / sum(1-actual)
    )
  max(abs(df$cum_bad - df$cum_good))
}

ks_stat <- ks_test(val_df$actual, val_df$pd_logit)
cat("\nKS Statistic:", round(ks_stat, 4),
    ifelse(ks_stat > 0.30, " → GOOD", " → REVIEW"), "\n")

# ── 9f. Sensitivity / Stress Test on ECL ──────────────────────────────────────
stress_ecl <- function(pd_shock, lgd_shock) {
  loan_data %>%
    mutate(
      pd_stressed  = pmin(pd_pit * (1 + pd_shock),  0.9999),
      lgd_stressed = pmin(lgd_downturn * (1 + lgd_shock), 0.99),
      ecl_stressed = case_when(
        stage == "Stage 1" ~ pd_stressed * lgd_stressed * ead * df_12m,
        stage == "Stage 2" ~ pd_stressed * maturity_years*0.8 *
          lgd_stressed * ead * df_lifetime,
        stage == "Stage 3" ~ lgd_stressed * ead * df_lifetime,
        TRUE ~ 0
      )
    ) %>%
    summarise(
      scenario     = paste0("PD+", pd_shock*100, "% / LGD+", lgd_shock*100, "%"),
      ecl_stressed = sum(pmax(ecl_stressed, 0), na.rm=TRUE)
    )
}

stress_results <- bind_rows(
  stress_ecl(0.00, 0.00),
  stress_ecl(0.25, 0.10),
  stress_ecl(0.50, 0.20),
  stress_ecl(1.00, 0.30)
)

cat("\n── ECL Stress Test Results ─────────────────────────\n")
print(stress_results %>% mutate(ecl_stressed = dollar(ecl_stressed)))


# =============================================================================
# SECTION 10 – REPORTING SUMMARY (ECB DISCLOSURE-READY)
# =============================================================================

cat("\n\n")
cat("╔══════════════════════════════════════════════════════════════╗\n")
cat("║          IFRS 9 ECL REPORTING SUMMARY                       ║\n")
cat("╠══════════════════════════════════════════════════════════════╣\n")
cat("║  Portfolio Overview                                          ║\n")
cat(sprintf("║   Total loans            : %8d                        ║\n", nrow(loan_data)))
cat(sprintf("║   Total EAD              : %s                 ║\n",
            formatC(sum(loan_data$ead,na.rm=TRUE), format="f", big.mark=",",
                    digits=0, width=12)))
cat(sprintf("║   Total ECL (weighted)   : %s                 ║\n",
            formatC(sum(loan_data$ecl,na.rm=TRUE), format="f", big.mark=",",
                    digits=0, width=12)))
cat(sprintf("║   Portfolio Coverage     : %.2f%%                          ║\n",
            100 * sum(loan_data$ecl,na.rm=TRUE) / sum(loan_data$ead,na.rm=TRUE)))
cat("╠══════════════════════════════════════════════════════════════╣\n")
cat("║  Model Performance (12M PD Logistic)                        ║\n")
cat(sprintf("║   AUC / Gini             : %.4f / %.4f                  ║\n",
            auc_val, gini_val))
cat(sprintf("║   KS Statistic           : %.4f                          ║\n", ks_stat))
cat(sprintf("║   Brier Skill Score      : %.4f                          ║\n", brier_skill))
cat(sprintf("║   PSI (stability)        : %.4f                          ║\n", psi_res$psi))
cat("╠══════════════════════════════════════════════════════════════╣\n")
cat("║  Stage Distribution                                         ║\n")
stg <- table(loan_data$stage)
cat(sprintf("║   Stage 1  : %5d loans  (%.1f%%)                        ║\n",
            stg["Stage 1"], 100*stg["Stage 1"]/nrow(loan_data)))
cat(sprintf("║   Stage 2  : %5d loans  (%.1f%%)                         ║\n",
            stg["Stage 2"], 100*stg["Stage 2"]/nrow(loan_data)))
cat(sprintf("║   Stage 3  : %5d loans  (%.1f%%)                         ║\n",
            stg["Stage 3"], 100*stg["Stage 3"]/nrow(loan_data)))
cat("╚══════════════════════════════════════════════════════════════╝\n")


# ── Optional: Save outputs ────────────────────────────────────────────────────
# write_csv(loan_data, "ifrs9_ecl_results.csv")
# write_csv(ecl_summary, "ifrs9_stage_summary.csv")
# saveRDS(pd_logit, "pd_model_logit.rds")
# saveRDS(beta_lgd, "lgd_model_beta.rds")
# saveRDS(ccf_beta, "ccf_model_beta.rds")

cat("\n✔  All IFRS 9 ECB models completed successfully.\n")