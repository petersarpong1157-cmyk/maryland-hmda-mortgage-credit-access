#Step 1

library(readr)

setwd("C:/Users/peter/Desktop/Research Publications")

hmda_raw <- read_csv(
  "loan_purposes_1_state_MD.csv",
  col_types = cols(.default = col_character()),
  show_col_types = FALSE
)

dim(hmda_raw)
problems(hmda_raw)

# STEP 2: VERIFY VARIABLES FOR PAPER 2

vars_needed <- c(
  "action_taken",
  "census_tract",
  "tract_minority_population_percent",
  "tract_to_msa_income_percentage",
  "income",
  "loan_amount",
  "debt_to_income_ratio",
  "loan_to_value_ratio",
  "loan_term",
  "occupancy_type",
  "construction_method",
  "conforming_loan_limit",
  "business_or_commercial_purpose",
  "applicant_credit_score_type"
)

# Confirm that every required variable exists
vars_needed %in% names(hmda_raw)

# Show the names of any variables that are missing
vars_needed[!vars_needed %in% names(hmda_raw)]


# STEP 3A: VERIFY ACTION_TAKEN VALUES

table(hmda_raw$action_taken, useNA = "ifany")

# STEP 3B: CONSTRUCT CREDIT-DECISION SAMPLE

decision_sample <- hmda_raw[
  hmda_raw$action_taken %in% c("1", "2", "3"),
]

# Verify sample size
nrow(decision_sample)

# Verify action distribution
table(decision_sample$action_taken)

# Create binary denial outcome
decision_sample$Denied <- ifelse(
  decision_sample$action_taken == "3",
  1,
  0
)

# Verify outcome
table(decision_sample$Denied)

# Calculate descriptive denial rate
mean(decision_sample$Denied) * 100


# STEP 3C: INSPECT RAW VALUES BEFORE RECODING

# Missing values as currently imported
sapply(
  decision_sample[c(
    "tract_minority_population_percent",
    "tract_to_msa_income_percentage",
    "income",
    "loan_amount",
    "debt_to_income_ratio",
    "loan_to_value_ratio",
    "loan_term",
    "occupancy_type",
    "construction_method",
    "conforming_loan_limit",
    "business_or_commercial_purpose"
  )],
  function(x) sum(is.na(x))
)

# Inspect categorical/special-code variables
table(decision_sample$occupancy_type, useNA = "ifany")
table(decision_sample$construction_method, useNA = "ifany")
table(decision_sample$conforming_loan_limit, useNA = "ifany")
table(decision_sample$business_or_commercial_purpose, useNA = "ifany")

# Inspect DTI values
sort(unique(decision_sample$debt_to_income_ratio))

# Check special/non-numeric values in LTV and loan term
unique(
  decision_sample$loan_to_value_ratio[
    is.na(suppressWarnings(as.numeric(decision_sample$loan_to_value_ratio)))
  ]
)

unique(
  decision_sample$loan_term[
    is.na(suppressWarnings(as.numeric(decision_sample$loan_term)))
  ])

# STEP 4A: RECODE NUMERIC VARIABLES

# Income
decision_sample$income_num <-
  suppressWarnings(as.numeric(decision_sample$income))

# Loan amount
decision_sample$loan_amount_num <-
  suppressWarnings(as.numeric(decision_sample$loan_amount))

# LTV: "Exempt" and other nonnumeric values become NA
decision_sample$ltv_num <-
  suppressWarnings(as.numeric(decision_sample$loan_to_value_ratio))

# Loan term: "Exempt" and other nonnumeric values become NA
decision_sample$loan_term_num <-
  suppressWarnings(as.numeric(decision_sample$loan_term))

# DTI conversion
decision_sample$dti_num <- NA_real_

decision_sample$dti_num[
  decision_sample$debt_to_income_ratio == "<20%"
] <- 15

decision_sample$dti_num[
  decision_sample$debt_to_income_ratio == "20%-<30%"
] <- 25

decision_sample$dti_num[
  decision_sample$debt_to_income_ratio == "30%-<36%"
] <- 33

decision_sample$dti_num[
  decision_sample$debt_to_income_ratio == "50%-60%"
] <- 55

decision_sample$dti_num[
  decision_sample$debt_to_income_ratio == ">60%"
] <- 65

# Directly reported numeric DTI values
numeric_dti <- suppressWarnings(
  as.numeric(decision_sample$debt_to_income_ratio)
)

decision_sample$dti_num[
  !is.na(numeric_dti)
] <- numeric_dti[!is.na(numeric_dti)]


# VERIFY RECODING

sapply(
  decision_sample[c(
    "income_num",
    "loan_amount_num",
    "dti_num",
    "ltv_num",
    "loan_term_num"
  )],
  function(x) sum(is.na(x))
)

summary(
  decision_sample[c(
    "income_num",
    "loan_amount_num",
    "dti_num",
    "ltv_num",
    "loan_term_num"
  )])


# STEP 4B: RECODE VERIFIED CATEGORICAL VARIABLES

decision_sample$occupancy_factor <- factor(
  decision_sample$occupancy_type,
  levels = c("1", "2", "3"),
  labels = c(
    "Principal residence",
    "Second residence",
    "Investment property"
  )
)

decision_sample$construction_factor <- factor(
  decision_sample$construction_method,
  levels = c("1", "2"),
  labels = c(
    "Site-built",
    "Manufactured home"
  )
)

# Preserve official source codes for now
decision_sample$conforming_factor <- factor(
  decision_sample$conforming_loan_limit
)

decision_sample$business_factor <- factor(
  decision_sample$business_or_commercial_purpose
)

# Verify recoding
table(decision_sample$occupancy_factor, useNA = "ifany")
table(decision_sample$construction_factor, useNA = "ifany")
table(decision_sample$conforming_factor, useNA = "ifany")
table(decision_sample$business_factor, useNA = "ifany")


# STEP 5A: CREATE COMMON COMPLETE-CASE ANALYTICAL SAMPLE

analysis_vars <- c(
  "Denied",
  "tract_minority_population_percent",
  "tract_to_msa_income_percentage",
  "income_num",
  "loan_amount_num",
  "dti_num",
  "ltv_num",
  "loan_term_num",
  "occupancy_factor",
  "construction_factor",
  "conforming_factor",
  "business_factor"
)

analysis_sample <- decision_sample[
  complete.cases(decision_sample[, analysis_vars]),
]

# Verify sample size
nrow(analysis_sample)

# Verify denial outcome
table(analysis_sample$Denied)

# Denial rate in common analytical sample
mean(analysis_sample$Denied) * 100



# STEP 5B: VERIFY FOCAL TRACT VARIABLES

summary(as.numeric(
  analysis_sample$tract_minority_population_percent
))

summary(as.numeric(
  analysis_sample$tract_to_msa_income_percentage
))



# STEP 6A: CREATE NUMERIC FOCAL TRACT VARIABLES

analysis_sample$tract_minority_pct <- as.numeric(
  analysis_sample$tract_minority_population_percent
)

analysis_sample$tract_income_pct <- as.numeric(
  analysis_sample$tract_to_msa_income_percentage
)

# Final verification
sum(is.na(analysis_sample$tract_minority_pct))
sum(is.na(analysis_sample$tract_income_pct))


# STEP 6B: DESCRIPTIVE STATISTICS BY DENIAL STATUS

aggregate(
  cbind(
    tract_minority_pct,
    tract_income_pct,
    income_num,
    loan_amount_num,
    dti_num,
    ltv_num,
    loan_term_num
  ) ~ Denied,
  data = analysis_sample,
  FUN = mean
)

# Correlation between the two focal tract variables
cor(
  analysis_sample$tract_minority_pct,
  analysis_sample$tract_income_pct
)


# STEP 7A: MODEL 1 — BASELINE GEOGRAPHIC MODEL

model1 <- glm(
  Denied ~ tract_minority_pct + tract_income_pct,
  data = analysis_sample,
  family = binomial(link = "logit")
)

summary(model1)


# Odds ratios and 95% confidence intervals

model1_or <- cbind(
  Odds_Ratio = exp(coef(model1)),
  exp(confint.default(model1))
)

model1_or


# STEP 7B: MODEL 2 — PRIMARY ADJUSTED LOGISTIC MODEL

model2 <- glm(
  Denied ~
    tract_minority_pct +
    tract_income_pct +
    income_num +
    loan_amount_num +
    dti_num +
    ltv_num +
    loan_term_num,
  data = analysis_sample,
  family = binomial(link = "logit")
)

summary(model2)

# MODEL 2 ODDS RATIOS AND 95% CIs

model2_or <- cbind(
  Odds_Ratio = exp(coef(model2)),
  exp(confint.default(model2))
)

model2_or

nobs(model2)


# STEP 7C: MODEL 3 — FORWARD AIC SENSITIVITY ANALYSIS

# Starting model: focal geographic variables are forced in
model3_start <- glm(
  Denied ~ tract_minority_pct + tract_income_pct,
  data = analysis_sample,
  family = binomial(link = "logit")
)

# Full candidate model
model3_full <- glm(
  Denied ~
    tract_minority_pct +
    tract_income_pct +
    income_num +
    loan_amount_num +
    dti_num +
    ltv_num +
    loan_term_num +
    occupancy_factor +
    construction_factor +
    conforming_factor +
    business_factor,
  data = analysis_sample,
  family = binomial(link = "logit")
)

# Forward AIC selection
model3 <- step(
  model3_start,
  scope = list(
    lower = formula(model3_start),
    upper = formula(model3_full)
  ),
  direction = "forward",
  trace = TRUE
)

# Variables selected
formula(model3)

# Final Model 3 results
summary(model3)

# Odds ratios and 95% CIs
model3_or <- cbind(
  Odds_Ratio = exp(coef(model3)),
  exp(confint.default(model3))
)

model3_or

# Confirm sample size
nobs(model3)

# Final AIC
AIC(model3)

# STEP 7E: CHECK CATEGORICAL LEVELS AGAINST DENIAL

table(
  analysis_sample$conforming_factor,
  analysis_sample$Denied,
  useNA = "ifany"
)

table(
  analysis_sample$business_factor,
  analysis_sample$Denied,
  useNA = "ifany"
)

table(
  analysis_sample$construction_factor,
  analysis_sample$Denied,
  useNA = "ifany"
)

table(
  analysis_sample$occupancy_factor,
  analysis_sample$Denied,
  useNA = "ifany"
)

# Check fitted probabilities from Model 3
range(fitted(model3))

sum(fitted(model3) < 1e-8)
sum(fitted(model3) > 1 - 1e-8)


# STEP 7F: MODEL 3 SENSITIVITY — REMOVE SPARSE U LEVEL

analysis_stepwise <- droplevels(
  analysis_sample[analysis_sample$conforming_factor != "U", ]
)

# Verify exactly what changed
nrow(analysis_stepwise)
table(analysis_stepwise$conforming_factor)
table(analysis_stepwise$Denied)



# STEP 7G: RERUN MODEL 3 WITHOUT SPARSE U LEVEL

model3_start_sens <- glm(
  Denied ~ tract_minority_pct + tract_income_pct,
  data = analysis_stepwise,
  family = binomial(link = "logit")
)

model3_full_sens <- glm(
  Denied ~
    tract_minority_pct +
    tract_income_pct +
    income_num +
    loan_amount_num +
    dti_num +
    ltv_num +
    loan_term_num +
    occupancy_factor +
    construction_factor +
    conforming_factor +
    business_factor,
  data = analysis_stepwise,
  family = binomial(link = "logit")
)

model3_sens <- step(
  model3_start_sens,
  scope = list(
    lower = formula(model3_start_sens),
    upper = formula(model3_full_sens)
  ),
  direction = "forward",
  trace = TRUE
)


formula(model3_sens)

summary(model3_sens)

model3_sens_or <- cbind(
  Odds_Ratio = exp(coef(model3_sens)),
  exp(confint.default(model3_sens))
)

model3_sens_or

nobs(model3_sens)

AIC(model3_sens)


# STEP 7H: INVESTIGATE REMAINING GLM WARNING

# Range of linear predictors
range(predict(model3_sens, type = "link"))

# Range of fitted probabilities
range(fitted(model3_sens))

# Count extremely small / large fitted probabilities
sum(fitted(model3_sens) < 1e-8)
sum(fitted(model3_sens) > 1 - 1e-8)

# Identify observations with the most extreme fitted probabilities
head(
  analysis_stepwise[
    order(fitted(model3_sens)),
    c(
      "Denied",
      "tract_minority_pct",
      "tract_income_pct",
      "income_num",
      "loan_amount_num",
      "dti_num",
      "ltv_num",
      "loan_term_num",
      "occupancy_factor",
      "construction_factor",
      "conforming_factor",
      "business_factor"
    )
  ],
  10
)

head(
  analysis_stepwise[
    order(fitted(model3_sens), decreasing = TRUE),
    c(
      "Denied",
      "tract_minority_pct",
      "tract_income_pct",
      "income_num",
      "loan_amount_num",
      "dti_num",
      "ltv_num",
      "loan_term_num",
      "occupancy_factor",
      "construction_factor",
      "conforming_factor",
      "business_factor"
    )
  ],
  10
)


# STEP 7I: REFIT FINAL MODEL 3 DIRECTLY

model3_final <- glm(
  Denied ~
    tract_minority_pct +
    tract_income_pct +
    dti_num +
    business_factor +
    construction_factor +
    loan_term_num +
    conforming_factor +
    loan_amount_num +
    occupancy_factor +
    ltv_num,
  data = analysis_stepwise,
  family = binomial(link = "logit")
)

summary(model3_final)

range(fitted(model3_final))

AIC(model3_final)

nobs(model3_final)


# STEP 8A: MULTICOLLINEARITY DIAGNOSTIC FOR PRIMARY MODEL 2

# Install only if needed:
# install.packages("car")

library(car)

vif(model2)

# STEP 8B: INFLUENCE DIAGNOSTICS FOR PRIMARY MODEL 2

# Cook's distance
cooks_d <- cooks.distance(model2)

summary(cooks_d)

# Largest Cook's distance
max(cooks_d)

# Observation with largest Cook's distance
which.max(cooks_d)

# Number exceeding common 4/n screening threshold
cook_cutoff <- 4 / nobs(model2)
cook_cutoff

sum(cooks_d > cook_cutoff)



# STEP 8C: INSPECT MOST INFLUENTIAL MODEL 2 OBSERVATIONS

top_cook <- order(cooks_d, decreasing = TRUE)[1:10]

data.frame(
  row = top_cook,
  cooks_d = cooks_d[top_cook],
  analysis_sample[
    top_cook,
    c(
      "Denied",
      "tract_minority_pct",
      "tract_income_pct",
      "income_num",
      "loan_amount_num",
      "dti_num",
      "ltv_num",
      "loan_term_num"
    )
  ]
)


# DFBETAS for the two focal geographic coefficients

dfb <- dfbetas(model2)

max(abs(dfb[, "tract_minority_pct"]))
which.max(abs(dfb[, "tract_minority_pct"]))

max(abs(dfb[, "tract_income_pct"]))
which.max(abs(dfb[, "tract_income_pct"]))


# STEP 8D: CHECK LINEARITY IN THE LOGIT

analysis_sample$dti_logterm <-
  analysis_sample$dti_num * log(analysis_sample$dti_num)

analysis_sample$ltv_logterm <-
  analysis_sample$ltv_num * log(analysis_sample$ltv_num)

analysis_sample$loan_term_logterm <-
  analysis_sample$loan_term_num * log(analysis_sample$loan_term_num)

analysis_sample$loan_amount_logterm <-
  analysis_sample$loan_amount_num * log(analysis_sample$loan_amount_num)

linearity_model <- glm(
  Denied ~
    tract_minority_pct +
    tract_income_pct +
    income_num +
    loan_amount_num +
    dti_num +
    ltv_num +
    loan_term_num +
    loan_amount_logterm +
    dti_logterm +
    ltv_logterm +
    loan_term_logterm,
  data = analysis_sample,
  family = binomial(link = "logit")
)

summary(linearity_model)


# STEP 8E: NONLINEAR FUNCTIONAL-FORM ROBUSTNESS CHECK

library(splines)

model2_spline <- glm(
  Denied ~
    tract_minority_pct +
    tract_income_pct +
    income_num +
    ns(loan_amount_num, df = 3) +
    ns(dti_num, df = 3) +
    ns(ltv_num, df = 3) +
    ns(loan_term_num, df = 3),
  data = analysis_sample,
  family = binomial(link = "logit")
)

summary(model2_spline)

AIC(model2)
AIC(model2_spline)

nobs(model2_spline)

coef(summary(model2_spline))[
  c("tract_minority_pct", "tract_income_pct"),
]

exp(cbind(
  OR = coef(model2_spline),
  confint.default(model2_spline)
))[c("tract_minority_pct", "tract_income_pct"), ]


# STEP 8F: 10-PERCENTAGE-POINT EFFECT FOR TRACT MINORITY COMPOSITION

# Primary Model 2
beta_m2 <- coef(model2)["tract_minority_pct"]
se_m2 <- coef(summary(model2))["tract_minority_pct", "Std. Error"]

c(
  OR_10pp = exp(10 * beta_m2),
  CI_low = exp(10 * (beta_m2 - 1.96 * se_m2)),
  CI_high = exp(10 * (beta_m2 + 1.96 * se_m2))
)

# Nonlinear robustness model
beta_spline <- coef(model2_spline)["tract_minority_pct"]
se_spline <- coef(summary(model2_spline))[
  "tract_minority_pct", "Std. Error"
]

c(
  OR_10pp = exp(10 * beta_spline),
  CI_low = exp(10 * (beta_spline - 1.96 * se_spline)),
  CI_high = exp(10 * (beta_spline + 1.96 * se_spline))
)


# STEP 8G: CALIBRATION OF PRIMARY MODEL 2

analysis_sample$pred_m2 <- fitted(model2)

# Divide observations into 10 groups by predicted probability
analysis_sample$calibration_group <- cut(
  analysis_sample$pred_m2,
  breaks = quantile(
    analysis_sample$pred_m2,
    probs = seq(0, 1, 0.1),
    na.rm = TRUE
  ),
  include.lowest = TRUE,
  duplicates = "drop"
)

calibration_table <- aggregate(
  cbind(
    Observed = analysis_sample$Denied,
    Predicted = analysis_sample$pred_m2
  ) ~ calibration_group,
  data = analysis_sample,
  FUN = mean
)

calibration_table$N <- as.vector(
  table(analysis_sample$calibration_group)
)

calibration_table


# STEP 8H: CALIBRATION OF NONLINEAR SPLINE MODEL

analysis_sample$pred_spline <- fitted(model2_spline)

analysis_sample$calibration_group_spline <- cut(
  analysis_sample$pred_spline,
  breaks = quantile(
    analysis_sample$pred_spline,
    probs = seq(0, 1, 0.1),
    na.rm = TRUE
  ),
  include.lowest = TRUE,
  duplicates = "drop"
)

calibration_spline <- aggregate(
  cbind(
    Observed = analysis_sample$Denied,
    Predicted = analysis_sample$pred_spline
  ) ~ calibration_group_spline,
  data = analysis_sample,
  FUN = mean
)

calibration_spline$N <- as.vector(
  table(analysis_sample$calibration_group_spline)
)

calibration_spline

# STEP 8I: VERIFY CENSUS-TRACT CLUSTER IDENTIFIER

length(unique(analysis_sample$census_tract))

sum(is.na(analysis_sample$census_tract))

summary(table(analysis_sample$census_tract))

head(
  sort(
    table(analysis_sample$census_tract),
    decreasing = TRUE
  ),
  10
)

# STEP 8J: CENSUS-TRACT CLUSTERED STANDARD ERRORS
install.packages("sandwich")
library(sandwich)
library(lmtest)

# Sensitivity sample with observed census tract
cluster_sample <- analysis_sample[
  !is.na(analysis_sample$census_tract),
]

nrow(cluster_sample)
length(unique(cluster_sample$census_tract))

# Refit the primary specification on exactly this sample
model2_cluster_sample <- glm(
  Denied ~
    tract_minority_pct +
    tract_income_pct +
    income_num +
    loan_amount_num +
    dti_num +
    ltv_num +
    loan_term_num,
  data = cluster_sample,
  family = binomial(link = "logit")
)

# Ordinary inference on the same 64,490 observations
coef(summary(model2_cluster_sample))[
  c("tract_minority_pct", "tract_income_pct"),
]

# Census-tract clustered covariance matrix
vcov_tract <- vcovCL(
  model2_cluster_sample,
  cluster = ~ census_tract,
  type = "HC1"
)

# Cluster-robust inference
cluster_results <- coeftest(
  model2_cluster_sample,
  vcov. = vcov_tract
)

cluster_results[
  c("tract_minority_pct", "tract_income_pct"),
]


# STEP 8K: 10-PP TRACT-MINORITY EFFECT WITH CLUSTERED SE

beta_cluster <- cluster_results[
  "tract_minority_pct", "Estimate"
]

se_cluster <- cluster_results[
  "tract_minority_pct", "Std. Error"
]

c(
  OR_10pp = exp(10 * beta_cluster),
  CI_low = exp(10 * (beta_cluster - 1.96 * se_cluster)),
  CI_high = exp(10 * (beta_cluster + 1.96 * se_cluster))
)

# STEP 8L: COMPLETE-CASE SELECTION DIAGNOSTIC

decision_sample$Included_Analysis <- ifelse(
  rownames(decision_sample) %in% rownames(analysis_sample),
  1,
  0
)

table(decision_sample$Included_Analysis)

prop.table(
  table(
    decision_sample$Included_Analysis,
    decision_sample$Denied
  ),
  margin = 1
)

# Missingness by denial status
missing_by_denial <- aggregate(
  cbind(
    income_missing = is.na(income_num),
    dti_missing = is.na(dti_num),
    ltv_missing = is.na(ltv_num),
    loan_term_missing = is.na(loan_term_num),
    conforming_missing = is.na(conforming_factor)
  ) ~ Denied,
  data = decision_sample,
  FUN = mean
)

missing_by_denial

# STEP 8L CORRECTION: EXACT COMPLETE-CASE INDICATOR

decision_sample$Included_Analysis <- as.integer(
  complete.cases(decision_sample[, analysis_vars])
)

table(decision_sample$Included_Analysis)

table(
  decision_sample$Included_Analysis,
  decision_sample$Denied
)

prop.table(
  table(
    decision_sample$Included_Analysis,
    decision_sample$Denied
  ),
  margin = 1
)

sum(decision_sample$Included_Analysis)
sum(decision_sample$Included_Analysis == 0)

mean(
  decision_sample$Denied[
    decision_sample$Included_Analysis == 1
  ]
)

mean(
  decision_sample$Denied[
    decision_sample$Included_Analysis == 0
  ]
)


# STEP 8M: INVESTIGATE BUSINESS-PURPOSE CODE 1111 EXCLUSIONS

business_1111 <- decision_sample[
  decision_sample$business_or_commercial_purpose == "1111",
]

nrow(business_1111)

table(business_1111$Denied)

c(
  income_missing = sum(is.na(business_1111$income_num)),
  dti_missing = sum(is.na(business_1111$dti_num)),
  ltv_missing = sum(is.na(business_1111$ltv_num)),
  loan_term_missing = sum(is.na(business_1111$loan_term_num)),
  conforming_missing = sum(is.na(business_1111$conforming_factor))
)

# Number satisfying ALL Model 3 complete-case requirements
sum(
  complete.cases(
    business_1111[, analysis_vars]
  )
)


# STEP 8N: VERIFY FINAL MODEL 3 FACTOR LEVELS AND SAMPLE

nobs(model3_final)

levels(droplevels(model.frame(model3_final)$business_factor))
table(model.frame(model3_final)$business_factor)

levels(droplevels(model.frame(model3_final)$conforming_factor))
table(model.frame(model3_final)$conforming_factor)

levels(droplevels(model.frame(model3_final)$construction_factor))
table(model.frame(model3_final)$construction_factor)

levels(droplevels(model.frame(model3_final)$occupancy_factor))
table(model.frame(model3_final)$occupancy_factor)


# STEP 8O: ROC/AUC DISCRIMINATION

library(pROC)

# Primary Model 2
roc_m2 <- roc(
  response = analysis_sample$Denied,
  predictor = fitted(model2),
  levels = c(0, 1),
  direction = "<",
  quiet = TRUE
)

auc(roc_m2)
ci.auc(roc_m2)

# Nonlinear spline robustness model
roc_spline <- roc(
  response = analysis_sample$Denied,
  predictor = fitted(model2_spline),
  levels = c(0, 1),
  direction = "<",
  quiet = TRUE
)

auc(roc_spline)
ci.auc(roc_spline)

# Compare AUCs because both models use the same observations
roc.test(
  roc_m2,
  roc_spline,
  paired = TRUE,
  method = "delong"
)


#8p Refit reduced model on the full decision sample
reduced_full <- glm(
  Denied ~
    tract_minority_pct +
    tract_income_pct +
    loan_amount_num,
  data = decision_sample,
  family = binomial(link = "logit")
)

nobs(reduced_full)

coef(summary(reduced_full))[
  c("tract_minority_pct", "tract_income_pct"),
]

# Compare 10-percentage-point tract-minority ORs
c(
  Full_decision_sample =
    exp(10 * coef(reduced_full)["tract_minority_pct"]),
  
  Complete_case_sample =
    exp(10 * coef(reduced_complete)["tract_minority_pct"])
)


# STEP 8Q: DIAGNOSE WARNING IN FULL-SAMPLE REDUCED MODEL

range(fitted(reduced_full))

sum(fitted(reduced_full) < 1e-8)
sum(fitted(reduced_full) > 1 - 1e-8)

range(predict(reduced_full, type = "link"))

# Identify observations with the most extreme fitted probabilities
head(
  decision_sample[
    order(fitted(reduced_full)),
    c(
      "Denied",
      "tract_minority_pct",
      "tract_income_pct",
      "loan_amount_num"
    )
  ],
  10
)

head(
  decision_sample[
    order(fitted(reduced_full), decreasing = TRUE),
    c(
      "Denied",
      "tract_minority_pct",
      "tract_income_pct",
      "loan_amount_num"
    )
  ],
  10
)


# STEP 8R: NONLINEAR REDUCED-MODEL SELECTION SENSITIVITY

library(splines)

reduced_full_spline <- glm(
  Denied ~
    tract_minority_pct +
    tract_income_pct +
    ns(loan_amount_num, df = 3),
  data = decision_sample,
  family = binomial(link = "logit")
)

reduced_complete_spline <- glm(
  Denied ~
    tract_minority_pct +
    tract_income_pct +
    ns(loan_amount_num, df = 3),
  data = analysis_sample,
  family = binomial(link = "logit")
)

nobs(reduced_full_spline)
nobs(reduced_complete_spline)

coef(summary(reduced_full_spline))[
  c("tract_minority_pct", "tract_income_pct"),
]

coef(summary(reduced_complete_spline))[
  c("tract_minority_pct", "tract_income_pct"),
]

# 10-percentage-point tract-minority OR comparison
c(
  Full_decision_spline =
    exp(10 * coef(reduced_full_spline)["tract_minority_pct"]),
  
  Complete_case_spline =
    exp(10 * coef(reduced_complete_spline)["tract_minority_pct"])
)

# Check whether extreme fitted probabilities remain
range(fitted(reduced_full_spline))
sum(fitted(reduced_full_spline) < 1e-8)


# STEP 8S: BRIER SCORE COMPARISON

brier_m2 <- mean(
  (analysis_sample$Denied - fitted(model2))^2
)

brier_spline <- mean(
  (analysis_sample$Denied - fitted(model2_spline))^2
)

c(
  Model2_Brier = brier_m2,
  Spline_Brier = brier_spline
)

# STEP 8T: FINAL MODEL-DIAGNOSTIC SUMMARY

data.frame(
  Model = c("Primary Model 2", "Spline robustness"),
  N = c(nobs(model2), nobs(model2_spline)),
  AIC = c(AIC(model2), AIC(model2_spline)),
  Residual_Deviance = c(deviance(model2), deviance(model2_spline)),
  AUC = c(
    as.numeric(auc(roc_m2)),
    as.numeric(auc(roc_spline))
  ),
  Brier = c(
    brier_m2,
    brier_spline
  ),
  Minority_10pp_OR = c(
    exp(10 * coef(model2)["tract_minority_pct"]),
    exp(10 * coef(model2_spline)["tract_minority_pct"])
  )
)


# STEP 9A: TABLE 1 — CONTINUOUS VARIABLE DESCRIPTIVES

continuous_vars <- c(
  "tract_minority_pct",
  "tract_income_pct",
  "income_num",
  "loan_amount_num",
  "dti_num",
  "ltv_num",
  "loan_term_num"
)

table1_continuous <- data.frame(
  Variable = continuous_vars,
  Mean = sapply(
    analysis_sample[, continuous_vars],
    mean,
    na.rm = TRUE
  ),
  SD = sapply(
    analysis_sample[, continuous_vars],
    sd,
    na.rm = TRUE
  ),
  Median = sapply(
    analysis_sample[, continuous_vars],
    median,
    na.rm = TRUE
  ),
  Min = sapply(
    analysis_sample[, continuous_vars],
    min,
    na.rm = TRUE
  ),
  Max = sapply(
    analysis_sample[, continuous_vars],
    max,
    na.rm = TRUE
  )
)

table1_continuous

# TABLE 1 — OUTCOME

table(analysis_sample$Denied)

prop.table(table(analysis_sample$Denied)) * 100



# STEP 9B: TABLE 1 — CATEGORICAL CHARACTERISTICS

# Occupancy
table(analysis_sample$occupancy_factor)
prop.table(table(analysis_sample$occupancy_factor)) * 100

# Construction method
table(analysis_sample$construction_factor)
prop.table(table(analysis_sample$construction_factor)) * 100

# Conforming-loan-limit SOURCE CODES
table(analysis_sample$conforming_factor)
prop.table(table(analysis_sample$conforming_factor)) * 100

# Business-purpose SOURCE CODES
table(analysis_sample$business_factor)
prop.table(table(analysis_sample$business_factor)) * 100


# STEP 9C: CONTINUOUS CHARACTERISTICS BY DENIAL STATUS

table1_by_denial <- aggregate(
  analysis_sample[, continuous_vars],
  by = list(Denied = analysis_sample$Denied),
  FUN = mean
)

table1_by_denial

table1_sd_by_denial <- aggregate(
  analysis_sample[, continuous_vars],
  by = list(Denied = analysis_sample$Denied),
  FUN = sd
)

table1_sd_by_denial


# STEP 9D: CATEGORICAL CHARACTERISTICS BY DENIAL STATUS

# Occupancy
table(analysis_sample$Denied, analysis_sample$occupancy_factor)
prop.table(
  table(analysis_sample$Denied, analysis_sample$occupancy_factor),
  margin = 1
) * 100

# Construction
table(analysis_sample$Denied, analysis_sample$construction_factor)
prop.table(
  table(analysis_sample$Denied, analysis_sample$construction_factor),
  margin = 1
) * 100

# Conforming-loan-limit source codes
table(analysis_sample$Denied, analysis_sample$conforming_factor)
prop.table(
  table(analysis_sample$Denied, analysis_sample$conforming_factor),
  margin = 1
) * 100

# Business-purpose source codes
table(analysis_sample$Denied, analysis_sample$business_factor)
prop.table(
  table(analysis_sample$Denied, analysis_sample$business_factor),
  margin = 1
) * 100

# STEP 9E: TABLE 2 — MODELS 1 AND 2 RESULTS

extract_logit <- function(model) {
  s <- coef(summary(model))
  
  data.frame(
    Variable = rownames(s),
    Beta = s[, "Estimate"],
    SE = s[, "Std. Error"],
    OR = exp(s[, "Estimate"]),
    CI_Lower = exp(s[, "Estimate"] - 1.96 * s[, "Std. Error"]),
    CI_Upper = exp(s[, "Estimate"] + 1.96 * s[, "Std. Error"]),
    P_Value = s[, "Pr(>|z|)"],
    row.names = NULL
  )
}

table2_model1 <- extract_logit(model1)
table2_model2 <- extract_logit(model2)

table2_model1
table2_model2

# Model fit statistics
data.frame(
  Model = c("Model 1", "Model 2"),
  N = c(nobs(model1), nobs(model2)),
  AIC = c(AIC(model1), AIC(model2)),
  Residual_Deviance = c(deviance(model1), deviance(model2))
)


# STEP 9F: TABLE 2 — MODEL 3 SENSITIVITY RESULTS

table2_model3 <- extract_logit(model3_final)

table2_model3

data.frame(
  Model = "Model 3 sensitivity",
  N = nobs(model3_final),
  AIC = AIC(model3_final),
  Residual_Deviance = deviance(model3_final)
)


# STEP 9G: COMBINED REGRESSION RESULTS TABLE

format_result <- function(beta, se, or, lower, upper, p) {
  
  p_text <- ifelse(
    p < 0.001,
    "<.001",
    sprintf("%.3f", p)
  )
  
  sprintf(
    "%.3f (%.3f)\nOR %.3f [%.3f, %.3f]\np %s",
    beta, se, or, lower, upper, p_text
  )
}

table2_model1$Model1 <- with(
  table2_model1,
  format_result(Beta, SE, OR, CI_Lower, CI_Upper, P_Value)
)

table2_model2$Model2 <- with(
  table2_model2,
  format_result(Beta, SE, OR, CI_Lower, CI_Upper, P_Value)
)

table2_model3$Model3 <- with(
  table2_model3,
  format_result(Beta, SE, OR, CI_Lower, CI_Upper, P_Value)
)

all_variables <- unique(c(
  table2_model1$Variable,
  table2_model2$Variable,
  table2_model3$Variable
))

combined_table2 <- data.frame(
  Variable = all_variables
)

combined_table2 <- merge(
  combined_table2,
  table2_model1[, c("Variable", "Model1")],
  by = "Variable",
  all.x = TRUE,
  sort = FALSE
)

combined_table2 <- merge(
  combined_table2,
  table2_model2[, c("Variable", "Model2")],
  by = "Variable",
  all.x = TRUE,
  sort = FALSE
)

combined_table2 <- merge(
  combined_table2,
  table2_model3[, c("Variable", "Model3")],
  by = "Variable",
  all.x = TRUE,
  sort = FALSE
)

combined_table2



# STEP 9H: PUBLICATION-READY TABLE 2
# Values are taken directly from the fitted model objects.

pretty_names <- c(
  "(Intercept)" = "Intercept",
  "tract_minority_pct" = "Tract minority population (%)",
  "tract_income_pct" = "Tract-to-MSA income (%)",
  "income_num" = "Applicant income",
  "loan_amount_num" = "Loan amount",
  "dti_num" = "Debt-to-income ratio",
  "ltv_num" = "Loan-to-value ratio",
  "loan_term_num" = "Loan term (months)",
  "construction_factorManufactured home" = "Construction: Manufactured home",
  "conforming_factorNC" = "Conforming-loan-limit code: NC",
  "business_factor2" = "Business-purpose code: 2",
  "occupancy_factorSecond residence" = "Occupancy: Second residence",
  "occupancy_factorInvestment property" = "Occupancy: Investment property"
)

# Desired row order
row_order <- names(pretty_names)

# Publication formatting
format_pub <- function(beta, se, or, lower, upper, p) {
  
  p_text <- ifelse(
    p < 0.001,
    "< .001",
    sprintf("%.3f", p)
  )
  
  # Use scientific notation where ordinary rounding would hide the estimate
  if (abs(beta) < 0.001 && beta != 0) {
    beta_text <- formatC(beta, format = "e", digits = 2)
    se_text   <- formatC(se, format = "e", digits = 2)
  } else {
    beta_text <- sprintf("%.3f", beta)
    se_text   <- sprintf("%.3f", se)
  }
  
  # More precision for odds ratios extremely close to 1
  if (abs(or - 1) < 0.001) {
    or_text    <- sprintf("%.6f", or)
    lower_text <- sprintf("%.6f", lower)
    upper_text <- sprintf("%.6f", upper)
  } else {
    or_text    <- sprintf("%.3f", or)
    lower_text <- sprintf("%.3f", lower)
    upper_text <- sprintf("%.3f", upper)
  }
  
  paste0(
    beta_text, " (", se_text, "); ",
    "OR ", or_text,
    " [", lower_text, ", ", upper_text, "]; ",
    "p ", p_text
  )
}

make_pub_column <- function(x) {
  setNames(
    mapply(
      format_pub,
      x$Beta,
      x$SE,
      x$OR,
      x$CI_Lower,
      x$CI_Upper,
      x$P_Value,
      USE.NAMES = FALSE
    ),
    x$Variable
  )
}

m1 <- make_pub_column(table2_model1)
m2 <- make_pub_column(table2_model2)
m3 <- make_pub_column(table2_model3)

Table2_final <- data.frame(
  Variable = unname(pretty_names[row_order]),
  `Model 1: Geographic` =
    unname(m1[row_order]),
  `Model 2: Primary adjusted` =
    unname(m2[row_order]),
  `Model 3: AIC sensitivity` =
    unname(m3[row_order]),
  check.names = FALSE
)

# Replace variables not included in a model with an em dash
Table2_final[is.na(Table2_final)] <- "\u2014"

print(Table2_final, row.names = FALSE)

# Model-fit information
Table2_fit <- data.frame(
  Statistic = c(
    "N",
    "AIC",
    "Residual deviance"
  ),
  `Model 1: Geographic` = c(
    nobs(model1),
    AIC(model1),
    deviance(model1)
  ),
  `Model 2: Primary adjusted` = c(
    nobs(model2),
    AIC(model2),
    deviance(model2)
  ),
  `Model 3: AIC sensitivity` = c(
    nobs(model3_final),
    AIC(model3_final),
    deviance(model3_final)
  ),
  check.names = FALSE
)

print(Table2_fit, row.names = FALSE)



# STEP 9I: FINAL PRECISION FOR TABLE 2
# Formatting only — models and estimates are NOT changed.

format_pub_final <- function(beta, se, or, lower, upper, p) {
  
  p_text <- ifelse(
    p < 0.001,
    "< .001",
    sprintf("%.3f", p)
  )
  
  # Coefficient and SE precision
  if (abs(beta) < 0.0001 && beta != 0) {
    beta_text <- formatC(beta, format = "e", digits = 2)
  } else if (abs(beta) < 0.01) {
    beta_text <- sprintf("%.5f", beta)
  } else {
    beta_text <- sprintf("%.3f", beta)
  }
  
  if (se < 0.0001) {
    se_text <- formatC(se, format = "e", digits = 2)
  } else if (se < 0.01) {
    se_text <- sprintf("%.5f", se)
  } else {
    se_text <- sprintf("%.3f", se)
  }
  
  # OR precision
  if (abs(or - 1) < 0.001) {
    or_text    <- sprintf("%.6f", or)
    lower_text <- sprintf("%.6f", lower)
    upper_text <- sprintf("%.6f", upper)
  } else {
    or_text    <- sprintf("%.3f", or)
    lower_text <- sprintf("%.3f", lower)
    upper_text <- sprintf("%.3f", upper)
  }
  
  paste0(
    beta_text, " (", se_text, "); ",
    "OR ", or_text,
    " [", lower_text, ", ", upper_text, "]; ",
    "p ", p_text
  )
}

make_pub_column_final <- function(x) {
  setNames(
    mapply(
      format_pub_final,
      x$Beta,
      x$SE,
      x$OR,
      x$CI_Lower,
      x$CI_Upper,
      x$P_Value,
      USE.NAMES = FALSE
    ),
    x$Variable
  )
}

m1_final <- make_pub_column_final(table2_model1)
m2_final <- make_pub_column_final(table2_model2)
m3_final <- make_pub_column_final(table2_model3)

Table2_final <- data.frame(
  Variable = unname(pretty_names[row_order]),
  `Model 1: Geographic` = unname(m1_final[row_order]),
  `Model 2: Primary adjusted` = unname(m2_final[row_order]),
  `Model 3: AIC sensitivity` = unname(m3_final[row_order]),
  check.names = FALSE
)

Table2_final[is.na(Table2_final)] <- "\u2014"

print(Table2_final, row.names = FALSE)



# FINAL TABLE 2 PRECISION FIX — formatting only

format_pub_final2 <- function(beta, se, or, lower, upper, p) {
  
  p_text <- ifelse(
    p < 0.001,
    "< .001",
    sprintf("%.3f", p)
  )
  
  if (abs(beta) < 0.0001 && beta != 0) {
    beta_text <- formatC(beta, format = "e", digits = 2)
  } else if (abs(beta) < 0.01) {
    beta_text <- sprintf("%.5f", beta)
  } else {
    beta_text <- sprintf("%.3f", beta)
  }
  
  if (se < 0.0001) {
    se_text <- formatC(se, format = "e", digits = 2)
  } else if (se < 0.01) {
    se_text <- sprintf("%.5f", se)
  } else {
    se_text <- sprintf("%.3f", se)
  }
  
  # Show more precision whenever OR is close to 1
  if (or > 0.995 && or < 1.005) {
    or_text    <- sprintf("%.6f", or)
    lower_text <- sprintf("%.6f", lower)
    upper_text <- sprintf("%.6f", upper)
  } else {
    or_text    <- sprintf("%.3f", or)
    lower_text <- sprintf("%.3f", lower)
    upper_text <- sprintf("%.3f", upper)
  }
  
  paste0(
    beta_text, " (", se_text, "); ",
    "OR ", or_text,
    " [", lower_text, ", ", upper_text, "]; ",
    "p ", p_text
  )
}

make_pub_column_final2 <- function(x) {
  setNames(
    mapply(
      format_pub_final2,
      x$Beta, x$SE, x$OR,
      x$CI_Lower, x$CI_Upper, x$P_Value,
      USE.NAMES = FALSE
    ),
    x$Variable
  )
}

m1_final <- make_pub_column_final2(table2_model1)
m2_final <- make_pub_column_final2(table2_model2)
m3_final <- make_pub_column_final2(table2_model3)

Table2_final <- data.frame(
  Variable = unname(pretty_names[row_order]),
  `Model 1: Geographic` = unname(m1_final[row_order]),
  `Model 2: Primary adjusted` = unname(m2_final[row_order]),
  `Model 3: AIC sensitivity` = unname(m3_final[row_order]),
  check.names = FALSE
)

Table2_final[is.na(Table2_final)] <- "\u2014"

print(Table2_final, row.names = FALSE)



# STEP 10A: ROBUSTNESS AND DIAGNOSTIC SUMMARY TABLE
# Uses only previously verified results

Table3_summary <- data.frame(
  Analysis = c(
    "Primary logistic model",
    "Spline robustness model",
    "Census-tract clustered inference",
    "Primary model discrimination",
    "Spline model discrimination",
    "Primary model calibration error",
    "Spline model calibration error",
    "Complete-case selection",
    "Reduced model: full decision sample",
    "Reduced model: complete-case sample"
  ),
  
  Result = c(
    "Tract minority: 10-pp OR = 1.089676",
    "Tract minority: 10-pp OR = 1.095095",
    "Tract minority: 10-pp OR = 1.091397",
    "AUC = 0.7088729",
    "AUC = 0.7661609",
    "Brier score = 0.07296275",
    "Brier score = 0.06576696",
    "Excluded denial rate = 26.290672%; included = 9.038926%",
    "Tract minority: 10-pp OR = 1.050278",
    "Tract minority: 10-pp OR = 1.123286"
  ),
  
  Detail = c(
    "95% CI: 1.078636–1.100829",
    "95% CI: 1.082997–1.107328",
    "95% CI: 1.073546–1.109546",
    "95% CI: 0.7006–0.7171",
    "95% CI: 0.7590–0.7733",
    "N = 64,532",
    "N = 64,532",
    "Excluded N = 9,220; included N = 64,532",
    "N = 73,752",
    "N = 64,532"
  ),
  
  stringsAsFactors = FALSE
)

print(Table3_summary, row.names = FALSE)



# STEP 10B: ADD TRACT-INCOME ROBUSTNESS RESULTS

Table3_income <- data.frame(
  Analysis = c(
    "Primary logistic model — tract income",
    "Spline robustness model — tract income",
    "Census-tract clustered inference — tract income"
  ),
  
  Result = c(
    "Beta = -0.001219; OR = 0.998782; p = .0116",
    "Beta = -0.0002695; OR = 0.999731; p = .6046",
    "Beta = -0.0010321; p = .1651"
  ),
  
  Detail = c(
    "95% CI for OR: 0.997837–0.999728",
    "95% CI for OR: 0.998711–1.000751",
    "Cluster-robust SE = 0.0007435; N = 64,490"
  ),
  
  stringsAsFactors = FALSE
)

Table3_final <- rbind(
  Table3_summary,
  Table3_income
)

print(Table3_final, row.names = FALSE)





# STEP 11A: FIGURE 1
# Observed denial rate by tract minority-population composition

# Create 10 groups based on tract minority-population percentage
analysis_sample$minority_decile <- cut(
  analysis_sample$tract_minority_pct,
  breaks = quantile(
    analysis_sample$tract_minority_pct,
    probs = seq(0, 1, 0.1),
    na.rm = TRUE
  ),
  include.lowest = TRUE,
  duplicates = "drop"
)

# Calculate observed denial rate within each group
figure1_data <- aggregate(
  cbind(Denied, tract_minority_pct) ~ minority_decile,
  data = analysis_sample,
  FUN = mean
)

# Number of observations in each group
figure1_n <- aggregate(
  Denied ~ minority_decile,
  data = analysis_sample,
  FUN = length
)

figure1_data$N <- figure1_n$Denied

names(figure1_data)[2:3] <- c(
  "Observed_Denial_Rate",
  "Mean_Tract_Minority_Pct"
)

print(figure1_data)

# Plot
plot(
  figure1_data$Mean_Tract_Minority_Pct,
  figure1_data$Observed_Denial_Rate * 100,
  type = "b",
  pch = 19,
  xlab = "Mean census-tract minority population (%)",
  ylab = "Observed mortgage denial rate (%)",
  main = "Observed Mortgage Denial Rate by\nCensus-Tract Minority Population Composition"
)



# STEP 11B: FINAL PUBLICATION FIGURE 1
# Uses the already-verified figure1_data

plot(
  figure1_data$Mean_Tract_Minority_Pct,
  figure1_data$Observed_Denial_Rate * 100,
  pch = 19,
  xlab = "Mean census-tract minority population (%)",
  ylab = "Observed mortgage denial rate (%)",
  main = "Observed Mortgage Denial Rate by\nCensus-Tract Minority Population Composition",
  ylim = c(0, 16)
)

# Optional light reference grid
grid()

# Redraw points over grid
points(
  figure1_data$Mean_Tract_Minority_Pct,
  figure1_data$Observed_Denial_Rate * 100,
  pch = 19
)


# STEP 11C: FIGURE 2
# Adjusted tract-minority association across robustness specifications

# Primary Model 2
b_primary  <- coef(model2)["tract_minority_pct"]
se_primary <- sqrt(vcov(model2)["tract_minority_pct",
                                "tract_minority_pct"])

# 10-percentage-point OR and 95% CI
or_primary <- exp(10 * b_primary)
lo_primary <- exp(10 * (b_primary - 1.96 * se_primary))
hi_primary <- exp(10 * (b_primary + 1.96 * se_primary))

# Spline model
b_spline  <- coef(model2_spline)["tract_minority_pct"]
se_spline <- sqrt(vcov(model2_spline)["tract_minority_pct",
                                      "tract_minority_pct"])

or_spline <- exp(10 * b_spline)
lo_spline <- exp(10 * (b_spline - 1.96 * se_spline))
hi_spline <- exp(10 * (b_spline + 1.96 * se_spline))

# Display the two model-based estimates first
figure2_check <- data.frame(
  Specification = c(
    "Primary logistic model",
    "Spline robustness model"
  ),
  OR_10pp = c(or_primary, or_spline),
  CI_Lower = c(lo_primary, lo_spline),
  CI_Upper = c(hi_primary, hi_spline)
)

print(figure2_check)



# STEP 11D: VERIFY CLUSTER-ROBUST ESTIMATE FOR FIGURE 2

# Use the same nonmissing-census-tract sample
cluster_sample <- analysis_sample[
  !is.na(analysis_sample$census_tract),
]

# Refit Model 2 on exactly that sample
model2_cluster_sample <- glm(
  Denied ~ tract_minority_pct +
    tract_income_pct +
    income_num +
    loan_amount_num +
    dti_num +
    ltv_num +
    loan_term_num,
  data = cluster_sample,
  family = binomial(link = "logit")
)

# Cluster-robust covariance matrix by census tract
library(sandwich)

vcov_cluster <- vcovCL(
  model2_cluster_sample,
  cluster = ~ census_tract,
  type = "HC1"
)

b_cluster <- coef(model2_cluster_sample)["tract_minority_pct"]

se_cluster <- sqrt(
  vcov_cluster[
    "tract_minority_pct",
    "tract_minority_pct"
  ]
)

# 10-percentage-point OR and 95% CI
or_cluster <- exp(10 * b_cluster)
lo_cluster <- exp(10 * (b_cluster - 1.96 * se_cluster))
hi_cluster <- exp(10 * (b_cluster + 1.96 * se_cluster))

cluster_check <- data.frame(
  N = nobs(model2_cluster_sample),
  Beta = b_cluster,
  Cluster_SE = se_cluster,
  OR_10pp = or_cluster,
  CI_Lower = lo_cluster,
  CI_Upper = hi_cluster
)

print(cluster_check)




# STEP 11E: FINAL FIGURE 2
# Adjusted association of a 10-percentage-point increase
# in tract minority population with mortgage denial odds

figure2_data <- data.frame(
  Specification = c(
    "Primary logistic model",
    "Spline robustness model",
    "Census-tract clustered inference"
  ),
  OR = c(
    or_primary,
    or_spline,
    or_cluster
  ),
  Lower = c(
    lo_primary,
    lo_spline,
    lo_cluster
  ),
  Upper = c(
    hi_primary,
    hi_spline,
    hi_cluster
  )
)

print(figure2_data)

# Reverse order so primary model appears at top
y_pos <- 3:1

plot(
  figure2_data$OR,
  y_pos,
  xlim = c(1.05, 1.13),
  ylim = c(0.5, 3.5),
  yaxt = "n",
  ylab = "",
  xlab = "Odds ratio for a 10-percentage-point increase",
  pch = 19,
  main = "Adjusted Association Between Census-Tract\nMinority Population Composition and Mortgage Denial"
)

axis(
  2,
  at = y_pos,
  labels = figure2_data$Specification,
  las = 1
)

# 95% confidence intervals
segments(
  figure2_data$Lower,
  y_pos,
  figure2_data$Upper,
  y_pos
)

# End caps
segments(
  figure2_data$Lower,
  y_pos - 0.07,
  figure2_data$Lower,
  y_pos + 0.07
)

segments(
  figure2_data$Upper,
  y_pos - 0.07,
  figure2_data$Upper,
  y_pos + 0.07
)

# Null value
abline(v = 1, lty = 2)



# VERIFY THE EXACT PRIMARY COMPLETE-CASE SAMPLE

analysis_vars <- c(
  "Denied",
  "tract_minority_population_percent",
  "tract_to_msa_income_percentage",
  "income_num",
  "loan_amount_num",
  "dti_num",
  "ltv_num",
  "loan_term_num",
  "occupancy_factor",
  "construction_factor",
  "conforming_factor",
  "business_factor"
)

# Recreate primary sample exactly
analysis_sample_check <- decision_sample[
  complete.cases(decision_sample[, analysis_vars]),
]

# Verify N and denial counts
cat("PRIMARY ANALYSIS SAMPLE\n")
cat("N =", nrow(analysis_sample_check), "\n")
cat("Non-denied =", sum(analysis_sample_check$Denied == 0), "\n")
cat("Denied =", sum(analysis_sample_check$Denied == 1), "\n")
cat(
  "Denial rate =",
  100 * mean(analysis_sample_check$Denied),
  "%\n\n"
)

# Check conforming-loan-limit codes in primary sample
cat("CONFORMING CODES IN PRIMARY SAMPLE\n")
print(table(
  analysis_sample_check$conforming_factor,
  analysis_sample_check$Denied,
  useNA = "ifany"
))

# Reproduce Model 3 sample restriction separately
analysis_stepwise_check <- droplevels(
  analysis_sample_check[
    analysis_sample_check$conforming_factor != "U",
  ]
)

cat("\nMODEL 3 SENSITIVITY SAMPLE\n")
cat("N =", nrow(analysis_stepwise_check), "\n")
cat("Non-denied =", sum(analysis_stepwise_check$Denied == 0), "\n")
cat("Denied =", sum(analysis_stepwise_check$Denied == 1), "\n")



# SOFTWARE AND PACKAGE VERSION CHECK

R.version.string

packages <- c(
  "splines",
  "MASS",
  "sandwich",
  "lmtest",
  "pROC"
)

for (pkg in packages) {
  cat(pkg, ": ")
  if (requireNamespace(pkg, quietly = TRUE)) {
    cat(as.character(packageVersion(pkg)), "\n")
  } else {
    cat("Not installed\n")
  }
}

