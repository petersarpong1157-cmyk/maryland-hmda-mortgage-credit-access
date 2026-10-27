# JREL methodological extension — analysis code, not completed findings
# Requires the official, locally stored Maryland 2025 HMDA loan-level CSV.
# Does not modify the existing repository analysis or invent numerical findings.
# Requires packages: readr, dplyr, fixest

input_file <- "loan_purposes_1_state_MD.csv"
output_dir <- "JREL_extension_results"
if (!file.exists(input_file)) stop("Place the verified input CSV in the working directory: ", input_file)
for (p in c("readr", "dplyr", "fixest")) {
  if (!requireNamespace(p, quietly = TRUE)) stop("Install required R package: ", p)
}
dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

raw <- readr::read_csv(input_file,
  col_types = readr::cols(.default = readr::col_character()),
  show_col_types = FALSE)
required <- c("action_taken", "lei", "census_tract", "tract_minority_population_percent",
              "tract_to_msa_income_percentage", "income", "loan_amount",
              "debt_to_income_ratio", "loan_to_value_ratio", "loan_term")
missing <- setdiff(required, names(raw))
if (length(missing)) stop("Required columns absent: ", paste(missing, collapse = ", "))

# Confirm the supplied CSV is indeed purchase-only, rather than assuming its filename proves this.
if ("loan_purpose" %in% names(raw)) {
  print(table(raw$loan_purpose, useNA = "ifany"))
  if (any(!is.na(raw$loan_purpose) & raw$loan_purpose != "1")) {
    stop("Input contains non-purchase applications. Verify extraction before proceeding.")
  }
}

# Do not infer a value for missing debt-to-income ratios. Retain HMDA category structure.
dti_band <- function(x) {
  y <- trimws(as.character(x))
  out <- rep(NA_character_, length(y))
  out[y == "<20%"] <- "lt20"
  out[y == "20%-<30%"] <- "20to30"
  out[y == "30%-<36%"] <- "30to36"
  out[y == "50%-60%"] <- "50to60"
  out[y == ">60%"] <- "gt60"
  n <- suppressWarnings(as.numeric(y))
  idx <- !is.na(n)
  out[idx & n < 20] <- "lt20"
  out[idx & n >= 20 & n < 30] <- "20to30"
  out[idx & n >= 30 & n < 36] <- "30to36"
  out[idx & n >= 36 & n < 50] <- "36to50"
  out[idx & n >= 50 & n <= 60] <- "50to60"
  out[idx & n > 60] <- "gt60"
  factor(out, levels = c("30to36", "lt20", "20to30", "36to50", "50to60", "gt60"))
}

s <- raw |>
  dplyr::filter(action_taken %in% c("1", "2", "3")) |>
  dplyr::mutate(
    denied = as.integer(action_taken == "3"),
    tract_minority_pct = suppressWarnings(as.numeric(tract_minority_population_percent)),
    tract_income_pct = suppressWarnings(as.numeric(tract_to_msa_income_percentage)),
    income_num = suppressWarnings(as.numeric(income)),
    loan_amount_num = suppressWarnings(as.numeric(loan_amount)),
    ltv_num = suppressWarnings(as.numeric(loan_to_value_ratio)),
    term_num = suppressWarnings(as.numeric(loan_term)),
    dti_band = dti_band(debt_to_income_ratio),
    lei = factor(lei), census_tract = factor(census_tract)
  )

# Pre-specify conventional control categories where fields are available.
optional_controls <- intersect(c("loan_type", "occupancy_type", "construction_method",
                                 "lien_status", "derived_dwelling_category"), names(s))
for (v in optional_controls) s[[v]] <- factor(s[[v]])

cols <- c("denied", "tract_minority_pct", "tract_income_pct", "income_num",
          "loan_amount_num", "ltv_num", "term_num", "dti_band", "lei", "census_tract", optional_controls)
complete <- s[stats::complete.cases(s[, cols, drop = FALSE]), , drop = FALSE]
complete <- droplevels(complete)
if (nrow(complete) == 0L) stop("No complete records; inspect missingness and categorical recodes.")
if (length(unique(complete$denied)) < 2) stop("Outcome has one class only.")
if (nlevels(complete$lei) < 2) stop("Need multiple lenders to estimate lender fixed effects.")

# Diagnostic sample attrition, borrower outcome, geography, and lender coverage.
audit <- data.frame(
  measure = c("raw_rows", "lender_decisions", "common_complete_cases", "complete_case_denials",
              "complete_case_lenders", "complete_case_tracts"),
  value = c(nrow(raw), nrow(s), nrow(complete), sum(complete$denied),
            nlevels(complete$lei), nlevels(complete$census_tract))
)
readr::write_csv(audit, file.path(output_dir, "sample_audit.csv"))
missing_summary <- data.frame(
  variable = cols,
  missing_n = vapply(s[, cols, drop = FALSE], function(x) sum(is.na(x)), integer(1)),
  total_n = nrow(s)
)
readr::write_csv(missing_summary, file.path(output_dir, "missingness_audit.csv"))

# SAME-SAMPLE models: changes in coefficient are not driven by changing N.
# 10 percentage point exposure gives directly interpretable log-odds coefficient.
complete$minority10 <- complete$tract_minority_pct / 10
controls <- paste(c("tract_income_pct", "income_num", "splines::ns(loan_amount_num, df=3)",
                    "splines::ns(ltv_num, df=3)", "splines::ns(term_num, df=3)",
                    "dti_band", optional_controls), collapse = " + ")
f_base <- stats::as.formula("denied ~ minority10 + tract_income_pct")
f_adjusted <- stats::as.formula(paste("denied ~ minority10 +", controls))

# With tract clustering, inference recognizes correlated errors inside census tracts.
m1 <- fixest::feglm(f_base, data = complete, family = "binomial", vcov = ~census_tract)
m2 <- fixest::feglm(f_adjusted, data = complete, family = "binomial", vcov = ~census_tract)
f_fe <- stats::as.formula(paste("denied ~ minority10 +", controls, "| lei"))
m3 <- fixest::feglm(f_fe, data = complete, family = "binomial", vcov = ~census_tract)

extract <- function(model, name) {
  ct <- fixest::coeftable(model)
  if (!"minority10" %in% rownames(ct)) stop("Exposure not estimable in ", name)
  b <- unname(ct["minority10", "Estimate"])
  se <- unname(ct["minority10", "Std. Error"])
  data.frame(model = name, n = stats::nobs(model), beta = b, se_clustered = se,
             OR_per_10pp = exp(b), CI_low = exp(b - 1.96*se),
             CI_high = exp(b + 1.96*se),
             note = "Association only; not evidence of discrimination")
}
result <- dplyr::bind_rows(extract(m1, "Geographic_baseline"),
                          extract(m2, "Adjusted"),
                          extract(m3, "Adjusted_lender_FE"))
readr::write_csv(result, file.path(output_dir, "same_sample_lender_comparison.csv"))
saveRDS(list(audit = audit, models = list(m1=m1, m2=m2, m3=m3)),
        file.path(output_dir, "models_and_audit.rds"))
print(audit)
print(result)
cat("\nResults saved in:", normalizePath(output_dir), "\n")
cat("IMPORTANT: Review dropped observations/lenders from FE estimation and verify remaining covariate support.\n")
cat("This script does not resolve omitted numerical credit scores or selection bias.\n")
