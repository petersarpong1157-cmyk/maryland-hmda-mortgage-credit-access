# Maryland 2025 HMDA — JREL revised primary analysis
# Independent clean-session reproduction script; historical script preserved separately.
# R 4.5.2; readr, dplyr, fixest (recorded fixest 0.14.2)
# Run from the folder containing loan_purposes_1_state_MD.csv, or supply its path:
#   Rscript JREL_Reproducibility_Analysis.R /path/to/loan_purposes_1_state_MD.csv
# Source file fingerprint from validated analysis:
#   bfc55246d7307f2f2e44ba4a5e7c12b8 (MD5)
# Association only, not a causal effect or a determination of discrimination.

for (p in c("readr", "dplyr", "fixest")) {
  if (!requireNamespace(p, quietly = TRUE)) stop("Install required package: ", p)
}
args <- commandArgs(trailingOnly = TRUE)
input_file <- if (length(args)) args[[1]] else "loan_purposes_1_state_MD.csv"
if (!file.exists(input_file)) stop("Cannot find source CSV: ", input_file)
outdir <- "JREL_clean_run_results"
dir.create(outdir, recursive = TRUE, showWarnings = FALSE)

expected_md5 <- "bfc55246d7307f2f2e44ba4a5e7c12b8"
actual_md5 <- unname(tools::md5sum(input_file))
if (!identical(tolower(actual_md5), expected_md5)) {
  stop("Input CSV MD5 differs from the validated source. Expected ", expected_md5,
       "; found ", actual_md5, ". No results were produced.")
}

raw <- readr::read_csv(input_file,
                       col_types = readr::cols(.default = readr::col_character()),
                       show_col_types = FALSE)
required <- c("action_taken", "loan_purpose", "lei", "census_tract",
              "tract_minority_population_percent", "tract_to_msa_income_percentage",
              "income", "loan_amount", "loan_to_value_ratio", "loan_term",
              "debt_to_income_ratio", "loan_type", "occupancy_type",
              "construction_method", "lien_status", "derived_dwelling_category",
              "total_units")
missing_cols <- setdiff(required, names(raw))
if (length(missing_cols)) stop("Source missing columns: ", paste(missing_cols, collapse = ", "))
if (any(!is.na(raw$loan_purpose) & raw$loan_purpose != "1"))
  stop("Source includes non-purchase loans; inspect the source dataset.")

# Exact DTI recoding from the original methodological extension.
dti_band_fn <- function(x) {
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

# Original decisions: 1 originated, 2 approved not accepted, 3 denied.
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
    dti_band = dti_band_fn(debt_to_income_ratio)
  )

# Exactly the user-verified restriction. HMDA total_units code '1' means 1-4 units.
r <- s |>
  dplyr::filter(occupancy_type == "1", construction_method == "1",
                lien_status == "1",
                derived_dwelling_category == "Single Family (1-4 Units):Site-Built",
                total_units == "1")

# Original extension complete-case definition includes dwelling category and all listed controls.
optional_controls <- c("loan_type", "occupancy_type", "construction_method",
                       "lien_status", "derived_dwelling_category")
columns_to_check <- c("denied", "tract_minority_pct", "tract_income_pct",
                      "income_num", "loan_amount_num", "ltv_num", "term_num",
                      "dti_band", "lei", "census_tract", optional_controls)
cc <- r[stats::complete.cases(r[, columns_to_check, drop = FALSE]), , drop = FALSE]
cc <- as.data.frame(cc)
cc$minority10 <- cc$tract_minority_pct / 10
cc$dti_band <- factor(cc$dti_band, levels = levels(dti_band_fn(character())))
cc$loan_type <- factor(cc$loan_type)
cc$lei <- factor(cc$lei)
cc$census_tract <- factor(cc$census_tract)

fe_formula <- denied ~ minority10 + tract_income_pct +
  splines::ns(asinh(income_num / 100), df = 3) +
  splines::ns(loan_amount_num, df = 3) +
  splines::ns(ltv_num, df = 3) +
  splines::ns(term_num, df = 3) + dti_band + loan_type | lei
adjusted_formula <- denied ~ minority10 + tract_income_pct +
  splines::ns(asinh(income_num / 100), df = 3) +
  splines::ns(loan_amount_num, df = 3) +
  splines::ns(ltv_num, df = 3) +
  splines::ns(term_num, df = 3) + dti_band + loan_type

# First pass identifies the lenders / observations that the FE estimator can retain.
# IMPORTANT: ns() computes spline knots from the estimation data. The frozen
# manuscript model was fitted on the *already restricted* same_sample.
# We therefore refit BOTH final models on those same 55,913 observations.
selection_fe <- fixest::feglm(fe_formula, data = cc, family = "binomial",
                             vcov = ~census_tract, notes = FALSE)
retained_rows <- fixest::obs(selection_fe)
same_sample <- droplevels(cc[retained_rows, , drop = FALSE])

m_fe <- fixest::feglm(fe_formula, data = same_sample,
                      family = "binomial", vcov = ~census_tract, notes = FALSE)
m_adj <- fixest::feglm(adjusted_formula, data = same_sample,
                       family = "binomial", vcov = ~census_tract, notes = FALSE)
if (stats::nobs(m_fe) != nrow(same_sample))
  stop("Final FE refit unexpectedly dropped observations.")

flow <- data.frame(
  Stage = c("Initial lender-decision applications",
            "Restricted owner-occupied site-built first-lien 1-4 units",
            "Complete-case applications", "Primary lender-FE estimation sample"),
  Applications = c(nrow(s), nrow(r), nrow(cc), nobs(m_fe))
)
flow$Excluded_From_Previous <- c(NA_integer_, -diff(flow$Applications))

coverage <- data.frame(
  Measure = c("Primary sample N", "Distinct lenders", "Distinct census tracts",
              "Denied applications", "Non-denied applications"),
  Value = c(nrow(same_sample), dplyr::n_distinct(same_sample$lei),
            dplyr::n_distinct(same_sample$census_tract),
            sum(same_sample$denied == 1), sum(same_sample$denied == 0))
)

# The descriptive table uses all restricted applications, not the regression subset.
r$neighborhood_group <- cut(r$tract_minority_pct,
                            breaks = c(0, 20, 40, 60, 80, 100),
                            labels = c("0-20%", "20-40%", "40-60%", "60-80%", "80-100%"),
                            include.lowest = TRUE)
r$included <- stats::complete.cases(r[, columns_to_check, drop = FALSE])
descriptive <- r |>
  dplyr::filter(!is.na(neighborhood_group)) |>
  dplyr::group_by(neighborhood_group) |>
  dplyr::summarise(Applications = dplyr::n(), Denied = sum(denied),
                   Denial_Rate_Pct = round(100 * mean(denied), 3),
                   Complete_Cases = sum(included),
                   Completeness_Pct = round(100 * mean(included), 3),
                   .groups = "drop")

extract <- function(fit, label) {
  ct <- fixest::coeftable(fit)
  if (!"minority10" %in% rownames(ct)) stop("Focal coefficient missing: ", label)
  b <- unname(ct["minority10", "Estimate"])
  se <- unname(ct["minority10", "Std. Error"])
  data.frame(Model = label, N = stats::nobs(fit), Beta = b,
             Clustered_SE = se, OR_10pp = exp(b),
             CI_Low = exp(b - 1.96 * se), CI_High = exp(b + 1.96 * se))
}
models <- dplyr::bind_rows(
  extract(m_adj, "Adjusted, no lender FE"),
  extract(m_fe, "Adjusted with lender FE (primary)"))

# Fail closed if the frozen results cannot be recreated.
check_equal <- function(label, actual, expected, tolerance = 0) {
  if (length(actual) != length(expected) || anyNA(actual) ||
      any(abs(as.numeric(actual) - as.numeric(expected)) > tolerance)) {
    stop(label, " DID NOT MATCH. Actual: ", paste(actual, collapse = ", "),
         "; expected: ", paste(expected, collapse = ", "))
  }
}
check_equal("Sample flow", flow$Applications, c(73752, 60707, 57518, 55913))
check_equal("Coverage", coverage$Value, c(55913, 267, 1432, 4816, 51097))
check_equal("Odds ratios", models$OR_10pp,
            c(1.10649373736253, 1.08209127272025), tolerance = 1e-5)
check_equal("Descriptive group counts", descriptive$Applications,
            c(11121, 16261, 11690, 8926, 12709))
check_equal("Descriptive group denials", descriptive$Denied,
            c(579, 1144, 906, 900, 1784))

readr::write_csv(flow, file.path(outdir, "JREL_Final_Sample_Flow.csv"))
readr::write_csv(coverage, file.path(outdir, "JREL_Primary_Sample_Coverage.csv"))
readr::write_csv(descriptive, file.path(outdir, "JREL_Final_Descriptive_Table.csv"))
readr::write_csv(models, file.path(outdir, "JREL_Final_Main_Regression.csv"))
readr::write_csv(as.data.frame(table(DTI_Category = same_sample$dti_band)),
                 file.path(outdir, "JREL_DTI_Category_Frequencies.csv"))
writeLines(c(paste("Input MD5:", actual_md5), capture.output(sessionInfo())),
           file.path(outdir, "JREL_R_Session_Info.txt"))
cat("\nPASS: source checksum, sample flow, coverage, descriptive counts and primary ORs match.\n")
print(flow, row.names = FALSE)
print(models, row.names = FALSE)
cat("Results saved under: ", normalizePath(outdir), "\n", sep = "")
