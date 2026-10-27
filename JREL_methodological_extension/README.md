# Maryland 2025 HMDA Mortgage Denial Analysis

## Repository status

Curated analytical outputs from a research manuscript
under preparation. This is not yet an independently
reproduced end-to-end analysis release.

## Primary analytical sample

- 55,913 mortgage applications
- 267 lending institutions
- 1,432 census tracts
- 4,816 denied applications

## Primary model

A lender-fixed-effects logistic regression with
census-tract-clustered standard errors.

For a 10-percentage-point higher neighborhood
minority population share:

- Odds ratio: 1.082091
- 95% CI: 1.061519 to 1.103062

These are observational associations, not
causal estimates or findings of discrimination.

## Important limitations

- Numerical borrower credit scores are unavailable.
- Public HMDA data omit other underwriting details.
- Complete-case and lender eligibility restrictions
  affect the analyzed population.
- IPCW and multiple-imputation results remain
  exploratory and their inference is not finalized.

## Reproducibility status

The source CSV checksum and R environment are
documented under documentation/.
The complete original-data-to-results pipeline
has not yet been verified in a clean R session.

## Data availability

The source is the 2025 Maryland HMDA public
loan-level data. The source CSV is not included
in this curated release.

## Release contents

- results/main: principal findings
- results/sensitivity: robustness analyses
- results/missingness: reporting-completeness audits
- results/exploratory: preliminary analyses
- documentation: software and provenance records
