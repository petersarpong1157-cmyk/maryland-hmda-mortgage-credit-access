# Mortgage Denial and Neighborhood Composition: A Reproducible Analysis of Maryland's 2025 HMDA Data

## Overview

This repository contains the R code and research documentation for **Mortgage Denial and Neighborhood Composition: A Reproducible Analysis of Maryland's 2025 HMDA Data**, including the original exploratory analysis and a substantially documented **methodological extension prepared for journal submission**.

**Authors:** Peter Sarpong and Monica Verma

**Affiliations:**  
Peter Sarpong — Independent Researcher, Gaithersburg, Maryland, USA  
Monica Verma — Associate Professor, Karnavati University, Uvarsad, Gandhinagar, Gujarat 382422, India

**Practical purpose:** The revised analysis demonstrates how lender adjustment, selectively reported loan-to-value (LTV) information, and sample restrictions shape estimated associations between neighborhood characteristics and mortgage denial. Findings are observational and are not causal estimates of neighborhood or lender effects.

## Dataset and sample flow

The study analyzes public **2025 Maryland Home Mortgage Disclosure Act (HMDA)** loan-level information from the FFIEC/CFPB publication system. The source CSV is **not redistributed** here.

For the journal-oriented methodological extension:

| Stage | Applications |
| --- | ---: |
| Lender decisions (action_taken 1, 2, or 3) | 73,752 |
| Restricted owner-occupied, site-built, first-lien, one-to-four-unit purchase sample | 60,707 |
| Complete financial covariates, before fixed-effects exclusions | 57,518 |
| Primary lender-fixed-effects estimation sample | **55,913** |

The primary estimation sample comprises **267 lenders**, **1,432 census tracts**, and **4,816 observed denials**. Its original Maryland source CSV has MD5 checksum `bfc55246d7307f2f2e44ba4a5e7c12b8`.

**Historical note:** The original repository-root analysis and documentation referred to a **64,533-application complete-case sample**, with a different analytical specification. That is not the primary sample in the revised journal-oriented extension. The original code is retained as a distinct, historical workflow rather than silently overwritten.

## Principal results of the revised analysis

The focal association concerns a **10-percentage-point increase in census-tract minority population share**.

| Same-sample specification | Odds ratio | 95% confidence interval |
| --- | ---: | --- |
| Adjusted logistic model without lender fixed effects | 1.106494 | 1.084255–1.129189 |
| Logistic model with lender fixed effects (primary) | **1.082091** | **1.061519–1.103062** |

The models adjust for tract income, flexible borrower income, loan amount, LTV, loan term, categorical debt-to-income ratio, and loan type. The primary model includes lender fixed effects with census-tract-clustered inference.

The difference between odds ratios **must not** be interpreted as the causal effect of lenders: logistic odds ratios are noncollapsible, and public HMDA omits important underwriting information, including numerical credit scores.

### Loan-to-value reporting

Missing LTV values are disproportionately concentrated in particular lender–product combinations, including VA loans at one large lender. This supports a **reporting-completeness and sample-selection audit**, not a claim that VA mortgages universally lack LTV data. Conventional-only, lender-exclusion, and no-LTV sensitivity models are documented in the extension.

## Spatial analysis and model diagnostics

**Neighborhood composition:** Across **1,433 mapped Maryland tracts**, queen-contiguity Moran's I = **0.831385** and rook-contiguity Moran's I = **0.838785**, with 999-permutation p = **0.001** for both. These results concern spatial clustering in the **neighborhood-composition variable**, not model residuals. The mapped sample contains **60,663 applications** and differs from the primary 1,432-tract regression sample. Of 1,475 TIGER/Line Maryland tracts, 42 are marked *Not represented*.

**Exploratory model prediction-error diagnostic:** All **55,913** primary-model applications were represented across **1,432 tracts**. Moran's I for aggregated standardized observed-minus-expected denial diagnostics was **0.025743** (queen; descriptive permutation p = 0.098) and **0.030512** (rook; p = 0.070). Restricting to tracts with at least 20 applications gave **0.028073** (queen; p = 0.178) and **0.025355** (rook; p = 0.230). These are **in-sample descriptive model checks**, not formally calibrated residual-independence tests: the permutation procedure does not fully account for estimating fitted probabilities.

**In-sample calibration check:** The primary model had **4,816 observed denials** and approximately **4,816 expected denials** overall, a near-equality expected for this fitted specification and not evidence of out-of-sample calibration. The median tract contributed **32 applications** (interquartile range 20–49).

The model-diagnostic and calibration summaries were calculated interactively in R and are described here based on the saved analysis outputs supplied by the authors. **Do not infer that these newer CSVs have already been committed to GitHub or independently reproduced end-to-end**.

## Repository organization

- [Original analysis script](HMDA_Credit_Access_Analysis.R) — preserved historical analysis, including its original specifications.
- [Journal-oriented methodological extension](JREL_methodological_extension/README.md) — validated primary sample, estimates, figures, sensitivity tables, and documentation.
- [Primary reproduction script](JREL_methodological_extension/code/JREL_Reproducibility_Analysis.R) — used for the documented clean-session reproduction of the main model.
- [Spatial Moran's I script](JREL_methodological_extension/code/JREL_Spatial_Morans_I.R) and [spatial results CSV](JREL_methodological_extension/results/spatial/JREL_Moran_Robustness.csv) — separately reproduced neighborhood-composition spatial analysis.
- [Figure 4 map reproduction code](JREL_methodological_extension/code/Figure_4_Map_Reproduction.R) — supplied for transparency; a separate clean-session execution of this standalone script remains outstanding.

### Research figures

1. [Figure 1 — Unadjusted neighborhood denial rates](JREL_methodological_extension/figures/Figure_1_Neighborhood_Denial_Rates.svg)
2. [Figure 2 — Primary and sensitivity odds ratios](JREL_methodological_extension/figures/Figure_2_Primary_and_Sensitivity_ORs.svg)
3. [Figure 3 — Missing LTV by loan type](JREL_methodological_extension/figures/Figure_3_Missing_LTV_by_Loan_Type.svg)
4. [Figure 4 — Maryland tract composition map](JREL_methodological_extension/figures/Figure_4_Maryland_Map_Final.png) ([SVG](JREL_methodological_extension/figures/Figure_4_Maryland_Map_Final.svg))

The map shows tract minority share, **not** adjusted regression effects or discriminatory lending conduct.

## Software and reproducibility

The analysis was conducted in **R**. The original workflow recorded R 4.5.2 and packages including `splines`, `MASS`, `sandwich`, `lmtest`, `pROC`, and `mice`; the extension also uses `fixest` and spatial tools such as `sf` and `spdep`. See the extension's documentation for recorded versions and validation scope.

**Reproduced:** Core sample flow and primary estimates in a clean R session; neighborhood-composition queen/rook Moran's I results in a separate R session.

**Not represented as fully reproduced:** Every sensitivity table, the standalone cartographic script, and all exploratory IPCW/multiple-imputation and prediction-error diagnostics. Those findings must not be misrepresented as independently validated primary inference.

The public 2025 HMDA source file must be obtained separately. Consult the methodological extension for the input checksum, model details, and code.

## Research declarations

- **Funding:** The authors received no financial support for the research, authorship, or publication of this work.
- **Competing interests:** The authors declare no known financial or non-financial competing interests that could have influenced the work, subject to both authors' final confirmation.

## Zenodo archive and versions

**Release title for the forthcoming journal-oriented Zenodo deposit:** *Mortgage Denial and Neighborhood Composition: A Reproducible Analysis of Maryland's 2025 HMDA Data*. No new DOI has been issued yet.

An **earlier project archive** is available at **[DOI: 10.5281/zenodo.22654848](https://doi.org/10.5281/zenodo.22654848)**. This identifier is **not** being presented as the DOI for the revised JREL methodological extension.

**The updated journal-oriented archive has not yet been published on Zenodo.** Once a verified new deposit/release is published, add its actual record DOI and version citation here. Do not infer a new DOI from the earlier archive.

## Citation

Sarpong, P., & Verma, M. *Mortgage Denial and Neighborhood Composition: A Reproducible Analysis of Maryland's 2025 HMDA Data.* Manuscript in preparation.

When citing archived code, use the exact Zenodo record and version actually consulted.

## Interpretation and limitations

This single-state, single-year observational study is intended as an auditable methodological case study, not a nationwide estimate, a temporal trend, a finding of discrimination, or a causal decomposition of lender and neighborhood effects. The underlying data lack numerical credit scores and other underwriting details. Missing LTV and lender-specific reporting patterns affect the sample available to analysts. Exploratory weighting, imputation, spatial prediction-error, and in-sample calibration diagnostics are explicitly distinguished from the independently reproduced primary results.
