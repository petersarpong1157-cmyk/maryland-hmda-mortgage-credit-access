# Maryland 2025 HMDA Mortgage Denial Analysis — Methodological Extension

This folder contains supplementary methods, reproducible primary analyses, descriptive and sensitivity results, and four figures for a research manuscript in preparation. It extends, and does not replace, the earlier project in the repository root.

## Primary analytical sample and results

- 73,752 applications with lender decisions (action taken 1, 2, or 3)
- 60,707 restricted owner-occupied, site-built, first-lien, one-to-four-unit purchase applications
- 57,518 complete-case applications before fixed-effects exclusion
- 55,913 applications in the primary lender-fixed-effects estimation sample
- 267 lenders; 1,432 census tracts; 4,816 denied applications
- Adjusted model without lender fixed effects: OR 1.106494 (95% CI 1.084255–1.129189)
- Primary lender-fixed-effects model: OR 1.082091 (95% CI 1.061519–1.103062)

Odds ratios refer to a ten-percentage-point increase in census-tract minority population share. Models control for tract income, flexible applicant income, loan amount, loan-to-value ratio, term, DTI category, and loan type; inference is clustered by census tract. These are observational associations, not causal estimates or findings of discrimination. Differences between logistic odds ratios need not identify a lender effect.

## Figures

- [Figure 1: Denial rates by neighborhood composition](figures/Figure_1_Neighborhood_Denial_Rates.svg) — unadjusted rates in the restricted sample.
- [Figure 2: Primary and sensitivity odds ratios](figures/Figure_2_Primary_and_Sensitivity_ORs.svg) — adjusted associations with confidence intervals.
- [Figure 3: Missing LTV by loan type](figures/Figure_3_Missing_LTV_by_Loan_Type.svg) — descriptive reporting patterns.
- [Figure 4: Maryland neighborhood composition map (PNG)](figures/Figure_4_Maryland_Map_Final.png) · [vector SVG](figures/Figure_4_Maryland_Map_Final.svg) — statewide and metropolitan panels.

Figure 4 maps 1,433 tracts represented in 60,663 restricted applications with valid mapping data; 42 Maryland tracts are marked **Not represented**. Map geography is not the same as the 1,432-tract primary regression estimation sample. Boundaries come from 2025 Census TIGER/Line geography. District of Columbia is outside Maryland and not mapped as a Maryland tract.

## Code and reproducibility

- [Primary analysis script](code/JREL_Reproducibility_Analysis.R): passed a local clean-run check against the documented source checksum, sample flow, coverage, descriptive counts, and primary odds ratios.
- [Figure 4 map reproduction script](code/Figure_4_Map_Reproduction.R): reconstructs the verified cartographic workflow from the same source CSV and official Census boundaries. **The standalone map script has not yet been separately validated in a fresh R session.**
- [Original methodological extension](code/JREL_Original_Extension.R): preserved for transparency and historical comparison.

The input CSV is not provided in this repository. Its documented MD5 checksum is `bfc55246d7307f2f2e44ba4a5e7c12b8`. Run code locally in a directory containing the matching Maryland HMDA source CSV. The original-data-to-results primary model and descriptive summaries passed the recorded clean-session reproduction; supplementary sensitivity and missing-data pipelines have **not** all been independently rerun from the source.

## Other contents

- `results/main/` — primary regression, descriptive table, sample flow and coverage.
- `results/sensitivity/` — selected robustness tables; some generated from the original interactive research session.
- `results/missingness/` — lender/product reporting completeness.
- `results/exploratory/` — preliminary IPCW and multiple-imputation diagnostics, **not finalized inferential analyses**.
- `documentation/` — reproducibility manifest, model/software versions, DTI recoding audit, script inventory, and validation record.

## Interpretation and limitations

The estimates do not establish borrower-level discrimination or a causal neighborhood effect. Public HMDA lacks numerical credit scores and other underwriting details. Missing LTV is concentrated in selected loan products and lenders, and complete-case and fixed-effects restrictions affect sample representation. The weighting and imputation pilot analyses remain exploratory.

## Figure provenance

Figures 1–3 were rendered from the tabulated model/descriptive outputs. Figure 4 was generated locally in R with `sf`, `ggplot2`, Census 2025 TIGER/Line boundaries, and the HMDA tract-level composition variable. Source categories and map controls are documented in the map script. No synthetic mortgage observations were used.
