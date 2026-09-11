# Neighborhood Minority Composition and Mortgage Denial: Evidence from Maryland's 2025 HMDA Data

## Overview

This repository contains the R code used to prepare the analytical samples and reproduce the statistical analyses for the study:

**Neighborhood Minority Composition and Mortgage Denial: Evidence from Maryland's 2025 HMDA Data**

**Authors:** Peter Sarpong and Monica Verma

**Affiliations:**  
Peter Sarpong — Independent Researcher, Gaithersburg, Maryland, USA  
Monica Verma — Associate Professor, Karnavati University, Uvarsad, Gandhinagar, Gujarat 382422, India

## Data

The study uses the 2025 Maryland Home Mortgage Disclosure Act (HMDA) loan-level data published by the Federal Financial Institutions Examination Council (FFIEC) and Consumer Financial Protection Bureau (CFPB).

The raw HMDA dataset is not redistributed in this repository. Users should obtain the public 2025 HMDA data directly from the FFIEC/CFPB HMDA data publication platform.

The lender-decision sample consists of 73,752 applications with `action_taken` values of 1, 2, or 3. The primary complete-case analytical sample contains 64,533 applications.

## Code

The main analysis script is:

`HMDA_Credit_Access_Analysis.R`

The script contains the data preparation and statistical analyses used for the study, including:

- Primary spline logistic regression
- Missing-data assessment
- Multiple imputation sensitivity analysis
- County fixed-effects sensitivity analysis
- Applicant race/ethnicity sensitivity analysis
- Categorical debt-to-income (DTI) sensitivity analysis
- Geographic heterogeneity analysis within Maryland
- Code used to produce the study figures

## Software

The analysis was conducted using R version 4.5.2.

Packages used include:

- splines 4.5.2
- MASS 7.3-65
- sandwich 3.1-3
- lmtest 0.9-40
- pROC 1.19.0.1
- mice 3.19.0

## Reproducibility

The primary complete-case analytical sample contains 64,533 applications.

The primary specification is a logistic regression using natural cubic splines with 3 degrees of freedom for loan amount, debt-to-income ratio, loan-to-value ratio, and loan term. Census-tract minority population share, census-tract income relative to area median income, and applicant income enter the model linearly.

The study also evaluates the robustness of the tract-minority association using multiple imputation, county fixed effects, adjustment for applicant race/ethnicity, categorical treatment of debt-to-income ratio, and regional heterogeneity analyses within Maryland.

The raw HMDA data are not included in this repository, so users must obtain the public 2025 Maryland HMDA data separately before running the analysis.

## Archived Code

An archived version of this code repository is available through Zenodo:

**DOI:** 10.5281/zenodo.22654848

## Citation

If you use this code, please cite the associated research paper:

Sarpong, P., & Verma, M. *Neighborhood Minority Composition and Mortgage Denial: Evidence from Maryland's 2025 HMDA Data.*
