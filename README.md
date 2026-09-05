# Geographic Variation in Mortgage Credit Access: Evidence from 2025 Maryland HMDA Data

## Overview

This repository contains the R code used to prepare the analytical sample and reproduce the statistical analyses for the study:

**Geographic Variation in Mortgage Credit Access: Evidence from 2025 Maryland HMDA Data**

**Author:** Peter Sarpong  
**Affiliation:** Independent Researcher, Washington, DC, USA

## Data

The study uses the 2025 Maryland Home Mortgage Disclosure Act (HMDA) loan-level data published by the Federal Financial Institutions Examination Council (FFIEC) and Consumer Financial Protection Bureau (CFPB).

The raw HMDA dataset is not redistributed in this repository. Users should obtain the public 2025 HMDA data directly from the FFIEC/CFPB HMDA data publication platform.

## Code

The main analysis script is:

`HMDA_Credit_Access_Analysis.R`

The script contains the data preparation and statistical analysis used for the study.

## Software

The analysis was conducted using R version 4.5.2.

Packages used include:

- splines 4.5.2
- MASS 7.3-65
- sandwich 3.1-3
- lmtest 0.9-40
- pROC 1.19.0.1

## Reproducibility

The primary complete-case analytical sample contains 64,532 applications. Model 3 uses 64,531 observations after excluding the single application with an undetermined conforming-loan-limit status because of category sparsity.

## Citation

If you use this code, please cite the associated research paper:

Sarpong, P. *Geographic Variation in Mortgage Credit Access: Evidence from 2025 Maryland HMDA Data.*
