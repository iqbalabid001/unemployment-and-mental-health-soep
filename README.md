# Unemployment & Mental Health — SOEP Panel Analysis

> **University Project** — submitted as part of the Master Seminar in Applied Economics course.

## Overview

This project investigates how unemployment duration affects mental health trajectories using the **SOEP Teaching Dataset (v40)**, a longitudinal panel survey from Germany. The analysis moves beyond simple before/after comparisons to track how the *same individual's* mental health evolves over time spent unemployed.

## Key Findings

- **Honeymoon Effect:** Mental health increases slightly (+0.48 points) immediately after job loss, consistent with short-term relief from workplace stress.
- **Scarring Effect:** Fixed Effects models reveal a cumulative decline of ~0.075 points per year of unemployment for the same individual — the opposite of what aggregate data suggests.
- **Social Support Matters:** Married individuals experience a pronounced relief phase before scarring sets in. Widowed individuals receive almost no relief, deteriorating immediately upon job loss.

## Methods

- Fixed Effects (Within) Panel Models via `plm`
- Quadratic interaction models (`unemployment_duration²` × `marital_status`)
- Factor-based event study plot to test for non-linearity
- Paired t-tests and OLS for transition analysis

## Files

| File | Description |
|------|-------------|
| `Final_Report_code_Iqbal.Rmd` | Full R Markdown source code |
| `Final_Report_code_Iqbal.html` | Knitted HTML report (open in browser) |
| `Final_Report_refs_Iqbal.bib` | Bibliography references |
| `raw_report_code .R` | Plain R Code used for Rmd |

## Data

This analysis uses the **SOEP Teaching v40** dataset, which is not included in this repository due to data licensing restrictions. The SOEP data can be accessed via the [DIW Berlin](https://www.diw.de/en/diw_01.c.601584.en/data_access.html).

## Requirements

```r
library(haven)
library(tidyverse)
library(plm)
library(broom)
library(ggcorrplot)
library(moments)
library(knitr)
library(conflicted)
```

To reproduce the report, place the SOEP Teaching v40 data folder in the same directory as the `.Rmd` file and knit in RStudio.
