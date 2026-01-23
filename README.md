# Heuristics for Healthier Food Selection

This repository contains the analysis code and supporting materials for the paper  
**“Heuristics for Healthier Food Selection”**  
(Maria Almudena Claassen, Kamil Fulawka, Mubashir Sultan, Anastasia Kozyreva, Ralph Hertwig).

The project evaluates simple food-selection heuristics using large-scale nutrient data and simulated pairwise product choices.

---

## Repository structure

```text
.
├── analyses/
├── data/
├── figures/
├── heuristics_functions/
├── results/
├── .gitignore
└── food_heuristics.Rproj
```

---

## Folder descriptions

### `data/`
Contains supporting data files required for the analyses.

- `bls_clean/`  
  Includes names of product categories and subcategories, as well as metadata on nutrients.

- `heuristic_apply_guides/`  
  Scripts that prepare data for testing folk heuristics (e.g., *eat raw*, *eat brown*, *eat wild*).

> **Note:** The original raw product-level dataset cannot be shared publicly due to data access restrictions.  
> If the raw data are available, they should be saved under:
>
> ```
> data/BLS_3.02/BLS_3.02.txt
> ```

---

### `analyses/`
A collection of scripts that apply heuristics to relevant subsets of **pairwise choices between similar products**, and then summarize and visualize the results.

- Scripts `00_data_prep.R`, `00_overview.R`, and `01–03_*.R` **cannot be run without access to the original raw data**.
- Later scripts operate on results available in `results`

---

### `figures/`
PDF files containing figures reported in the paper and supplementary materials (e.g., Figures 1–3, Figure A1–A3).

---

### `heuristics_functions/`
Defines the core functions used to:
- Apply individual heuristics to a single pairwise product choice
- Evaluate heuristic performance
- Scale heuristic application to all possible pairwise choices within product categories

These functions form the computational backbone of the analyses.

---

### `results/`
Serialized R objects containing heuristic evaluation outputs:

- `heu_res.rds`  
  Raw results (e.g., nutrient ratios) from applying heuristics to pairwise product choices.

- `heu_res_preped.rds`  
  Statistical summaries of the raw results, used to generate Figure 2 and Figure A1.

---

## Getting started

The project is set up as an RStudio project.  
To work with the code, open `food_heuristics.Rproj` (double-click it or open it from RStudio).  
This will set the working directory to the repository root and ensures relative paths used in the scripts resolve correctly.

---

## Dependencies

The analyses use standard R packages (e.g., tidyverse-style tooling).  
Install required packages with:

```r
pkgs <- c(
  "future", "data.table", "future.apply", "patchwork", "viridis",
  "ggplot2", "stringr", "dplyr"
)
install.packages(setdiff(pkgs, rownames(installed.packages())))
```

---

## Notes on reproducibility

Due to data access restrictions, full end-to-end reproduction of the analyses requires access to the original product-level dataset. The repository is structured to make all analytical steps, assumptions, and transformations explicit once the raw data are available.

---

## Contact

For questions regarding the code or data structure, please contact the authors.
