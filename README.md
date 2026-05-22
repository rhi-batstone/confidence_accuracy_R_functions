Updated: 22 May 2026


# Confidence-Accuracy (CA) Analysis Functions

This repository/script (`ca_analysis_functions.R`) provides a comprehensive suite of R functions for analyzing and visualizing the relationship between confidence and accuracy, commonly used in eyewitness memory research and decision-making studies. 

The script includes tools to compute Calibration and Confidence-Accuracy Characteristic (CAC) curves, plot them, calculate statistical metrics (Calibration Index, Over/Underconfidence, ANDI), and generate ROC curves.

## Dependencies
To use these functions, ensure you have the following R packages installed:
* `dplyr` / `tidyr` / `purrr` (via the `tidyverse` suite)
* `ggplot2`
* `jtools` (for `theme_apa()`)
* `viridis` (for color scales)

---

## Function Reference

### 1. Data Preparation & Processing

#### `compute_cac_cali()`
Summarises CAC and calibration values based on confidence bins and conditions.
* **`df`**: Data frame containing the data.
* **`conf_col`**: Name of the numeric confidence scale column (e.g., `"conf_3"`).
* **`conf_bin_col`**: Name of the confidence bins column.
* **`decision_col`**: Column with lineup decisions (must be: `"CID"`, `"TP_FID"`, `"TA_FID"`, `"CR"`, `"IR"`).
* **`condition_col`**: (Optional) Name of the condition column.
* **`choosing`**: Set to `"identification"` (default) or `"rejection"`.
* **`te`**: Tredoux's E value for lineup size adjustment (defaults to `1`).

### 2. Plotting Functions

#### `plot_cac()`
Generates a Confidence-Accuracy Characteristic (CAC) plot using `ggplot2`.
* **`df`**: Output data frame from `compute_cac_cali()`.
* **`condition_col`**: (Optional) Column indicating experimental conditions.
* **`conf_col`**: Confidence bins column (defaults to `"conf_bins"`).
* **`levels_conf`**: Factor ordering for confidence levels (defaults to `c("Low", "Medium", "High")`).
* **`viridis`**: Boolean to use Viridis color palette (defaults to `TRUE`).

#### `plot_calib()`
Generates a Calibration curve plot.
* Accepts similar arguments to `plot_cac()`, relying on `cali` and `cali_se` columns calculated during the data processing step. Includes an identity line for perfect calibration.

#### `plot_roc()`
Plots Receiver Operating Characteristic (ROC) curves based on false and correct identification rates.
* **`groups`**: Vector of groups to include.
* **`te_means`**: Named vector mapping groups to their specific `te_mean` (Tredoux's E).
* **`tp_col`**: Column indicating target presence (`"tp"` / `1` or `"ta"` / `0`).
* **`accuracy_col`**: Column indicating if the decision was accurate.

### 3. Statistical Indices

#### `CalcCalib()`
Calculates the Weighted Mean Squared Error (Calibration Index). Lower values indicate better calibration.
* **`e`**: Tredoux's E adjustment (defaults to `1`).
* **`accuracy_col`**: Binary accuracy column (defaults to `"accuracy"`).

#### `CalcOU()`
Calculates the Over/Under confidence statistic. 
* Positive values indicate overconfidence; negative values indicate underconfidence.

#### `CalcANDI()`
Calculates the Adjusted Normalised Discrimination Index (ANDI).
* Measures discrimination accuracy while accounting for the number of confidence bins and response baseline. 

### 4. Inferential Statistics (ICIs)

#### `ICI_noBoot()`
Calculates Independent Confidence Intervals (ICIs) for two proportions based on Tryon and Lewis (2008), testing for statistical differences without bootstrapping.
* Returns a list containing the interval string and a boolean indicating overlap.

#### `generate_ici_table()`
Iterates across conditions within each confidence bin to compute pairwise ICIs using `ICI_noBoot()`.
* Returns a tidy table of comparisons, the corresponding intervals, and whether they overlap.

### 5. Helpers

#### `drop_zero(x)`
Removes the leading zero from decimals (e.g., turns `0.5` into `.5`) for APA-formatted plots.
