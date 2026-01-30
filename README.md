# WNN Forecasting R Package

![R Version](https://img.shields.io/badge/R-v4.0+-blue.svg)
![License](https://img.shields.io/badge/License-MIT-green.svg)
![Type](https://img.shields.io/badge/Focus-Time--Series-orange.svg)

This R package was developed as part of a Master's project to implement the **Weighted Nearest Neighbors (WNN)** forecasting method for electricity consumption data.

## 🚀 Installation

You can install the package using one of the two methods below.

### Method 1: GitHub
If you have `devtools` installed, you can pull the package directly from this repository:

```r
# 1. Install devtools 
if (!require("devtools")) install.packages("devtools")

# 2. Install repository
devtools::install_github("maissaladjimi/WNN_Time_Series_Forecasting")

# 3. Load the library
library(MaissaLajimi)
```

### Method 2: Local Installation (.tar.gz)
If you have the source archive file, run this in your R console:
```r
install.packages("path/to/MaissaLajimi_0.1.0.tar.gz", repos = NULL, type = "source")

library(MaissaLajimi)
```

## 📖 Quick Start Guide  

### 1. Data Preparation
The package includes a sample dataset, ElecTrain, containing 15-minute electricity and temperature readings.
```r
# Load the electricity consumption dataset
data("ElecTrain")

# Extract power consumption and temperature
df_power <- na.omit(ElecTrain$power)
df_temp  <- na.omit(ElecTrain$temp)

# Check data dimensions
cat("Total observations:", length(df_power), "\n")
#> Total observations: 4699
cat("Total days:", length(df_power) / 96, "\n")
#> Total days: 48.94792
cat("Frequency: 15-minute intervals (96 per day)\n")
#> Frequency: 15-minute intervals (96 per day)
```
### 2. Training and Test Split
We divide the data to evaluate the model performance on the final 3 days.
```r
n <- length(df_power)
h_test <- 3 * 96  # Last 3 days

# Training data (Jan 1 - Feb 15)
train_y <- df_power[1:(n - h_test)]
train_X <- matrix(df_temp[1:(n - h_test)], ncol = 1)

# Test data (Feb 16-18)
test_y <- df_power[(n - h_test + 1):n]
test_X <- matrix(df_temp[(n - h_test + 1):n], ncol = 1)

cat("Training samples:", length(train_y), "\n")
#> Training samples: 4411
cat("Test samples:", length(test_y), "\n")
#> Test samples: 288
```
### 3. Generate WNN Forecast
The wnn() function executes the forecasting logic
```r
# Generate forecast for last 3 days
result <- wnn(
  y          = train_y,           # Historical power consumption
  X          = train_X,           # Historical temperature
  m          = 96,                # Window size (1 day = 96 intervals)
  k          = 30,                # Number of neighbors
  h          = length(test_y),    # Forecast horizon (3 days)
  X_future   = test_X,            # Future temperature
  scale      = TRUE,              # Enable standardization
  temp_lags  = 1,                 # Use only current temperature
  return_all = TRUE,              # Return detailed diagnostics
  plot       = TRUE,              # Show forecast plot
  test_y     = test_y             # For performance metrics
)
#> 
#> ================================================
#>   TRAINING WNN MODEL WITH PARAMETERS :
#> ------------------------------------------------
#>  • Horizon:    288 step(s)
#>  • Neighbors:  k = 30
#>  • Window (m): 96
#>  • Lags:        1 temp lag(s)
#>  • Scaling:     Enabled
#> 
#> ---- Scaling Statistics ----
#>   Consumption: mean = 231.37, sd = 57.43
#>   Exogenous:   mean = 10.76, sd = 2.71
#> 
#> ------------------------------------------------
#>   PERFORMANCE METRICS (vs Test Set)
#> ------------------------------------------------
#>    RMSE:  33.57
#>    MAE:   16.80
```

### 📊 WNN vs. SVM: February 19 Forecast
A key feature of this package is the ability to compare the WNN model's accuracy against other algorithms like Support Vector Machines (SVM).

**Load Pre-computed SVM Forecast**
```r
# Load SVM forecast for Feb 19
data("MaissaLajimi")
svm_forecast <- as.numeric(MaissaLajimi[[1]])

cat("SVM forecast loaded:", length(svm_forecast), "observations\n")
#> SVM forecast loaded: 96 observations
```
**Generate WNN Forecast for Feb 19**
```r
# Use ALL available data to forecast Feb 19
h_steps <- 96  # One full day

# Future temperature for Feb 19 (available in dataset)
X_future_feb19 <- matrix(df_temp[(n + 1):(n + h_steps)], ncol = 1)

# Generate WNN forecast
feb19_wnn_result <- wnn(
  y          = df_power[1:n],              # All historical data
  X          = matrix(df_temp[1:n], ncol = 1),
  m          = 96,
  k          = 30,
  h          = h_steps,
  X_future   = X_future_feb19,
  scale      = TRUE,
  temp_lags  = 1,
  return_all = TRUE,
  plot       = FALSE                      
)
#> 
#> ================================================
#>   TRAINING WNN MODEL WITH PARAMETERS :
#> ------------------------------------------------
#>  • Horizon:    96 step(s)
#>  • Neighbors:  k = 30
#>  • Window (m): 96
#>  • Lags:        1 temp lag(s)
#>  • Scaling:     Enabled
#> 
#> ---- Scaling Statistics ----
#>   Consumption: mean = 231.16, sd = 58.62
#>   Exogenous:   mean = 10.88, sd = 2.73

wnn_forecast_feb19 <- feb19_wnn_result$forecasts
```
**Compare WNN vs SVM**
```r
# Compare both forecasts
comparison <- compare_forecasts(
  external_forecast = svm_forecast,
  wnn_forecast      = wnn_forecast_feb19,
  y                 = df_power[1:n],      
  method_name       = "SVM"
)
#> 
#> ================================================
#>   COMPARISON SUMMARY: WNN vs SVM
#> ------------------------------------------------
#>       Metric Historical     WNN     SVM
#>    Mean (kW)    207.366 228.003 233.001
#>      Std Dev     91.677  55.537  58.031
#>          Min      0.000 148.326 145.210
#>          Max    306.300 302.568 317.079
#>    Coeff Var      0.442   0.244   0.249
#>  Pattern Cor      1.000   0.813   0.798
#> ================================================
```
**Visualizing Results**

```r
# View the plots

# Plot 1: Time series with both forecasts
print(comparison$plots$comparison)

# Plot 2: Difference between forecasts
print(comparison$plots$difference)
```


