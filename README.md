# WNN Forecasting R Package

This R package was developed as part of a Master's project to implement the **Weighted Nearest Neighbors (WNN)** forecasting method for electricity consumption data.

## 🚀 Installation

You can install the package using one of the two methods below.

### Method 1: Installation from GitHub
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
