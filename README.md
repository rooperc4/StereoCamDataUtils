Helper functions for SEBASTES data wrangling
================
Chris Rooper
February 25, 2022

To install the package

``` r
library(devtools)
devtools::install_github("rooperc4/StereoCamDataUtils")
library("StereoCamDataUtils")
```

## Purpose

The functions contained in this package are used to deal with the data
from image analyses conducted using SEBASTES stereo camera software.
Currently there are two functions, but more will be added in the future.

![](StereoCam_Data_Utils_files/figure-gfm/Figure%201-1.png)<!-- -->

## Existing Functions

### Data replace

This function replaces the \*.db data files with new versions by
matching the deployment ID’s and replacing the appropriate data files

``` r
SEB_data_replace("C:/Pathname.../new data","C:/Pathname.../Deployment directories")
```

### Data replace

This function concatenates the \*.db data files from multiple
deployments into a single frame table (which includes accessory data)
and a target table.

``` r
SEB_data_concatenate("C:/Pathname.../Deployment directories")
```
