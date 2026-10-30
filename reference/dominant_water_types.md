# Return dominant water types

Find the dominant waters type in a group of samples, but only if there
are more than `n` samples with a valid water type. HCO3 and HCO3\* types
are treated as equivalent. Water types are considered dominant if they
are in \> `p` proportion of samples.

## Usage

``` r
dominant_water_types(samples, n = 5, p = 0.75)
```

## Arguments

- samples:

  Data frame with at least a `water_type` column.

- n:

  Numeric. Minimum number of samples needed to determine dominant water
  types.

- p:

  Numeric. Proportion of samples with a given water type for it to be
  considered dominant.

## Examples

``` r
wt <- data.frame(
  water_type = c(
    "Mg-Na-Ca-HCO3-SO4",
    "Mg-Na-Ca-HCO3-SO4",
    "Mg-Na-Ca",
    "Ca-Mg-HCO3-SO4",
    "Ca-Mg-HCO3-SO4",
    "Mg-Na",
    "Mg-Na",
    "Ca-Mg-Na")
  )
dominant_water_types(wt)
#> [1] "Mg-Na-Ca"          "Ca-Mg-HCO3-SO4"    "Mg-Na"            
#> [4] "Mg-Na-Ca-HCO3-SO4"
```
