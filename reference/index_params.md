# Title

Title

## Usage

``` r
index_params()
```

## Value

Data frame of parameters

## Examples

``` r
index_params()
#> # A tibble: 111 × 10
#>    ems_param       ems_code ems_unit aqua_code aqua_unit water_quality smwr_code
#>    <chr>           <chr>    <chr>    <chr>     <chr>     <lgl>         <chr>    
#>  1 Acidity pH 4.5  132      mg/L     NA        NA        TRUE          NA       
#>  2 Acidity pH 8.3  131      mg/L     NA        NA        TRUE          NA       
#>  3 Alkalinity Phe… 101      mg/L     NA        NA        TRUE          NA       
#>  4 Alkalinity Tot… 102      mg/L     Meas_Alk  mg/L      TRUE          NA       
#>  5 Aluminum Disso… AL-D     mg/L     Al_diss   ug/L      TRUE          aluminum 
#>  6 Aluminum Total  AL-T     mg/L     Al_tot    ug/L      TRUE          NA       
#>  7 Anion Sum       ASUM     mEq/L    anion_sum mEq/L     TRUE          NA       
#>  8 Antimony Disso… SB-D     mg/L     Sb_diss   ug/L      TRUE          NA       
#>  9 Antimony Total  SB-T     mg/L     Sb_tot    ug/L      TRUE          NA       
#> 10 Arsenic Dissol… AS-D     mg/L     As_diss   ug/L      TRUE          NA       
#> # ℹ 101 more rows
#> # ℹ 3 more variables: enmods_param <chr>, enmods_description <chr>,
#> #   bcgwcat_param <chr>
```
