# Water Quality Data

A filtered and formatted subset of the BC Gov water quality guidelines
fetched with `bcdata::bcdc_get_data()`, from record
[`85d3990a-ec0a-4436-8ebd-150de3ba0747`](https://catalogue.data.gov.bc.ca/dataset/water-quality-guidelines-of-b-c-),
and resource `6f32a85b-a3d9-44c3-9a14-15175eba25b6`.

## Usage

``` r
wq_std()
```

## Value

tibble of formatted and filtered water quality guidelines

## Details

This data is formatted for easier use in R (e.g., lower case columns
names), and filtered to include only parameters for Drinking Water an
looking at only the Upper Limits of the Maximum Acceptable
Concentration. It is further filtered to only include parameters of
interest to groundwater analyses.

Three columns, `enmods_param`, `bcgwcat_param` and `n` have been added
to the data to reflect the corresponding parameter names in EnMoDS and
bcgwcat, as well as to reflect how many different guidelines there are
for a paraticular parameters (`n`).

## Examples

``` r
wq_std()
#> # A tibble: 21 × 13
#>    uniqueid variable   component ems_code  limit limitnotes      units condition
#>       <dbl> <chr>      <chr>     <chr>     <dbl> <chr>           <chr> <lgl>    
#>  1       81 Aluminum   Total     AL-T      9.5   NA              mg/L  NA       
#>  2      109 Arsenic    Total     AS-T      0.01  NA              mg/L  NA       
#>  3      154 Boron      Total     B--T      5     NA              mg/L  NA       
#>  4      173 Cadmium    Total     CD-T      0.005 NA              mg/L  NA       
#>  5      445 Fluoride   Total     1106      1.5   NA              mg/L  NA       
#>  6      558 Molybdenum Total     MO-T      0.088 NA              mg/L  NA       
#>  7      592 Nitrate    Total     1110     45     Reported as ni… mg/L  NA       
#>  8      593 Nitrate    Total     1110     10     Reported as N.  mg/L  NA       
#>  9      628 Nitrite    Dissolved 1111      3     Reported as ni… mg/L  NA       
#> 10      629 Nitrite    Dissolved 1111      1     Reported as N.  mg/L  NA       
#> # ℹ 11 more rows
#> # ℹ 5 more variables: predictedeffectlevel <chr>, status <chr>,
#> #   enmods_param <chr>, bcgwcat_param <chr>, n <dbl>
```
