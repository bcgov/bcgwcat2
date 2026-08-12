# Calculate water type

Water type based on anions Cl, SO4, HCO3 and cations Ca, Mg, Na and K.
Ions are ranked by proportion MEQ, all greater than 10% are listed in
descending order of presence, cations first. Water type is only
calculated for samples with valid charge balances. If HCO3 is missing,
we use Meas Alk as a replacement (indicated by an '\*' on HCO3 in the
water type). Otherwise, missing ions are ignored (i.e. treated as 0).
The column `missing_ion` indicates whether there is a problem with the
water type, such that it is missing a cation or anion (i.e. is all
cations or all anions). Water type is designed to compliment piper plots
by using the same ions so they are comparable.

## Usage

``` r
water_type(samples)
```

## Arguments

- samples:

  Data frame. Output from
  [`fmt_minimal()`](https://bcgov.github.io/bcgwcat2/reference/fmt.md)
  or
  [`fetch_renmods()`](https://bcgov.github.io/bcgwcat2/reference/fetch_renmods.md).
  If
  [`fetch_renmods()`](https://bcgov.github.io/bcgwcat2/reference/fetch_renmods.md)
  samples will be first formatted with
  [`fmt_minimal()`](https://bcgov.github.io/bcgwcat2/reference/fmt.md).

## Value

Data frame with added columns `water_type` and `missing_ion`.

## Examples

``` r
if (FALSE) { # interactive()
}
```
