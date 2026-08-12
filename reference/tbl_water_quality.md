# Reactable table for data overview

Reactable table for data overview

## Usage

``` r
tbl_water_quality(samples, which = NULL, params = NULL)
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

- which:

  Character. Which columns to include "rel" for relevant, otherwise all
  are included.

- params:

  Character vector. Only include these parameters.

## Value

Reactable data table

## Examples

``` r
if (FALSE) { # interactive()
s <- fetch_renmods(
  ems_ids = c("1401030", "1401377"),
  date_range = c("2020-01-01", "2022-02-01")
)
tbl_water_quality(s)
tbl_water_quality(s, "problems")
tbl_water_quality(s, "no_missing")

s <- fetch_renmods(
  ems_ids = c("E218582"),
  date_range = c("2026-01-01", "2026-06-01")
)
tbl_water_quality(s)
tbl_water_quality(s, "problems")
tbl_water_quality(s, "no_missing")
water_quality(s)
}
```
