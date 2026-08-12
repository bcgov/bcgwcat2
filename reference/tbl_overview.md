# Reactable table for data overview

Reactable table for data overview

## Usage

``` r
tbl_overview(samples, which)
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

## Value

Reactable data table

## Examples

``` r
if (FALSE) { # interactive()
s <- fetch_renmods(
  ems_ids = c("1401030", "1401377"),
  date_range = c("2020-01-01", "2022-02-01")
)
tbl_overview(s, "rel")

s <- fetch_renmods(
  ems_ids = "E309359",
  date_range = c("1900-01-01", "2099-01-01")
)
tbl_overview(s, "rel")

s <- fetch_renmods(
  ems_ids = "E102388",
  date_range = c("2026-01-01", "2027-01-01")
)
fmt_wide(s)
tbl_overview(fmt_wide(s), "rel")
}
```
