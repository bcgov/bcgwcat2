# Assess problems with water quality

Adds water quality guideline limits to the data and adds a column
`water_quality_problem` which is `TRUE` if a water quality limit is
exceeded, `FALSE` if not. See
[`wq_std()`](https://bcgov.github.io/bcgwcat2/reference/wq_std.md) for
more information on the water quality guidelines.

## Usage

``` r
water_quality(samples)
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

Tibble of sites, samples, parameters, water quality limits and problems.

## Examples

``` r
if (FALSE) { # interactive()
s <- fetch_renmods(
  ems_ids = c("1401030", "1401377"),
  date_range = c("2020-01-01", "2022-02-01")
)
water_quality(s)
}
```
