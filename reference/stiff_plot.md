# Create Stiff plot

Create Stiff plot

## Usage

``` r
stiff_plot(
  samples,
  ems_id = NULL,
  colour = TRUE,
  legend = TRUE,
  valid = TRUE,
  with_Alk = TRUE
)
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

- ems_id:

  IDs to plot if dataset includes more than one

- colour:

  Whether to add colour by `ems_id`

- legend:

  Whether to show the legend

- valid:

  Logical. Keep only valid data (charge balances \<=10)

- with_Alk:

  Logical. Whether to use `Meas_Alk_meq` for `HCO3_meq` if missing.
  Default TRUE. Matches behaviour of
  [`water_type()`](https://bcgov.github.io/bcgwcat2/reference/water_type.md).

## Examples

``` r
if (FALSE) { # interactive()
s <- fetch_renmods(
  ems_ids = c("1401030"),
  date_range = c("1990-01-01", "2026-06-01")
)
stiff_plot(s)
s <- fetch_renmods(
  ems_ids = c("E218582"),
  date_range = c("2026-01-01", "2026-06-01")
)
stiff_plot(s)
}
```
