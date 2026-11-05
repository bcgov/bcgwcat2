# Create Stiff Plot

Create Stiff Plot

## Usage

``` r
stiff_plot(
  samples,
  group = "location_id",
  colour = "viridis",
  legend_position = c(0.9, 0.8),
  legend_title = group,
  qc_valid_only = TRUE,
  qc_with_alkalinity = TRUE,
  margin = 10
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

- group:

  Character. Column by which to group data for colour, shape, filled and
  size.

- legend_position:

  Character or Numeric. Location of legend passed to `legend.position`
  argument of
  [`ggplot2::theme()`](https://ggplot2.tidyverse.org/reference/theme.html).
  One of none", "left", "right", "bottom", "top", "inside", OR a vector
  of two numeric values x, and y to specify an exact position.

- legend_title:

  Character. Title of legend. Defaults to `group`.

- qc_with_alkalinity:

  Logical. Whether to use Alkalinity
  (`alkalinity_total_4.5_as_caco3_meq/l`) for HCO3
  (`bicarbonate_alkalinity_as_caco3_meq/l`) if HCO3 is missing. Default
  TRUE. Matches behaviour of `water_type()`.

- margin:

  Numeric. Plot margin size in "pt". See
  [`ggplot2::margin()`](https://ggplot2.tidyverse.org/reference/element.html).

## Value

ggplot Stiff Plot

## Examples

``` r
if (FALSE) { # interactive()
s <- fetch_renmods(
  ems_ids = c("1401030"),
  date_range = c("1990-01-01", "2024-06-01")
)
stiff_plot(s)

s <- fetch_renmods(
  ems_ids = c("E218582"),
  date_range = c("2026-01-01", "2026-06-01")
)
stiff_plot(s)

s <- fetch_renmods(
  ems_ids = c("E298733"),
  date_range = c("2026-01-01", "2026-06-01")
)
stiff_plot(s)

s <- fetch_renmods(ems_ids = c("1401030", "1401377", "E292373"))

s <- fetch_renmods(ems_ids = "1401377")
stiff_plot(s)
}
```
