# Create a Piper Plot

Create a Piper Plot

## Usage

``` r
piper_plot(
  samples,
  group = "location_id",
  legend_position = c(0.9, 0.8),
  legend_title = group,
  qc_valid_only = TRUE,
  qc_omit_outliers = TRUE,
  qc_with_alkalinity = TRUE,
  point_colour = "viridis",
  point_shape = 16,
  point_size = 3,
  margin = 10,
  scale = 1
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

- qc_omit_outliers:

  Logical. Whether to omit outliers by looking at dominant water types
  (see `?dominant_water_types()`). Default TRUE.

- qc_with_alkalinity:

  Logical. Whether to use Alkalinity
  (`alkalinity_total_4.5_as_caco3_meq/l`) for HCO3
  (`bicarbonate_alkalinity_as_caco3_meq/l`) if HCO3 is missing. Default
  TRUE. Matches behaviour of `water_type()`.

- point_colour:

  Character. Colour or colours by which to colour points. Either a
  single value (applied to all), or a vector of values the same length
  as the number of `groups`. Can also be "viridis", which will use the
  viridis colour scale.

- point_shape:

  Numeric. Shape of points to use, 1 - 25, see
  ?[`ggplot2::scale_shape()`](https://ggplot2.tidyverse.org/reference/scale_shape.html).
  Either a single value (applied to all), or a vector of values the same
  length as the number of `groups`.

- margin:

  Numeric. Plot margin size in "pt". See
  [`ggplot2::margin()`](https://ggplot2.tidyverse.org/reference/element.html).

- scale:

  Numeric. Scale text (axes titles and tick labels).

- qc_valid:

  Logical. Keep only valid data (charge balances \<=10)

## Value

ggplot Piper Plot

## Examples

``` r
if (FALSE) { # interactive()
s <- fetch_renmods(ems_ids = "E208101", date_range = c("2001-01-01", "2015-01-01"))
piper_plot(s)

ss <- fmt_minimal(s)
piper_plot(ss)
}
```
