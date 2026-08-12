# Format EnMoDS data

This family of functions formats and filters data returned by
[`fetch_renmods()`](https://bcgov.github.io/bcgwcat2/reference/fetch_renmods.md).
All functions use `fmt_minimal()` as their first step.

## Usage

``` r
fmt_minimal(
  samples,
  save = FALSE,
  out_path = paste0("bcgwcat_", Sys.Date(), ".csv")
)

fmt_wide(
  samples,
  include_units = TRUE,
  fix_multi_method = "first",
  save = FALSE,
  out_path = paste0("wide_", Sys.Date(), ".csv")
)
```

## Arguments

- samples:

  Data frame. Output from
  [`fetch_renmods()`](https://bcgov.github.io/bcgwcat2/reference/fetch_renmods.md)

- save:

  Logical. Whether to automatically save the formatted data to
  `out_path`.

- out_path:

  Character. Location and file name to save data to if `save = TRUE`.

- include_units:

  Logical. Whether or not to append units to parameter column names.

- fix_multi_method:

  Character. How should multiple measurements be fixed?

## Value

Filtered and formatted data frame of EnMoDS data.

## Details

**`fmt_minimal()`**

- keeps only columns listed in
  [`index_meta()`](https://bcgov.github.io/bcgwcat2/reference/index_meta.md)
  and
  [`index_meta_params()`](https://bcgov.github.io/bcgwcat2/reference/index_meta_params.md)

- filters to only parameters (observed properties) listed in
  [`index_params()`](https://bcgov.github.io/bcgwcat2/reference/index_params.md)

- all column names converted to lower case (snake_case)

- creates `obs_well` filled with Observation Well listed in
  `location_name`

**`fmt_wide()`** builds on `fmt_minimal()`

- transform the parameters/observed properties from long into wide
  format

- if any parameters for a specific observation (identified by
  `location_id` and `observation_date_time`) have been measured multiple
  times with different methods, a single measurement will be selected

**`fmt_aquachem()`** - Builds on `fmt_wide()`

- create a data set which can be imported into AquaChem

- rename columns to those required by AquaChem

- adds units as second row of data (very awkward in R, so best if only
  used with AquaChem)

## Examples

``` r
if (FALSE) { # interactive()
s <- fetch_renmods(
  ems_ids = c("1401030", "1401377"),
  date_range = c("2020-01-01", "2022-02-01")
)

fmt_minimal(s)
fmt_wide(s)

s
}
```
