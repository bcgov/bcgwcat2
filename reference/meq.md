# Calculate MEQ values

Calculates MEQ values for 'long' data in mg/L. Expects output of either
[`fetch_renmods()`](https://bcgov.github.io/bcgwcat2/reference/fetch_renmods.md)
or [`fmt_minimal()`](https://bcgov.github.io/bcgwcat2/reference/fmt.md).

## Usage

``` r
meq(samples)
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

## Details

For conversion details and a description of the data see
[`meq_conversion()`](https://bcgov.github.io/bcgwcat2/reference/meq_conversion.md).

MEQs are calculated by dividing the parameter concentration in mg/L by
the conversion factor. "result_unit" is required to ensure the units are
in mg/L before calculating MEQs

**Note: This is an internal function, exported for clarity in
calculations**

## Examples

``` r
if (FALSE) { # interactive()
s <- data.frame(observed_property_id = "", result_value = "", result_unit = "")
meq(s)

s <- fetch_renmods(
  ems_ids = c("1401030", "1401377"),
  date_range = c("2020-01-01", "2022-02-01")
)

meq(s)

fmt_minimal(s)
fmt_wide(s)
}
```
