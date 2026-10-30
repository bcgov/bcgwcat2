# Fetch renmods data for specific ems_ids

Light wrapper around
[`renmods::renmods_connect()`](https://bcgov.github.io/renmods/reference/renmods_connect.html)
to fetch data by date and EMS ID. Also collects data into memory, so can
take some time if a lot of data are requested.

## Usage

``` r
fetch_renmods(ems_ids, date_range = NULL)
```

## Arguments

- ems_ids:

  Character Vector. EMS ids to filter EnMoDS samples to.

- date_range:

  Character/Date Vector. Start/end dates to filter to. `NULL` returns
  all data.

## Value

Data frame of filtered EnMoDS data.

## Examples

``` r
if (FALSE) { # interactive()
s <- fetch_renmods(
  ems_ids = c("1401030", "1401377"),
  date_range = c("2020-01-01", "2022-02-01")
)

 s <- fetch_renmods(
  ems_ids = 22769669,
  date_range = c("2020-01-01", "2022-02-01")
)
}
```
