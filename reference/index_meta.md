# Title

Title

## Usage

``` r
index_meta()
```

## Value

Data frame of metadata variables

## Examples

``` r
index_meta()
#> # A tibble: 9 × 5
#>   bcgwcat_col        enmods_col         ems_col          aqua_col    notes      
#>   <chr>              <chr>              <chr>            <chr>       <chr>      
#> 1 location_id        Location_ID        ems_id           NA          NA         
#> 2 NA                 NA                 NA               SampleID    Aqua was E…
#> 3 observed_date_time Observed_Date_Time collection_start Sample_Date NA         
#> 4 medium             Medium             sample_state     Watertype   NA         
#> 5 location_latitude  Location_Latitude  latitude         Coord_Lat   NA         
#> 6 location_longitude Location_Longitude longitude        Coord_Long  NA         
#> 7 location_elevation Location_Elevation NA               NA          NA         
#> 8 NA                 Location_Name      NA               NA          NA         
#> 9 obs_well           NA                 obs_well         StationID   Bcgwcat is…
```
