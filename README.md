<!-- badges: start -->

[![img](https://img.shields.io/badge/Lifecycle-Experimental-339999)](https://github.com/bcgov/repomountie/blob/master/doc/lifecycle-badges.md)
[![R-CMD-check](https://github.com/bcgov/bcgwcat2/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/bcgov/bcgwcat2/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

# bcgwcat2 - Groundwater Chemistry Analysis Tool

> Adapted from [bcgwcat](https://github.com/bcgov/bcgwcat) in light of
> the transition from EMS to EnMoDS. This may replace bcgwcat entirely
> in the future.

The goal of bcgwcat2 is to provide easy access to EnMods data as well as
to provide workflow tools specific to those working with groundwater.

> **Warning! This tool is a work in progress and is not ready for use.**

### Features

These tools are available through R functions as well as through a
**Shiny user-interface**. This tool can help with:

- Downloading EnMoDS data via the
  [renmods](http://github.com/bcgov/renmods) package
- Formatting data for easier use in R or other software (e.g., AquaChem)
- Calculating charge balances
- Calculating water types
- Providing water quality summaries
- Piperplots and Stiff plots

### Installation

Install/Update [**R**](https://cloud.r-project.org/) and
[**RStudio**](https://rstudio.com/)

Then install this R package:

``` r
# install.packages("pak", dependencies = TRUE) # If you don't already have pak installed
pak::pkg_install("bcgov/bcgwcat2") #"Update All" if asked
```

### Usage

- Launch the Shiny App - `bcgwcat_app()`

#### Example

This is a basic example which shows you how to solve a common problem:

``` r
## basic example code
```

### Project Status

Early development

### Getting Help or Reporting an Issue

To report bugs/issues/feature requests, please file an
[issue](https://github.com/bcgov/bcgwcat2/issues/).

### How to Contribute

If you would like to contribute to the package, please see our
[CONTRIBUTING](CONTRIBUTING.md) guidelines.

Please note that this project is released with a [Contributor Code of
Conduct](CODE_OF_CONDUCT.md). By participating in this project you agree
to abide by its terms.

### License

    Copyright 2026 Province of British Columbia

    Licensed under the Apache License, Version 2.0 (the &quot;License&quot;);
    you may not use this file except in compliance with the License.
    You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on an &quot;AS IS&quot; BASIS,
    WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
    See the License for the specific language governing permissions and limitations under the License.
