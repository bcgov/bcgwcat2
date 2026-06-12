# Copyright 2026 Province of British Columbia
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
# http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.
# Steps to release a new version of the renmods package

# Bump version
file.edit("DESCRIPTION")

# Add changes to NEWS
file.edit("NEWS.md")

# Rebuild README
devtools::build_readme()

# Precompile Vignettes
source("vignettes/articles/precompile.R")

# Standard checks ------------------------------------------
devtools::run_examples()
devtools::check() # Ctrl-Shift-E to check via Terminal

# Run tests - Compare snapshots - BUILD PACKAGE FIRST
devtools::test() # Use Ctrl-Shift-T to test non-interactively
devtools::test(filter = "gw_app")
testthat::snapshot_review()
testthat::snapshot_review(path = "inst/shiny/tests/testthat")


# Additional quality checks -------------------------------

# Checks for good practices, but not code coverage
goodpractice::gp(checks = all_checks()[all_checks() != "cov"])

# Create local code coverage report for detailed exploration
covr::report(
  covr::package_coverage(function_exclusions = c("\\.onLoad", "^skip")),
  file = "covr/cov_report.html",
)
spelling::spell_check_package() # Check spelling in docs
urlchecker::url_check() # Check URLs in documentation

# Check docs
pkgdown::build_site()

# Platform checks
rhub::rhub_check() # Check on multiple platforms
devtools::check_win_devel() # Check on Windows R-devel

# Push changes
# Once CI clears, merge PR
# Create GitHub release

## Going up a version? Create signed release on github
usethis::use_github_release()

# Testing Docs ----------------------------------------
pkgdown::build_site()
pkgdown::build_article("bcgwcat")
pkgdown::build_article("ions")
pkgdown::build_article("piperplots")
pkgdown::build_reference_index()
