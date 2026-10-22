test_that("fmt_minimal() - no multiple MEQ values for EMS ID 22769669", {
  # Uses the local renmods cache (all data types), which is too large for CI
  skip_on_cran()
  skip_on_ci()

  # All dates
  s <- fetch_renmods(ems_ids = "22769669", date_range = NULL)
  expect_gt(nrow(s), 0)

  # NOTE: Expected to fail for now (and that's ok). This site currently
  # triggers "Multiple MEQ values for single parameter observations" from
  # `fix_multiple_measures(type = "meq")` inside `charge_balance()`.
  expect_no_message(fmt_minimal(s), message = "Multiple MEQ values")
})
