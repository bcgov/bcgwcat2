test_that("meq() calculates meq values", {
  s <- data.frame(
    bcgwcat_param = c(
      "calcium_dissolved",
      "chloride_dissolved",
      "sulfate_dissolved",
      "ph",
      "temperature"
    ),
    observed_property_id = c(
      "Calcium Dissolved",
      "Chloride Dissolved",
      "Sulfate Dissolved",
      "pH (acidity)",
      "Temperature"
    ),
    result_value = c(40.078, 35.453, 96.0626, 4, 10),
    result_unit = c("mg/L", "mg/L", "mg/L", "pH units", "C")
  )

  m <- meq(s)

  # Only parameters with meq conversions are kept
  expect_equal(
    m$bcgwcat_param,
    c("calcium_dissolved", "chloride_dissolved", "sulfate_dissolved", "ph")
  )
  expect_true(all(m$result_unit == "meq/l"))

  # mg/L divided by (atomic mass / valency)
  expect_equal(m$result_value[1:3], c(2, 1, 2))

  # pH converted to H+ meq/l: 10^(-pH) * 1000
  expect_equal(m$result_value[4], 0.1)
})
