genericChecks <- function(result) {
  expect_equal(class(result), "data.frame")
  expect_equal(colnames(result), c("title", "signature"))
}

test_that("getItemsList happy flow", {

  uploadedFiles <- c("incidence_attrition",
                     "incidence_estimates",
                     "prevalence_attrition")

  result <- getItemsList(uploadedFiles)

  genericChecks(result)
  expect_equal(nrow(result), 9)

  uploadedFiles <- c("incidence_attrition",
                     "prevalence_attrition")

  result <- getItemsList(uploadedFiles)

  genericChecks(result)
  expect_equal(nrow(result), 1)
})

test_that("getItemsList edge cases", {

  result <- getItemsList(uploadedFiles = NULL)

  genericChecks(result)
  expect_equal(nrow(result), 0)
})

test_that("addPreviewItemType happy flow", {
  result <- addPreviewItemType(previewItemString = "incidenceRatePerYearPlot(incidence_estimates)",
                               previewItemType = "Facet by outcome")

  expect_equal(class(result), "character")
  expect_equal(result, "incidenceRatePerYearPlot(incidence_estimates, \"Facet by outcome\")")

  # type might be empty, set default
  result <- addPreviewItemType(previewItemString = "incidenceRatePerYearPlot(incidence_estimates)",
                               previewItemType = NULL)

  expect_equal(class(result), "character")
  expect_equal(result, "incidenceRatePerYearPlot(incidence_estimates, \"Facet by outcome\")")
})


test_that("addPreviewItemType edge cases", {
  result <- addPreviewItemType(previewItemString = "incidenceRatePerYearPlot_no_brackets",
                               previewItemType = "Facet by outcome")
  expect_equal(result, "incidenceRatePerYearPlot_no_brackets")

  result <- addPreviewItemType(previewItemString = "",
                               previewItemType = "Facet by outcome")
  expect_equal(result, "")
})
