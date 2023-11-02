genericChecks <- function(result) {
  expect_equal(class(result), "data.frame")
  expect_equal(colnames(result), c("title", "signature"))
}

test_that("getItemsList happy flow", {

  uploadedFiles <- c("incidence_attrition",
                     "prevalence_attrition")

  result <- getItemsList(uploadedFiles)

  genericChecks(result)
  expect_equal(nrow(result), 1)

  uploadedFiles <- c("incidence_attrition",
                     "prevalence_attrition",
                     "incidence_estimates")

  result <- getItemsList(uploadedFiles)

  genericChecks(result)
  expect_equal(nrow(result), 5)

  uploadedFiles <- c("incidence_attrition",
                     "prevalence_attrition",
                     "incidence_estimates",
                     "prevalence_estimates")

  result <- getItemsList(uploadedFiles)

  genericChecks(result)
  expect_equal(nrow(result), 8)
})

test_that("getItemsList edge cases", {

  result <- getItemsList(uploadedFiles = NULL)

  genericChecks(result)
  expect_equal(nrow(result), 0)
})

test_that("addPreviewItemType happy flow", {
  result <- addPreviewItemType(previewItemString = "plotIncidence(incidenceCommonData(), colour, facet)",
                               previewItemType = "Facet by outcome")

  expect_equal(class(result), "character")
  expect_equal(result, "plotIncidence(incidenceCommonData(), colour = 'cdm_name', facet = 'outcome_cohort_name')")

  # type might be empty, set default
  result <- addPreviewItemType(previewItemString = "plotIncidence(incidenceCommonData(), colour, facet)",
                               previewItemType = NULL)

  expect_equal(class(result), "character")
  expect_equal(result, "plotIncidence(incidenceCommonData(), colour = 'cdm_name', facet = 'outcome_cohort_name')")
})


test_that("addPreviewItemType edge cases", {
  result <- addPreviewItemType(previewItemString = "incidenceRatePerYearPlot_no_brackets",
                               previewItemType = "Facet by outcome")
  expect_equal(result, "incidenceRatePerYearPlot_no_brackets")

  result <- addPreviewItemType(previewItemString = "",
                               previewItemType = "Facet by outcome")
  expect_equal(result, "")
})

