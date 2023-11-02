test_that("getItemsList all", {
  items <- c("incidence_attrition", "prevalence_attrition", "incidence_estimates", "prevalence_estimates", "treatmentPathways")
  menuList <- getItemsList(items)
  expect_equal(length(menuList), 12)
})

test_that("getItemsList attrition both", {
  items <- c("incidence_attrition", "prevalence_attrition")
  menuList <- getItemsList(items)
  expect_equal(menuList, c("Table - Number of participants",
                           "Table - Incidence Attrition",
                           "Table - Prevalence Attrition"))
})

test_that("getItemsList only incidence_attrition", {
  items <- c("incidence_attrition")
  menuList <- getItemsList(items)
  expect_equal(menuList, c("Table - Incidence Attrition"))
})

test_that("getItemsList only prevalence_attrition", {
  items <- c("prevalence_attrition")
  menuList <- getItemsList(items)
  expect_equal(menuList, c("Table - Prevalence Attrition"))
})

test_that("getItemsList only incidence_estimate", {
  items <- c("incidence_estimates")
  menuList <- getItemsList(items)
  expect_equal(length(menuList), 4)
})

test_that("getItemsList only prevalence", {
  items <- c("prevalence_estimates")
  menuList <- getItemsList(items)
  expect_equal(length(menuList), 3)
})

test_that("getItemsList treatmentPatterns", {
  items <- c("treatmentPathways")
  menuList <- getItemsList(items)
  expect_equal(menuList, c("Sunburst Plot - TreatmentPatterns",
                           "Sankey Diagram - TreatmentPatterns"))
})

test_that("getFunction table1NumPar", {
  functionText <- getFunction(name = "table1NumPar")
  expect_equal(functionText, c("table1NumPar(incidence_attrition, prevalence_attrition)"))
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

