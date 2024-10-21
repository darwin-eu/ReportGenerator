# IncidencePrevalence
test_that("getIncidencePrevalence", {
  fileDataPath <- "C:\\Users\\cbarboza\\Documents\\darwin-docs\\packages\\darwin-dev\\ReportGenerator\\results\\010\\p3-c1-010-results-ipci\\p3-c1-010-results-ipci\\results_IPCI.zip"
  uploadedData <- joinDatabases(fileDataPath)
  incidence_result <- getIncidencePrevalence(uploadedData, type = "incidence")

})

test_that("getItemsList all", {
  items <- c("incidence", "summarised_large_scale_characteristics", "point_prevalence",
             "period_prevalence", "summarised_characteristics", "competing_risk",
             "single_event")
  menuList <- getItemsList(items)
  expect_equal(length(menuList), 17)
})


test_that("getItemsList all", {
  items <- c("cohortAttrition",
            "incidence_attrition",
             "prevalence_attrition",
             "incidence_estimates",
             "prevalence_estimates",
             "treatmentPathways",
             "summarised_characteristics",
             "summarised_large_scale_characteristics",
             "single_event",
             "competing_risk")
  menuList <- getItemsList(items)
  expect_equal(length(menuList), 17)
})

test_that("getItemsList cohort attrition", {
  items <- "cohortAttrition"
  menuList <- getItemsList(items)
  expect_equal(menuList, "Cohort Attrition - Table")
})

test_that("getItemsList attrition both", {
  items <- c("incidence_attrition", "prevalence_attrition")
  menuList <- getItemsList(items)
  expect_equal(menuList, c("Number of participants - Table",
                           "Incidence Attrition - Table",
                           "Prevalence Attrition - Table"))
})

test_that("getItemsList only incidence_attrition", {
  items <- c("incidence_attrition")
  menuList <- getItemsList(items)
  expect_equal(menuList, c("Incidence Attrition - Table"))
})

test_that("getItemsList only prevalence_attrition", {
  items <- c("prevalence_attrition")
  menuList <- getItemsList(items)
  expect_equal(menuList, c("Prevalence Attrition - Table"))
})

test_that("getItemsList only incidence_estimate", {
  items <- c("incidence_estimates")
  menuList <- getItemsList(items)
  expect_equal(length(menuList), 3)
})

test_that("getItemsList only prevalence", {
  items <- c("prevalence_estimates")
  menuList <- getItemsList(items)
  expect_equal(length(menuList), 3)
})

test_that("getItemsList only summarised_characteristics", {
  items <- c("summarised_characteristics")
  menuList <- getItemsList(items)
  expect_equal(length(menuList), 1)
  expect_equal(menuList, "Summarised Characteristics - Table")
})

# TreatmentPatterns

test_that("getItemsList treatmentPatterns", {
  items <- c("treatmentPathways")
  menuList <- getItemsList(items)
  expect_equal(menuList, c("Treatment Pathways Interactive Plots"))
})

test_that("getItemsList joining to apps", {
  itemsList <- list(objects = NULL)
  itemsIP <- c("incidence_attrition", "prevalence_attrition", "incidence_estimates", "prevalence_estimates")
  itemsTP <- c("treatmentPathways")
  menuListIP <- getItemsList(itemsIP)
  menuListTP <- getItemsList(itemsTP)
  itemsList$objects[["items"]] <- c(itemsList$objects[["items"]], menuListIP)
  itemsList$objects[["items"]] <- c(itemsList$objects[["items"]], menuListTP)
  expect_equal(length(itemsList$objects[["items"]]), 10)
})

test_that("getItemConfig for getting a function", {
  title <- c("Number of participants - Table")
  expression <- getItemConfig(input = "title",
                              output = "function",
                              inputValue = title)
  expect_equal(expression, c("table1NumPar(incidence_attrition, prevalence_attrition)"))
})

test_that("getItemConfig for getting a function", {
  title <- c("Summarised Characteristics - Table")
  expression <- getItemConfig(input = "title",
                              output = "function",
                              inputValue = title)
  expect_equal(expression, c("tableCharacteristics(summarisedCharacteristics)"))
})

test_that("getItemConfig for getting options", {
  title <- c("Number of participants - Table")
  itemOptions <- getItemConfig(input = "title",
                               output = "options",
                               inputValue = title)
  expect_equal(itemOptions, NULL)
})

test_that("getItemConfig for getting options", {
  title <- c("Number of participants - Table")
  itemOptions <- getItemConfig(input = "title",
                               output = "options",
                               inputValue = title)
  expect_equal(itemOptions, NULL)
})

test_that("getItemConfig for getting options", {
  title <- c("Incidence rate per year - Plot")
  itemOptions <- getItemConfig(input = "title",
                               output = "function",
                               inputValue = title)
  expect_equal(itemOptions, "plotIncidence(incidence_estimates, colour, facet, ribbon, options)")
})

test_that("getItemConfig for getting options ERROR", {
  title <- c("Table - Number of Giberish")
  itemOptions <- getItemConfig(input = "title",
                               output = "options",
                               inputValue = title)
  expect_equal(itemOptions, NULL)
})

test_that("getFunctionReport error more than length 1", {
  title <- c("Number of participants - Table", "Incidence Attrition - Table")
  expect_error(getItemConfig(input = "title",
                             output = "function",
                             inputValue = title))
})

test_that("addPreviewItemType happy flow", {
  result <- addPreviewItemType(previewItemString = getItemConfig(input = "title",
                                                                 output = "function",
                                                                 inputValue = "Incidence rate per year - Plot"),
                               previewItemType = "Facet by outcome")

  expect_equal(class(result), "character")
  expect_equal(result, "plotIncidence(incidence_estimates, colour = 'cdm_name', facet = 'outcome_cohort_name', ribbon, options)")

  # type might be empty, set default
  result <- addPreviewItemType(previewItemString = getItemConfig(input = "title",
                                                                 output = "function",
                                                                 inputValue = "Incidence rate per year - Plot"),
                               previewItemType = NULL)

  expect_equal(class(result), "character")
  expect_equal(result, "plotIncidence(incidence_estimates, colour = 'cdm_name', facet = 'outcome_cohort_name', ribbon, options)")
})


test_that("addPreviewItemType edge cases", {
  result <- addPreviewItemType(previewItemString = "incidenceRatePerYearPlot_no_brackets",
                               previewItemType = "Facet by outcome")
  expect_equal(result, "incidenceRatePerYearPlot_no_brackets")

  result <- addPreviewItemType(previewItemString = "",
                               previewItemType = "Facet by outcome")
  expect_equal(result, "")
})

# PatientProfiles

test_that("PatientProfiles Both Summaries", {
  items <- c("summarised_characteristics", "summarised_large_scale_characteristics")
  menuList <- getItemsList(items)
  expect_equal(length(menuList), 2)
})

test_that("PatientProfiles Summary", {
  items <- c("summarised_characteristics")
  menuList <- getItemsList(items)
  expect_equal(menuList, "Summarised Characteristics - Table")
})

test_that("PatientProfiles LSC", {
  items <- c("summarised_large_scale_characteristics")
  menuList <- getItemsList(items)
  expect_equal(menuList, "Summarised Large Scale Characteristics - Table")
})

