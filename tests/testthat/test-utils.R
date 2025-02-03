# IncidencePrevalence
test_that("getSummarisedData summarise_characteristics", {
  fileDataPath <- list.files(test_path("studies", "summarised_zip"),
                             pattern = "zip",
                             full.names = TRUE)
  uploadedData <- joinDatabases(fileDataPath)
  summarise_characteristics_result <- getSummarisedData(uploadedData$summarised_result, type_result = 'summarise_characteristics')
  expect_equal(settings(summarise_characteristics_result)$result_type, "summarise_characteristics")

})

test_that("getIncidencePrevalence", {
  fileDataPath <- list.files(test_path("studies", "summarised_zip"),
                             pattern = "zip",
                             full.names = TRUE)
  uploadedData <- joinDatabases(fileDataPath)
  survival_result <- getSummarisedData(uploadedData$summarised_result, type_result = 'survival')
  expect_equal(settings(survival_result)$analysis_type, c("competing_risk", "single_event"))
})



test_that("getItemsList all", {
  items <- c("incidence", "summarise_large_scale_characteristics", "point_prevalence",
             "period_prevalence", "summarise_characteristics", "competing_risk",
             "single_event", "TreatmentPatterns")
  menuList <- getItemsList(items)
  expect_equal(length(menuList), 6)
})


test_that("getItemsList all", {
  items <- c("cohortAttrition",
            "incidence_attrition",
             "prevalence_attrition",
             "incidence_estimates",
             "prevalence_estimates",
             "TreatmentPatterns",
             "summarise_characteristics",
             "summarise_large_scale_characteristics",
             "single_event",
             "competing_risk")
  menuList <- getItemsList(items)
  expect_equal(length(menuList), 8)
})

test_that("getItemsList summarised", {
  items <- c("summarise_characteristics", "summarise_characteristics")
  menuList <- getItemsList(items)
  expect_equal(length(menuList), 1)

  items <- c("summarise_characteristics")
  menuList <- getItemsList(items)
  expect_equal(length(menuList), 1)

  items <- c("summarise_characteristics")
  menuList <- getItemsList(items)
  expect_equal(length(menuList), 1)
})

test_that("getItemsList cohort attrition", {
  items <- "cohortAttrition"
  menuList <- getItemsList(items)
  expect_equal(menuList, "Cohort Attrition - Table")
})

test_that("getItemsList attrition both", {
  items <- c("incidence_attrition", "prevalence_attrition")
  menuList <- getItemsList(items)
  expect_equal(menuList, c("Incidence Attrition", "Prevalence Attrition"))
})

test_that("getItemsList only incidence_attrition", {
  items <- c("incidence_attrition")
  menuList <- getItemsList(items)
  expect_equal(menuList, c("Incidence Attrition"))
})

test_that("getItemsList only prevalence_attrition", {
  items <- c("prevalence_attrition")
  menuList <- getItemsList(items)
  expect_equal(menuList, c("Prevalence Attrition"))
})

test_that("getItemsList only incidence_estimate", {
  items <- c("incidence_estimates")
  menuList <- getItemsList(items)
  expect_equal(length(menuList), 0)
})

test_that("getItemsList only prevalence", {
  items <- c("prevalence_estimates")
  menuList <- getItemsList(items)
  expect_equal(length(menuList), 0)
})

test_that("getItemsList only summarise_characteristics", {
  items <- c("summarise_characteristics")
  menuList <- getItemsList(items)
  expect_equal(length(menuList), 1)
  expect_equal(menuList, "Summarised Characteristics")
})

# TreatmentPatterns

test_that("getItemsList treatmentPatterns", {
  items <- c("TreatmentPatterns")
  menuList <- getItemsList(items)
  expect_equal(menuList, c("Treatment Pathways"))
})

test_that("getItemsList joining to apps", {
  itemsList <- list(objects = NULL)
  itemsIP <- c("incidence_attrition", "prevalence_attrition", "incidence_estimates", "prevalence_estimates")
  itemsTP <- c("TreatmentPatterns")
  menuListIP <- getItemsList(itemsIP)
  menuListTP <- getItemsList(itemsTP)
  itemsList$objects[["items"]] <- c(itemsList$objects[["items"]], menuListIP)
  itemsList$objects[["items"]] <- c(itemsList$objects[["items"]], menuListTP)
  expect_equal(length(itemsList$objects[["items"]]), 3)
})

test_that("getItemConfig for getting a function", {
  input <- "object"
  output <- "function"
  # incidence table
  inputValue <- "incidence_table"
  expression <- getItemConfig(input = input,
                               output = output,
                               inputValue = inputValue)
  expect_equal(expression, "tableIncidence(incidence_estimates, type = \"gt\", .options = list())" )

  # incidence plot
  inputValue <- "incidence_plot"
  expression <- getItemConfig(input = input,
                              output = output,
                              inputValue = inputValue)
  expect_equal(expression, "plotIncidence(incidence_estimates)")

  # incidence
  inputValue <- "prevalence_table"
  expression <- getItemConfig(input = input,
                              output = output,
                              inputValue = inputValue)
  expect_equal(expression, "tablePrevalence(prevalence, type = \"gt\", .options = list())")

  # summarised_characteristics
  inputValue <- "summarised_characteristics"
  expression <- getItemConfig(input = input,
                              output = output,
                              inputValue = inputValue)
  expect_equal(expression, c("tableCharacteristics(summarisedCharacteristics)"))

  # summarised_large_scale_characteristics
  inputValue <- "summarised_large_scale_characteristics"
  expression <- getItemConfig(input = input,
                              output = output,
                              inputValue = inputValue)
  expect_equal(expression, c("tableCharacteristics(summarisedCharacteristics)"))

  # single_event
  inputValue <- "single_event"
  expression <- getItemConfig(input = input,
                              output = output,
                              inputValue = inputValue)
  expect_equal(expression, c("cohortSurvivalTable(survivalEstimate)"))

  # single_event
  inputValue <- "competing_risk"
  expression <- getItemConfig(input = input,
                              output = output,
                              inputValue = inputValue)
  expect_equal(expression, c("CohortSurvival::plotSurvival(survivalEstimate, facet, colour)"))

  # TreatmentPatterns
  # inputValue <- "TreatmentPatterns"
  # expression <- getItemConfig(input = input,
  #                             output = output,
  #                             inputValue = inputValue)
  # expect_equal(expression, c("CohortSurvival::plotSurvival(survivalEstimate, facet, colour)"))

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
                                                                 inputValue = "Incidence"),
                               previewItemType = "Facet by outcome")

  expect_equal(class(result), "character")
  expect_equal(result, "tableIncidence(incidence, type = \"gt\", .options = list())")

  # type might be empty, set default
  result <- addPreviewItemType(previewItemString = getItemConfig(input = "title",
                                                                 output = "function",
                                                                 inputValue = "Incidence"),
                               previewItemType = NULL)

  expect_equal(class(result), "character")
  expect_equal(result, "tableIncidence(incidence, type = \"gt\", .options = list())")
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
  items <- c("summarise_characteristics", "summarise_large_scale_characteristics")
  menuList <- getItemsList(items)
  expect_equal(length(menuList), 2)
})

test_that("PatientProfiles Summary", {
  items <- c("summarise_characteristics")
  menuList <- getItemsList(items)
  expect_equal(menuList, "Summarised Characteristics")
})

test_that("PatientProfiles LSC", {
  items <- c("summarise_large_scale_characteristics")
  menuList <- getItemsList(items)
  expect_equal(menuList, "Summarised Large Scale Characteristics")
})

