# Analysis sum

test_that("Analysis Sum", {
  # fileDataPath <- list.files(test_path("studies", "summarised_zip"),
  #                            pattern = "zip",
  #                            full.names = TRUE)[1]

  fileDataPath <- list.files("~/Documents/darwin-docs/studyPackages/Thromboembolic/P3C3005TEandcancer/results", full.names = TRUE)

  data_joined <- joinDatabases(fileDataPath)
  settingsData <- settings(data_joined$summarised_result)

  items <- analysisNamesAvailable(settingsData = settingsData)

  expect_equal(items, c("incidence", "incidence_attrition", "summarise_large_scale_characteristics",
                        "prevalence", "prevalence_attrition", "summarise_characteristics",
                        "competing_risk", "single_event"))
})

# test_that("Analysis Type not found", {
#   fileDataPath <- "~/Documents/darwin-docs/packages/darwin-dev/ReportGenerator/results/meeting/erasmus/P3-C1-019-Suicide/p3-c1-019-2_Study_results.zip"
#
#   data_joined <- joinDatabases(fileDataPath)
#   settingsData <- settings(data_joined$summarised_result)
#
#   items <- analysisNamesAvailable(settingsData = settingsData)
#
#   expect_equal(items, c("summarise_characteristics", "incidence", "incidence_attrition" ))
# })

test_that("getItemsList all", {
  items <- c("incidence",
             "prevalence",
             "summarise_large_scale_characteristics",
             "summarise_characteristics",
             "competing_risk",
             "single_event",
             "TreatmentPatterns")
  menuList <- getItemsList(items)
  expect_equal(length(menuList), 7)
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
  items <- c("incidence", "prevalence")
  menuList <- getItemsList(items)
  expect_equal(menuList, c("Incidence", "Prevalence"))
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
  itemsIP <- c("incidence", "prevalence")
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
  expect_equal(expression, "tableIncidence" )

  # incidence plot
  inputValue <- "incidence_plot"
  expression <- getItemConfig(input = input,
                              output = output,
                              inputValue = inputValue)
  expect_equal(expression, "plotIncidence")

  # incidence
  inputValue <- "prevalence_table"
  expression <- getItemConfig(input = input,
                              output = output,
                              inputValue = inputValue)
  expect_equal(expression, "tablePrevalence")

  # summarised_characteristics
  inputValue <- "summarised_characteristics"
  expression <- getItemConfig(input = input,
                              output = output,
                              inputValue = inputValue)
  expect_equal(expression, c("tableCharacteristics"))

  # summarised_large_scale_characteristics
  inputValue <- "summarised_large_scale_characteristics"
  expression <- getItemConfig(input = input,
                              output = output,
                              inputValue = inputValue)
  expect_equal(expression, c("tableLargeScaleCharacteristics"))

  # single_event table
  inputValue <- "single_event - Table"
  expression <- getItemConfig(input = input,
                              output = output,
                              inputValue = inputValue)
  expect_equal(expression, c("tableSurvival"))

  # single_event table
  inputValue <- "competing_risk - Table"
  expression <- getItemConfig(input = input,
                              output = output,
                              inputValue = inputValue)
  expect_equal(expression, c("tableSurvival"))

  # single_event plot
  inputValue <- "single_event - Plot"
  expression <- getItemConfig(input = input,
                              output = output,
                              inputValue = inputValue)
  expect_equal(expression, c("plotSurvival"))

  # single_event plot
  inputValue <- "competing_risk - Plot"
  expression <- getItemConfig(input = input,
                              output = output,
                              inputValue = inputValue)
  expect_equal(expression, c("plotSurvival"))

  # TreatmentPatterns
  inputValue <- "Sunburst Plot - TreatmentPatterns"
  expression <- getItemConfig(input = input,
                              output = output,
                              inputValue = inputValue)
  expect_equal(expression, c("ggSunburst"))

})

test_that("getFunctionReport error more than length 1", {
  title <- c("Number of participants - Table", "Incidence Attrition - Table")
  expect_error(getItemConfig(input = "title",
                             output = "function",
                             inputValue = title))
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


