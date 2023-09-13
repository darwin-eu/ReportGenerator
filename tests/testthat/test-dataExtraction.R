test_that("loading zip files", {
  fileDataPath <- list.files(system.file("extdata", "zip", package = "ReportGenerator"), pattern = "zip", full.names = TRUE)[1]
  variablesConfigYaml(fileDataPath = fileDataPath,
                      package = "IncidencePrevalence",
                      version = "0.4.1")
  configData <- yaml.load_file(system.file("config", "variablesConfig.yaml", package = "ReportGenerator"))
  package <- "IncidencePrevalence"
  versionData <- "0.4.1"
  configData <- configData[[package]][[versionData]]
  expect_equal(length(configData), 4)
  expect_type(configData, "list")
})

test_that("loading zip files", {
  uploadedFiles <- list.files(system.file("extdata", "zip", package = "ReportGenerator"), pattern = "zip", full.names = TRUE)
  csvLocation <- file.path(tempdir(), "testLocation")
  joinedZipFiles <- joinDatabase(uploadedFiles = uploadedFiles, csvLocation = csvLocation)
  unlink(csvLocation, recursive = TRUE)
  expect_equal(length(joinedZipFiles), 4)
  expect_type(joinedZipFiles, "character")
})

test_that("loading csv files", {
  uploadedFiles <- list.files(system.file("extdata", "csv", package = "ReportGenerator"), pattern = "csv", full.names = TRUE)
  csvLocation <- file.path(tempdir(), "testLocation")
  joinedZipFiles <- joinDatabase(uploadedFiles = uploadedFiles, csvLocation = csvLocation)
  unlink(csvLocation, recursive = TRUE)
  expect_equal(length(joinedZipFiles), 4)
  expect_type(joinedZipFiles, "character")
})

test_that("column check for csv files", {
  csvFiles <- list.files(system.file("extdata", "csv", package = "ReportGenerator"), pattern = "csv", full.names = TRUE)
  configData <- yaml.load_file(system.file("config", "variablesConfig.yaml", package = "ReportGenerator"))
  package <- "IncidencePrevalence"
  versionData <- "0.4.1"
  configData <- configData[[package]][[versionData]]
  configDataTypes <- names(configData)
  data <- columnCheck(csvFiles, configData, configDataTypes)
  expect_equal(length(data), 4)
  expect_type(data, "list")
})

test_that("data clean incidence attrition type", {
  incidence_attrition <- dataCleanAttrition(incidence_attrition = incidence_attrition_latest)
  expect_type(incidence_attrition, "list")
})

test_that("data clean prevalence attrition type", {
  prevalence_attrition <- dataCleanAttrition(prevalence_attrition = prevalence_attrition_latest)
  expect_type(prevalence_attrition, "list")
})


