test_that("Complete sequence of functions inside joinDatabases ZIP", {

  fileDataPath <- list.files(test_path("studies", "summarised_zip"),
                             pattern = "zip",
                             full.names = TRUE)
  # fileDataPath <- "C:\\Users\\cbarboza\\Documents\\darwin-docs\\packages\\darwin-dev\\ReportGenerator\\results\\result_\\p3-c1-010-results-ipci\\p3-c1-010-results-ipci\\results_IPCI.zip"
  checkmate::assertCharacter(fileDataPath)

  # Configuration
  configData <- yaml.load_file(system.file("config",
                                           "variablesConfig.yaml",
                                           package = "ReportGenerator"))
  packagesNames <- names(configData)
  fileType <- getFileType(fileDataPath)
  unzipDir <- unzipDir()

  # `unzipFiles()` Unzip files
  databaseFolders <- unzipFiles(unzipDir = unzipDir,
                                fileDataPath = fileDataPath)

  # `extractCSV()` iterates folders for CSV files
  csv_files <- extractCSV(databaseFolders = databaseFolders,
                          configData = configData)


  # grep("incidence", csv_files, value = TRUE)
  #
  # acne_suicide_incidence_results <- omopgenerics::importSummarisedResult(csv_list$summarised_result_list[2])
  # glimpse(settings(acne_suicide_incidence_results))
  #
  # overall_suicide_incidence_results <- omopgenerics::importSummarisedResult(csv_list$summarised_result_list[4])
  # glimpse(settings(overall_suicide_incidence_results))
  #
  # psoriasis_suicide_incidence_results <- omopgenerics::importSummarisedResult(csv_list$summarised_result_list[2])
  # glimpse(settings(psoriasis_suicide_incidence_results))

  # Get list of csv files
  csv_list <- processCSV(csv_files)

  csv_list$summarised_result_list

  summarised_result <- omopgenerics::importSummarisedResult(csv_list$summarised_result_list)

  uploadedFiles <- summarised_result

})

test_that("Loading 1 zip files whole study", {
  fileDataPath <- list.files(test_path("studies", "summarised_zip"),
                             pattern = "zip",
                             full.names = TRUE)
  logger <- log4r::logger()
  unzipDir <- file.path(tempdir(), "single")
  uploadedFileDataList <- joinDatabases(fileDataPath = fileDataPath[1])
  expect_equal(length(uploadedFileDataList), 2)
  expect_type(uploadedFileDataList, "list")
  unlink(unzipDir, recursive = TRUE)
})

test_that("Loading multiple zip files whole study", {
  fileDataPath <- list.files(test_path("studies", "summarised_zip"),
                             pattern = "zip",
                             full.names = TRUE)
  unzipDir <- file.path(tempdir(), "multi")
  uploadedFileDataList <- joinDatabases(fileDataPath = fileDataPath)
  expect_equal(length(uploadedFileDataList), 2)
  expect_type(uploadedFileDataList, "list")
  unlink(unzipDir, recursive = TRUE)
})

test_that("getFileType() either zip or csv", {
  fileDataPath <- list.files(test_path("studies", "summarised_zip"),
                             pattern = "zip",
                             full.names = TRUE)
  fileType <- getFileType(fileDataPath)
  expect_equal(fileType, "zip")
  fileDataPath <- list.files(testthat::test_path("studies", "summarised_csv"),
                             pattern = "csv",
                             full.names = TRUE)
  fileType <- getFileType(fileDataPath)
  expect_equal(fileType, "csv")
})

test_that("unzipFiles() correctly", {
  unzipDir <- file.path(tempdir(), "unzipData")
  fileDataPath <- list.files(test_path("studies", "summarised_zip"),
                             pattern = "zip",
                             full.names = TRUE)

  # `unzipFiles()` Unzip files
  databaseFolders <- unzipFiles(unzipDir = unzipDir,
                                fileDataPath = fileDataPath)
  expect_length(databaseFolders, 5)
})

test_that("extractCSV() correctly", {
  configData <- yaml.load_file(system.file("config",
                                           "variablesConfig.yaml",
                                           package = "ReportGenerator"))
  unzipDir <- file.path(tempdir(), "unzipData")
  fileDataPath <- list.files(test_path("studies", "summarised_zip"),
                             pattern = "zip",
                             full.names = TRUE)

  databaseFolders <- unzipFiles(unzipDir = unzipDir,
                                fileDataPath = fileDataPath)

  # `extractCSV()` iterates folders for CSV files
  data <- extractCSV(databaseFolders = databaseFolders,
                     configData = configData)
  expect_character(data)

})

test_that("`getDatabaseName()` if there is a metadata file from TP", {
  # With metadata
  filesLocation <- list.files(testthat::test_path("studies", "summarised_csv"),
                              pattern = "csv",
                              full.names = TRUE)
  databaseName <- getDatabaseName(filesLocation = filesLocation)
  expect_equal(databaseName, "Synthea synthetic health database")

  # No metadata
  filesLocation <- list.files(testthat::test_path("studies", "summarised_csv"),
                              pattern = "csv",
                              full.names = TRUE)
  databaseName <- getDatabaseName(filesLocation = filesLocation[2])
  expect_null(databaseName)

})

test_that("processCSV() files into a list with summarisedResult objects", {
  configData <- yaml.load_file(system.file("config",
                                           "variablesConfig.yaml",
                                           package = "ReportGenerator"))

  filesLocation <- list.files(testthat::test_path("studies", "summarised_csv"),
                              pattern = "csv",
                              full.names = TRUE)

  databaseName <- getDatabaseName(filesLocation = filesLocation[2])

  # databaseName <- getDatabaseName(filesLocation = filesLocation)

  data <- processCSV(filesLocation)

  expect_length(data, 2)

})

test_that("processCSV() summarised result", {

  configData <- yaml.load_file(system.file("config",
                                           "variablesConfig.yaml",
                                           package = "ReportGenerator"))

  filesLocation <- list.files(testthat::test_path("studies",
                                                  "summarised_csv"),
                              pattern = "csv",
                              full.names = TRUE)

  databaseName <- getDatabaseName(filesLocation = filesLocation)

  filesLocation <- filesLocation[3]

  data <- processCSV(filesLocation)
  expect_length(data, 2)
})

test_that("processCSV() in a loop", {

  configData <- yaml.load_file(system.file("config",
                                           "variablesConfig.yaml",
                                           package = "ReportGenerator"))
  unzipDir <- file.path(tempdir(), "unzipData")
  fileDataPath <- list.files(test_path("studies", "summarised_zip"),
                             pattern = "zip",
                             full.names = TRUE)

  databaseFolders <- unzipFiles(unzipDir = unzipDir,
                                fileDataPath = fileDataPath)

  data <- list()

  for (i in 1:length(databaseFolders)) {
    # i = 1
    filesList <- databaseFolders[i]
    filesLocation <- list.files(filesList,
                                pattern = ".csv",
                                full.names = TRUE,
                                recursive = TRUE)
    data <- processCSV(filesLocation)
  }
  expect_length(data, 2)
})

test_that("Loading multiple csv files whole study", {
  fileDataPath <- list.files(testthat::test_path("studies", "summarised_csv"),
                             pattern = "csv",
                             full.names = TRUE)
  uploadedFiles <- joinDatabases(fileDataPath = fileDataPath)
  expect_equal(length(uploadedFiles), 2)
  expect_type(uploadedFiles, "list")
})

test_that("loadFileData iteration per result id", {
  data <- list()
  filesLocation <- testthat::test_path("studies", "summarised_csv", "treatmentPathways.csv")
  assertFileExists(filesLocation)
  configData <- yaml.load_file(system.file("config",
                                           "variablesConfig.yaml",
                                           package = "ReportGenerator"))
  resultsData <- read_csv(filesLocation, show_col_types = FALSE, col_types = c(.default = "c"))
  resultsColumns <- names(resultsData)
  fileName <- NULL

  data <- loadFileData(data,
                       filesLocation,
                       configData,
                       resultsData,
                       resultsColumns,
                       databaseName)

  expect_equal(names(data), "TreatmentPatterns")
  TreatmentData <- data$TreatmentPatterns
  expect_equal(names(TreatmentData), c("treatmentPathways"))
  expect_equal(TreatmentData$treatmentPathways %>% pull(cdm_name) %>% unique(), "CHUBX")
  expect_equal(TreatmentData$treatmentPathways %>% pull(cdm_name) %>% unique(), "CHUBX")

})

test_that("additional columns summary_characteristics", {
  data <- testData$CohortCharacteristics$summarised_characteristics

  result <- additionalCols(data)

  expect_equal(names(result), c("result_id", "cdm_name", "group_name", "group_level", "strata_name",
                                "strata_level", "variable_name", "variable_level", "estimate_name",
                                "estimate_type", "estimate_value", "additional_name", "additional_level"))

})

test_that("getPackageData returns data for summarised_characteristics", {
  data <- list()
  resultsData <- testData$CohortCharacteristics$summarised_characteristics
  package <- unique(settings(resultsData)$package_name)
  resultType <- unique(settings(resultsData)$result_type)[1]
  resultsColumns <- names(resultsData)
  configData <- yaml.load_file(system.file("config", "variablesConfig.yaml", package = "ReportGenerator"))
  logger <- log4r::logger()
  result <- getPackageData(data, package, resultType, resultsColumns, resultsData, configData)
  expect_equal(class(result), "list")
  expect_equal(names(result), "CohortCharacteristics")
  expect_equal(names(result$CohortCharacteristics$summarised_characteristics),
               c("result_id", "cdm_name", "group_name", "group_level", "strata_name", "strata_level", "variable_name",
                 "variable_level", "estimate_name", "estimate_type", "estimate_value",
                 "additional_name", "additional_level"))
})

test_that("additional columns summarised_large_scale_characteristics", {
  data <- testData$CohortCharacteristics$summarised_characteristics
  result <- additionalCols(data)
  expect_equal(names(result), c("result_id", "cdm_name", "group_name", "group_level", "strata_name",
                                "strata_level", "variable_name", "variable_level", "estimate_name",
                                "estimate_type", "estimate_value", "additional_name", "additional_level"))
})

test_that("getPackageData returns data for summarised_large_scale_characteristics", {
  data <- list()
  resultsData <- testData$CohortCharacteristics$summarised_large_scale_characteristics
  package <- unique(settings(resultsData)$package_name)
  resultType <- unique(settings(resultsData)$result_type)[1]
  resultsColumns <- names(resultsData)
  configData <- yaml.load_file(system.file("config", "variablesConfig.yaml", package = "ReportGenerator"))
  result <- getPackageData(data, package, resultType, resultsColumns, resultsData, configData)
  expect_equal(class(result), "list")
  expect_equal(names(result), "CohortCharacteristics")
  expect_equal(names(result$CohortCharacteristics$summarised_large_scale_characteristics),
               c("result_id", "cdm_name", "group_name", "group_level", "strata_name",
                 "strata_level", "variable_name", "variable_level", "estimate_name",
                 "estimate_type", "estimate_value", "additional_name", "additional_level"))
})

test_that("additional columns `Survival Estimate`", {
  data <- testData$CohortSurvival$single_event
  result <- additionalCols(data)
  expect_equal(names(result), c("result_id", "cdm_name", "group_name", "group_level", "strata_name",
                                "strata_level", "variable_name", "variable_level", "estimate_name",
                                "estimate_type", "estimate_value", "additional_name", "additional_level"))
})

test_that("getPackageData returns data from `Survival Estimate`", {
  data <- list()
  resultsData <- testData$CohortSurvival$single_event
  package <- unique(settings(resultsData)$package_name)
  resultType <- unique(settings(resultsData)$result_type)[1]
  resultsColumns <- names(resultsData)
  configData <- yaml.load_file(system.file("config", "variablesConfig.yaml", package = "ReportGenerator"))
  result <- getPackageData(data, package, resultType, resultsColumns, resultsData, configData)
  expect_equal(class(result), "list")
  expect_equal(names(result), "CohortSurvival")
  expect_equal(names(result$CohortSurvival$survival),
               c("result_id", "cdm_name", "group_name", "group_level", "strata_name", "strata_level", "variable_name",
                 "variable_level", "estimate_name", "estimate_type", "estimate_value",
                 "additional_name", "additional_level"))
})


test_that("Cohort Attrition", {


  configData <- yaml.load_file(system.file("config",
                                           "variablesConfig.yaml",
                                           package = "ReportGenerator"))
  unzipDir <- file.path(tempdir(), "unzipData")
  fileDataPath <- list.files(testthat::test_path("studies",
                                                 "misc",
                                                 "chondrosarcoma"),
                             pattern = "zip",
                             full.names = TRUE)

  databaseFolders <- unzipFiles(unzipDir = unzipDir,
                                fileDataPath = fileDataPath)

  data <- extractCSV(databaseFolders = databaseFolders,
                     configData = configData)
  expect_character(data)

})
