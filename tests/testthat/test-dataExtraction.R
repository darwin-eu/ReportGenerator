test_that("Loading 1 zip files whole study", {
  fileDataPath <- list.files(testthat::test_path("studies", "zip"),
                             pattern = "zip",
                             full.names = TRUE)
  logger <- log4r::logger()
  uploadedFileDataList <- joinDatabases(fileDataPath = fileDataPath[1],
                                 logger = logger)
  expect_equal(length(uploadedFileDataList), 4)
  expect_type(uploadedFileDataList, "list")
})

test_that("Loading multiple zip files whole study", {
  fileDataPath <- list.files(testthat::test_path("studies", "zip"),
                             pattern = "zip",
                             full.names = TRUE)
  logger <- log4r::logger()
  uploadedFileDataList <- joinDatabases(fileDataPath = fileDataPath,
                                 logger = logger)
  expect_equal(length(uploadedFileDataList), 4)
  expect_type(uploadedFileDataList, "list")
})

test_that("getFileType() either zip or csv", {
  fileDataPath <- list.files(testthat::test_path("studies", "zip"),
                             pattern = "zip",
                             full.names = TRUE)
  fileType <- getFileType(fileDataPath)
  expect_equal(fileType, "zip")
  fileDataPath <- list.files(testthat::test_path("studies", "csv"),
                             pattern = "csv",
                             full.names = TRUE)
  fileType <- getFileType(fileDataPath)
  expect_equal(fileType, "csv")

})

test_that("unzipFiles() correctly", {
  unzipDir <- file.path(tempdir(), "unzipData")
  fileDataPath <- list.files(testthat::test_path("studies", "zip"),
                             pattern = "zip",
                             full.names = TRUE)

  # `unzipFiles()` Unzip files
  databaseFolders <- unzipFiles(unzipDir = unzipDir,
                                fileDataPath = fileDataPath,
                                logger = logger)
  expect_length(databaseFolders, 5)
})

test_that("extractCSV() correctly", {
  configData <- yaml.load_file(system.file("config",
                                           "variablesConfig.yaml",
                                           package = "ReportGenerator"))
  unzipDir <- file.path(tempdir(), "unzipData")
  fileDataPath <- list.files(testthat::test_path("studies", "zip"),
                             pattern = "zip",
                             full.names = TRUE)

  fileDataPath <- list.files("C:/Users/cbarboza/Documents/darwin-docs/studyPackages/P2C1014PrescriptionsICU/results",
                             pattern = "zip",
                             full.names = TRUE)
  databaseFolders <- unzipFiles(unzipDir = unzipDir,
                                fileDataPath = fileDataPath,
                                logger = logger)

  # `extractCSV()` iterates folders for CSV files
  data <- extractCSV(databaseFolders = databaseFolders,
                     configData = configData,
                     logger = logger)
  expect_list(data)

})

test_that("`getDatabaseName()` if there is a metadata file from TP", {
  # With metadata
  filesLocation <- list.files(testthat::test_path("studies", "csv"),
                              pattern = "csv",
                              full.names = TRUE)
  databaseName <- getDatabaseName(filesLocation = filesLocation)
  expect_equal(databaseName, "Synthea synthetic health database")

  # No metadata
  filesLocation <- list.files(testthat::test_path("studies", "csv"),
                              pattern = "csv",
                              full.names = TRUE)
  databaseName <- getDatabaseName(filesLocation = filesLocation[2])
  expect_null(databaseName)

})

test_that("processCSV() files into a list with summarisedResult objects", {
  configData <- yaml.load_file(system.file("config",
                                           "variablesConfig.yaml",
                                           package = "ReportGenerator"))

  filesLocation <- list.files(testthat::test_path("studies", "csv"),
                              pattern = "csv",
                              full.names = TRUE)

  databaseName <- getDatabaseName(filesLocation = filesLocation[2])

  # databaseName <- getDatabaseName(filesLocation = filesLocation)

  data <- processCSV(data = NULL, filesLocation, configData, databaseName)

  expect_length(data, 4)

})


test_that("processCSV() in a loop", {

  configData <- yaml.load_file(system.file("config",
                                           "variablesConfig.yaml",
                                           package = "ReportGenerator"))
  unzipDir <- file.path(tempdir(), "unzipData")
  fileDataPath <- list.files(testthat::test_path("studies", "zip"),
                             pattern = "zip",
                             full.names = TRUE)

  databaseFolders <- unzipFiles(unzipDir = unzipDir,
                                fileDataPath = fileDataPath,
                                logger = logger)

  data <- list()

  for (i in 1:length(databaseFolders)) {
    # i = 1
    filesList <- databaseFolders[i]
    filesLocation <- list.files(filesList,
                                pattern = ".csv",
                                full.names = TRUE,
                                recursive = TRUE)
    databaseName <- getDatabaseName(filesLocation)
    data <- processCSV(data, filesLocation, configData, databaseName)
  }
  expect_length(data, 4)
})

test_that("Loading 1 csv files whole study", {
  csvLocation <- file.path(tempdir(), "dataLocation")
  dir.create(csvLocation)
  fileDataPath <- list.files(testthat::test_path("studies", "csv"),
                             pattern = "csv",
                             full.names = TRUE)
  fileName <- list.files(testthat::test_path("studies", "csv"),
                         pattern = "csv")
  fileName <- tools::file_path_sans_ext(fileName)
  logger <- log4r::logger()
  uploadedFiles <- joinDatabases(fileDataPath = fileDataPath[3],
                                fileName = fileName[3],
                                logger = logger)
  expect_equal(length(uploadedFiles), 1)
  expect_type(uploadedFiles, "list")
  unlink(csvLocation, recursive = TRUE)
})

test_that("Loading multiple csv files whole study", {
  csvLocation <- file.path(tempdir(), "dataLocation")
  dir.create(csvLocation)
  fileDataPath <- list.files(testthat::test_path("studies", "csv"),
                             pattern = "csv",
                             full.names = TRUE)
  fileName <- list.files(testthat::test_path("studies", "csv"),
                         pattern = "csv")
  fileName <- tools::file_path_sans_ext(fileName)
  logger <- log4r::logger()
  uploadedFiles <- joinDatabases(fileDataPath = fileDataPath,
                                fileName = fileName,
                                logger = logger)
  expect_equal(length(uploadedFiles), 4)
  expect_type(uploadedFiles, "list")
  unlink(csvLocation, recursive = TRUE)
})

test_that("loadFileData iteration per result id", {
  data <- list()
  filesLocation <- testthat::test_path("studies", "misc", "ls_chr_results.csv")
  configData <- yaml.load_file(system.file("config",
                                           "variablesConfig.yaml",
                                           package = "ReportGenerator"))
  resultsData <- read_csv(filesLocation, show_col_types = FALSE, col_types = c(.default = "c"))
  resultsColumns <- names(resultsData)

  data <- loadFileData(data,
                       fileName,
                       configData,
                       resultsData,
                       resultsColumns,
                       databaseName,
                       logger)

  expect_equal(names(data), "CohortCharacteristics")
  CohortCharacteristicsData <- data$CohortCharacteristics
  expect_equal(names(CohortCharacteristicsData), c("summarised_characteristics", "summarised_large_scale_characteristics"))
  expect_equal(CohortCharacteristicsData$summarised_characteristics %>% pull(result_id) %>% unique(), 1)
  expect_equal(CohortCharacteristicsData$summarised_large_scale_characteristics %>% pull(result_id) %>% unique(), c(2,3))

})

test_that("loadFileData iteration per result id two databases", {

  data <- list()
  filesLocation <- testthat::test_path("studies", "misc", "chr_results.csv")
  configData <- yaml.load_file(system.file("config",
                                           "variablesConfig.yaml",
                                           package = "ReportGenerator"))
  resultsData <- read_csv(filesLocation, show_col_types = FALSE, col_types = c(.default = "c"))
  resultsColumns <- names(resultsData)


  data <- loadFileData(data,
                       fileName,
                       configData,
                       resultsData,
                       resultsColumns,
                       databaseName,
                       logger)

  filesLocation <- testthat::test_path("studies", "misc", "chr_results_other_db.csv")
  resultsData <- read_csv(filesLocation, show_col_types = FALSE, col_types = c(.default = "c"))
  resultsColumns <- names(resultsData)

  data <- loadFileData(data,
                       fileName,
                       configData,
                       resultsData,
                       resultsColumns,
                       databaseName,
                       logger)

  expect_equal(names(data), "CohortCharacteristics")
  CohortCharacteristicsData <- data$CohortCharacteristics
  expect_equal(names(CohortCharacteristicsData), c("summarised_characteristics"))
  expect_equal(CohortCharacteristicsData$summarised_characteristics %>% pull(result_id) %>% unique(), 1)
  expect_equal(CohortCharacteristicsData$summarised_characteristics %>% pull(cdm_name) %>% unique(), c("IPCI-20240430", "BORDEAUX"))
})

test_that("loadFileData iteration per result id error same files 'checkGroupCount()'", {
  data <- list()
  filesLocation <- testthat::test_path("studies", "misc", "chr_results.csv")
  configData <- yaml.load_file(system.file("config",
                                           "variablesConfig.yaml",
                                           package = "ReportGenerator"))
  resultsData <- read_csv(filesLocation, show_col_types = FALSE, col_types = c(.default = "c"))
  resultsColumns <- names(resultsData)


  data <- loadFileData(data,
                       fileName,
                       configData,
                       resultsData,
                       resultsColumns,
                       databaseName,
                       logger)

  filesLocation <- testthat::test_path("studies", "misc", "chr_results.csv")
  resultsData <- read_csv(filesLocation, show_col_types = FALSE, col_types = c(.default = "c"))
  resultsColumns <- names(resultsData)

  expect_error(data <- loadFileData(data,
                                    fileName,
                                    configData,
                                    resultsData,
                                    resultsColumns,
                                    databaseName,
                                    logger))
})

test_that("additional columns summary_characteristics", {
  data <- testData$summarised_characteristics

  result <- additionalCols(data)

  expect_equal(names(result), c("result_id", "cdm_name", "group_name", "group_level", "strata_name", "strata_level", "variable_name",
                                "variable_level", "estimate_name", "estimate_type", "estimate_value",
                                "additional_name", "additional_level", "result_type", "package_name", "package_version"))

})

test_that("getPackageData returns data for summarised_characteristics", {
  data <- list()
  resultsData <- testData$summarised_characteristics
  package <- unique(resultsData$package_name)
  resultType <- unique(resultsData$result_type)[1]
  resultsColumns <- names(resultsData)
  configData <- yaml.load_file(system.file("config", "variablesConfig.yaml", package = "ReportGenerator"))
  logger <- log4r::logger()
  result <- getPackageData(data, package, resultType, resultsColumns, resultsData, configData, logger = logger)
  expect_equal(class(result), "list")
  expect_equal(names(result), "PatientProfiles")
  expect_equal(names(result$PatientProfiles$summarised_characteristics),
               c("result_id", "cdm_name", "group_name", "group_level", "strata_name", "strata_level", "variable_name",
                 "variable_level", "estimate_name", "estimate_type", "estimate_value",
                 "additional_name", "additional_level", "result_type", "package_name", "package_version"))
})

test_that("additional columns summarised_large_scale_characteristics", {
  data <- testData$summarised_large_scale_characteristics
  result <- additionalCols(data)
  expect_equal(names(result), c("result_id", "cdm_name", "group_name", "group_level", "strata_name", "strata_level",
                                "variable_name", "variable_level", "estimate_name", "estimate_type", "estimate_value",
                                "additional_name", "additional_level", "table_name", "type", "analysis", "result_type",
                                "package_name", "package_version"))
})

test_that("getPackageData returns data for summarised_large_scale_characteristics", {
  data <- list()
  resultsData <- testData$summarised_large_scale_characteristics
  package <- unique(resultsData$package_name)
  resultType <- unique(resultsData$result_type)
  resultsColumns <- names(resultsData)
  configData <- yaml.load_file(system.file("config", "variablesConfig.yaml", package = "ReportGenerator"))
  logger <- log4r::logger()
  result <- getPackageData(data, package, resultType, resultsColumns, resultsData, configData, logger = logger)
  expect_equal(class(result), "list")
  expect_equal(names(result), "PatientProfiles")
  expect_equal(names(result$PatientProfiles$summarised_large_scale_characteristics),
               c("result_id", "cdm_name",
                 "group_name", "group_level", "strata_name", "strata_level", "variable_name",
                 "variable_level", "estimate_name", "estimate_type", "estimate_value",
                 "additional_name", "additional_level", "table_name", "type", "analysis", "result_type",
                 "package_name", "package_version"))
})

test_that("additional columns `Survival Estimate`", {
  data <- testData$`Survival estimate`
  result <- additionalCols(data)
  expect_equal(names(result), c("result_id", "cdm_name", "group_name", "group_level", "strata_name", "strata_level", "variable_name",
                                "variable_level", "estimate_name", "estimate_type", "estimate_value",
                                "additional_name", "additional_level", "result_type", "package_name", "package_version", "analysis_type"
  ))
})

test_that("getPackageData returns data from `Survival Estimate`", {
  data <- list()
  resultsData <- testData$`Survival estimate`
  package <- unique(resultsData$package_name)
  resultType <- unique(resultsData$result_type)
  resultsColumns <- names(resultsData)
  configData <- yaml.load_file(system.file("config", "variablesConfig.yaml", package = "ReportGenerator"))
  logger <- log4r::logger()
  result <- getPackageData(data, package, resultType, resultsColumns, resultsData, configData, logger = logger)
  expect_equal(class(result), "list")
  expect_equal(names(result), "CohortSurvival")
  expect_equal(names(result$CohortSurvival$`survival`),
               c("result_id", "cdm_name", "group_name", "group_level", "strata_name", "strata_level", "variable_name",
                 "variable_level", "estimate_name", "estimate_type", "estimate_value",
                 "additional_name", "additional_level", "result_type", "package_name", "package_version", "analysis_type"))
})
