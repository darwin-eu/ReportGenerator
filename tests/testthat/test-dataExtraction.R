test_that("Loading 1 zip files whole study", {
  outputDir <- file.path(tempdir(), "dataLocation")
  dir.create(outputDir)
  fileDataPath <- list.files(testthat::test_path("studies", "zip"),
                             pattern = "zip",
                             full.names = TRUE)
  logger <- log4r::logger()
  uploadedFiles <- joinDatabases(fileDataPath = fileDataPath[1],
                                 outputDir = outputDir,
                                 logger = logger)
  expect_equal(length(uploadedFiles), 4)
  expect_type(uploadedFiles, "list")
  unlink(csvLocation, recursive = TRUE)
})

test_that("Loading multiple zip files whole study", {
  csvLocation <- file.path(tempdir(), "dataLocation")
  dir.create(csvLocation)
  fileDataPath <- list.files(testthat::test_path("studies", "zip"),
                             pattern = "zip",
                             full.names = TRUE)
  logger <- log4r::logger()
  uploadedFiles <- joinDatabases(fileDataPath = fileDataPath,
                                csvLocation = csvLocation,
                                logger = logger)
  expect_equal(length(uploadedFiles), 4)
  expect_type(uploadedFiles, "list")
  unlink(csvLocation, recursive = TRUE)
})

# Melanoma (runs just with local dataset)

# test_that("Loading multiple zip files whole study", {
#   csvLocation <- file.path(tempdir(), "dataLocation")
#   dir.create(csvLocation)
#   fileDataPath <- list.files(here::here("results",
#                                         "MultipleMyeloma",
#                                         "zip"),
#                              pattern = "zip",
#                              full.names = TRUE)
#   uploadedFiles <- joinDatabases(fileDataPath = fileDataPath,
#                                 csvLocation = csvLocation)
#   expect_equal(length(uploadedFiles), 3)
#   expect_type(uploadedFiles, "list")
#   expect_true("PatientProfiles" %in% names(uploadedFiles))
#   unlink(csvLocation, recursive = TRUE)
# })

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
                                csvLocation = csvLocation,
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
                                csvLocation = csvLocation,
                                logger = logger)
  expect_equal(length(uploadedFiles), 4)
  expect_type(uploadedFiles, "list")
  unlink(csvLocation, recursive = TRUE)
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

test_that("iterates through fileNames", {
  configData <- yaml.load_file(system.file("config", "variablesConfig.yaml", package = "ReportGenerator"))
  fileDataPath <- list.files(testthat::test_path("studies", "zip"),
                             pattern = "zip",
                             full.names = TRUE)
  packagesNames <- names(configData)

  csvLocation <- tempdir()

  data <- list()

  folderNumber <- 0

  logger <- log4r::logger()
  # Unzips every zip and puts the files in a separate folder in the temp dir
  for (fileLocation in fileDataPath) {
    folderNumber <- folderNumber + 1
    unzip(zipfile = fileLocation,
          exdir = file.path(csvLocation, paste0("database", as.character(folderNumber))))
  }
  # List of unzipped database directories where files are located
  databaseFolders <- dir(csvLocation, pattern = "database", full.names = TRUE)
  for (filesList in databaseFolders) {
    # filesList <- databaseFolders[1]
    # List of unzipped database directories where files are located
    filesLocation <- list.files(filesList,
                                pattern = ".csv",
                                full.names = TRUE,
                                recursive = TRUE)
    metadata <- filesLocation[stringr::str_detect(filesLocation, "metadata")]
    if (!identical(metadata, character(0))) {
      databaseName <- readr::read_csv(metadata, show_col_types = FALSE) %>%
        pull(cdmSourceName) %>%
        unique()
    } else {
      databaseName <- "CDWBordeux"
    }
    expect_equal(databaseName, "Synthea synthetic health database")
    # Iterates every individual fileName
    for (fileName in filesLocation) {

      # fileName <- filesLocation[1]
      resultsData <- read_csv(fileName, show_col_types = FALSE)
      resultsColumns <- names(resultsData)
      # Checks the type of every individual fileName
      data <- loadFileData(data, fileName, configData, resultsData, resultsColumns, databaseName, logger = logger)

      }
  }
  expect_equal(length(data), 4)
  unlink(csvLocation, recursive = TRUE)
})

# test_that("iterates through fileNames", {
#   configData <- yaml.load_file(system.file("config", "variablesConfig.yaml", package = "ReportGenerator"))
#   fileDataPath <- list.files("D:/Users/cbarboza/Documents/darwin-docs/studyPackages/P2C1014PrescriptionsICU/results/zip", full.names = TRUE)
#   packagesNames <- names(configData)
#
#   csvLocation <- tempdir()
#
#   data <- list()
#
#   folderNumber <- 0
#
#   logger <- log4r::logger()
#   # Unzips every zip and puts the files in a separate folder in the temp dir
#   for (fileLocation in fileDataPath) {
#     folderNumber <- folderNumber + 1
#     unzip(zipfile = fileLocation,
#           exdir = file.path(csvLocation, paste0("database", as.character(folderNumber))))
#   }
#   # List of unzipped database directories where files are located
#   databaseFolders <- dir(csvLocation, pattern = "database", full.names = TRUE)
#   for (filesList in databaseFolders) {
#     # filesList <- databaseFolders[1]
#     # List of unzipped database directories where files are located
#     filesLocation <- list.files(filesList,
#                                 pattern = ".csv",
#                                 full.names = TRUE,
#                                 recursive = TRUE)
#     metadata <- filesLocation[stringr::str_detect(filesLocation, "metadata")]
#     if (!identical(metadata, character(0))) {
#       databaseName <- readr::read_csv(metadata, show_col_types = FALSE) %>%
#         pull(cdmSourceName) %>%
#         unique()
#     }
#
#     # Iterates every individual fileName
#     for (fileName in filesLocation) {
#
#       fileName <- filesLocation[3]
#       resultsData <- read_csv(fileName, show_col_types = FALSE)
#       resultsColumns <- names(resultsData)
#       # Checks the type of every individual fileName
#       print(fileName)
#       data <- loadFileData(data,
#                            fileName,
#                            configData,
#                            resultsData,
#                            resultsColumns,
#                            databaseName,
#                            logger = logger)
#
#     }
#   }
#   expect_equal(length(data), 4)
#   unlink(csvLocation, recursive = TRUE)
# })

# test_that("Objective 3", {
#   configData <- yaml.load_file(system.file("config", "variablesConfig.yaml", package = "ReportGenerator"))
#   fileDataPath <- list.files("D:/Users/cbarboza/Documents/darwin-docs/studyPackages/P2C1014PrescriptionsICU/results/objective_3", full.names = TRUE)
#   packagesNames <- names(configData)
#
#   csvLocation <- tempdir()
#
#   data <- list()
#
#   folderNumber <- 0
#
#   logger <- log4r::logger()
#   # Unzips every zip and puts the files in a separate folder in the temp dir
#   for (fileLocation in fileDataPath) {
#     folderNumber <- folderNumber + 1
#     unzip(zipfile = fileLocation,
#           exdir = file.path(csvLocation, paste0("database", as.character(folderNumber))))
#   }
#   # List of unzipped database directories where files are located
#   databaseFolders <- dir(csvLocation, pattern = "database", full.names = TRUE)
#   for (filesList in databaseFolders) {
#     # filesList <- databaseFolders[1]
#     # List of unzipped database directories where files are located
#     filesLocation <- list.files(filesList,
#                                 pattern = ".csv",
#                                 full.names = TRUE,
#                                 recursive = TRUE)
#     # metadata <- filesLocation[stringr::str_detect(filesLocation, "metadata")]
#     # if (!identical(metadata, character(0))) {
#     #   databaseName <- readr::read_csv(metadata, show_col_types = FALSE) %>%
#     #     pull(cdmSourceName) %>%
#     #     unique()
#     # }
#
#     # Iterates every individual fileName
#     for (fileName in filesLocation) {
#
#       # fileName <- filesLocation[3]
#       resultsData <- read_csv(fileName, show_col_types = FALSE)
#       resultsColumns <- names(resultsData)
#       # Checks the type of every individual fileName
#       print(fileName)
#       data <- loadFileData(data,
#                            fileName,
#                            configData,
#                            resultsData,
#                            resultsColumns,
#                            databaseName,
#                            logger = logger)
#
#     }
#   }
#   expect_equal(length(data), 4)
#   unlink(csvLocation, recursive = TRUE)
# })

# test_that("iterates through fileNames study polipharmacy", {
#
#   configData <- yaml.load_file(system.file("config", "variablesConfig.yaml", package = "ReportGenerator"))
#   packagesNames <- names(configData)
#   data <- list()
#   csvLocation <- file.path(tempdir(), "databaseTest")
#   fileLocation <- here::here("results", "results.zip")
#   checkmate::assert_file_exists(fileLocation)
#
#   # Unzips every zip and puts the files in a separate folder in the temp dir
#   unzip(zipfile = fileLocation,
#         exdir = csvLocation)
#   # List of unzipped database directories where files are located
#   filesLocation <- list.files(csvLocation,
#                               pattern = ".csv",
#                               full.names = TRUE,
#                               recursive = TRUE)
#
#   # Iterates every individual fileName
#   # for (fileName in filesLocation) {
#
#
#     fileName <- filesLocation[4]
#     resultsData <- read_csv(fileName, show_col_types = FALSE)
#     resultsColumns <- names(resultsData)
#     # Checks the type of every individual fileName
#     data <- loadFileData(data, fileName, configData, resultsData, resultsColumns, databaseName)
#
#
#     # }
#   expect_equal(length(data), 1)
#   unlink(csvLocation, recursive = TRUE)
# })

# test_that("ICU Prescriptions Objective 1 Data", {
#   csvLocation <- file.path(tempdir(), "dataLocation")
#   dir.create(csvLocation)
#   fileDataPath <- list.files("~/darwin-docs/studyPackages/P2C1014PrescriptionsICU/results",
#                              pattern = "zip",
#                              full.names = TRUE)
#   logger <- log4r::logger()
#   uploadedFiles <- joinDatabases(fileDataPath = fileDataPath[1],
#                                 csvLocation = csvLocation,
#                                 logger = logger)
#   expect_equal(length(uploadedFiles), 1)
#   expect_type(uploadedFiles, "list")
#   unlink(csvLocation, recursive = TRUE)
# })
