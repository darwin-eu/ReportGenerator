test_that("test basic", {

  testData <- testData()
  expect_equal(names(testData), c("incidence_attrition",
                                  "incidence_estimates",
                                  "prevalence_attrition",
                                  "prevalence_estimates",
                                  "treatmentPathways_test"))
})

test_that("VariablesConfig IncidencePrevalence", {
  fileDataPath <- list.files(testthat::test_path("IncPrev",
                                                 "0.5.1",
                                                 "zip"),
                             pattern = "zip",
                             full.names = TRUE)
  variablesConfigYaml(fileDataPath = fileDataPath[1],
                      package = "IncidencePrevalence",
                      version = "0.5.1")
  package <- "IncidencePrevalence"
  version <- "0.5.1"
  configData <- yaml.load_file(system.file("config", "variablesConfig.yaml", package = "ReportGenerator"))
  expect_equal(length(configData[[package]][[version]]), 4)
  expect_type(configData[[package]][[version]], "list")
})

test_that("VariablesConfig IncidencePrevalence", {
  fileDataPath <- list.files(testthat::test_path("IncPrev",
                                                 "0.6.0",
                                                 "zip"),
                             pattern = "zip",
                             full.names = TRUE)
  variablesConfigYaml(fileDataPath = fileDataPath[1],
                      package = "IncidencePrevalence",
                      version = "0.6.0")
  package <- "IncidencePrevalence"
  version <- "0.6.0"
  configData <- yaml.load_file(system.file("config", "variablesConfig.yaml", package = "ReportGenerator"))
  expect_equal(length(configData[[package]][[version]]), 4)
  expect_type(configData[[package]][[version]], "list")
})

test_that("VariablesConfig TreatmentPatterns", {
  fileDataPath <- list.files(testthat::test_path("TrePat", "csv"),
                             pattern = "csv",
                             full.names = TRUE)
  variablesConfigYaml(fileDataPath = fileDataPath,
                      package = "TreatmentPatterns",
                      version = "2.5.0")
  package <- "TreatmentPatterns"
  version <- "2.5.0"
  configData <- yaml.load_file(system.file("config", "variablesConfig.yaml", package = "ReportGenerator"))
  expect_equal(length(configData[[package]][[version]]), 1)
  expect_type(configData[[package]][[version]], "list")
})

test_that("Loading 1 zip files IncidencePrevalence", {
  csvLocation <- file.path(tempdir(), "dataLocation")
  dir.create(csvLocation)
  fileDataPath <- list.files(testthat::test_path("IncPrev",
                                                 "0.5.1",
                                                 "zip"),
                             pattern = "zip",
                             full.names = TRUE)
  uploadedFiles <- joinDatabase(fileDataPath = fileDataPath[1],
                                package = "IncidencePrevalence",
                                versionData = "0.5.1",
                                csvLocation = csvLocation)
  expect_equal(length(uploadedFiles), 4)
  expect_type(uploadedFiles, "list")
  unlink(csvLocation, recursive = TRUE)
})

test_that("Loading multiple zip files IncidencePrevalence", {
  csvLocation <- file.path(tempdir(), "dataLocation")
  dir.create(csvLocation)
  fileDataPath <- list.files(testthat::test_path("IncPrev",
                                                 "0.5.1",
                                                 "zip"),
                             pattern = "zip",
                             full.names = TRUE)
  uploadedFiles <- joinDatabase(fileDataPath = fileDataPath,
                                package = "IncidencePrevalence",
                                versionData = "0.5.1",
                                csvLocation = csvLocation)
  expect_equal(length(uploadedFiles), 4)
  expect_type(uploadedFiles, "list")
  unlink(csvLocation, recursive = TRUE)
})

test_that("Loading 1 csv files IncidencePrevalence", {
  csvLocation <- file.path(tempdir(), "dataLocation")
  dir.create(csvLocation)
  fileDataPath <- list.files(testthat::test_path("IncPrev",
                                                 "0.5.1",
                                                 "csv"),
                             pattern = "csv",
                             full.names = TRUE)
  fileName <- list.files(testthat::test_path("IncPrev",
                                             "0.5.1",
                                             "csv"),
                         pattern = "csv")
  fileName <- tools::file_path_sans_ext(fileName)
  uploadedFiles <- joinDatabase(fileDataPath = fileDataPath[3],
                                fileName = fileName[3],
                                package = "IncidencePrevalence",
                                versionData = "0.5.1",
                                csvLocation = csvLocation)
  expect_equal(length(uploadedFiles), 1)
  expect_type(uploadedFiles, "list")
  unlink(csvLocation, recursive = TRUE)
})

# test_that("Loading 1 csv files extra column check IncidencePrevalence", {
#   csvFiles <- list.files(testthat::test_path("IncPrev",
#                                              "extras"),
#                          pattern = "csv",
#                          full.names = TRUE)
#   csvFiles <- csvFiles[1]
#   fileName <- list.files(testthat::test_path("IncPrev",
#                                              "extras"),
#                          pattern = "csv")
#   fileName <- tools::file_path_sans_ext(fileName)
#   fileName <- fileName[1]
#   package <- "IncidencePrevalence"
#   versionData <- "0.5.1"
#   configData <- yaml.load_file(system.file("config", "variablesConfig.yaml", package = "ReportGenerator"))
#   configData <- configData[[package]][[versionData]]
#   configDataTypes <- names(configData)
#   uploadedFiles <- columnCheck(csvFiles = csvFiles,
#                                fileName = fileName,
#                                configData = configData,
#                                configDataTypes = configDataTypes)
#   expect_equal(length(uploadedFiles), 1)
#   expect_type(uploadedFiles, "list")
# })

test_that("Loading multiple csv files IncidencePrevalence", {
  csvLocation <- file.path(tempdir(), "dataLocation")
  dir.create(csvLocation)
  fileDataPath <- list.files(testthat::test_path("IncPrev",
                                                 "0.5.1",
                                                 "csv"),
                             pattern = "csv",
                             full.names = TRUE)
  fileName <- list.files(testthat::test_path("IncPrev",
                                             "0.5.1",
                                             "csv"),
                         pattern = "csv")
  fileName <- tools::file_path_sans_ext(fileName)
  uploadedFiles <- joinDatabase(fileDataPath = fileDataPath,
                                fileName = fileName,
                                package = "IncidencePrevalence",
                                versionData = "0.5.1",
                                csvLocation = csvLocation)
  expect_equal(length(uploadedFiles), 4)
  expect_type(uploadedFiles, "list")
  unlink(csvLocation, recursive = TRUE)
})

test_that("Loading 1 csv files TreatmentPatterns", {
  csvLocation <- file.path(tempdir(), "dataLocation")
  dir.create(csvLocation)
  fileDataPath <- list.files(testthat::test_path("TrePat",
                                                 "2.5.2",
                                                 "csv",
                                                 "CHUBX"),
                             pattern = "csv",
                             full.names = TRUE)
  fileDataPath <- fileDataPath[stringr::str_detect(fileDataPath, "treatmentPathways")]
  package <- "TreatmentPatterns"
  versionData <- "2.5.2"
  fileName <- "treatmentPathways"
  uploadedFiles <- joinDatabase(fileDataPath = fileDataPath,
                                fileName = fileName,
                                package = package,
                                versionData = versionData,
                                csvLocation = csvLocation)
  expect_equal(length(uploadedFiles), 1)
  expect_equal(length(unique(uploadedFiles$treatmentPathways$cdm_name)), 1)
  expect_type(uploadedFiles, "list")
  unlink(csvLocation, recursive = TRUE)
})

test_that("Loading multiple csv files TreatmentPatterns", {
  csvLocation <- file.path(tempdir(), "dataLocation")
  dir.create(csvLocation)
  fileDataPath <- list.files(testthat::test_path("TrePat",
                                                 "2.5.2",
                                                 "csv"),
                             pattern = "treatmentPathways.csv",
                             full.names = TRUE,
                             recursive = TRUE)
  package <- "TreatmentPatterns"
  versionData <- "2.5.2"
  fileName <- list.files(testthat::test_path("TrePat",
                                             "2.5.2",
                                             "csv"),
                         pattern = "treatmentPathways.csv",
                         recursive = TRUE)
  fileName <- tools::file_path_sans_ext(fileName)
  uploadedFiles <- joinDatabase(fileDataPath = fileDataPath,
                                fileName = fileName,
                                package = package,
                                versionData = versionData,
                                csvLocation = csvLocation)
  expect_equal(length(uploadedFiles), 1)
  expect_equal(length(unique(uploadedFiles$treatmentPathways$cdm_name)), 3)
  expect_type(uploadedFiles, "list")
  unlink(csvLocation, recursive = TRUE)
})

test_that("Loading 1 zip files TreatmentPatterns", {
  csvLocation <- file.path(tempdir(), "dataLocation")
  dir.create(csvLocation)
  fileDataPath <- list.files(testthat::test_path("TrePat",
                                                 "2.5.2",
                                                 "zip"),
                             pattern = "zip",
                             full.names = TRUE)
  package <- "TreatmentPatterns"
  versionData <- "2.5.2"
  uploadedFiles <- joinDatabase(fileDataPath = fileDataPath[1],
                                package = package,
                                versionData = versionData,
                                csvLocation = csvLocation)
  expect_equal(length(uploadedFiles), 1)
  expect_equal(length(unique(uploadedFiles$treatmentPathways$cdm_name)), 1)
  expect_type(uploadedFiles, "list")
  unlink(csvLocation, recursive = TRUE)
})

test_that("Loading multiple zip files TreatmentPatterns", {
  csvLocation <- file.path(tempdir(), "dataLocation")
  dir.create(csvLocation)
  fileDataPath <- list.files(testthat::test_path("TrePat",
                                                 "2.5.2",
                                                 "zip"),
                             pattern = "zip",
                             full.names = TRUE)
  package <- "TreatmentPatterns"
  versionData <- "2.5.2"
  uploadedFiles <- joinDatabase(fileDataPath = fileDataPath,
                                package = package,
                                versionData = versionData,
                                csvLocation = csvLocation)
  expect_equal(length(uploadedFiles), 1)
  expect_type(uploadedFiles, "list")
  expect_equal(length(unique(uploadedFiles$treatmentPathways$cdm_name)), 3)
  unlink(csvLocation, recursive = TRUE)
})

test_that("data clean incidence attrition type", {
  incidence_attrition <- dataCleanAttrition(incidence_attrition = incidence_attrition_test)
  expect_type(incidence_attrition, "list")
})

test_that("data clean incidence attrition type no reason_id", {
  incidence_attrition <- incidence_attrition_test %>% select(-reason_id) %>%
    mutate(current_n = number_subjects,
           excluded = excluded_subjects)
  incidence_attrition <- dataCleanAttrition(incidence_attrition = incidence_attrition)
  expect_type(incidence_attrition, "list")
})

test_that("data clean prevalence attrition type", {
  prevalence_attrition <- dataCleanAttrition(prevalence_attrition = prevalence_attrition_test)
  expect_type(prevalence_attrition, "list")
})

test_that("data clean prevalence attrition type no reason_id", {
  prevalence_attrition <- prevalence_attrition_test %>% select(-reason_id) %>%
    mutate(current_n = number_subjects,
           excluded = excluded_subjects)
  prevalence_attrition <- dataCleanAttrition(prevalence_attrition = prevalence_attrition)
  expect_type(prevalence_attrition, "list")
})

# PatientProfiles

test_that("Loading 1 zip files PatientProfiles", {
  csvLocation <- file.path(tempdir(), "dataLocation")
  dir.create(csvLocation)
  fileDataPath <- list.files(testthat::test_path("PP",
                                                 "0.5.1",
                                                 "zip"),
                             pattern = "zip",
                             full.names = TRUE)
  uploadedFiles <- joinDatabase(fileDataPath = fileDataPath[1],
                                package = "PatientProfiles",
                                versionData = "0.5.1",
                                csvLocation = csvLocation)
  expect_equal(length(uploadedFiles), 2)
  expect_type(uploadedFiles, "list")
  unlink(csvLocation, recursive = TRUE)
})

test_that("Loading multiple zip files PatientProfiles", {
  csvLocation <- file.path(tempdir(), "dataLocation")
  dir.create(csvLocation)
  fileDataPath <- list.files(testthat::test_path("PP",
                                                 "0.5.1",
                                                 "zip"),
                             pattern = "zip",
                             full.names = TRUE)
  uploadedFiles <- joinDatabase(fileDataPath = fileDataPath,
                                package = "PatientProfiles",
                                versionData = "0.5.1",
                                csvLocation = csvLocation)
  expect_equal(length(uploadedFiles), 2)
  expect_type(uploadedFiles, "list")
  unlink(csvLocation, recursive = TRUE)
})

test_that("Loading 1 csv files PatientProfiles", {
  # Characteristics
  csvLocation <- file.path(tempdir(), "dataLocation")
  dir.create(csvLocation)
  fileDataPath <- list.files(testthat::test_path("PP",
                                                 "0.5.1",
                                                 "csv"),
                             pattern = "csv",
                             full.names = TRUE)
  fileName <- list.files(testthat::test_path("PP",
                                             "0.5.1",
                                             "csv"),
                         pattern = "csv")
  fileName <- tools::file_path_sans_ext(fileName)
  uploadedFiles <- joinDatabase(fileDataPath = fileDataPath[2],
                                fileName = fileName[2],
                                package = "PatientProfiles",
                                versionData = "0.5.1",
                                csvLocation = csvLocation)
  expect_equal(length(uploadedFiles), 1)
  expect_type(uploadedFiles, "list")
  unlink(csvLocation, recursive = TRUE)
  # LSC
  csvLocation <- file.path(tempdir(), "dataLocation")
  dir.create(csvLocation)
  fileDataPath <- list.files(testthat::test_path("PP",
                                                 "0.5.1",
                                                 "csv"),
                             pattern = "csv",
                             full.names = TRUE)
  fileName <- list.files(testthat::test_path("PP",
                                             "0.5.1",
                                             "csv"),
                         pattern = "csv")
  fileName <- tools::file_path_sans_ext(fileName)
  uploadedFiles <- joinDatabase(fileDataPath = fileDataPath[4],
                                fileName = fileName[4],
                                package = "PatientProfiles",
                                versionData = "0.5.1",
                                csvLocation = csvLocation)
  expect_equal(length(uploadedFiles), 1)
  expect_type(uploadedFiles, "list")
  unlink(csvLocation, recursive = TRUE)
})

# test_that("Loading 1 csv files extra column check PatientProfiles", {
#   csvFiles <- list.files(testthat::test_path("IncPrev",
#                                              "extras"),
#                          pattern = "csv",
#                          full.names = TRUE)
#   csvFiles <- csvFiles[1]
#   fileName <- list.files(testthat::test_path("IncPrev",
#                                              "extras"),
#                          pattern = "csv")
#   fileName <- tools::file_path_sans_ext(fileName)
#   fileName <- fileName[1]
#   package <- "PatientProfiles"
#   versionData <- "0.5.1"
#   configData <- yaml.load_file(system.file("config", "variablesConfig.yaml", package = "ReportGenerator"))
#   configData <- configData[[package]][[versionData]]
#   configDataTypes <- names(configData)
#   uploadedFiles <- columnCheck(csvFiles = csvFiles,
#                                fileName = fileName,
#                                configData = configData,
#                                configDataTypes = configDataTypes)
#   expect_equal(length(uploadedFiles), 1)
#   expect_type(uploadedFiles, "list")
# })

test_that("Loading multiple csv files PatientProfiles", {
  csvLocation <- file.path(tempdir(), "dataLocation")
  dir.create(csvLocation)
  fileDataPath <- list.files(testthat::test_path("PP",
                                                 "0.5.1",
                                                 "csv"),
                             pattern = "csv",
                             full.names = TRUE)
  fileName <- list.files(testthat::test_path("PP",
                                             "0.5.1",
                                             "csv"),
                         pattern = "csv")
  fileName <- tools::file_path_sans_ext(fileName)
  uploadedFiles <- joinDatabase(fileDataPath = fileDataPath,
                                fileName = fileName,
                                package = "PatientProfiles",
                                versionData = "0.5.1",
                                csvLocation = csvLocation)
  expect_equal(length(uploadedFiles), 2)
  expect_type(uploadedFiles, "list")
  unlink(csvLocation, recursive = TRUE)
})
