test_that("summarised Characteristics and LSC", {
  testServer(reportGenerator(), {
    summarised_characteristics_data <- testData$CohortCharacteristics$summarised_characteristics
    summarised_large_scale_characteristics_data <- testData$CohortCharacteristics$summarised_large_scale_characteristics

    expect_s3_class(characteristicsUI("characteristics",
                                      uploadedFiles = summarised_characteristics_data), "shiny.tag.list")
    expect_s3_class(characteristicsServer("characteristics",
                                          uploadedFiles = summarised_large_scale_characteristics_data), "reactiveVal")
    expect_s3_class(characteristicsUI("lsc",
                                      uploadedFiles = summarised_characteristics_data), "shiny.tag.list")
    expect_s3_class(characteristicsServer("lsc",
                                          uploadedFiles = summarised_large_scale_characteristics_data), "reactiveVal")
  })
})


test_that("summarised Characteristics and LSC", {
  csvLocation <- file.path(tempdir(), "dataLocation")
  dir.create(csvLocation)
  fileDataPath <- list.files(test_path("studies", "summarised_zip"),
                             pattern = "zip",
                             full.names = TRUE)
  logger <- log4r::logger()
  unzipDir <- file.path(tempdir(), "lsc")
  uploadedFiles <- joinDatabases(fileDataPath = fileDataPath[1])
  testServer(reportGenerator(), {
    expect_s3_class(characteristicsUI("characteristics", uploadedFiles), "shiny.tag.list")
    expect_s3_class(characteristicsServer("characteristics", uploadedFiles), "reactiveVal")
  })
})

# test_that("settings for LSC filter", {
#   data <- list()
#   filesLocation <- testthat::test_path("studies", "misc", "ls_chr_results.csv")
#   configData <- yaml.load_file(system.file("config",
#                                            "variablesConfig.yaml",
#                                            package = "ReportGenerator"))
#   resultsData <- read_csv(filesLocation, show_col_types = FALSE, col_types = c(.default = "c"))
#   resultsColumns <- names(resultsData)
#
#   data <- loadFileData(data,
#                        filesLocation,
#                        configData,
#                        resultsData,
#                        resultsColumns,
#                        databaseName)
#
#   settingsLSC <- settings(data$CohortCharacteristics$summarised_large_scale_characteristics)
#
#   settingsLSC %>% select(result_id, )
#
#   settings(data$CohortCharacteristics$summarised_characteristics)
#
#
#   fileDataPath <- list.files(test_path("studies", "summarised_zip"),
#                              pattern = "zip",
#                              full.names = TRUE)
#   logger <- log4r::logger()
#   uploadedFiles <- joinDatabases(fileDataPath = fileDataPath[1],
#                                         logger = logger)
#
#
#
#   csvLocation <- file.path(tempdir(), "dataLocation")
#   dir.create(csvLocation)
#   fileDataPath <- list.files(test_path("studies", "summarised_zip"),
#                              pattern = "zip",
#                              full.names = TRUE)
#   logger <- log4r::logger()
#   uploadedFiles <- joinDatabases(fileDataPath = fileDataPath[1],
#                                  logger = logger)
#   testServer(reportGenerator(), {
#     expect_s3_class(characteristicsUI("characteristics", uploadedFiles), "shiny.tag.list")
#     expect_s3_class(characteristicsServer("characteristics", uploadedFiles), "reactiveVal")
#   })
# })
