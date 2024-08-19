test_that("summarised Characteristics and LSC", {
  testServer(reportGenerator(), {
    uploadedFiles <- list()
    uploadedFiles[["CohortCharacteristics"]][["summarised_characteristics"]] <- testData$summarised_characteristics
    uploadedFiles[["CohortCharacteristics"]][["summarised_large_scale_characteristics"]] <- testData$summarised_large_scale_characteristics

    expect_s3_class(characteristicsUI("characteristics",
                                      uploadedFiles), "shiny.tag.list")
    expect_s3_class(characteristicsServer("characteristics",
                                          uploadedFiles), "reactiveVal")
    expect_s3_class(characteristicsUI("lsc",
                                      uploadedFiles), "shiny.tag.list")
    expect_s3_class(characteristicsServer("lsc",
                                          uploadedFiles), "reactiveVal")
  })
})


test_that("summarised Characteristics and LSC", {
  csvLocation <- file.path(tempdir(), "dataLocation")
  dir.create(csvLocation)
  fileDataPath <- list.files(testthat::test_path("studies", "zip"),
                             pattern = "zip",
                             full.names = TRUE)
  logger <- log4r::logger()
  unzipDir <- file.path(tempdir(), "lsc")
  uploadedFiles <- joinDatabases(fileDataPath = fileDataPath[1],
                                 unzipDir = unzipDir,
                                 logger = logger)
  testServer(reportGenerator(), {
    expect_s3_class(characteristicsUI("characteristics", uploadedFiles$CohortCharacteristics$summarised_characteristics), "shiny.tag.list")
    expect_s3_class(characteristicsServer("characteristics", uploadedFiles$CohortCharacteristics$summarised_characteristics), "reactiveVal")
  })
})

test_that("settings for LSC filter", {
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

  testServer(reportGenerator(), {
    expect_s3_class(characteristicsUI("characteristics", data$CohortCharacteristics$summarised_characteristics), "shiny.tag.list")
    expect_s3_class(characteristicsServer("characteristics", data$CohortCharacteristics$summarised_characteristics), "reactiveVal")
  })
  unlink(unzipDir, recursive = TRUE)
})
