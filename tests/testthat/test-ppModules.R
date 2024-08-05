test_that("summarised Characteristics and LSC", {
  testServer(reportGenerator(), {
    expect_s3_class(characteristicsUI("characteristics",
                                      testData$summarised_characteristics), "shiny.tag.list")
    expect_s3_class(characteristicsServer("characteristics",
                                          testData$summarised_characteristics), "reactiveVal")
    expect_s3_class(characteristicsUI("lsc",
                                      testData$summarised_large_scale_characteristics), "shiny.tag.list")
    expect_s3_class(characteristicsServer("lsc",
                                          testData$summarised_large_scale_characteristics), "reactiveVal")
  })
})


test_that("summarised Characteristics and LSC", {
  csvLocation <- file.path(tempdir(), "dataLocation")
  dir.create(csvLocation)
  fileDataPath <- list.files(testthat::test_path("studies", "zip"),
                             pattern = "zip",
                             full.names = TRUE)
  logger <- log4r::logger()
  uploadedFiles <- joinDatabases(fileDataPath = fileDataPath[1],
                                logger = logger)
  testServer(reportGenerator(), {
    expect_s3_class(characteristicsUI("characteristics", uploadedFiles$PatientProfiles$summarised_characteristics), "shiny.tag.list")
    expect_s3_class(characteristicsServer("characteristics", uploadedFiles$PatientProfiles$summarised_characteristics), "reactiveVal")
  })
})

test_that("settings for LSC filter", {

  data <- list()
  filesLocation <- testthat::test_path("studies", "misc", "ls_chr_results.csv")
  # filesLocation <- "C:/Users/cbarboza/Downloads/cdm_gold_202401_ild_output/cdm_gold_202401_ild_output/survival_results.csv"
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

  settingsLSC <- settings(data$CohortCharacteristics$summarised_large_scale_characteristics)

  unique(uploadedFiles$dataPP$summarised_large_scale_characteristics$result_id)

  settingsLSC %>% select(result_id, )



  settings(data$CohortCharacteristics$summarised_characteristics)


  fileDataPath <- list.files(testthat::test_path("studies", "zip"),
                             pattern = "zip",
                             full.names = TRUE)
  logger <- log4r::logger()
  uploadedFileDataList <- joinDatabases(fileDataPath = fileDataPath[1],
                                        logger = logger)



  csvLocation <- file.path(tempdir(), "dataLocation")
  dir.create(csvLocation)
  fileDataPath <- list.files(testthat::test_path("studies", "zip"),
                             pattern = "zip",
                             full.names = TRUE)
  logger <- log4r::logger()
  uploadedFiles <- joinDatabases(fileDataPath = fileDataPath[1],
                                 logger = logger)
  testServer(reportGenerator(), {
    expect_s3_class(characteristicsUI("characteristics", uploadedFiles$PatientProfiles$summarised_characteristics), "shiny.tag.list")
    expect_s3_class(characteristicsServer("characteristics", uploadedFiles$PatientProfiles$summarised_characteristics), "reactiveVal")
  })
})
