test_that("summarised Characteristics and LSC", {
  testServer(reportGenerator(), {
    summarise_characteristics_data <- testData$CohortCharacteristics$summarise_characteristics
    summarise_large_scale_characteristics_data <- testData$CohortCharacteristics$summarise_large_scale_characteristics

    expect_s3_class(characteristicsUI("characteristics",
                                      uploadedFiles = summarise_characteristics_data), "shiny.tag.list")
    expect_s3_class(characteristicsServer("characteristics",
                                          uploadedFiles = summarise_large_scale_characteristics_data), "reactiveVal")
    expect_s3_class(characteristicsUI("lsc",
                                      uploadedFiles = summarise_characteristics_data), "shiny.tag.list")
    expect_s3_class(characteristicsServer("lsc",
                                          uploadedFiles = summarise_large_scale_characteristics_data), "reactiveVal")
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
