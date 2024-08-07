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
  unzipDir <- file.path(tempdir(), "lsc")
  uploadedFiles <- joinDatabases(fileDataPath = fileDataPath[1],
                                 unzipDir = unzipDir,
                                 logger = logger)
  testServer(reportGenerator(), {
    expect_s3_class(characteristicsUI("characteristics", uploadedFiles$PatientProfiles$summarised_characteristics), "shiny.tag.list")
    expect_s3_class(characteristicsServer("characteristics", uploadedFiles$PatientProfiles$summarised_characteristics), "reactiveVal")
  })
  unlink(unzipDir, recursive = TRUE)
})
