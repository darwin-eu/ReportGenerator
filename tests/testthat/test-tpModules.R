test_that("TP Data", {

  fileDataPath <- list.files(testthat::test_path("studies", "zip"),
                             pattern = "zip",
                             full.names = TRUE)
  tp_data <- joinDatabases(fileDataPath)

})
