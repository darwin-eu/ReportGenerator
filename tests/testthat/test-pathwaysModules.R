test_that("TP Data", {

  fileDataPath <- list.files(test_path("studies", "summarised_zip"),
                             pattern = "zip",
                             full.names = TRUE)
  tp_data <- joinDatabases(fileDataPath)

})
