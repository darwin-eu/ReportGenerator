test_that("TP Data", {

  fileDataPath <- list.files(test_path("studies", "summarised_zip"),
                             pattern = "zip",
                             full.names = TRUE)
  tp_data <- joinDatabases(fileDataPath)

  pathwaysData <- tp_data$other_result$TreatmentPatterns$treatmentPathways

  pathwaysData$freq <- as.numeric(pathwaysData$freq)

  TreatmentPatterns::ggSunburst(treatmentPathways = pathwaysData,
                                groupCombinations = FALSE,
                                unit = "percent")

})
