test_that("mock data works", {
  outputDir <- file.path(tempdir(), "dataLocation")
  dir.create(outputDir)
  generateMockData(databaseName = c("CHUBX"),
                   simulatePopulation = FALSE,
                   outputDir = outputDir)
  expect_equal(list.files(file.path(outputDir, "results", "0.5.1")),"mock_data_ReportGenerator_CHUBX.zip")
  unlink(outputDir, recursive = TRUE)
})
