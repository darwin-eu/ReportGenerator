# test_that("mock data works", {
#   outputDir <- file.path(tempdir(), "dataLocation")
#   dir.create(outputDir)
#   ReportGenerator::generateMockData(databaseName = c("CHUBX"),
#                    simulatePopulation = FALSE,
#                    outputDir = outputDir)
#   expect_equal(list.files(file.path(outputDir, "0.6.0")),"mock_data_ReportGenerator_CHUBX.zip")
#   unlink(outputDir, recursive = TRUE)
# })
