# Tests waiting for latest release that solves update of dbplyr

# test_that("generateMockData result output", {
#   outputPath <- file.path(tempdir(), "dataLocation")
#   dir.create(outputPath)
#   databaseName <- c("CHUBX")
#   simulatePopulation <- TRUE
#   internal <- FALSE
#   generateMockData(databaseName = databaseName,
#                    simulatePopulation = simulatePopulation,
#                    outputPath = outputPath,
#                    internal = internal)
#   expect_equal(list.files(file.path(outputPath)),"mock_data_CHUBX.zip")
#   unlink(outputPath, recursive = TRUE)
# })

# test_that("generateMockData result output", {
#   outputPath <- file.path(tempdir(), "dataLocation")
#   dir.create(outputPath)
#   generateMockData(databaseName = c("CHUBX", "CPRD GOLD", "IMASIS"),
#                    simulatePopulation = TRUE,
#                    outputPath = outputPath,
#                    internal = FALSE)
#   expect_equal(list.files(file.path(outputPath)), c("mock_data_CHUBX.zip",
#                                                     "mock_data_CPRD GOLD.zip",
#                                                     "mock_data_IMASIS.zip"))
#   unlink(outputPath, recursive = TRUE)
# })

