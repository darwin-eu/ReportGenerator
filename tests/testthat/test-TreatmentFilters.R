test_that("SankeyDiagram filter returns correct class 2.5.2", {
  csvLocation <- file.path(tempdir(), "dataLocation")
  dir.create(csvLocation)
  fileDataPath <- list.files(testthat::test_path("TrePat",
                                                 "2.5.2",
                                                 "csv",
                                                 "CHUBX"),
                             pattern = "csv",
                             full.names = TRUE)
  fileDataPath <- fileDataPath[stringr::str_detect(fileDataPath, "treatmentPathways")]
  fileName <- "treatmentPathways"
  uploadedFiles <- list()
  uploadedFiles$dataTP <- joinDatabase(fileDataPath = fileDataPath,
                                       fileName = fileName,
                                       csvLocation = csvLocation)$TreatmentPatterns
  responseHTML <- sankeyDiagramFilters(uploadedFiles = uploadedFiles)
  expect_s3_class(responseHTML, "shiny.tag.list")
  unlink(csvLocation, recursive = TRUE)
})

test_that("SankeyDiagram filter returns correct class 2.5.0", {
  csvLocation <- file.path(tempdir(), "dataLocation")
  dir.create(csvLocation)
  fileDataPath <- list.files(testthat::test_path("TrePat",
                                                 "2.5.2",
                                                 "csv",
                                                 "CHUBX"),
                             pattern = "csv",
                             full.names = TRUE)
  fileDataPath <- fileDataPath[stringr::str_detect(fileDataPath, "treatmentPathways")]
  fileName <- "treatmentPathways"
  uploadedFiles <- list()
  uploadedFiles$dataTP <- joinDatabase(fileDataPath = fileDataPath,
                                       fileName = fileName,
                                       csvLocation = csvLocation)$TreatmentPatterns
  uploadedFiles <- uploadedFiles$dataTP
  uploadedFiles$treatmentPathways <- mutate(uploadedFiles$treatmentPathway, index_year = indexYear)
  responseHTML <- sankeyDiagramFilters(uploadedFiles = uploadedFiles,
                                       version = "2.5.0")
  expect_s3_class(responseHTML, "shiny.tag.list")
  unlink(csvLocation, recursive = TRUE)
})

test_that("sunburstDiagram filter returns correct class 2.5.2", {
  csvLocation <- file.path(tempdir(), "dataLocation")
  dir.create(csvLocation)
  fileDataPath <- list.files(testthat::test_path("TrePat",
                                                 "2.5.2",
                                                 "csv",
                                                 "CHUBX"),
                             pattern = "csv",
                             full.names = TRUE)
  fileDataPath <- fileDataPath[stringr::str_detect(fileDataPath, "treatmentPathways")]
  fileName <- "treatmentPathways"
  uploadedFiles <- list()
  uploadedFiles$dataTP <- joinDatabase(fileDataPath = fileDataPath,
                                       fileName = fileName,
                                       csvLocation = csvLocation)$TreatmentPatterns
  responseHTML <- sunburstDiagramFilters(uploadedFiles = uploadedFiles)
  expect_s3_class(responseHTML, "shiny.tag.list")
  unlink(csvLocation, recursive = TRUE)
})

test_that("sunburstDiagram filter returns correct class 2.5.0", {
  csvLocation <- file.path(tempdir(), "dataLocation")
  dir.create(csvLocation)
  fileDataPath <- list.files(testthat::test_path("TrePat",
                                                 "2.5.2",
                                                 "csv",
                                                 "CHUBX"),
                             pattern = "csv",
                             full.names = TRUE)
  fileDataPath <- fileDataPath[stringr::str_detect(fileDataPath, "treatmentPathways")]
  fileName <- "treatmentPathways"
  uploadedFiles <- list()
  uploadedFiles$dataTP <- joinDatabase(fileDataPath = fileDataPath,
                                       fileName = fileName,
                                       csvLocation = csvLocation)$TreatmentPatterns
  uploadedFiles <- uploadedFiles$dataTP
  uploadedFiles$treatmentPathways <- mutate(uploadedFiles$treatmentPathway, index_year = indexYear)
  responseHTML <- sunburstDiagramFilters(uploadedFiles = uploadedFiles)
  expect_s3_class(responseHTML, "shiny.tag.list")
  unlink(csvLocation, recursive = TRUE)
})
