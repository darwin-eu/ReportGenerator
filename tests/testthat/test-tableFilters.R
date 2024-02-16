test_that("tableNumParfilter returns correct class", {
  csvLocation <- file.path(tempdir(), "dataLocation")
  dir.create(csvLocation)
  fileDataPath <- list.files(testthat::test_path("IncPrev",
                                                 "0.5.1",
                                                 "zip"),
                             pattern = "zip",
                             full.names = TRUE)
  uploadedFiles <- joinDatabase(fileDataPath = fileDataPath[1],
                                csvLocation = csvLocation)
  uploadedFiles$dataIP <- uploadedFiles$IncidencePrevalence
  responseHTML <- tableNumParFilters(uploadedFiles = uploadedFiles)
  expect_s3_class(responseHTML, "shiny.tag.list")
  unlink(csvLocation, recursive = TRUE)
})

test_that("tableAttInc returns correct class", {
  csvLocation <- file.path(tempdir(), "dataLocation")
  dir.create(csvLocation)
  fileDataPath <- list.files(testthat::test_path("IncPrev",
                                                 "0.5.1",
                                                 "zip"),
                             pattern = "zip",
                             full.names = TRUE)
  uploadedFiles <- joinDatabase(fileDataPath = fileDataPath[1],
                                csvLocation = csvLocation)
  uploadedFiles$dataIP <- uploadedFiles$IncidencePrevalence
  responseHTML <- ReportGenerator:::tableAttIncFilters(uploadedFiles = uploadedFiles)
  expect_s3_class(responseHTML, "shiny.tag.list")
  unlink(csvLocation, recursive = TRUE)
})

test_that("tableAttPrev returns correct class", {
  csvLocation <- file.path(tempdir(), "dataLocation")
  dir.create(csvLocation)
  fileDataPath <- list.files(testthat::test_path("IncPrev",
                                                 "0.5.1",
                                                 "zip"),
                             pattern = "zip",
                             full.names = TRUE)
  uploadedFiles <- joinDatabase(fileDataPath = fileDataPath[1],
                                csvLocation = csvLocation)
  uploadedFiles$dataIP <- uploadedFiles$IncidencePrevalence
  responseHTML <- tableAttPrevFilters(uploadedFiles = uploadedFiles)
  expect_s3_class(responseHTML, "shiny.tag.list")
  unlink(csvLocation, recursive = TRUE)
})

test_that("tableSexFilters returns correct class", {
  csvLocation <- file.path(tempdir(), "dataLocation")
  dir.create(csvLocation)
  fileDataPath <- list.files(testthat::test_path("IncPrev",
                                                 "0.5.1",
                                                 "zip"),
                             pattern = "zip",
                             full.names = TRUE)
  uploadedFiles <- joinDatabase(fileDataPath = fileDataPath[1],
                                csvLocation = csvLocation)
  uploadedFiles$dataIP <- uploadedFiles$IncidencePrevalence
  responseHTML <- tableSexFilters(uploadedFiles = uploadedFiles)
  expect_s3_class(responseHTML, "shiny.tag.list")
  unlink(csvLocation, recursive = TRUE)
})


