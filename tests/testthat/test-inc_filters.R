test_that("IncYear filter returns correct class", {
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
  responseHTML <- incPlotByYearFilters(uploadedFiles = uploadedFiles,
                                       objectChoice = "Plot - Incidence rate per year by sex")
  expect_s3_class(responseHTML, "shiny.tag.list")
  unlink(csvLocation, recursive = TRUE)
})

test_that("IncSex filter returns correct class", {
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
  responseHTML <- incPlotSexFilters(uploadedFiles = uploadedFiles,
                                       objectChoice = "Plot - Incidence rate per year by sex")
  expect_s3_class(responseHTML, "shiny.tag.list")
  unlink(csvLocation, recursive = TRUE)
})

test_that("IncAge filter returns correct class", {
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
  responseHTML <- incPlotAgeFilters(uploadedFiles = uploadedFiles,
                                    objectChoice = "Plot - Incidence rate per year by sex")
  expect_s3_class(responseHTML, "shiny.tag.list")
  unlink(csvLocation, recursive = TRUE)
})
