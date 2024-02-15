test_that("PrevYear filter returns correct class", {
  csvLocation <- file.path(tempdir(), "dataLocation")
  dir.create(csvLocation)
  fileDataPath <- list.files(testthat::test_path("IP",
                                                 "0.5.1",
                                                 "zip"),
                             pattern = "zip",
                             full.names = TRUE)
  uploadedFiles <- list()
  uploadedFiles$dataIP <- joinDatabase(fileDataPath = fileDataPath[1],
                                       package = "IncidencePrevalence",
                                       versionData = "0.5.1",
                                       csvLocation = csvLocation)
  responseHTML <- prevPlotByYearFilters(uploadedFiles = uploadedFiles,
                                       objectChoice = "Plot - Prevalence rate per year by sex")
  expect_s3_class(responseHTML, "shiny.tag.list")
  unlink(csvLocation, recursive = TRUE)
})

test_that("PrevSex filter returns correct class", {
  csvLocation <- file.path(tempdir(), "dataLocation")
  dir.create(csvLocation)
  fileDataPath <- list.files(testthat::test_path("IP",
                                                 "0.5.1",
                                                 "zip"),
                             pattern = "zip",
                             full.names = TRUE)
  uploadedFiles <- list()
  uploadedFiles$dataIP <- joinDatabase(fileDataPath = fileDataPath[1],
                                       package = "IncidencePrevalence",
                                       versionData = "0.5.1",
                                       csvLocation = csvLocation)
  responseHTML <- prevPlotSexFilters(uploadedFiles = uploadedFiles,
                                    objectChoice = "Plot - Incidence rate per year by sex")
  expect_s3_class(responseHTML, "shiny.tag.list")
  unlink(csvLocation, recursive = TRUE)
})

test_that("PrevAge filter returns correct class", {
  csvLocation <- file.path(tempdir(), "dataLocation")
  dir.create(csvLocation)
  fileDataPath <- list.files(testthat::test_path("IP",
                                                 "0.5.1",
                                                 "zip"),
                             pattern = "zip",
                             full.names = TRUE)
  uploadedFiles <- list()
  uploadedFiles$dataIP <- joinDatabase(fileDataPath = fileDataPath[1],
                                       package = "IncidencePrevalence",
                                       versionData = "0.5.1",
                                       csvLocation = csvLocation)
  responseHTML <- prevPlotAgeFilters(uploadedFiles = uploadedFiles,
                                    objectChoice = "Plot - Prevalence rate per year by sex")
  expect_s3_class(responseHTML, "shiny.tag.list")
  unlink(csvLocation, recursive = TRUE)
})
