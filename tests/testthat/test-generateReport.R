test_that("Generate Report", {
  reportDocx <- read_docx(path = system.file("templates",
                                             "word",
                                             "DARWIN_EU_Study_Report.docx",
                                             package = "ReportGenerator"))
  reportItemsPath <- testthat::test_path("studies", "reportItems.rds")
  reportItems <- read_rds(reportItemsPath)
  dataReportList <- reportItems$reportItems
  testdir <- file.path(tempdir(), "reportItems")
  dir.create(testdir)
  fileName <-  file.path(testdir, "report.docx")
  logger <- log4r::logger()
  testthat::expect_no_error(generateReport(reportDocx,
                                           dataReportList,
                                           fileName,
                                           logger))
  unlink(fileName)
})

test_that("Generate Report tablenumpar ild", {
  reportDocx <- read_docx(path = system.file("templates",
                                             "word",
                                             "DARWIN_EU_Study_Report.docx",
                                             package = "ReportGenerator"))
  reportItemsPath <- testthat::test_path("studies", "misc", "ild", "reportItems.rds")
  reportItems <- read_rds(reportItemsPath)
  dataReportList <- reportItems$reportItems
  testdir <- file.path(tempdir(), "reportItems")
  dir.create(testdir)
  fileName <-  file.path(testdir, "report.docx")
  logger <- log4r::logger()
  testthat::expect_no_error(generateReport(reportDocx,
                                           dataReportList,
                                           fileName,
                                           logger))
  unlink(fileName)
})
