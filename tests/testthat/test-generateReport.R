test_that("Generate Report incidence_plot and incidence_table", {
  reportDocx <- read_docx(path = system.file("templates", "word", "DARWIN_EU_Study_Report.docx", package = "ReportGenerator"))
  reportItemsPath <- testthat::test_path("studies", "generate_report_test", "reportI_items-test-incidence-table-plot.rds")
  reportItems <- read_rds(reportItemsPath)
  dataReportList <- reportItems$reportItems
  testdir <- file.path(tempdir(), "reportItems")
  dir.create(testdir)
  fileName <-  file.path(testdir, "report.docx")
  logger <- log4r::logger()
  # generateReport(reportDocx, dataReportList, fileName, logger)
  testthat::expect_no_error(generateReport(reportDocx,
                                           dataReportList,
                                           fileName,
                                           logger))
  unlink(testdir, recursive = TRUE)
})

test_that("Generate Report prevalence_plot and prevalence_table", {
  reportDocx <- read_docx(path = system.file("templates", "word", "DARWIN_EU_Study_Report.docx", package = "ReportGenerator"))
  reportItemsPath <- testthat::test_path("studies", "generate_report_test", "reportI_items-test-prevalence-table-plot.rds")
  reportItems <- read_rds(reportItemsPath)
  dataReportList <- reportItems$reportItems
  testdir <- file.path(tempdir(), "reportItems")
  dir.create(testdir)
  fileName <-  file.path(testdir, "report.docx")
  logger <- log4r::logger()
  # generateReport(reportDocx, dataReportList, fileName, logger)
  testthat::expect_no_error(generateReport(reportDocx,
                                           dataReportList,
                                           fileName,
                                           logger))
  unlink(testdir, recursive = TRUE)
})

test_that("Generate Report summarise_characteristics", {
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
  unlink(testdir, recursive = TRUE)
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
  unlink(testdir, recursive = TRUE)
})
