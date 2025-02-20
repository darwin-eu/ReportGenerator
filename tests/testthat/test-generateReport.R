test_that("Generate Report incidence_plot and incidence_table", {
  reportDocx <- read_docx(path = system.file("templates", "word", "DARWIN_EU_Study_Report.docx", package = "ReportGenerator"))
  reportItemsPath <- testthat::test_path("studies", "generate_report_test", "reportI_items_test_incidence_table_plot.rds")
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
  reportItemsPath <- testthat::test_path("studies", "generate_report_test", "reportI_items_test_prevalence_table_plot.rds")
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

test_that("Generate report incidence attrition", {
  reportDocx <- read_docx(path = system.file("templates",
                                             "word",
                                             "DARWIN_EU_Study_Report.docx",
                                             package = "ReportGenerator"))
  reportItemsPath <- testthat::test_path("studies", "generate_report_test", "reportI_items_test_incidence_attrition.rds")
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

test_that("Generate report prevalence attrition", {
  reportDocx <- read_docx(path = system.file("templates",
                                             "word",
                                             "DARWIN_EU_Study_Report.docx",
                                             package = "ReportGenerator"))
  reportItemsPath <- testthat::test_path("studies", "generate_report_test", "reportI_items_test_prevalence_attrition.rds")
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

test_that("Generate Report summarise_characteristics", {
  reportDocx <- read_docx(path = system.file("templates",
                                             "word",
                                             "DARWIN_EU_Study_Report.docx",
                                             package = "ReportGenerator"))
  reportItemsPath <- testthat::test_path("studies", "generate_report_test", "reportI_items_test_cohort_characteristics_table_plot.rds")
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

test_that("Generate Report large_scale_characteristics", {
  reportDocx <- read_docx(path = system.file("templates",
                                             "word",
                                             "DARWIN_EU_Study_Report.docx",
                                             package = "ReportGenerator"))
  reportItemsPath <- testthat::test_path("studies", "generate_report_test", "reportI_items_test_large_scale_table_plot.rds")
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

test_that("Generate report survival single_event table", {
  reportDocx <- read_docx(path = system.file("templates",
                                             "word",
                                             "DARWIN_EU_Study_Report.docx",
                                             package = "ReportGenerator"))
  reportItemsPath <- testthat::test_path("studies", "generate_report_test", "reportI_items_test_survival_single_table.rds")
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

test_that("Generate report survival single_event plot", {
  reportDocx <- read_docx(path = system.file("templates",
                                             "word",
                                             "DARWIN_EU_Study_Report.docx",
                                             package = "ReportGenerator"))
  reportItemsPath <- testthat::test_path("studies", "generate_report_test", "reportI_items_test_survival_single_plot.rds")
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

test_that("Generate report survival competing_risk table", {
  reportDocx <- read_docx(path = system.file("templates",
                                             "word",
                                             "DARWIN_EU_Study_Report.docx",
                                             package = "ReportGenerator"))
  reportItemsPath <- testthat::test_path("studies", "generate_report_test", "reportI_items_test_survival_competing_table.rds")
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

test_that("Generate report survival competing_risk plot", {
  reportDocx <- read_docx(path = system.file("templates",
                                             "word",
                                             "DARWIN_EU_Study_Report.docx",
                                             package = "ReportGenerator"))
  reportItemsPath <- testthat::test_path("studies", "generate_report_test", "reportI_items_test_survival_competing_plot.rds")
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

test_that("Generate report TreatmentPatterns Sunburst chart", {
  reportDocx <- read_docx(path = system.file("templates",
                                             "word",
                                             "DARWIN_EU_Study_Report.docx",
                                             package = "ReportGenerator"))
  reportItemsPath <- testthat::test_path("studies", "generate_report_test", "reportI_items_test_pathways_sunburst.rds")
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
