test_that("Generate Report", {
  reportDocx <- read_docx(path = system.file("templates",
                                             "word",
                                             "DARWIN_EU_Study_Report.docx",
                                             package = "ReportGenerator"))
  dataReportList <- reportItems$reportItems
  fileName <-  file.path(tmpDir, "report.docx")
  logger <- log4r::logger()
  testthat::expect_no_error(generateReport(reportDocx,
                                           dataReportList,
                                           fileName,
                                           logger))
})
