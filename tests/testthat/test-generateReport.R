test_that("Generate Report", {
  reportDocx <- read_docx(path = system.file("templates",
                                             "word",
                                             "DARWIN_EU_Study_Report.docx",
                                             package = "ReportGenerator"))
  dataReportList <- reportItems$reportItems
  fileName <-  here::here("results", "report.docx" )
  logger <- log4r::logger()
  generateReport(reportDocx,
                 dataReportList,
                 fileName,
                 logger)
})
