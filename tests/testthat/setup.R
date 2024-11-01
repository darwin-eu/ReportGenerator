# actions to perform before tests have been started
tmpDir <- paste0(tempdir(), .Platform$file.sep)
dir.create(tmpDir, showWarnings = FALSE)

log_file <- glue::glue("testlog.txt")
logger <- log4r::logger(threshold = "INFO", appenders = list(log4r::console_appender(), log4r::file_appender(log_file)))

# load testdata
# testData <- readRDS(file.path(testthat::test_path("testData.rds")))
load(system.file("R", "sysdata.rda", package = "ReportGenerator"))
reportItems <- readRDS(file.path(testthat::test_path("reportItems.rds")))
