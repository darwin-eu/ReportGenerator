# actions to perform before tests have been started
tmpDir <- paste0(tempdir(), .Platform$file.sep)
dir.create(tmpDir, showWarnings = FALSE)
