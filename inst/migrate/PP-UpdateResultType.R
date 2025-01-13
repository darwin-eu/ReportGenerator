# Migration script for Patient Profiles: update result_type field to what it should be in latest version
library(readr)
library(glue)

# Config
inputZipFile <- "~/Code/ReportGenerator/results/StudyResults.zip"
outputZipFile <- "~/Code/ReportGenerator/results/StudyResults.zip"
filePattern <- "patientCharacteristics"
inputCsvSeparator <- ","

# unzip files
if (!file.exists(inputZipFile)) {
  stop("Input file does not exist!")
}
tempDir <- tempdir()
outDir <- file.path(tempDir, gsub(".zip", "", basename(inputZipFile)))
utils::unzip(zipfile = inputZipFile,
             exdir = tempDir)

# filter files
files <- list.files(outDir)
ppFiles <- files[grepl(filePattern, files)]

# update if needed
anyUpdates <- FALSE
for (filename in ppFiles) {
  fileUpdated <- FALSE
  fullFilename <- file.path(outDir, filename)
  data <- read_delim(fullFilename, delim = inputCsvSeparator, show_col_types = FALSE)
  # check result_type is available
  colNames <- colnames(data)
  if (length(colNames) > 1) {
    if ("result_type" %in% colNames) {
      resultType <- unique(data$result_type)
      if (resultType != "summarise_characteristics") {
        data$result_type <- "summarise_characteristics"
        fileUpdated <- anyUpdates <- TRUE
      }
    } else {
      data$result_type <- "summarise_characteristics"
      fileUpdated <- anyUpdates <- TRUE
    }
  } else {
    message("There is just one column in the data, did you set the right separator?")
  }
  if (fileUpdated) {
    write.csv(data, fullFilename)
    message(glue::glue("File {filename} has been updated"))
  } else {
    message(glue::glue("File {filename} has not been updated"))
  }
}

# create zip
if (anyUpdates) {
  utils::zip(zipfile = outputZipFile, files = file.path(outDir, files), extras = "-j")
  message(glue::glue("The updated zip file is available at {outputZipFile}"))
} else {
  message("No modifications made: your zip file is already up-to-date!")
}
