library(testthat)
library(ReportGenerator)

# Dummy data
data <- data.frame(
  event_cohort_name1 = c(
    "Acetaminophen", "Aspirin", "Amoxicillin+Clavulanate", "Acetaminophen",
    "Aspirin", "Aspirin", "Acetaminophen"),
  event_cohort_name2 = c(
    NA, NA, NA, "Amoxicillin+Clavulanate", "Acetaminophen",
    "Amoxicillin+Clavulanate", "Aspirin"),
  freq = c(206, 211, 48, 6, 12, 6, 14))

tmpDir <- paste0(tempdir(), "\\")

test_that("void", {
  expect_error(outputSankeyDiagram())
})


test_that("minimal", {
  # Make tmpDir

  outputSankeyDiagram(
    data = data,
    outputFolder = tmpDir,
    groupCombinations = TRUE,
    fileName = "sankeyDiagram.html")

  expect_true(file.exists(paste0(tmpDir, "\\sankeyDiagram.html")))
})
