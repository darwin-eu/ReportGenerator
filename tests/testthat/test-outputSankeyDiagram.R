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

test_that("void", {
  expect_error(outputSankeyDiagram())
})


test_that("minimal", {
  outputSankeyDiagram(
    data = data,
    outputFolder = tmpDir,
    groupCombinations = TRUE,
    fileName = "sankeyDiagram.html")

  path <- normalizePath(paste0(tmpDir, "sankeyDiagram.html"), mustWork = FALSE)

  expect_true(file.exists(path))
})
