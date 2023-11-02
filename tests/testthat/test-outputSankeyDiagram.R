library(testthat)
library(ReportGenerator)

# Dummy data
data <- data.frame(
  path = c("Acetaminophen", "Acetaminophen-Amoxicillin+Clavulanate", "Acetaminophen-Aspirin"),
  freq = c(206, 6, 14),
  sex = c("all", "all", "all"),
  age = c("all", "all", "all"),
  indexYear = c("all", "all", "all"))

test_that("void", {
  expect_error(outputSankeyDiagram())
})

test_that("Sankey created file", {
  outputFile <- tempfile(fileext = "html")
  outputSankey <- createSankeyDiagram(
    treatmentPathways = data,
    outputFile = outputFile,
    groupCombinations = TRUE,
    minFreq = 5)
  expect_true(file.exists(outputFile))
})
