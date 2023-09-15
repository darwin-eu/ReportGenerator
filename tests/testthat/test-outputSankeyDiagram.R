library(testthat)
library(ReportGenerator)

# Dummy data
data <- data.frame(
  path = c("Acetaminophen", "Acetaminophen-Amoxicillin+Clavulanate", "Acetaminophen-Aspirin"),
  freq = c(206, 6, 14),
  sex = c("all", "all", "all"),
  age = c("all", "all", "all"),
  index_year = c("all", "all", "all"))

test_that("void", {
  expect_error(outputSankeyDiagram())
})

test_that("minimal", {
  outputSankey <- createSankeyDiagram(
    treatmentPathways = data,
    groupCombinations = FALSE,
    minFreq = 5)
  expect_class(outputSankey, "gvis")
})
