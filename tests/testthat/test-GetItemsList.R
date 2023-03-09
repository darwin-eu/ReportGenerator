genericChecks <- function(result) {
  expect_equal(class(result), "data.frame")
  expect_equal(colnames(result), c("title", "signature"))
}

test_that("getItemsList happy flow", {

  uploadedFiles <- c("incidence_attrition_example_data.csv",
                     "incidence_estimates_example_data.csv",
                     "prevalence_attrition_example_data.csv")

  result <- getItemsList(uploadedFiles)

  genericChecks(result)
  expect_equal(nrow(result), 9)

  uploadedFiles <- c("incidence_attrition_example_data.csv",
                     "prevalence_attrition_example_data.csv")

  result <- getItemsList(uploadedFiles)

  genericChecks(result)
  expect_equal(nrow(result), 1)
})

test_that("getItemsList edge cases", {

  result <- getItemsList(uploadedFiles = NULL)

  genericChecks(result)
  expect_equal(nrow(result), 0)
})
