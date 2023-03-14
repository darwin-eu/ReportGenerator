library(ReportGenerator)
library(testthat)


json <- "{ \"data\" : {\"name\":\"root\",\"children\":[{\"name\":\"4\",\"children\":[{\"name\":\"End\",\"size\":\"211\"},{\"name\":\"1\",\"children\":[{\"name\":\"End\",\"size\":\"12\"}]},{\"name\":\"10\",\"children\":[{\"name\":\"End\",\"size\":\"6\"}]}]},{\"name\":\"1\",\"children\":[{\"name\":\"End\",\"size\":\"206\"},{\"name\":\"4\",\"children\":[{\"name\":\"End\",\"size\":\"14\"}]},{\"name\":\"10\",\"children\":[{\"name\":\"End\",\"size\":\"6\"}]}]},{\"name\":\"10\",\"children\":[{\"name\":\"End\",\"size\":\"48\"}]}]}, \"lookup\" : [{ \"key\": \"1\", \"value\": \"Acetaminophen\"},{ \"key\": \"2\", \"value\": \"Amoxicillin\"},{ \"key\": \"4\", \"value\": \"Aspirin\"},{ \"key\": \"8\", \"value\": \"Clavulanate\"},{ \"key\": \"16\", \"value\": \"Doxylamine\"},{ \"key\": \"32\", \"value\": \"PenicillinV\"},{ \"key\": \"64\", \"value\": \"Other\"},{ \"key\": \"End\", \"value\": \"End\"}]}"
tmpFile <- tempfile(fileext = ".html")

test_that("void", {
  expect_error(
    createLegend()
  )
})

test_that("minimal", {
  createLegend(
    inputJSON = json,
    fileName = tmpFile)

  expect_true(
    file.exists(tmpFile)
  )
})
