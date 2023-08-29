test_that("Table 1 attrition type single", {
  table1attrition <- table1NumPar(prevalence_attrition = prevalence_attrition_latest,
                                  incidence_attrition = incidence_attrition_latest)
  expect_s3_class(table1attrition, "huxtable")
})

test_that("Table 1 attrition type multiple", {
  table1attrition <- table1NumPar(prevalence_attrition = prevalence_attrition_latest_multiple,
                                  incidence_attrition = incidence_attrition_latest_multiple)
  expect_s3_class(table1attrition, "huxtable")
})

test_that("Table 1 sex age", {
  table1SexAge <- table1SexAge(incidence_estimates = incidence_estimates_latest)
  expect_s3_class(table1SexAge, "huxtable")
})

test_that("Table 2 Inc Over", {
  table2IncOver <- table2IncOver(incidence_estimates = incidence_estimates_latest)
  expect_s3_class(table2IncOver, "huxtable")
})

test_that("Table 3 Inc Year", {
  table3IncYear <- table3IncYear(incidence_estimates = incidence_estimates_latest)
  expect_s3_class(table3IncYear, "huxtable")
})

test_that("Table 4 Inc Age", {
  table4IncAge <- table4IncAge(incidence_estimates = incidence_estimates_latest)
  expect_s3_class(table4IncAge, "huxtable")
})

test_that("Table 5 Inc Sex", {
  table5IncSex <- table5IncSex(incidence_estimates = incidence_estimates_latest)
  expect_s3_class(table5IncSex, "huxtable")
})
