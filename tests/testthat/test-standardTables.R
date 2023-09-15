test_that("Table 1 attrition type single", {
  prevalence_attrition <- prevalence_attrition_test %>%
    filter(cdm_name == "IPCI")
  incidence_attrition <- incidence_attrition_test %>%
    filter(cdm_name == "IPCI")
  table1attrition <- table1NumPar(prevalence_attrition = prevalence_attrition,
                                  incidence_attrition = incidence_attrition)
  expect_s3_class(table1attrition, "huxtable")
})

test_that("Table 1 attrition type multiple", {
  table1attrition <- table1NumPar(prevalence_attrition = prevalence_attrition_test,
                                  incidence_attrition = incidence_attrition_test)
  expect_s3_class(table1attrition, "huxtable")
})

test_that("Table 1 Inc Prev returns table", {
  table1Inc <- table1Inc(incidence_estimates = incidence_estimates_test)
  expect_s3_class(table1Inc, "gt_tbl")
})

test_that("Table 2 Inc Over", {
  table2IncOver <- table2IncOver(incidence_estimates = incidence_estimates_test)
  expect_s3_class(table2IncOver, "huxtable")
})

test_that("Table 3 Inc Year", {
  table3IncYear <- table3IncYear(incidence_estimates = incidence_estimates_test)
  expect_s3_class(table3IncYear, "huxtable")
})

test_that("Table 4 Inc Age", {
  table4IncAge <- table4IncAge(incidence_estimates = incidence_estimates_test)
  expect_s3_class(table4IncAge, "huxtable")
})

test_that("Table 5 Inc Sex", {
  table5IncSex <- table5IncSex(incidence_estimates = incidence_estimates_test)
  expect_s3_class(table5IncSex, "huxtable")
})

