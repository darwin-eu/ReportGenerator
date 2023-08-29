test_that("Table 1 attrition type", {
  table1attrition <- table1NumPar(prevalence_attrition = prevalence_attrition_latest,
                                  incidence_attrition = incidence_attrition_latest)
  expect_s3_class(table1attrition, "huxtable")
})
