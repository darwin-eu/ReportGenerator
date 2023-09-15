test_that("check automatic character single", {
  autotext <- table1aAutText(incidence_attrition = incidence_attrition_test, prevalence_attrition = prevalence_attrition_test)
  expect_type(autotext, "character")
})

test_that("check automatic character two databases", {

  incidence_attrition_test_two <- incidence_attrition_test %>% filter(cdm_name %in% c("CHUBX", "CPRD GOLD"))
  prevalence_attrition_test_two <- prevalence_attrition_test %>% filter(cdm_name %in% c("CHUBX", "CPRD GOLD"))
  autotext <- table1aAutText(incidence_attrition = incidence_attrition_test_two, prevalence_attrition = prevalence_attrition_test_two)
  expect_type(autotext, "character")
})

test_that("check automatic character three databases", {
  autotext <- table1aAutText(incidence_attrition = incidence_attrition_test, prevalence_attrition = incidence_attrition_test)
  expect_type(autotext, "character")
})


