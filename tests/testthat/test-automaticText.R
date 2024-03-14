test_that("check automatic character single", {
  autotext <- table1aAutText(incidence_attrition = incidence_attrition_test, prevalence_attrition = prevalence_attrition_test)
  expect_type(autotext, "character")
})

test_that("check automatic character two databases", {
  incidence_attrition_test_two <- incidence_attrition_test %>% filter(cdm_name %in% c("CHUBX", "IPCI"))
  prevalence_attrition_test_two <- prevalence_attrition_test %>% filter(cdm_name %in% c("CHUBX", "IPCI"))
  autotext <- table1aAutText(incidence_attrition = incidence_attrition_test_two, prevalence_attrition = prevalence_attrition_test_two)
  expect_type(autotext, "character")
})

test_that("check automatic character three databases", {
  autotext <- table1aAutText(incidence_attrition = incidence_attrition_test, prevalence_attrition = incidence_attrition_test)
  expect_type(autotext, "character")
})

test_that("check automatic incidence attrition", {
  autotext <- tableAttrition(attritionData = incidence_attrition_test)
  expect_type(autotext, "character")
})

test_that("check automatic character two databases incidence attrition", {

  incidence_attrition_test_two <- incidence_attrition_test %>% filter(cdm_name %in% c("CHUBX", "CPRD GOLD"))
  autotext <- tableAttrition(attritionData = incidence_attrition_test)
  expect_type(autotext, "character")
})

test_that("check automatic character three databases incidence attrition", {
  autotext <- tableAttrition(attritionData = incidence_attrition_test)
  expect_type(autotext, "character")
})

test_that("check automatic prevalence attrition", {
  autotext <- tableAttrition(attritionData = prevalence_attrition_test)
  expect_type(autotext, "character")
})

test_that("check automatic character two databases prevalence attrition", {

  prevalence_attrition_test_two <- prevalence_attrition_test %>% filter(cdm_name %in% c("CHUBX", "CPRD GOLD"))
  autotext <- tableAttrition(attritionData = prevalence_attrition_test)
  expect_type(autotext, "character")
})

test_that("check automatic character three databases prevalence attrition", {
  autotext <- tableAttrition(attritionData = prevalence_attrition_test)
  expect_type(autotext, "character")
})

test_that("autoCaptionCharac result", {
  summarised_characteristics <- testData$summarised_characteristics
  caption <- autoCaptionCharac(summarised_characteristics)
  expect_equal(caption, "Demographic characteristics of Hepatitisb and Hepatitisc patients")
})



