test_that("check automatic character single", {
  autotext <- table1aAutText(incidence_attrition = incidence_attrition_latest, prevalence_attrition = prevalence_attrition_latest)
  expect_type(autotext, "character")
})

test_that("check automatic character two databases", {

  incidence_attrition_latest_two <- incidence_attrition_latest_multiple %>% filter(cdm_name %in% c("CHUBX", "CPRD GOLD"))
  prevalence_attrition_latest_two <- prevalence_attrition_latest_multiple %>% filter(cdm_name %in% c("CHUBX", "CPRD GOLD"))
  autotext <- table1aAutText(incidence_attrition = incidence_attrition_latest_two, prevalence_attrition = prevalence_attrition_latest_two)
  expect_type(autotext, "character")
})

test_that("check automatic character three databases", {
  autotext <- table1aAutText(incidence_attrition = incidence_attrition_latest_multiple, prevalence_attrition = incidence_attrition_latest_multiple)
  expect_type(autotext, "character")
})


