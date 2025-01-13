test_that("automatic text w/ one database", {
  incidence_attrition <- testData$IncidencePrevalence$incidence_estimates %>%
    visOmopResults::filterSettings(result_type == "incidence_attrition") %>%
    filter(cdm_name == "CHUBX")
  autotext <- table1aAutText(attrition_data = incidence_attrition)
  prevalence_attrition <- testData$IncidencePrevalence$prevalence_estimates %>%
    visOmopResults::filterSettings(result_type == "prevalence_attrition")
  autotext <- table1aAutText(attrition_data = prevalence_attrition)
  expect_type(autotext, "character")
})

test_that("check automatic incidence attrition", {
  incidence_attrition <- testData$IncidencePrevalence$incidence_estimates %>%
    visOmopResults::filterSettings(result_type == "incidence_attrition")
  autotext <- tableAttrition(attrition_data = incidence_attrition)
  expect_type(autotext, "character")
})

test_that("check automatic character two databases incidence attrition", {
  incidence_attrition <- testData$IncidencePrevalence$incidence_estimates %>%
    visOmopResults::filterSettings(result_type == "incidence_attrition")
  incidence_attrition_two <- incidence_attrition %>% dplyr::filter(cdm_name %in% c("CHUBX", "CPRD GOLD"))
  autotext <- tableAttrition(attrition_data = incidence_attrition_two)
  expect_type(autotext, "character")
})

test_that("check automatic character three databases incidence attrition", {
  incidence_attrition <- testData$IncidencePrevalence$incidence_estimates %>%
    visOmopResults::filterSettings(result_type == "incidence_attrition")
  incidence_attrition_two <- incidence_attrition %>% dplyr::filter(cdm_name %in% c("CHUBX", "CPRD GOLD", "SIDIAP"))
  autotext <- tableAttrition(attrition_data = incidence_attrition_two)
  expect_type(autotext, "character")
})



test_that("check automatic character two databases prevalence attrition", {
  prevalence_attrition <- testData$IncidencePrevalence$prevalence_estimates %>%
    visOmopResults::filterSettings(result_type == "prevalence_attrition")
  prevalence_attrition_two <- prevalence_attrition %>% dplyr::filter(cdm_name %in% c("CHUBX", "CPRD GOLD"))
  autotext <- tableAttrition(attrition_data = prevalence_attrition)
  expect_type(autotext, "character")
})

test_that("check automatic character three databases prevalence attrition", {
  prevalence_attrition <- testData$IncidencePrevalence$prevalence_estimates %>%
    visOmopResults::filterSettings(result_type == "prevalence_attrition")
  prevalence_attrition_two <- prevalence_attrition %>% dplyr::filter(cdm_name %in% c("CHUBX", "CPRD GOLD", "SIDIAP"))

  autotext <- tableAttrition(attrition_data = prevalence_attrition)
  expect_type(autotext, "character")
})

test_that("autoCaptionCharac result", {
  summarise_characteristics <- testData$summarise_characteristics
  caption <- autoCaptionCharac(summarise_characteristics)
  expect_equal(caption, "Demographic characteristics of  patients." )
})



