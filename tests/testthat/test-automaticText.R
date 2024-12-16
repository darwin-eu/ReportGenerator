# test_that("automatic text w/ one database", {
#   incidence_attrition <- testData$IncidencePrevalence$incidence_estimates %>%
#     visOmopResults::filterSettings(result_type == "incidence_attrition")
#   prevalence_attrition <- testData$IncidencePrevalence$prevalence_estimates %>%
#     visOmopResults::filterSettings(result_type == "prevalence_attrition")
#   autotext <- table1aAutText(incidence_attrition,
#                              prevalence_attrition)
#   expect_type(autotext, "character")
# })
#
# test_that("check automatic text multiple databases", {
#   incidence_attrition <- testData$IncidencePrevalence$incidence_estimates %>%
#     visOmopResults::filterSettings(result_type == "incidence_attrition")
#   prevalence_attrition <- testData$IncidencePrevalence$prevalence_estimates %>%
#     visOmopResults::filterSettings(result_type == "prevalence_attrition")
#   autotext <- table1aAutText(incidence_attrition = incidence_attrition, prevalence_attrition = prevalence_attrition)
#   expect_type(autotext, "character")
# })
#
# test_that("check automatic character three databases", {
#   incidence_attrition <- testData$IncidencePrevalence$incidence_estimates %>%
#     visOmopResults::filterSettings(result_type == "incidence_attrition")
#   prevalence_attrition <- testData$IncidencePrevalence$prevalence_estimates %>%
#     visOmopResults::filterSettings(result_type == "prevalence_attrition")
#   autotext <- table1aAutText(incidence_attrition = incidence_attrition, prevalence_attrition = incidence_attrition)
#   expect_type(autotext, "character")
# })
#
# test_that("check automatic incidence attrition", {
#   incidence_attrition <- testData$IncidencePrevalence$incidence_estimates %>%
#     visOmopResults::filterSettings(result_type == "incidence_attrition")
#   prevalence_attrition <- testData$IncidencePrevalence$prevalence_estimates %>%
#     visOmopResults::filterSettings(result_type == "prevalence_attrition")
#   autotext <- tableAttrition(attritionData = incidence_attrition)
#   expect_type(autotext, "character")
# })
#
# test_that("check automatic character two databases incidence attrition", {
#   incidence_attrition <- testData$IncidencePrevalence$incidence_estimates %>%
#     visOmopResults::filterSettings(result_type == "incidence_attrition")
#   prevalence_attrition <- testData$IncidencePrevalence$prevalence_estimates %>%
#     visOmopResults::filterSettings(result_type == "prevalence_attrition")
#
#   incidence_attrition_two <- incidence_attrition %>% dplyr::filter(cdm_name %in% c("CHUBX", "CPRD GOLD"))
#   autotext <- tableAttrition(attritionData = incidence_attrition)
#   expect_type(autotext, "character")
# })
#
# test_that("check automatic character three databases incidence attrition", {
#   incidence_attrition <- testData$IncidencePrevalence$incidence_estimates %>%
#     visOmopResults::filterSettings(result_type == "incidence_attrition")
#   prevalence_attrition <- testData$IncidencePrevalence$prevalence_estimates %>%
#     visOmopResults::filterSettings(result_type == "prevalence_attrition")
#
#   autotext <- tableAttrition(attritionData = incidence_attrition)
#   expect_type(autotext, "character")
# })
#
# test_that("check automatic prevalence attrition", {
#   incidence_attrition <- testData$IncidencePrevalence$incidence_estimates %>%
#     visOmopResults::filterSettings(result_type == "incidence_attrition")
#   prevalence_attrition <- testData$IncidencePrevalence$prevalence_estimates %>%
#     visOmopResults::filterSettings(result_type == "prevalence_attrition")
#
#   autotext <- tableAttrition(attritionData = prevalence_attrition)
#   expect_type(autotext, "character")
# })
#
# test_that("check automatic character two databases prevalence attrition", {
#
#   incidence_attrition <- testData$IncidencePrevalence$incidence_estimates %>%
#     visOmopResults::filterSettings(result_type == "incidence_attrition")
#   prevalence_attrition <- testData$IncidencePrevalence$prevalence_estimates %>%
#     visOmopResults::filterSettings(result_type == "prevalence_attrition")
#
#   prevalence_attrition_two <- prevalence_attrition %>% dplyr::filter(cdm_name %in% c("CHUBX", "CPRD GOLD"))
#   autotext <- tableAttrition(attritionData = prevalence_attrition)
#   expect_type(autotext, "character")
# })
#
# test_that("check automatic character three databases prevalence attrition", {
#   incidence_attrition <- testData$IncidencePrevalence$incidence_estimates %>%
#     visOmopResults::filterSettings(result_type == "incidence_attrition")
#   prevalence_attrition <- testData$IncidencePrevalence$prevalence_estimates %>%
#     visOmopResults::filterSettings(result_type == "prevalence_attrition")
#
#   autotext <- tableAttrition(attritionData = prevalence_attrition)
#   expect_type(autotext, "character")
# })
#
# test_that("autoCaptionCharac result", {
#   incidence_attrition <- testData$IncidencePrevalence$incidence_estimates %>%
#     visOmopResults::filterSettings(result_type == "incidence_attrition")
#   prevalence_attrition <- testData$IncidencePrevalence$prevalence_estimates %>%
#     visOmopResults::filterSettings(result_type == "prevalence_attrition")
#
#   summarise_characteristics <- testData$summarise_characteristics
#   caption <- autoCaptionCharac(summarise_characteristics)
#   expect_equal(caption, "Demographic characteristics of  patients." )
# })
#
#
#
