test_that("Prevalence Attrition", {

  fileDataPath <- list.files(test_path("studies", "summarised_zip"),
                             pattern = "zip",
                             full.names = TRUE)
  test_data <- joinDatabases(fileDataPath[1])

  prevalence_attrition <- test_data$summarised_result %>%
    visOmopResults::filterSettings(result_type == "prevalence_attrition") %>%
    dplyr::filter(cdm_name %in% "CHUBX") %>%
    visOmopResults::filterSettings(denominator_sex %in% "Female",
                                   denominator_age_group %in% "0 to 64")

  expect_error(tablePrevalenceAttrition(result = prevalence_attrition,
                           type = "gt",
                           header = "variable_name",
                           groupColumn = c("cdm_name", "variable_level"),
                           settingsColumn = NULL,
                           hide = "estimate_name"))

})
