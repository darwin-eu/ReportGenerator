test_that("survival modules classes", {
  testServer(reportGenerator(), {
    expect_s3_class(cohortSurvivalUI("single_event", testData$CohortSurvival$single_event), "shiny.tag.list")
    expect_s3_class(cohortSurvivalUI("competing_risk", testData$CohortSurvival$competing_risk), "shiny.tag.list")
  })
})

test_that("competing risk test data in tableSurvival", {

  fileDataPath <- list.files(testthat::test_path("studies", "summarised_zip"),
                             pattern = "zip",
                             full.names = TRUE)
  uploadedData <- joinDatabases(fileDataPath[1:2])

  competing_risk_data <- getSummarisedData(uploadedData = uploadedData$summarised_result,
                                           type_result = "survival") %>%
    visOmopResults::filterSettings(analysis_type == "competing_risk")

  single_event_data <- getSummarisedData(uploadedData = uploadedData$summarised_result,
                                           type_result = "survival") %>%
    visOmopResults::filterSettings(analysis_type == "single_event")

  competing_risk_data <- competing_risk_data %>%
    filter(cdm_name %in% "CHUBX",
           result_id %in% 1,
           group_name %in% "cohort",
           group_level %in% "mgus_diagnosis",
           strata_name %in% "overall",
           strata_level %in% "overall",
           variable_name %in% c("cumulative_failure_probability", "survival_event", "survival_summary"),
           variable_level %in% c("progression", "death_cohort"),
           estimate_type %in% "numeric")
  # %>%
  #   visOmopResults::filterAdditional(outcome %in% "death_cohort",
  #                                    eventgap %in% "overall")

  tableSurvival(competing_risk_data)

})
