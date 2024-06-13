test_that("incidencePrevalenceData works", {
  sampleSize <- 5000
  incidencePrevalenceData <- getIncidencePrevalence(sampleSize = sampleSize)
  expect_equal(length(incidencePrevalenceData), 4)
  expect_equal(names(incidencePrevalenceData), c("incidence_estimates",
                                                 "incidence_attrition",
                                                 "prevalence_estimates",
                                                 "prevalence_attrition"))
  expect_equal(class(incidencePrevalenceData), "list")
})

test_that("treatmentPathwaysData works", {
  if (!dir.exists(Sys.getenv("EUNOMIA_DATA_FOLDER"))) {
    eunomia_temp_folder <- file.path(tempdir(), "eunomia_data_folder")
    dir.create(eunomia_temp_folder)
    Sys.setenv(EUNOMIA_DATA_FOLDER = eunomia_temp_folder)
    treatmentPathwaysData <- getTreatmentPathways()
    expect_equal(length(treatmentPathwaysData), 6)
    expect_equal(names(treatmentPathwaysData), c("countsAge", "countsSex",
                                                 "countsYear", "metadata",
                                                 "summaryStatsTherapyDuration",
                                                 "treatmentPathways"))
    expect_equal(class(treatmentPathwaysData), "list")
    unlink(eunomia_temp_folder, recursive = TRUE)
  } else {
    treatmentPathwaysData <- getTreatmentPathways()
    expect_equal(length(treatmentPathwaysData), 6)
    expect_equal(names(treatmentPathwaysData), c("countsAge", "countsSex",
                                                 "countsYear", "metadata",
                                                 "summaryStatsTherapyDuration",
                                                 "treatmentPathways"))
    expect_equal(class(treatmentPathwaysData), "list")
  }
})

### The following tests break because of the new version of dbplyr

# test_that("characteristicsData works", {
#   characteristicsData <- getCharacteristicsResult()
#   expect_equal(length(characteristicsData), 16)
#   expect_equal(names(characteristicsData), c("result_id", "cdm_name",
#                                              "result_type", "package_name",
#                                              "package_version", "group_name",
#                                              "group_level", "strata_name",
#                                              "strata_level", "variable_name",
#                                              "variable_level", "estimate_name",
#                                              "estimate_type", "estimate_value",
#                                              "additional_name", "additional_level"))
#   expect_equal(class(characteristicsData), c("summarised_result", "omop_result",
#                                              "tbl_df", "tbl", "data.frame"))
# })

# test_that("largeScaleCharacteristicsData works", {
#   largeScaleCharacteristicsData <- getLargeScaleCharacteristicsResult()
#   expect_equal(length(largeScaleCharacteristicsData), 16)
#   expect_equal(names(largeScaleCharacteristicsData), c("result_id", "cdm_name",
#                                                        "result_type", "package_name",
#                                                        "package_version", "group_name",
#                                                        "group_level", "strata_name",
#                                                        "strata_level", "variable_name",
#                                                        "variable_level", "estimate_name",
#                                                        "estimate_type", "estimate_value",
#                                                        "additional_name", "additional_level"))
#   expect_equal(class(largeScaleCharacteristicsData), c("summarised_result", "omop_result",
#                                                        "tbl_df", "tbl", "data.frame"))
# })

# test_that("cohortSurvivalData works", {
#   cohortSurvivalData <- getCohortSurvival()
#   expect_equal(length(cohortSurvivalData), 1)
#   expect_equal(names(cohortSurvivalData), "survivalEstimate")
#   expect_equal(class(cohortSurvivalData), "list")
# })

