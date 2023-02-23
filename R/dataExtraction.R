#' Extracts denominator data from files in CSV format.
#' @param importFolderDenominator Location of denominator data.
#' @return A tibble
#' @export
#'
denominatorExtraction <- function (importFolderDenominator = here("inst/csv/denominatorMockData")) {

  result <- bind_rows(
    lapply(
      list.files(
        importFolderDenominator,
        pattern = ".csv",
        full.names = TRUE
      ),
      read_csv
    )
  )

  return(result)
}
#' Extracts incidence data from files in CSV format and saves the result in RDS.
#' @param importFolderIndcidence Location of incidence results.
#' @param studyName In a string, the name of the study.
#' @return A tibble
#' @export
incidenceExtraction <- function (importFolderIndcidence = here("inst/csv/incidenceMockResults"),
                                      studyName = "mock_data") {

  result <- bind_rows(
    lapply(
      list.files(
        importFolderIndcidence,
        pattern = ".csv",
        full.names = TRUE
      ),
      read_csv
    )
  ) %>% mutate(denominator_age_group = gsub(";", "-", denominator_age_group))
  return(result)
}
#' Extracts incidence data from files in CSV format and saves the result in RDS.
#' @param importFolderIndcidence Location of incidence results.
#' @param studyName In a string, the name of the study.
#' @return A tibble
#' @export
#'
incidenceExtractionToRDS <- function (importFolderIndcidence = here("inst/csv/incidenceMockResults"),
                                      studyName = "mock_data") {

  result <- bind_rows(
    lapply(
      list.files(
        importFolderIndcidence,
        pattern = ".csv",
        full.names = TRUE
      ),
      read_csv
    )
  ) %>% mutate(denominator_age_group = gsub(";", "-", denominator_age_group))

  subDir <- here("inst",
                 "data",
                 "incidence")

  if (!file.exists(subDir)) {
    dir.create(file.path(here("inst",
                              "data",
                              "incidence")),
               recursive = TRUE)
  }

  saveRDS(result,
          here("inst",
               "data",
               "incidence",
               paste0(
                 studyName,
                 ".rds"
               )
               )
          )
}
#' Extracts prevalence data from files in CSV format and saves the result in RDS.
#' @param importFolderPrevalence Location of prevalence results.
#' @param studyName In a string, the name of the study.
#' @return A tibble
#' @export
#'
prevalenceExtraction <- function (importFolderPrevalence = here("inst/csv/prevalenceMockResults"),
                                  studyName = "mock_data") {

  result <- bind_rows(
    lapply(
      list.files(
        importFolderPrevalence,
        pattern = ".csv",
        full.names = TRUE
      ),
      read_csv
    )
  ) %>% mutate(denominator_age_group = gsub(";", "-", denominator_age_group))
  return(result)

}
#' Extracts prevalence data from files in CSV format nd saves the result in RDS.
#' @param importFolderPrevalence Location of prevalence results.
#' @param studyName In a string, the name of the study.
#' @return A tibble
#' @export
#'
prevalenceExtractionToRDS <- function (importFolderPrevalence = here("inst/csv/prevalenceMockResults"),
                                       studyName = "mock_data") {

  result <- bind_rows(
    lapply(
      list.files(
        importFolderPrevalence,
        pattern = ".csv",
        full.names = TRUE
      ),
      read_csv
    )
  ) %>% mutate(denominator_age_group = gsub(";", "-", denominator_age_group))

  subDir <- here("inst",
                 "data",
                 "prevalence")

  if (!file.exists(subDir)) {
    dir.create(file.path(here("inst",
                              "data",
                              "prevalence")),
               recursive = TRUE)
  }

  saveRDS(result,
          here("inst",
               "data",
               "prevalence",
               paste0(
                 studyName,
                 ".rds"
               )
          )
  )

}
# Extracts denominator data from files in CSV format.
# bloodCancerPrevalenceExtractionToRDS <- function () {
#
#   prepare_output <- function(result){
#     result <- result %>%
#       mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "acute_ll_broad_2y", "ALL broad (2 year partial prevalence)")) %>%
#       mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "acute_ll_broad_5y", "ALL broad (5 year partial prevalence)")) %>%
#       mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "acute_ll_broad_end", "ALL broad (Complete prevalence)")) %>%
#       mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "acute_ll_narrow_2y", "ALL narrow (2 year partial prevalence)")) %>%
#       mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "acute_ll_narrow_5y", "ALL narrow (5 year partial prevalence)")) %>%
#       mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "acute_ll_narrow_end", "ALL narrow (Complete prevalence)")) %>%
#       mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "aml_broad_2y", "AML broad (2 year partial prevalence)")) %>%
#       mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "aml_broad_5y", "AML broad (5 year partial prevalence)")) %>%
#       mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "aml_broad_end", "AML broad (Complete prevalence)")) %>%
#       mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "aml_narrow_2y", "AML narrow (2 year partial prevalence)")) %>%
#       mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "aml_narrow_5y", "AML narrow (5 year partial prevalence)")) %>%
#       mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "aml_narrow_end", "AML narrow (Complete prevalence)")) %>%
#       mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "chronic_ll_broad_2y", "CLL broad (2 year partial prevalence)")) %>%
#       mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "chronic_ll_broad_5y", "CLL broad (5 year partial prevalence)")) %>%
#       mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "chronic_ll_broad_end", "CLL broad (Complete prevalence)")) %>%
#       mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "chronic_ll_narrow_2y", "CLL narrow (2 year partial prevalence)")) %>%
#       mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "chronic_ll_narrow_5y", "CLL narrow (5 year partial prevalence)")) %>%
#       mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "chronic_ll_narrow_end", "CLL narrow (Complete prevalence)")) %>%
#       mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "mm_broad_2y", "MM broad (2 year partial prevalence)")) %>%
#       mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "mm_broad_5y", "MM broad (5 year partial prevalence)")) %>%
#       mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "mm_broad_end", "MM broad (Complete prevalence)")) %>%
#       mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "mm_narrow_2y", "MM narrow (2 year partial prevalence)")) %>%
#       mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "mm_narrow_5y", "MM narrow (5 year partial prevalence)")) %>%
#       mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "mm_narrow_end", "MM narrow (Complete prevalence)")) %>%
#       mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "mm_broad_2y", "MM broad (2 year partial prevalence)")) %>%
#       mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "mm_broad_5y", "MM broad (5 year partial prevalence)")) %>%
#       mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "dlbc_w_fl3b_2y", "DLBC including FL3B (2 year partial prevalence)")) %>%
#       mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "dlbc_w_fl3b_5y", "DLBC including FL3B (5 year partial prevalence)")) %>%
#       mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "dlbc_w_fl3b_end", "DLBC including FL3B (Complete prevalence)")) %>%
#       mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "dlbc_wo_fl3b_2y", "DLBC excluding FL3B (2 year partial prevalence)")) %>%
#       mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "dlbc_wo_fl3b_5y", "DLBC excluding FL3B (5 year partial prevalence)")) %>%
#       mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "dlbc_wo_fl3b_end", "DLBC excluding FL3B (Complete prevalence)")) %>%
#       mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "fl_w_fl3b_2y", "FL including FL3B (2 year partial prevalence)")) %>%
#       mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "fl_w_fl3b_5y", "FL including FL3B (5 year partial prevalence)")) %>%
#       mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "fl_w_fl3b_end", "FL including FL3B (Complete prevalence)")) %>%
#       mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "fl_wo_fl3b_2y", "FL excluding FL3B (2 year partial prevalence)")) %>%
#       mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "fl_wo_fl3b_5y", "FL excluding FL3B (5 year partial prevalence)")) %>%
#       mutate(outcome_cohort_name = replace(outcome_cohort_name, outcome_cohort_name == "fl_wo_fl3b_end", "FL excluding FL3B (Complete prevalence)"))
#
#     result<- result %>%
#       mutate(denominator_age_group = stringr::str_replace(denominator_age_group, ";", " to ")) %>%
#       mutate(denominator_age_group = replace(denominator_age_group, denominator_age_group == "0 to 44", "\u226444")) %>%
#       mutate(denominator_age_group = replace(denominator_age_group, denominator_age_group == "65 to 150", "\u226565")) %>%
#       mutate(denominator_age_group = replace(denominator_age_group, denominator_age_group == "0 to 150", "All ages")) %>%
#       mutate(denominator_age_group = factor(denominator_age_group,
#                                             levels = c("All ages",
#                                                        "\u226444",
#                                                        "45 to 64",
#                                                        "\u226565",
#                                                        "0 to 9",
#                                                        "10 to 19",
#                                                        "20 to 29",
#                                                        "30 to 39",
#                                                        "40 to 49",
#                                                        "50 to 59",
#                                                        "60 to 69",
#                                                        "70 to 79",
#                                                        "80 to 89",
#                                                        "90 to 99",
#                                                        "100 to 150")))
#
#     result<- result %>%
#       mutate(denominator_days_prior_history = factor(denominator_days_prior_history,
#                                                      levels = c("0","365", "1095")))
#
#     return(result)
#   }
#
#   # printing numbers with 3 decimal place and commas
#   nice.num3<-function(x) {
#     trimws(format(round(x,3),
#                   big.mark=",",
#                   nsmall = 3,
#                   digits=3,
#                   scientific=FALSE))}
#
#   nice.num.count<-function(x) {
#     trimws(format(x,
#                   big.mark=",",
#                   nsmall = 0,
#                   digits=1,
#                   scientific=FALSE))}
#
#   # Load, prepare, and merge results -----
#   results <- list.files(here("Results/bloodCancerPrevalence/networkResults"), full.names = TRUE)
#
#   # prevalence attrition
#   cdm_snapshot_files<-results[stringr::str_detect(results, ".csv")]
#   cdm_snapshot_files<-results[stringr::str_detect(results, "cdm_snapshot")]
#   cdm_snapshot <- list()
#   for(i in seq_along(cdm_snapshot_files)){
#     cdm_snapshot[[i]]<-readr::read_csv(cdm_snapshot_files[[i]],
#                                        show_col_types = FALSE)  %>%
#       dplyr::mutate(cdm_version =as.character(cdm_version))
#   }
#   cdm_snapshot <- dplyr::bind_rows(cdm_snapshot)
#   cdm_snapshot <- cdm_snapshot %>%
#     mutate(person_cnt=nice.num.count(person_cnt)) %>%
#     mutate(observation_period_cnt=nice.num.count(observation_period_cnt))
#   cdm_snapshot <- cdm_snapshot %>%
#     dplyr::select(!c("cdm_holder", "cdm_release_date")) %>%
#     dplyr::rename("Database"="cdm_source_name") %>%
#     dplyr::rename("OMOP CDM version"="cdm_version") %>%
#     dplyr::rename("OMOP CDM vocabulary"="vocabulary_version") %>%
#     dplyr::rename("Number of people in the database"="person_cnt") %>%
#     dplyr::rename("Number of observation periods"="observation_period_cnt")
#   cdm_snapshot <- cdm_snapshot %>%
#     mutate(Database = replace(Database, Database == "CPRDGOLD202007", "CPRD GOLD")) %>%
#     mutate(Database = replace(Database, Database == "IPCI Marlin", "IPCI")) %>%
#     mutate(Database = replace(Database, Database == "SIDIAP OHDSI CDM V5.3 Database 22t2", "SIDIAP"))
#
#
#   subDir <- here("inst",
#                  "data",
#                  "bloodCancerPrevalence")
#
#   if (!file.exists(subDir)) {
#     dir.create(file.path(here("inst"),
#                          "data",
#                          "bloodCancerPrevalence"),
#                recursive = TRUE)
#   }
#
#   saveRDS(cdm_snapshot,
#           here("inst",
#                "data",
#                "bloodCancerPrevalence",
#                "cdm_snapshot.rds"))
#
#   # prevalence estimates
#
#   prevalence_estimates_files<-results[stringr::str_detect(results, ".csv")]
#
#   prevalence_estimates_files<-results[stringr::str_detect(results, "prevalence_estimates")]
#
#   prevalence_estimates <- list()
#
#   for(i in seq_along(prevalence_estimates_files)){
#
#     prevalence_estimates[[i]]<-readr::read_csv(prevalence_estimates_files[[i]],
#                                                show_col_types = FALSE)
#   }
#
#   prevalence_estimates <- dplyr::bind_rows(prevalence_estimates)
#
#   prevalence_estimates <- prepare_output(prevalence_estimates)
#
#   prevalence_estimates <- prevalence_estimates %>%
#
#     mutate("Prevalence (95% CI)"= ifelse(!is.na(prevalence),
#
#                                          paste0(paste0(nice.num3(prevalence*100), "%"), " (",
#
#                                                 paste0(nice.num3(prevalence_95CI_lower*100), "%")," to ",
#
#                                                 paste0(nice.num3(prevalence_95CI_upper*100), "%"), ")"), NA
#     ))
#
#   subDir <- here("inst",
#                  "data",
#                  "prevalence")
#
#   if (!file.exists(subDir)) {
#     dir.create(file.path(here("inst",
#                          "data",
#                          "prevalence")
#                          ),
#                recursive = TRUE)
#   }
#
#   saveRDS(prevalence_estimates,
#           here("inst",
#                "data",
#                "prevalence",
#                "blood_cancer.rds"))
#
#   # prevalence attrition
#
#   prevalence_attrition_files<-results[stringr::str_detect(results, ".csv")]
#
#   prevalence_attrition_files<-results[stringr::str_detect(results, "prevalence_attrition")]
#
#   prevalence_attrition <- list()
#
#   for (i in seq_along(prevalence_attrition_files)) {
#
#     prevalence_attrition[[i]]<-readr::read_csv(prevalence_attrition_files[[i]],
#                                                show_col_types = FALSE)
#     }
#
#   prevalence_attrition <- dplyr::bind_rows(prevalence_attrition)
#
#   prevalence_attrition <- prepare_output(prevalence_attrition)
#
#   saveRDS(prevalence_attrition,
#           here("inst",
#                "data",
#                "bloodCancerPrevalence",
#                "prevalence_attrition.rds"))
#
# }
utils::globalVariables(c("denominator_age_group",
                         "denominator_age_group"))
