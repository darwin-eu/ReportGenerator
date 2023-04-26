prevalence_attrition_CPRD_GOLD <- read_csv("~/darwin-docs/darwinReport/bloodCancerReporting/networkResults/prevalenceAttrition/prevalence_attrition_CPRD GOLD.csv")
prevalence_attrition_IPCI <- read_csv("~/darwin-docs/darwinReport/bloodCancerReporting/networkResults/prevalenceAttrition/prevalence_attrition_IPCI.csv")
prevalence_attrition_SIDIAP <- read_csv("~/darwin-docs/darwinReport/bloodCancerReporting/networkResults/prevalenceAttrition/prevalence_attrition_SIDIAP.csv")

table1Prevalence <- function(prevalence_attrition) {

if (!("reason_id" %in% names(prevalence_attrition))) {

  prevalence_attrition <- prevalence_attrition %>%
    mutate(reason_id = case_when(reason == "Starting population"  ~ 1,
                                 reason == "Missing year of birth"  ~ 2,
                                 reason == "Missing sex"  ~ 3,
                                 reason == "Cannot satisfy age criteria during the study period based on year of birth"  ~ 4,
                                 reason == "No observation time available during study period"  ~ 5,
                                 reason == "Doesn't satisfy age criteria during the study period"  ~ 6,
                                 reason == "Prior history requirement not fullfilled during study period"  ~ 7,
                                 reason == "Not Female"  ~ 8,
                                 reason == "Not Male"  ~ 8,
                                 reason == "No observation time available after applying age and prior history criteria"  ~ 10,
                                 reason == "Starting analysis population" ~ 11,
                                 reason == "Not observed during the complete database interval"  ~ 14,
                                 reason == "Do not satisfy full contribution requirement for an interval"  ~ 16,
                                 reason == "Excluded due to prior event (do not pass outcome washout during study period)" ~ 12),
           number_subjects = current_n,
           excluded_subjects = excluded)


  # unique(prevalence_attrition$reason)
}

tablePrevalenceAtt <- prevalence_attrition %>%
  group_by(reason_id,
           reason) %>%
  summarise(current_n = round(mean(number_subjects ), 0),
            excluded = round(mean(excluded_subjects ), 0)) %>%
  mutate(analysis_step = case_when(between(reason_id, 1, 8) ~ "initial",
                                   between(reason_id, 10, 16) ~ "prevalence"))


tablePrevalenceAtt <- tablePrevalenceAtt[,-1]

tablePrevalenceAtt <- tablePrevalenceAtt %>%
  select(analysis_step, everything())

return(tablePrevalenceAtt)

}



table1Prevalence(prevalence_attrition_CPRD_GOLD)

table1Prevalence(prevalence_attrition_IPCI)

table1Prevalence(prevalence_attrition_SIDIAP)
