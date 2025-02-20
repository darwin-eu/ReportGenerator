tabPanelSelection <- function(selection, uploaded_files) {
  if (selection == "Incidence") {
    tabPanel(selection, incidencePrevalenceUI(selection, uploaded_files$incidence, uploaded_files$incidence_attrition))
  } else if (selection == "Prevalence") {
    tabPanel(selection, incidencePrevalenceUI(selection, uploaded_files$prevalence, uploaded_files$prevalence_attrition))
  } else if (selection == "Treatment Pathways") {
    tabPanel(selection, pathwaysUI(selection, uploaded_files$treatment_pathways))
  } else if (selection == "Summarised Characteristics") {
    tabPanel(selection, characteristicsUI("characteristics", uploaded_files$summarise_characteristics))
  } else if (selection == "Summarised Large Scale Characteristics") {
    tabPanel(selection, largeScaleUI("lsc", uploaded_files$summarise_large_scale_characteristics))
  } else if (selection == "Survival - Single Event") {
    tabPanel(selection, cohortSurvivalUI("single_event", uploaded_files$single_event))
  } else if (selection == "Survival - Competing Risk") {
    tabPanel(selection, cohortSurvivalUI("competing_risk", uploaded_files$competing_risk))
  }
}
