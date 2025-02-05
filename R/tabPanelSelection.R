tabPanelSelection <- function(selection, uploadedFiles) {
  if (selection == "Incidence") {
    tabPanel(selection, incidenceUI(selection, uploadedFiles = uploadedFiles$incidence, uploadedFilesAttrition = uploadedFiles$incidence_attrition))
  } else if (selection == "Prevalence") {
    tabPanel(selection, prevalenceUI(selection, uploadedFiles = uploadedFiles$prevalence, uploadedFilesAttrition = uploadedFiles$prevalence_attrition))
  } else if (selection == "Treatment Pathways") {
    tabPanel(selection, pathwaysUI(selection, uploadedFiles = uploadedFiles$treatment_pathways))
  } else if (selection == "Summarised Characteristics") {
    tabPanel(selection, characteristicsUI("characteristics", uploadedFiles$summarise_characteristics))
  } else if (selection == "Summarised Large Scale Characteristics") {
    tabPanel(selection, largeScaleUI("lsc", uploadedFiles$summarise_large_scale_characteristics))
  } else if (selection == "Survival - Single Event") {
    tabPanel(selection, cohortSurvivalUI("single_event", uploadedFiles$single_event))
  } else if (selection == "Survival - Competing Risk") {
    tabPanel(selection, cohortSurvivalUI("competing_risk",  uploadedFiles$competing_risk))
  }
}
