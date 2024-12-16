tabPanelSelection <- function(selection, uploadedFiles) {
  if (selection == "Incidence Attrition") {
    tabPanel(selection, attritionUI(selection, uploadedFiles$incidence_attrition))
  } else if (selection == "Prevalence Attrition") {
    tabPanel(selection, attritionUI(selection, uploadedFiles$incidence_attrition))
  } else if (selection == "Incidence") {
    tabPanel(selection, incidenceUI(selection, uploadedFiles = uploadedFiles$incidence))
  } else if (selection == "Prevalence") {
    tabPanel(selection, prevalenceUI(selection, uploadedFiles = uploadedFiles$prevalence))
  } else if (selection == "Treatment Pathways") {
    tabPanel(selection, patternsUI(selection, uploadedFiles = uploadedFiles$treatment_pathways))
  } else if (selection == "Summarised Characteristics") {
    tabPanel(selection, characteristicsUI("characteristics", uploadedFiles$summarise_characteristics))
  } else if (selection == "Summarised Large Scale Characteristics") {
    tabPanel(selection, characteristicsUI("lsc", uploadedFiles$summarise_large_scale_characteristics))
  } else if (selection == "Survival - Single Event") {
    tabPanel(selection, cohortSurvivalUI("single_event", uploadedFiles$single_event))
  } else if (selection == "Survival - Competing Risk") {
    tabPanel(selection, cohortSurvivalUI("competing_risk",  uploadedFiles$competing_risk))
  }
}
