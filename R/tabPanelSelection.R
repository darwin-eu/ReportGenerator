tabPanelSelection <- function(selection, uploadedFiles) {
  if (selection == "Incidence Attrition") {
    tabPanel(selection, attritionUI(selection, uploadedFiles))
  } else if (selection == "Incidence Attrition") {
    tabPanel(selection, attritionIncPrevUI(selection, uploadedFiles$attrition))
  } else if (selection == "Attrition") {
    # tabPanel(selection, attritionIncPrevUI(selection, uploadedFiles))
  # } else if (selection == "Number of participants by sex and age group - Table") {
  #   tabPanel(selection, tableUI(selection, uploadedFiles))
  } else if (selection == "Incidence") {
    tabPanel(selection, incidenceMinUI(selection, uploadedFiles = uploadedFiles$incidence))
    # tabPanel(selection, incidenceSumUI(selection, uploadedFiles = uploadedFiles$incidence))
  } else if (selection == "Prevalence") {
    tabPanel(selection, prevalenceSumUI(selection, uploadedFiles = uploadedFiles$prevalence))
  } else if (selection == "Treatment Pathways Interactive Plots") {
    tabPanel(selection, patternsUI(selection, uploadedFiles))
  } else if (selection == "Summarised Characteristics") {
    tabPanel(selection, characteristicsUI("characteristics", uploadedFiles$summarised_characteristics))
  } else if (selection == "Summarised Large Scale Characteristics - Table") {
    tabPanel(selection, characteristicsUI("lsc", uploadedFiles))
  } else if (selection == "Survival - Single Event") {
    tabPanel(selection, cohortSurvivalUI("survivalTable", uploadedFiles$single_event))
  } else if (selection == "Single Event - Plot") {
    tabPanel(selection, cohortSurvivalUI("survivalPlot", uploadedFiles))
  } else if (selection == "Competing Risk - Table") {
    tabPanel(selection, cohortSurvivalUI("failureTable", uploadedFiles))
  } else if (selection == "Competing Risk - Plot") {
    tabPanel(selection, cohortSurvivalUI("failurePlot", uploadedFiles))
  }
}
