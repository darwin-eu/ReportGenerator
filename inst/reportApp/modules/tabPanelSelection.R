tabPanelSelection <- function(selection, uploadedFiles) {
  if (selection == "Number of participants - Table") {
    tabPanel(selection, attritionUI(selection, uploadedFiles))
  } else if (selection == "Incidence Attrition - Table") {
    tabPanel(selection, attritionUI(selection, uploadedFiles))
  } else if (selection == "Prevalence Attrition - Table") {
    tabPanel(selection, attritionUI(selection, uploadedFiles))
  } else if (selection == "Table - Number of participants by sex and age group") {
    tabPanel(selection, tableUI(selection, uploadedFiles))
  } else if (selection == "Incidence rate per year - Plot") {
    tabPanel(selection, incidenceUI(selection, uploadedFiles))
  } else if (selection == "Incidence rate per year by sex - Plot") {
    tabPanel(selection, incidenceUI(selection, uploadedFiles))
  } else if (selection == "Incidence rate per year by age - Plot") {
    tabPanel(selection, incidenceUI(selection, uploadedFiles))
  } else if (selection == "Prevalence per year - Plot") {
    tabPanel(selection, prevalenceUI(selection, uploadedFiles))
  } else if (selection == "Prevalence per year by sex - Plot") {
    tabPanel(selection, prevalenceUI(selection, uploadedFiles))
  } else if (selection == "Prevalence per year by age - Plot") {
    tabPanel(selection, prevalenceUI(selection, uploadedFiles))
  } else if (selection == "Treatment Pathways Interactive Plots") {
    tabPanel(selection, patternsUI(selection, uploadedFiles))
  } else if (selection == "Summarised Characteristics - Table") {
    tabPanel(selection, characteristicsUI("characteristics", uploadedFiles))
  } else if (selection == "Summarised Large Scale Characteristics - Table") {
    tabPanel(selection, characteristicsUI("lsc", uploadedFiles))
  } else if (selection == "Survival table") {
    tabPanel(selection, cohortSurvivalUI("survivalTable", uploadedFiles))
  } else if (selection == "Single Event - Plot") {
    tabPanel(selection, cohortSurvivalUI("survivalPlot", uploadedFiles))
  } else if (selection == "Competing Risk - Table") {
    tabPanel(selection, cohortSurvivalUI("failureTable", uploadedFiles))
  } else if (selection == "Competing Risk - Plot") {
    tabPanel(selection, cohortSurvivalUI("failurePlot", uploadedFiles))
  }
}
