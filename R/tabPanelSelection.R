tabPanelSelection <- function(selection, uploadedFiles, version) {
  if (selection == "Table - Number of participants") {
    tabPanel(selection, attritionUI(selection, uploadedFiles))
  } else if (selection == "Table - Incidence Attrition") {
    tabPanel(selection, attritionUI(selection, uploadedFiles))
  } else if (selection == "Table - Prevalence Attrition") {
    tabPanel(selection, attritionUI(selection, uploadedFiles))
  } else if (selection == "Table - Number of participants by sex and age group") {
    tabPanel(selection, tableUI(selection, uploadedFiles))
  } else if (selection == "Plot - Incidence rate per year") {
    tabPanel(selection, incidenceUI(selection, uploadedFiles))
  } else if (selection == "Plot - Incidence rate per year by sex") {
    tabPanel(selection, incidenceUI(selection, uploadedFiles))
  } else if (selection == "Plot - Incidence rate per year by age") {
    tabPanel(selection, incidenceUI(selection, uploadedFiles))
  } else if (selection == "Plot - Prevalence per year") {
    tabPanel(selection, prevalenceUI(selection, uploadedFiles))
  } else if (selection == "Plot - Prevalence per year by sex") {
    tabPanel(selection, prevalenceUI(selection, uploadedFiles))
  } else if (selection == "Plot - Prevalence per year by age") {
    tabPanel(selection, prevalenceUI(selection, uploadedFiles))
  } else if (selection == "Sankey Diagram - TreatmentPatterns") {
    tabPanel(selection, sankeyDiagramFilters(uploadedFiles, version), htmlOutput("previewSankeyDiagram"))
  } else if (selection == "Sunburst Plot - TreatmentPatterns") {
    tabPanel(selection, sunburstDiagramFilters(uploadedFiles, version), htmlOutput("previewSunburstPlot"))
  } else if (selection == "Summarised Characteristics") {
    tabPanel(selection, characteristicsUI("characteristics", uploadedFiles))
  } else if (selection == "Summarised Large Scale Characteristics") {
    tabPanel(selection, characteristicsUI("lsc", uploadedFiles))
  } else if (selection == "Survival table") {
    tabPanel(selection, cohortSurvivalUI("survivalTable", uploadedFiles$dataCS$`Survival estimate`))
  } else if (selection == "Survival plot") {
    tabPanel(selection, cohortSurvivalUI("survivalPlot", uploadedFiles$dataCS$`Survival estimate`))
  } else if (selection == "Cumulative incidence table") {
    tabPanel(selection, cohortSurvivalUI("failureTable", uploadedFiles$dataCS$`Survival cumulative incidence`))
  } else if (selection == "Cumulative incidence plot") {
    tabPanel(selection, cohortSurvivalUI("failurePlot", uploadedFiles$dataCS$`Survival cumulative incidence`))
  }
}
