tabPanelSelection <- function(selection, uploadedFiles, version) {
  if (selection == "Table - Number of participants") {
    tabPanel(selection, tableNumParFilters(uploadedFiles), tableOutput("previewTable1"))
  } else if (selection == "Table - Incidence Attrition") {
    tabPanel(selection, tableAttIncFilters(uploadedFiles), tableOutput("previewTableAttInc"))
  } else if (selection == "Table - Prevalence Attrition") {
    tabPanel(selection, tableAttPrevFilters(uploadedFiles), tableOutput("previewTableAttPrev"))
  } else if (selection == "Table - Number of participants by sex and age group") {
    tabPanel(selection, tableSexFilters(uploadedFiles), gt_output("previewTableSex"))
  } else if (selection == "Plot - Incidence rate per year") {
    tabPanel(selection, incPlotByYearFilters(uploadedFiles, selection), plotOutput("previewFigure1"))
  } else if (selection == "Plot - Incidence rate per year by sex") {
    tabPanel(selection, incPlotSexFilters(uploadedFiles, selection), plotOutput("previewFigure2"))
  } else if (selection == "Plot - Incidence rate per year by age") {
    tabPanel(selection, incPlotAgeFilters(uploadedFiles, selection), plotOutput("previewFigure3"))
  } else if (selection == "Plot - Prevalence rate per year") {
    tabPanel(selection, prevPlotByYearFilters(uploadedFiles, selection), plotOutput("previewFigure4"))
  } else if (selection == "Plot - Prevalence rate per year by sex") {
    tabPanel(selection, prevPlotSexFilters(uploadedFiles, selection), plotOutput("previewFigure5"))
  } else if (selection == "Plot - Prevalence rate per year by age") {
    tabPanel(selection, prevPlotAgeFilters(uploadedFiles, selection), plotOutput("previewFigure6"))
  } else if (selection == "Sankey Diagram - TreatmentPatterns") {
    tabPanel(selection, sankeyDiagramFilters(uploadedFiles, version), htmlOutput("previewSankeyDiagram"))
  } else if (selection == "Sunburst Plot - TreatmentPatterns") {
    tabPanel(selection, sunburstDiagramFilters(uploadedFiles, version), htmlOutput("previewSunburstPlot"))
  } else if (selection == "Summary Characteristics") {
    tabPanel(selection, characteristicsUI("charac", uploadedFiles$dataPP$`Summary characteristics`))
  } else if (selection == "Summarised Large Scale Characteristics") {
    tabPanel(selection, characteristicsUI("lsc", uploadedFiles$dataPP$`Summarised Large Scale Characteristics`))
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
