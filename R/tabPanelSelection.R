tabPanelSelection <- function(selection, uploadedFiles, menuFun) {
  if (selection == "Table - Number of participants") {
    tabPanel(selection, tableNumParFilters(uploadedFiles), tableOutput("previewTable1"))
  } else if (selection == "Table - Number of participants by sex and age group") {
    tabPanel(selection, tableSexFilters(uploadedFiles), gt_output("previewTableSex"))
  } else if (selection == "Plot - Incidence rate per year") {
    tabPanel(selection, incPlotByYearFilters(uploadedFiles, menuFun, selection), plotOutput("previewFigure1"))
  } else if (selection == "Plot - Incidence rate per year by sex") {
    tabPanel(selection, incPlotSexFilters(uploadedFiles, menuFun, selection), plotOutput("previewFigure2"))
  } else if (selection == "Plot - Incidence rate per year by age") {
    tabPanel(selection, incPlotAgeFilters(uploadedFiles, menuFun, selection), plotOutput("previewFigure3"))
  } else if (selection == "Plot - Prevalence rate per year") {
    tabPanel(selection, prevPlotByYearFilters(uploadedFiles, menuFun, selection), plotOutput("previewFigure4"))
  } else if (selection == "Plot - Prevalence rate per year by sex") {
    tabPanel(selection, prevPlotSexFilters(uploadedFiles, menuFun, selection), plotOutput("previewFigure5"))
  } else if (selection == "Plot - Prevalence rate per year by age") {
    tabPanel(selection, prevPlotAgeFilters(uploadedFiles, menuFun, selection), plotOutput("previewFigure6"))
  } else if (selection == "Sankey Diagram - TreatmentPatterns") {
    tabPanel(selection, sankeyDiagramFilters(uploadedFiles), htmlOutput("previewSankeyDiagram"))
  } else if (selection == "Sunburst Plot - TreatmentPatterns") {
    tabPanel(selection, outburstDiagramFilters(uploadedFiles), htmlOutput("previewOutburstPlot"))
  }
}
