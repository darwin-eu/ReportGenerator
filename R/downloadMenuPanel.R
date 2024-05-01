menuSelection <- function(selection, uploadedFiles) {
  if (selection == "Table - Number of participants") {
    menuItem(selection
             # ,
             # attritionUI(selection, uploadedFiles)
             )
  } else if (selection == "Table - Incidence Attrition") {
    menuItem(selection
             # ,
             # attritionUI(selection, uploadedFiles)
             )
  } else if (selection == "Table - Prevalence Attrition") {
    menuItem(selection
             # ,
             # attritionUI(selection, uploadedFiles)
             )
  } else if (selection == "Table - Number of participants by sex and age group") {
    menuItem(selection
             # ,
             # tableUI(selection, uploadedFiles)
             )
  } else if (selection == "Plot - Incidence rate per year") {
    menuItem(selection
             # ,
             # incidenceUI(selection, uploadedFiles)
             )
  } else if (selection == "Plot - Incidence rate per year by sex") {
    menuItem(selection
             # ,
             # incidenceUI(selection, uploadedFiles)
             )
  } else if (selection == "Plot - Incidence rate per year by age") {
    menuItem(selection
             # ,
             # incidenceUI(selection, uploadedFiles)
             )
  } else if (selection == "Plot - Prevalence per year") {
    menuItem(selection
             # ,
             # prevalenceUI(selection, uploadedFiles)
             )
  } else if (selection == "Plot - Prevalence per year by sex") {
    menuItem(selection
             # ,
             # prevalenceUI(selection, uploadedFiles)
             )
  } else if (selection == "Plot - Prevalence per year by age") {
    menuItem(selection
             # ,
             # prevalenceUI(selection, uploadedFiles)
             )
  } else if (selection == "Treatment Pathways Interactive Plots") {
    menuItem(selection
             # ,
             # patternsUI(selection, uploadedFiles)
             )
  } else if (selection == "summarised_characteristics") {
    menuItem(selection
             # ,
             # characteristicsUI("characteristics", uploadedFiles)
             )
  } else if (selection == "Summarised Large Scale Characteristics") {
    menuItem(selection
             # ,
             # characteristicsUI("lsc", uploadedFiles)
             )
  } else if (selection == "Survival table") {
    menuItem(selection
             # ,
             # cohortSurvivalUI("survivalTable", uploadedFiles)
             )
  } else if (selection == "Survival plot") {
    menuItem(selection
             # ,
             # cohortSurvivalUI("survivalPlot", uploadedFiles)
             )
  } else if (selection == "Cumulative incidence table") {
    menuItem(selection
             # ,
             # cohortSurvivalUI("failureTable", uploadedFiles)
             )
  } else if (selection == "Cumulative incidence plot") {
    menuItem(selection
             # ,
             # cohortSurvivalUI("failurePlot", uploadedFiles)
             )
  }
}
