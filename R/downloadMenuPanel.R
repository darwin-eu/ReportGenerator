menuSelection <- function(selection, uploadedFiles) {
  if (selection == "Number of participants - Table") {
    menuItem(selection
             # ,
             # attritionUI(selection, uploadedFiles)
             )
  } else if (selection == "Incidence Attrition - Table") {
    menuItem(selection
             # ,
             # attritionUI(selection, uploadedFiles)
             )
  } else if (selection == "Prevalence Attrition - Table") {
    menuItem(selection
             # ,
             # attritionUI(selection, uploadedFiles)
             )
  } else if (selection == "Number of participants by sex and age group - Table") {
    menuItem(selection
             # ,
             # tableUI(selection, uploadedFiles)
             )
  } else if (selection == "Incidence rate per year - Plot") {
    menuItem(selection
             # ,
             # incidenceUI(selection, uploadedFiles)
             )
  } else if (selection == "Incidence rate per year by sex - Plot") {
    menuItem(selection
             # ,
             # incidenceUI(selection, uploadedFiles)
             )
  } else if (selection == "Incidence rate per year by age - Plot") {
    menuItem(selection
             # ,
             # incidenceUI(selection, uploadedFiles)
             )
  } else if (selection == "Prevalence per year - Plot") {
    menuItem(selection
             # ,
             # prevalenceUI(selection, uploadedFiles)
             )
  } else if (selection == "Prevalence per year by sex - Plot") {
    menuItem(selection
             # ,
             # prevalenceUI(selection, uploadedFiles)
             )
  } else if (selection == "Prevalence per year by age - Plot") {
    menuItem(selection
             # ,
             # prevalenceUI(selection, uploadedFiles)
             )
  } else if (selection == "Treatment Pathways Interactive Plots") {
    menuItem(selection
             # ,
             # patternsUI(selection, uploadedFiles)
             )
  } else if (selection == "Summarised Characteristics - Table") {
    menuItem(selection
             # ,
             # characteristicsUI("characteristics", uploadedFiles)
             )
  } else if (selection == "Summarised Large Scale Characteristics - Table") {
    menuItem(selection
             # ,
             # characteristicsUI("lsc", uploadedFiles)
             )
  } else if (selection == "Single Event - Table") {
    menuItem(selection
             # ,
             # cohortSurvivalUI("survivalTable", uploadedFiles)
             )
  } else if (selection == "Single Event - Plot") {
    menuItem(selection
             # ,
             # cohortSurvivalUI("survivalPlot", uploadedFiles)
             )
  } else if (selection == "Competing Risk - Table") {
    menuItem(selection
             # ,
             # cohortSurvivalUI("failureTable", uploadedFiles)
             )
  } else if (selection == "Competing Risk - Plot") {
    menuItem(selection
             # ,
             # cohortSurvivalUI("failurePlot", uploadedFiles)
             )
  }
}
