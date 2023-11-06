tableNumParFilters <- function(uploadedFiles) {
  tagList(
    fluidRow(
      column(4,
             selectInput(inputId = "analysisIdTable1",
                         label = "Analysis ID",
                         choices = unique(uploadedFiles$dataIP$incidence_attrition$analysis_id))
      ),
      column(8,
             textAreaInput("captionTable1",
                           "Caption",
                           table1aAutText(uploadedFiles$dataIP$incidence_attrition,
                                          uploadedFiles$dataIP$prevalence_attrition),
                           width = '100%',
                           height = "130px")
      )
    ),
    fluidRow(
      column(4,
      checkboxInput(inputId = "lockTableNumPar",
                    label = "Add data to report",
                    value = FALSE)
      )
    )
  )
}

tableAttIncFilters <- function(uploadedFiles) {
  tagList(
    fluidRow(
      column(4,
             selectInput(inputId = "analysisIdTable1",
                         label = "Analysis ID",
                         choices = unique(uploadedFiles$dataIP$incidence_attrition$analysis_id))
      ),
      column(8,
             textAreaInput("captionTableInc",
                           "Caption",
                           tableAttrition(uploadedFiles$dataIP$incidence_attrition),
                           width = '100%',
                           height = "130px")
      )
    ),
    fluidRow(
      column(4,
             checkboxInput(inputId = "lockTableIncAtt",
                           label = "Add data to report",
                           value = FALSE)
      )
    )
  )
}

tableAttPrevFilters <- function(uploadedFiles) {
  tagList(
    fluidRow(
      column(4,
             selectInput(inputId = "analysisIdTable1",
                         label = "Analysis ID",
                         choices = unique(uploadedFiles$dataIP$prevalence_attrition$analysis_id))
      ),
      column(8,
             textAreaInput("captionTablePrev",
                           "Caption",
                           tableAttrition(uploadedFiles$dataIP$prevalence_attrition),
                           width = '100%',
                           height = "130px")
      )
    ),
    fluidRow(
      column(4,
             checkboxInput(inputId = "lockTablePrevAtt",
                           label = "Add data to report",
                           value = FALSE)
      )
    )
  )
}

tableSexFilters <- function(uploadedFiles) {
  tagList(
    fluidRow(
      column(8,
             textAreaInput("captionTableSexAge",
                           "Caption",
                           "Table 1. Displays the total number of drug users, for each of the databases, during the study period. Total number of drug A users ranged from XXX to XXX across databases. Total number of drug B users ranged from XXX to XXX across databases. [continue for each drug]. When stratified by sex, number of male drug A users ranged from XXX to XXX  across databases, whereas number of female drug A users ranged from XXX to XXX across databases. [continue for each drug]. When stratified by age, number of drug A users in age group 1 ranged from XXX to XXX across databases, whereas number of drug A users in age group 2 ranged from XXX to XXX across databases. [continue for each age group and each drug ]. In summary, there were more users of [insert drug with highest count]; and least users of [insert drug with lowest count]. [Describe other observed patterns in the data].",
                           width = '100%',
                           height = "130px")
      )
    ),
    fluidRow(
      column(4,
             checkboxInput(inputId = "lockTableSex",
                           label = "Add data to report",
                           value = FALSE)
      ),
    )
  )
}




