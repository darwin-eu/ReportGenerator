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
             actionButton("lockTableSex", "Add item to report")
             # lockItemsUI("lockTableSex")
      ),
    )
  )
}




