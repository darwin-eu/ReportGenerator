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

tableSexFilters <- function(uploadedFiles) {
  tagList(
    # fluidRow(
    #   column(4,
    #          selectInput(inputId = "analysisIdTable1",
    #                      label = "Analysis ID",
    #                      choices = unique(uploadedFiles$dataIP$incidence_estimates$analysis_id))
    #   ),
    #   column(8,
    #          textAreaInput("captionTable1",
    #                        "Caption",
    #                        table1aAutText(uploadedFiles$dataIP$incidence_estimates,
    #                                       uploadedFiles$dataIP$prevalence_estimates),
    #                        height = "130px")
    #   )
    # ),
    fluidRow(
      column(4,
             checkboxInput(inputId = "lockTableSex",
                           label = "Add data to report",
                           value = FALSE)
      ),
    )
  )
}




