sankeyDiagramFilters <- function(uploadedFiles) {
  tagList(
    # fluidRow(
    #   column(4,
    #          selectInput(inputId = "analysisIdTable1",
    #                      label = "Analysis ID",
    #                      choices = unique(uploadedFiles$dataTP$incidence_attrition$analysis_id))
    #   ),
    #   column(8,
    #          textAreaInput("captionTable1",
    #                        "Caption",
    #                        table1aAutText(uploadedFiles$dataTP$incidence_attrition,
    #                                       uploadedFiles$dataTP$prevalence_attrition),
    #                        height = "130px")
    #   )
    # ),
    fluidRow(
      column(4,
             checkboxInput(inputId = "lockTreatmentSankey",
                           label = "Add data to report",
                           value = FALSE)
      )
    )
  )
}

outburstDiagramFilters <- function(uploadedFiles) {
  tagList(
    # fluidRow(
    #   column(4,
    #          selectInput(inputId = "analysisIdTable1",
    #                      label = "Analysis ID",
    #                      choices = unique(uploadedFiles$dataTP$incidence_attrition$analysis_id))
    #   ),
    #   column(8,
    #          textAreaInput("captionTable1",
    #                        "Caption",
    #                        table1aAutText(uploadedFiles$dataTP$incidence_attrition,
    #                                       uploadedFiles$dataTP$prevalence_attrition),
    #                        height = "130px")
    #   )
    # ),
    fluidRow(
      column(4,
             checkboxInput(inputId = "lockTreatmentOutburst",
                           label = "Add data to report",
                           value = FALSE)
      )
    )
  )
}
