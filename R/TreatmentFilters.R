sankeyDiagramFilters <- function(uploadedFiles) {
  tagList(
    fluidRow(
      pickerInput(
        inputId = "sexSankey",
        label = "Sex",
        choices = unique(uploadedFiles[["dataTP"]][["treatmentPathways"]]$sex),
        selected = unique(uploadedFiles[["dataTP"]][["treatmentPathways"]]$sex)[1],
        multiple = FALSE
      ),
      pickerInput(
        inputId = "ageSankey",
        label = "Age",
        choices = unique(uploadedFiles[["dataTP"]][["treatmentPathways"]]$age),
        selected = unique(uploadedFiles[["dataTP"]][["treatmentPathways"]]$age)[1],
        multiple = FALSE
      ),
      pickerInput(
        inputId = "indexSankey",
        label = "Index year",
        choices = unique(uploadedFiles[["dataTP"]][["treatmentPathways"]]$indexYear),
        selected = unique(uploadedFiles[["dataTP"]][["treatmentPathways"]]$indexYear)[1],
        multiple = FALSE
      ),
    fluidRow(
      # column(4,
      #        checkboxInput(inputId = "lockTreatmentSankey",
      #                      label = "Add data to report",
      #                      value = FALSE)
      #        ),
      column(4,
             downloadButton("downloadSankey", "Download Plot")
      ),
      )
    ),
  )
}

sunburstDiagramFilters <- function(uploadedFiles) {
  tagList(
    fluidRow(
      pickerInput(
        inputId = "sexSunburst",
        label = "Sex",
        choices = unique(uploadedFiles[["dataTP"]][["treatmentPathways"]]$sex),
        selected = unique(uploadedFiles[["dataTP"]][["treatmentPathways"]]$sex)[1],
        multiple = FALSE
      ),
      pickerInput(
        inputId = "ageSunburst",
        label = "Age",
        choices = unique(uploadedFiles[["dataTP"]][["treatmentPathways"]]$age),
        selected = unique(uploadedFiles[["dataTP"]][["treatmentPathways"]]$age)[1],
        multiple = FALSE
      ),
      pickerInput(
        inputId = "indexSunburst",
        label = "Index year",
        choices = unique(uploadedFiles[["dataTP"]][["treatmentPathways"]]$indexYear),
        selected = unique(uploadedFiles[["dataTP"]][["treatmentPathways"]]$indexYear)[1],
        multiple = FALSE
      ),
    fluidRow(
      # column(4,
      #        checkboxInput(inputId = "lockTreatmentSunburst",
      #                      label = "Add data to report",
      #                      value = FALSE)
      #        ),
      column(4,
             downloadButton("downloadSunburst", "Download Plot")
      ),
      )
    )
  )
}
