#' createLegend
#'
#' Write html file for the sunburst plot legend.
#'
#' @param fileName Output file name for the legend.
#' @param inputJSON JSON-data to create legend
#'
#' @returns NULL
createLegend <- function(inputJSON, fileName) {
  # Load template HTML file
  html <- paste(
    readLines(file.path(
      system.file(package = "ReportGenerator"),
      "TreatmentPatterns",
      "legend.html"
    )),
    collapse = "\n"
  )

  html <- sub("@insert_data", inputJSON, html)

  # Save HTML file as sunburst_@studyName
  writeLines(
    text = html,
    con = fileName)
}
