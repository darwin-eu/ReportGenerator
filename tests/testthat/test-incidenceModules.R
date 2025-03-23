test_that("tableIncidenceStyle", {

  reportItemsPath <- testthat::test_path("studies", "generate_report_test", "reportI_items_test_incidence_table_plot.rds")
  reportItems <- read_rds(reportItemsPath)
  dataReportList <- reportItems$reportItems
  incidence_data <- dataReportList$nqhz$incidence_table$result
  tableIncidence(incidence_data,
                  .options = list(style = list(
                    "header" = list(gt::cell_text(weight = "bold"),
                                    gt::cell_fill(color = "orange"))),
                                  caption = "Hello World!")
                  )

  visOmopResults::tableOptions()


})

test_that("Incidence module works", {
  testServer(reportGenerator(), {
    uploaded_files <- testData$IncidencePrevalence$incidence_estimates
    uploaded_files_attrition <- testData$IncidencePrevalence$incidence_estimates %>%
      visOmopResults::filterSettings(result_type == "incidence_attrition")
    selection <- "Incidence"
    expect_s3_class(incidencePrevalenceUI(selection, uploaded_files, uploaded_files_attrition), "shiny.tag.list")
  })
})
