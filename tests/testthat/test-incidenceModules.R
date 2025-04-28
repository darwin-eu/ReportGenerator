test_that("Pedriatic patients", {
  # fileDataPath <- list.files(test_path("studies", "summarised_zip"), pattern = "zip", full.names = TRUE)
  fileDataPath <- "C:\\Users\\cbarboza\\Documents\\darwin-docs\\packages\\darwin-dev\\ReportGenerator\\results\\meeting\\erasmus\\HIV\\Results_TestData_P3C1020ARTPaediatricHIV_20250409.zip"
  uploaded_files <- joinDatabases(fileDataPath = fileDataPath)
  uploadedFilesIncidence <- uploaded_files$summarised_result %>%
    visOmopResults::filterSettings(result_type == "incidence")
  uploadedFilesIncidenceAttrition <- uploaded_files$summarised_result %>%
    visOmopResults::filterSettings(result_type == "incidence_atttrition")


  # uploadedFilesIncidence %>% settings() %>% View()
  # uploadedFilesIncidence %>% View()
  uploadedFilesIncidence %>% omopgenerics::filterAdditional(analysis_interval == "years")
  omopgenerics::additionalColumns(uploadedFilesIncidence)
  omopgenerics::splitAdditional(uploadedFilesIncidence)
  tableIncidence(result = uploadedFilesIncidence,
                 header = c("cdm_name"),
                 groupColumn = NULL,
                 settingsColumn = NULL,
                 hide = NULL,
                 .options = list(style = getDarwinStyle(),
                                 caption = NULL)
  )


  expect_equal(length(uploaded_files), 2)
  expect_type(uploaded_files, "list")
})

test_that("tableIncidenceStyle", {

  reportItemsPath <- testthat::test_path("studies", "generate_report_test", "reportI_items_test_incidence_table_plot.rds")
  reportItems <- read_rds(reportItemsPath)
  dataReportList <- reportItems$reportItems
  incidence_data <- dataReportList$Vdmj$incidence_table$result

  expect_no_error(IncidencePrevalence::tableIncidence(result = incidence_data,
                                                      .options = list(style = getDarwinStyle(),
                                                                      caption = NULL)
  ))

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
