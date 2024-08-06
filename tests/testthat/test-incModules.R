test_that("Incidence rate per year module", {
  testServer(reportGenerator(), {
    uploadedFiles <- list()
    uploadedFiles[["IncidencePrevalence"]][["incidence_estimates"]] <- testData$incidence_estimates
    selection <- "Incidence rate per year - Plot"
    expect_s3_class(incidenceUI(selection, uploadedFiles), "shiny.tag.list")
    dataIncidenceYear <- incidenceServer(id = "Incidence rate per year - Plot",
                                         reactive(testData$incidence_estimates))
    expect_s3_class(dataIncidenceYear, "reactiveVal")
  })
})

test_that("Incidence rate per year by sex module", {
  testServer(reportGenerator(), {
    uploadedFiles <- list()
    uploadedFiles[["IncidencePrevalence"]][["incidence_estimates"]] <- testData$incidence_estimates
    selection <- "Incidence rate per year by sex - Plot"
    expect_s3_class(incidenceUI(selection, uploadedFiles), "shiny.tag.list")
    dataIncidenceSex <- incidenceServer(id = "Incidence rate per year by sex - Plot",
                                        reactive(testData$incidence_estimates))
    expect_s3_class(dataIncidenceSex, "reactiveVal")
  })
})

test_that("Incidence rate per year by age module", {
  testServer(reportGenerator(), {
    uploadedFiles <- list()
    uploadedFiles[["IncidencePrevalence"]][["incidence_estimates"]] <- testData$incidence_estimates
    selection <- "Incidence rate per year by age - Plot"
    expect_s3_class(incidenceUI(selection, uploadedFiles), "shiny.tag.list")
    dataIncidenceAge <- incidenceServer(id = "Incidence rate per year by age - Plot",
                                        reactive(testData$incidence_estimates))
    expect_s3_class(dataIncidenceAge, "reactiveVal")
  })
})
