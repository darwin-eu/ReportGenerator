test_that("Incidence rate per year module", {
  testServer(reportGenerator(), {
    uploadedFiles <- list()
    uploadedFiles[["dataIP"]][["incidence_estimates"]] <- testData$incidence_estimates
    selection <- "Plot - Incidence rate per year"
    expect_s3_class(incidenceUI(selection, uploadedFiles), "shiny.tag.list")
    dataIncidenceYear <- incidenceServer(id = "Plot - Incidence rate per year",
                                         reactive(testData$incidence_estimates))
    expect_s3_class(dataIncidenceYear, "reactiveVal")
  })
})

test_that("Incidence rate per year by sex module", {
  testServer(reportGenerator(), {
    uploadedFiles <- list()
    uploadedFiles[["dataIP"]][["incidence_estimates"]] <- testData$incidence_estimates
    selection <- "Plot - Incidence rate per year by sex"
    expect_s3_class(incidenceUI(selection, uploadedFiles), "shiny.tag.list")
    dataIncidenceSex <- incidenceServer(id = "Plot - Incidence rate per year by sex",
                                        reactive(testData$incidence_estimates))
    expect_s3_class(dataIncidenceSex, "reactiveVal")
  })
})

test_that("Incidence rate per year by age module", {
  testServer(reportGenerator(), {
    uploadedFiles <- list()
    uploadedFiles[["dataIP"]][["incidence_estimates"]] <- testData$incidence_estimates
    selection <- "Plot - Incidence rate per year by age"
    expect_s3_class(incidenceUI(selection, uploadedFiles), "shiny.tag.list")
    dataIncidenceAge <- incidenceServer(id = "Plot - Incidence rate per year by age",
                                        reactive(testData$incidence_estimates))
    expect_s3_class(dataIncidenceAge, "reactiveVal")
  })
})
