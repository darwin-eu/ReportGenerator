test_that("Prevalence per year module", {
  testServer(reportGenerator(), {
    uploadedFiles <- list()
    uploadedFiles[["IncidencePrevalence"]][["prevalence_estimates"]] <- testData$prevalence_estimates
    selection <- "Prevalence per year - Plot"
    expect_s3_class(prevalenceUI(selection, uploadedFiles), "shiny.tag.list")
    dataPrevalenceYear <- prevalenceServer(id = selection, reactive(uploadedFiles))
    expect_s3_class(dataPrevalenceYear, "reactiveVal")
  })
})

test_that("Prevalence per year by sex module", {
  testServer(reportGenerator(), {
    uploadedFiles <- list()
    uploadedFiles[["IncidencePrevalence"]][["prevalence_estimates"]] <- testData$prevalence_estimates
    selection <- "Prevalence per year by sex - Plot"
    expect_s3_class(prevalenceUI(selection, uploadedFiles), "shiny.tag.list")
    dataPrevalenceSex <- prevalenceServer(id = "Prevalence per year by sex - Plot",
                                          reactive(uploadedFiles))
    expect_s3_class(dataPrevalenceSex, "reactiveVal")
  })
})

test_that("Prevalence per year by age module", {
  testServer(reportGenerator(), {
    uploadedFiles <- list()
    uploadedFiles[["IncidencePrevalence"]][["prevalence_estimates"]] <- testData$prevalence_estimates
    selection <- "Prevalence per year by sex - Plot"
    expect_s3_class(prevalenceUI(selection, uploadedFiles), "shiny.tag.list")
    dataPrevalenceAge <- prevalenceServer(id = "Prevalence per year by age - Plot",
                                          reactive(uploadedFiles))
    expect_s3_class(dataPrevalenceAge, "reactiveVal")
  })
})
