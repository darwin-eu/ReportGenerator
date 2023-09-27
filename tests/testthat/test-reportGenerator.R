test_that("prevalenceAttritionCommon correct columns", {
  testServer(reportGenerator(), {
    session$setInputs(analysisIdTable1 = 1)
    uploadedFiles$dataIP$prevalence_attrition <- prevalence_attrition_test
    expect_equal(length(names(prevalenceAttritionCommon())), 25)
  })
})

test_that("incidenceAttritionCommon correct columns", {
  testServer(reportGenerator(), {
    session$setInputs(analysisIdTable1 = 1)
    uploadedFiles$dataIP$incidence_attrition <- incidence_attrition_test
    expect_equal(length(names(incidenceAttritionCommon())), 25)
  })
})

test_that("menuFun()", {
  testServer(reportGenerator(), {
    expect_equal(length(names(menuFun())), 5)
  })
})

test_that("previewTable1() FALSE", {
  testServer(reportGenerator(), {
    uploadedFiles$dataIP$prevalence_attrition <- prevalence_attrition_test
    uploadedFiles$dataIP$incidence_attrition <- incidence_attrition_test
    dataReport <- list()
    session$setInputs(analysisIdTable1 = 1)
    prevalenceAttritionCommon()
    session$setInputs(analysisIdTable1 = 1)
    incidenceAttritionCommon()
    menuFun()
    session$setInputs(lockTableNumPar = FALSE)
    expect_equal(class(output$previewTable1), "character")
  })
})

test_that("previewTable1() TRUE", {
  testServer(reportGenerator(), {
    uploadedFiles$dataIP$prevalence_attrition <- prevalence_attrition_test
    uploadedFiles$dataIP$incidence_attrition <- incidence_attrition_test
    dataReport <- list()
    session$setInputs(analysisIdTable1 = 1)
    prevalenceAttritionCommon()
    session$setInputs(analysisIdTable1 = 1)
    incidenceAttritionCommon()
    menuFun()
    session$setInputs(lockTableNumPar = TRUE)
    expect_equal(class(output$previewTable1), "character")
  })
})

test_that("previewTableSex() FALSE", {
  testServer(reportGenerator(), {
    uploadedFiles$dataIP$incidence_estimates <- incidence_estimates_test
    dataReport <- list()
    menuFun()
    session$setInputs(lockTableSex = FALSE)
    expect_equal(class(output$previewTableSex), "list")
  })
})

test_that("previewTableSex() TRUE", {
  testServer(reportGenerator(), {
    uploadedFiles$dataIP$incidence_estimates <- incidence_estimates_test
    dataReport <- list()
    menuFun()
    session$setInputs(lockTableSex = TRUE)
    expect_equal(class(output$previewTableSex), "list")
  })
})

test_that("incidenceFigure1() FALSE", {
  testServer(reportGenerator(), {
    uploadedFiles$dataIP$incidence_estimates <- incidence_estimates_test
    dataReport <- list()
    menuFun()
    session$setInputs(lockDataIncidenceYear = FALSE,
                      washoutIncidenceYear = 180,
                      daysPriorIncidenceYear = 365,
                      databaseIncidenceYear = "CHUBX",
                      outcomeIncidenceYear = "cohort_1",
                      sexIncidenceYear = "Female",
                      ageIncidenceYear = "18 to 99",
                      timeFromIncidenceYear = "2008-01-01",
                      timeToIncidenceYear = "2011-01-01",
                      intervalIncidenceYear = "years",
                      repeatedIncidenceYear = FALSE,
                      facetIncidenceYear = "Facet by database")
    expect_class(incidenceFigure1(), "ggplot")
  })
})

test_that("incidenceFigure1() TRUE", {
  testServer(reportGenerator(), {
    uploadedFiles$dataIP$incidence_estimates <- incidence_estimates_test
    dataReport <- list()
    menuFun()
    session$setInputs(lockDataIncidenceYear = TRUE,
                      washoutIncidenceYear = 180,
                      daysPriorIncidenceYear = 365,
                      databaseIncidenceYear = "CHUBX",
                      outcomeIncidenceYear = "cohort_1",
                      sexIncidenceYear = "Female",
                      ageIncidenceYear = "18 to 99",
                      timeFromIncidenceYear = "2008-01-01",
                      timeToIncidenceYear = "2011-01-01",
                      intervalIncidenceYear = "years",
                      repeatedIncidenceYear = FALSE,
                      facetIncidenceYear = "Facet by outcome")
    expect_class(incidenceFigure1(), "ggplot")
  })
})

test_that("incidenceFigure2Sex() FALSE", {
  testServer(reportGenerator(), {
    uploadedFiles$dataIP$incidence_estimates <- incidence_estimates_test
    dataReport <- list()
    menuFun()
    session$setInputs(lockDataIncidenceSex = FALSE,
                      washoutIncidenceSex = 180,
                      daysPriorIncidenceSex = 365,
                      databaseIncidenceSex = "CHUBX",
                      outcomeIncidenceSex = "cohort_1",
                      sexIncidenceSex = "Female",
                      ageIncidenceSex = "18 to 99",
                      timeFromIncidenceSex = "2008-01-01",
                      timeToIncidenceSex = "2011-01-01",
                      intervalIncidenceSex = "Years",
                      repeatedIncidenceSex = FALSE,
                      facetIncidenceSex = "Facet by database")
    expect_class(incidenceFigure2Sex(), "ggplot")
  })
})

test_that("incidenceFigure2Sex() TRUE", {
  testServer(reportGenerator(), {
    uploadedFiles$dataIP$incidence_estimates <- incidence_estimates_test
    dataReport <- list()
    menuFun()
    session$setInputs(lockDataIncidenceSex = TRUE,
                      washoutIncidenceSex = 180,
                      daysPriorIncidenceSex = 365,
                      databaseIncidenceSex = "CHUBX",
                      outcomeIncidenceSex = "cohort_1",
                      sexIncidenceSex = "Female",
                      ageIncidenceSex = "18 to 99",
                      timeFromIncidenceSex = "2008-01-01",
                      timeToIncidenceSex = "2011-01-01",
                      intervalIncidenceSex = "Years",
                      repeatedIncidenceSex = FALSE,
                      facetIncidenceSex = "Facet by outcome")
    expect_class(incidenceFigure2Sex(), "ggplot")
  })
})

test_that("incidenceFigure3Age() FALSE", {
  testServer(reportGenerator(), {
    uploadedFiles$dataIP$incidence_estimates <- incidence_estimates_test
    dataReport <- list()
    menuFun()
    session$setInputs(lockDataIncidenceAge = FALSE,
                      washoutIncidenceAge = 180,
                      daysPriorIncidenceAge = 365,
                      databaseIncidenceAge = "CHUBX",
                      outcomeIncidenceAge = "cohort_1",
                      sexIncidenceAge = "Female",
                      ageIncidenceAge = "18 to 99",
                      timeFromIncidenceAge = "2008-01-01",
                      timeToIncidenceAge = "2011-01-01",
                      intervalIncidenceAge = "Years",
                      repeatedIncidenceAge = FALSE,
                      facetIncidenceAge = "Facet by database")
    expect_class(incidenceFigure3Age(), "ggplot")
  })
})

test_that("incidenceFigure3Age() TRUE", {
  testServer(reportGenerator(), {
    uploadedFiles$dataIP$incidence_estimates <- incidence_estimates_test
    dataReport <- list()
    menuFun()
    session$setInputs(lockDataIncidenceAge = TRUE,
                      washoutIncidenceAge = 180,
                      daysPriorIncidenceAge = 365,
                      databaseIncidenceAge = "CHUBX",
                      outcomeIncidenceAge = "cohort_1",
                      sexIncidenceAge = "Female",
                      ageIncidenceAge = "18 to 99",
                      timeFromIncidenceAge = "2008-01-01",
                      timeToIncidenceAge = "2011-01-01",
                      intervalIncidenceAge = "Years",
                      repeatedIncidenceAge = FALSE,
                      facetIncidenceAge = "Facet by outcome")
    expect_class(incidenceFigure3Age(), "ggplot")
  })
})

test_that("prevalenceFigure4() FALSE", {
  testServer(reportGenerator(), {
    uploadedFiles$dataIP$prevalence_estimates <- prevalence_estimates_test
    dataReport <- list()
    menuFun()
    session$setInputs(lockDataPrevalenceYear = FALSE,
                      databasePrevalenceYear = "CHUBX",
                      outcomePrevalenceYear = "cohort_1",
                      sexPrevalenceYear = "Female",
                      agePrevalenceYear = "18 to 99",
                      timeFromPrevalenceYear = "2008-01-01",
                      timeToPrevalenceYear = "2011-01-01",
                      intervalPrevalenceYear = "Years",
                      typePrevalenceYear = "period",
                      facetPrevalenceYear = "Facet by database")
    expect_class(prevalenceFigure4(), "ggplot")
  })
})

test_that("prevalenceFigure4() TRUE", {
  testServer(reportGenerator(), {
    uploadedFiles$dataIP$prevalence_estimates <- prevalence_estimates_test
    dataReport <- list()
    menuFun()
    session$setInputs(lockDataPrevalenceYear = TRUE,
                      databasePrevalenceYear = "CHUBX",
                      outcomePrevalenceYear = "cohort_1",
                      sexPrevalenceYear = "Female",
                      agePrevalenceYear = "18 to 99",
                      timeFromPrevalenceYear = "2008-01-01",
                      timeToPrevalenceYear = "2011-01-01",
                      intervalPrevalenceYear = "Years",
                      typePrevalenceYear = "point",
                      facetPrevalenceYear = "Facet by outcome")
    expect_class(prevalenceFigure4(), "ggplot")
  })
})

test_that("prevalenceFigure5() FALSE", {
  testServer(reportGenerator(), {
    uploadedFiles$dataIP$prevalence_estimates <- prevalence_estimates_test
    dataReport <- list()
    menuFun()
    session$setInputs(lockDataPrevalenceSex = FALSE,
                      databasePrevalenceSex = "CHUBX",
                      outcomePrevalenceSex = "cohort_1",
                      sexPrevalenceSex = "Female",
                      agePrevalenceSex = "18 to 99",
                      timeFromPrevalenceSex = "2008-01-01",
                      timeToPrevalenceSex = "2011-01-01",
                      intervalPrevalenceSex = "Years",
                      typePrevalenceSex = "period",
                      facetPrevalenceSex = "Facet by database")
    expect_class(prevalenceFigure5(), "ggplot")
  })
})

test_that("prevalenceFigure5() TRUE", {
  testServer(reportGenerator(), {
    uploadedFiles$dataIP$prevalence_estimates <- prevalence_estimates_test
    dataReport <- list()
    menuFun()
    session$setInputs(lockDataPrevalenceSex = TRUE,
                      databasePrevalenceSex = "CHUBX",
                      outcomePrevalenceSex = "cohort_1",
                      sexPrevalenceSex = "Female",
                      agePrevalenceSex = "18 to 99",
                      timeFromPrevalenceSex = "2008-01-01",
                      timeToPrevalenceSex = "2011-01-01",
                      intervalPrevalenceSex = "Years",
                      typePrevalenceSex = "point",
                      facetPrevalenceSex = "Facet by outcome")
    expect_class(prevalenceFigure5(), "ggplot")
  })
})

test_that("prevalenceFigure6() FALSE", {
  testServer(reportGenerator(), {
    uploadedFiles$dataIP$prevalence_estimates <- prevalence_estimates_test
    dataReport <- list()
    menuFun()
    session$setInputs(lockDataPrevalenceAge = FALSE,
                      databasePrevalenceAge = "CHUBX",
                      outcomePrevalenceAge = "cohort_1",
                      sexPrevalenceAge = "Female",
                      agePrevalenceAge = "18 to 99",
                      timeFromPrevalenceAge = "2008-01-01",
                      timeToPrevalenceAge = "2011-01-01",
                      intervalPrevalenceAge = "Years",
                      typePrevalenceAge = "period",
                      facetPrevalenceAge = "Facet by database")
    expect_class(prevalenceFigure6(), "ggplot")
  })
})

test_that("prevalenceFigure6() TRUE", {
  testServer(reportGenerator(), {
    uploadedFiles$dataIP$prevalence_estimates <- prevalence_estimates_test
    dataReport <- list()
    menuFun()
    session$setInputs(lockDataPrevalenceAge = TRUE,
                      databasePrevalenceAge = "CHUBX",
                      outcomePrevalenceAge = "cohort_1",
                      sexPrevalenceAge = "Female",
                      agePrevalenceAge = "18 to 99",
                      timeFromPrevalenceAge = "2008-01-01",
                      timeToPrevalenceAge = "2011-01-01",
                      intervalPrevalenceAge = "Years",
                      typePrevalenceAge = "point",
                      facetPrevalenceAge = "Facet by outcome")
    expect_class(prevalenceFigure6(), "ggplot")
  })
})

test_that("previewOutburstPlot renderUI FALSE", {
  testServer(reportGenerator(), {
    uploadedFiles <- list()
    uploadedFiles[["dataTP"]][["treatmentPathways"]] <- treatmentPathways_test
    dataReport <- list()
    menuFun()
    session$setInputs(lockTreatmentOutburst = FALSE)

  #   objectChoice <- "Sunburst Plot - TreatmentPatterns"
  #   outputDirOutburst <- file.path(tempdir(), "outputDirOutburst")
  #   dir.create(outputDirOutburst)
  # 	outputFile <- file.path(outputDirOutburst, "outburstDiagram.html")
  #   uploadedFiles[["dataTP"]][["outputFile"]] <- outputFile
  #   uploadedFiles[["dataTP"]][["returnHTML"]] <- TRUE
  #   object <- eval(parse(text = menuFun %>%
  #                          dplyr::filter(title == objectChoice) %>%
  #                          dplyr::pull(signature)), envir = uploadedFiles[["dataTP"]])
  #   sunburst <- object$sunburst
  #   sunburstPlot <- HTML(sunburst)

    # print(output$previewOutburstPlot)
    expect_error(output$previewOutburstPlot, "object 'treatmentPathways' not found")
  })
})



