testData <- testData()

incidence_attrition_test <- testData$incidence_attrition
incidence_estimates_test <- testData$incidence_estimates
prevalence_attrition_test <- testData$prevalence_attrition
prevalence_estimates_test <- testData$prevalence_estimates
treatmentPathways_test <- testData$treatmentPathways_test
summarisedCharacteristics <- testData$`Summarised Characteristics`
summarisedCharacteristicsLSC <- testData$`Summarised Large Scale Characteristics`

test_that("datasetLoad IncPrev", {
  testServer(reportGenerator(), {
    session$setInputs(datasetLoad = data.frame(name = c("mock_data_ReportGenerator_CHUBX.zip",
                                                        "mock_data_ReportGenerator_CHUBX.zip",
                                                        "mock_data_ReportGenerator_IMASIS.zip"),
                                               datapath = c(test_path("IncPrev", "0.5.1", "zip", "mock_data_ReportGenerator_CHUBX.zip"),
                                                            test_path("IncPrev", "0.5.1", "zip", "mock_data_ReportGenerator_CPRD_GOLD.zip"),
                                                            test_path("IncPrev", "0.5.1", "zip", "mock_data_ReportGenerator_IMASIS.zip"))),
                      dataVersion = "0.5.1")

    expect_equal(length(uploadedFiles$dataIP), 4)
  })
})

test_that("datasetLoad IncPrev 0.6.0", {
  testServer(reportGenerator(), {
    session$setInputs(datasetLoad = data.frame(name = c("mock_data_ReportGenerator_CHUBX.zip"),
                                               datapath = c(test_path("IncPrev", "0.6.0", "zip", "mock_data_ReportGenerator_CHUBX.zip"))),
                      dataVersion = "0.6.0")

    expect_equal(length(uploadedFiles$dataIP), 4)
  })
})

test_that("datasetLoad IncPrev Wrong Data", {
  testServer(reportGenerator(), {
    session$setInputs(datasetLoad = data.frame(name = c("CHUBX.zip"),
                                               datapath = c(test_path("TrePat", "2.5.2", "zip", "CHUBX.zip"),
                                                            test_path("TrePat", "2.5.2", "zip", "CPRD.zip"),
                                                            test_path("TrePat", "2.5.2", "zip", "IQVIA.zip"))),
                      dataVersion = "0.6.0")

    expect_equal(length(uploadedFiles$dataIP), 0)
  })
})

test_that("datasetLoad IncPrev Wrong Data 0.6.0", {
  testServer(reportGenerator(), {
    session$setInputs(datasetLoad = data.frame(name = c("CHUBX.zip"),
                                               datapath = c(test_path("TrePat", "2.5.2", "zip", "CHUBX.zip"),
                                                            test_path("TrePat", "2.5.2", "zip", "CPRD.zip"),
                                                            test_path("TrePat", "2.5.2", "zip", "IQVIA.zip"))),
                      dataVersion = "0.6.0")

    expect_equal(length(uploadedFiles$dataIP), 0)
  })
})

test_that("datasetLoad TrePat", {
  testServer(reportGenerator(), {
    session$setInputs(datasetLoadTP = data.frame(name = c("CHUBX.zip",
                                                          "CPRD.zip",
                                                          "IQVIA.zip"),
                                                 datapath = c(test_path("TrePat", "2.5.2", "zip", "CHUBX.zip"),
                                                              test_path("TrePat", "2.5.2", "zip", "CPRD.zip"),
                                                              test_path("TrePat", "2.5.2", "zip", "IQVIA.zip"))),
                      dataVersionTP = "2.5.2")

    expect_equal(length(uploadedFiles$dataTP), 1)
  })
})

test_that("datasetLoad TrePat Wrong Data 0.6.0", {
  testServer(reportGenerator(), {
    session$setInputs(datasetLoadTP = data.frame(name = c("mock_data_ReportGenerator_CHUBX.zip"),
                                                 datapath = c(test_path("IncPrev", "0.6.0", "zip", "mock_data_ReportGenerator_CHUBX.zip"))),
                      dataVersionTP = "2.5.2")

    expect_equal(length(uploadedFiles$dataTP), 0)
  })
})

test_that("datasetLoad TrePat Wrong Data", {
  testServer(reportGenerator(), {
    session$setInputs(datasetLoadTP = data.frame(name = c("mock_data_ReportGenerator_CHUBX.zip",
                                                          "mock_data_ReportGenerator_CHUBX.zip",
                                                          "mock_data_ReportGenerator_IMASIS.zip"),
                                                 datapath = c(test_path("IncPrev", "0.5.1", "zip", "mock_data_ReportGenerator_CHUBX.zip"),
                                                              test_path("IncPrev", "0.5.1", "zip", "mock_data_ReportGenerator_CPRD_GOLD.zip"),
                                                              test_path("IncPrev", "0.5.1", "zip", "mock_data_ReportGenerator_IMASIS.zip"))),
                      dataVersionTP = "2.5.2")

    expect_equal(length(uploadedFiles$dataTP), 0)
  })
})


test_that("datasetLoad TrePat Wrong Data 0.6.0", {
  testServer(reportGenerator(), {
    session$setInputs(datasetLoadTP = data.frame(name = c("mock_data_ReportGenerator_CHUBX.zip"),
                                                 datapath = c(test_path("IncPrev", "0.6.0", "zip", "mock_data_ReportGenerator_CHUBX.zip"))),
                      dataVersionTP = "2.5.2")

    expect_equal(length(uploadedFiles$dataTP), 0)
  })
})

test_that("datasetLoad PP", {
  testServer(reportGenerator(), {
    session$setInputs(datasetLoadPP = data.frame(name = c("results_CPRD.zip",
                                                        "results_EBB.zip",
                                                        "results_IPCI.zip"),
                                               datapath = c(test_path("PP", "0.5.1", "zip", "results_CPRD.zip"),
                                                            test_path("PP", "0.5.1", "zip", "results_EBB.zip"),
                                                            test_path("PP", "0.5.1", "zip", "results_IPCI.zip"))),
                      dataVersionPP = "0.5.1")

    expect_equal(length(uploadedFiles$dataPP), 2)
  })
})

test_that("datasetLoad PP", {
  testServer(reportGenerator(), {
    session$setInputs(datasetLoadPP = data.frame(name = c("patientCharacteristics_hepatitisb.csv"),
                                                 datapath = c(test_path("PP", "0.5.1", "csv", "patientCharacteristics_hepatitisb.csv"))),
                      dataVersionPP = "0.5.1")
    expect_equal(length(uploadedFiles$dataPP), 1)
  })
})


test_that("datasetLoad PP 0.5.1", {
  testServer(reportGenerator(), {
    session$setInputs(datasetLoadPP = data.frame(name = c("results_CPRD.zip"),
                                               datapath = c(test_path("PP", "0.5.1", "zip", "results_CPRD.zip"))),
                      dataVersionPP = "0.5.1")

    expect_equal(length(uploadedFiles$dataPP), 2)
  })
})

test_that("datasetLoad PP Wrong Data", {
  testServer(reportGenerator(), {
    session$setInputs(datasetLoadPP = data.frame(name = c("CHUBX.zip"),
                                               datapath = c(test_path("TrePat", "2.5.2", "zip", "CHUBX.zip"),
                                                            test_path("TrePat", "2.5.2", "zip", "CPRD.zip"),
                                                            test_path("TrePat", "2.5.2", "zip", "IQVIA.zip"))),
                      dataVersionPP = "0.6.0")

    expect_equal(length(uploadedFiles$dataIP), 0)
  })
})

test_that("reset data", {
  testServer(reportGenerator(), {
    session$setInputs(resetData = TRUE)
    expect_equal(uploadedFiles$dataIP, NULL)
    expect_equal(uploadedFiles$dataTP, NULL)
    expect_equal(uploadedFiles$dataPP, NULL)
    expect_equal(itemsList$objects, NULL)
    expect_s3_class(datasetLoadServer("IncidencePrevalence"), "shiny.render.function")
    expect_s3_class(datasetLoadServer("TreatmentPatterns"), "shiny.render.function")
    expect_s3_class(datasetLoadServer("PatientProfiles"), "shiny.render.function")
  })
})

test_that("prevalenceAttritionCommon correct columns", {
  # uploadedFiles <- list()
  testServer(reportGenerator(), {
    session$setInputs(analysisIdTable1 = 1)
    uploadedFiles$dataIP$prevalence_attrition <- prevalence_attrition_test
    expect_equal(length(names(prevalenceAttritionCommon())), 24)
  })
})

test_that("if prevalenceAttritionCommon is NULL", {
  testServer(reportGenerator(), {
    session$setInputs(analysisIdTable1 = 1)
    expect_equal(prevalenceAttritionCommon(), NULL)
  })
})

test_that("incidenceAttritionCommon correct columns", {
  testServer(reportGenerator(), {
    session$setInputs(analysisIdTable1 = 1)
    uploadedFiles$dataIP$incidence_attrition <- incidence_attrition_test
    expect_equal(length(names(incidenceAttritionCommon())), 24)
  })
})

test_that("if incidenceAttritionCommon is NULL", {
  testServer(reportGenerator(), {
    session$setInputs(analysisIdTable1 = 1)
    expect_equal(incidenceAttritionCommon(), NULL)
  })
})


test_that("previewTable1() FALSE", {
  # uploadedFiles <- list()
  testServer(reportGenerator(), {
    uploadedFiles$dataIP$prevalence_attrition <- prevalence_attrition_test
    uploadedFiles$dataIP$incidence_attrition <- incidence_attrition_test
    dataReport <- list()
    session$setInputs(analysisIdTable1 = 1)
    prevalenceAttritionCommon()
    session$setInputs(analysisIdTable1 = 1)
    incidenceAttritionCommon()
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
    session$setInputs(lockTableNumPar = TRUE)
    expect_equal(class(output$previewTable1), "character")
  })
})

test_that("previewTableAttInc() FALSE", {
  testServer(reportGenerator(), {
    uploadedFiles$dataIP$incidence_attrition <- incidence_attrition_test
    dataReport <- list()
    session$setInputs(analysisIdTable1 = 1)
    incidenceAttritionCommon()
    session$setInputs(lockTableIncAtt  = FALSE)
    expect_equal(class(output$previewTableAttInc), "character")
  })
})

test_that("previewTableAttInc() TRUE", {
  testServer(reportGenerator(), {
    uploadedFiles$dataIP$incidence_attrition <- incidence_attrition_test
    dataReport <- list()
    session$setInputs(analysisIdTable1 = 1)
    incidenceAttritionCommon()
    session$setInputs(lockTableIncAtt  = TRUE)
    expect_equal(class(output$previewTableAttInc), "character")
  })
})

test_that("previewTableAttPrev() FALSE", {
  testServer(reportGenerator(), {
    uploadedFiles$dataIP$prevalence_attrition <- prevalence_attrition_test
    dataReport <- list()
    session$setInputs(analysisIdTable1 = 1)
    prevalenceAttritionCommon()
    session$setInputs(lockTablePrevAtt  = FALSE)
    expect_equal(class(output$previewTableAttPrev), "character")
  })
})

test_that("previewTableAttPrev() TRUE", {
  testServer(reportGenerator(), {
    uploadedFiles$dataIP$prevalence_attrition <- prevalence_attrition_test
    dataReport <- list()
    session$setInputs(analysisIdTable1 = 1)
    prevalenceAttritionCommon()
    session$setInputs(lockTablePrevAtt  = TRUE)
    expect_equal(class(output$previewTableAttPrev), "character")
  })
})

test_that("previewTableSex() FALSE", {
  testServer(reportGenerator(), {
    uploadedFiles$dataIP$incidence_estimates <- incidence_estimates_test
    dataReport <- list()
    session$setInputs(lockTableSex = FALSE)
    expect_equal(class(output$previewTableSex), "list")
  })
})

test_that("previewTableSex() TRUE", {
  testServer(reportGenerator(), {
    uploadedFiles$dataIP$incidence_estimates <- incidence_estimates_test
    dataReport <- list()
    session$setInputs(lockTableSex = TRUE)
    testthat::expect_equal(class(output$previewTableSex), "list")
  })
})

test_that("incidenceFigure1() FALSE", {
  testServer(reportGenerator(), {
    uploadedFiles$dataIP$incidence_estimates <- incidence_estimates_test
    dataReport <- list()
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
    testthat::expect_s3_class(incidenceFigure1(), "data.frame")
  })
})

test_that("incidenceFigure1() TRUE", {
  testServer(reportGenerator(), {
    uploadedFiles$dataIP$incidence_estimates <- incidence_estimates_test
    dataReport <- list()
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
    testthat::expect_s3_class(incidenceFigure1(), "data.frame")
  })
})

test_that("incidenceFigure2Sex() FALSE", {
  testServer(reportGenerator(), {
    uploadedFiles$dataIP$incidence_estimates <- incidence_estimates_test
    dataReport <- list()
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
    testthat::expect_s3_class(incidenceFigure2Sex(), "data.frame")
  })
})

test_that("incidenceFigure2Sex() TRUE", {
  testServer(reportGenerator(), {
    uploadedFiles$dataIP$incidence_estimates <- incidence_estimates_test
    dataReport <- list()
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
    testthat::expect_s3_class(incidenceFigure2Sex(), "data.frame")
  })
})

test_that("incidenceFigure3Age() FALSE", {
  testServer(reportGenerator(), {
    uploadedFiles$dataIP$incidence_estimates <- incidence_estimates_test
    dataReport <- list()
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
    testthat::expect_s3_class(incidenceFigure3Age(), "data.frame")
  })
})

test_that("incidenceFigure3Age() TRUE", {
  testServer(reportGenerator(), {
    uploadedFiles$dataIP$incidence_estimates <- incidence_estimates_test
    dataReport <- list()
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
    testthat::expect_s3_class(incidenceFigure3Age(), "data.frame")
  })
})

test_that("prevalenceFigure4() FALSE", {
  testServer(reportGenerator(), {
    uploadedFiles$dataIP$prevalence_estimates <- prevalence_estimates_test
    dataReport <- list()

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
    testthat::expect_s3_class(prevalenceFigure4(), "data.frame")
  })
})

test_that("prevalenceFigure4() TRUE", {
  testServer(reportGenerator(), {
    uploadedFiles$dataIP$prevalence_estimates <- prevalence_estimates_test
    dataReport <- list()
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
    testthat::expect_s3_class(prevalenceFigure4(), "data.frame")
  })
})

test_that("prevalenceFigure5() FALSE", {
  testServer(reportGenerator(), {
    uploadedFiles$dataIP$prevalence_estimates <- prevalence_estimates_test
    dataReport <- list()
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
    testthat::expect_s3_class(prevalenceFigure5(), "data.frame")
  })
})

test_that("prevalenceFigure5() TRUE", {
  testServer(reportGenerator(), {
    uploadedFiles$dataIP$prevalence_estimates <- prevalence_estimates_test
    dataReport <- list()
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
    testthat::expect_s3_class(prevalenceFigure5(), "data.frame")
  })
})

test_that("prevalenceFigure6() FALSE", {
  testServer(reportGenerator(), {
    uploadedFiles$dataIP$prevalence_estimates <- prevalence_estimates_test
    dataReport <- list()
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
    testthat::expect_s3_class(prevalenceFigure6(), "data.frame")
  })
})

test_that("prevalenceFigure6() TRUE", {
  testServer(reportGenerator(), {
    uploadedFiles$dataIP$prevalence_estimates <- prevalence_estimates_test
    dataReport <- list()
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
    testthat::expect_s3_class(prevalenceFigure6(), "data.frame")
  })
})

test_that("Sunburst data 2.5.2", {
  testServer(reportGenerator(), {
    treatmentPathways_test <- mutate(treatmentPathways_test,
                                     cdm_name = "1")
    uploadedFiles$dataTP$treatmentPathways <- treatmentPathways_test
    session$setInputs(sexSunburst = "all",
                      ageSunburst = "all",
                      indexSunburst = "all",
                      cdmSunburst = "1",
                      dataVersionTP = "2.5.2")
    testthat::expect_s3_class(treatmentDataSunburst(), "data.frame")
    testthat::expect_s3_class(uploadedFiles$dataTP$treatmentPathways, "data.frame")
  })
})

test_that("Sunburst data 2.5.0", {
  testServer(reportGenerator(), {
    treatmentPathways_test <- mutate(treatmentPathways_test,
                                     cdm_name = "1",
                                     index_year = indexYear)
    uploadedFiles$dataTP$treatmentPathways <- treatmentPathways_test
    session$setInputs(sexSunburst = "all",
                      ageSunburst = "all",
                      indexSunburst = "all",
                      cdmSunburst = "1",
                      dataVersionTP = "2.5.0")
    testthat::expect_s3_class(treatmentDataSunburst(), "data.frame")
    testthat::expect_s3_class(uploadedFiles$dataTP$treatmentPathways, "data.frame")
  })
})

test_that("lockTreatmentSunburst TRUE", {
  testServer(reportGenerator(), {
    treatmentPathways_test <- mutate(treatmentPathways_test,
                                     cdm_name = "1")
    uploadedFiles$dataTP$treatmentPathways <- treatmentPathways_test
    dataReport <- list()
    session$setInputs(sexSunburst = "all",
                      ageSunburst = "all",
                      indexSunburst = "all",
                      cdmSunburst = "1",
                      dataVersionTP = "2.5.2",
                      lockTreatmentSunburst = TRUE)
    testthat::expect_s3_class(treatmentDataSunburst(), "data.frame")
  })
})

test_that("lockTreatmentSunburst FALSE", {
  testServer(reportGenerator(), {
    treatmentPathways_test <- mutate(treatmentPathways_test,
                                     cdm_name = "1",
                                     index_year = indexYear)
    uploadedFiles$dataTP$treatmentPathways <- treatmentPathways_test
    dataReport <- list()
    session$setInputs(sexSunburst = "all",
                      ageSunburst = "all",
                      indexSunburst = "all",
                      cdmSunburst = "1",
                      dataVersionTP = "2.5.2",
                      lockTreatmentSunburst = FALSE)
    testthat::expect_s3_class(treatmentDataSunburst(), "data.frame")
  })
})

test_that("Sankey data 2.5.2", {
  testServer(reportGenerator(), {
    treatmentPathways_test <- mutate(treatmentPathways_test,
                                     cdm_name = "1")
    uploadedFiles$dataTP$treatmentPathways <- treatmentPathways_test
    session$setInputs(sexSankey = "all",
                      ageSankey = "all",
                      indexSankey = "all",
                      cdmSankey = "1",
                      dataVersionTP = "2.5.2")
    testthat::expect_s3_class(treatmentDataSankey(), "data.frame")
    testthat::expect_s3_class(uploadedFiles$dataTP$treatmentPathways, "data.frame")
  })
})

test_that("Sankey data 2.5.0", {
  testServer(reportGenerator(), {
    treatmentPathways_test <- mutate(treatmentPathways_test,
                                     cdm_name = "1",
                                     index_year = indexYear)
    uploadedFiles$dataTP$treatmentPathways <- treatmentPathways_test
    session$setInputs(sexSankey = "all",
                      ageSankey = "all",
                      indexSankey = "all",
                      cdmSankey = "1",
                      dataVersionTP = "2.5.0")
    testthat::expect_s3_class(treatmentDataSankey(), "data.frame")
    testthat::expect_s3_class(uploadedFiles$dataTP$treatmentPathways, "data.frame")
  })
})

test_that("lockTreatmentSankey TRUE", {
  testServer(reportGenerator(), {
    treatmentPathways_test <- mutate(treatmentPathways_test,
                                     cdm_name = "1",
                                     index_year = indexYear)
    uploadedFiles$dataTP$treatmentPathways <- treatmentPathways_test
    dataReport <- list()
    session$setInputs(sexSankey = "all",
                      ageSankey = "all",
                      indexSankey = "all",
                      cdmSankey = "1",
                      dataVersionTP = "2.5.2",
                      lockTreatmentSankey = TRUE)
    testthat::expect_s3_class(treatmentDataSankey(), "data.frame")
  })
})

test_that("lockTreatmentSankey FALSE", {
  testServer(reportGenerator(), {
    treatmentPathways_test <- mutate(treatmentPathways_test,
                                     cdm_name = "1",
                                     index_year = indexYear)
    uploadedFiles$dataTP$treatmentPathways <- treatmentPathways_test
    dataReport <- list()
    session$setInputs(sexSankey = "all",
                      ageSankey = "all",
                      indexSankey = "all",
                      cdmSankey = "1",
                      dataVersionTP = "2.5.2",
                      lockTreatmentSankey = FALSE)
    testthat::expect_s3_class(treatmentDataSankey(), "data.frame")
  })
})

test_that("summarised Characteristics and LSC", {
  testServer(reportGenerator(), {
    expect_s3_class(characteristicsUI("characteristics",
                                      dataset = summarisedCharacteristics), "shiny.tag.list")
    expect_s3_class(characteristicsServer("characteristics",
                                          dataset = summarisedCharacteristics), "reactiveVal")
    expect_s3_class(characteristicsUI("lsc",
                                      dataset = summarisedCharacteristicsLSC), "shiny.tag.list")
    expect_s3_class(characteristicsServer("lsc",
                                          dataset = summarisedCharacteristicsLSC), "reactiveVal")
  })
})





