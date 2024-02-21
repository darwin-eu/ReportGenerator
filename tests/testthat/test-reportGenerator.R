testData <- testData()

incidence_attrition_test <- testData$incidence_attrition
incidence_estimates_test <- testData$incidence_estimates
prevalence_attrition_test <- testData$prevalence_attrition
prevalence_estimates_test <- testData$prevalence_estimates
treatmentPathways_test <- testData$treatmentPathways_test
summarisedCharacteristics <- testData$`Summarised Characteristics`
summarisedCharacteristicsLSC <- testData$`Summarised Large Scale Characteristics`
survivalEstimate <- testData$`Survival estimate`
survivalCumulativeIncidence <- testData$`Survival cumulative incidence`

test_that("datasetLoad IncPrev", {
  testServer(reportGenerator(), {
    session$setInputs(datasetLoad = data.frame(name = c("mock_data_ReportGenerator_CHUBX.zip",
                                                        "mock_data_ReportGenerator_CHUBX.zip",
                                                        "mock_data_ReportGenerator_IMASIS.zip"),
                                               datapath = c(test_path("IP", "0.5.1", "zip", "mock_data_ReportGenerator_CHUBX.zip"),
                                                            test_path("IP", "0.5.1", "zip", "mock_data_ReportGenerator_CPRD_GOLD.zip"),
                                                            test_path("IP", "0.5.1", "zip", "mock_data_ReportGenerator_IMASIS.zip"))),
                      dataVersion = "0.5.1")

    expect_equal(length(uploadedFiles$dataIP), 4)
  })
})

test_that("datasetLoad IncPrev 0.6.0", {
  testServer(reportGenerator(), {
    session$setInputs(datasetLoad = data.frame(name = c("mock_data_ReportGenerator_CHUBX.zip"),
                                               datapath = c(test_path("IP", "0.6.0", "zip", "mock_data_ReportGenerator_CHUBX.zip"))),
                      dataVersion = "0.6.0")

    expect_equal(length(uploadedFiles$dataIP), 4)
  })
})

test_that("datasetLoad IncPrev Wrong Data", {
  testServer(reportGenerator(), {
    session$setInputs(datasetLoad = data.frame(name = c("CHUBX.zip"),
                                               datapath = c(test_path("TP", "2.5.2", "zip", "CHUBX.zip"),
                                                            test_path("TP", "2.5.2", "zip", "CPRD.zip"),
                                                            test_path("TP", "2.5.2", "zip", "IQVIA.zip"))),
                      dataVersion = "0.6.0")

    expect_equal(length(uploadedFiles$dataIP), 0)
  })
})

test_that("datasetLoad IncPrev Wrong Data 0.6.0", {
  testServer(reportGenerator(), {
    session$setInputs(datasetLoad = data.frame(name = c("CHUBX.zip"),
                                               datapath = c(test_path("TP", "2.5.2", "zip", "CHUBX.zip"),
                                                            test_path("TP", "2.5.2", "zip", "CPRD.zip"),
                                                            test_path("TP", "2.5.2", "zip", "IQVIA.zip"))),
                      dataVersion = "0.6.0")

    expect_equal(length(uploadedFiles$dataIP), 0)
  })
})

test_that("datasetLoad TrePat", {
  testServer(reportGenerator(), {
    session$setInputs(datasetLoadTP = data.frame(name = c("CHUBX.zip",
                                                          "CPRD.zip",
                                                          "IQVIA.zip"),
                                                 datapath = c(test_path("TP", "2.5.2", "zip", "CHUBX.zip"),
                                                              test_path("TP", "2.5.2", "zip", "CPRD.zip"),
                                                              test_path("TP", "2.5.2", "zip", "IQVIA.zip"))),
                      dataVersionTP = "2.5.2")

    expect_equal(length(uploadedFiles$dataTP), 1)
  })
})

test_that("datasetLoad TrePat Wrong Data 0.6.0", {
  testServer(reportGenerator(), {
    session$setInputs(datasetLoadTP = data.frame(name = c("mock_data_ReportGenerator_CHUBX.zip"),
                                                 datapath = c(test_path("IP", "0.6.0", "zip", "mock_data_ReportGenerator_CHUBX.zip"))),
                      dataVersionTP = "2.5.2")

    expect_equal(length(uploadedFiles$dataTP), 0)
  })
})

test_that("datasetLoad TrePat Wrong Data", {
  testServer(reportGenerator(), {
    session$setInputs(datasetLoadTP = data.frame(name = c("mock_data_ReportGenerator_CHUBX.zip",
                                                          "mock_data_ReportGenerator_CHUBX.zip",
                                                          "mock_data_ReportGenerator_IMASIS.zip"),
                                                 datapath = c(test_path("IP", "0.5.1", "zip", "mock_data_ReportGenerator_CHUBX.zip"),
                                                              test_path("IP", "0.5.1", "zip", "mock_data_ReportGenerator_CPRD_GOLD.zip"),
                                                              test_path("IP", "0.5.1", "zip", "mock_data_ReportGenerator_IMASIS.zip"))),
                      dataVersionTP = "2.5.2")

    expect_equal(length(uploadedFiles$dataTP), 0)
  })
})


test_that("datasetLoad TrePat Wrong Data 0.6.0", {
  testServer(reportGenerator(), {
    session$setInputs(datasetLoadTP = data.frame(name = c("mock_data_ReportGenerator_CHUBX.zip"),
                                                 datapath = c(test_path("IP", "0.6.0", "zip", "mock_data_ReportGenerator_CHUBX.zip"))),
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
                                               datapath = c(test_path("TP", "2.5.2", "zip", "CHUBX.zip"),
                                                            test_path("TP", "2.5.2", "zip", "CPRD.zip"),
                                                            test_path("TP", "2.5.2", "zip", "IQVIA.zip"))),
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

test_that("summarised Characteristics and LSC", {
  testServer(reportGenerator(), {
    expect_s3_class(characteristicsUI("characteristics",
                                      summarisedCharacteristics), "shiny.tag.list")
    expect_s3_class(characteristicsServer("characteristics",
                                          summarisedCharacteristics), "reactiveVal")
    expect_s3_class(characteristicsUI("lsc",
                                      summarisedCharacteristicsLSC), "shiny.tag.list")
    expect_s3_class(characteristicsServer("lsc",
                                          summarisedCharacteristicsLSC), "reactiveVal")
  })
})

test_that("survival modules classes", {
  testServer(reportGenerator(), {
    expect_s3_class(cohortSurvivalUI("survivalTable", survivalEstimate), "shiny.tag.list")
    expect_s3_class(cohortSurvivalServer("survivalTable", survivalEstimate), "shiny.render.function")
    expect_s3_class(cohortSurvivalUI("survivalPlot", survivalEstimate), "shiny.tag.list")
    expect_s3_class(cohortSurvivalServer("survivalPlot", survivalEstimate), "shiny.render.function")
    expect_s3_class(cohortSurvivalUI("failureTable", survivalCumulativeIncidence), "shiny.tag.list")
    expect_s3_class(cohortSurvivalServer("failureTable", survivalCumulativeIncidence), "shiny.render.function")
    expect_s3_class(cohortSurvivalUI("failurePlot", survivalCumulativeIncidence), "shiny.tag.list")
    expect_s3_class(cohortSurvivalServer("failurePlot", survivalCumulativeIncidence), "shiny.render.function")
  })
})




