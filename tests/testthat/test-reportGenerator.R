test_that("datasetLoad IncPrev", {
  testServer(reportGenerator(logger), {
    session$setInputs(datasetLoad = data.frame(name = c("StudyResults.zip"),
                                               datapath = c(test_path("studies",
                                                                      "zip",
                                                                      "StudyResults.zip"))))

    expect_equal(length(uploaded_files), 3)
  })
})

test_that("reset data", {
  testServer(reportGenerator(logger), {
    session$setInputs(resetData = TRUE)
    expect_equal(uploaded_files$IncidencePrevalence, NULL)
    expect_equal(uploaded_files$TreatmentPatterns, NULL)
    expect_equal(uploaded_files$CohortCharacteristics, NULL)
    expect_equal(itemsList$objects, NULL)
    expect_s3_class(datasetLoadServer("IncidencePrevalence"), "shiny.render.function")
    expect_s3_class(datasetLoadServer("TreatmentPatterns"), "shiny.render.function")
    expect_s3_class(datasetLoadServer("PatientProfiles"), "shiny.render.function")
  })
})




