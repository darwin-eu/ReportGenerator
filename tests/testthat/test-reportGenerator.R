test_that("datasetLoad IncPrev", {
  testServer(reportGenerator(), {
    session$setInputs(datasetLoad = data.frame(name = c("StudyResults.zip"),
                                               datapath = c(test_path("studies",
                                                                      "zip",
                                                                      "StudyResults.zip"))))

    expect_equal(length(uploadedFiles), 3)
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





