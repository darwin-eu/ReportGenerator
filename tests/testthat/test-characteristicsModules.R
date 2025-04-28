test_that("summarised Characteristics and LSC", {
  testServer(reportGenerator(), {
    summarise_characteristics_data <- testData$CohortCharacteristics$summarise_characteristics
    summarise_large_scale_characteristics_data <- testData$CohortCharacteristics$summarise_large_scale_characteristics

    expect_s3_class(characteristicsUI("characteristics",
                                      uploaded_files = summarise_characteristics_data), "shiny.tag.list")
    expect_s3_class(characteristicsServer("characteristics",
                                          uploaded_files = summarise_large_scale_characteristics_data), "reactiveVal")
    expect_s3_class(characteristicsUI("lsc",
                                      uploaded_files = summarise_characteristics_data), "shiny.tag.list")
    expect_s3_class(characteristicsServer("lsc",
                                          uploaded_files = summarise_large_scale_characteristics_data), "reactiveVal")
  })
})


# test_that("summarised Characteristics and LSC", {
#   csvLocation <- file.path(tempdir(), "dataLocation")
#   dir.create(csvLocation)
#   fileDataPath <- list.files(test_path("studies", "summarised_zip"),
#                              pattern = "zip",
#                              full.names = TRUE)
#   logger <- log4r::logger()
#   unzipDir <- file.path(tempdir(), "lsc")
#   uploaded_files <- joinDatabases(fileDataPath = fileDataPath[1])
#
#   # uploaded_files <- uploaded_files$summarised_result %>%
#   #   visOmopResults::filterSettings(result_type == "summarise_characteristics")
#
#   testServer(reportGenerator(), {
#     expect_s3_class(characteristicsUI("characteristics", uploaded_files), "shiny.tag.list")
#     # expect_s3_class(characteristicsServer("characteristics", reactive(uploaded_files)), "reactiveVal")
#   })
#   unlink(csvLocation, recursive = TRUE)
# })
