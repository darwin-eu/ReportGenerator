test_that("table classes", {
  skip_if(!file.exists(here("inst/data/antibioticsProcessed/dataShiny.RData")))

  load(here("inst/data/antibioticsProcessed/dataShiny.RData"))

  expect_s3_class(table1NumPar(incidence_attrition,
                           prevalence_attrition), "flextable")
  expect_s3_class(table2IncOver(incidence_estimates), "data.frame")
  expect_s3_class(table3IncYear(incidence_estimates), "data.frame")
  expect_s3_class(table4IncAge(incidence_estimates), "data.frame")
})
