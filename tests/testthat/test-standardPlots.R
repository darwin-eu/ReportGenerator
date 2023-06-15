test_that("addPreviewItemType", {

  # class(incidence_estimates) <- c("IncidencePrevalenceResult",
  #                                 "IncidenceResult",
  #                                 "tbl_df",
  #                                 "tbl",
  #                                 "data.frame")
  # previewItemString <- "plotIncidence(incidence_estimates, colour, facet)"
  # previewItemType <- "Facet by database"
  #
  # plotIncidence(incidence_estimates, colour = 'denominator_sex', facet = 'database_name', grid = FALSE)
  # plotIncidence(incidence_estimates, colour = 'denominator_sex', facet = 'outcome_cohort_name', grid = FALSE)
  #
  #
  # expression <- addPreviewItemType(previewItemString, previewItemType)
  # eval(parse(text = expression))
})

test_that("incidenceRatePerYearPlot", {

  # class(incidence_estimates) <- c("IncidencePrevalenceResult", "IncidenceResult", "tbl_df", "tbl", "data.frame")
  # type <- "Facet by database"
  #
  # incidenceRatePerYearPlot(incidence_estimates, "Facet by database")
  # incidenceRatePerYearPlot(incidence_estimates, "Facet by outcome")
  #
  # incidenceRatePerYearGroupBySexPlot(incidence_estimates, "Facet by database")
  # incidenceRatePerYearGroupBySexPlot(incidence_estimates, "Facet by outcome")
  #
  # incidenceRatePerYearColorByAgePlot(incidence_estimates, type)
  #
  #
  # incidenceRatePerYearFacetByDBAgeGroupPlot(incidence_estimates, "Facet by database")
  # incidenceRatePerYearFacetByDBAgeGroupPlot(incidence_estimates, "Facet by outcome")
  #
  #
  # incidenceFigure2(incidence_estimates)
  #
  # names(incidence_estimates)
  #
  #
  #   incidence_estimates %>%
  #     # filter(denominator_age_group == "0-99",
  #     #        denominator_sex != "Both") %>%
  #     ggplot(aes(x = incidence_start_date,
  #                y = incidence_100000_pys,
  #                group = denominator_sex,
  #                col = database_name)) +
  #     facet_grid(cols = vars(denominator_sex)) +
  #     # scale_y_continuous(labels = scales::percent,
  #     #                    limits = c(0,NA)) +
  #     geom_line() +
  #     geom_point() +
  #     theme_bw() +
  #     labs(x = "Calendar year",
  #          y = "Incidence rate per 100000 person-years",
  #          col = "Database name")
  #
  #   incidenceFigureData


})



