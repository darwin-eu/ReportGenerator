#' table1NumPar
#'
#' @import here flextable dplyr
#' @export
table1NumPar <- function (incidence_attrition,
                          prevalence_attrition) {

  databaseNamePrev <- unique(prevalence_attrition$database_name)

  databaseNamePrev <- databaseNamePrev[1:3]

  tablePrevalenceAtt <- prevalence_attrition %>%
    filter(database_name == databaseNamePrev[1]) %>%
    group_by(step,
             reason,
             current_n,
             excluded) %>%
    summarise()

  databaseNameInc <- unique(incidence_attrition$database_name)

  databaseNameInc <- databaseNameInc[1:3]

  tableIncidenceAtt <- incidence_attrition %>%
    filter(database_name == databaseNameInc[1],
           step == "Estimating incidence",
           analysis_interval == "years") %>%
    group_by(step,
             reason) %>%
    summarise(current_n = round(mean(current_n), 0),
              excluded = round(mean(excluded), 0))

  tablePrevIncData <- bind_rows(tablePrevalenceAtt, tableIncidenceAtt)

  # for (i in databaseNamePrev[2:length(databaseNamePrev)])

  for (i in databaseNamePrev[2:3]) {

    # i <- 2

    subPrevalenceAtt <- prevalence_attrition %>%
      filter(database_name == i) %>%
      group_by(step,
               reason,
               current_n,
               excluded) %>%
      summarise()

    subIncidenceAtt <- incidence_attrition %>%
      filter(database_name == i,
             step == "Estimating incidence",
             analysis_interval == "years") %>%
      group_by(step,
               reason) %>%
      summarise(current_n = round(mean(current_n), 0),
                excluded = round(mean(excluded), 0))

    subPrevIncData <- bind_rows(subPrevalenceAtt, subIncidenceAtt)

    subPrevalenceAtt <- subPrevIncData[, -c(1:2)]

    tablePrevIncData <- bind_cols(tablePrevIncData,
                                    subPrevalenceAtt)

  }

  headerNames <- gsub("\\..*","", names(tablePrevIncData))

  subtitles <- c(" ", databaseNamePrev)

  flexTableAtt <- flextable(tablePrevIncData)

  # Deletes original header

  flexTableAtt <- delete_part(x = flexTableAtt, part = "header")

  # Adds header with repeaded col_keys

  flexTableAtt <- add_header_row(flexTableAtt,
                                 values = headerNames,
                                 colwidths = rep(1, 8),
                                 top = FALSE)

  # flexTableAtt <- add_header_row(flexTableAtt,
  #                                values = headerNames,
  #                                colwidths = rep(1, length(headerNames)),
  #                                top = FALSE)

  # Adds header specifying database names

  flexTableAtt <- add_header_row(flexTableAtt,
                                 values = subtitles,
                                 colwidths = rep(2, length(subtitles)),
                                 top = TRUE) %>%
    theme_box()

  # flexTableAtt <- add_header_row(flexTableAtt,
  #                                values = subtitles,
  #                                colwidths = rep(2, length(subtitles)),
  #                                top = TRUE) %>%
  #   theme_box()

  return(flexTableAtt)
}

#' table2IncOver
#'
#' @import here flextable dplyr
#' @export
table2IncOver <- function (incidence_estimates) {

  # names(incidence_estimates)
  #
  # tableIncidence <- incidence_estimates %>%
  #   group_by(outcome_cohort_name,
  #            database_name,
  #            n_persons,
  #            person_years,
  #            n_events,
  #            incidence_100000_pys) %>%
  #   summarise()
  #
  # # View(tableIncidence)
  #
  # return(tableIncidence)

# names(incidence_estimates)
#
# incidence_estimates <- incidence_estimates[grep("\\(", incidence_estimates[["outcome_cohort_name"]]), ]
# View(incidence_estimates)
# tableIncidence <- incidence_estimates %>%
#   filter(database_name == "CPRD GOLD",
#          n_persons == 8215316) %>%
#   group_by(outcome_cohort_name,
#            database_name,
#            n_persons,
#            person_years,
#            n_events,
#            incidence_100000_pys) %>%
#   summarise()
#
# View(tableIncidence)

  incidence_estimates <- incidence_estimates[grep("\\(", incidence_estimates[["outcome_cohort_name"]]), ]

  databaseNamePrev <- unique(prevalence_attrition$database_name)

  databaseNamePrev <- databaseNamePrev[1:3]

  # View(incidence_estimates)
  tableIncidence <- incidence_estimates %>%
    filter(database_name %in% databaseNamePrev) %>%
    group_by(outcome_cohort_name,
             database_name,
             n_persons,
             person_years,
             n_events,
             incidence_100000_pys) %>%
    summarise()

  return(tableIncidence)

}

#' table3IncYear
#'
#' @import here flextable dplyr
#' @export
table3IncYear <- function (incidence_estimates) {

  # load(here("inst/data/antibioticsProcessed/dataShiny.RData"))
  #
  # names(incidence_estimates)

  incidence_estimates <- incidence_estimates[grep("\\(", incidence_estimates[["outcome_cohort_name"]]), ]

  databaseNamePrev <- unique(prevalence_attrition$database_name)

  databaseNamePrev <- databaseNamePrev[1:3]

  tableIncidence <- incidence_estimates %>%
    filter(database_name %in% databaseNamePrev) %>%
    filter(analysis_interval == "years") %>%
    group_by(outcome_cohort_name,
             database_name,
             incidence_start_date,
             n_persons,
             person_years,
             n_events,
             incidence_100000_pys) %>%
    summarise()

 return(tableIncidence)
}

#' table4IncAge
#'
#' @import here flextable dplyr
#' @export
table4IncAge <- function (incidence_estimates) {

  # load(here("inst/data/antibioticsProcessed/dataShiny.RData"))
  #
  # names(incidence_estimates)

  incidence_estimates <- incidence_estimates[grep("\\(", incidence_estimates[["outcome_cohort_name"]]), ]

  databaseNamePrev <- unique(prevalence_attrition$database_name)

  databaseNamePrev <- databaseNamePrev[1:3]

  tableIncidence <- incidence_estimates %>%
    filter(database_name %in% databaseNamePrev) %>%
    group_by(outcome_cohort_name,
             database_name,
             denominator_age_group,
             n_persons,
             person_years,
             n_events,
             incidence_100000_pys) %>%
    summarise()

  # View(tableIncidence)

  return(tableIncidence)
}

#' table5IncSex
#'
#' @import here flextable dplyr
#' @export
table5IncSex <- function (incidence_estimates) {

  # load(here("inst/data/antibioticsProcessed/dataShiny.RData"))
  #
  # names(incidence_estimates)

  incidence_estimates <- incidence_estimates[grep("\\(", incidence_estimates[["outcome_cohort_name"]]), ]

  databaseNamePrev <- unique(prevalence_attrition$database_name)

  databaseNamePrev <- databaseNamePrev[1:3]

  tableIncidence <- incidence_estimates %>%
    filter(database_name %in% databaseNamePrev) %>%
    group_by(outcome_cohort_name,
             database_name,
             denominator_sex,
             n_persons,
             person_years,
             n_events,
             incidence_100000_pys) %>%
    summarise()

  # View(tableIncidence)

  return(tableIncidence)
}
