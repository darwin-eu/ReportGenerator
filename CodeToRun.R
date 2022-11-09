library(reportGenerator)
library(IncidencePrevalenceReport)
library(dplyr)
library(tidyr)
library(ggplot2)
library(devtools)
library(duckdb)

# Instaling new development version

document()
check()
install()

load_all()

# Example with mock data

cdm <- mockIncidencePrevalenceRef(sampleSize = 50000)

# Denominator populations

dpop <- collectDenominator(cdm = cdm,
                           startDate  = as.Date("2008-01-01"),
                           endDate  = as.Date("2012-01-01"),
                           ageStrata  = list(c(18,65)),
                           sexStrata  = "Female",
                           daysPriorHistory  = 365)
glimpse(dpop)
# Adding denominator population to cdm object

cdm$denominator <- dpop$denominator_population

# Incidence calculation

incidence <- computeIncidence(
  cdm = cdm,
  table_name_denominator = "denominator",
  table_name_outcomes = "outcome",
  cohort_ids_outcomes = "1",
  cohort_ids_denominator_pops = "1",
  time_interval = c("Years"),
  outcome_washout_windows = 180
)

incidence %>%
  glimpse()

# Prevalence calculation

prevalence <- computePrevalence(
  cdm = cdm,
  denominatorTable = "denominator",
  denominatorId  = c("1"),
  outcomeTable = "outcome",
  outcomeId  = "1",
  interval ="Years",
  type = "period",
  minCellCount = 0
  )

# prevalence$prevalence_estimates %>%
#   left_join(prevalence$analysis_settings,
#             by = "prevalence_analysis_id") %>%
#   left_join(dpop$denominator_settings,
#             by=c("denominator_id" = "cohort_definition_id")) %>%
#   ggplot(aes(start_date , prev))+
#   facet_grid(start_date ~ sex_strata)+
#   geom_bar(stat = "identity") +
#   scale_y_continuous(labels = scales::percent,
#                      limits = c(0,NA))+
#   theme_bw()


# prevalence$prevalence_estimates %>%
#   left_join(prevalence$analysis_settings,
#             by = "prevalence_analysis_id") %>%
#   left_join(dpop$denominator_settings,
#             by=c("cohort_id_denominator_pop"="cohort_definition_id")) %>%
#   ggplot(aes(start_time, prev))+
#   facet_grid(age_strata ~ sex_strata)+
#   geom_bar(stat='identity')+
#   scale_y_continuous(labels = scales::percent,
#                      limits = c(0,NA))+
#   theme_bw()


# prev <- collect_pop_prevalence(
#   cdm = cdm,
#   table_name_denominator = "denominator",
#   table_name_outcomes = "outcome",
#   cohort_ids_outcomes = "1",
#   cohort_ids_denominator_pops = c("1", "2", "3","4","5","6"),
#   time_intervals="months",
#   type = "period",
#   minimum_cell_count = 0
# )



##### STUDY REPORT LAUNCH

title <- "Incidence Prevalence Report"
author <- "Cesar Barboza Gutierrez"
abstract <- "Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat."
denominator <- dpop
prevalence <- prevalence
incidence <- incidence


load_all()

## RUN

reportIncidencePrevalence(title,
                          author,
                          abstract,
                          denominator,
                          incidence,
                          prevalence,
                          format = "word")
#
