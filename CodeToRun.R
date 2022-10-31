library(IncidencePrevalence)
library(IncidencePrevalenceReport)
library(dplyr)
library(tidyr)
library(ggplot2)

# Example with mock data

cdm <- generate_mock_incidence_prevalence_db(sample_size = 50000,
                                             out_pre = 0.5)
# Denominator populations

dpop <- collect_denominator_pops(cdm = cdm,
                                 study_start_date = as.Date("2008-01-01"),
                                 study_end_date = as.Date("2012-01-01"),
                                 study_age_stratas = list(c(18,65)),
                                 study_sex_stratas = "Female",
                                 study_days_prior_history = 365)

# Adding denominator population to cdm object

cdm$denominator <- dpop$denominator_population

# Prevalence calculation

prevalence <- collect_pop_prevalence(
  cdm = cdm,
  table_name_denominator = "denominator",
  cohort_ids_denominator_pops = "1",
  table_name_outcomes = "outcome",
  cohort_ids_outcomes = "1",
  time_intervals="Years",
  type = "point",
  minimum_cell_count = 0
)

# Incidence calculation

incidence <- collect_pop_incidence(
  cdm = cdm,
  table_name_denominator = "denominator",
  table_name_outcomes = "outcome",
  cohort_ids_outcomes = "1",
  cohort_ids_denominator_pops = "1",
  time_interval = c("Years"),
  outcome_washout_windows = 180
)

##### STUDY REPORT LAUNCH

title <- "Incidence Prevalence Report"
author <- "CESAR BARBOZA"
denominator <- dpop
prevalence <- prevalence
incidence <- incidence

## RUN

reportIncidencePrevalence(title,
                          author,
                          denominator,
                          prevalence,
                          incidence)

