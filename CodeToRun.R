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
  cohort_ids_denominator_pops = c("1"),
  table_name_outcomes = "outcome",
  cohort_ids_outcomes = "1",
  time_intervals="Years",
  type = "point",
  minimum_cell_count = 0
)

prevalence$prevalence_estimates %>%
  left_join(prevalence$analysis_settings,
            by = "prevalence_analysis_id") %>%
  left_join(dpop$denominator_settings,
            by=c("cohort_id_denominator_pop"="cohort_definition_id")) %>%
  ggplot(aes(start_time, prev))+
  facet_grid(age_strata ~ sex_strata)+
  geom_point()+
  scale_y_continuous(labels = scales::percent,
                     limits = c(0,NA))+
  theme_bw()


prevalence$prevalence_estimates %>%
  left_join(prevalence$analysis_settings,
            by = "prevalence_analysis_id") %>%
  left_join(dpop$denominator_settings,
            by=c("cohort_id_denominator_pop"="cohort_definition_id")) %>%
  ggplot(aes(start_time, prev))+
  facet_grid(age_strata ~ sex_strata)+
  geom_bar(stat='identity')+
  scale_y_continuous(labels = scales::percent,
                     limits = c(0,NA))+
  theme_bw()


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

# Incidence calculation

# incidence <- collect_pop_incidence(
#   cdm = cdm,
#   table_name_denominator = "denominator",
#   table_name_outcomes = "outcome",
#   cohort_ids_outcomes = "1",
#   cohort_ids_denominator_pops = "1",
#   time_interval = c("Years"),
#   outcome_washout_windows = 180
# )
#
# incidence %>%
#   glimpse()

##### STUDY REPORT LAUNCH

title <- "Incidence Prevalence Report"
author <- "Cesar Barboza Gutierrez"
denominator <- dpop
prevalence <- prevalence
# incidence <- incidence


## RUN

reportIncidencePrevalence(title,
                          author,
                          denominator,
                          prevalence,
                          word = TRUE)


