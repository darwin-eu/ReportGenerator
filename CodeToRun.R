# Installation

library(devtools)

document()
check()
install()

# Example
library(IncidencePrevalence)
library(dplyr)
library(tidyr)
library(ggplot2)
library(devtools)
library(duckdb)

# Loading data from CSV

incidenceData <- bind_rows(
  lapply(
    list.files(
      here("inst/csv/incidenceMockResults"),
      pattern = ".csv",
      full.names = TRUE
    ),
    read_csv
  )
)

prevalenceData <- bind_rows(
  lapply(
    list.files(
      here("inst/csv/prevalenceMockResults"),
      pattern = ".csv",
      full.names = TRUE
    ),
    read_csv
  )
)

# Table 1. <Drug users / Patients with condition X> in <list all data sources> during <stud period>

# New drug users / patients
colnames(incidenceData)
incidenceTable1 <- incidenceData %>%
  group_by(database_name) %>%
  summarise(`Number of new <Drug users / Patients with condition X>` = sum(n_persons))

# Total number of drug users / patients

prevalenceTable1 <- prevalenceData %>%
  group_by(database_name) %>%
  summarise(`Total number of <Drug users / Patients with condition X>` = sum(numerator))

# For publication in report

table1 <- left_join(incidenceTable1,
                    prevalenceTable1,
                    by = "database_name")

table1

View(table1)

# Figure 1. Incidence rate/s of drug/s use over calendar time (per month/year) overall

incidenceFigure1 <- incidenceData %>%
  filter(sex_strata == "Both",
         age_strata == "0;99") %>%
  ggplot(aes(x = time, y = ir_100000_pys, col = database_name)) +
  # scale_y_continuous(labels = scales::percent,
  #                    limits = c(0,NA)) +
  geom_line(aes(group = 1)) +
  geom_point() +
  geom_errorbar(aes(ymin = ir_100000_pys_low,
                    ymax = ir_100000_pys_high)) +
  theme_bw() +
  labs(x = "Calendar year",
       y = "Incidence rate per 100000 person-years",
       col = "Database name")

incidenceFigure1

# Figure 2. by year/month: two plots – males/females, all databases


incidenceFigure2 <- incidenceData %>%
  filter(age_strata == "0;99",
         sex_strata != "Both") %>%
  ggplot(aes(x = time,
             y = ir_100000_pys,
             group = sex_strata,
             col = database_name)) +
  facet_grid(cols = vars(sex_strata)) +
  # scale_y_continuous(labels = scales::percent,
  #                    limits = c(0,NA)) +
  geom_line() +
  geom_point() +
  theme_bw() +
  labs(x = "Calendar year",
       y = "Incidence rate per 100000 person-years",
       col = "Database name")

incidenceFigure2

# Figure 2b . by year/month: plot for each database, diff lines per age group

incidenceFigure2b <- incidenceData %>%
  filter(age_strata != "0;99",
         sex_strata == "Both") %>%
  ggplot(aes(x = time,
             y = ir_100000_pys)) +
  facet_grid(rows = vars(database_name)) +
  # scale_y_continuous(labels = scales::percent,
  #                    limits = c(0,NA)) +
  geom_line(aes(colour = age_strata)) +
  geom_point() +
  theme_bw() +
  labs(x = "Calendar year",
       y = "Incidence rate per 100000 person-years",
       colour = "Age group")

incidenceFigure2b

# Figure 2c . by age group (x-axis) for databases (color) and sex (dashed/line)

# v. 1

incidenceFigure2c <- incidenceData %>%
  filter(age_strata != "0;99",
         sex_strata != "Both") %>%
  ggplot(aes(x = time,
             y = ir_100000_pys,
             col = database_name)) +
  facet_grid(cols = vars(age_strata)) +
  # scale_y_continuous(labels = scales::percent,
  #                    limits = c(0,NA)) +
  geom_line(aes(linetype = sex_strata)) +
  geom_point() +
  theme_bw() +
  labs(x = "Calendar year",
       y = "Incidence rate per 100000 person-years",
       col = "Database name",
       linetype = "Sex")

incidenceFigure2c

# v. 2

incidenceFigure2c <- incidenceData %>%
  filter(age_strata != "0;99",
         sex_strata != "Both") %>%
  ggplot(aes(x = time,
             y = ir_100000_pys,
             col = database_name)) +
  facet_grid(rows = vars(database_name),
             cols = vars(age_strata)) +
  # scale_y_continuous(labels = scales::percent,
  #                    limits = c(0,NA)) +
  geom_line(aes(linetype = sex_strata)) +
  geom_point() +
  theme_bw() +
  labs(x = "Calendar year",
       y = "Incidence rate per 100000 person-years",
       col = "Database name",
       linetype = "Sex")

incidenceFigure2c









# incidenceFigure2 <- incidenceData %>%
#   filter(age_strata != "0;99",
#          sex_strata != "Both") %>%
#   ggplot(aes(x = age_strata,
#              y = ir_100000_pys,
#              col = database_name)) +
#   # scale_y_continuous(labels = scales::percent,
#   #                    limits = c(0,NA)) +
#   facet_grid(cols = vars(sex_strata),
#              rows = vars(time)) +
#   geom_line(aes(group = 1)) +
#   geom_point() +
#   geom_errorbar(aes(ymin = ir_100000_pys_low,
#                     ymax = ir_100000_pys_high)) +
#   theme_bw() +
#   labs(x = "Calendar year",
#        y = " Incidence rate per 100000 person-years")
#
# incidenceFigure2


# Example with mock data

# cdm <- mockIncidencePrevalenceRef(sampleSize = 50000)
#
# personCDM <- cdm$person
#
# personCDM <- as_tibble(personCDM)
#
# length(personCDM$person_id)

# Denominator population

# Figure 1: All databases, by year and month

# dpop <- collectDenominator(cdm = cdm,
#                            startDate  = as.Date("2000-01-01"),
#                            endDate  = as.Date("2012-01-01"),
#                            daysPriorHistory  = 365)
#
# cdm$denominator <- dpop$denominator_population
#
# incidence <- computeIncidence(
#   cdm = cdm,
#   denominatorTable = "denominator",
#   outcomeTable = "outcome",
#   outcomeId = "1",
#   interval = c("Years"),
#   outcomeWashout = 180
# )
#
# incidenceEstimates <- incidence$incidence_estimates
#
# colnames(incidenceEstimates)
#
# incidenceTable <- incidenceEstimates %>% select(Time = "time",
#                                                 `Number of persons` = "n_persons",
#                                                 `Person days`  = "person_days",
#                                                 `Incidence rate / 100000` = "ir_100000_pys")
#
# incidenceGraph <- incidenceEstimates %>%
#   left_join(incidence$analysis_settings,
#             by = "incidence_analysis_id") %>%
#   left_join(dpop$denominator_settings,
#             by=c("denominator_id" = "cohort_definition_id")) %>%
#   ggplot(aes(x = time, y = ir_100000_pys)) +
#   # scale_y_continuous(labels = scales::percent,
#   #                    limits = c(0,NA)) +
#   geom_line(aes(group = 1)) +
#   geom_point() +
#   geom_errorbar(aes(ymin = ir_100000_pys_low,
#                     ymax = ir_100000_pys_high)) +
#   # geom_ribbon(aes(ymin = ir_100000_pys_low,
#   #                 ymax = ir_100000_pys_high)) +
#   theme_bw() +
#   labs(x = "Year",
#        y = " Incidence rate per 100000 person-years")
#
# incidenceGraph

# Figure 2:

## males and females,

## One plot, all databases,

## by year and month options
#
# dpop <- collectDenominator(cdm = cdm,
#                            startDate  = as.Date("2008-01-01"),
#                            endDate  = as.Date("2012-01-01"),
#                            ageStrata  = list(c(0, 9),
#                                              c(10, 19),
#                                              c(20, 29),
#                                              c(30, 39),
#                                              c(40, 49),
#                                              c(50, 59),
#                                              c(60, 69),
#                                              c(80, 89),
#                                              c(90, 99)),
#                            sexStrata  = c("Female", "Male"),
#                            daysPriorHistory  = 365)

# Figure 2:

## age groups (x-axis)

## plot for each database,

## by year and month

# Figure 2b:

## agre group (x-axis)


# dpop <- collectDenominator(cdm = cdm,
#                            startDate  = as.Date("2008-01-01"),
#                            endDate  = as.Date("2012-01-01"),
#                            sexStrata  = c("Female", "Male"),
#                            daysPriorHistory  = 365)

# glimpse(dpop)

# Adding denominator population to cdm object

# cdm$denominator <- dpop$denominator_population

# Incidence calculation



# incidence <- computeIncidence(
#   cdm = cdm,
#   denominatorTable = "denominator",
#   outcomeTable = "outcome",
#   outcomeId = "1",
#   denominatorId  = c("1", "2"),
#   interval = c("Years"),
#   outcomeWashout = 180
# )


# incidence <- computeIncidence(
#   cdm = cdm,
#   denominatorTable = "denominator",
#   outcomeTable = "outcome",
#   outcomeId = "1",
#   denominatorId  = c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12", "13", "14", "15", "16", "17", "18"),
#   interval = c("Years"),
#   outcomeWashout = 180
# )
#
#
# head(incidence$incidence_estimates)

# incidence <- computeIncidence(
#   cdm = cdm,
#   denominatorTable = "denominator",
#   outcomeTable = "outcome",
#   outcomeId = "1",
#   denominatorId  = c("1", "2"),
#   interval = c("Years"),
#   outcomeWashout = 180
# )
#
# incidence %>%
#   glimpse()
#
# incidenceEstimates <- incidence$incidence_estimates
#
# colnames(incidenceEstimates)
#
# incidenceTable <- incidenceEstimates %>% select(Time = "time",
#                                                  `Number of persons` = "n_persons",
#                                                  `Person days`  = "person_days",
#                                                  `Incidence rate / 100000` = "ir_100000_pys")
#
# incidenceGraph <- incidenceEstimates %>%
#   left_join(incidence$analysis_settings,
#             by = "incidence_analysis_id") %>%
#   left_join(dpop$denominator_settings,
#             by=c("denominator_id" = "cohort_definition_id")) %>%
#   ggplot(aes(x = time, y = ir_100000_pys, group = sex_strata, col = sex_strata)) +
#   facet_grid(cols = vars(age_strata)) +
#   # scale_y_continuous(labels = scales::percent,
#   #                    limits = c(0,NA)) +
#   geom_line() +
#   geom_point() +
#   geom_errorbar(aes(ymin = ir_100000_pys_low,
#                     ymax = ir_100000_pys_high)) +
#   # geom_ribbon(aes(ymin = ir_100000_pys_low,
#   #                 ymax = ir_100000_pys_high)) +
#   theme_bw() +
#   labs(x = "Year",
#        y = " Incidence rate per 100000 person-years")
#
#

# Age and Sex

# incidenceGraph <- incidenceEstimates %>%
#   left_join(incidence$analysis_settings,
#           by = "incidence_analysis_id") %>%
#   left_join(dpop$denominator_settings,
#             by=c("denominator_id" = "cohort_definition_id")) %>%
#   ggplot(aes(x = time, y = ir_100000_pys, group = sex_strata, col = sex_strata)) +
#   facet_grid(cols = vars(age_strata)) +
#   # scale_y_continuous(labels = scales::percent,
#   #                    limits = c(0,NA)) +
#   geom_line() +
#   geom_point() +
#   geom_errorbar(aes(ymin = ir_100000_pys_low,
#                     ymax = ir_100000_pys_high)) +
#   # geom_ribbon(aes(ymin = ir_100000_pys_low,
#   #                 ymax = ir_100000_pys_high)) +
#   theme_bw() +
#   labs(x = "Year",
#        y = " Incidence rate per 100000 person-years")
#
# incidenceGraph

#
# incidenceEstimates %>%
#   ggplot(aes(x = time,
#              y = ir_100000_pys)) +
#   scale_y_continuous(labels = scales::percent,
#                      limits = c(0,NA)) +
#   geom_line() +
#   geom_point() +
#   theme_bw()
#
#
# incidenceEstimates <- incidenceEstimates %>%
#   left_join(incidence$analysis_settings,
#             by = "incidence_analysis_id") %>%
#   left_join(dpop$denominator_settings,
#             by=c("denominator_id" = "cohort_definition_id"))
#
# View(incidenceEstimates)
#
#
# incidenceGraph


# Prevalence calculation

# prevalence <- computePrevalence(
#   cdm = cdm,
#   denominatorTable = "denominator",
#   denominatorId  = c("1"),
#   outcomeTable = "outcome",
#   outcomeId  = "1",
#   interval ="Years",
#   type = "period",
#   minCellCount = 0
#   )

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
incidence <- incidence
prevalence <- prevalence



load_all()

## RUN

incidencePrevalenceReport(title,
                          author,
                          abstract,
                          denominator,
                          incidence,
                          prevalence,
                          format = "word")

