# Installation

library(devtools)

document()

check()

# install()

load_all()

##### STUDY REPORT LAUNCH

title <- "Incidence Prevalence Report Generator Test"
authors <- c("Jane Roe",
             "John Roe",
             "Richard Roe")
authorsInstitution <- c("Erasmus MC",
                        "Oxford University",
                        "Erasmus MC")
abstractText <- "ABSTRACT: Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat."
# denominator <- dpop
# incidence <- incidence
# prevalence <- prevalence



load_all()

## RUN

incidencePrevalenceReport(title = title,
                          authors = authors,
                          authorsInstitution = authorsInstitution,
                          abstractText = abstractText,
                          byCondition = TRUE,
                          format = "word")

studyReportApp()


