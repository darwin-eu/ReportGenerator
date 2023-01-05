# Installation

library(devtools)

# install()

load_all()

# Generate mock data

# mockSampleCSV()

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

## RUN

incidencePrevalenceReport(title = title,
                          authors = authors,
                          authorsInstitution = authorsInstitution,
                          abstractText = abstractText,
                          byCondition = TRUE,
                          format = "word")

# Launch app

resultsDashboard()


