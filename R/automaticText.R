#' Generic to create automatic text paragraph for the caption for table from summarise_characteristics object.
#'
#' @param summarisedCharacteristics Data frame with summarise_characteristics data
#'
#' @return Automatic text as a character string
#'
#' @import glue
#' @importFrom scales label_percent
autoCaptionCharac <- function(summarisedCharacteristics) {
  groupLevels <- glue::glue_collapse(unique(summarisedCharacteristics$group_level), ", ", last = " and ")
  result <- glue::glue("Demographic characteristics of {groupLevels} patients.")
  return(result)
}

#' Generates automatic text paragraph for Table 1
#'
#' @param incidence_attrition Data frame with incidence attrition data
#' @param prevalence_attrition Data frame with incidence attrition data
#'
#' @return Automatic text as a character string
#' @export
#'
#' @import glue
#' @importFrom scales label_percent
table1aAutText <- function(attrition_data) {

  tablePrevalenceAttTotal <- attrition_data %>%
    dplyr::filter(strata_name == "reason",
                  strata_level == "Starting population") %>%
    group_by(cdm_name,
             strata_name) %>%
    summarise(current_n = unique(as.numeric(estimate_value))) %>%
    filter(!is.na(current_n)) %>%
    arrange(desc(current_n))

  tablePrevalenceAttFem <- attrition_data %>%
    dplyr::filter(strata_name == "reason",
                  strata_level == "Not Male") %>%
    group_by(cdm_name,
             strata_name) %>%
    summarise(current_n = unique(as.numeric(estimate_value))) %>%
    filter(!is.na(current_n)) %>%
    arrange(desc(current_n))

  tablePrevNotObs <- attrition_data %>%
    dplyr::filter(strata_name == "reason",
                  strata_level == "Not observed during the complete database interval") %>%
    group_by(cdm_name,
             strata_name) %>%
    summarise(current_n = unique(as.numeric(estimate_value))) %>%
    filter(!is.na(current_n)) %>%
    arrange(desc(current_n))

  totalParticipants <- sum(tablePrevalenceAttTotal$current_n)
  totalParticipantsChar <- format(sum(tablePrevalenceAttTotal$current_n), big.mark=",", scientific = FALSE)
  minParticipants <- tablePrevalenceAttTotal$current_n[which.min(tablePrevalenceAttTotal$current_n)]
  minParticipantsChar <- format(tablePrevalenceAttTotal$current_n[which.min(tablePrevalenceAttTotal$current_n)], big.mark=",", scientific = FALSE)
  databaseNameMin <- tablePrevalenceAttTotal$cdm_name[which.min(tablePrevalenceAttTotal$current_n)]
  maxParticipantsChar <- format(tablePrevalenceAttTotal$current_n[which.max(tablePrevalenceAttTotal$current_n)], big.mark=",", scientific = FALSE)
  maxParticipants <- tablePrevalenceAttTotal$current_n[which.max(tablePrevalenceAttTotal$current_n)]
  databaseNameMax <- tablePrevalenceAttTotal$cdm_name[which.max(tablePrevalenceAttTotal$current_n)]
  minFemales <- tablePrevalenceAttFem  %>% dplyr::filter(cdm_name == databaseNameMin) %>% pull(current_n)
  maxFemales <- tablePrevalenceAttFem  %>% dplyr::filter(cdm_name == databaseNameMax) %>% pull(current_n)
  minFemaleProp <- (minFemales / as.numeric(minParticipants))
  maxFemaleProp <- (maxFemales / as.numeric(maxParticipants))
  minFemaleProp <- label_percent(accuracy = 0.01)(minFemaleProp)
  maxFemaleProp <- label_percent(accuracy = 0.01)(maxFemaleProp)

  numberDatabaseChar <- phraseList(initialPhrase = " of which ",
                                   adjective = "",
                                   totalParticipants,
                                   databaseName = tablePrevalenceAttTotal$cdm_name,
                                   individuals = tablePrevalenceAttTotal$current_n)

  femPropChar <- phraseList(initialPhrase = " From those, ",
                            adjective = "female",
                            totalParticipants,
                            databaseName = tablePrevalenceAttFem$cdm_name,
                            individuals = tablePrevalenceAttFem$current_n)

  excludedRec <- phraseList(initialPhrase = " A further ",
                            adjective = "excluded due to not having been observed for the complete database interval ",
                            totalParticipants,
                            databaseName = tablePrevNotObs$cdm_name,
                            individuals = tablePrevNotObs$current_n)


  autoText <- glue(
    "Table 1. A total of {totalParticipantsChar} participants were included in the study,",
    "{numberDatabaseChar}",
    "The starting populations ranged from {minParticipantsChar} ({label_percent(accuracy = 0.01)(minParticipants/totalParticipants)} in {databaseNameMin}) to {maxParticipantsChar} ({label_percent(accuracy = 0.01)(maxParticipants/totalParticipants)} in {databaseNameMax}).",
    "{femPropChar}",
    "{excludedRec}",
    .sep = " ")

  autoText <- gsub("  ", " ", autoText)
  autoText <- gsub("  ", " ", autoText)
  return(autoText)
}

#' Generates automatic text paragraph for Table Incidence Attrition
#'
#' @param attritionData Data frame with incidence attrition data
#'
#' @return Automatic text as a character string
#' @export
#'
#' @import glue
#' @importFrom scales label_percent
tableAttrition <- function(attrition_data) {

  tableAttTotal <- attrition_data %>%
    dplyr::filter(strata_name == "reason",
                  strata_level == "Starting population") %>%
    group_by(cdm_name,
             strata_name) %>%
    summarise(current_n = unique(as.numeric(estimate_value))) %>%
    filter(!is.na(current_n)) %>%
    arrange(desc(current_n))

  tableAttFem <- attrition_data %>%
    dplyr::filter(strata_name == "reason",
                  strata_level == "Not Male") %>%
    group_by(cdm_name,
             strata_name) %>%
    summarise(current_n = unique(as.numeric(estimate_value))) %>%
    filter(!is.na(current_n)) %>%
    arrange(desc(current_n))

  tableAttNotObs <- attrition_data %>%
    dplyr::filter(strata_name == "reason",
                  strata_level == "Not observed during the complete database interval") %>%
    group_by(cdm_name,
             strata_name) %>%
    summarise(current_n = unique(as.numeric(estimate_value))) %>%
    filter(!is.na(current_n)) %>%
    arrange(desc(current_n))

  totalParticipants <- sum(tableAttTotal$current_n)
  totalParticipantsChar <- format(sum(tableAttTotal$current_n), big.mark=",", scientific = FALSE)
  minParticipants <- tableAttTotal$current_n[which.min(tableAttTotal$current_n)]
  minParticipantsChar <- format(tableAttTotal$current_n[which.min(tableAttTotal$current_n)], big.mark=",", scientific = FALSE)
  databaseNameMin <- tableAttTotal$cdm_name[which.min(tableAttTotal$current_n)]
  maxParticipantsChar <- format(tableAttTotal$current_n[which.max(tableAttTotal$current_n)], big.mark=",", scientific = FALSE)
  maxParticipants <- tableAttTotal$current_n[which.max(tableAttTotal$current_n)]
  databaseNameMax <- tableAttTotal$cdm_name[which.max(tableAttTotal$current_n)]
  minFemales <- tableAttFem  %>% dplyr::filter(cdm_name == databaseNameMin) %>% pull(current_n)
  maxFemales <- tableAttFem  %>% dplyr::filter(cdm_name == databaseNameMax) %>% pull(current_n)
  minFemaleProp <- (minFemales / as.numeric(minParticipants))
  maxFemaleProp <- (maxFemales / as.numeric(maxParticipants))
  minFemaleProp <- label_percent(accuracy = 0.01)(minFemaleProp)
  maxFemaleProp <- label_percent(accuracy = 0.01)(maxFemaleProp)

  numberDatabaseChar <- phraseList(initialPhrase = " of which ",
                                   adjective = "",
                                   totalParticipants,
                                   databaseName = tableAttTotal$cdm_name,
                                   individuals = tableAttTotal$current_n)

  femPropChar <- phraseList(initialPhrase = " From those, ",
                            adjective = "female",
                            totalParticipants,
                            databaseName = tableAttFem$cdm_name,
                            individuals = tableAttFem$current_n)

  excludedRec <- phraseList(initialPhrase = " A further ",
                            adjective = "excluded due to not having been observed for the complete database interval ",
                            totalParticipants,
                            databaseName = tableAttNotObs$cdm_name,
                            individuals = tableAttNotObs$current_n)


  autoText <- glue(
    "Table 1. A total of {totalParticipantsChar} participants were included in the study,",
    "{numberDatabaseChar}",
    "The starting populations ranged from {minParticipantsChar} ({label_percent(accuracy = 0.01)(minParticipants/totalParticipants)} in {databaseNameMin}) to {maxParticipantsChar} ({label_percent(accuracy = 0.01)(maxParticipants/totalParticipants)} in {databaseNameMax}).",
    "{femPropChar}",
    "{excludedRec}",
    .sep = " ")

  autoText <- gsub("  ", " ", autoText)
  autoText <- gsub("  ", " ", autoText)
  return(autoText)
}

phraseList <- function(initialPhrase,
                       adjective = "",
                       totalParticipants,
                       databaseName,
                       individuals) {

  if (length(databaseName) == 1) {

    firstPhrase <- paste0(initialPhrase,
                          format(individuals[1], big.mark=",", scientific = FALSE),
                          " (",
                          label_percent(accuracy = 0.01)(individuals[1]/totalParticipants[1]),
                          ") ",
                          " were ",
                          adjective,
                          " from ",
                          databaseName[1],
                          ". ",
                          collapse = ", ")

    result <- gsub("  ", " ", paste(firstPhrase))


  } else if (length(databaseName) == 2) {

    firstPhrase <- paste0(initialPhrase,
                          format(individuals[1], big.mark=",", scientific = FALSE),
                          " (",
                          label_percent(accuracy = 0.01)(individuals[1]/totalParticipants[1]),
                          ") ",
                          " were ",
                          adjective,
                          " from ",
                          databaseName[1],
                          ", ",
                          collapse = ", ")

    secondPhrase <- paste0(" and ",
                          paste0(format(tail(individuals, 1),
                                        big.mark=",",
                                        scientific = FALSE),
                                 " (",
                                 label_percent(accuracy = 0.01)(tail(individuals, 1)/totalParticipants),
                                 ") ",
                                 "from ",
                                 tail(databaseName, 1),
                                 "."))

    result <- gsub("  ", " ", paste(firstPhrase, secondPhrase))

  } else if (length(databaseName) >= 3) {

    databaseNameMid <- head(tail(databaseName, -1), -1)
    individualsMid <- head(tail(individuals, -1), -1)

    firstPhrase <- paste0(initialPhrase,
                          format(individuals[1], big.mark=",", scientific = FALSE),
                          " (",
                          label_percent(accuracy = 0.01)(individuals[1]/totalParticipants[1]),
                          ") ",
                          " were ",
                          adjective,
                          " from ",
                          databaseName[1],
                          ", ",
                          collapse = ", ")

    secondPhrase <- paste0(format(individualsMid,
                                   big.mark=",",
                                   scientific = FALSE),
                            " (",
                            label_percent(accuracy = 0.01)(individualsMid/totalParticipants),
                            ") ",
                            "from ",
                            databaseNameMid,
                            collapse = ", ")
    secondPhrase <- paste0(secondPhrase, ",")

    thirdPhrase <- paste0(" and ",
                          paste0(format(tail(individuals, 1),
                                 big.mark=",",
                                 scientific = FALSE),
                          " (",
                          label_percent(accuracy = 0.01)(tail(individuals, 1)/totalParticipants),
                          ") ",
                          "from ",
                          tail(databaseName, 1),
                          "."))
    result <- gsub("  ", " ", paste(firstPhrase, secondPhrase, thirdPhrase))

  }

  return(result)

}
