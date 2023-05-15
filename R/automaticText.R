#' Generates automatic text paragraph for Table 1
#'
#' @param incidence_attrition Data frame with incidence attrition data
#' @param prevalence_attrition Data frame with incidence attrition data
#'
#' @return Automatic text as a character string
#' @export
#'
#' @import rLDCP here glue
table1aAutText <- function(incidence_attrition, prevalence_attrition) {

  # variables for aut text

  incidence_attrition <- read.csv("C:\\Users\\cbarboza\\AppData\\Local\\Temp\\RtmpMR0civ//csvFilesFolder/incidence_attrition.csv")
  prevalence_attrition <- read.csv("C:\\Users\\cbarboza\\AppData\\Local\\Temp\\RtmpMR0civ//csvFilesFolder/prevalence_attrition.csv")

  names(incidence_attrition)



  tablePrevalenceAtt <- prevalence_attrition %>%
    filter(reason == "Starting population") %>%
    group_by(database_name,
             reason) %>%
    summarise(current_n = unique(number_subjects))

  totalParticipants <- format(sum(tablePrevalenceAtt$current_n), big.mark=",", scientific=FALSE)

  autoText <- cat(totalParticipants)

  glue("Table 1. A total of {totalParticipants} study participants were included from all databases combined.",
       "The starting populations ranged from minParticipants (in databaseNameMin) to XXX (in databaseMax).",
       "Of those, XXX and XXX were female, and had been included in the database for at least 1 year prior to the index date.",
       "XXX and XXX were excluded from [insert database] [insert database] from the prevalence calculations due to not being observed for at least a full calendar year (January to December) during the study period.",
       .sep = " ")

  # Table 1: "A total of {totalParticipants} study participants were
  # included from all databases combined. The starting populations ranged from
  # minParticipants (in databaseNameMin) to XXX (in databaseMax). Of those, XXX and
  # XXX were female, and had been included in the database for at least 1 year
  # prior to the index date. XXX and XXX were excluded from [insert database]
  # and [insert database] from the prevalence calculations due to not being
  # observed for at least a full calendar year (January to December) during
  # the study period. XXX and XXX were excluded from [insert database] and
  # [insert database] from the incidence calculations due to having a previous
  # prescription of XXX. A further XXX and XXX were excluded from [insert
  # database] and [insert database] (depends on how many databases are included
  # in Table 1) due to not having been observed for a complete database interval
  # (at least one full calendar year, different for incidence vs prevalence,
  # depending on the specified database interval requirement (which could be
  # different for these parameters)


  tablePrevalenceAtt <- tablePrevalenceAtt[,-1]

  tablePrevalenceAtt <- tablePrevalenceAtt %>%
    select(analysis_step, everything())



  # Style Guide example

  # Table 1: "A total of totalParticipants study participants were
  # included from all databases combined. The starting populations ranged from
  # minParticipants (in databaseNameMin) to XXX (in databaseMax). Of those, XXX and
  # XXX were female, and had been included in the database for at least 1 year
  # prior to the index date. XXX and XXX were excluded from [insert database]
  # and [insert database] from the prevalence calculations due to not being
  # observed for at least a full calendar year (January to December) during
  # the study period. XXX and XXX were excluded from [insert database] and
  # [insert database] from the incidence calculations due to having a previous
  # prescription of XXX. A further XXX and XXX were excluded from [insert
  # database] and [insert database] (depends on how many databases are included
  # in Table 1) due to not having been observed for a complete database interval
  # (at least one full calendar year, different for incidence vs prevalence,
  # depending on the specified database interval requirement (which could be
  # different for these parameters)

  input1 <- read.csv(system.file(file = "extdata/data.csv", package = "rLDCP"), sep=";", dec=".", header=TRUE)

  values <- c(50, 950)

  input1 <- rbind(input1, values)



  # data definition

  temperature <- c(t(input1[1]))

  light <- c(t(input1[2]))

  # Data Structure definition

  input <- c()

  my_method <- function (input){
    return(input)
  }

  my_data <- data_structure(input, my_method)

  # CP definition

  cp_temp <- cp("cp_temp", c("cold", "warm", "hot", "burning hell"))
  cp_light <- cp("cp_light", c("low", "medium", "high", "extremely high"))
  cp_comfort <- cp("cp_comfort", c("very comfortable", "comfortable", "uncomfortable", "extremely uncomfortable"))

  # PMs definition

  g_pm_temp<- function(u, y){
    y$w <- degree_mf(fuzzy_partitions(trapezoid_mf(-10, -10, 10, 20),
                                      triangle_mf(10, 20, 25),
                                      triangle_mf(20, 30, 35),
                                      trapezoid_mf(30, 35, 60, 60)), u)
    # y$w <- degree_mf(fuzzy_partitions(trapezoid_mf(-10, -10, 10, 20),
    #                                   triangle_mf(10, 20, 25),
    #                                   trapezoid_mf(20, 25, 40, 40)), u)
    return(y)
    }

  t_pm_temp<- function(y){
    templates <- c(" the temperature is cold ", " the temperature is warm ", " the temperature is hot ", " the temperature is extremely hot ")
    return(templates[which.max(y$w)])
  }

  pm_temp <- pm(y = cp_temp, g = g_pm_temp, t = t_pm_temp)

  g_pm_light<- function(u,y){
    y$w <- degree_mf(fuzzy_partitions(trapezoid_mf(0, 0, 300, 500) ,
                                      triangle_mf(300, 500, 700),
                                      triangle_mf(500, 700, 900),
                                      trapezoid_mf(700, 900, 1000, 1000) ), u)
    y }

  t_pm_light<- function(y){
    templates <- c(" the light intensity is low", " the light intensity is medium", " the light intensity is high", " the light intensity is extremely high")
    return(templates[which.max(y$w)])
  }

  pm_light <- pm(y = cp_light, g = g_pm_light, t = t_pm_light)

  # cp_temp <- cp("cp_temp", c("cold", "warm", "hot", "burning hell"))
  # cp_light <- cp("cp_light", c("low", "medium", "high", "extremely high"))
  # cp_comfort <- cp("cp_comfort", c("very comfortable", "comfortable", "uncomfortable", "extremely uncomfortable"))

  g_pm_comfort<- function(u,y){
    operator <- operator(min, max)
    y$w <- infer_rules(fuzzy_rules(fuzzy_rule(1, 0, 0, 0,  1, 0, 0, 0,  0, 0, 1, 0),
                                   fuzzy_rule(0, 1, 0, 0,  1, 0, 0, 0,  0, 1, 0, 0),
                                   fuzzy_rule(0, 0, 1, 0,  1, 0, 0, 0,  0, 0, 1, 0),
                                   fuzzy_rule(0, 0, 0, 1,  1, 0, 0, 0,  0, 0, 0, 1),

                                   fuzzy_rule(1, 0, 0, 0,  0, 1, 0, 0,  0, 0, 1, 0),
                                   fuzzy_rule(0, 1, 0, 0,  0, 1, 0, 0,  0, 1, 0, 0),
                                   fuzzy_rule(0, 0, 1, 0,  0, 1, 0, 0,  0, 0, 1, 0),
                                   fuzzy_rule(0, 0, 0, 1,  0, 1, 0, 0,  0, 0, 0, 1),


                                   fuzzy_rule(1, 0, 0, 0,  0, 0, 1, 0,  0, 0, 1, 0),
                                   fuzzy_rule(0, 1, 0, 0,  0, 0, 1, 0,  0, 1, 0, 0),
                                   fuzzy_rule(0, 0, 1, 0,  0, 0, 1, 0,  0, 0, 1, 0),
                                   fuzzy_rule(0, 0, 0, 1,  0, 0, 1, 0,  0, 0, 0, 1),

                                   fuzzy_rule(1, 0, 0, 0,  0, 0, 0, 1,  0, 0, 1, 0),
                                   fuzzy_rule(0, 1, 0, 0,  0, 0, 0, 1,  0, 1, 0, 0),
                                   fuzzy_rule(0, 0, 1, 0,  0, 0, 0, 1,  0, 0, 1, 0),
                                   fuzzy_rule(0, 0, 0, 1,  0, 0, 0, 1,  0, 0, 0, 1)), operator,
                       list(u[[1]]$w,u[[2]]$w ))
    return(y)
    }

  t_pm_comfort<- function(y){
    templates <- c("The room is very comfortable", "The room is comfortable", "The room is uncomfortable", "The room is extremely uncomfortable")
    return(templates[which.max(y$w)])
  }

  pm_comfort <- pm(y = cp_comfort, g = g_pm_comfort, t = t_pm_comfort)

  # GLMP definition

  glmp_method <- function(pm, input){
    pm$pm_temp  <- pm_infer(pm$pm_temp, input[1])
    pm$pm_light  <- pm_infer(pm$pm_light, input[2])
    pm$pm_comfort  <- pm_infer(pm$pm_comfort, list(pm$pm_temp$y, pm$pm_light$y))
    return(pm)
  }
  my_glmp <- glmp(list(pm_temp = pm_temp, pm_light = pm_light, pm_comfort = pm_comfort), glmp_method)

  # Report Template definition

  report_method <- function(properties, pm){
    paste(pm_report(pm$pm_comfort), ", because", pm_report(pm$pm_temp),
          "and",
          pm_report(pm$pm_light),
          ".",
          sep="")}
  properties = NULL

  my_report <- report_template(properties, report_method)

  # LDCP definition

  my_ldcp <- ldcp(data = my_data, glmp = my_glmp, report=my_report)

  my_ldcp <- ldcp_run(ldcp=my_ldcp, input=current_input)

  for(i in 1:length(temperature)){
    current_input <- c(temperature[i],light[i])
    my_ldcp <- ldcp_run(ldcp=my_ldcp,input=current_input)
    cat("Input: c(temperature,light), c(", paste(current_input, collapse=",", sep=""), ")", sep="", fill = TRUE)
    cat("Output: ",my_ldcp$report$description,fill = TRUE)
    cat(fill = TRUE)
    #file.show("report.pdf")
  }

}
