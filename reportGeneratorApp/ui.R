#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
library(DT)

# Module for characteristics UI
source("modules/attModules.R", local = TRUE)
source("modules/csModules.R", local = TRUE)
source("modules/IncModules.R", local = TRUE)
source("modules/ppModules.R", local = TRUE)
source("modules/prevModules.R", local = TRUE)
source("modules/standardTables.R", local = TRUE)
source("modules/tableModules.R", local = TRUE)
source("modules/tabPanelSelection.R", local = TRUE)
source("modules/tpModules.R", local = TRUE)
source("modules/automaticText.R", local = TRUE)
source("global.R", local = TRUE)

ui <- dashboardPage(
  dashboardHeader(title = "ReportGenerator"),
  dashboardSidebar(
    sidebarMenu(
      # uiOutput("navPanelPreview")
      menuItem(
        text = "Home",
        tabName = "home"
      ),
      menuItem(
        text = "Study Results",
        tabName = "study_results"
      )
    )
  ),
  dashboardBody(
    tabItems(
    tabItem(
      tabName = "home",
      h4("DARWIN EU® - Study Results"),
      h5(tags$a(href="https://www.encepp.eu/encepp/viewResource.htm?id=107783",
                "EU PAS Register",
                target="_blank")),
    ),
    tabItem(
      tabName = "study_results",
      tabsetPanel(
        id = "mainPanel",
        tabPanel("Results",
                 fluidRow(
                   column(width = 12,
                          shinycssloaders::withSpinner(uiOutput("navPanelPreview"))
                   )
                 )
        )
      )
    )
    )
  )
)
