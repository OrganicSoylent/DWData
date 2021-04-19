library(rvest)
library(RCurl)
library(DT)
library(reactable)
library(shiny)
library(shinyWidgets)
library(shinydashboard)
library(XML)
library(leaflet)
library(ggplot2)
library(tidyr)
library(dplyr)
library(foreach)
library(parallel)
library(doParallel)


header <- dashboardHeader(title = "DWData", titleWidth = 300
                          # dropdownMenu(type = "messages",
                          #              messageItem(
                          #                from = "Allergieinfo",
                          #                message = "Keine erhöhte Pollenkonz."
                          #              ),
                          #              messageItem(
                          #                from = "New User",
                          #                message = "How do I register?",
                          #                icon = icon("question"),
                          #                time = strftime(Sys.time(),"%H:%M:%S", tz = "UTC")
                          #              ),
                          #              messageItem(
                          #                from = "Support",
                          #                message = "The new server is ready.",
                          #                icon = icon("life-ring"),
                          #                time = strftime(Sys.Date(), "%d.%m.%Y", tz = "UTC")
                          #              )
                          # )
)
sidebar <- dashboardSidebar(width = 300, # tags$style(HTML(".main-sidebar{width: 300px;}"))
                            # menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
                            # menuItem("Widgets", tabName = "widgets", icon = icon("th")),
                            # menuItem("Stationsauswahl", tabName = "stationen", icon = icon("th"),
                            
                            checkboxGroupInput("prod", 
                                               label = "Messprodukte", 
                                               choiceValues = c("KL","RR","PE|PS","EB","FF","MI|MN","SO","SY","TU","AE"),
                                               # c("a", "b",  "c", "d", "e", "f", "f", "g", "h", "i", "j", "k", "l"),
                                               # "KL" "RR" "PE" "EB" "FF" "MI" "MN" "SO" "SY" "TU" "AE" "EF" "SF"
                                               inline = FALSE,
                                               selected = c("KL"), # ,"RR","PE|PS","EB","FF","MI|MN","SO","SY","TU","AE"
                                               choiceNames = c("KL : Klimadaten",
                                                               "RR : Niederschlagsdaten (tägl.)",
                                                               "PE/PS : phänologischen Beobachtungen",
                                                               "EB : Erdbodentemperatur (tägl.)",
                                                               "FF : Winddaten (stündl.)",
                                                               "MI/MN : autom. Messungen (10 min.)",
                                                               "SO : Sonnenscheindauer (stündl.)",
                                                               "SY : autom. Messungen (stündl.)",
                                                               "TU : Temperatur und rel. Feuchte (stündl.)",
                                                               "AE : aerologische Beobachtungen")
                                               ),
                            menuItemOutput("altitude"),
                            dateRangeInput("daterange",
                                           label = "gemessen im Zeitraum",
                                           start = Sys.Date()-365, end = Sys.Date()-7, # as.Date("1781-01-01")
                                           min = as.Date("1781-01-01"), max = Sys.Date(),
                                           startview = "year",
                                           separator = "bis", format = "dd.mm.yyyy", language = "de")
                            # actionLink("selectall", "Alle auswählen")

                            )
body <- dashboardBody(
  fluidPage(
    fluidRow(
      column(width = 6,
      box(reactable::reactableOutput("table"), title = "Zur Auswahl verfügbare Stationen", width = 12),
      box(verbatimTextOutput('rownr'), width = 12),
      box(actionButton("button","Download Auswahl"), width = 12)
      ),
      column(width = 6,
      box(leaflet::leafletOutput("mapplot"), title = "Karte", width = 12), # background = "blue"
      box(reactable::reactableOutput("table2"), title = "Messprodukt Informationen zu ausgewählten Stationen", width = 12)
      )
    )
      # 
      # # box(title = "Stationshöhe Histogramm",width = 4,
      # #     plotOutput("hist"))

    )
)

ui <- shinyUI(dashboardPage(skin = "blue", header, sidebar, body))

shinyApp(ui, server)

