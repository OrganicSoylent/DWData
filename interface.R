library(rvest)
library(RCurl)
library(DT)
library(shiny)
library(shinydashboard)
library(mapview)
library(leaflet)
library(ggplot2)

products <- c("TU : Temperatur und rel. Feuchte (stündl.)",
              "RR : Niederschlagsdaten (tägl.)",
              "FF : Winddaten (stündl.)",
              "SO : Sonnenscheindauer (stündl.)",
              "EB : Erdbodentemperatur (tägl.)",
              "KL : Klimadaten",
              "AE : aerologische Beobachtungen",
              "PE/PS : phänologischen Beobachtungen",
              "SY : autom. Messungen (stündl.)",
              "MI/MN : autom. Messungen (10 min.)")


header <- dashboardHeader(title = "Stationslexikon",
                          dropdownMenu(type = "messages",
                                       messageItem(
                                         from = "Allergieinfo",
                                         message = "Keine erhöhte Pollenkonz."
                                       ),
                                       messageItem(
                                         from = "New User",
                                         message = "How do I register?",
                                         icon = icon("question"),
                                         time = strftime(Sys.time(),"%H:%M:%S", tz = "UTC")
                                       ),
                                       messageItem(
                                         from = "Support",
                                         message = "The new server is ready.",
                                         icon = icon("life-ring"),
                                         time = strftime(Sys.Date(), "%d.%m.%Y", tz = "UTC")
                                       )
                          )
)
sidebar <- dashboardSidebar(width = 300, # tags$style(HTML(".main-sidebar{width: 300px;}"))
                            menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
                            # menuItem("Widgets", tabName = "widgets", icon = icon("th")),
                            # menuItem("Stationsauswahl", tabName = "stationen", icon = icon("th"),
                            sidebarMenuOutput("menu"),
                            checkboxGroupInput("checkboxes", 
                                               label = "Stationsprodukte", 
                                               choiceValues = c("TU","RR","FF","SO","EB","KL","AE","PE/PS","SY","MI/MN"),
                                               c("a", "b",  "c", "d", "e", "f", "f", "g", "h", "i", "j", "k", "l"),
                                               # "KL" "RR" "PE" "EB" "FF" "MI" "MN" "SO" "SY" "TU" "AE" "EF" "SF"
                                               inline = FALSE,
                                               selected = 10,
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
                            actionLink("selectall", "Alle auswählen"),
                            dateRangeInput('dateRange2',
                                           label = "Zeitraum von - bis",
                                           start = as.Date("1781-01-01"), end = Sys.Date() - 1,
                                           min = as.Date("1781-01-01"), max = Sys.Date(),
                                           separator = " - ", format = "dd.mm.yyyy")
)
body <- dashboardBody(
  fluidPage(
    fluidRow(
      box(DT::dataTableOutput("DTtable"), width = 8),
      box(leaflet::leafletOutput("mapplot"), width = 4)
    ),
    fluidRow(
      box(verbatimTextOutput('rownr'), width = 8),
    ),
      # 
      # # box(title = "Stationshöhe Histogramm",width = 4,
      # #     plotOutput("hist"))
    fluidRow(
      box(actionButton("button","Download Auswahl"))
    )
))

ui <- shinyUI(dashboardPage(skin = "blue", header, sidebar, body))


