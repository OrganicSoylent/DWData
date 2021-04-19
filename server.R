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


#### Server ####
server <- function(input, output) { # , session
  #### data section ####
  withProgress(message = 'verarbeite Stationslexikon', value = 0, {
  ## "https://www.dwd.de/DE/leistungen/klimadatendeutschland/statliste/statlex_html.html?view=nasPublication&nn=16102"
  
  url <- "https://www.dwd.de/DE/leistungen/klimadatendeutschland/statliste/statlex_html.html?view=nasPublication&nn=16102"
  # url <- "https://www.dwd.de/DE/leistungen/klimadatendeutschland/statliste/statlex_html.html;jsessionid=D70931FBB12457D14A53133624C40537.live11054?view=nasPublication&nn=16102"
  webpage <- read_html(url)
  
  tabhead <- html_text(html_nodes(webpage,"th"))
  tabhead <- tabhead[2:12]
  nrows <- length(html_nodes(webpage,"tr"))
  nodes <- html_nodes(webpage, "tr")
  
  incProgress(1/4)
  ## multithreaded creation of station-list dataframe
  cluster = makeCluster(detectCores(), type = "SOCK")
  registerDoParallel(cluster)
  
  tab <- c()
  row <- strsplit(as.character(nodes[3:nrows]),"\n")

  tab <- foreach(i = 1:length(row), .combine = 'rbind') %do% {
      rbind(tab,gsub(c(">|<"),"", regmatches(row[[i]],regexpr(">.*.<",row[[i]])) ) )
    }
  stopCluster(cluster)
  
  tab <- as.data.frame(tab)
  colnames(tab)  <- c("Station","ID","Messprodukte","stat-kennung",
                      "Breite","Länge","Höhe","Flussgebiet","Bundesland","Beginn","Ende")
  # tab <- tab[,c(-4,-8)]
  tab$ID <- as.numeric(tab$ID)
  tab$Breite <- as.numeric(tab$Breite)
  tab$Länge <- as.numeric(tab$Länge)
  tab$Höhe <- as.numeric(tab$Höhe)
  tab$Beginn <- as.Date(tab$Beginn, "%d.%m.%Y")
  tab$Ende <- as.Date(tab$Ende, "%d.%m.%Y")
  
  tab2 <- suppressMessages(tab[, c(-4,-8)] %>%
    group_by(Station) %>%
    summarise(
      "Station" = unique(Station), 
      "ID" = unique(ID),
      "Messprodukte" = unique(paste(Messprodukte, collapse = ", ")), 
      "Breite" = unique(Breite),
      "Länge" = unique(Länge), 
      "Höhe" = unique(Höhe), 
      "Bundesland" = unique(Bundesland)) %>%
    as.data.frame())
  
  ## creating SpatialPointsDataframe for map (not necessary)
  # pt <- tab2
  # sp::coordinates(pt) <- ~Länge+Breite
  # sp::proj4string(pt) <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
  
  incProgress(2/4)
  
  
  ## altitude and time range sidebar menu
  output$altitude <- renderMenu({sliderInput("altitude", "Stationshöhe",
              min = min(tab$Höhe), max = max(tab$Höhe),
              value = c(min(tab$Höhe),max(tab$Höhe)), sep = NA)
  })
  
  output$daterange <- renderMenu({
  })
  
  

  #### Input Filters for table ####
  ## output is a function that creates a dataframe -> tb()[ , ]
  tb <- eventReactive(c(input$prod, input$altitude, input$daterange), {
    ## selection for product
    suppressMessages(inner_join(tab2[grep(paste(input$prod,collapse="|"), tab2$Messprodukte), ], ## 'paste(input$prod,collapse="|")' delivers regex compatible vector c("PE|RR|...")
    ## selection for altitude
               tab2[which(tab2$Höhe >= input$altitude[1] & 
                               tab2$Höhe <= input$altitude[2]), ]) %>%
      ## selection for time range
      inner_join(tab2[tab2$ID %in% unique(tab$ID[which(tab$Beginn <= input$daterange[1] &
                                                            tab$Ende >= input$daterange[2])]), ]) %>%
      as.data.frame()) # reactable can't work with dplyr/pandas type tables - needs dataframe
  })

  
  ### in table selected rows
  selected <- reactive(getReactableState("table", "selected"))
  
  pt <- eventReactive(selected(), {
    tab2[which(!is.na(match(tab2$ID, tb()[selected(),"ID"]))), ]
    })
  

  #### map ####
  output$mapplot <- renderLeaflet({
    leaflet() %>%
      setView(8.7862, 50.0899, zoom = 5) %>%
      addProviderTiles("OpenTopoMap",providers$Stamen.TonerLite,
                       options = providerTileOptions(noWrap = TRUE)) %>%
      addLegend(position = "bottomright", colors = c("blue","red"), 
                labels = c("Stationen mit ausgewählten Produkt(en)",
                           "Ausgewählte Stationen"))
  })
  observe({
      leafletProxy("mapplot") %>%
      clearMarkers() %>% # clears markers of previous selection on input update
      
      addCircleMarkers(data = tb(), lat = ~Breite, lng = ~Länge, 
                       color = "blue", radius = 1, opacity = .75, # fillOpacity = .33,
                       label = paste0("",tb()$Station),
                       popup = paste(sep = "<br/>",
                                     paste0("<b>",tb()$Station,"</b>"),
                                     paste("ID:",tb()$ID),
                                     paste("Höhe:",tb()$Höhe,"m"),
                                     paste("Messpr.:",tb()$Messprodukte)),
                       # paste("Land:",pt2()$Bundesland)),
                       popupOptions = list(closeButton = FALSE)) %>%
      
      addCircleMarkers(data = pt(), lat = ~Breite, lng = ~Länge,
                       color = "red", radius = 4, opacity = .75, # fillOpacity = .33
                       label = ~Station,
                       popup = paste(sep = "<br/>",
                                     paste0("<b>",pt()$Station,"</b>"),
                                     paste("ID:",pt()$ID),
                                     paste("Höhe:",pt()$Höhe,"m"),
                                     paste("Messpr.:",pt()$Messprodukte)),
                       # paste("Land:",pt2()$Bundesland)),
                       popupOptions = list(closeButton = FALSE))
    
  })


  incProgress(3/4)

  #### interactive table ####
  output$table <- reactable::renderReactable({
    reactable(tb()[,c(1,2,3,6,7)], 
              filterable = TRUE, searchable = TRUE, selection = "multiple", onClick = "select", 
                          striped = TRUE, highlight = TRUE, bordered = TRUE, outlined = TRUE,
              defaultPageSize = 10, showPageSizeOptions = TRUE,  pageSizeOptions = c(10, 25, 50, 100),
              # theme = reactableTheme(color = "white", backgroundColor = bgcol, borderColor = "#808080", stripedColor = strpcol),
              columns = list(
                ID = colDef(align = "left"),
                Höhe = colDef(name = "Höhe [m ü. NN]", align = "center"),
                Messprodukte = colDef(name = "verf. Messprodukte")
                # Breite = colDef(name = "Breite (lat)", filterable = FALSE, align = "right"),
                # Länge = colDef(name = "Länge (lon)", filterable = FALSE, align = "left")
                            ),
              language = reactableLang(
                              searchPlaceholder = "Suche...",
                              noData = "Keine Einträge gefunden",
                              pageInfo = "{rowStart} bis {rowEnd} von {rows} Einträgen",
                              pagePrevious = "\u276e",
                              pageNext = "\u276f",
                              pageSizeOptions = "Zeige {rows}",
                              pagePreviousLabel = "Vorherige Seite",
                              pageNextLabel = "Nächste Seite",
                              deselectRowLabel = "Auswahl aufheben"
                            )
    )
  }) # rendertable
  
  output$table2 <- reactable::renderReactable({
    reactable(tab[tab$ID %in% tb()[selected(),"ID"], c("Station","Messprodukte","Beginn","Ende")],
              groupBy = c("Station"),
              striped = TRUE, highlight = TRUE, outlined = TRUE, filterable = TRUE,
              # theme = reactableTheme(color = "white", backgroundColor = bgcol, stripedColor = strpcol),
              columns = list(Beginn = colDef(filterable = F), Ende = colDef(filterable = F)),
              language = reactableLang(
                searchPlaceholder = "Suche...",
                noData = "Keine Einträge gefunden",
                pageInfo = "{rowStart} bis {rowEnd} von {rows} Einträgen",
                pagePrevious = "\u276e",
                pageNext = "\u276f",
                pageSizeOptions = "Zeige {rows}",
                pagePreviousLabel = "Vorherige Seite",
                pageNextLabel = "Nächste Seite",
                deselectRowLabel = "Auswahl aufheben"
              )
    )
  }) # rendertable2
  
  incProgress(4/4)
  #### Testboxes for selected stations ####
  # output$rownr <- renderText({
  #   paste("Anzahl ausgewählter Stationen:", length(tb()[selected(),"ID"]) )
  # })
  

  output$rownr <- renderText({
    paste("Stationen ausgewählt:",length(selected()) )
  })
  # output$rownr <- renderText({
  #   length(tb()[,1])
  #   })
  
    # output$rownr <-  renderText(paste("Stationen ausgewählt:",
    #                                   length(tb()[input$DTtable_rows_selected, "ID"])) )
    # output$rownr <- renderText(substr(input$checkboxes,1,2))

    #### Function to read and store selected DWD data ####
    readDWD <- function(stat_id, resol, prod, time){
      baseURL <- "ftp://opendata.dwd.de/climate_environment/CDC/observations_germany/climate/"
      lookURL_h <- paste0(baseURL,resol,"/",prod,"/","historical","/")
      lookURL_r <- paste0(baseURL,resol,"/",prod,"/","recent","/")
      
      ## vector mit allen Files
      fls_h <- strsplit(RCurl::getURL(lookURL_h,dirlistonly = T), "\r\n")[[1]]
      fls_r <- strsplit(RCurl::getURL(lookURL_r,dirlistonly = T), "\r\n")[[1]]
      
      ## vector mit dateinamen der .zip dateien
      varnames_h <- strsplit(fls_h[min(which(grepl("*.zip",fls_h)))],"_")[[1]]
      varnames_r <- strsplit(fls_r[min(which(grepl("*.zip",fls_r)))],"_")[[1]]
      
      db <- c(1:length(stat_id))
      for(i in 1:length(stat_id)){
        ## IDs mit Länge > 5 nicht zugelassen
        if(nchar(stat_id[i])>5){stop("invalid station ID length")}
        ## Eingegebene IDs werden auf 5 Stellen vereinheitlicht
        stat_id[i] <- paste0(paste(rep("0",5-nchar(stat_id[i])),collapse = ""),stat_id[i])
        ## Auswahl der i-ten Station - Dateiname
        file_h <- fls_h[which(grepl(paste0("*_",stat_id[i],"_*"), fls_h))]
        file_r <- fls_r[which(grepl(paste0("*_",stat_id[i],"_*"), fls_r))]
        
        ## URL der i-ten zip-Datei
        endURL_h <- paste0(baseURL,resol,"/",prod,"/","historical","/",file_h)
        endURL_r <- paste0(baseURL,resol,"/",prod,"/","recent","/",file_r)
        
        ## Verzeichnis und Name der temporären Datei
        temp_h <- tempfile()
        temp_r <- tempfile()
        
        fln_h <- paste0(temp_h,"_",varnames_h[6])
        fln_r <- paste0(temp_r,"_",varnames_r[6])
        
        ## Download der zip-Datei in temp-Ordner
        download.file(url = endURL_h, destfile = fln_h)
        download.file(url = endURL_r, destfile = fln_r)
        
        ## Entpacken der temp zip-Datei
        file_h <-  unzip(fln_h, list=TRUE)
        file_r <-  unzip(fln_r, list=TRUE)
        
        ## Einlesen des Stationsproduktes, Ablegen in Datenbank
        tab_h <- read.table(unz(fln_h,file_h$Name[grep("produkt_",file_h$Name)]) ,
                            header=T, sep = ";")
        tab_r <- read.table(unz(fln_r,file_r$Name[grep("produkt_",file_r$Name)]) ,
                            header=T, sep = ";")
        tab <- rbind(tab_h,tab_r)
        db[i] <- list(tab)
        ## Löschung der temporären Datei und temp-filenames
        file.remove(fln_h)
        file.remove(fln_r)
        
        unlink(temp_h)
        unlink(temp_r)
        
      }
      return(db)
    }
    
    observeEvent(input$button, {
      output$id <- readDWD(stat_id = tb()[selected(),"ID"], resol = "daily", prod = "kl")
      })
    
    # kl <- readDWD(stat_id = output$rownr, resol = "daily", prod = "kl")
  
  # observe(
  #   output$hist <- renderPlot(
  #     hist(tab2[,"Stations.höhe"], breaks = 100, ylab = "Anzahl", xlab = "[m ü. NN]", main = NA)
  #   )
  # )
   
    
  }) # Progressbar
}


shinyApp(ui, server)





