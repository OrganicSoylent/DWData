library(rvest)
library(RCurl)
library(DT)
library(shiny)
library(shinydashboard)
library(mapview)
library(leaflet)
library(ggplot2)
library(tidyr)
library(dplyr)

# shinyApp(ui, server)

tab <- read.table("C:/Users/gabba/Desktop/Dasboard/statlist.csv", header = T, sep = ",")
# tab <- tab[,c(3,4,2,1,5:11)]
load("C:/Users/gabba/Desktop/Dasboard/statlex.RData")

# tab2 <- tab[-which(tab$Beginn == ""),c(1,2,5,6,7,9,10,11)]
# tab2 <- tab2[grep(FALSE,duplicated(tab2$Stations_ID)),]

cd <- data.frame(dwd = unique(tab$Kennung), code = c("a","b","c","d","e","f","f","g","h","i","j","k","l"))
tab$code <- NA
for(i in 1:length(unique(tab$Kennung))){
  tab$code[which(tab$Kennung == unique(tab$Kennung)[i])] <- cd[i,2]
}


{
id <- c()
kenn <- c()
alt <- c()
bund <- c()
lon <- c()
lat <- c()
for(i in 1:length(new.db)){
  id[i] <- new.db[[i]]$Stations_ID[1]
  kenn[i] <- paste(new.db[[i]]$Kennung[1:length(new.db[[i]]$Kennung)], collapse = ", ")
  alt[i] <- new.db[[i]]$`Stations-höhe`[1]
  bund[i] <- new.db[[i]]$Bundesland[1]
  lon[i] <- new.db[[i]]$Länge[1]
  lat[i] <- new.db[[i]]$Breite[1]
}
}

length(unique(tab$Kennung))

tab2 <- data.frame("Station" = names(new.db), 
                   "ID" = as.numeric(id), 
                   "Produkte" = kenn,
                   "m.ü.NN" = as.numeric(alt),
                   "Latitude" = as.numeric(lat),
                   "Longitude" = as.numeric(lon), 
                   "Bundesland" = bund)

pt <- tab2
sp::coordinates(pt) <- ~Longitude+Latitude
sp::proj4string(pt) <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"

tab2$Station[rowSums(sapply(tab2, '%in%', "KL")) > 0]

grep("KL",tab2$Produkte)

tab$Stationsname[which(tab$Kennung %in% "KL")]

which(tab2$ID %in% unique(tab$Stations_ID[which(tab$Kennung %in% substr(products,1,2)[1:3])]) )




#### Server ####
server <- function(input, output, session) {
  output$menu <- renderMenu(
    sliderInput("range", "Stationshöhe [m. ü. NN]",
                min = min(tab2$m.ü.NN), max = max(tab2$m.ü.NN),
                value = c(min(tab2$m.ü.NN),max(tab2$m.ü.NN)), sep = NA)
  )
  
  # t1 <- as.Date(tab2$Beginn,"%d.%m.%Y")
  # t2 <- as.Date(tab2$Ende,"%d.%m.%Y")
  
  # output$menu2 <- renderMenu(
  #   sliderInput("range2", "Zeitraum",
  #               min = min(as.numeric(strftime(t1,"%Y"))), max = max(as.numeric(strftime(t2,"%Y"))),
  #               value = c(min(as.numeric(strftime(t1,"%Y"))),max(as.numeric(strftime(t2,"%Y")))), sep = "")
  # )
  

  pt2 <- eventReactive(c(input$range,input$range2), 
                       pt[which(pt$m.ü.NN >= input$range[1] & 
                                  pt$m.ü.NN <= input$range[2]), ]) # & 
                                  # as.numeric(strftime(as.Date(pt$Beginn,"%d.%m.%Y"),"%Y")) >= input$range2[1]  &
                                  # as.numeric(strftime(as.Date(pt$Beginn,"%d.%m.%Y"),"%Y")) <= input$range2[2]), ])
  
  output$mapplot <- renderLeaflet({
    leaflet() %>% 
      addProviderTiles("OpenTopoMap",providers$Stamen.TonerLite,
                       options = providerTileOptions(noWrap = TRUE)) %>%
      addCircleMarkers(data = pt2(), radius = 1.3, fillOpacity = 1, label = pt2()$Station,
                       popup = paste(sep = "<br/>",
                                     paste0("<b>",pt2()$Station,"</b>"),
                                     paste("ID:",pt2()$ID),
                                     paste("Höhe:",pt2()$m.ü.NN,"m"),
                                     paste("Land:",pt2()$Bundesland),
                                     pt2()$Produkte),
                                     # paste("Beginn:",pt2()$Beginn),
                                     # paste("Ende:",pt2()$Ende)), 
                       popupOptions = list(closeButton = FALSE))
  })
  # output$mapplot <- mapview::renderMapview(mapview(pt,
  #           legend = F, map.types = c("OpenStreetMap","OpenTopoMap"),
  #           layer.name = "aktive DWD Stationen",
  #           label = pt$Stationsname, lwd = 1, cex = 3)
  #   )
  tb <- eventReactive(c(input$range,input$range2), 
                      tab2[which(tab2$m.ü.NN >= input$range[1]  &
                                   tab2$m.ü.NN <= input$range[2]), ]) # &
                                   # as.numeric(strftime(as.Date(tab2$Beginn,"%d.%m.%Y"),"%Y")) >= input$range2[1]  &
                                   # as.numeric(strftime(as.Date(tab2$Beginn,"%d.%m.%Y"),"%Y")) <= input$range2[2]), ])
  # observe(
    output$DTtable <- DT::renderDataTable(
      datatable(tb(),
                escape = 0,
                rownames = F, 
                extensions = "Buttons",
                colnames = c("Station","Stations-ID","Messprodukt","Höhe [m ü. NN]","Breite (lat)","Länge (lon)","Bundesland"),
                selection = list(mode = "multiple", target = "row"),
                options = list(pageLength = 15, dom = "lfrtBip",
                               buttons = list("copy","csv"),
                               language = 
                                 list(search = "Suche:",
                                      sLengthMenu = "Zeige _MENU_ Ergebnisse pro Seite",
                                      sZeroRecords = "Nichts gefunden",
                                      sInfo = "Zeige _START_ bis _END_ aus _TOTAL_ Einträgen",
                                      sInfoEmpty = "Zeige 0 bis 0 von 0 Einträgen",
                                      sInfoFiltered = "(aus _MAX_ gefiltert)",
                                      sProcessing = "Bearbeite...",
                                      oPaginate = list(sPrevious = "Vorherige",
                                                       sNext = "Nächste",
                                                       sLast = "Letzte",
                                                       sFirst = "Erste")))
                ) )
  # )
  
    # output$rownr <-  renderText(paste("Stationen ausgewählt:",
    #                                   length(tb()[input$DTtable_rows_selected, "ID"])) )
    output$rownr <- renderText(substr(input$checkboxes,1,2))

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
      output$id <- readDWD(stat_id = tb()[input$DTtable_rows_selected, "Stations_ID"], resol = "daily", prod = "kl")
      })
    
    
    
    # kl <- readDWD(stat_id = output$rownr, resol = "daily", prod = "kl")
  
  # observe(
  #   output$hist <- renderPlot(
  #     hist(tab2[,"Stations.höhe"], breaks = 100, ylab = "Anzahl", xlab = "[m ü. NN]", main = NA)
  #   )
  # )
}


shinyApp(ui, server)

