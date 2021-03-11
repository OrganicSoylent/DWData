library(rvest)
library(RCurl)
library(DT)
library(shiny)
library(shinydashboard)
library(mapview)
library(leaflet)
library(ggplot2)

tab <- read.table("C:/Users/gabba/Desktop/Dasboard/statlist.csv", header = T, sep = ",")

tab2 <- tab[-which(tab$Beginn == ""),c(1,2,5,6,7,9,10,11)]
tab2 <- tab2[grep(FALSE,duplicated(tab2$Stations_ID)),]

pt <- tab2
sp::coordinates(pt) <- ~Länge+Breite
sp::proj4string(pt) <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"

# SHINYDASHBOARD ----------------------------------------------------------

shinyApp(ui, server)


###################################################################

ui <- fluidPage(
  titlePanel("Stationslexikon"),
  sidebarLayout(
    sidebarPanel(
      leaflet::leafletOutput("mapplot"),
      plainview::plainViewOutput("test")
    ),
    
    mainPanel(
      DT::dataTableOutput("DTtable"),
      DT::dataTableOutput("selectTable")
    )
  )
)

server <- function(input, output) {
  output$mapplot <- mapview::renderMapview({
    mapview(pt,legend = F, map.types = c("OpenStreetMap","OpenTopoMap"), 
            layer.name = "aktive DWD Stationen", 
            label = pt$Stationsname, lwd = 1, cex = 2.5)
  })
  output$DTtable <- DT::renderDataTable({
    datatable(tabtab, rownames = F, # extensions = "Buttons",
              options = list(dom = "Bfrtip", pageLength = 10 # ,
                             # buttons = list("copy","csv"),
              )
    ) })
}
shinyApp(ui, server)
###########################################################################



# callback = "function(table) {
#       table.on('click.dt', 'tr', function() {
#             table.$('tr.selected').removeClass('selected');
#             $(this).toggleClass('selected');            
#         Shiny.onInputChange('rows',
#                             table.rows('.selected').data()[0][0]);
#       });
#     }"
# output$DTtable <- DT::renderDataTable({
#   datatable(tabtab, rownames = F, 
#             options = list(
#               language = list(search = "Suche:",sLengthMenu = "Zeige _MENU_ Ergebnisse pro Seite",
#                               sZeroRecords = "Nichts gefunden", 
#                               sInfo = "Zeige _START_ bis _END_ aus _TOTAL_ Einträgen",
#                               sInfoEmpty = "Zeige 0 bis 0 von 0 Einträgen",
#                               sInfoFiltered = "(aus _MAX_ gefiltert)", sProcessing = "Bearbeite...",
#                               oPaginate = list(sPrevious = "Vorherige",sNext = "Nächste",
#                                                sLast = "Letzte", sFirst = "Erste")),
#               pageLength = 10,
#               headerCallback = JS(headerCallback)))
# })


# $('#TableName').DataTable({
#   "language": {
#     "sProcessing":    "Bearbeite...",
#     "sLengthMenu":    "Zeige _MENU_ Ergebnisse",
#     "sZeroRecords":   "keine Einträge",
#     "sEmptyTable":    "In dieser Tabelle sind keine Daten verfügbar",
#     "sInfo":          "Zeige _START_ von _END_ aus _TOTAL_ Einträgen",
#     "sInfoEmpty":     "Zeige 0 von 0 aus 0 Einträgen",
#     "sInfoFiltered":  "(Aus _MAX_ gefiltert)",
#     "sInfoPostFix":   "",
#     "sSearch":        "Suche:",
#     "sUrl":           "",
#     "sInfoThousands":  ",",
#     "sLoadingRecords": "Lade...",
#     "oPaginate": {
#       "sFirst":    "Erste",
#       "sLast":    "Letzte",
#       "sNext":    "Nächste",
#       "sPrevious": "Vorherige"
#     },
#     "oAria": {
#       "sSortAscending":  ": Aufsteigende Sortierung",
#       "sSortDescending": ": Absteigende Sortierung"
#     }
#   }
# });


# headerCallback <- c(
#   "function(thead, data, start, end, display){",
#   "  var tooltips = ['tooltip1','tooltip2','tooltip3','tooltip4','tooltip5'];",
#   "  for(var i=0; i<5; i++){",
#   "    $('th:eq('+i+')',thead).attr('title', tooltips[i]);",
#   "  }",
#   "}"
# )


# headerCallback <- c(
#   "function(thead, data, start, end, display){",
#   "  var tooltip3 = [
#   'AE = Stationen mit aerologischen Beobachtungen\\n'+
#   'EB = Stationen mit täglichen Daten der Erdbodentemperatur\\n'+
#   'FF = Stationen mit stündlichen Winddaten\\n'+
#   'KL = Stationen mit Klimadaten\\n'+
#   'MI/MN = Stationen mit automatischen Messungen\\n'+
#   'PE/PS = Stationen mit phänologischen Beobachtungen\\n'+
#   'RR = Stationen mit täglichen Niederschlagsdaten\\n'+
#   'SO = Stationen mit stündlichen Daten der Sonnenscheindauer\\n'+
#   'SY = Stationen mit stündlichen, automatischen Messungen\\n'+
#   'TU = Stationen mit stündlichen Daten der Temperatur und der relativen Feuchte'];",
#   
#   "   var tooltip9 = [
#   'BB = Brandenburg\\n'+	
#   'NW = Nordrhein-Westfalen\\n'+
#   'BE = Berlin\\n'+ 
#   'RP = Rheinland-Pfalz\\n'+
#   'BW = Baden-Württemberg\\n'+
#   'SH = Schleswig-Holstein\\n'+
#   'BY = Bayern\\n'+
#   'SL = Saarland\\n'+
#   'HB = Bremen\\n'+
#   'SN = Sachsen\\n'+
#   'HE = Hessen\\n'+
#   'ST = Sachsen-Anhalt\\n'+
#   'HH = Hamburg\\n'+
#   'TH = Thüringen\\n'+
#   'MV = Mecklenburg-Vorpommern\\n'+
#   'T = Tirol\\n'+
#   'NI = Niedersachsen'
#   ];",
#   
#   "  var tooltips = ['','','',tooltip9];",
#   "  for(var i=0; i<4; i++){",
#   "    $('th:eq('+i+')',thead).attr('title', tooltips[i]);",
#   "  }",
#   "}"
# )

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

kl <- readDWD(stat_id = id, resol = "daily", prod = "kl")

# url <- "https://www.dwd.de/DE/leistungen/klimadatendeutschland/statliste/statlex_html.html;jsessionid=83013DE4D3B021352FE316509C3DCFDC.live11042?view=nasPublication&nn=16102"
# 
# stat.list <- read_html(url)
# 
# webtab <- html_table(stat.list)
# 
# tab2 <- webtab[[1]]
# names(tab2) <- tab2[1,]
# tab2 <- tab2[2:length(tab2[,1]),]
# 
# write.csv(tab2, file = "C:/Users/gabba/Desktop/Dasboard/statlist_full.csv", row.names = F)

