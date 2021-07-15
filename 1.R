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

url <- "https://www.dwd.de/DE/leistungen/klimadatendeutschland/statliste/statlex_html.html?view=nasPublication&nn=16102"
# url <- "https://www.dwd.de/DE/leistungen/klimadatendeutschland/statliste/statlex_html.html;jsessionid=D70931FBB12457D14A53133624C40537.live11054?view=nasPublication&nn=16102"
webpage <- read_html(url)

tabhead <- html_text(html_nodes(webpage,"th"))
tabhead <- tabhead[2:12]
nrows <- length(html_nodes(webpage,"tr"))
nodes <- html_nodes(webpage, "tr")


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

tab2 <- tab[, c(-4,-8)] %>%
  group_by(Station) %>%
  summarise(
    "Station" = unique(Station), 
    "ID" = unique(ID),
    "Messprodukte" = unique(paste(Messprodukte, collapse = ", ")), 
    "Breite" = unique(Breite),
    "Länge" = unique(Länge), 
    "Höhe" = unique(Höhe), 
    "Bundesland" = unique(Bundesland)) %>%
  as.data.frame()

# tab3 <- tab[,c(1,2,3,10,11)] %>% nest_by(Station,ID)
# tab3 <- tab[,c(1,2,3,10,11)] %>%
#   group_by(ID) %>%
#   nest(infos = c(Messprodukte,Beginn,Ende)) %>% as.data.frame()


leaflet() %>%
  setView(8.7862, 50.0899, zoom = 5) %>%
  addProviderTiles("OpenTopoMap",providers$Stamen.TonerLite,
                   options = providerTileOptions(noWrap = TRUE)) %>%
  addLegend(position = "bottomright", colors = c("blue","red"), 
            labels = c("Stationen mit ausgewählten Produkt(en)",
                       "Ausgewählte Stationen")) %>%
  
  addCircleMarkers(data = tab2, lat = tab2$Breite[1:100], lng = tab2$Länge[1:100],
                   color = "blue", radius = 1, opacity = .75, # fillOpacity = .33,
                   label = paste0("",tab2$Station),
                   popup = paste(sep = "<br/>",
                                 paste0("<b>",tab2$Station,"</b>"),
                                 paste("ID:",tab2$ID),
                                 paste("Höhe:",tab2$Höhe,"m"),
                                 paste("Messpr.:",tab2$Messprodukte)),
                   # paste("Land:",pt2()$Bundesland)),
                   popupOptions = list(closeButton = FALSE)) %>%
  
  # addCircleMarkers(lat = tab2$Breite[1:50], lng = tab2$Länge[1:50],
  #                  color = "red", radius = 4, opacity = .75, # fillOpacity = .33
  #                  label = paste0("",tab2$Station),
  #                  popup = paste(sep = "<br/>",
  #                                paste0("<b>",tab2$Station,"</b>"),
  #                                paste("ID:",tab2$ID),
  #                                paste("Höhe:",tab2$Höhe,"m"),
  #                                paste("Messpr.:",tab2$Messprodukte)),
  #                  # paste("Land:",pt2()$Bundesland)),
  #                  popupOptions = list(closeButton = FALSE))



## Filtern nach Messprodukt
input1 <- paste(c("KL"),collapse = "|")
input2 <- c(700,1000)
input3 <- c(as.Date("1999-01-01"),Sys.Date()-14)
# 
# 
sel1 <- tab2[grep(input1, tab2$Messprodukte), ]
# 
# sel2 <- tab2[which(tab2$H?he >= input2[1] &
#         tab2$H?he <= input2[2]), ]
# 
# sel3 <- tab2[tab2$ID %in% unique(tab$ID[which(tab$Beginn >= input3[1] &
#         tab$Beginn <= input3[2])]), ]
# 
# inner_join(sel1,sel2) %>%
#   inner_join(sel3)

library(RCurl)
dir <- getURL("opendata.dwd.de/climate_environment/CDC/observations_germany/climate/",
              verbose=TRUE,ftp.use.epsv=TRUE, dirlistonly = TRUE)
dir
library(XML)
getHTMLLinks(dir)




input.daterange <- "bla"
input.altitude <- "blub"
input.prod <- "KL"
selected

c("KL","RR","PE|PS","EB","FF","MI|MN","SO","SY","TU","AE")

if(input.prod == "KL"){
  input.prod <- "climate"
  resol <- "daily"
  add <- "kl"
sprintf("ftp://opendata.dwd.de/climate_environment/CDC/observations_germany/%s/%s/%s", 
        input.prod, resol,add)
}
"tageswerte_RR_00001_19120101_19860630_hist.zip"


unzip(path, list = TRUE)



# readDWD <- function(stat_id, resol, prod, time){
#   baseURL <- "ftp://opendata.dwd.de/climate_environment/CDC/observations_germany/climate/"
#   lookURL_h <- paste0(baseURL,resol,"/",prod,"/","historical","/")
#   lookURL_r <- paste0(baseURL,resol,"/",prod,"/","recent","/")
#   
#   ## vector mit allen Files
#   fls_h <- strsplit(RCurl::getURL(lookURL_h,dirlistonly = T), "\r\n")[[1]]
#   fls_r <- strsplit(RCurl::getURL(lookURL_r,dirlistonly = T), "\r\n")[[1]]
#   
#   ## vector mit dateinamen der .zip dateien
#   varnames_h <- strsplit(fls_h[min(which(grepl("*.zip",fls_h)))],"_")[[1]]
#   varnames_r <- strsplit(fls_r[min(which(grepl("*.zip",fls_r)))],"_")[[1]]
#   
#   db <- c(1:length(stat_id))
#   for(i in 1:length(stat_id)){
#     ## IDs mit L?nge > 5 nicht zugelassen
#     if(nchar(stat_id[i])>5){stop("invalid station ID length")}
#     ## Eingegebene IDs werden auf 5 Stellen vereinheitlicht
#     stat_id[i] <- paste0(paste(rep("0",5-nchar(stat_id[i])),collapse = ""),stat_id[i])
#     ## Auswahl der i-ten Station - Dateiname
#     file_h <- fls_h[which(grepl(paste0("*_",stat_id[i],"_*"), fls_h))]
#     file_r <- fls_r[which(grepl(paste0("*_",stat_id[i],"_*"), fls_r))]
#     
#     ## URL der i-ten zip-Datei
#     endURL_h <- paste0(baseURL,resol,"/",prod,"/","historical","/",file_h)
#     endURL_r <- paste0(baseURL,resol,"/",prod,"/","recent","/",file_r)
#     
#     ## Verzeichnis und Name der tempor?ren Datei
#     temp_h <- tempfile()
#     temp_r <- tempfile()
#     
#     fln_h <- paste0(temp_h,"_",varnames_h[6])
#     fln_r <- paste0(temp_r,"_",varnames_r[6])
#     
#     ## Download der zip-Datei in temp-Ordner
#     download.file(url = endURL_h, destfile = fln_h)
#     download.file(url = endURL_r, destfile = fln_r)
#     
#     ## Entpacken der temp zip-Datei
#     file_h <-  unzip(fln_h, list=TRUE)
#     file_r <-  unzip(fln_r, list=TRUE)
#     
#     ## Einlesen des Stationsproduktes, Ablegen in Datenbank
#     tab_h <- read.table(unz(fln_h,file_h$Name[grep("produkt_",file_h$Name)]) ,
#                         header=T, sep = ";")
#     tab_r <- read.table(unz(fln_r,file_r$Name[grep("produkt_",file_r$Name)]) ,
#                         header=T, sep = ";")
#     tab <- rbind(tab_h,tab_r)
#     db[i] <- list(tab)
#     ## L?schung der tempor?ren Datei und temp-filenames
#     file.remove(fln_h)
#     file.remove(fln_r)
#     
#     unlink(temp_h)
#     unlink(temp_r)
#     
#   }
#   return(db)
# }




