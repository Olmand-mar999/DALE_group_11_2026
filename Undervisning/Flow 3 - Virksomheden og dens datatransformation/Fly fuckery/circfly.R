library(jsonlite)
library(httr)
library(stringr)
library(leaflet)
source("util.R")

# hent flydata eksempel
circ_fly <- readRDS("circMT.rds")
currentDF <- readRDS("currentDF2.rds")
flight_example <- readRDS("flight_example.rds")
fly_data <- readRDS("fly_liste.rds")

# gemmer data i en rds fil i mappen training_data
training_files <- list.files(path = "./training_data", pattern = "*rds", full.names=T)
training_lists <- lapply(training_files, readRDS)

baseurl <- "https://opensky-network.org/api"

# begge endpoints
endpointS <- "/states/all"
endpointT <- "/tracks/all"

# begge query's uden parametre
queryS <- paste0(baseurl,endpointS)
queryT <- paste0(baseurl,endpointT)

# Token
token=getToken()


#### NORDSØEN ####
lamin=52.055129
lamax=56.196869
lomin=-6.065140
lomax=4.305954

# Udkommenteret så vi ikke løber tør for tokens
#url_states <- getRawFrombbox(queryS, token, lamin, lomin, lamax, lomax)
#current=content(url_states, as="text")
#currentJS=fromJSON(current)
#currentDF=currentJS$states
#currentDF=as.data.frame(currentDF)
# kalder funktionen SetColnames der sætter kolonnenavnene
#currentDF <- SetColnames(currentDF)
#saveRDS(currentDF, "currentDF2.rds")

# Find hvilke lande der har fly i luften
tabel_data <- as.data.frame(table(currentDF$origin_country))


# Vi kan ikke finde tracks for fly der ikke er i luften
# Det vil sige alle fly hvor on_ground = TRUE skal fjernes
in_air <- currentDF[currentDF$on_ground == FALSE,]

# find all icaos and put in vector
icaov=in_air$icao24

# Udkommenteret så vi ikke løber tør for tokens
# find all tracks for each normal icao
# start with one test icao
#ictest <- icaov[[30]]
#url_tracks <- paste0(queryT,"?icao24=",ictest, "&time=0")
#res <- GET(url_tracks,add_headers(Authorization = paste("Bearer", token)))
#res$status_code
#resretval <- fromJSON(content(res, as="text"))
#flight_example <- as.data.frame(resretval)
#colnames(flight_example) <- colnames(circ_fly)
#saveRDS(flight_example, "flight_example.rds")

# plots for nyt fly
Plot_lat_lng(flight_example)
Plot_time_crs(flight_example)
Plot_time_alt(flight_example)
MapPlot(flight_example)

# plots for cirklende fly
Plot_lat_lng(circ_fly)
Plot_time_crs(circ_fly)
Plot_time_alt(circ_fly)
MapPlot(circ_fly)

# Udkommenteret så vi ikke løber tør for tokens
# gem tracks for alle fly
#fly_liste <- list()
#for (i in 1:length(icaov)){
#  url_tracks <- paste0(queryT,"?icao24=",icaov[i], "&time=0")
#  res <- GET(url_tracks,add_headers(Authorization = paste("Bearer", token)))
#  if (res$status_code == 200){
#     resretval <- fromJSON(content(res, as="text", encoding = "UTF-8"))
#     fly <- as.data.frame(resretval)
#     colnames(fly) <- colnames(circ_fly)
#     fly_liste <- c(fly_liste, list(fly))
#  }
  #Sys.sleep(0.2) Tilføjede linje for at se om det hjalp. It did not
#}
#saveRDS(fly_liste, "fly_liste.rds")

# Sætter en højdegrænse for at fjerne helikoptere og privatfly
fly_high <- list()
for (i in 1:length(fly_data)){
  fly <- fly_data[[i]]
  if (max(fly[,"alt"]) > 4500){
    fly$sd <- sd(fly$crs)
    fly_high <- c(fly_high, list(fly))
  }
}

MapPlot(fly_high[[4]])

# Giver forskellen mellem hver observation i forhold til den forrige
#dlatv=diff(flight_example$lat)
#dlnv=diff(flight_example$lng)
#dlatv=c(0,dlatv)
#dlnv=c(0,dlnv)
#flight_example$dlat=dlatv
#flight_example$dlng=dlnv

#ggplot(flight_example,aes(x=time,y=lat))+geom_line()+
#  theme(axis.text.x = element_text(angle = 90))+
#  scale_x_continuous(breaks = scales::pretty_breaks(n=35))


# apply circmetrics
# lm
#cmodel=lm(lat ~ lng+crs+alt, data=flight_example)
#summary(cmodel)
# sd
#csd=sd(flight_example$crs)

# myOwnDetectionAlg
# use diff to track change of fortegn



# run R script from terminal
