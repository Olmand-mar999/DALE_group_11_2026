library(jsonlite)
library(httr)
library(stringr)
library(leaflet)
source("util.R")

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

url_states <- getRawFrombbox(queryS, token, lamin, lomin, lamax, lomax)

current=content(url_states, as="text")
currentJS=fromJSON(current)
currentDF=currentJS$states
currentDF=as.data.frame(currentDF)

# kalder funktionen SetColnames der sætter kolonnenavnene
currentDF <- SetColnames(currentDF)

# Find hvilke lande der har fly i luften
tabel_data <- as.data.frame(table(currentDF$origin_country))

# hent flydata eksempel
circ_fly <- readRDS("circMT.rds")

# find all icaos and put in vector
icaov=currentDF$icao24

# find all tracks for each normal icao
# start with one test icao
ictest <- icaov[[8]]
url_tracks <- paste0(queryT,"?icao24=",ictest, "&time=0")
res <- GET(url_tracks,add_headers(Authorization = paste("Bearer", token)))
res$status_code
resretval <- fromJSON(content(res, as="text"))
flight_example <- as.data.frame(resretval)
colnames(flight_example) <- colnames(circ_fly)


# gemmer data i en rds fil i mappen training_data
training_files <- list.files(path = "./training_data", pattern = "*rds", full.names=T)
training_lists <- lapply(training_files, readRDS)

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

# Giver forskellen mellem hver observation i forhold til den forrige
dlatv=diff(flight_example$lat)
dlnv=diff(flight_example$lng)
dlatv=c(0,dlatv)
dlnv=c(0,dlnv)
flight_example$dlat=dlatv
flight_example$dlng=dlnv

ggplot(flight_example,aes(x=time,y=lat))+geom_line()+
  theme(axis.text.x = element_text(angle = 90))+
  scale_x_continuous(breaks = scales::pretty_breaks(n=35))


# apply circmetrics
# lm
cmodel=lm(lat ~ lng+crs+alt, data=flight_example)
summary(cmodel)
# sd
csd=sd(flight_example$crs)

# myOwnDetectionAlg
# use diff to track change of fortegn



# run R script from terminal
