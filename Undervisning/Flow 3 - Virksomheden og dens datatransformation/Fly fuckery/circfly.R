# =====================================================================
# 0. OPSÆTNING
# =====================================================================

library(jsonlite)
library(httr)
library(stringr)
library(leaflet)
source("util.R")

# hent flydata eksempel og gemte rds fly
circ_fly <- readRDS("circMT.rds")
circ_fly$sd <- sd(circ_fly$crs)
currentDF <- readRDS("currentDF2.rds")
flight_example <- readRDS("flight_example.rds")
fly_data <- readRDS("fly_liste.rds")

# gemmer data i en rds fil i mappen training_data
training_files <- list.files(path = "./training_data", pattern = "*rds", full.names=T)
training <- lapply(training_files, readRDS)

# tilføj standardafvigelse til training files
training_lists <- list()
for (i in 1:length(training)){
  data <- training[[i]]
  data$sd <- sd(data$crs)
  training_lists <- c(training_lists, list(data))
}

# =====================================================================
# 1. URL information
# =====================================================================

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

# =====================================================================
# 2. Hent alle states - fly tilgængelige lige nu
# =====================================================================

# Udkommenteret så vi ikke løber tør for tokens
#url_states <- getRawFrombbox(queryS, token, lamin, lomin, lamax, lomax)
#current=content(url_states, as="text")
#currentJS=fromJSON(current)
#currentDF=currentJS$states
#currentDF=as.data.frame(currentDF)
# kalder funktionen SetColnames der sætter kolonnenavnene
#currentDF <- SetColnames(currentDF)
#saveRDS(currentDF, "currentDF3.rds")

# Find hvilke lande der har fly i luften
tabel_data <- as.data.frame(table(currentDF$origin_country))

# Vi kan ikke finde tracks for fly der ikke er i luften
# Det vil sige alle fly hvor on_ground = TRUE skal fjernes
in_air <- currentDF[currentDF$on_ground == FALSE,]

# find all icaos and put in vector
icaov=in_air$icao24

# =====================================================================
# 3. Hent tracks for enkelt testfly
# =====================================================================

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
#saveRDS(flight_example, "flight_example2.rds")

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

# =====================================================================
# 4. Hent tracks for alle fly
# =====================================================================

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
#}
#saveRDS(fly_liste, "fly_liste2.rds")

# =====================================================================
# 5. Find alle cirkulerende fly
# =====================================================================

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


# Fjern landinger og letninger fra fly, da de godt kan cirkle lidt der
fly_inAir <- list()
for(i in 1:length(fly_high)){
  fly <- fly_high[[i]]
  fly <- fly[fly$alt > 2500,]
  fly_inAir <- c(fly_inAir, list(fly))
}

MapPlot(fly_inAir[[4]])
MapPlot(training_lists[[2]])

# tilføj wulfs data til det skal testes på om det cirkulerer
fly_all <- c(fly_high, training_lists, list(circ_fly))

# Find ud af om flyende cirkulerer. Gem icao navnene på dem der gør
# Skal det være fly_all eller fly_inAir ?
fly_circle <- list()
icao_off <- data.frame(icao = NULL)
for (i in 1:length(fly_all)){
  fly <- fly_all[[i]]
  if (any(fly$crs >= 0 & fly$crs < 45) &&
      any(fly$crs >= 45 & fly$crs < 90) &&
      any(fly$crs >= 90 & fly$crs < 135) &&
      any(fly$crs >= 135 & fly$crs < 180) &&
      any(fly$crs >= 180 & fly$crs < 225) &&
      any(fly$crs >= 225 & fly$crs < 270) &&
      any(fly$crs >= 270 & fly$crs < 315) &&
      any(fly$crs >= 315 & fly$crs <= 360)) {
    fly_circle <- c(fly_circle, list(fly))
    icao_off <- rbind(icao_off, data.frame(icao = fly$icao24[1]))
  }
}

MapPlot(fly_circle[[1]])

fly_circle[[1]]

# Lav dataframe der indeholder alle icaonavne, deres standardafvigelse,
# og om hvorvidt de cirkulerer
dataframe <- data.frame(icao=NULL, sdcourse=NULL)
for (i in 1:length(fly_all)){
  fly <- fly_all[[i]]
  data <- data.frame(icao = fly$icao24[1], 
                     sdcourse = fly$sd[1],
                     isOff = ifelse(fly$icao24[1] %in% icao_off[,1], 1, 0))
  sdcourse <- data.frame
  dataframe <- rbind(dataframe, data)
}


# =====================================================================
# Wulf kode - skal det bruges???
# =====================================================================

# Alt nedenstående er noget wulf oprindeligt har skrevet

tal <-1

MapPlot(fly_circle[[tal]])

# Giver forskellen mellem hver observation i forhold til den forrige
dlatv=diff(fly_circle[[tal]]$lat)
dlnv=diff(fly_circle[[tal]]$lng)
dlatv=c(0,dlatv)
dlnv=c(0,dlnv)
fly_circle[[tal]]$dlat=dlatv
fly_circle[[tal]]$dlng=dlnv
max(fly_circle[[tal]]$dlng)
max(fly_circle[[tal]]$dlat)

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
