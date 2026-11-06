library(httr)
library(jsonlite)

clientID="emma.hartz.hald@gmail.com-api-client"
clientSecret="WbBlWwgijyHqRNOJml3P2hyUo1asUUGC"


# Request an access token

getToken <- function() {
  token=NULL
  response <- POST(
    url = "https://auth.opensky-network.org/auth/realms/opensky-network/protocol/openid-connect/token",
    body = list(
      grant_type = "client_credentials",
      client_id = clientID,
      client_secret = clientSecret
    ),
    encode = "form"
  )
  
  # Parse the JSON response
  response$status_code
  if (response$status_code == 200) {
    content <- content(response, as = "parsed", type = "application/json")
    token <- content$access_token
  }
  return(token)
}


# 401 betyder error
# Laver basic url uden argumenter
getRawFromUrl <- function(url, token){
  rawres <- GET(url, add_headers(Authorization = paste("Bearer", token, sep = " ")))
  status <- rawres$status_code
  if(status==401){
    token <- getToken() }
  return(rawres)
}

# Laver url med bbox
getRawFrombbox <- function(baseurl, token, lamin, lomin, lamax, lomax){
  url <- paste0(baseurl, "?", "lamin=", lamin, "&lomin=",lomin, "&lamax=", lamax, "&lomax=", lomax)
  rawres <- GET(url, add_headers(Authorization = paste("Bearer", token, sep = " ")))
  status <- rawres$status_code
  if(status==401){
    token <- getToken() }
  return(rawres)
}

# Sætter navne på dataframes
SetColnames <- function(dataframe){
  page=read_html("https://openskynetwork.github.io/opensky-api/rest.html")
  tabs=page |> html_elements("table") |> html_table()
  vars=tabs[[5]]
  colnames(vars)
  
  #lav dette til en vector der kan bruges i colnames
  names=unlist(vars$Property)
  names=names[1:(length(names)-1)]
  
  colnames(dataframe) <- names
  return(dataframe)
}


# funktion til plots
Plot_lat_lng <- function(flydata){
  plot(flydata[,"lat"], flydata[, "lng"], 
       type = "p", 
       main = "latitude and longtitude", 
       xlab = "latitude", 
       ylab = "longtitude")
}

Plot_time_crs <- function(flydata){
  plot(flydata[,"time"], flydata[, "crs"], 
       type = "p", 
       main = "crs and time", 
       xlab = "time", 
       ylab = "crs")
}

Plot_time_alt <- function(flydata){
  plot(flydata[,"time"], flydata[, "alt"], 
       type = "p", 
       main = "altitude and time", 
       xlab = "time", 
       ylab = "altitude")
}

MapPlot <- function(flydata){
  leaflet(flydata) %>%
    addTiles() %>%
    addPolylines(lat = ~lat, lng = ~lng)
}

