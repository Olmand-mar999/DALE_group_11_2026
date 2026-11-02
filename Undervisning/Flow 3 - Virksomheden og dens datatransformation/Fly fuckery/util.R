library(httr)
library(jsonlite)

clientID="ellen_thygesen-api-client"
clientSecret="aNyow6x8okxVIrOFPGSCDPaN0INyP8me"


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
getRawFromUrl <- function(url, token){
  rawres <- GET(url, add_headers(Authorization = paste("Bearer", token, sep = " ")))
  status <- rawres$status_code
  if(status==401){
    token <- getToken() }
  return(rawres)
}

getRawFrombbox <- function(baseurl, token, lamin, lomin, lamax, lomax){
  url <- paste0(baseurl, "?", "lamin=", lamin, "&lomin=",lomin, "&lamax=", lamax, "&lomax=", lomax)
  rawres <- GET(url, add_headers(Authorization = paste("Bearer", token, sep = " ")))
  status <- rawres$status_code
  if(status==401){
    token <- getToken() }
  return(rawres)
}
