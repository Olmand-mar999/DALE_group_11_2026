library(jsonlite)
library(httr)
library(stringr)
source("util.R")

baseurl <- "https://opensky-network.org/api"
endpoints <- "/states/all"
query <- paste0(baseurl,endpoints)


#### POINT REYES ####
lamin=34.841183
lamax=38.228414
lomin=-123.143884
lomax=-120.573969

fullurl=paste0("https://opensky-network.org/api/states/all?lamin=",lamin,"&lomin=",lomin,"&lamax=",lamax,"&lomax=",lomax)

# lav en tæller som tæller op til en grænse
freq=4
counter=0
limit=5
prflightlist = list()
token=getToken()
while (counter <= limit) {
  res <- httr::GET(fullurl,add_headers(Authorization = paste("Bearer", token)))
  if (res$status_code == 401) {
    token=getToken()
  } else {
    rescontent <- httr::content(res, as="text")
    resretval <- jsonlite::fromJSON(rescontent)
    statedfpr <- as.data.frame(resretval$states)
    
    # append dataframe to list
    prflightlist <- append(prflightlist,list(statedfpr))
    # sleep for x seconds
    Sys.sleep(freq)
    counter = counter + 1
  }
}
filename="my_flights.rds"
saveRDS(prflightlist,filename)
# run R script from terminal
