library(httr)
library(jsonlite)
library(tidyr)
library(dplyr)
library(ggplot2)

#global variables for query
windirparam="wind_dir_past1h"
windforceparam="wind_speed_past1h"
bak='f76cb11d-5c0f-494f-abfe-2cbfda38eed5'
ak='b78be2c4-dfa5-4432-8b85-639cc56e42d7'
aarhsid="06074"
sid="06180"
baseurl='https://dmigw.govcloud.dk/v2/metObs/collections/observation/items?'
baseurl='https://opendataapi.dmi.dk/v2/metObs/collections/observation/items?'
fd="2022-11-14T00:00:00Z"
td="2022-11-21T00:00:00Z"
nfd="2024-07-26T00:00:00Z"
ntd="2024-08-25T00:00:00Z"

# now get data from function for aarhus
windforcedf=getdatafromdmi(windforceparam,sid,fd,td)
winddirdf=getdatafromdmi(windirparam,sid,fd,td)
totalwind=inner_join(windforcedf,winddirdf, by="observed")
wnames=c("observed","force","dir")
colnames(totalwind) <- wnames

#anholt
asid="06079"
totalwinda=NULL
windforcedfa=getdatafromdmi(windforceparam,asid,fd,td)
winddirdfa=getdatafromdmi(windirparam,asid,fd,td)
totalwinda=merge(windforcedfa,winddirdfa, by="observed")
wnames=c("observed","force","dir")
colnames(totalwinda) <- wnames


totalwind$tic <- 1:nrow(totalwind)
totalwind$xend <- totalwind$tic+cos(totalwind$dir)
totalwind$yend <- totalwind$force+sin(totalwind$dir)

#fra grader til radianer
totalwind$dir_rad <- totalwind$dir * (pi / 180)
totalwind$yd <- totalwind$force+(sin(totalwind$dir_rad))

#now plot
p <- ggplot(data=totalwind, aes(x=tic, y=force,color=force))+
  scale_color_gradient(low = "green", high = "red")+
  geom_line(size=2)+
  theme(axis.text.x = element_text(angle = 90))

# add vectors
p <- p+  geom_segment(data=totalwind,  
                      aes(x=tic, y=force,  xend=xd, yend=yd),
                      arrow = arrow(type = "closed",  length = unit(0.04, "inches")),
                      color="black" ,
                      lineend = "round") 
# get all tics
p <- p+ scale_x_continuous(breaks = totalwind$tic) 
p
p <- ggplot(data=totalwind, aes(x=tic,
                                y=force, 
                                xend=xd,
                                yend=yd))+
  geom_point(color="black",size=0.2)+
  geom_segment(arrow = arrow(type = "closed",  length = unit(0.04, "inches")),color="black" ,
               lineend = "round") 
p
baseurl='https://dmigw.govcloud.dk/v2/metObs/collections/observation/items?'
getdatafromdmi <- function(param,station,frd,tod) {
  # constructing query-url
  varurl=paste0('parameterId=',param,'&stationId=',station,'&datetime=',frd,'/',tod,'&api-key=',ak)
  totalurl=paste0(baseurl,varurl)
  print(totalurl)
  #totalurl="https://opendataapi.dmi.dk/v2/metObs/collections/observation/items?parameterId=wind_dir_past1h&stationId=06180&datetime=2022-11-14T00:00:00Z/2022-11-21T00:00:00Z&api-key=b78be2c4-dfa5-4432-8b85-639cc56e42d7"
  totalurl
  
  # request
  res <- GET(totalurl)
  #res$status_code
  print(res$status_code)
  resraw <- httr::content(res, type="text") 
  resdf <- fromJSON(resraw)
  #resdf$features$properties
  
  #extract relevant data
  obsdf=resdf$features$properties
  winddf=obsdf[,c('value','observed')]
  winddf=arrange(winddf,observed)
  #2022-11-21T00:00:00Z
  winddf$observed=as.POSIXct(winddf$observed,format="%Y-%m-%dT%H:%M:%S")
  return(winddf)
}
