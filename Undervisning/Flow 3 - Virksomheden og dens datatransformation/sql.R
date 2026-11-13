library(dplyr)
library(RMariaDB)
library(DBI)

# connect til world data i sql

#Sys.setenv(localpw = "")


wc <- dbConnect(MariaDB(),
                host = "localhost",
                user = "root",
                password = Sys.getenv("localpw"),
                db = "world",
                port = "3306")

query <- "select c.name as 'landenavn', c.population, ctr.name from city c
join country ctr on ctr.code = c.CountryCode
where ctr.continent = 'Europe'
order by ctr.name"

resdf <- dbGetQuery(wc, query)

baseQ <- "select c.name as 'landenavn', c.population, ctr.name from city c
join country ctr on ctr.code = c.CountryCode
where ctr.continent = '"
endQ <- "'order by ctr.name"

get_cont <- function(continent){
  query <- paste0(baseQ, continent, endQ)
  data <- dbGetQuery(wc, query)
  return(data)
}

ny_res <- get_cont('Africa')


# hent alt og merge i R.
country <- dbGetQuery(wc, "select * from country")
city  <- dbGetQuery(wc, "select * from city")
countrylanguage <- dbGetQuery(wc, "select * from countrylanguage")
