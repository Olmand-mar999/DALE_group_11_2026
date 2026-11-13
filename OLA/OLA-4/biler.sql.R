library(dplyr)
library(RMariaDB)
library(DBI)

# connect til world data i sql

#Sys.setenv(localpw = "")


wc <- dbConnect(MariaDB(),
                host = "localhost",
                user = "root",
                password = Sys.getenv("localpw"),
                db = "emp",
                port = "3306")

#biler <- readRDS("bilbasen.rds")
#biler2 <- unique(biler)

#dbWriteTable(wc, "biler", biler)

# henter employee tabel fra emp database
query <-  "select * from emp"
dfemp <- dbGetQuery(wc, query)
query2 <-  "select * from biler"
dfcar <- dbGetQuery(wc, query2)
dfcarrental <- data.frame(EMPNO = 7369, 
                          C_ID = 7039566, 
                          Lejedag = Sys.Date()-1, 
                          returneringsdag = Sys.Date(), 
                          Description = "Kurt")

#dbWriteTable(wc, "rentals", dfcarrental, overwrite = T)
#dbWriteTable(wc, "rentals", dfcarrental, append = T)
