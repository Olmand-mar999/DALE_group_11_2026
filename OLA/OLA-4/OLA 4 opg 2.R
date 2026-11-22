library(dplyr)
library(RMariaDB)
library(DBI)

# ============================================================
# Indsæt data i sql tabel
# ============================================================

# connect til world data i sql

#Sys.setenv(localpw = "")

wc <- dbConnect(MariaDB(),
                host = "localhost",
                user = "root",
                password = Sys.getenv("localpw"),
                db = "bilbasen",
                port = "3306")

bilbasen <- readRDS("html_fuckery/bilbasen.rds")

# fjern første 2 rækker. De bruges som nye biler i 2.3
bilbasen <- bilbasen[3:nrow(bilbasen),]

# Vi har lavet skemaerne biler, forhandler og bilpriser i sql.
# Vi vil nu lave de dataframes der indeholder det data der skal indputtes

fh <- unique(bilbasen[!is.na(bilbasen$logo), c("logo", "lokation", "scrapedate")])
forhandler <- data.frame(logo_link  = fh$logo,
                         lokation   = fh$lokation,
                         scraped_at = fh$scrapedate)

dbWriteTable(wc, "forhandler", forhandler, append = T)

fmap <- dbGetQuery(wc, "SELECT forhandler_id, logo_link FROM forhandler")
bilbasen$forhandler_id <- fmap$forhandler_id[match(bilbasen$logo, fmap$logo_link)]

biler <- data.frame(car_id        = bilbasen$id,
                    model         = bilbasen$model,
                    forhandler_id = bilbasen$forhandler_id,
                    aargang       = bilbasen$aargang,
                    km            = bilbasen$km,
                    raekkevidde   = bilbasen$raekkevidde,
                    link          = bilbasen$link,
                    scraped_at    = bilbasen$scrapedate)

priser <- data.frame(car_id     = bilbasen$id,
                     pris       = bilbasen$pris,
                     scraped_at = bilbasen$scrapedate)

dbWriteTable(wc, "biler", biler, append = T)
dbWriteTable(wc, "bil_priser", priser, append = T)

# ============================================================
# Lav en simuleret kørsel
# ============================================================

simuleret <- readRDS("html_fuckery/bilbasen.rds")

# ændr scrapedate til næste dag
simuleret$scrapedate <- simuleret$scrapedate + 1

# slet 5 rækker
simuleret <- simuleret[-c(3:7),]

# ændr pris
simuleret$pris[9:11] <- simuleret$pris[9:11] + 10000

saveRDS(simuleret, "html_fuckery/simuleret.rds")
simuleret_færdig <- readRDS("html_fuckery/simuleret.rds")

# ============================================================
# Opdater bilbasen skema i sql
# ============================================================

library(dplyr)

solgte_biler <- anti_join(bilbasen, simuleret_færdig, by = "id")

kolonner <- c("id", "model", "logo", "lokation", "aargang",
              "km", "raekkevidde", "link", "pris")

nye_priser <- bind_rows(
  anti_join(simuleret_færdig, bilbasen, by = kolonner),
  anti_join(bilbasen, simuleret_færdig, by = kolonner)
) %>%
  anti_join(solgte_biler, by = "id") %>%
  group_by(id) %>%
  slice_max(order_by = pris, n = 1)

korrekt_scrape <- simuleret_færdig$scrapedate[1]

nye_priser$scrapedate <- korrekt_scrape

fmap <- dbGetQuery(wc, "
  SELECT forhandler_id, logo_link FROM forhandler
")

nye_priser$forhandler_id <-
  fmap$forhandler_id[match(nye_priser$logo, fmap$logo_link)]

eksisterende_biler <- dbGetQuery(wc, "
  SELECT car_id FROM biler
")

nye_biler <- anti_join(
  nye_priser,
  eksisterende_biler,
  by = c("id" = "car_id")
)

biler <- data.frame(
  car_id        = nye_biler$id,
  model         = nye_biler$model,
  forhandler_id = nye_biler$forhandler_id,
  aargang       = nye_biler$aargang,
  km            = nye_biler$km,
  raekkevidde   = nye_biler$raekkevidde,
  link          = nye_biler$link,
  scraped_at    = nye_biler$scrapedate
)

dbWriteTable(wc, "biler", biler, append = TRUE)

priser <- data.frame(
  car_id     = nye_priser$id,
  pris       = nye_priser$pris,
  scraped_at = nye_priser$scrapedate,
  sold       = 0
)


solgte_priser <- data.frame(
  car_id     = solgte_biler$id,
  pris       = solgte_biler$pris,
  scraped_at = solgte_biler$scrapedate + 1,
  sold       = 1
)

dbWriteTable(wc, "bil_priser", solgte_priser, append = T)
dbWriteTable(wc, "bil_priser", priser, append = T)

