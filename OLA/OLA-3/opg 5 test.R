# =====================================================================
# 0. OPSÆTNING OG HJÆLPERE
# =====================================================================
library(httr)
library(jsonlite)
library(ggplot2)

# Parameters
base <- "https://opendataapi.dmi.dk/v2/metObs/collections/observation/items?"
param_fart <- "wind_speed"      # Middelvind over 10 minutter (m/s)
param_retning <- "wind_dir"     # Middelretning over 10 minutter (grader, hvor vinden kommer fra)
param_stoed <- "wind_max"       # Højeste 3-sekunders vind inden for 10 minutter (stød)

# StationsID
anholt <- "06079"
aarhus <- "06074"
kegnaes <- "06119"      # Fundet igennem kilde

# Tid for 2022
fra_2022 <- "2022-11-14T00:00:00Z"      # november 2022: starttid
til_2022 <- "2022-11-21T00:00:00Z"      # november 2022: sluttid

# Tid for storm 2023
fra_2023 <- "2023-10-17T00:00:00Z"      # bred uge omkring stormen i 2023: starttid (UTC)
til_2023 <- "2023-10-24T00:00:00Z"      # bred uge omkring stormen i 2023: sluttid (UTC)


# =====================================================================
# 1. Function 
# =====================================================================

# Function
hent <- function(param, station, fra, til) {
  u <- paste0(base, "parameterId=", param, "&stationId=", station,
              "&datetime=", fra, "/", til, "&limit=300000")       # Konstruer URL
  svar <- GET(u)                                                        # Kald på API
  tekst <- content(svar, as = "text", encoding = "UTF-8")               # Lav til tekst format
  p <- fromJSON(tekst)$features$properties                              # Formater til JSON
}


# =====================================================================
# 2. Hent data 
# =====================================================================

# Anholt 2022
anholt_fart22 <- hent(param_fart, anholt, fra_2022, til_2022)
anholt_retning22 <- hent(param_retning, anholt, fra_2022, til_2022)
anholt_stoed22 <- hent(param_stoed, anholt, fra_2022, til_2022)
anholt22 <- rbind(anholt_fart22, anholt_retning22, anholt_stoed22)

# Aarhus 2022
aarhus_fart22 <- hent(param_fart, aarhus, fra_2022, til_2022)
aarhus_retning22 <- hent(param_retning, aarhus, fra_2022, til_2022)
aarhus_stoed22 <- hent(param_stoed, aarhus, fra_2022, til_2022)
aarhus22 <- rbind(aarhus_fart22, aarhus_retning22, aarhus_stoed22)

# Anholt 2023
anholt_fart23 <- hent(param_fart, anholt, fra_2023, til_2023)
anholt_retning23 <- hent(param_retning, anholt, fra_2023, til_2023)
anholt_stoed23 <- hent(param_stoed, anholt, fra_2023, til_2023)
anholt23 <- rbind(anholt_fart23, anholt_retning23, anholt_stoed23)

# Aarhus 2022
aarhus_fart23 <- hent(param_fart, aarhus, fra_2023, til_2023)
aarhus_retning23 <- hent(param_retning, aarhus, fra_2023, til_2023)
aarhus_stoed23 <- hent(param_stoed, aarhus, fra_2023, til_2023)
aarhus23 <- rbind(aarhus_fart23, aarhus_retning23, aarhus_stoed23)

# Kegnaes 2022
kegnaes_fart23 <- hent(param_fart, kegnaes, fra_2023, til_2023)
kegnaes_retning23 <- hent(param_retning, kegnaes, fra_2023, til_2023)
kegnaes_stoed23 <- hent(param_stoed, kegnaes, fra_2023, til_2023)
kegnaes23 <- rbind(kegnaes_fart23, kegnaes_retning23, kegnaes_stoed23)
