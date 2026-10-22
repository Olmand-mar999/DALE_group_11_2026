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

# Aarhus 2022
aarhus_fart22 <- hent(param_fart, aarhus, fra_2022, til_2022)
aarhus_retning22 <- hent(param_retning, aarhus, fra_2022, til_2022)
aarhus_stoed22 <- hent(param_stoed, aarhus, fra_2022, til_2022)

# Anholt 2023
anholt_fart23 <- hent(param_fart, anholt, fra_2023, til_2023)
anholt_retning23 <- hent(param_retning, anholt, fra_2023, til_2023)
anholt_stoed23 <- hent(param_stoed, anholt, fra_2023, til_2023)

# Aarhus 2022
aarhus_fart23 <- hent(param_fart, aarhus, fra_2023, til_2023)
aarhus_retning23 <- hent(param_retning, aarhus, fra_2023, til_2023)
aarhus_stoed23 <- hent(param_stoed, aarhus, fra_2023, til_2023)

# Kegnaes 2022
kegnaes_fart23 <- hent(param_fart, kegnaes, fra_2023, til_2023)
kegnaes_retning23 <- hent(param_retning, kegnaes, fra_2023, til_2023)
kegnaes_stoed23 <- hent(param_stoed, kegnaes, fra_2023, til_2023)

# =====================================================================
# 3. dataframes fra data
# =====================================================================

# dataframe med anholt 22
anholt22 <- data.frame(
  periode = anholt_fart22$observed,
  Station_id = anholt_fart22$stationId,
  fart = anholt_fart22$value, 
  retning = anholt_retning22$value, 
  stoed = anholt_stoed22$value)
  
# dataframe med anholt 23
anholt23 <- data.frame(
  periode = anholt_fart23$observed,
  Station_id = anholt_fart23$stationId,
  fart = anholt_fart23$value, 
  retning = anholt_retning23$value, 
  stoed = anholt_stoed23$value)

# dataframe med aarhus 22
aarhus22 <- data.frame(
  periode = aarhus_fart22$observed,
  Station_id = aarhus_fart22$stationId,
  fart = aarhus_fart22$value, 
  retning = aarhus_retning22$value, 
  stoed = aarhus_stoed22$value)

# dataframe med aarhus 23
aarhus23 <- data.frame(
  periode = aarhus_fart23$observed,
  Station_id = aarhus_fart23$stationId,
  fart = aarhus_fart23$value, 
  retning = aarhus_retning23$value, 
  stoed = aarhus_stoed23$value)

# dataframe med kegnaes 23
kegnaes23 <- data.frame(
  periode = kegnaes_fart23$observed,
  Station_id = kegnaes_fart23$stationId,
  fart = kegnaes_fart23$value, 
  retning = kegnaes_retning23$value, 
  stoed = kegnaes_stoed23$value)

