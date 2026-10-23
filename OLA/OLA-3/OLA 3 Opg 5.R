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

# Aarhus 2023
aarhus_fart23 <- hent(param_fart, aarhus, fra_2023, til_2023)
aarhus_retning23 <- hent(param_retning, aarhus, fra_2023, til_2023)
aarhus_stoed23 <- hent(param_stoed, aarhus, fra_2023, til_2023)

# Kegnaes 2023
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


# =====================================================================
# 4. PLOTS TIL OPGAVE 5.2
# =====================================================================

# ---------- 4A: 2022 ----------
d22 <- rbind(anholt22, aarhus22)                      # saml 2022 i én tabel
d22$station <- factor(d22$Station_id,
                      levels = c(anholt, aarhus),
                      labels = c("Anholt", "Århus"))  # navne i stedet for ID
d22$tid <- as.POSIXct(d22$periode, format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")  # læs som UTC
attr(d22$tid, "tzone") <- "Europe/Copenhagen"         # vis i dansk tid
d22$retning_hen <- (d22$retning + 180) %% 360         # retning vinden blæser MOD
top22 <- d22[which.max(d22$fart), ]                   # række med højeste vind
ord22 <- c("nord", "nordøst", "øst", "sydøst", "syd", "sydvest", "vest", "nordvest")[(round(top22$retning / 45) %% 8) + 1]  # grader -> ord
print(top22)                                          # kontrol: station, tid, fart, retning
print(ord22)                                          # hvor kom vinden fra?
pil22 <- d22[seq(1, nrow(d22), by = 72), ]            # én pil hver 12. time
pil22$y <- max(d22$fart) * 1.22                       # pilerække højere oppe, over gridlinjen

g22 <- ggplot(d22, aes(x = tid, y = fart, colour = station)) +   # data og farve
  geom_line(linewidth = 0.9) +                        # vindlinje
  geom_text(data = pil22, aes(y = y, angle = -retning_hen),
            label = "↑", size = 6) +                  # større pile
  facet_wrap(~station, ncol = 1) +                    # ét panel pr. station
  scale_colour_manual(values = c("Anholt" = "#1B5E20", "Århus" = "#81C784")) +  # grønne nuancer
  coord_cartesian(ylim = c(0, max(d22$fart) * 1.3)) + # mere luft øverst til pilerækken
  labs(title = paste0("Vinden var kraftigst ved ", top22$station, " og kom fra ", ord22),  # konkluderende titel
       subtitle = "Pilene viser, hvorhen vinden blæser (pil mod venstre = vind fra øst)",  # læsevejledning
       x = "Tidspunkt (dansk tid)",                   # x-akse
       y = "Middelvind, 10 minutter (m/s)",           # y-akse
       caption = "Kilde: DMI Open Data, metObs (wind_speed og wind_dir)") +  # kilde nederst til højre
  theme_minimal() +                                   # ren baggrund
  theme(legend.position = "none",                     # ingen signaturforklaring
        strip.text = element_text(face = "bold"),     # fed panelnavn
        plot.title = element_text(face = "bold", colour = "#1B5E20"))  # grøn titel
print(g22)                                            # vis graf

# ---------- 4B: Stormen oktober 2023 ----------
d23 <- rbind(kegnaes23, anholt23)                     # Kegnæs mod Anholt (kontrast)
d23$station <- factor(d23$Station_id,
                      levels = c(kegnaes, anholt),
                      labels = c("Kegnæs Fyr", "Anholt"))  # navne i stedet for ID
d23$tid <- as.POSIXct(d23$periode, format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")  # læs som UTC
attr(d23$tid, "tzone") <- "Europe/Copenhagen"         # vis i dansk tid
d23$retning_hen <- (d23$retning + 180) %% 360         # retning vinden blæser MOD

# Find stormperioden i data (middelvind >= 24,5 m/s = storm)
storm_tid <- d23$tid[d23$fart >= 24.5]                # tidspunkter med stormstyrke
storm_fra <- min(storm_tid)                           # første stormobservation
storm_til <- max(storm_tid)                           # sidste stormobservation
print(c(format(storm_fra), format(storm_til)))        # kontrol: hvornår var det storm?

# Zoom ind: 1 døgn før og efter stormen
d23z <- d23[d23$tid >= storm_fra - 86400 & d23$tid <= storm_til + 86400, ]  # udsnit
top23 <- d23z[which.max(d23z$fart), ]                 # række med højeste vind
maks_anholt <- max(d23z$fart[d23z$station == "Anholt"])  # Anholts højeste vind
pil23 <- d23z[seq(1, nrow(d23z), by = 36), ]          # én pil hver 6. time
pil23$y <- max(d23z$fart) * 1.12                      # alle pile i samme række øverst

g23 <- ggplot(d23z, aes(x = tid, y = fart, colour = station)) +  # data og farve
  geom_hline(yintercept = 24.5, linetype = "dashed", colour = "grey50") +  # stormgrænse
  geom_line(linewidth = 0.9) +                        # vindlinje
  geom_text(data = pil23, aes(y = y, angle = -retning_hen),
            label = "↑", size = 6) +                  # større pile
  facet_wrap(~station, ncol = 1) +                    # ét panel pr. station
  scale_colour_manual(values = c("Kegnæs Fyr" = "#1B5E20", "Anholt" = "#81C784")) +  # grønne nuancer
  coord_cartesian(ylim = c(0, max(d23z$fart) * 1.2)) +  # y-aksen starter i 0 og får luft øverst
  labs(title = paste0("Stormen ramte ", top23$station, " med ",
                      format(round(top23$fart, 1), decimal.mark = ","),
                      " m/s, mens Anholt højst nåede ",
                      format(round(maks_anholt, 1), decimal.mark = ",")),  # konkluderende titel
       subtitle = "Stiplet linje = stormstyrke (24,5 m/s). Pilene viser, hvorhen vinden blæser",  # læsevejledning
       x = "Tidspunkt (dansk tid)",                   # x-akse
       y = "Middelvind, 10 minutter (m/s)",           # y-akse
       caption = "Kilde: DMI Open Data, metObs (wind_speed og wind_dir)") +  # kilde nederst til højre
  theme_minimal() +                                   # ren baggrund
  theme(legend.position = "none",                     # ingen signaturforklaring
        strip.text = element_text(face = "bold"),     # fed panelnavn
        plot.title = element_text(face = "bold", colour = "#1B5E20"))  # grøn titel
print(g23)                                            # vis graf