# =====================================================================
# 0. OPSÆTNING OG HJÆLPERE
# =====================================================================
library(httr)         # henter data fra internettet
library(jsonlite)     # læser JSON
library(ggplot2)      # tegner grafer

base <- "https://opendataapi.dmi.dk/v2/metObs/collections/observation/items?"  # baseURL (ingen nøgle)
param_fart <- "wind_speed"      # middelvind over 10 minutter (m/s)
param_retning <- "wind_dir"     # middelretning over 10 minutter (grader, hvor vinden kommer fra)
param_stoed <- "wind_max"       # højeste 3-sekunders vind inden for 10 minutter (stød)
param_fart_t <- "wind_speed_past1h"     # timemiddel af vindstyrke (til 2022)
param_retning_t <- "wind_dir_past1h"    # timemiddel af retning (til 2022)
anholt <- "06079"               # Anholt
aarhus <- "06074"               # Århus

fra_2022 <- "2022-11-14T00:00:00Z"      # november 2022: starttid
til_2022 <- "2022-11-21T00:00:00Z"      # november 2022: sluttid
bbox <- "7,54,16,58"                    # kasse omkring Danmark: vest, syd, øst, nord (længde- og breddegrader)
fra_bred <- "2023-10-17T00:00:00Z"      # bred uge omkring stormen i 2023: starttid (UTC)
til_bred <- "2023-10-24T00:00:00Z"      # bred uge omkring stormen i 2023: sluttid (UTC)
taerskel <- 24.5                        # DMI: storm = middelvind på mindst 24,5 m/s

lav_farver <- function(navne) setNames(c("#1B5E20", "#66BB6A", "#9CCC65")[seq_along(navne)], navne)   # grønne nuancer til stationerne
tema <- theme_minimal() +
  theme(plot.title = element_text(face = "bold", colour = "#14532D"),
        plot.subtitle = element_text(colour = "grey30"),
        plot.caption = element_text(size = 8, colour = "grey40", hjust = 1),
        strip.text = element_text(face = "bold"))        # fælles tema: hvid baggrund, grøn titel, kilde nederst til højre
komma <- function(x) format(round(x, 1), decimal.mark = ",", nsmall = 1)   # tal med komma og én decimal
ord <- c("nord", "nordøst", "øst", "sydøst", "syd", "sydvest", "vest", "nordvest")   # de 8 retninger som ord
retning_ord <- function(grader) ord[floor(((grader + 22.5) %% 360) / 45) + 1]   # grader til retningsord
snit_retning <- function(grader) {
  r <- grader * pi / 180                                       # grader til radianer
  m <- atan2(mean(sin(r)), mean(cos(r))) * 180 / pi            # gennemsnitsretning via vektorer (ikke direkte gennemsnit)
  (m + 360) %% 360                                             # sørg for 0-360
}
titel_topp <- function(data) {
  t <- data[which.max(data$fart), ]                            # rækken med den kraftigste vind
  paste0("Vindstyrken toppede ved ", t$station, " med ", komma(t$fart),
         " m/s, hvor vinden kom fra ", retning_ord(t$retning)) # konkluderende titel bygget af data
}

# =====================================================================
# 1. Funktioner til hentning af data
# =====================================================================
hent <- function(station, param, fra, til) {
  u <- paste0(base, "parameterId=", param, "&stationId=", station,
              "&datetime=", fra, "/", til, "&limit=300000")        # sæt URL'en sammen
  svar <- GET(u)                                                   # send forespørgslen
  stop_for_status(svar)                                            # stop med fejl, hvis serveren afviser
  tekst <- content(svar, as = "text", encoding = "UTF-8")          # hent svaret som tekst
  p <- fromJSON(tekst)$features$properties                         # tabellen med målingerne
  if (is.null(p)) stop(paste0("Ingen data fra station ", station, " for ", param))   # tydelig fejl hvis svaret er tomt
  p <- p[order(p$observed), ]                                      # sortér så ældste måling kommer først
  tid <- as.POSIXct(p$observed, format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")  # tekst til tid (UTC)
  attr(tid, "tzone") <- "Europe/Copenhagen"                        # vis tiden som dansk tid
  data.frame(tid = tid, value = p$value)                           # send en tabel tilbage
}

hent_alle <- function(param, fra, til) {
  u <- paste0(base, "parameterId=", param, "&bbox=", bbox,
              "&datetime=", fra, "/", til, "&limit=300000")        # sæt URL'en sammen (ingen stationId = alle stationer)
  svar <- GET(u, timeout(180))                                     # send forespørgslen (op til 3 minutter)
  stop_for_status(svar)                                            # stop med fejl, hvis serveren afviser
  p <- fromJSON(content(svar, as = "text", encoding = "UTF-8"))$features$properties   # tabellen med målingerne
  if (is.null(p)) stop("Ingen data for ", param)                   # tydelig fejl hvis svaret er tomt
  tid <- as.POSIXct(p$observed, format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")   # tekst til tid (UTC)
  attr(tid, "tzone") <- "Europe/Copenhagen"                        # vis tiden som dansk tid
  data.frame(stationId = p$stationId, tid = tid, value = p$value)  # send en tabel tilbage
}

hent_station <- function(station, navn, fra, til, pf = param_fart, pr = param_retning) {
  x <- merge(hent(station, pf, fra, til), hent(station, pr, fra, til), by = "tid")   # saml fart og retning på tid
  names(x) <- c("tid", "fart", "retning")           # giv kolonnerne navne
  x$station <- navn                                 # tilføj stationens navn
  x                                                 # send tabellen tilbage
}

# =====================================================================
# 2. Graf-funktion (bruges til både 2022 og 2023, så graferne ligner hinanden)
# =====================================================================
graf_vind <- function(data, titel, ytekst, kilde, farver, pil_hver = 3) {
  pil <- data[which(data$retning > 0 & format(data$tid, "%M") == "00" &
                      as.numeric(format(data$tid, "%H")) %% pil_hver == 0), ]   # udvælg tidspunkter til pile
  pil$retning_hen <- (pil$retning + 180) %% 360                 # retningen vinden blæser HEN mod
  pil$y <- pil$fart + 0.08 * max(data$fart)                     # placér pilen lidt over linjen
  g <- ggplot(data, aes(x = tid, y = fart, colour = station))   # tid på x, vindstyrke på y
  g <- g + geom_line(linewidth = 0.8)                           # vindlinjen
  g <- g + geom_text(data = pil, aes(y = y, angle = -retning_hen), label = "↑", size = 4)   # pile roteret efter retning
  g <- g + facet_wrap(~ station, ncol = 1)                      # ét panel per station
  g <- g + scale_colour_manual(values = farver)                 # grønne nuancer
  g <- g + labs(title = titel,
                subtitle = paste0("Pilene viser, hvorhen vinden blæste (op = nord, til højre = øst). Én pil hver ", pil_hver, ". time"),
                x = "Tidspunkt (dansk tid)", y = ytekst, caption = kilde)   # titler og kilde
  g + tema + theme(legend.position = "none")                    # fælles tema, ingen forklaring
}

# #####################################################################
# DEL A: STORMEN I NOVEMBER 2022 (grafen i opgaven)
# #####################################################################

# =====================================================================
# A1. Hent data
# =====================================================================
a22 <- hent_station(anholt, "Anholt", fra_2022, til_2022, param_fart_t, param_retning_t)   # Anholt, 2022
h22 <- hent_station(aarhus, "Århus", fra_2022, til_2022, param_fart_t, param_retning_t)    # Århus, 2022
d22 <- rbind(a22, h22)                                         # læg begge stationer i én tabel
table(d22$station)                                             # forvent ca. 169 rækker per station (7 dage × 24 + 1)
summary(d22$fart)                                              # tjek for urealistiske værdier

# =====================================================================
# A2. Hvilken retning kom vinden fra i 2022?
# =====================================================================
tapply(d22$fart, d22$station, max)                             # højeste vindstyrke per station (opgavens graf: toppen er ca. 12,5 m/s)
topp22 <- do.call(rbind, lapply(split(d22, d22$station), function(x) x[which.max(x$fart), ]))   # kraftigste måling per station
topp22                                                         # tid, fart og retning (grader) per station
snit_retning(d22$retning[d22$retning > 0])                     # gennemsnitsretning over hele ugen (90 = øst)

# =====================================================================
# A3. Graf 2022
# =====================================================================
g22 <- graf_vind(d22, titel_topp(d22), "Middelvind, timegennemsnit (m/s)",
                 "Kilde: DMI Open Data, metObs (wind_speed_past1h og wind_dir_past1h)",
                 lav_farver(c("Anholt", "Århus")), pil_hver = 6)   # graf med pile
g22                                                            # vis grafen

# #####################################################################
# DEL B: STORMEN I OKTOBER 2023 (samme graf, men bedre)
# #####################################################################

# =====================================================================
# B1. Hent alle danske stationer i en bred uge
# =====================================================================
alle_fart <- hent_alle(param_fart, fra_bred, til_bred)         # middelvind, alle stationer
alle_retn <- hent_alle(param_retning, fra_bred, til_bred)      # retning, alle stationer
nrow(alle_fart)                                                # tjek: skal være under 300000, ellers er svaret afkortet
length(unique(alle_fart$stationId))                            # antal stationer
summary(alle_fart$value)                                       # tjek for urealistiske værdier

# =====================================================================
# B2. Find stormens datoer i data
# =====================================================================
storm_m <- alle_fart[which(alle_fart$value >= taerskel), ]     # alle målinger på stormstyrke, alle stationer
if (nrow(storm_m) == 0) stop("Ingen målinger over tærsklen: sænk taerskel eller udvid ugen")   # stop hvis intet findes
table(storm_m$stationId)                                       # hvilke stationer nåede stormstyrke
table(as.Date(storm_m$tid, tz = "Europe/Copenhagen"))          # på hvilke datoer
storm_fra <- as.POSIXct(trunc(min(storm_m$tid), "days"))                  # døgnet med den første stormmåling (kl. 00:00)
storm_til <- as.POSIXct(trunc(max(storm_m$tid), "days")) + 24 * 3600      # slutningen af døgnet med den sidste
storm_tekst <- paste0(format(storm_fra, "%d/%m"), "-", format(storm_til - 1, "%d/%m"))   # fx "20/10-21/10"
storm_tekst                                                    # sammenlign med DMI: 20.-21. oktober

# =====================================================================
# B3. Find de stationer, der viser stormen bedst
# =====================================================================
rang <- aggregate(value ~ stationId, data = alle_fart, FUN = max)    # højeste middelvind per station
antal <- table(alle_fart$stationId)                            # antal målinger per station
fuld <- names(antal)[antal >= 0.9 * max(antal)]                # stationer med næsten komplette data
rang <- rang[rang$stationId %in% fuld, ]                       # behold kun stationer med komplette data
rang <- rang[order(-rang$value), ]                             # rangér efter højeste middelvind
st <- fromJSON(content(GET("https://opendataapi.dmi.dk/v2/metObs/collections/station/items?limit=10000"),
                       as = "text", encoding = "UTF-8"))$features$properties   # stationstabel (til navne)
rang$navn <- st$name[match(rang$stationId, st$stationId)]      # slå stationsnavne op
head(rang, 10)                                                 # de 10 stationer med mest vind
valgte <- rang$stationId[1:2]                                  # vælg de to stationer med mest vind
navne <- rang$navn[1:2]                                        # deres navne
# valgte <- c(anholt, aarhus); navne <- c("Anholt", "Århus")   # fjern # for at gå tilbage til opgavens stationer

# =====================================================================
# B4. Saml data for de valgte stationer og vælg vindue
# =====================================================================
dd <- merge(alle_fart[alle_fart$stationId %in% valgte, ],
            alle_retn[alle_retn$stationId %in% valgte, ], by = c("stationId", "tid"))   # saml fart og retning
names(dd) <- c("stationId", "tid", "fart", "retning")          # giv kolonnerne navne
dd$station <- navne[match(dd$stationId, valgte)]               # tilføj stationsnavn
vindue_fra <- storm_fra - 24 * 3600                            # vindue: ét døgn før stormen
vindue_til <- storm_til + 24 * 3600                            # vindue: ét døgn efter stormen
v <- dd[dd$tid >= vindue_fra & dd$tid < vindue_til, ]          # data fra vinduet omkring stormen
s <- dd[dd$tid >= storm_fra & dd$tid < storm_til, ]            # data fra selve stormen

# =====================================================================
# B5. Tal til teksten (stød og retning)
# =====================================================================
tapply(s$fart, s$station, max)                                 # højeste middelvind under stormen
topp23 <- do.call(rbind, lapply(split(s, s$station), function(x) x[which.max(x$fart), ]))   # kraftigste måling per station
topp23                                                         # tid, fart og retning (grader) per station
stoed <- do.call(rbind, lapply(valgte, function(id)
  data.frame(stationId = id, hent(id, param_stoed, fra_bred, til_bred))))   # vindstød for de valgte stationer
stoed$station <- navne[match(stoed$stationId, valgte)]         # tilføj stationsnavn
tapply(stoed$value, stoed$station, max, na.rm = TRUE)          # højeste vindstød per station
dd$ost <- dd$retning >= 45 & dd$retning <= 135                 # TRUE hvis vinden kommer fra øst (45-135 grader)
dd$dag <- as.Date(dd$tid, tz = "Europe/Copenhagen")            # dato (dansk tid)
aggregate(ost ~ dag + station, data = dd, FUN = mean)          # andel af døgnet med østenvind (støtte til DMI's "langvarig østenvind")

# =====================================================================
# B6. Graf 2023
# =====================================================================
g23 <- graf_vind(v, titel_topp(s), "Middelvind, 10-minutters gennemsnit (m/s)",
                 "Kilde: DMI Open Data, metObs (wind_speed og wind_dir). Stormdatoer: DMI.dk",
                 lav_farver(navne))                            # graf med pile
g23                                                            # vis grafen