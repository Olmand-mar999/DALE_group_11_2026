# =====================================================================
# OPGAVE 3.6 - SIDE QUEST: Radiokommunikation koblet med OpenSky-data
#
# Kør fra mappen OLA/OLA-4 (samme sted som util.R og fly_fuckery/).
# API-kaldene er udkommenteret, så scriptet kan køres af alle i gruppen
# ud fra de gemte rds-filer uden at bruge OpenSky-kald.
#
# Lyd: LiveATC.net, London Control (North Sea), 133.700 MHz.
# Arkivfil: EIDW9-London-Control-North-Nov-20-2026-1000Z.mp3 (10:00-10:32 UTC)
# Transskriberet med Microsoft Word (Transskriber). Lydfilen deles IKKE
# (LiveATC må kun bruges ikke-kommercielt).
# =====================================================================

library(jsonlite)
library(httr)
library(leaflet)
library(dplyr)
source("util.R")   # getToken(), getRawFrombbox(), SetColnames()

baseurl <- "https://opensky-network.org/api"
queryS  <- paste0(baseurl, "/states/all")
queryT  <- paste0(baseurl, "/tracks/all")

# Nordsø-boksen
lamin <- 52.055129; lamax <- 56.196869
lomin <- -6.065140; lomax <- 4.305954

circ_fly  <- readRDS("fly_fuckery/circMT.rds")
kolonner  <- setdiff(colnames(circ_fly), "sd")   # kolonnenavne til tracks


# =====================================================================
# 1. FUNKTIONER
# =====================================================================

moerk_lilla <- "#5b2a9e"

nordpil_html <- paste0(
  "<div style='text-align:center;font-family:sans-serif;font-weight:bold;",
  "background:white;padding:4px 8px;border-radius:6px;box-shadow:0 1px 4px rgba(0,0,0,.3);'>",
  "<div style='font-size:22px;line-height:22px;color:#5b2a9e'>&#9650;</div>N</div>")

# Kort over ét fly med start (sort) og slut (lilla)
kort_fly <- function(fly, farve = moerk_lilla) {
  fly <- as.data.frame(fly)
  n <- nrow(fly)
  leaflet(fly) %>%
    addTiles() %>%
    addPolylines(lng = ~as.numeric(lng), lat = ~as.numeric(lat),
                 color = farve, weight = 4, opacity = 0.9) %>%
    addCircleMarkers(lng = as.numeric(fly$lng[1]), lat = as.numeric(fly$lat[1]),
                     radius = 7, color = "white", weight = 2,
                     fillColor = "black", fillOpacity = 1, label = "Start") %>%
    addCircleMarkers(lng = as.numeric(fly$lng[n]), lat = as.numeric(fly$lat[n]),
                     radius = 7, color = "white", weight = 2,
                     fillColor = farve, fillOpacity = 1, label = "Slut") %>%
    addControl(html = nordpil_html, position = "topright")
}

# Måling nærmest et tidspunkt (UTC)
naermeste <- function(fly, tid_utc) {
  t <- as.numeric(as.POSIXct(tid_utc, tz = "UTC"))
  fly[which.min(abs(as.numeric(fly$time) - t)), ]
}

# Kort med markør dér, hvor flyet var ved radioopkaldet
kort_med_radio <- function(fly, radio_tid_utc, tekst) {
  fly <- as.data.frame(fly)
  m <- naermeste(fly, radio_tid_utc)
  maaling <- format(as.POSIXct(as.numeric(m$time), origin = "1970-01-01", tz = "UTC"), "%H:%M:%S")
  kort_fly(fly) %>%
    addMarkers(lng = as.numeric(m$lng), lat = as.numeric(m$lat),
               popup = paste0("<b>Opkald ca. ", substr(radio_tid_utc, 12, 19), " UTC</b><br>",
                              tekst, "<br>Nærmeste måling: ", maaling, " UTC",
                              "<br>Højde: ", m$alt, " m<br>Kurs: ", m$crs, "°"))
}

# Hent rute fra /tracks/all. tid_utc = NULL giver seneste rute (time=0)
hent_rute <- function(icao, tid_utc = NULL) {
  t <- if (is.null(tid_utc)) 0 else as.numeric(as.POSIXct(tid_utc, tz = "UTC"))
  r <- as.data.frame(fromJSON(content(
    GET(paste0(queryT, "?icao24=", icao, "&time=", t),
        add_headers(Authorization = paste("Bearer", token))),
    as = "text", encoding = "UTF-8")))
  colnames(r) <- kolonner
  r
}

# Log et flys live-position hvert 30. sekund (bruges mens man lytter)
log_fly <- function(callsign, minutter = 10) {
  log <- data.frame()
  slut <- Sys.time() + minutter * 60
  while (Sys.time() < slut) {
    s <- SetColnames(as.data.frame(fromJSON(content(
      getRawFrombbox(queryS, token, lamin, lomin, lamax, lomax),
      as = "text", encoding = "UTF-8"))$states))
    r <- s[trimws(s$callsign) == callsign,
           c("callsign", "latitude", "longitude", "baro_altitude", "true_track", "vertical_rate")]
    if (nrow(r) > 0) {
      r$tid_utc <- format(Sys.time(), tz = "UTC")
      log <- rbind(log, r)
      print(r)
    }
    Sys.sleep(30)
  }
  log
}


# =====================================================================
# 2. HENT DATA (udkommenteret - resultaterne er gemt i fly_fuckery/)
# =====================================================================

# token <- getToken()

# Fly over Nordsøen med callsign (hentet kl. ca. 10:26 UTC d. 20/11-2026)
# url_states <- getRawFrombbox(queryS, token, lamin, lomin, lamax, lomax)
# nu <- SetColnames(as.data.frame(fromJSON(content(url_states, as = "text", encoding = "UTF-8"))$states))
# kandidater <- nu[nu$on_ground == FALSE & trimws(nu$callsign) != "", ]
# kandidater$callsign <- trimws(kandidater$callsign)
# saveRDS(kandidater, "fly_fuckery/radio_kandidater.rds")

# Ruter for de tre fly
# dlh440 <- hent_rute("3c4a8c", "2026-11-20 10:31:00"); saveRDS(dlh440, "fly_fuckery/radio_fly.rds")
# uae161 <- hent_rute("8963d5", "2026-11-20 10:24:48"); saveRDS(uae161, "fly_fuckery/radio_uae161.rds")
# aal71  <- hent_rute("ac3448", "2026-11-20 10:28:00"); saveRDS(aal71,  "fly_fuckery/radio_aal71.rds")

# Indlæs de gemte ruter
dlh440 <- readRDS("fly_fuckery/radio_fly.rds")
uae161 <- readRDS("fly_fuckery/radio_uae161.rds")
aal71  <- readRDS("fly_fuckery/radio_aal71.rds")


# =====================================================================
# 3. OPKALD FUNDET I TRANSSKRIPTIONEN
#    Tid = tid i optagelsen + 10:00 UTC (optagelsen starter ca. kl. 10:00 UTC)
# =====================================================================

opkald <- data.frame(
  fly     = c("UAE161", "AAL71"),
  tid_utc = c("2026-11-20 10:24:48", "2026-11-20 10:28:00"),
  tekst   = c("Emirates 161: '... level 340'",
              "American 71: 'Any chance direct Bilto for American Seven One?'")
)

# Tabel: flyets position, højde og kurs ved hvert opkald
ruter <- list(UAE161 = uae161, AAL71 = aal71)

resultat <- do.call(rbind, lapply(seq_len(nrow(opkald)), function(k) {
  m <- naermeste(ruter[[opkald$fly[k]]], opkald$tid_utc[k])
  data.frame(fly           = opkald$fly[k],
             opkald_utc    = substr(opkald$tid_utc[k], 12, 19),
             maaling_utc   = format(as.POSIXct(as.numeric(m$time), origin = "1970-01-01", tz = "UTC"), "%H:%M:%S"),
             lat           = round(as.numeric(m$lat), 2),
             lng           = round(as.numeric(m$lng), 2),
             hoejde_m      = as.numeric(m$alt),
             hoejde_fod    = round(as.numeric(m$alt) / 0.3048),
             kurs          = as.numeric(m$crs),
             sagt_i_radio  = opkald$tekst[k])
}))
resultat

# UAE161 meldte "level 340" = flight level 340 = 34.000 fod.
# OpenSky målte 10.363 m = 33.999 fod -> radio og data passer sammen.


# =====================================================================
# 4. KORT (figur 12 og 13) - tag skærmbillede i Viewer (Export -> Save as Image)
# =====================================================================

kort_med_radio(uae161, opkald$tid_utc[1], opkald$tekst[1])   # figur 12
kort_med_radio(aal71,  opkald$tid_utc[2], opkald$tekst[2])   # figur 13

# DLH440 (hørt live 10:30-10:32 UTC, men ikke fundet i transskriptionen)
kort_med_radio(dlh440, "2026-11-20 10:31:00", "Lufthansa 440: lyttet efter, ikke fundet i optagelsen")


# =====================================================================
# 5. HØJDE OG KURS OVER TID MED RADIOOPKALD MARKERET
# =====================================================================
library(ggplot2)

lys_lilla <- "#d9c8f5"
tema_fly  <- theme_minimal(base_size = 12) +
  theme(plot.title = element_text(face = "bold"),
        plot.subtitle = element_text(colour = "grey35"),
        panel.grid.minor = element_blank())

# Højde over tid. meldt_fl = flight level piloten sagde i radioen (fx 340)
plot_radio_hoejde <- function(fly, opkald_utc, titel, meldt_fl = NULL) {
  fly <- as.data.frame(fly)
  fly$tid <- as.POSIXct(as.numeric(fly$time), origin = "1970-01-01", tz = "UTC")
  fly$alt <- as.numeric(fly$alt)
  # Fjern manglende værdier og fejlmålinger (fly flyver ikke over 15 km), og sortér efter tid
  fly <- fly[!is.na(fly$alt) & !is.na(fly$tid) & fly$alt >= 0 & fly$alt <= 15000, ]
  fly <- fly[order(fly$tid), ]
  t_op <- as.POSIXct(opkald_utc, tz = "UTC")
  m <- naermeste(fly, opkald_utc)
  
  p <- ggplot(fly, aes(x = tid, y = alt)) +
    geom_ribbon(aes(ymin = 0, ymax = alt), fill = lys_lilla, alpha = 0.7) +
    geom_line(colour = moerk_lilla, linewidth = 1.1) +
    geom_vline(xintercept = t_op, linetype = "dashed", colour = "grey20") +
    annotate("label", x = t_op, y = max(fly$alt) * 0.45, size = 3.6,
             hjust = ifelse(t_op < mean(range(fly$tid)), -0.05, 1.05),   # boksen på den side, hvor der er plads
             label.size = 0, fill = "white",
             label = paste0("Radioopkald ", format(t_op, "%H:%M"), " UTC\n",
                            "OpenSky: ", format(m$alt, big.mark = ".", decimal.mark = ","), " m = ",
                            format(round(as.numeric(m$alt) / 0.3048), big.mark = ".", decimal.mark = ","), " fod"))
  if (!is.null(meldt_fl)) {
    meldt_m <- meldt_fl * 100 * 0.3048
    p <- p +
      geom_hline(yintercept = meldt_m, colour = "#e8590c", linewidth = 0.8) +
      annotate("text", x = min(fly$tid), y = meldt_m, vjust = -0.6, hjust = 0, size = 3.6,
               colour = "#e8590c", fontface = "bold",
               label = paste0("Meldt i radioen: flight level ", meldt_fl,
                              " (", format(round(meldt_m), big.mark = ".", decimal.mark = ","), " m)"))
  }
  p +
    scale_y_continuous(labels = function(x) paste(format(x, big.mark = ".", decimal.mark = ",",
                                                         scientific = FALSE), "m"),
                       expand = expansion(mult = c(0, 0.12))) +
    scale_x_datetime(date_labels = "%H:%M") +
    labs(title = titel, x = "Tid (UTC)", y = "Højde") +
    tema_fly
}

# Kurs over tid
plot_radio_kurs <- function(fly, opkald_utc, titel) {
  fly <- as.data.frame(fly)
  fly$tid <- as.POSIXct(as.numeric(fly$time), origin = "1970-01-01", tz = "UTC")
  ggplot(fly, aes(x = tid, y = as.numeric(crs))) +
    geom_point(colour = moerk_lilla, size = 1.6) +
    geom_vline(xintercept = as.POSIXct(opkald_utc, tz = "UTC"), linetype = "dashed", colour = "grey20") +
    scale_y_continuous(limits = c(0, 360), breaks = seq(0, 360, 90),
                       labels = c("N", "Ø", "S", "V", "N")) +
    scale_x_datetime(date_labels = "%H:%M") +
    labs(title = titel, subtitle = "Stiplet linje = radioopkald",
         x = "Tid (UTC)", y = "Kurs") +
    tema_fly
}

p_uae_hoejde <- plot_radio_hoejde(uae161, opkald$tid_utc[1],
                                  "UAE161: piloten melder flight level 340, og OpenSky måler 34.000 fod",
                                  meldt_fl = 340)
p_aal_hoejde <- plot_radio_hoejde(aal71, opkald$tid_utc[2],
                                  "AAL71: højde over tid med radioopkald markeret")
p_uae_kurs   <- plot_radio_kurs(uae161, opkald$tid_utc[1], "UAE161: kurs over tid")
p_aal_kurs   <- plot_radio_kurs(aal71,  opkald$tid_utc[2], "AAL71: kurs over tid")

p_uae_hoejde
p_aal_hoejde
p_uae_kurs
p_aal_kurs

ggsave("plot_radio_uae161_hoejde.png", p_uae_hoejde, width = 10, height = 5, dpi = 300, bg = "white")
ggsave("plot_radio_aal71_hoejde.png",  p_aal_hoejde, width = 10, height = 5, dpi = 300, bg = "white")
ggsave("plot_radio_uae161_kurs.png",   p_uae_kurs,   width = 10, height = 4, dpi = 300, bg = "white")
ggsave("plot_radio_aal71_kurs.png",    p_aal_kurs,   width = 10, height = 4, dpi = 300, bg = "white")