# =====================================================================
# 0. OPSÆTNING
# =====================================================================

library(jsonlite)
library(httr)
library(stringr)
library(leaflet)
source("util.R")

library(ggplot2)
library(dplyr)
library(leaflet)

# hent flydata eksempel og gemte rds fly
circ_fly <- readRDS("fly_fuckery/circMT.rds")
circ_fly$sd <- sd(circ_fly$crs)
currentDF <- readRDS("fly_fuckery/currentDF2.rds")
flight_example <- readRDS("fly_fuckery/flight_example.rds")
fly_data <- readRDS("fly_fuckery/fly_liste.rds")

# gemmer data i en rds fil i mappen training_data
training_files <- list.files(path = "./fly_fuckery/training_data", pattern = "*rds", full.names=T)
training <- lapply(training_files, readRDS)

# tilføj standardafvigelse til training files
training_lists <- list()
for (i in 1:length(training)){
  data <- training[[i]]
  data$sd <- sd(data$crs)
  training_lists <- c(training_lists, list(data))
}

# =====================================================================
# 1. URL information
# =====================================================================

baseurl <- "https://opensky-network.org/api"

# begge endpoints
endpointS <- "/states/all"
endpointT <- "/tracks/all"

# begge query's uden parametre
queryS <- paste0(baseurl,endpointS)
queryT <- paste0(baseurl,endpointT)

# Token
token=getToken()

#### NORDSØEN ####
lamin=52.055129
lamax=56.196869
lomin=-6.065140
lomax=4.305954

# =====================================================================
# 2. Hent alle states - fly tilgængelige lige nu
# =====================================================================

# Udkommenteret så vi ikke løber tør for tokens
#url_states <- getRawFrombbox(queryS, token, lamin, lomin, lamax, lomax)
#current=content(url_states, as="text")
#currentJS=fromJSON(current)
#currentDF=currentJS$states
#currentDF=as.data.frame(currentDF)
# kalder funktionen SetColnames der sætter kolonnenavnene
#currentDF <- SetColnames(currentDF)
#saveRDS(currentDF, "currentDF3.rds")

# Find hvilke lande der har fly i luften
tabel_data <- as.data.frame(table(currentDF$origin_country))

# Vi kan ikke finde tracks for fly der ikke er i luften
# Det vil sige alle fly hvor on_ground = TRUE skal fjernes
in_air <- currentDF[currentDF$on_ground == FALSE,]

# find all icaos and put in vector
icaov=in_air$icao24

# =====================================================================
# 3. Hent tracks for enkelt testfly
# =====================================================================

# Udkommenteret så vi ikke løber tør for tokens
# find all tracks for each normal icao
# start with one test icao
#ictest <- icaov[[30]]
#url_tracks <- paste0(queryT,"?icao24=",ictest, "&time=0")
#res <- GET(url_tracks,add_headers(Authorization = paste("Bearer", token)))
#res$status_code
#resretval <- fromJSON(content(res, as="text"))
#flight_example <- as.data.frame(resretval)
#colnames(flight_example) <- colnames(circ_fly)
#saveRDS(flight_example, "flight_example2.rds")

# plots for nyt fly
Plot_lat_lng(flight_example)
Plot_time_crs(flight_example)
Plot_time_alt(flight_example)
MapPlot(flight_example)

# plots for cirklende fly
Plot_lat_lng(circ_fly)
Plot_time_crs(circ_fly)
Plot_time_alt(circ_fly)
MapPlot(circ_fly)



# =====================================================================
# 4. Hent tracks for alle fly
# =====================================================================

# Udkommenteret så vi ikke løber tør for tokens
# gem tracks for alle fly
#fly_liste <- list()
#for (i in 1:length(icaov)){
#  url_tracks <- paste0(queryT,"?icao24=",icaov[i], "&time=0")
#  res <- GET(url_tracks,add_headers(Authorization = paste("Bearer", token)))
#  if (res$status_code == 200){
#     resretval <- fromJSON(content(res, as="text", encoding = "UTF-8"))
#     fly <- as.data.frame(resretval)
#     colnames(fly) <- colnames(circ_fly)
#     fly_liste <- c(fly_liste, list(fly))
#  }
#}
#saveRDS(fly_liste, "fly_liste2.rds")

# =====================================================================
# 5. Find alle cirkulerende fly
# =====================================================================

# Sætter en højdegrænse for at fjerne helikoptere og privatfly
fly_high <- list()
for (i in 1:length(fly_data)){
  fly <- fly_data[[i]]
  if (max(fly[,"alt"]) > 4500){
    fly$sd <- sd(fly$crs)
    fly_high <- c(fly_high, list(fly))
  }
}

# tilføj wulfs data til det skal testes på om det cirkulerer
fly_all <- c(fly_high, training_lists, list(circ_fly))

# Find ud af om flyende cirkulerer. Gem icao navnene på dem der gør
fly_circle <- list()
icao_off <- data.frame(icao = NULL)
breaks <- seq(0, 360, by = 15) # bruges til at tjekke om et fly cirkulerer

for (i in 1:length(fly_all)) {
  fly <- fly_all[[i]]
  # Tæl antal punkter i hvert 10-graders interval
  interval_check <- table(cut(
    fly$crs,
    breaks = breaks,
    right = FALSE # inkluderer 0 i første interval
    # Så første interval kører fra 0-15, inkluderer 0 men ikke 15
  ))
  if (all(interval_check > 0)) {
    fly_circle <- c(fly_circle, list(fly))
    icao_off <- rbind(icao_off, data.frame(icao = fly$icao24[1]))
  }
}

# Lav dataframe der indeholder alle icaonavne, deres standardafvigelse,
# og om hvorvidt de cirkulerer
dataframe <- data.frame(icao=NULL, sdcourse=NULL)
for (i in 1:length(fly_all)){
  fly <- fly_all[[i]]
  data <- data.frame(icao = fly$icao24[1], 
                     sdcourse = fly$sd[1],
                     lmres = summary(lm(lat ~ lng+crs+alt, data=fly))$r.squared,
                     isOff = ifelse(fly$icao24[1] %in% icao_off[,1], 1, 0))
  sdcourse <- data.frame
  dataframe <- rbind(dataframe, data)
}

sus_fly <- dataframe[dataframe$isOff == 1,]
saveRDS(sus_fly, "fly_fuckery/sus_fly.rds")
readRDS("fly_fuckery/sus_fly.rds")




# =====================================================================
# 6. Send e-mail
# =====================================================================

library(blastula)

email <- compose_email(
  body = md("
    Hej,

    Hermed fremsendes datasættet sus_fly.

    Venlig hilsen
    Gruppe 11
  ")
) |>
  add_attachment(file = "fly_fuckery/sus_fly.rds")

#smtp_send(
#  email,
#  from = "ellenthygesen1@gmail.com",
#  to = "sigint-20@fe.dk",
#  subject = "Sus cirkulerende fly",
#  credentials = creds(
#    user = "ellenthygesen1@gmail.com",
#    provider = "gmail"
#  )
#)
#------------------------------------------------------------------------
#PLOTS EMMA
#------------------------------------------------------------------------
# =====================================================================
# PLOTS (erstatter Plot_lat_lng, Plot_time_crs, Plot_time_alt, MapPlot)
# Indsæt efter afsnit 2 (når flight_example og circ_fly er indlæst)
# =====================================================================

moerk_lilla <- "#5b2a9e"
lys_lilla   <- "#d9c8f5"
graa        <- "#8a879c"

tema_fly <- theme_minimal(base_size = 12) +
  theme(plot.title = element_text(face = "bold"),
        plot.subtitle = element_text(colour = "grey35"),
        panel.grid.minor = element_blank())

# Tid i minutter fra første måling
klargoer <- function(fly) {
  fly <- as.data.frame(fly)
  fly$minutter <- (as.numeric(fly$time) - min(as.numeric(fly$time), na.rm = TRUE)) / 60
  fly
}

# Rute set oppefra med pile i flyveretningen (erstatter Plot_lat_lng)
plot_rute <- function(fly, titel, antal_pile = 12) {
  fly <- klargoer(fly)
  fly$lng <- as.numeric(fly$lng); fly$lat <- as.numeric(fly$lat)
  n <- nrow(fly)
  idx <- unique(round(seq(2, n - 1, length.out = antal_pile)))
  pile <- data.frame(x = fly$lng[idx], y = fly$lat[idx],
                     xend = fly$lng[idx + 1], yend = fly$lat[idx + 1])
  # Placering af kompasrosen i øverste højre hjørne
  xr <- range(fly$lng, na.rm = TRUE); yr <- range(fly$lat, na.rm = TRUE)
  dx <- diff(xr); dy <- diff(yr); r <- 0.07 * max(dx, dy)
  kx <- xr[2] + 0.12 * dx; ky <- yr[2] - 0.05 * dy
  ggplot(fly, aes(x = lng, y = lat)) +
    annotate("segment", x = kx, xend = kx, y = ky - r, yend = ky + r, colour = "grey30",
             linewidth = 0.8, arrow = arrow(length = unit(0.3, "cm"), type = "closed")) +
    annotate("segment", x = kx - r, xend = kx + r, y = ky, yend = ky, colour = "grey30", linewidth = 0.5) +
    annotate("text", x = kx, y = ky + r * 1.45, label = "N", fontface = "bold", size = 4.5) +
    annotate("text", x = kx + r * 1.4, y = ky, label = "\u00d8", size = 3.5, colour = "grey30") +
    annotate("text", x = kx, y = ky - r * 1.4, label = "S", size = 3.5, colour = "grey30") +
    annotate("text", x = kx - r * 1.4, y = ky, label = "V", size = 3.5, colour = "grey30") +
    geom_path(aes(colour = minutter), linewidth = 1.2, lineend = "round") +
    geom_segment(data = pile, aes(x = x, y = y, xend = xend, yend = yend),
                 colour = "black", linewidth = 0.4,
                 arrow = arrow(length = unit(0.25, "cm"), type = "closed")) +
    geom_point(data = fly[1, ], colour = "black", size = 3.5, shape = 17) +
    geom_point(data = fly[n, ], colour = "black", size = 3.5, shape = 15) +
    scale_colour_gradient(low = lys_lilla, high = moerk_lilla, name = "Minutter") +
    coord_quickmap() +
    labs(title = titel, subtitle = "Pilene viser flyveretningen. Trekant = start, firkant = slut",
         x = "L\u00e6ngdegrad", y = "Breddegrad") +
    tema_fly
}

# Kompas: hvor meget tid har flyet brugt i hver retning?
plot_kompas <- function(fly, titel, farve = moerk_lilla) {
  fly <- as.data.frame(fly)
  ggplot(data.frame(crs = as.numeric(fly$crs) %% 360), aes(x = crs)) +
    geom_histogram(breaks = seq(0, 360, by = 15), fill = farve, colour = "white",
                   linewidth = 0.3, boundary = 0) +
    coord_polar(start = 0, direction = 1) +
    scale_x_continuous(limits = c(0, 360), breaks = c(0, 45, 90, 135, 180, 225, 270, 315),
                       labels = c("N", "N\u00d8", "\u00d8", "S\u00d8", "S", "SV", "V", "NV")) +
    labs(title = titel, subtitle = "S\u00f8jlerne viser antal m\u00e5linger i hver retning (15 grader)",
         x = NULL, y = NULL) +
    tema_fly +
    theme(axis.text.y = element_blank(),
          axis.text.x = element_text(size = 12, face = "bold", colour = "black"))
}

# To kompasser ved siden af hinanden (hvert kompas skaleres til sin egen største retning)
plot_kompas_sammenlign <- function(normal, cirkel) {
  tael <- function(fly, navn) {
    crs <- as.numeric(fly$crs) %% 360
    midt <- seq(7.5, 352.5, by = 15)
    antal <- as.numeric(table(cut(crs, breaks = seq(0, 360, by = 15), right = FALSE)))
    data.frame(retning = midt, andel = antal / max(antal), fly = navn)
  }
  d <- rbind(tael(normal, "Normalt fly"), tael(cirkel, "Cirklende fly"))
  d$fly <- factor(d$fly, levels = c("Normalt fly", "Cirklende fly"))
  ggplot(d, aes(x = retning, y = andel, fill = fly)) +
    geom_col(width = 15, colour = "white", linewidth = 0.3) +
    coord_polar(start = 0, direction = 1) +
    facet_wrap(~ fly) +
    scale_fill_manual(values = c("Normalt fly" = graa, "Cirklende fly" = moerk_lilla), guide = "none") +
    scale_x_continuous(limits = c(0, 360), breaks = c(0, 45, 90, 135, 180, 225, 270, 315),
                       labels = c("N", "N\u00d8", "\u00d8", "S\u00d8", "S", "SV", "V", "NV")) +
    scale_y_continuous(limits = c(0, 1)) +
    labs(title = "Kompas: hvilke retninger har flyet fl\u00f8jet i?",
         subtitle = "Et normalt fly holder \u00e9n retning. Et cirklende fly flyver i alle retninger.",
         x = NULL, y = NULL) +
    tema_fly +
    theme(axis.text.y = element_blank(),
          axis.text.x = element_text(size = 11, face = "bold", colour = "black"),
          strip.text = element_text(face = "bold", size = 13))
}

# Kurs over tid (erstatter Plot_time_crs)
plot_kurs <- function(fly, titel) {
  fly <- klargoer(fly)
  ggplot(fly, aes(x = minutter, y = as.numeric(crs))) +
    geom_point(colour = moerk_lilla, size = 1.5, alpha = 0.8) +
    scale_y_continuous(limits = c(0, 360), breaks = seq(0, 360, 90),
                       labels = c("N", "Ø", "S", "V", "N")) +
    labs(title = titel, x = "Minutter siden første måling", y = "Kurs") +
    tema_fly
}

# Højde over tid (erstatter Plot_time_alt)
plot_hoejde <- function(fly, titel) {
  fly <- klargoer(fly)
  ggplot(fly, aes(x = minutter, y = as.numeric(alt))) +
    geom_area(fill = lys_lilla, alpha = 0.7) +
    geom_line(colour = moerk_lilla, linewidth = 1.1) +
    scale_y_continuous(labels = function(x) paste(format(x, big.mark = ".", decimal.mark = ",",
                                                         scientific = FALSE), "m")) +
    labs(title = titel, x = "Minutter siden første måling", y = "Højde") +
    tema_fly
}


# Nordpil til kortene
nordpil_html <- paste0(
  "<div style='text-align:center;font-family:sans-serif;font-weight:bold;",
  "background:white;padding:4px 8px;border-radius:6px;box-shadow:0 1px 4px rgba(0,0,0,.3);'>",
  "<div style='font-size:22px;line-height:22px;color:", "#5b2a9e", "'>&#9650;</div>N</div>")

# Kort (erstatter MapPlot)
kort_fly <- function(fly, farve = moerk_lilla, navn = fly$icao24[1]) {
  fly <- as.data.frame(fly)
  n <- nrow(fly)
  leaflet(fly) %>%
    addTiles() %>%
    addPolylines(lng = ~as.numeric(lng), lat = ~as.numeric(lat),
                 color = farve, weight = 4, opacity = 0.9,
                 popup = paste0("<b>", navn, "</b><br>Maks. højde: ",
                                round(max(as.numeric(fly$alt), na.rm = TRUE)), " m")) %>%
    addCircleMarkers(lng = as.numeric(fly$lng[1]), lat = as.numeric(fly$lat[1]),
                     radius = 7, color = "white", weight = 2,
                     fillColor = "black", fillOpacity = 1, label = "Start") %>%
    addCircleMarkers(lng = as.numeric(fly$lng[n]), lat = as.numeric(fly$lat[n]),
                     radius = 7, color = "white", weight = 2,
                     fillColor = farve, fillOpacity = 1, label = "Slut") %>%
    addControl(html = nordpil_html, position = "topright")
}

# Normalt og cirklende fly på samme kort
kort_sammenlign <- function(normal, cirkel) {
  normal <- as.data.frame(normal); cirkel <- as.data.frame(cirkel)
  leaflet() %>%
    addTiles() %>%
    addPolylines(lng = as.numeric(normal$lng), lat = as.numeric(normal$lat),
                 color = graa, weight = 4, opacity = 0.9, group = "Normalt fly") %>%
    addPolylines(lng = as.numeric(cirkel$lng), lat = as.numeric(cirkel$lat),
                 color = moerk_lilla, weight = 4, opacity = 0.9, group = "Cirklende fly") %>%
    addLegend("bottomright", colors = c(graa, moerk_lilla),
              labels = c("Normalt fly", "Cirklende fly"), opacity = 1) %>%
    addLayersControl(overlayGroups = c("Normalt fly", "Cirklende fly"),
                     options = layersControlOptions(collapsed = FALSE)) %>%
    addControl(html = nordpil_html, position = "topleft")
}

# ---- Opgave 3.1: fly fordelt på lande ----
lande <- as.data.frame(table(currentDF$origin_country))
colnames(lande) <- c("land", "antal")
lande <- head(lande[order(-lande$antal), ], 15)

p_lande <- ggplot(lande, aes(x = antal, y = reorder(land, antal))) +
  geom_col(fill = moerk_lilla, width = 0.7) +
  geom_text(aes(label = antal), hjust = -0.3, size = 3.8) +
  scale_x_continuous(expand = expansion(mult = c(0, 0.1))) +
  labs(title = "Fly over Nordsøen fordelt på land",
       subtitle = paste("De", nrow(lande), "lande med flest fly i luftrummet"),
       x = "Antal fly", y = NULL) +
  tema_fly + theme(panel.grid.major.y = element_blank())
#p_lande

# ---- Opgave 3.2: plots for nyt fly ----
p_rute_normal   <- plot_rute(flight_example, "Normalt fly");                                 p_rute_normal
p_kurs_normal   <- plot_kurs(flight_example, "Normalt fly: kursen er næsten konstant");  p_kurs_normal
p_hoejde_normal <- plot_hoejde(flight_example, "Normalt fly");                               p_hoejde_normal
#kort_fly(flight_example, farve = graa)

# ---- Opgave 3.2: plots for cirklende fly ----
p_rute_cirkel   <- plot_rute(circ_fly, "Cirklende fly");                                     p_rute_cirkel
p_kurs_cirkel   <- plot_kurs(circ_fly, "Cirklende fly: kursen løber hele vejen rundt"); p_kurs_cirkel
p_hoejde_cirkel <- plot_hoejde(circ_fly, "Cirklende fly");                                   p_hoejde_cirkel
#kort_fly(circ_fly)

# Begge fly på samme kort
#kort_sammenlign(flight_example, circ_fly)

# Kompas for hvert fly og side om side
p_kompas_normal <- plot_kompas(flight_example, "Kompas: normalt fly", farve = graa); p_kompas_normal
p_kompas_cirkel <- plot_kompas(circ_fly, "Kompas: cirklende fly");                   p_kompas_cirkel
p_kompas        <- plot_kompas_sammenlign(flight_example, circ_fly);                  p_kompas


# =====================================================================
# 5. Find alle cirkulerende fly
# =====================================================================

# Samlet drejning: hvor mange grader har flyet drejet i alt?
# Kursen hopper fra 359 til 0 grader, så hver ændring lægges i intervallet -180 til 180.
kurs_aendring <- function(crs) {
  d <- diff(as.numeric(crs))
  ((d + 180) %% 360) - 180
}

# Sætter en højdegrænse for at fjerne helikoptere og privatfly
fly_high <- list()
for (i in 1:length(fly_data)) {
  fly <- fly_data[[i]]
  if (max(as.numeric(fly[, "alt"]), na.rm = TRUE) > 4500) {
    fly_high <- c(fly_high, list(fly))
  }
}

# Tilføj Wulfs data, så algoritmen også testes på kendte cirklende fly
fly_all <- c(fly_high, training_lists, list(circ_fly))

# Grænser for algoritmen
breaks      <- seq(0, 360, by = 15)  # 24 intervaller på 15 grader
min_cirkler <- 0                      # 0 = kun metode 1 bruges (antal cirkler gemmes kun til diskussion)

# Kør algoritmen på hvert fly og saml resultatet i én dataframe
# Kun drejninger over Nordsøen tæller for VORES fly (fly_high).
# Wulfs træningsfly er fra andre områder, så for dem bruges hele ruten.
kun_nordsoeen <- TRUE
kilde <- c(rep("Egne fly", length(fly_high)),
           rep("Wulf", length(training_lists) + 1))

dataframe <- do.call(rbind, Map(function(fly, kilde_fly) {
  crs <- as.numeric(fly$crs)
  sd_hele_ruten <- sd(crs, na.rm = TRUE)
  if (kun_nordsoeen && kilde_fly == "Egne fly") {
    inde <- as.numeric(fly$lat) >= lamin & as.numeric(fly$lat) <= lamax &
      as.numeric(fly$lng) >= lomin & as.numeric(fly$lng) <= lomax
    crs <- crs[which(inde)]
    if (length(crs) < 3) crs <- c(0, 0, 0)   # flyet var næsten ikke over Nordsøen
  }
  
  # Metode 1: har kursen været i alle 24 intervaller på 15 grader?
  interval_check <- table(cut(crs, breaks = breaks, right = FALSE))
  antal_retninger <- sum(interval_check > 0)          # hvor mange af de 24 retninger flyet har fløjet i
  alle_retninger  <- antal_retninger == 24
  
  # Metode 2: hvor mange hele cirkler har flyet drejet?
  antal_cirkler <- abs(sum(kurs_aendring(crs), na.rm = TRUE)) / 360
  
  data.frame(icao           = fly$icao24[1],
             sdcourse       = sd_hele_ruten,
             antal_retninger = antal_retninger,
             antal_cirkler  = round(antal_cirkler, 1),
             alle_retninger = alle_retninger,
             lmres          = summary(lm(lat ~ lng + crs + alt, data = fly))$r.squared,
             kilde          = kilde_fly,
             isOff          = as.numeric(alle_retninger & antal_cirkler >= min_cirkler))
}, fly_all, kilde))

# Mistænkelige fly
sus_fly    <- dataframe[dataframe$isOff == 1, ]
fly_circle <- fly_all[dataframe$isOff == 1]
#sus_fly
#saveRDS(sus_fly, "fly_fuckery/sus_fly.rds")


# =====================================================================
# Plots af algoritmens resultat
# =====================================================================

dataframe$type <- ifelse(dataframe$isOff == 1, "Cirklende", "Normal")
farver_type <- c("Cirklende" = moerk_lilla, "Normal" = graa)

# Standardafvigelse på kursen
p_sd <- ggplot(dataframe, aes(x = sdcourse, y = kilde, colour = type)) +
  geom_jitter(height = 0.15, size = 2.5, alpha = 0.75) +
  scale_colour_manual(values = farver_type, name = NULL) +
  labs(title = "Standardafvigelse p\u00e5 kursen kan ikke skille flyene ad",
       subtitle = "Normale fly mod nord f\u00e5r h\u00f8j spredning, fordi kursen hopper mellem 359 og 0 grader",
       x = "Standardafvigelse p\u00e5 kurs (grader)", y = NULL) +
  tema_fly + theme(legend.position = "top")
#p_sd

# Algoritmen: hvor mange af de 24 retninger har flyet fløjet i?
p_retninger <- ggplot(dataframe, aes(x = antal_retninger, y = kilde, colour = type)) +
  annotate("rect", xmin = 23.5, xmax = 24.5, ymin = -Inf, ymax = Inf, fill = lys_lilla, alpha = 0.5) +
  geom_jitter(height = 0.15, width = 0.15, size = 2.8, alpha = 0.8) +
  scale_colour_manual(values = farver_type, name = NULL) +
  scale_x_continuous(limits = c(0, 24.5), breaks = c(0, 6, 12, 18, 24)) +
  labs(title = "Algoritmen: har flyet fl\u00f8jet i alle 24 retninger?",
       subtitle = "Kursen deles i 24 intervaller p\u00e5 15 grader. Fly i det lilla felt markeres som cirklende.",
       x = "Antal retninger flyet har fl\u00f8jet i (ud af 24)", y = NULL) +
  tema_fly + theme(legend.position = "top")
#p_retninger

# Resultat: hvor mange fly bliver fundet fra hver kilde?
#table(dataframe$kilde, dataframe$type)

# Kort over hvert mistænkeligt fly
#for (i in seq_along(fly_circle)) print(kort_fly(fly_circle[[i]]))


# =====================================================================
# Wulf kode - skal det bruges???
# =====================================================================

# Alt nedenstående er noget wulf oprindeligt har skrevet

#tal <-1

#MapPlot(fly_circle[[tal]])

# Giver forskellen mellem hver observation i forhold til den forrige
#rows <- nrow(fly_circle[[tal]])
#dlatv=diff(fly_circle[[tal]]$lat)
#dlnv=diff(fly_circle[[tal]]$lng)
#dlatv=c(0,dlatv)
#dlnv=c(0,dlnv)
#fly_circle[[tal]]$dlat=dlatv
#fly_circle[[tal]]$dlng=dlnv
#min(abs(fly_circle[[tal]]$dlng[5:rows]))
#min(abs(fly_circle[[tal]]$dlat[5:rows]))

#ggplot(flight_example,aes(x=time,y=lat))+geom_line()+
#  theme(axis.text.x = element_text(angle = 90))+
#  scale_x_continuous(breaks = scales::pretty_breaks(n=35))

#tal <- 15
#min(fly_high[[tal]]$crs)
#max(fly_high[[tal]]$crs)

#MapPlot(training_lists[[1]])
#MapPlot(fly_circle[[1]])
# apply circmetrics
# lm
#cmodel=lm(lat ~ lng+crs+alt, data=flight_example)
#summary(cmodel)
# sd
#csd=sd(flight_example$crs)

# myOwnDetectionAlg
# use diff to track change of fortegn

# run R script from terminal



# =====================================================================
# 3.6 SIDE QUEST: Radiokommunikation (LiveATC) koblet med OpenSky-data
# Kræver: queryS, queryT, lamin/lamax/lomin/lomax, getToken(), getRawFrombbox(),
#         SetColnames(), circ_fly og plotfunktionerne fra opg3_plots_og_algoritme.R
# =====================================================================

library(jsonlite)
library(httr)
library(leaflet)
library(ggplot2)

token <- getToken()

# ---------------------------------------------------------------------
# 1. Find fly over Nordsøen lige nu (kandidater at lytte efter)
# ---------------------------------------------------------------------
url_states <- getRawFrombbox(queryS, token, lamin, lomin, lamax, lomax)
url_states$status_code   # skal være 200

nu <- SetColnames(as.data.frame(fromJSON(content(url_states, as = "text", encoding = "UTF-8"))$states))
kandidater <- nu[nu$on_ground == FALSE & trimws(nu$callsign) != "", ]
kandidater$callsign <- trimws(kandidater$callsign)
kandidater[, c("icao24", "callsign", "origin_country", "latitude", "longitude", "baro_altitude")]
saveRDS(kandidater, "fly_fuckery/radio_kandidater.rds")

# ---------------------------------------------------------------------
# 2. (Valgfrit) Log flyets live-position hvert 30. sekund, mens I lytter
# ---------------------------------------------------------------------
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
# dlh_log <- log_fly("DLH440", minutter = 10)
# saveRDS(dlh_log, "fly_fuckery/radio_log.rds")

# ---------------------------------------------------------------------
# 3. Hent flyets rute (DLH440 = icao24 3c4a8c)
# ---------------------------------------------------------------------
radio_icao <- "3c4a8c"

radio_fly <- as.data.frame(fromJSON(content(
  GET(paste0(queryT, "?icao24=", radio_icao, "&time=0"),
      add_headers(Authorization = paste("Bearer", token))),
  as = "text", encoding = "UTF-8")))
colnames(radio_fly) <- setdiff(colnames(circ_fly), "sd")
saveRDS(radio_fly, "fly_fuckery/radio_fly.rds")
# radio_fly <- readRDS("fly_fuckery/radio_fly.rds")   # brug den gemte, så API-kald spares

# ---------------------------------------------------------------------
# 4. De opkald vi hørte på London Control (North Sea), 133.700 MHz
#    Tider i UTC (dansk tid minus 1 time)
# ---------------------------------------------------------------------
opkald <- data.frame(
  tid_utc = c("2026-11-20 10:31:00", "2026-11-20 10:32:30"),
  tekst   = c("Opkald 1 med London Control (11:30-11:32 dansk tid)",
              "Opkald 2 med London Control (11:32:00-11:32:55 dansk tid)")
)

# Flyets position, højde og kurs ved hvert opkald (nærmeste måling)
naermeste <- function(fly, tid_utc) {
  t <- as.numeric(as.POSIXct(tid_utc, tz = "UTC"))
  fly[which.min(abs(as.numeric(fly$time) - t)), ]
}
ved_opkald <- do.call(rbind, lapply(opkald$tid_utc, function(x) naermeste(radio_fly, x)))
ved_opkald$opkald_utc   <- opkald$tid_utc
ved_opkald$maaling_utc  <- format(as.POSIXct(as.numeric(ved_opkald$time), origin = "1970-01-01", tz = "UTC"))
ved_opkald[, c("opkald_utc", "maaling_utc", "lat", "lng", "alt", "crs")]

# Tidspunkter for alle målinger (viser hvor langt der er mellem punkterne)
format(as.POSIXct(as.numeric(radio_fly$time), origin = "1970-01-01", tz = "UTC"))

# ---------------------------------------------------------------------
# 5. Kort med ruten og opkaldene
# ---------------------------------------------------------------------
kort_med_radio_flere <- function(fly, opkald) {
  fly <- as.data.frame(fly)
  kort <- kort_fly(fly)
  for (k in seq_len(nrow(opkald))) {
    i <- which.min(abs(as.numeric(fly$time) - as.numeric(as.POSIXct(opkald$tid_utc[k], tz = "UTC"))))
    kort <- kort %>%
      addMarkers(lng = as.numeric(fly$lng[i]), lat = as.numeric(fly$lat[i]),
                 label = paste("Opkald", k),
                 popup = paste0("<b>Opkald ", k, ": ", opkald$tid_utc[k], " UTC</b><br>",
                                opkald$tekst[k], "<br>Højde: ", fly$alt[i], " m<br>Kurs: ",
                                fly$crs[i], "°"))
  }
  kort
}
kort_med_radio_flere(radio_fly, opkald)

# ---------------------------------------------------------------------
# 6. Højde over tid med opkaldene markeret
# ---------------------------------------------------------------------
radio_plot <- radio_fly
radio_plot$tid <- as.POSIXct(as.numeric(radio_plot$time), origin = "1970-01-01", tz = "UTC")
opkald$tid <- as.POSIXct(opkald$tid_utc, tz = "UTC")

p_radio_hoejde <- ggplot(radio_plot, aes(x = tid, y = as.numeric(alt))) +
  geom_area(fill = "#d9c8f5", alpha = 0.7) +
  geom_line(colour = "#5b2a9e", linewidth = 1.1) +
  geom_vline(data = opkald, aes(xintercept = tid), linetype = "dashed", colour = "grey30") +
  geom_text(data = opkald, aes(x = tid, y = max(as.numeric(radio_plot$alt)) * 1.05,
                               label = paste("Opkald", seq_len(nrow(opkald)))),
            hjust = 1.05, size = 3.5) +
  scale_y_continuous(labels = function(x) paste(format(x, big.mark = ".", decimal.mark = ",",
                                                       scientific = FALSE), "m")) +
  labs(title = "DLH440: højde over tid med radioopkald markeret",
       subtitle = "Stiplede linjer = opkald hørt på London Control (North Sea). Tid i UTC.",
       x = "Tid (UTC)", y = "Højde") +
  theme_minimal(base_size = 12) +
  theme(plot.title = element_text(face = "bold"), panel.grid.minor = element_blank())
p_radio_hoejde
ggsave("plot_radio_hoejde.png", p_radio_hoejde, width = 9, height = 5, dpi = 300, bg = "white")


# Installér én gang (kan tage nogle minutter)
install.packages(c("remotes", "av"))
remotes::install_github("bnosac/audio.whisper")

library(av)
library(audio.whisper)

# Klip 10:30:00-10:34:00 ud og konvertér til det format, Whisper kræver (16 kHz, mono wav)
av_audio_convert("fly_fuckery/london_1030Z.mp3", "fly_fuckery/klip.wav",
                 channels = 1, sample_rate = 16000,
                 start_time = 0, total_time = 240)

# Hent modellen og transskribér (første gang downloades modellen automatisk)
model <- whisper("small.en")
trans <- predict(model, newdata = "fly_fuckery/klip.wav", language = "en")
trans$data[, c("from", "to", "text")]