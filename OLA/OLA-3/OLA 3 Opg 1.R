# =====================================================================
# 0. OPSÆTNING
# =====================================================================
library(dkstat)
library(ggplot2)
library(httr)
library(jsonlite)
library(readr)
options(scipen = 999)

# ---- Indstillinger ----
tidszone         <- "Europe/Copenhagen"
startdato        <- as.Date("2000-01-01")   # første kvartal der bruges (forbrug og forventninger)
Q_pr_aar <- 4                       # bruges til årlig vækst
antal_top <- 5 # hvor mange af de bedste indikatorer vi gemmer
antal_modeller <- 20    # antal bedste indikatorer der får en forudsigelse i sektion 7

# Spørgsmål der indgår i alle kombinationer
# F2  Familiens økonomi i dag vs. for et år siden
# F3  Familiens økonomiske situation om et år, sammenlignet med i dag
# F4  Danmarks økonomi i dag vs. for et år siden
# F5  Danmarks økonomiske situation om et år, sammenlignet med i dag
# F6  Priser i dag, sammenlignet med for et år siden
# F7  Priser om et år, sammenlignet med i dag
# F8  Arbejdsløsheden om et år, sammenlignet med i dag
# F9  Fordelagtigt at købe forbrugsgoder nu
# F10 Forbrugsgoder de næste 12 måneder
# F11 Fornuftigt at spare op i den nuværende situation
# F12 Regner med at kunne spare op i de kommende 12 måneder
# F13 Familiens økonomiske situation lige nu: kan spare / penge slår til / bruger mere end man tjener


# Kendte indikatorer
kendte <- c(MCCI       = "F2+F3+F9+F10",           # Artiklens mikrospørgsmål
            EU_CCI     = "F3+F5+F8+F12",           # Europas indikator
            Alle_mikro = "F2+F3+F9+F10+F12+F13",   # Alle mikrospørgsmål
            DI         = "F2+F4+F9+F10",
            DST        = "F2+F3+F4+F5+F9")

# =====================================================================
# 1. FORBRUGSDATA
# =====================================================================

# ---- Hentning af forbrugsdata ----
Forbrugsdata <- dst_get_data("NKN1", query = list(TRANSAKT = "*", PRISENHED = "*", SÆSON = "*", Tid = "*"))

##### Måske skal P31S14D eller P31S1MD bruges #####
# Husholdningernes forbrugsudgifter (P31S14D), kædede værdier i mia. kr. (LKV_M), sæsonkorrigeret (Y)
Forbrugsdata <- Forbrugsdata[grepl("^P31S14D", Forbrugsdata$TRANSAKT) &
                               grepl("^LKV_M ", Forbrugsdata$PRISENHED) &
                               grepl("^Y ", Forbrugsdata$SÆSON), ]

# ---- Årlig realvækst pr. kvartal ----
# Forbruget som almindelig vektor og antal kvartaler
x <- Forbrugsdata$value
n <- length(x)

# Vækst i pct. i forhold til samme kvartal året før
Forbrugsdata_vaekst <- (x[(Q_pr_aar + 1):n] / x[1:(n - Q_pr_aar)] - 1) * 100

# Saml kvartalsdato og vækst i en tabel
Forbrugsdata <- data.frame(Kvartal = as.Date(Forbrugsdata$TID[(Q_pr_aar + 1):n], tz = tidszone),
                           Forbrug_vaekst = Forbrugsdata_vaekst)

# Behold kun rækker fra og med startdato
Forbrugsdata <- Forbrugsdata[Forbrugsdata$Kvartal >= startdato, ]


# =====================================================================
# 2. FORBRUGERTILLIDSUNDERSØGELSEN (FORVENTNINGER)
# =====================================================================

# ---- Hentning ----
Forventninger <- dst_get_data("FORV1", query = list(INDIKATOR = "*", Tid = "*"))

# ---- Tid og afgrænsning ----
# Tidskolonne sættes til datoformat
Forventninger$TID <- as.Date(Forventninger$TID, tz = tidszone)

# Behold kun rækker fra og med 1. januar 2000
Forventninger <- Forventninger[Forventninger$TID >= startdato, ]

# ---- Aggregering til kvartaler ----
# Placér hver måned i sit kvartal. Kvartalet får datoen på kvartalets første dag
Forventninger$Kvartal <- as.Date(cut(Forventninger$TID, "quarter"))

# Beregn gennemsnittet af value for hver kombination af kvartal og indikator
KV_lang <- aggregate(value ~ Kvartal + INDIKATOR, data = Forventninger, FUN = mean)

# ---- Fra lang til bred tabel ----
# Gør tabellen "bred": én række pr. kvartal og én kolonne pr. indikator
KV <- reshape(KV_lang, idvar = "Kvartal", timevar = "INDIKATOR", direction = "wide")

# # reshape sætter "value." foran alle kolonnenavne, så det fjernes her
names(KV) <- sub("^value\\.", "", names(KV))


# =====================================================================
# 3. SPØRGSMÅL FRA FORBRUGERTILLIDSUNDERSØGELSEN
# =====================================================================

# ---- Udtræk af de enkelte spørgsmål ----

# Alle spørgsmål
F2  <- KV[[grep("^F2 ",  colnames(KV))]]   
F3  <- KV[[grep("^F3 ",  colnames(KV))]]   
F4  <- KV[[grep("^F4 ",  colnames(KV))]]   
F5  <- KV[[grep("^F5 ",  colnames(KV))]]   
F6  <- KV[[grep("^F6 ",  colnames(KV))]]   
F7  <- KV[[grep("^F7 ",  colnames(KV))]]   
F8  <- KV[[grep("^F8 ",  colnames(KV))]]   
F9  <- KV[[grep("^F9 ",  colnames(KV))]]  
F10 <- KV[[grep("^F10 ", colnames(KV))]]  
F11 <- KV[[grep("^F11 ",  colnames(KV))]]  
F12 <- KV[[grep("^F12 ",  colnames(KV))]]  
F13 <- KV[[grep("^F13 ",  colnames(KV))]]  

# ---- Vend fortegn ----
# Spørgsmål hvor "stigning" er dårligt, og som derfor vendes (priser og arbejdsløshed)
F6 <- F6 * (-1)
F7 <- F7 * (-1) 
F8 <- F8 * (-1)

# ---- Saml spørgsmålene ----
# Saml alle spørgsmål i et dataframe
F_total <- data.frame(
  F2 = F2,
  F3 = F3,
  F4 = F4,
  F5 = F5,
  F6 = F6,
  F7 = F7,
  F8 = F8,
  F9 = F9,
  F10 = F10,
  F12 = F12,
  F13 = F13
)

# Brug kvartalsdatoen som rækkenavn, så vi senere kan matche mod forbrugsdata
rownames(F_total) <- format(KV$Kvartal)


# =====================================================================
# 4. ALLE KOMBINATIONER AF INDIKATORER
# =====================================================================

# ---- Generér alle kombinationer ----

# Lav liste der indeholder alle kombinationer
liste <- list()

# Lav et loop der looper over alle kombinationer
for (i in 1:ncol(F_total)){
  data <- combn(F_total, i, simplify = F) # Gemmer alle kombinationer fra 1 til 11.
  liste <- c(liste, data) # Samler dem i en lang liste
}

# ---- Beregn gennemsnit for hver kombination ----

# Lav en tom dataframe der skal indeholde alle indikatorerne
indikator_liste <- data.frame(matrix(nrow = nrow(F_total), ncol = 0))

# Loop over alle listeelementer i listen, beregn gennemsnittet og gem i ny dataframe
for (i in 1:length(liste)) {
  means <- rowMeans(liste[[i]]) # Beregner rowmeans af alle listeelementer
  indikator_liste <- cbind(indikator_liste, means) # Sætter dem sammen i samme dataframe
  colnames(indikator_liste)[i] <- paste(colnames(liste[[i]]), collapse = "+") # Indsætter navnene til at være en kombination af alle spørgsmål der indgår i indikatoren
}

# Dobbeltjek om indikator listen stemmer overens med F_total. Skal give det samme
indikator_liste$F2[1]
F_total$F2[1]

# Dobbeltjek om rowmeans regner rigtigt
indikator_liste$`F4+F10+F12`[1]
(F_total$F4[1] + F_total$F10[1] + F_total$F12[1]) / 3


# ---- Klargør forbrugsdata og match mod indikatorerne ----
Forbrug <- data.frame(realvaekst = Forbrugsdata$Forbrug_vaekst,
                      row.names = format(Forbrugsdata$Kvartal))

# Kvartaler hvor vi har både indikatorer og forbrugsdata
faelles_kv <- intersect(rownames(F_total), rownames(Forbrug))

faelles <- data.frame(Kvartal = as.Date(faelles_kv),
                      realvaekst = Forbrug[faelles_kv, "realvaekst"],
                      indikator_liste[faelles_kv,],
                      check.names = FALSE, row.names = NULL)

faelles <- data.frame(Kvartal = as.Date(faelles_kv),
                      realvaekst = Forbrug[faelles_kv, "realvaekst"],
                      indikator_liste[faelles_kv, , drop = FALSE],
                      check.names = FALSE, row.names = NULL)


# =====================================================================
# 5. DEN BEDSTE KOMBINATION AF INDIKATORER
# =====================================================================

# Alle kolonner undtagen Kvartal og realvaekst er indikatorer
indikator_kolonner <- setdiff(colnames(faelles), c("Kvartal", "realvaekst"))
M <- as.matrix(faelles[, indikator_kolonner])
y <- faelles$realvaekst

# Korrelation og R^2 for alle indikatorer
korrelation <- cor(M, y)[, 1]
forklaringsgrader <- data.frame(korrelation = korrelation, R = korrelation^2)

# Sortér fra bedst til dårligst og gem de bedste
rangeret <- forklaringsgrader[order(forklaringsgrader$R, decreasing = TRUE), ]
top_indikatorer <- rangeret[1:antal_top, ]


# =====================================================================
# 6. SAMMENLIGNING AF INDIKATORER
# =====================================================================

top_navne  <- rownames(top_indikatorer)
alle_navne <- c(kendte, top_navne)

Kendte_indikatorer <- forklaringsgrader[alle_navne, ]
rownames(Kendte_indikatorer)[seq_along(kendte)] <- names(kendte)


# =====================================================================
# 7. FORUDSIGELSER AF DE BEDSTE MODELLER
# =====================================================================

# Vælg indikatorer: de bedste + DI
navne <- unique(c(rownames(rangeret)[1:antal_modeller], kendte[["DI"]]))

# Kvartaler der mangler forbrugsdata = de næste kvartaler der skal forudsiges
nye_kv <- setdiff(rownames(F_total), faelles_kv)

# Loop: estimér en model pr. indikator og forudsig alle de nye kvartaler
forudsagte_vaerdier <- data.frame()

for (navn in navne) {
  # Eget dataframe, så navnet på variablen i modellen altid er 'indikator'
  data <- data.frame(realvaekst = faelles$realvaekst,
                     indikator  = indikator_liste[faelles_kv, navn])
  model <- lm(realvaekst ~ indikator, data = data)
  
  # Indikatorens værdier for de nye kvartaler
  ny_data <- data.frame(indikator = indikator_liste[nye_kv, navn])
  
  forudsagte_vaerdier <- rbind(
    forudsagte_vaerdier,
    data.frame(Indikator = navn,
               Kvartal = nye_kv,
               R2 = forklaringsgrader[navn, "R"],
               Forudsagt = as.numeric(predict(model, newdata = ny_data)))
  )
}


# =====================================================================
# 8. GRAFER
# =====================================================================

# ---- Indstillinger til graferne ----
# (defineres også her, så sektion 8 virker, selvom den kopieres ind i et andet script)
if (!exists("start_graf1")) start_graf1 <- as.Date("2007-01-01")

# ---- Fælles farver, tema og hjælpefunktioner ----
groen_moerk <- "#1B5E20"   # vigtigste serie
groen_mid   <- "#558B2F"   # aksetekst til søjler
groen_lys   <- "#8BC34A"   # søjler
groen_bleg  <- "#C5E1A5"   # sekundære søjler
graa        <- "#8C8C8C"   # sammenligningsserie (DI)

tema_opgave <- theme_minimal(base_size = 12) +
  theme(plot.title          = element_text(face = "bold", size = 14),
        plot.subtitle       = element_text(colour = "grey30", margin = margin(b = 10)),
        plot.title.position = "plot",
        plot.caption        = element_text(hjust = 0, colour = "grey40", size = 9,
                                           margin = margin(t = 10)),
        plot.caption.position = "plot",        # kilder venstrestillet helt ude i kanten
        panel.grid.minor    = element_blank(),
        panel.grid.major.x  = element_blank(),
        axis.title          = element_text(colour = "grey30"),
        plot.margin         = margin(12, 16, 12, 12),
        plot.background = element_rect(fill = "transparent", color = NA))

# Tal med dansk decimalkomma
fmt <- function(x, d = 2) format(round(x, d), nsmall = d, decimal.mark = ",")


# ---------------------------------------------------------------------
# 8.1 Graf 1: Inflation (søjler) vs. F7 med vendt fortegn (linje)
# ---------------------------------------------------------------------

# Inflation hentes direkte fra Statistikbankens API (PRIS01)
hent_meta <- function(tabel) {
  res <- GET(paste0("https://api.statbank.dk/v1/tableinfo/", tabel, "?lang=da&format=JSON"))
  stop_for_status(res)
  fromJSON(content(res, "text", encoding = "UTF-8"))
}

find_vaerdi <- function(meta, var_id, moenster) {
  v <- meta$variables$values[[which(meta$variables$id == var_id)]]
  hit <- v$id[grepl(moenster, v$text, ignore.case = TRUE) | v$id == moenster]
  if (length(hit) == 0) stop("Fandt ingen værdi i ", var_id, " der matcher '", moenster, "'")
  hit[1]
}

hent_statbank <- function(tabel, valg) {
  variabler <- lapply(names(valg), function(nm) list(code = nm, values = list(valg[[nm]])))
  variabler <- c(variabler, list(list(code = "Tid", values = list("*"))))
  res <- POST("https://api.statbank.dk/v1/data",
              body = list(table = tabel, format = "BULK", lang = "da",
                          valuePresentation = "Code", variables = variabler),
              encode = "json")
  stop_for_status(res)
  d <- read_delim(I(content(res, "text", encoding = "UTF-8")), delim = ";",
                  col_types = cols(.default = "c"))
  names(d) <- toupper(names(d))
  d <- data.frame(dato   = as.Date(paste0(sub("M", "-", d$TID), "-01")),
                  vaerdi = as.numeric(gsub(",", ".", d$INDHOLD)))
  d[!is.na(d$vaerdi), ]
}

meta_pris <- hent_meta("PRIS01")
var_ids   <- meta_pris$variables$id
varegr_id <- var_ids[grepl("VAREGR", var_ids, ignore.case = TRUE)][1]
enhed_id  <- var_ids[grepl("ENHED",  var_ids, ignore.case = TRUE)][1]

inflation <- hent_statbank("PRIS01", setNames(
  list(find_vaerdi(meta_pris, varegr_id, "i alt"),
       find_vaerdi(meta_pris, enhed_id,  "samme måned")),
  c(varegr_id, enhed_id)))
names(inflation)[2] <- "inflation"

# F7 på månedsniveau tages fra de forventningsdata, vi allerede har hentet
f7_rk    <- Forventninger[grepl("^F7 ", Forventninger$INDIKATOR), ]
f7_tekst <- sub("^F7 ", "", f7_rk$INDIKATOR[1])          # spørgsmålets tekst fra DST
f7_m     <- data.frame(dato = as.Date(format(f7_rk$TID, "%Y-%m-01")),
                       f7   = f7_rk$value * (-1))          # vend fortegn, som i sektion 3

g1 <- merge(inflation, f7_m, by = "dato")
g1 <- g1[g1$dato >= start_graf1, ]

# Skalér F7 over på inflationsaksen
inf_rng <- range(g1$inflation)
f7_rng  <- range(g1$f7)
k1 <- diff(inf_rng) / diff(f7_rng)
til_venstre <- function(v) inf_rng[1] + (v - f7_rng[1]) * k1
til_hoejre  <- function(v) f7_rng[1] + (v - inf_rng[1]) / k1
g1$f7_skaleret <- til_venstre(g1$f7)

korr1   <- cor(g1$inflation, g1$f7)
seneste <- g1[which.max(g1$dato), ]

graf1 <- ggplot(g1, aes(x = dato)) +
  geom_col(aes(y = inflation), fill = groen_lys, colour = groen_lys,
           linewidth = 0.2, width = 31) +
  geom_line(aes(y = f7_skaleret), colour = groen_moerk, linewidth = 1.1) +
  geom_hline(yintercept = 0, colour = "grey40", linewidth = 0.3) +
  scale_y_continuous(name = "Inflation, pct. (søjler)",
                     sec.axis = sec_axis(~ til_hoejre(.), name = "F7, nettotal × (−1) (linje)")) +
  scale_x_date(date_breaks = "2 years", date_labels = "%Y",
               expand = expansion(mult = 0.01)) +
  labs(title = "Danskernes prisforventninger bevæger sig ikke i takt med inflationen",
       subtitle = paste0("Korrelation mellem inflation og F7 (fortegn vendt): ", fmt(korr1),
                         ". Seneste måling (", format(seneste$dato, "%B %Y"), "): inflation ",
                         fmt(seneste$inflation, 1), " pct., F7 ", fmt(seneste$f7, 1), "."),
       x = NULL,
       caption = paste0(
         "Anm.: F7 er ganget med −1, så en stigning i forventede priser trækker ned.\n",
         "Inflation: PRIS01, forbrugerprisindeks i alt, ændring i forhold til samme måned året før.\n",
         "F7: FORV1, '", f7_tekst, "' (nettotal).\n",
         "Kilde: Danmarks Statistik, statistikbanken.dk (PRIS01 og FORV1)")) +
  tema_opgave +
  theme(axis.title.y.left  = element_text(colour = groen_mid),
        axis.text.y.left   = element_text(colour = groen_mid),
        axis.title.y.right = element_text(colour = groen_moerk),
        axis.text.y.right  = element_text(colour = groen_moerk))

print(graf1)


# ---------------------------------------------------------------------
# 8.2 Graf 2: Forklaringsgrad (R²) for kendte indikatorer vs. top 5
# ---------------------------------------------------------------------

# Vi bruger den bedste kombination med mindst to spørgsmål (navnet indeholder "+"),
# så valget ikke afhænger af, at et enkelt spørgsmål tilfældigt ligger øverst.
bedste <- rownames(rangeret)[grepl("\\+", rownames(rangeret))][1]
di     <- kendte[["DI"]]

g2 <- data.frame(navn   = rownames(Kendte_indikatorer),
                 R2     = Kendte_indikatorer$R,
                 kendt  = c(rep(TRUE, length(kendte)), rep(FALSE, antal_top)))
g2$navn <- gsub("_", " ", g2$navn)                        # "Alle_mikro" -> "Alle mikro"
g2$navn <- make.unique(g2$navn)
g2$navn <- factor(g2$navn, levels = g2$navn[order(g2$R2)])  # sortér efter R²
g2$farve <- ifelse(g2$kendt, groen_bleg, groen_moerk)

graf2 <- ggplot(g2, aes(x = R2, y = navn)) +
  geom_col(fill = g2$farve, width = 0.7) +
  geom_text(aes(label = fmt(R2)), hjust = -0.15, size = 3.6, colour = "grey20") +
  scale_x_continuous(name = "Forklaringsgrad (R²)",
                     labels = function(v) fmt(v, 1),
                     expand = expansion(mult = c(0, 0.12))) +
  labs(title = paste0("Den bedste kombination af flere spørgsmål (", bedste,
                      ") forklarer mere af privatforbruget end DI's indikator"),
       subtitle = paste0("R² på ", fmt(rangeret[bedste, "R"]), " mod ",
                         fmt(forklaringsgrader[di, "R"]), " for DI. ",
                         "Mørkegrøn: top 5 af alle kombinationer. Lysegrøn: kendte indikatorer."),
       y = NULL,
       caption = paste0(
         "Anm.: R² er beregnet ud fra korrelationen mellem indikatoren og den årlige realvækst i privatforbruget, kvartalsdata fra ",
         format(min(faelles$Kvartal), "%Y"), ".\n",
         "Kilde: Danmarks Statistik, statistikbanken.dk (NKN1 og FORV1) samt egne beregninger")) +
  tema_opgave +
  theme(panel.grid.major.y = element_blank(),
        panel.grid.major.x = element_line(colour = "grey90"))

print(graf2)

# ---------------------------------------------------------------------
# 8.3 Graf 3: Bedste indikator og DI vs. realvækst i privatforbruget
# ---------------------------------------------------------------------

vi_navn <- "VI"   # navnet på vores indikator i graf 3 (Vores Indikator)

g3 <- data.frame(Kvartal    = as.Date(rownames(F_total)),
                 bedste     = indikator_liste[[bedste]],
                 di         = indikator_liste[[di]],
                 realvaekst = Forbrug[rownames(F_total), "realvaekst"])   # NA for nye kvartaler

# Skalér søjlerne, så 0 ligger samme sted på begge akser
k3 <- max(abs(c(g3$bedste, g3$di)), na.rm = TRUE) / max(abs(g3$realvaekst), na.rm = TRUE)
g3$forbrug_skaleret <- g3$realvaekst * k3

# Direkte etiketter i enden af linjerne i stedet for en forklaringsboks
sidste <- g3[nrow(g3), ]
etiket_x <- sidste$Kvartal + 120

graf3 <- ggplot(g3, aes(x = Kvartal)) +
  geom_col(aes(y = forbrug_skaleret), fill = groen_bleg, width = 80, na.rm = TRUE) +
  geom_hline(yintercept = 0, colour = "grey40", linewidth = 0.3) +
  geom_line(aes(y = di),     colour = graa,        linewidth = 1) +
  geom_line(aes(y = bedste), colour = groen_moerk, linewidth = 1.2) +
  annotate("text", x = etiket_x, y = sidste$bedste, label = vi_navn,
           hjust = 0, colour = groen_moerk, fontface = "bold", size = 3.8) +
  annotate("text", x = etiket_x, y = sidste$di, label = "DI",
           hjust = 0, colour = graa, fontface = "bold", size = 3.8) +
  scale_y_continuous(name = "Indikator, nettotal (linjer)",
                     sec.axis = sec_axis(~ . / k3,
                                         name = "Årlig realvækst i privatforbruget, pct. (søjler)")) +
  scale_x_date(date_breaks = "2 years", date_labels = "%Y",
               expand = expansion(mult = c(0.01, 0.07))) +
  labs(title = paste0("Vores indikator (", vi_navn, ") følger privatforbruget tættere end DI's forbrugertillidsindikator"),
       subtitle = paste0("Mørkegrøn linje: ", vi_navn, ", ", bedste, " (R² = ", fmt(rangeret[bedste, "R"]), "). ",
                         "Grå linje: DI, ", di, " (R² = ", fmt(forklaringsgrader[di, "R"]), "). ",
                         "Søjler: årlig realvækst i privatforbruget."),
       x = NULL,
       caption = paste0(
         "Anm.: Indikatorerne er beregnet som simple gennemsnit af nettotallene for de valgte spørgsmål.\n",
         "Kilde: Danmarks Statistik, statistikbanken.dk (NKN1 og FORV1) samt egne beregninger")) +
  tema_opgave +
  theme(axis.title.y.right = element_text(colour = groen_mid),
        axis.text.y.right  = element_text(colour = groen_mid),
        panel.background = element_rect(fill = NA, colour = NA),
        plot.background  = element_rect(fill = NA, colour = NA),
        legend.background = element_rect(fill = NA, colour = NA),
        legend.box.background = element_rect(fill = NA, colour = NA)
        )

print(graf3)
#ggsave(
#  "graf3.png",
#  plot = graf3,
#  bg = "transparent",
#  width = 12,
#  height = 7,
#  dpi = 300
#)
# ---------------------------------------------------------------------
# 8.4 Graf 4: Oplevet prisudvikling (F6) vs. faktisk inflation
# ---------------------------------------------------------------------
# F6 vises UDEN vendt fortegn her: grafen handler om, hvordan folk oplever
# prisudviklingen, så højere F6 = folk oplever større prisstigninger.
# Sæt vend_f6 <- TRUE, hvis den skal vises med vendt fortegn som F7 i graf 1.
vend_f6  <- FALSE
skillevej <- as.Date("2021-01-01")   # før/efter inflation blev et stort medietema

f6_rk    <- Forventninger[grepl("^F6 ", Forventninger$INDIKATOR), ]
f6_tekst <- sub("^F6 ", "", f6_rk$INDIKATOR[1])
f6_m     <- data.frame(dato = as.Date(format(f6_rk$TID, "%Y-%m-01")),
                       f6   = f6_rk$value * ifelse(vend_f6, -1, 1))

g4 <- merge(inflation, f6_m, by = "dato")
g4 <- g4[g4$dato >= start_graf1, ]

inf4_rng <- range(g4$inflation)
f6_rng   <- range(g4$f6)
k4 <- diff(inf4_rng) / diff(f6_rng)
f6_til_venstre <- function(v) inf4_rng[1] + (v - f6_rng[1]) * k4
f6_til_hoejre  <- function(v) f6_rng[1] + (v - inf4_rng[1]) / k4
g4$f6_skaleret <- f6_til_venstre(g4$f6)

# Nøgletal: gennemsnit før skillevejen vs. seneste måling
foer      <- g4[g4$dato < skillevej, ]
seneste4  <- g4[which.max(g4$dato), ]
korr4     <- cor(g4$inflation, g4$f6)
f6_hoejere <- if (vend_f6) seneste4$f6 < mean(foer$f6) else seneste4$f6 > mean(foer$f6)

titel4 <- if (f6_hoejere) {
  "Danskerne oplever stadig større prisstigninger end før 2021"
} else {
  "Danskernes oplevelse af prisstigninger er tilbage på niveauet før 2021"
}

graf4 <- ggplot(g4, aes(x = dato)) +
  geom_col(aes(y = inflation), fill = groen_lys, colour = groen_lys,
           linewidth = 0.2, width = 31) +
  geom_line(aes(y = f6_skaleret), colour = groen_moerk, linewidth = 1.1) +
  geom_hline(yintercept = 0, colour = "grey40", linewidth = 0.3) +
  geom_vline(xintercept = skillevej, colour = "grey50", linetype = "dashed", linewidth = 0.4) +
  scale_y_continuous(name = "Inflation, pct. (søjler)",
                     sec.axis = sec_axis(~ f6_til_hoejre(.),
                                         name = paste0("F6, nettotal", ifelse(vend_f6, " × (−1)", ""),
                                                       " (linje)"))) +
  scale_x_date(date_breaks = "2 years", date_labels = "%Y",
               expand = expansion(mult = 0.01)) +
  labs(title = titel4,
       subtitle = paste0(
         "F6 i gennemsnit ", format(min(foer$dato), "%Y"), "–2020: ", fmt(mean(foer$f6), 1),
         ". Seneste måling (", format(seneste4$dato, "%B %Y"), "): ", fmt(seneste4$f6, 1),
         ". Inflation i gennemsnit ", format(min(foer$dato), "%Y"), "–2020: ",
         fmt(mean(foer$inflation), 1), " pct., seneste: ", fmt(seneste4$inflation, 1), " pct.\n",
         "Korrelation mellem inflation og F6: ", fmt(korr4), "."),
       x = NULL,
       caption = paste0(
         "Anm.: Stiplet linje markerer januar 2021.",
         ifelse(vend_f6, " F6 er ganget med −1.", " F6 er vist uden vendt fortegn."), "\n",
         "Inflation: PRIS01, forbrugerprisindeks i alt, ændring i forhold til samme måned året før.\n",
         "F6: FORV1, '", f6_tekst, "' (nettotal).\n",
         "Kilde: Danmarks Statistik, statistikbanken.dk (PRIS01 og FORV1) samt egne beregninger")) +
  tema_opgave +
  theme(axis.title.y.left  = element_text(colour = groen_mid),
        axis.text.y.left   = element_text(colour = groen_mid),
        axis.title.y.right = element_text(colour = groen_moerk),
        axis.text.y.right  = element_text(colour = groen_moerk))

print(graf4)


# ---------------------------------------------------------------------
# 8.5 Graf 5: Forudsigelse af næste kvartals realvækst i privatforbruget
# ---------------------------------------------------------------------

kv_navn <- function(d) {
  d <- as.Date(d)
  paste0(format(d, "%Y"), "K", (as.numeric(format(d, "%m")) - 1) %/% 3 + 1)
}

if (length(nye_kv) == 0) {
  message("Ingen kvartaler at forudsige: forbrugsdata dækker alle kvartaler med forventningsdata.")
} else {
  naeste_kv <- nye_kv[1]                                    # første kvartal uden forbrugsdata
  g5 <- forudsagte_vaerdier[forudsagte_vaerdier$Kvartal == naeste_kv, ]
  
  f_bedste <- g5$Forudsagt[g5$Indikator == bedste]
  f_di     <- g5$Forudsagt[g5$Indikator == di]
  f_snit   <- mean(g5$Forudsagt[g5$Indikator %in% rownames(rangeret)[1:antal_modeller]])
  seneste_faktisk <- tail(faelles$realvaekst, 1)
  seneste_kv      <- kv_navn(tail(faelles$Kvartal, 1))
  
  # Etiketter og farver: bedste mørkegrøn, DI grå, øvrige lysegrønne
  g5$etiket <- ifelse(g5$Indikator == di, paste0("DI (", di, ")"), g5$Indikator)
  g5$etiket <- make.unique(g5$etiket)
  g5$etiket <- factor(g5$etiket, levels = g5$etiket[order(g5$R2)])  # højeste R² øverst
  g5$farve  <- ifelse(g5$Indikator == bedste, groen_moerk,
                      ifelse(g5$Indikator == di, graa, groen_lys))
  g5$str    <- ifelse(g5$Indikator %in% c(bedste, di), 4, 3)
  
  graf5 <- ggplot(g5, aes(y = etiket)) +
    geom_vline(xintercept = 0, colour = "grey40", linewidth = 0.3) +
    geom_vline(xintercept = seneste_faktisk, colour = "grey50",
               linetype = "dashed", linewidth = 0.4) +
    annotate("text", x = seneste_faktisk, y = Inf, vjust = 1.3, hjust = -0.05,
             label = paste0("Faktisk ", seneste_kv, ": ", fmt(seneste_faktisk, 1), " pct."),
             colour = "grey40", size = 3.3) +
    geom_segment(aes(x = 0, xend = Forudsagt, yend = etiket),
                 colour = g5$farve, linewidth = 0.8) +
    geom_point(aes(x = Forudsagt), colour = g5$farve, size = g5$str) +
    geom_text(aes(x = Forudsagt, label = fmt(Forudsagt, 1)),
              hjust = ifelse(g5$Forudsagt >= 0, -0.5, 1.5), size = 3.2, colour = "grey20") +
    scale_x_continuous(name = "Forudsagt årlig realvækst i privatforbruget, pct.",
                       labels = function(v) fmt(v, 1),
                       expand = expansion(mult = 0.15)) +
    labs(title = paste0("Vores bedste model venter en realvækst i privatforbruget på ",
                        fmt(f_bedste, 1), " pct. i ", kv_navn(naeste_kv)),
         subtitle = paste0("Bedste model (", bedste, "): ", fmt(f_bedste, 1), " pct. ",
                           "DI: ", fmt(f_di, 1), " pct. ",
                           "Gennemsnit af de ", antal_modeller, " bedste modeller: ",
                           fmt(f_snit, 1), " pct.\n",
                           "Modellerne er sorteret efter forklaringsgrad (R²), højeste øverst."),
         y = NULL,
         caption = paste0(
           "Anm.: Forudsigelserne er lavet med simple lineære regressioner af den årlige realvækst ",
           "på hver indikator. Farver: mørkegrøn = bedste model, grå = DI.\n",
           "Kilde: Danmarks Statistik, statistikbanken.dk (NKN1 og FORV1) samt egne beregninger")) +
    tema_opgave +
    theme(panel.grid.major.y = element_blank(),
          panel.grid.major.x = element_line(colour = "grey90"),
          axis.text.y = element_text(size = 9))
  
  print(graf5)
}
