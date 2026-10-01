library(dkstat)
library(tidyr)

## Opgave 2.1 – Opdatering af DI’s forbrugertillidsindikator ----

# --- Klargøring af DST & DI - FTI ---
# Hentning af data for forbrugerforventninger
Forventninger <- dst_get_data("FORV1", query = list(INDIKATOR = "*", Tid = "*"))

# Datokolonne laves (kan muligvis kigge ind i om Tid bare skal reformateres)
Forventninger$Dato <- as.Date(Forventninger$TID, tz = "Europe/Copenhagen")

# Behold kun rækker fra og med 1. januar 2000
Forventninger <- Forventninger[Forventninger$Dato >= as.Date("2000-01-01"), ]

# Placér hver måned i sit kvartal. Kvartalet får datoen på kvartalets første dag
Forventninger$Kvartal <- as.Date(cut(Forventninger$Dato, "quarter"))

# Beregn gennemsnittet af value for hver kombination af kvartal og indikator
KV_lang <- aggregate(value ~ Kvartal + INDIKATOR, data = Forventninger, FUN = mean)

# Gør tabellen "bred": én række pr. kvartal og én kolonne pr. indikator
KV <- reshape(KV_lang, idvar = "Kvartal", timevar = "INDIKATOR", direction = "wide")

# # reshape sætter "value." foran alle kolonnenavne, så det fjernes her
names(KV) <- sub("^value\\.", "", names(KV))

# Sortér rækkerne så kvartalerne står i kronologisk rækkefølge
#KV <- KV[order(KV$Kvartal), ]

# Nulstil rækkenumrene
#rownames(KV) <- NULL

# DI-FTI's fire spørgsmål
F2  <- KV[[grep("^F2 ",  colnames(KV))]]   # Familiens økonomi i dag vs. for et år siden
F4  <- KV[[grep("^F4 ",  colnames(KV))]]   # Danmarks økonomi i dag vs. for et år siden
F9  <- KV[[grep("^F9 ",  colnames(KV))]]   # Fordelagtigt at købe forbrugsgoder nu
F10 <- KV[[grep("^F10 ", colnames(KV))]]   # Forbrugsgoder de næste 12 måneder

# DI-FTI er det simple gennemsnit af de fire spørgsmål
DI_FTI <- (F2 + F4 + F9 + F10) / 4

# DST's forbrugertillidsindikator
DST_FTI <- KV[[grep("^F1 ", colnames(KV))]]

# Samling af begge indikatorer med kvartalsdatoen i én tabel
fti <- data.frame(Kvartal = KV$Kvartal, DST_FTI = DST_FTI, DI_FTI = DI_FTI)

# --- Forbrugsdata ---
# Tjek af variabel navne for forbrugsdata
primeta <- dst_meta("NKN1")
primeta$variables
primeta$values$TRANSAKT
primeta$values$PRISENHED
primeta$values$SÆSON
primeta$values$Tid

# Hentning af forbrugsdata
Fork <- dst_get_data("NKN1", query = list(TRANSAKT = "*", PRISENHED = "*", SÆSON = "*", Tid = "*"))

##### Måske skal P31S14D eller P31S1MD bruges #####
# Behold husholdningernes forbrugsudgifter (P31S14D), kædede værdier i mia. kr. (LKV_M), sæsonkorrigeret (Y)
fk <- Fork[grepl("^P31S14D", Fork$TRANSAKT) &
             grepl("^LKV_M ", Fork$PRISENHED) &
             grepl("^Y ", Fork$SÆSON), ]

# Sortér efter tid
#fk <- fk[order(fk$TID), ]

# ---- Årlig realvækst pr. kvartal ----
# Forbruget som almindelig vektor og antal kvartaler
x <- fk$value
n <- length(x)

# Vækst i pct. for hvert kvartal i forhold til samme kvartal året før (4 kvartaler tidligere)
vaekst <- (x[5:n] / x[1:(n - 4)] - 1) * 100

# Saml kvartalsdato og vækst i en tabel
vaekst <- data.frame(Kvartal = as.Date(fk$TID[5:n], tz = "Europe/Copenhagen"),
                     Forbrug_vaekst = vaekst)

# Sæt vækst og indikatorer sammen via kvartalsdatoen
d <- merge(fti, vaekst, by = "Kvartal")

# --- Sammenligning: artiklens periode 2000K1-2016K2 ---
d1 <- d[d$Kvartal >= as.Date("2000-01-01") & d$Kvartal <= as.Date("2016-04-01"), ]
# Forklaringsgrad (R²) for DI-FTI og DST's FTI. Artiklen: 0,54 og 0,42
summary(lm(Forbrug_vaekst ~ DI_FTI,  data = d1))$r.squared
summary(lm(Forbrug_vaekst ~ DST_FTI, data = d1))$r.squared
# Korrelation for DI-FTI og DST's FTI. Artiklen: 0,73 og 0,65
cor(d1$Forbrug_vaekst, d1$DI_FTI)
cor(d1$Forbrug_vaekst, d1$DST_FTI)

# --- Sammenligning: hele perioden 2000K1-nu ---
d2 <- d[d$Kvartal >= as.Date("2000-01-01"), ]
summary(lm(Forbrug_vaekst ~ DI_FTI,  data = d2))$r.squared
summary(lm(Forbrug_vaekst ~ DST_FTI, data = d2))$r.squared
cor(d2$Forbrug_vaekst, d2$DI_FTI)
cor(d2$Forbrug_vaekst, d2$DST_FTI)

# --- Sammenligning: efter artiklen frem til nyeste data 2016K3-nu ---
d3 <- d[d$Kvartal >= as.Date("2016-07-01"), ]
summary(lm(Forbrug_vaekst ~ DI_FTI,  data = d3))$r.squared
summary(lm(Forbrug_vaekst ~ DST_FTI, data = d3))$r.squared
cor(d3$Forbrug_vaekst, d3$DI_FTI)
cor(d3$Forbrug_vaekst, d3$DST_FTI)


## Opgave 2.2 – Forudsigelser af forbruget ----
# Estimationsperiode: 1. kvt. 2000 til nyeste forbrugsdata 
d_est <- d[d$Kvartal >= as.Date("2000-01-01"), ]

# Simpel lineær regression af forbrugsvæksten på hver indikator
model_DI  <- lm(Forbrug_vaekst ~ DI_FTI,  data = d_est)
model_DST <- lm(Forbrug_vaekst ~ DST_FTI, data = d_est)

# Estimerede koefficienter (skæring og hældning) for begge modeller
coef(model_DI)
coef(model_DST)

# Indikatorernes værdier for det kvartal, som skal forudsiges
ny <- fti[fti$Kvartal == as.Date("2026-07-01"), ]
ny

# Forudsigelse af den årlige realvækst i kvartalet med hver model
predict(model_DI,  newdata = ny)
predict(model_DST, newdata = ny)

# Samme beregning i hånden: skæring + hældning × indikatorens værdi
coef(model_DI)[1]  + coef(model_DI)[2]  * ny$DI_FTI
coef(model_DST)[1] + coef(model_DST)[2] * ny$DST_FTI

# Forudsigelse med usikkerhedsinterval (95 pct.)
predict(model_DI,  newdata = ny, interval = "prediction")
predict(model_DST, newdata = ny, interval = "prediction")
