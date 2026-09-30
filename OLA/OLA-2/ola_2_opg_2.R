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
KV <- KV[order(KV$Kvartal), ]

# Nulstil rækkenumrene
rownames(KV) <- NULL

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

# Behold husholdningernes forbrugsudgifter (P31S14D), kædede værdier i mia. kr. (LKV_M), sæsonkorrigeret (Y)
fk <- Fork[grepl("^P31S14D", Fork$TRANSAKT) &
             grepl("^LKV_M ", Fork$PRISENHED) &
             grepl("^Y ", Fork$SÆSON), ]

# Sortér efter tid
fk <- fk[order(fk$TID), ]

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




# Faktisk vækst mod modellernes tilpassede værdier for de seneste 8 kvartaler
data.frame(Kvartal = tail(d_est$Kvartal, 8),
           faktisk = tail(d_est$Forbrug_vaekst, 8),
           DI      = tail(fitted(model_DI), 8),
           DST     = tail(fitted(model_DST), 8))

# Gennemsnitlig årlig vækst i hele estimationsperioden (til sammenligning i 2.3)
mean(d_est$Forbrug_vaekst)

# Gennemsnitlig fejl (faktisk minus tilpasset) over de seneste 8 kvartaler
mean(tail(residuals(model_DI), 8))
mean(tail(residuals(model_DST), 8))

# Antal af de seneste 8 residualer, der er positive (modellen for lav)
sum(tail(residuals(model_DI), 8) > 0)
sum(tail(residuals(model_DST), 8) > 0)

library(ggplot2)

# Residualer (faktisk minus modelforudsagt vækst) for begge modeller
res <- data.frame(Kvartal = d_est$Kvartal,
                  DI  = residuals(model_DI),
                  DST = residuals(model_DST))

# Residualer over tid med nullinje: skævhed viser sig som længere perioder over eller under nul
ggplot(res, aes(x = Kvartal)) +
  geom_line(aes(y = DI,  color = "DI-FTI")) +
  geom_line(aes(y = DST, color = "DST's FTI")) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(title = "Residualer: faktisk minus modelforudsagt vækst",
       x = "År", y = "Procentpoint", color = NULL) +
  theme_minimal()




#Hent Forbrugerforventninger data-----------------------------------------------
formeta <- dst_meta("FORV1")
formeta$variables
formeta$values$INDIKATOR
formeta$values$Tid


Forventninger <- dst_get_data("FORV1",
                      query = list(INDIKATOR = "*", Tid = "*"),
                      lang = "da", meta_data = formeta)

colnames(Forventninger) <- c("indikator", "tid", "nettotal")


Forventninger_wider <- pivot_wider(Forventninger, names_from = indikator, 
                             values_from = nettotal)

# Gem data fra 2000 og frem
forv1_00 <- subset(Forventninger_wider, tid >= as.Date("2000-01-01"))

# Feature engineering, kvartaler og FTI og DI-FTI-------------------------------

forv1_00$tid = as.character(forv1_00$tid)
forv1_00$tid = as.Date(forv1_00$tid)

# Antal komplette kvartaler. floor rounds down to lowest integer
kvartaler <- floor(nrow(forv1_00) / 3)

# Gruppér hver 3. måned. Giver grupper der slutter med værdien 106.
KV_grupper <- rep(1:kvartaler, each = 3)

# Fjern overskydende måneder der ikke indgår i et fuldt kvartal
KV_forventninger <- forv1_00[1:(kvartaler * 3), ]

# -1 i koden betyder ikke medtag 1. kolonne
KV_forventninger <- aggregate(
  KV_forventninger[, -1],
  by = list(KV_grupper = KV_grupper),
  FUN = function(x) mean(as.numeric(x), na.rm = TRUE)
)

# -1 i koden betyder den ikke medtager tid. De indsættes igen.
KV_måneder <- forv1_00$tid[seq(3, nrow(forv1_00), by = 3)]
KV_forventninger$Kvartal <- KV_måneder

# Flyt Kvartal til første kolonne og fjern KV_grupper som kolonne
KV_forventninger <- KV_forventninger[, c(
  "Kvartal",
  setdiff(names(KV_forventninger), c("Kvartal", "KV_grupper"))
)]

# Gem FTI og DI-FTI spørgsmål i eget dataframe
colnames(KV_forventninger)

FTI <- KV_forventninger[,c(1,3,4,5,6,7)]
DI_FTI <- KV_forventninger[,c(1,3,5,7,11)]

# Tilføj kolonne med simpelt gennemsnit af spørgsmålene
FTI$Gennemsnit <- rowMeans(FTI[, -1], na.rm = TRUE)
DI_FTI$Gennemsnit <- rowMeans(DI_FTI[, -1], na.rm = TRUE)

#Hent husholdningernes forbrugsudgifter-----------------------------------------
primeta <- dst_meta("NKN1")
primeta$variables
primeta$values$TRANSAKT
primeta$values$PRISENHED
primeta$values$SÆSON
primeta$values$Tid

my_query2 <- list(
  TRANSAKT = "P.31 Husholdningernes forbrugsudgifter",
  PRISENHED = "2020-priser, kædede værdier, (mia. kr.)",
  SÆSON = "Sæsonkorrigeret",
  Tid = "*"
)

nkn1 <- dst_get_data("NKN1", query = my_query2)
nkn1 <- nkn1[,4:5]
colnames(nkn1) <- c("Kvartal","Forbrug")

# Gem data fra 1999 og frem (1999 skal bruges til feature engineering)
Realvækst <- subset(nkn1, Kvartal >= as.Date("1999-01-01"))

# Pakken henter kvartaler som den første måned i kvartalet
# For at det skal matche forventningsdataen skal det laves om til den sidste måned

Realvækst$Kvartal <- seq(
  from = as.Date("1999-03-01"),
  by = "3 months",
  length.out = nrow(Realvækst)
)


# Feature engineering-----------------------------------------------------------

## Grunden til at der skal være 4 NA'er, er at funktionen returner
# 4 værdier mindre end der er i datasættet, så der der nødt til at blive lavet
# 4 tomme rækker så den ikke giver fejl
Realvækst$Forbrug <- c(
  rep(NA, 4),
  diff(log(Realvækst$Forbrug), lag = 4) * 100
)
Realvækst <- Realvækst[5:nrow(Realvækst),]

#Merge FTI og DI-FTI med forbrugsdata------------------------------------------

FTI_col = merge(Realvækst, FTI, by = "Kvartal")
DI_FTI_col = merge(Realvækst, DI_FTI, by = "Kvartal")

# Correlationer og forklaringsgrader--------------------------------------------

# Simple lineær regression med simpelt gennemsnit
DI_FTI_cor <- cor(DI_FTI_col$Forbrug, DI_FTI_col$Gennemsnit)
DI_FTI_model <- lm(Forbrug ~ Gennemsnit, data = DI_FTI_col)
DI_FTI_summary <- summary(DI_FTI_model)
DI_FTI_cor
DI_FTI_summary

FTI_cor <- cor(FTI_col$Forbrug, FTI_col$Gennemsnit)
FTI_model <- lm(Forbrug ~ Gennemsnit, data = FTI_col)
FTI_summary <- summary(FTI_model)
FTI_cor
FTI_summary


# Predict-----------------------------------------------------------------------

new_row <- nrow(FTI_col)*3 + 1
FTI_sidst <- forv1_00[new_row:nrow(forv1_00),c(1,3,4,5,6,7)]
DI_FTI_sidst <- forv1_00[new_row:nrow(forv1_00),c(1,3,5,7,11)]

FTI_sidst$Gennemsnit <- rowMeans(FTI_sidst[, -1], na.rm = TRUE)
DI_FTI_sidst$Gennemsnit <- rowMeans(DI_FTI_sidst[, -1], na.rm = TRUE)

FTI_sidst <- data.frame(Gennemsnit = mean(FTI_sidst$Gennemsnit))
DI_FTI_sidst <- data.frame(Gennemsnit = mean(DI_FTI_sidst$Gennemsnit))

FTI_pred <- predict(FTI_model, newdata = FTI_sidst)
DI_FTI_pred <- predict(DI_FTI_model, newdata = DI_FTI_sidst)
