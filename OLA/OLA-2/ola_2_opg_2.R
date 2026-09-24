library(dkstat)
library(tidyr)

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
