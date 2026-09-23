# Opg. 2.1
#alltable = dkstat::dst_get_tables()

#forv1_meta <- dst_meta("FORV1", lang = "da")
#forv1_meta$variables 
#forv1_meta$values$INDIKATOR

#forv1 <- dst_get_data("FORV1",
#                      query = list(INDIKATOR = "*", Tid = "*"),
#                      lang = "da", meta_data = forv1_meta)

#colnames(forv1) <- c("indikator", "tid", "nettotal")
#forv1$kode <- sub(" .*", "", forv1$indikator)

## Hvilke år har data for alle måneder
#maanedlig_check <- aggregate(!is.na(nettotal) ~ format(tid, "%Y"),
#                             data = forv1[forv1$indikator == "F1 Forbrugertillidsindikatoren", ],
#                             FUN = sum)
#names(maanedlig_check) <- c("aar", "antal_maaneder_med_data")
#maanedlig_check

## Fjern data fra før 1996
#forv1_96 <- subset(forv1, tid >= as.Date("1996-01-01"))



library(dkstat)
library(tidyr)

#Hent Forbrugerforventninger data-----------------------------------------------
formeta <- dst_meta("FORV1")
formeta$variables
formeta$values$INDIKATOR
formeta$values$Tid

my_query <- list(
  INDIKATOR = "*",
  Tid = "*"
)

Forventninger <- dst_get_data("FORV1", query = my_query)

Forventninger <- pivot_wider(Forventninger, names_from = INDIKATOR, 
                             values_from = value)

# Gem data fra 2000 og frem
data_cut <- which(Forventninger[,1]=="2000-01-01 CET")
Forventninger <- Forventninger[data_cut:nrow(Forventninger),]

# Feature engineering, kvartaler og FTI og DI-FTI-------------------------------

# Antal komplette kvartaler. floor rounds down to lowest integer
kvartaler <- floor(nrow(Forventninger) / 3)

# Gruppér hver 3. måned. Giver grupper der slutter med værdien 106.
KV_grupper <- rep(1:kvartaler, each = 3)

# Fjern overskydende måneder der ikke indgår i et fuldt kvartal
KV_forventninger <- Forventninger[1:(kvartaler * 3), ]

# -1 i koden betyder ikke medtag 1. kolonne
KV_forventninger <- aggregate(
  KV_forventninger[, -1],
  by = list(KV_grupper = KV_grupper),
  FUN = function(x) mean(as.numeric(x), na.rm = TRUE)
)

# -1 i koden betyder den ikke medtager perioderne. De indsættes igen.
KV_måneder <- Forventninger$TID[seq(3, nrow(Forventninger), by = 3)]
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

Forbrug <- dst_get_data("NKN1", query = my_query2)
Forbrug <- Forbrug[,4:5]
colnames(Forbrug) <- c("Kvartal","Forbrug")

# Gem data fra 1999 og frem (1999 skal bruges til feature engineering)
data_cut2 <- which(Forbrug[,1]=="1999-01-01 CET")
Forbrug <- Forbrug[data_cut2:nrow(Forbrug),]

# Pakken henter kvartaler som den første måned i kvartalet
# For at det skal matche forventningsdataen skal det laves om til den sidste måned

Forbrug$Kvartal <- seq(
  from = as.POSIXct("1999-03-01", tz = "Europe/Copenhagen"),
  by = "3 months",
  length.out = nrow(Forbrug)
)


# Feature engineering-----------------------------------------------------------

## Grunden til at der skal være 4 NA'er, er at funktionen returner
# 4 værdier mindre end der er i datasættet, så der der nødt til at blive lavet
# 4 tomme rækker så den ikke giver fejl
Forbrug$Forbrug <- c(
  rep(NA, 4),
  diff(log(Forbrug$Forbrug), lag = 4) * 100
)
Forbrug <- Forbrug[5:nrow(Forbrug),]

#Merge FTI og DI-FTI med forbrugsdata------------------------------------------

FTI_col = merge(Forbrug, FTI, by = "Kvartal")
DI_FTI_col = merge(Forbrug, DI_FTI, by = "Kvartal")

# Correlationer og forklaringsgrader--------------------------------------------

# Simple lineær regression med simpelt gennemsnit
DI_FTI_cor <- cor(DI_FTI_col$Forbrug, DI_FTI_col$Gennemsnit)
DI_FTI_model <- lm(Forbrug ~ Gennemsnit, data = DI_FTI_col)
DI_FTI_summary <- summary(DI_FTI_model)
DI_FTI_cor
DI_FTI_summary$r.squared
DI_FTI_summary

FTI_cor <- cor(FTI_col$Forbrug, FTI_col$Gennemsnit)
FTI_model <- lm(Forbrug ~ Gennemsnit, data = FTI_col)
FTI_summary <- summary(FTI_model)
FTI_cor
FTI_summary$r.squared
FTI_summary

# Multipel lineær regression
colnames(DI_FTI_col)
F2_DI = DI_FTI[,3]
F4_DI = DI_FTI[,4]
F9_DI = DI_FTI[,5]
F10_DI = DI_FTI[,6]

#DI_FTI_multi <- lm(Forbrug ~ F2_DI + F4_DI + F9_DI + F9_DI,
#                  data = DI_FTI_col)
#DI_FTI_multi_summary <- summary(DI_FTI_multi)
#DI_FTI_multi_summary

#colnames(FTI_col)
#F2_FTI = FTI[,3]
#F3_FTI = FTI[,4]
#F4_FTI = FTI[,5]
#F5_FTI = FTI[,6]
#F9_FTI = FTI[,7]

#FTI_multi <- lm(Forbrug ~ F2_FTI + F3_FTI + F4_FTI + F5_FTI + F9_FTI,
#                   data = FTI_col)
#FTI_multi_summary <- summary(FTI_multi)
#FTI_multi_summary


#DI_FTI_summary$r.squared
#DI_FTI_multi_summary$r.squared
#FTI_summary$r.squared
#FTI_multi_summary$r.squared

# Predict-----------------------------------------------------------------------
pred <- predict(FTI_model)
pred

Ny_data <- 15
predict(pred, newdata = ny_data)


library(dplyr)

data <- Forbrug %>%
  arrange(Kvartal) %>%   # sørg for at data er sorteret kronologisk efter tid
  mutate(
    gruppe = ifelse(Forbrug >= 0, "OP", "NED")
  )
table(data$gruppe)
min(Forbrug$Forbrug)
max(Forbrug$Forbrug)
