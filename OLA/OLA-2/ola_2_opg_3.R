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

FTI_cor <- cor(FTI_col$Forbrug, FTI_col$Gennemsnit)
FTI_model <- lm(Forbrug ~ Gennemsnit, data = FTI_col)
FTI_summary <- summary(FTI_model)


# 3.1 --------------------------
# Ligningen for de estimerede værdier er følgende:
# y^ = B^0 + B^1 * X

# DI_FTI
DI_FTI_B_0 <- DI_FTI_summary$coefficients[1,1]
DI_FTI_B_1 <- DI_FTI_summary$coefficients[2,1]

y_pred_DI_FTI <- DI_FTI_B_0 + DI_FTI_B_1*DI_FTI_col$Gennemsnit
y_pred_DI_FTI

# FTI
FTI_B_0 <- FTI_summary$coefficients[1,1]
FTI_B_1 <- FTI_summary$coefficients[2,1]

y_pred_FTI <- FTI_B_0 + FTI_B_1*FTI_col$Gennemsnit
y_pred_FTI

# 3.2 -----------------------------------------------------------------------

# Residuals = y - y^
Res_DI_FTI <- Realvækst$Forbrug - y_pred_DI_FTI
Res_FTI <- Realvækst$Forbrug - y_pred_FTI

plot(fitted(DI_FTI_model), Res_DI_FTI)
abline(0, 0)

plot(Res_DI_FTI)
abline(0, 0)

# 3.3 -----------------------------------------------------------------------

y_pred_DI_FTI_old <- predict(DI_FTI_model)
y_pred_FTI_old <- predict(FTI_model)


# SSR = (y - y^)^2
y <- Realvækst$Forbrug
RSS_DI_FTI <- sum((y - y_pred_DI_FTI_old)^2)
TSS_DI_FTI <- sum((y - mean(y))^2)

RSS_FTI <- sum((y - y_pred_FTI_old)^2)
TSS_FTI <- sum((y - mean(y))^2)

# 3.4 ------------------------------------------------------------------------

# r^2 = 1 - (RSS/TSS)
r_squared_DI_FTI <- 1 - (RSS_DI_FTI / TSS_DI_FTI)
r_squared_FTI <- 1 - (RSS_FTI / TSS_FTI)

r_squared_DI_FTI
DI_FTI_summary$r.squared
r_squared_FTI
FTI_summary$r.squared
