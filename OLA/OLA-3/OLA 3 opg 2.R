# =====================================================================
# 0. OPSÆTNING
# =====================================================================
library(dkstat)
library(pls)
options(scipen = 999)

# ---- Indstillinger ----
tidszone         <- "Europe/Copenhagen"
startdato        <- as.Date("2000-01-01")   # første kvartal der bruges (forbrug og forventninger)
Q_pr_aar <- 4                       # bruges til årlig vækst
antal_top <- 5 # hvor mange af de bedste indikatorer vi gemmer

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
F2  <- KV[[grep("^F2 ",  colnames(KV))]]   # Familiens økonomi i dag vs. for et år siden
F3  <- KV[[grep("^F3 ",  colnames(KV))]]   # Familiens økonomiske  situation om et år, sammenlignet med i dag
F4  <- KV[[grep("^F4 ",  colnames(KV))]]   # Danmarks økonomi i dag vs. for et år siden
F5  <- KV[[grep("^F5 ",  colnames(KV))]]   # Danmarks økonomiske situation om et år, sammenlignet med i dag
F6  <- KV[[grep("^F6 ",  colnames(KV))]]   # Priser i dag, sammenlignet med for et år siden
F7  <- KV[[grep("^F7 ",  colnames(KV))]]   # Priser om et år, sammenlignet med i dag
F8  <- KV[[grep("^F8 ",  colnames(KV))]]   # Arbejdsløsheden om et år, sammenlignet med i dag
F9  <- KV[[grep("^F9 ",  colnames(KV))]]   # Fordelagtigt at købe forbrugsgoder nu
F10 <- KV[[grep("^F10 ", colnames(KV))]]   # Forbrugsgoder de næste 12 måneder
F11 <- KV[[grep("^F11 ",  colnames(KV))]]  # Anser det som fornuftigt at spare op i den nuværende økonomiske situation
F12 <- KV[[grep("^F12 ",  colnames(KV))]]  # Regner med at kunne spare op i de kommende 12 måneder
F13 <- KV[[grep("^F13 ",  colnames(KV))]]  # Familiens økonomiske situation lige nu: kan spare/penge slår til/ bruger mere end man tjener

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
# 3. DST indikator
# =====================================================================

#DST        = "F2+F3+F4+F5+F9")

# ---- Klargør forbrugsdata og match mod indikatorerne ----
Forbrug <- data.frame(realvaekst = Forbrugsdata$Forbrug_vaekst,
                      row.names = format(Forbrugsdata$Kvartal))

# Kvartaler hvor vi har både indikatorer og forbrugsdata
faelles_kv <- intersect(rownames(F_total), rownames(Forbrug))

faelles <- data.frame(Kvartal = as.Date(faelles_kv),
                      realvaekst = Forbrug[faelles_kv, "realvaekst"],
                      F2 = F2[1:nrow(Forbrug)],
                      F3 = F3[1:nrow(Forbrug)],
                      F4 = F4[1:nrow(Forbrug)],
                      F5 = F5[1:nrow(Forbrug)],
                      F9 = F9[1:nrow(Forbrug)],
                      check.names = FALSE, row.names = NULL)


# =====================================================================
# 3. Multipel og PCA regression
# =====================================================================

# ---- udregn den multiple lineære regression ----

# Multipel lineær regression
multi <- lm(realvaekst ~ F2 + F3 + F4 + F5 + F9, data = faelles)
summary(multi)

# Der ryddes op i den multiple regression.
# De indikatorer der er statistisk insignifikante fjernes.
# F3 fjernes først. Derefter var det kun F2 der var insignifikant så den fjernes derefter 
multi_ren <- lm(realvaekst ~  F4 + F5 + F9, data = faelles)
summary(multi_ren)


# Lineær regression for hvert enkelt spørgsmål
lin_F2 <- lm(realvaekst ~ F2, data = faelles)
lin_F3 <- lm(realvaekst ~  F3, data = faelles)
lin_F4 <- lm(realvaekst ~  F4, data = faelles)
lin_F5 <- lm(realvaekst ~  F5, data = faelles)
lin_F9 <- lm(realvaekst ~  F9, data = faelles)
summary(lin_F2)
summary(lin_F3)
summary(lin_F4)
summary(lin_F5)
summary(lin_F9)


# PCA regression
pcr.fit <- pcr(realvaekst ~ F2 + F3 + F4 + F5 + F9, data = faelles,
               scale = TRUE )
summary(pcr.fit)
loadings.pcr.fit <- pcr.fit$loadings
loadings.pcr.fit
w.indicators.1 <- loadings.pcr.fit[1:5]^2
w.indicators.1
sum(w.indicators.1)


