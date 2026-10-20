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
# 3. DST logistisk regression
# =====================================================================

# Lav et faelles dataframe der indeholder alle de rækker forbruget og KV har tilfælles
Faelles <- merge(Forbrugsdata, KV, by = "Kvartal")

# Der laves en binær kolonne der viser hvor ofte
# den kvartalvise årlige realvækst er større end julehandlen 2025
# Væksten laves om til binære værdier, 0 for lavere og 1 for højere
julehandel_2025 <- Faelles$Forbrug_vaekst[Faelles$Kvartal == "2025-10-01"]
Faelles$binary <- ifelse(Forbrugsdata$Forbrug_vaekst > julehandel_2025, 1, 0)

# Lav en tabel der indeholder fordelingen
table_data <- table(Faelles$binary)

# Lav plot der viser fordelingen
bp <- barplot(
  table_data,
  ylim = c(0, 100),
  ylab = "Procent",
  xlab = "",
  main = "Realvæksten er typisk lavere end julehandlen 2025 ",
  names.arg = c("Lavere", "Højere"),
  las = 1,
  col = c("#558B2F", "#C5E1A5")
)
abline(0,0)

mtext(
  "Kilde: Danmarks Statistik, NKN1, P.31 Husholdningernes forbrugsudgifter",
  side = 1,
  line = 4,
  adj = 0)


# Lav logistisk regression og gem de forudsagte værdier som 0 eller 1
log_model  <- glm(binary ~ `F1 Forbrugertillidsindikatoren`, family = "binomial", data = Faelles)
Faelles$log_binary <- ifelse(
  predict(log_model, type = "response") > 0.5, 1, 0)


# Find de kvartaler der skal forudsiges for
Forudsigelses_data <- KV[!KV$Kvartal %in% Faelles$Kvartal, ]
Forudsigelses_data$`F1 Forbrugertillidsindikatoren`
#Forudsigelses_data <- rbind(Forudsigelses_data, 1:14)

# Bestem sandsynligheden for om den forudsagte værdi er større eller lavere end julehandlen 2025
Forud_vaekst <- predict(log_model, newdata = Forudsigelses_data, type = "response")
Forud_vaekst
ifelse(Forud_vaekst > 0.5, "Højere", "Lavere")

# =====================================================================
# 4. confusion matrix
# =====================================================================

# Confusion matrix tager kun faktorer som argument
Faelles$log_binary <- as.factor(Faelles$log_binary)
Faelles$binary <- as.factor(Faelles$binary)

# Der laves en confusion matrice for at se hvor god modellen er til at ramme rigtigt
conf_matrix <- confusionMatrix(Faelles$log_binary, Faelles$binary)
conf_matrix



