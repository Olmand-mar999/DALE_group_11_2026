# =====================================================================
# 0. OPSÆTNING
# =====================================================================
library(dkstat)
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
