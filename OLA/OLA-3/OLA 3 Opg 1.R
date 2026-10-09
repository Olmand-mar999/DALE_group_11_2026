library(dkstat)
library(caret)
options(scipen = 999)

# ---- Hentning af forbrugsdata ----

Forbrugsdata <- dst_get_data("NKN1", query = list(TRANSAKT = "*", PRISENHED = "*", SÆSON = "*", Tid = "*"))

##### Måske skal P31S14D eller P31S1MD bruges #####
# Behold husholdningernes forbrugsudgifter (P31S14D), kædede værdier i mia. kr. (LKV_M), sæsonkorrigeret (Y)
Forbrugsdata <- Forbrugsdata[grepl("^P31S14D", Forbrugsdata$TRANSAKT) &
                               grepl("^LKV_M ", Forbrugsdata$PRISENHED) &
                               grepl("^Y ", Forbrugsdata$SÆSON), ]

# ---- Årlig realvækst pr. kvartal ----
# Forbruget som almindelig vektor og antal kvartaler
x <- Forbrugsdata$value
n <- length(x)

# Vækst i pct. for hvert kvartal i forhold til samme kvartal året før (4 kvartaler tidligere)
Forbrugsdata_vaekst <- (x[5:n] / x[1:(n - 4)] - 1) * 100

# Saml kvartalsdato og vækst i en tabel
Forbrugsdata <- data.frame(Kvartal = as.Date(Forbrugsdata$TID[5:n], tz = "Europe/Copenhagen"),
                           Forbrug_vaekst = Forbrugsdata_vaekst)

# Behold kun rækker fra og med 1. kvartal 2000
Forbrugsdata <- Forbrugsdata[Forbrugsdata$Kvartal >= as.Date("2000-01-01"), ]


# ---- Hent forbrugertillidsundersøgelsen ----

Forventninger <- dst_get_data("FORV1", query = list(INDIKATOR = "*", Tid = "*"))

# Tidskolonne sættes til datoformat
Forventninger$TID <- as.Date(Forventninger$TID, tz = "Europe/Copenhagen")

# Behold kun rækker fra og med 1. januar 1998
Forventninger <- Forventninger[Forventninger$TID >= as.Date("2000-01-01"), ]

# Placér hver måned i sit kvartal. Kvartalet får datoen på kvartalets første dag
Forventninger$Kvartal <- as.Date(cut(Forventninger$TID, "quarter"))

# Beregn gennemsnittet af value for hver kombination af kvartal og indikator
KV_lang <- aggregate(value ~ Kvartal + INDIKATOR, data = Forventninger, FUN = mean)

# Gør tabellen "bred": én række pr. kvartal og én kolonne pr. indikator
KV <- reshape(KV_lang, idvar = "Kvartal", timevar = "INDIKATOR", direction = "wide")

# # reshape sætter "value." foran alle kolonnenavne, så det fjernes her
names(KV) <- sub("^value\\.", "", names(KV))


# ---- Saml alle relevante spørgsmål i ny dataframe ----

# Er artiklens forbrugertillidsindikator med mikro bedre end vores?
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

# Spørgsmål hvor "stigning" er dårligt, og som derfor vendes (priser og arbejdsløshed)
F6 <- F6 * (-1)
F7 <- F7 * (-1)
F8 <- F8 * (-1)

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


# ---- Alle kendte indikatorer ----

# Artiklens mikrospørgsmål
MCCI <- data.frame(Kvartal = KV$Kvartal, 
                   Indikator = (F2 + F3 + F9 + F10) / 4)

# Europas indikator spørgsmål
EU_CCI <- data.frame(Kvartal = KV$Kvartal, 
                     Indikator = (F3 + F5 + F8 + F12) / 4)

# Alle mikrospørgsmål
Alle_mikro <- data.frame(Kvartal = KV$Kvartal,
                         Indikator = (F2 + F3 + F9 + F10 + F12 + F13) / 6)

DI <- data.frame(Kvartal = KV$Kvartal,
                 Indikator = (F2 + F4 + F9 + F10) / 4)

DST <- data.frame(Kvartal = KV$Kvartal,
                  Indikator = (F2 + F3 + F4 + F5 + F9) / 5)

#bench <- c(EU_CCI = "F3+F5+F8+F12", MCCI = "F2+F3+F9+F10", DI = "F2+F4+F9+F10", DST = "F2+F3+F4+F5+F9")

# Merge alle de forskellige indikatorer med forbrugsdataen
MCCI_merge <- merge(MCCI, Forbrugsdata, by = "Kvartal")
EU_CCI_merge <- merge(EU_CCI, Forbrugsdata, by = "Kvartal")
Mikro_merge <- merge(Alle_mikro, Forbrugsdata, by = "Kvartal")
DI_merge <- merge(DI, Forbrugsdata, by = "Kvartal")
DST_merge <- merge(DST, Forbrugsdata, by = "Kvartal")


# ---- Find alle kombinationer af indikatorer ----

# Lav liste der indeholder alle kombinationer
liste <- list()

# Lav et loop der looper over alle kombinationer
for (i in 1:ncol(F_total)){
  data <- combn(F_total, i, simplify = F) # Gemmer alle kombinationer fra 1 til 11.
  liste <- c(liste, data) # Samler dem i en lang liste
}

# Lav en tom dataframe der skal indeholde alle indikatorerne
indikator_liste <- data.frame(matrix(nrow = nrow(F_total), ncol = 0))

# Loop over alle listeelementer i listen, beregn gennemsnittet og gem i ny dataframe
for (i in 1:length(liste)) {
  means <- rowMeans(liste[[i]]) # Beregner rowmeans af alle listeelementer
  indikator_liste <- cbind(indikator_liste, means) # Sætter dem sammen i samme dataframe
  colnames(indikator_liste)[i] <- paste(colnames(liste[[i]]), collapse = "+")
  # Indsætter navnene til at være en kombination af alle spørgsmål der indgår i indikatoren
}

# Brug kvartalsdatoen som rækkenavn, i forbrugsdataen
Forbrug <- data.frame(realvaekst = Forbrugsdata$Forbrug_vaekst)
rownames(Forbrug) <- format(Forbrugsdata$Kvartal)

# Merge indikator listen med forbrugsdataen ud fra rownames
faelles <- merge(Forbrug, indikator_liste, by = "row.names")
colnames(faelles)[1] <- "Kvartal"

# ---- Find den bedste kombination af indikatorer ----

# Beregn korrelationen mellem hver kombination og forbrugsvæksten
M <- as.matrix(faelles[, -(1:2)])
y <- faelles$realvaekst

# Beregn korrelation og R^2 for alle indikatorer og gem dem i et dataframe
forklaringsgrader <- data.frame(
  korrelation = cor(M, y)[, 1],
  R = (cor(M, y)[, 1])^2)


# ---- Gem de 5 indikatorer med højest r værdi ----

# Vis hvilke rækker der har de højeste forklaringsgrader og gem dem
top_5_indexer <- order(forklaringsgrader$R, decreasing = TRUE)[1:5]
top_5 <- forklaringsgrader[top_5_indexer,]


# ---- Sammenlign indikatorer ----

# Lav et dataframe der indeholder alle de indikatorer vi kender
alle_indikatorer <- cbind(MCCI_merge$Indikator, 
                          EU_CCI_merge$Indikator, 
                          Mikro_merge$Indikator, 
                          DI_merge$Indikator, 
                          DST_merge$Indikator)

# Sæt kolonnenavnene til at matche
colnames(alle_indikatorer) <- c("MCCI", "EU_CCI", "Mikro", "DI", "DST")

# Lav et dataframe der indeholder korrelation og R^2 for alle vores kendte indikatorer
Kendte_indikatorer <- data.frame(
  korrelation = cor(as.matrix(alle_indikatorer), y),
  R = cor(as.matrix(alle_indikatorer), y)^2
)

# Sammensæt
sammenligning <- rbind(top_5, Kendte_indikatorer)

