# =====================================================================
# 0. OPSÆTNING
# =====================================================================
library(dkstat)
library(caret)
options(scipen = 999)


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

# Vækst i pct. for hvert kvartal i forhold til samme kvartal året før (4 kvartaler tidligere)
Forbrugsdata_vaekst <- (x[5:n] / x[1:(n - 4)] - 1) * 100

# Saml kvartalsdato og vækst i en tabel
Forbrugsdata <- data.frame(Kvartal = as.Date(Forbrugsdata$TID[5:n], tz = "Europe/Copenhagen"),
                           Forbrug_vaekst = Forbrugsdata_vaekst)

# Behold kun rækker fra og med 1. kvartal 2000
Forbrugsdata <- Forbrugsdata[Forbrugsdata$Kvartal >= as.Date("2000-01-01"), ]


# =====================================================================
# 2. FORBRUGERTILLIDSUNDERSØGELSEN (FORVENTNINGER)
# =====================================================================

# ---- Hentning ----
Forventninger <- dst_get_data("FORV1", query = list(INDIKATOR = "*", Tid = "*"))

# ---- Tid og afgrænsning ----
# Tidskolonne sættes til datoformat
Forventninger$TID <- as.Date(Forventninger$TID, tz = "Europe/Copenhagen")

# Behold kun rækker fra og med 1. januar 2000
Forventninger <- Forventninger[Forventninger$TID >= as.Date("2000-01-01"), ]

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

cor(Forbrugsdata$Forbrug_vaekst, (F13[1:nrow(Forbrugsdata)]))

# ---- Vend fortegn ----
# Spørgsmål hvor "stigning" er dårligt, og som derfor vendes (priser og arbejdsløshed)
F6 <- F6 * (-1)
#F7 <- F7 * (-1)
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
# 4. KENDTE INDIKATORER
# =====================================================================

# ---- Definition af indikatorerne ----

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

# ---- Merge med forbrugsdata ----

# Merge alle de forskellige indikatorer med forbrugsdataen
MCCI_merge <- merge(MCCI, Forbrugsdata, by = "Kvartal")
EU_CCI_merge <- merge(EU_CCI, Forbrugsdata, by = "Kvartal")
Mikro_merge <- merge(Alle_mikro, Forbrugsdata, by = "Kvartal")
DI_merge <- merge(DI, Forbrugsdata, by = "Kvartal")
DST_merge <- merge(DST, Forbrugsdata, by = "Kvartal")


# =====================================================================
# 5. ALLE KOMBINATIONER AF INDIKATORER
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
  colnames(indikator_liste)[i] <- paste(colnames(liste[[i]]), collapse = "+")
  # Indsætter navnene til at være en kombination af alle spørgsmål der indgår i indikatoren
}

# Dobbeltjek om indikator listen stemmer overens med F_total. Skal give det samme
indikator_liste$F2[1]
F_total$F2[1]

# Dobbeltjek om rowmeans regner rigtigt
indikator_liste$`F4+F10+F12`[1]
(F_total$F4[1] + F_total$F10[1] + F_total$F12[1]) / 3



# ---- Klargør forbrugsdata og merge ----

# Brug kvartalsdatoen som rækkenavn, i forbrugsdataen
Forbrug <- data.frame(realvaekst = Forbrugsdata$Forbrug_vaekst)
rownames(Forbrug) <- format(Forbrugsdata$Kvartal)

# Merge indikator listen med forbrugsdataen ud fra rownames
faelles <- merge(Forbrug, indikator_liste, by = "row.names")
colnames(faelles)[1] <- "Kvartal"


# =====================================================================
# 6. DEN BEDSTE KOMBINATION AF INDIKATORER
# =====================================================================

# ---- Korrelation og forklaringsgrad ----

# Beregn korrelationen mellem hver kombination og forbrugsvæksten
# Fjern Kvartal og realvækst som er de kolonner vi ikke vil lave korrelation på
# setdiff gør at den gemmer alle kolonner der ikke hedder Kvartal eller realvaekst
indikator_kolonner <- setdiff(colnames(faelles), c("Kvartal", "realvaekst"))
M <- as.matrix(faelles[, indikator_kolonner])
y <- faelles$realvaekst

# Beregn korrelation og R^2 for alle indikatorer og gem dem i et dataframe
forklaringsgrader <- data.frame(
  korrelation = cor(M, y)[, 1],
  R = (cor(M, y)[, 1])^2)

# ---- Top 5 ----

# ---- Gem de 5 indikatorer med højest r værdi ----

# Vis hvilke rækker der har de højeste forklaringsgrader og gem dem
top_5_indexer <- order(forklaringsgrader$R, decreasing = TRUE)[1:5]
top_5 <- forklaringsgrader[top_5_indexer,]


# =====================================================================
# 7. SAMMENLIGNING AF INDIKATORER
# =====================================================================

# ---- Saml de kendte indikatorer ----

# Lav et dataframe der indeholder alle de indikatorer vi kender samt top_5
# Laves igen så alle indikatorerne har samme antal rækker som realvæksten
alle_indikatorer <- cbind(MCCI_merge$Kvartal,
                          MCCI_merge$Indikator, 
                          EU_CCI_merge$Indikator, 
                          Mikro_merge$Indikator, 
                          DI_merge$Indikator, 
                          DST_merge$Indikator,
                          indikator_liste[1:(nrow(indikator_liste)-1),top_5_indexer])

#Sæt kolonnenavnene til at matche
colnames(alle_indikatorer)[1] <- "Kvartal"
colnames(alle_indikatorer) <- gsub("_merge\\$Indikator","",colnames(alle_indikatorer))

# Lav et dataframe der indeholder korrelation og R^2 for alle vores indikatorer
Kendte_indikatorer <- data.frame(
  korrelation = cor(as.matrix(alle_indikatorer[,-1]), y),
  R = cor(as.matrix(alle_indikatorer[,-1]), y)^2
)

# ---- test om vi kan finde DI i vores array af R^2 ----

which(rownames(forklaringsgrader) == "F2+F4+F9+F10")
forklaringsgrader[290,]

# =====================================================================
# 8. Forudsigelser af de bedste modeller
# =====================================================================

# ---- saml alle de indikatorer vi vil lave forudsigelser på ----

# Gem de bedste 20 modeller
top_20_indexer <- order(forklaringsgrader$R, decreasing = TRUE)[1:20]
top_20 <- indikator_liste[1:(nrow(indikator_liste)-1),top_20_indexer]

# Gem dem i fælles dataframe
Forudsigelsesdata <- cbind(faelles[,1:2], top_20)

# ---- Find det kvartal vi laver forudsigelser på ----

# Indikatorernes værdier for det kvartal, som skal forudsiges
#ny_rows <- nrow(F_total) - nrow(Forbrug) - 1
#ny <- F_total[(nrow(F_total)-ny_rows):nrow(F_total),]
#ny <- F_total[rownames(F_total) == as.Date("2026-07-01"), ]
nye_KV <- setdiff(rownames(F_total), rownames(Forbrug))
ny <- indikator_liste[nye_KV,]




# ---- Loop der beregner alle forudsigelserne for det nyeste kvartal ----

# Liste til at gemme forudsigelser
forudsagte_værdier <- data.frame(
  Indikator = character(),
  Forudsagt_Q3 = numeric()
  )

# Lav loop der regner forudsagte værdier for 4. kvartal for de valgte indikatorer
for (i in 3:ncol(Forudsigelsesdata)){
  # Laver et nyt dataframe, så jeg selv kan styre hvilket navn predict leder efter
  data <- data.frame(
    realvaekst = Forudsigelsesdata$realvaekst, # Gemmer realvæksten
    indikator = Forudsigelsesdata[,i] # Gemmer indikator i
  )
  
  # Gemmer navnet på indikatoren der bruges og så beregner modellen
  name <- colnames(Forudsigelsesdata)[i]
  model <- lm(realvaekst ~ indikator,  data = data)
  
  # Hent indikatorens værdi for det nye kvartal i et nyt dataframe, så jeg selv kan styre navnet
  ny_data <- data.frame(
    indikator = ny[, name]
    )
  
  # Beregn den predictede værdi for det nyeste kvartal  
  predicted <- predict(model,  newdata = ny_data)
  
  # Gem den forudsagte værdi samt indikatornavnet i et nyt dataframe
  data_frame <- data.frame(
    Indikator = name,
    Forudsagt_Q3 = predicted
  )
  
  # Tilføj det nye dataframe til den samlede dataframe
  forudsagte_værdier <- rbind(
    forudsagte_værdier,
    data_frame)
}

# ---- Tilføj DI forudsigelse ----

# DI tilføjes til min liste af forudsagte værdier, da det er den der sammenlignes med
ny <- F_total[rownames(F_total) == as.Date("2026-07-01"), ]
Forudsigelsesdata$DI <- DI_merge$Indikator
model_DI  <- lm(realvaekst ~ DI,  data = Forudsigelsesdata)
ny$DI <- (ny$F2 + ny$F4 + ny$F9 + ny$F10) / 4
predict_DI <- predict(model_DI,  newdata = ny)
DI_data <- data.frame(Indikator = "DI", Forudsagt_Q3 = predict_DI)
forudsagte_værdier <- rbind(forudsagte_værdier, DI_data)


###### Alt under her skal slettes, hvis loopet er ok.

# Simpel lineær regression af forbrugsvæksten på hver indikator
#model_1  <- lm(realvaekst ~ F10,  data = Forudsigelsesdata)
#model_2 <- lm(realvaekst ~ `F4+F9+F10+F12+F13`, data = Forudsigelsesdata)
#model_DI  <- lm(realvaekst ~ DI,  data = Forudsigelsesdata)

# ---- Forudsig 3. kvartal ----

# Indikatorernes værdier for det kvartal, som skal forudsiges
#ny <- F_total[rownames(F_total) == as.Date("2026-07-01"), ]
#ny$DI <- (ny$F2 + ny$F4 + ny$F9 + ny$F10) / 4
#ny$F10 <- ny$F10
#ny$`F4+F9+F10+F12+F13` <- (ny$F4 + ny$F9 + ny$F10+ ny$F12 +ny$F13) / 5

# Forudsigelse af den årlige realvækst i kvartalet med hver model
#predict(model_DI,  newdata = ny)
#predict(model_1, newdata = ny)
#predict(model_2, newdata = ny)


# ---- Forudsig 4. kvartal ----

# Gennemsnitlig realvækst for fjerde kvartal
KV_4 <- seq(4,nrow(Forudsigelsesdata),4)
mean_4 <- mean(Forudsigelsesdata$realvaekst[KV_4])

# Gennemsnitlig realvækst for tredje kvartal
KV_3 <- seq(4,nrow(Forudsigelsesdata),4) - 1
mean_3 <- mean(Forudsigelsesdata$realvaekst[KV_3])

# Hvor meget højere er realvæksten i 4. kvartal end 3.kvartal
procent_stoerre <- (mean_4 - mean_3) / mean_3
procent_stoerre

# Bestem hvad værdierne for 4. kvartal bliver
forudsagte_værdier$Forudsagt_Q4 <- forudsagte_værdier$Forudsagt_Q3 * (1 + procent_stoerre)

# Er lidt i tvivl om fremgangsmåden er rigtig for negative værdier?
# Siden DI forudsigelsen er negativ, men vi vil forvente den stiger for 4. kv.
# Ellers så skal der måske være en if.else om hvordan den skal regne positive og negative?
# Er hele princippet for at regne 4. kvartal korrekt?


# =====================================================================
# 9. STABILITET AF RANGERING OVER TID
# =====================================================================


# ---- Lav loop der beregner rangering og R^2 over de seneste 20 kvartaler ----

# Find hvilken række i datasæt hvor corona startede
Corona_start <- which(faelles$Kvartal == "2020-01-01")

# Find hvor mange kvartaler der har været siden da
antal_KV <- nrow(faelles) - Corona_start
antal_KV

# Loop der beregner R_2 for alle indikatorer og gemmer de bedste
alle_kvartaler <- data.frame()

for (i in 1:antal_KV) {

  data_cut <- nrow(faelles) - i # Fjern ét kvartal mere for hver iteration
  indikatorer <- as.matrix(faelles[1:data_cut, -(1:2)]) # Kvartal fjernes fra indikatorerne
  forbruget <- faelles$realvaekst[1:data_cut] # Kvartal fjernes fra realvæksten
  
  
  # Beregn R^2 for alle indikatorer
  korrelation <- cor(indikatorer, forbruget) # Beregn korrelationen
  R_2 <- (korrelation)^2 # Beregn R_2
  names(R_2) <- colnames(indikatorer) # Sæt navne til at være indikatornavne
  sorteret <- sort(R_2, decreasing = TRUE) # Sortér alle indikatorer fra bedst til dårligst
  
  
  # Gem rang, indikator, R^2 og periode for alle indikatorer
  samlet <- data.frame(
    Rang = 1:length(sorteret),
    Indikator = names(sorteret),
    R_vaerdi = sorteret,
    Fjernede_kvartaler = i
  )
  alle_kvartaler <- rbind(alle_kvartaler, samlet)
}


# ---- Lav data.frame der indeholder gns rangering, R_2 og antal gange i top 10 ----

# Beregn gennemsnitlig rangering og gennemsnitlig R^2
gennemsnitlig_rangering <- aggregate(
  cbind(Rang, R_vaerdi) ~ Indikator,
  data = alle_kvartaler,
  FUN = mean
)


# Beregn hvor mange perioder hver indikator indgår i
antal <- table(alle_kvartaler$Indikator)


# Tæl hvor mange gange hver indikator var i top 10
top10 <- table(
  alle_kvartaler$Indikator[alle_kvartaler$Rang <= 10]
)


# Indsæt antallet af gange alle indikatorerne var i top 10
# Hvis indikatoren aldrig har været i top-10 bliver den sat til NA
gennemsnitlig_rangering$Antal_gange_i_top10 <-
  as.integer(top10[gennemsnitlig_rangering$Indikator])


# Indikatorer, der aldrig var i top 10, får værdien 0
gennemsnitlig_rangering$Antal_gange_i_top10[
  is.na(gennemsnitlig_rangering$Antal_gange_i_top10)] <- 0


# Omdøb kolonner
colnames(gennemsnitlig_rangering) <- c(
  "Indikator",
  "Gennemsnitlig_rang",
  "Gennemsnitlig_R2",
  "Antal gange i top-10"
)


