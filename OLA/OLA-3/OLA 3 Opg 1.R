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

# Behold kun rækker fra og med 1. januar 1998
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
M <- as.matrix(faelles[, -(1:2)])
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


# ---- Undersøg hvilken indikator er bedst over tid ----

# Array til at gemme de indekser der ligger i top-10 over tid
tabel_liste <- c()

# Loop der beregner R_2 for alle indikatorer og gemmer de bedste
for (i in 1:20){ # Vi siger 1:20, som er 5 års data. Det vurderes 5 år er nok.
  
  data_cut <- nrow(faelles) - i                        # Fjerner et kvartal af gangen
  indikatorer <- as.matrix(faelles[1:data_cut,-(1)]) # Gem indikatorerne for den nye tidsperiode
  forbruget <- faelles$realvaekst[1:data_cut]          # Gem realvæksten for den nye tidsperiode
  R_2 = cor(indikatorer, forbruget)^2                  # Beregn R_2 for alle indikatorerne
  max <- order(R_2[,1], decreasing = TRUE)[1:10]       # Gemmer hvilke rækker de 10 højeste R_2 ligger på
  tabel_liste <- c(tabel_liste,(rownames(R_2)[max]))   # Gemmer rownames for de rækkenavne med højest R_2
  
}

sort(table(tabel_liste))


# =====================================================================
# 8. STABILITET AF RANGERING OVER TID
# =====================================================================

# ---- Indstillinger (ret kun her) ----

vindue          <- 40   # Længden af hvert enkelt vindue i kvartaler (40 = 10 år)
periode_tilbage <- 60   # Hvor mange kvartaler vi samlet kigger tilbage fra seneste kvartal (60 = 15 år)
skridt          <- 1    # Hvor mange kvartaler hvert vindue rykkes frem. 1 = kraftigt overlap, 'vindue' = ingen overlap

# ---- Trin 1: Forbered data ----

M <- as.matrix(faelles[, -(1:2)])   # Alle kolonner i 'faelles' undtagen Kvartal og realvaekst, som matrix. Hver kolonne er én indikator-kombination
y <- faelles$realvaekst             # Forbrugsvæksten som vektor
n <- nrow(M)                        # Antal kvartaler i data

# ---- Trin 2: Beregn antal vinduer og startpunkt ----

antal_vinduer <- floor((periode_tilbage - vindue) / skridt) + 1  # Hvor mange vinduer der kan være i perioden, når de rykkes 'skridt' kvartaler ad gangen
foerste_start <- n - vindue + 1 - (antal_vinduer - 1) * skridt   # Rækken hvor første vindue starter. Regnet baglæns så sidste vindue slutter i række n (seneste kvartal)

if (vindue > periode_tilbage) stop("vindue må ikke være større end periode_tilbage")     # Stopper hvis vinduet er længere end hele perioden
if (antal_vinduer < 2) stop("Der skal være mindst 2 vinduer for at måle stabilitet")      # Stopper hvis der kun er ét vindue, da spredning ikke kan beregnes
if (foerste_start < 1) stop("periode_tilbage er længere end data. Max er ", n)           # Stopper hvis vi beder om mere data end der findes

cat("Antal vinduer:", antal_vinduer, "\n")                                                                # Skriver antal vinduer i konsollen
cat("Analyserer fra", format(faelles$Kvartal[foerste_start]), "til", format(faelles$Kvartal[n]), "\n")      # Skriver hvilken periode der samlet analyseres, så du kan kontrollere den

# ---- Trin 3: Forbered tom matrix ----

rang_matrix <- matrix(NA, nrow = ncol(M), ncol = antal_vinduer)  # Tom matrix: én række pr. kombination, én kolonne pr. vindue
rownames(rang_matrix) <- colnames(M)                              # Rækkenavne = kombinationernes navne (fx "F2+F4+F9+F10")
vindue_navne <- rep(NA, antal_vinduer)                            # Tom vektor til vinduernes slutkvartal

# ---- Trin 4: Loop over vinduerne ----

for (v in 1:antal_vinduer) {          # Loop hvor v er nummeret på det aktuelle vindue
  
  start <- foerste_start + (v - 1) * skridt   # Vinduets første række. Vindue 1 starter i foerste_start, vindue 2 'skridt' rækker senere osv.
  slut  <- start + vindue - 1                 # Vinduets sidste række
  idx   <- start:slut                         # Rækkenumrene i vinduet
  
  korr <- cor(M[idx, ], y[idx])[, 1]  # Korrelation mellem hver kombination og forbrugsvæksten, kun for kvartalerne i vinduet. [, 1] gør resultatet til en almindelig vektor
  
  r2 <- korr^2                        # Kvadrerer korrelationen for at få R^2
  r2[korr < 0] <- 0                   # Sætter R^2 til 0 hvor korrelationen er negativ, da en negativ sammenhæng ikke er en brugbar indikator
  
  rang <- rank(-r2, ties.method = "min")  # Rangerer kombinationerne. Minus vender rækkefølgen, så højeste R^2 får rang 1. Ved lighed får alle den laveste fælles rang
  
  rang_matrix[, v] <- rang            # Gemmer rangeringerne i kolonne v
  
  vindue_navne[v] <- format(faelles$Kvartal[slut])  # Gemmer vinduets slutkvartal som tekst
}

colnames(rang_matrix) <- vindue_navne # Sætter kolonnenavnene til vinduernes slutkvartaler

dim(rang_matrix)                      # Skal give (antal kombinationer, antal_vinduer)
rang_matrix[1:5, 1:min(5, antal_vinduer)]  # Viser de første 5 kombinationer i de første op til 5 vinduer til kontrol

# ---- Trin 5: Opsummér hver kombinations rangeringer ----

rang_oversigt <- data.frame(row.names = rownames(rang_matrix))  # Tom dataframe med kombinationernes navne som rækker

rang_oversigt$gns_rang     <- rowMeans(rang_matrix)                       # Gennemsnit af rangeringer pr. kombination. Lavere er bedre
rang_oversigt$bedste_rang  <- do.call(pmin, as.data.frame(rang_matrix))   # Laveste (bedste) rang pr. kombination på tværs af vinduer
rang_oversigt$vaerste_rang <- do.call(pmax, as.data.frame(rang_matrix))   # Højeste (værste) rang pr. kombination på tværs af vinduer
rang_oversigt$andel_top10  <- rowMeans(rang_matrix <= 10)                 # Andel af vinduer hvor kombinationen er i top 10 (TRUE tæller som 1, FALSE som 0)

afvigelser <- rang_matrix - rang_oversigt$gns_rang                        # Trækker hver rækkes gennemsnitsrang fra dens rangeringer
rang_oversigt$sd_rang <- sqrt(rowSums(afvigelser^2) / (antal_vinduer - 1))  # Standardafvigelse af rangeringerne. Lav sd = stabil rangering

# ---- Trin 6: Sortér efter gennemsnitsrang ----

raekkefoelge  <- order(rang_oversigt$gns_rang)   # Rækkefølgen der sorterer gns_rang fra lavest til højest
rang_oversigt <- rang_oversigt[raekkefoelge, ]   # Sorterer oversigten

head(rang_oversigt, 10)                          # De 10 kombinationer med lavest gennemsnitsrang

# ---- Trin 7: Hvor ligger de kendte indikatorer? ----

kendte <- c(MCCI       = "F2+F3+F9+F10",         # Navngiven vektor: indikatorens betegnelse og dens kombinationsnavn. MCCI
            EU_CCI     = "F3+F5+F8+F12",         # EU's indikator
            Alle_mikro = "F2+F3+F9+F10+F12+F13", # Alle mikrospørgsmål
            DI         = "F2+F4+F9+F10",         # DI's indikator
            DST        = "F2+F3+F4+F5+F9")       # DST's indikator

rang_oversigt[kendte, ]                          # Slår de fem op i oversigten og viser deres statistik