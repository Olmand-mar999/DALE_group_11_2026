# =====================================================================
# 0. OPSÆTNING
# =====================================================================
library(dkstat)
library(ggplot2)
library(dplyr)
options(scipen = 999)

# ---- Indstillinger ----
tidszone         <- "Europe/Copenhagen"
startdato        <- as.Date("2000-01-01")   # første kvartal der bruges (forbrug og forventninger)
Q_pr_aar <- 4                       # bruges til årlig vækst
antal_top <- 5 # hvor mange af de bedste indikatorer vi gemmer
top_graense <- 10                        # "top N" i stabilitetstesten
test_start  <- as.Date("2020-01-01")     # stabilitetstesten køres tilbage til dette kvartal

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
# 6. STABILITET AF RANGERING OVER TID
# =====================================================================

# ---- Find hvor mange kvartaler der skal fjernes bagfra ----
# Find rækken for startkvartalet og tjek at den findes
start_raekke <- which(faelles$Kvartal == test_start)
stopifnot(length(start_raekke) == 1)

# Antal kvartaler mellem det seneste kvartal og startkvartalet
antal_KV <- nrow(faelles) - start_raekke

# ---- Loop: beregn R^2 og rang for alle indikatorer, når ét kvartal fjernes ad gangen ----
alle_kvartaler <- data.frame()

for (i in 1:antal_KV) {
  data_cut <- nrow(faelles) - i   # fjern ét kvartal mere for hver iteration
  
  # R^2 for alle indikatorer, kun på de første data_cut kvartaler (M og y fra sektion 5)
  R_2 <- cor(M[1:data_cut, ], y[1:data_cut])[, 1]^2
  sorteret <- sort(R_2, decreasing = TRUE)   # bedst først
  
  # Gem rang, indikator, R^2 og periode for alle indikatorer
  samlet <- data.frame(
    Rang = 1:length(sorteret),
    Indikator = names(sorteret),
    R_vaerdi = sorteret,
    Fjernede_kvartaler = i
  )
  alle_kvartaler <- rbind(alle_kvartaler, samlet)
}

# ---- Gennemsnitlig rang, R^2 og antal gange i top N ----
gennemsnitlig_rangering <- aggregate(cbind(Rang, R_vaerdi) ~ Indikator,
                                     data = alle_kvartaler, FUN = mean)

# 1 hvis indikatoren var i top N i den periode, ellers 0. Summen giver antal gange (0 hvis aldrig)
alle_kvartaler$I_top <- as.integer(alle_kvartaler$Rang <= top_graense)
top_antal <- aggregate(I_top ~ Indikator, data = alle_kvartaler, FUN = sum)
gennemsnitlig_rangering <- merge(gennemsnitlig_rangering, top_antal, by = "Indikator")

colnames(gennemsnitlig_rangering) <- c("Indikator", "Gennemsnitlig_rang", "Gennemsnitlig_R2",
                                       paste("Antal gange i top", top_graense))

# Hvor stabile er de kendte indikatorer (MCCI, DI osv.) til sammenligning?
gennemsnitlig_rangering[gennemsnitlig_rangering$Indikator %in% kendte, ]


# =====================================================================
# 7. graf over udviklingen af realvækst over tid
# =====================================================================

ggplot(plot_data, aes(x = Kvartal, y = realvaekst)) +
  geom_col(
    fill = "#C5E1A5",
    width = 80
  ) +
  scale_x_date(
    date_breaks = "2 years",
    date_labels = "%Y"
  ) +
  scale_y_continuous(
    breaks = seq(-10, 15, by = 1)
  ) +
  labs(
    title = "Realvæksten bliver meget ustabil og svingende startende fra 2020",
    subtitle = "Årlig realvækst fordelt på kvartaler",
    x = "Kvartal",
    y = "Realvækst (%)",
    caption = "Kilde: Danmarks Statistik, NKN1"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid.minor = element_blank(),
    plot.caption = element_text(hjust = 0)
  )


# =====================================================================
# 8. graf med realvækst og bedste indikator
# =====================================================================

# Find indikatoren med den bedste gennemsnitlige rangering
bedste_indikator <- gennemsnitlig_rangering %>%
  slice_min(Gennemsnitlig_rang, n = 1, with_ties = FALSE) %>%
  pull(Indikator)

# Hent data for faktisk realvækst og den bedste indikator
plot_data <- faelles %>%
  select(Kvartal, realvaekst, all_of(bedste_indikator)) %>%
  rename(Indikator_vaerdi = all_of(bedste_indikator))

# Beregn skaleringsfaktor, så indikatoren kan vises sammen med realvæksten
faktor <- max(abs(plot_data$realvaekst), na.rm = TRUE) /
  max(abs(plot_data$Indikator_vaerdi), na.rm = TRUE)

# Tegn plottet
ggplot(plot_data, aes(x = Kvartal)) +
  geom_col(
    aes(y = realvaekst),
    fill = "grey90",
    width = 80
  ) +
  geom_line(
    aes(y = Indikator_vaerdi * faktor),
    color = "#558B2F",
    linewidth = 1
  ) +
  scale_x_date(
    date_breaks = "2 years",
    date_labels = "%Y"
  ) +
  scale_y_continuous(
    name = "Faktisk realvækst (%)",
    breaks = seq(
      floor(min(plot_data$realvaekst, na.rm = TRUE)),
      ceiling(max(plot_data$realvaekst, na.rm = TRUE)),
      by = 1
    ),
    sec.axis = sec_axis(
      ~ . / faktor,
      name = "Nettotal"
    )
  ) +
  labs(
    title = "Vores bedste indikator følger realvæksten bedst over hele perioden fra 2020-2026",
    subtitle = paste("Indikator med laveste gennemsnitlige rang:", bedste_indikator),
    x = "Kvartal",
    caption = "Kilde: Danmarks Statistik, NKN1 og FORV1"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid.minor = element_blank(),
    plot.caption = element_text(hjust = 0)
  )
