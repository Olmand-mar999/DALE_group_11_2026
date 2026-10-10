library(dkstat)
library(caret)
library(ggplot2)
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

# Loop gennem alle F-spørgsmål
# 
for (spg in names(F_total)) {
  plot(
    KV$Kvartal,
    F_total[[spg]],
    type = "l",
    xlab = "Kvartal",
    ylab = spg,
    main = paste(spg, "over tid"),
    xaxt = "n"
  )
  # Vandret linje ved y = 0
  abline(h = 0, col = "red", lty = 2)
  # Flere punkter på x-aksen
  n <- length(KV$Kvartal)
  akse_pos <- unique(round(seq(1, n, length.out = min(n, 20))))
  axis(
    1,
    at = KV$Kvartal[akse_pos],
    labels = format(KV$Kvartal[akse_pos], "%Y"),
    las = 2,
    cex.axis = 0.7
  )
}

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


# ---- Lav tests om mine loops regner rigtigt ----

# En række tests der tjekker at værdierne i vores indikator liste stemmer
# For at det er rigtigt skal alle listerne give det samme
indikator_liste$F2[1]
F_total$F2[1]

indikator_liste$F10[1]
F_total$F10[1]
mean(indikator_liste$F10)
mean(F_total$F10)

rownames(Kendte_indikatorer)

indikator_liste$`F4+F9+F10+F12+F13`[1]
(F_total$F4[1] + F_total$F9[1] + F_total$F10[1] + F_total$F12[1] + F_total$F13[1]) / 5
mean(indikator_liste$`F4+F9+F10+F12+F13`)
mean((F_total$F4 + F_total$F9 + F_total$F10 + F_total$F12 + F_total$F13) / 5)

indikator_liste$`F4+F10+F12`[1]
(F_total$F4[1] + F_total$F10[1] + F_total$F12[1]) / 3
mean(indikator_liste$`F4+F10+F12`)
mean((F_total$F4 + F_total$F10 + F_total$F12) / 3)


# ---- Lav plot over hver enkelt models udvikling i forhold til realvæsten ----

library(ggplot2)

# Tilføj realvækst til alle_indikator liste for at kunne lave regressioner
alle_indikatorer$realvaekst <- faelles$realvaekst

# Saml modellerne i en liste
modeller <- list(
  DI = lm(realvaekst ~ DI, data = alle_indikatorer),
  DST = lm(realvaekst ~ DST, data = alle_indikatorer),
  MCCI = lm(realvaekst ~ MCCI, data = alle_indikatorer),
  EU = lm(realvaekst ~ EU_CCI, data = alle_indikatorer),
  Mikro = lm(realvaekst ~ Mikro, data = alle_indikatorer),
  Model_1 = lm(realvaekst ~ alle_indikatorer[,7], data = alle_indikatorer),
  Model_2 = lm(realvaekst ~ alle_indikatorer[,8], data = alle_indikatorer),
  Model_3 = lm(realvaekst ~ alle_indikatorer[,9], data = alle_indikatorer),
  Model_4 = lm(realvaekst ~ alle_indikatorer[,10], data = alle_indikatorer),
  Model_5 = lm(realvaekst ~ alle_indikatorer[,11], data = alle_indikatorer)
)


# Loop gennem modellerne
for (navn in names(modeller)) {
  
  # Beregn forudsagte værdier
  alle_indikatorer$forudsagt <- predict(modeller[[navn]])
  
  # Beregn skaleringsfaktor til søjlerne
  faktor <- max(abs(alle_indikatorer$forudsagt), na.rm = TRUE) /
    max(abs(alle_indikatorer$realvaekst), na.rm = TRUE)
  
  # Lav plot
  p <- ggplot(alle_indikatorer, aes(x = Kvartal)) +
    geom_col(
      aes(y = realvaekst * faktor),
      fill = "grey75",
      alpha = 0.7
    ) +
    geom_line(
      aes(y = forudsagt, group = 1),
      color = "blue",
      linewidth = 1
    ) +
    scale_y_continuous(
      name = "Forudsagte værdier",
      sec.axis = sec_axis(
        ~ . / faktor,
        name = "Faktisk realvækst"
      )
    ) +
    labs(
      title = paste("Model:", navn),
      x = "Kvartal"
    ) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1)
    )
  
  print(p)
}


# ---- Lav plot der kun indeholder DI og de 3 bedste modeller ----

library(tidyr)
library(dplyr)

# Vælg modellerne og beregn forudsigelser
plot_data <- alle_indikatorer %>%
  mutate(
    DI = predict(model_DI),
    Model_1 = predict(model_1),
    Model_2 = predict(model_2),
    Model_3 = predict(model_3)
  ) %>%
  select(Kvartal, realvaekst, DI, Model_1, Model_2, Model_3) %>%
  pivot_longer(
    cols = c(DI, Model_1, Model_2, Model_3),
    names_to = "Model",
    values_to = "Forudsagt"
  )

# Beregn skaleringsfaktor til søjlerne
faktor <- max(abs(plot_data$Forudsagt), na.rm = TRUE) /
  max(abs(plot_data$realvaekst), na.rm = TRUE)

# Plot
ggplot() +
  geom_col(
    data = alle_indikatorer,
    aes(x = Kvartal, y = realvaekst * faktor),
    fill = "grey75",
    alpha = 0.7
  ) +
  geom_line(
    data = plot_data,
    aes(x = Kvartal, y = Forudsagt, color = Model, group = Model),
    linewidth = 1
  ) +
  scale_color_manual(
    values = c(
      "DI" = "blue",
      "Model_1" = "red",
      "Model_2" = "darkgreen",
      "Model_3" = "purple"
    )
  ) +
  scale_y_continuous(
    name = "Modellernes forudsagte værdier",
    sec.axis = sec_axis(
      ~ . / faktor,
      name = "Faktisk realvækst"
    )
  ) +
  labs(
    title = "DI, Model 1, Model 2 og Model 3 sammenlignet",
    x = "Kvartal",
    color = "Model"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "bottom"
  )


# ---- Analyser ----

# F10 er den eneste model der har væksten som positiv på noget tidspunkt efter
# ca 2024. Det er muligvis derfor den betragtes som den bedste model
# Dog er model 2 meget bedre til at ramme finanskrisen, men rammer ikke lige
# så tæt i resten. 
# Model 2 ligner umiddelbart mest hvordan DI ser ud
# F13 og F12 er konstant positive
# F10 er næsten kun negativ
# F9 er altid negativ
# Den næstbedste model indeholder F4, F9, F10, F12, F13.
# F4 er den eneste der er makro. 
# DI indeholder F2, F4, F9 og F10
# så alle de bedste indikatorer indeholder F10, og alle (pånær nr 1 sjovt nok)
# indeholder F4, og alle de 4 andre vi selv regner indeholder F12.
# Der er ingen af de bedste modeller der bruger F6, F7 og F8 som er dem vi vender
#
# Ved Tobias får nogen lidt andre tal end os, og også får DI til fortsat at være den bedste
# Har dog endnu ikke fundet en test der skulle vise at vores regner forkert