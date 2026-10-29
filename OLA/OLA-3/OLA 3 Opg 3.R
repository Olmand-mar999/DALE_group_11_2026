# =====================================================================
# 0. OPSÆTNING
# =====================================================================
library(dkstat)
library(caret)
library(ggplot2)
options(scipen = 999)

# ---- Indstillinger ----
tidszone         <- "Europe/Copenhagen"
startdato        <- as.Date("2000-01-01")   # første kvartal der bruges (forbrug og forventninger)
Q_pr_aar <- 4                       # bruges til årlig vækst

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

# Hent metadata
meta_nkn1 <- dst_meta("NKN1")
meta_nkn1$variables
meta_nkn1$values$TRANSAKT
meta_nkn1$values$PRISENHED
meta_nkn1$values$SÆSON
meta_nkn1$values$Tid

# Lav query
query_nkn1 <- list(
  TRANSAKT = "P.31 Husholdningernes forbrugsudgifter",
  PRISENHED = "2020-priser, kædede værdier, (mia. kr.)",
  SÆSON = "Sæsonkorrigeret",
  Tid = "*"
)

# ---- Hentning af forbrugsdata ----
Forbrugsdata <- dst_get_data("NKN1", query = query_nkn1)

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

# Hent metadata
meta_forv <- dst_meta("FORV1")
meta_forv$variables
meta_forv$values$INDIKATOR
meta_forv$values$Tid

# Lav query
query_forv <- list(
  INDIKATOR = "*",
  Tid = "*"
)

# ---- Hentning ----
Forventninger <- dst_get_data("FORV1", query = query_forv)

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

# ---- gem forbrug og tillidsindikator i fælles dataframe og lav binary kolonne ----
# Samle forbrug og forventninger på de kvartaler, de har til fælles
Faelles <- merge(Forbrugsdata, KV, by = "Kvartal")

# Y: 1 hvis forbruget er større end samme kvartal året før, ellers 0 (faktor med faste niveauer)
Faelles$binary <- factor(Faelles$Forbrug_vaekst > 0, levels = c(0, 1))

# Logistisk regression: sandsynlighed for vækst givet forbrugertillidsindikatoren ud fra træningsdata
log_model <- glm(binary ~ `F1 Forbrugertillidsindikatoren`, family = binomial, data = Faelles)

# Koefficienter og p-værdier
summary(log_model)

# Bestem binære værdier for de predictede testdata værdier
Faelles$log_binary <- factor(ifelse(predict(log_model) > 0.5, 1, 0), levels = c(0, 1))

# Forudsigelse for Kvartaler uden kendt forbrug, hvor vi kender indikatoren.
Forudsigelses_data <- KV[!KV$Kvartal %in% Faelles$Kvartal, ]

# Sandsynlighed for vækst i de kvartaler vi vil forudsige
forud <- predict(log_model, newdata = Forudsigelses_data, type = "response")

# Vis kvartal, sandsynlighed og forudsigelse samlet, for de kvartaler vi ikke har realvækst for
data.frame(Kvartal = Forudsigelses_data$Kvartal,
           sandsynlighed = forud,
           forudsagt = ifelse(forud > 0.5, "Vækst", "Fald"))

# =====================================================================
# 4. Confusion matrix
# =====================================================================

# positive = "1" gør vækst til den positive klasse; Accuracy skal sammenlignes med No Information Rate
confusionMatrix(data = Faelles$log_binary, reference = Faelles$binary, positive = "1")

# =====================================================================
# 5. graf
# =====================================================================

# Fordeling af 0 og 1 i procent
procent <- prop.table(table(Faelles$binary)) * 100

# Data til søjlediagrammet
data <- data.frame(
  kategori = c("Fald / ingen vækst", "Vækst"),
  procent = as.numeric(procent)
)

# Søjlediagram
ggplot(data, aes(x = kategori, y = procent, fill = kategori)) +
  geom_col(width = 0.6) +  # Grønne farver
  scale_fill_manual(values = c(
    "Fald / ingen vækst" = "#558B2F",
    "Vækst" = "#C5E1A5"
  )) +
  scale_y_continuous( # Y-akse fra 0 til 100 med intervaller på 10
    limits = c(0, 100),
    breaks = seq(0, 100, 10),
    expand = c(0, 0)
  ) +
  labs( # Titel og aksetekster
    title = "Over tid har der oftere været vækst end fald i husholdningernes forbrugsudgifter",
    subtitle = "Data dækker perioden 1994 til 2026",
    x = NULL,
    y = "Procent",
    caption = "Kilde: Danmarks Statistik, NKN1, P.31 Husholdningernes forbrugsudgifter"
  ) +
  theme_minimal() + # Design
  theme(
    panel.grid.major.y = element_line(color = "#E0E0E0"),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    legend.position = "none",
    plot.caption = element_text(hjust = 0)
  )
