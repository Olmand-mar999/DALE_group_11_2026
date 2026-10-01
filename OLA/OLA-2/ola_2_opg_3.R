library(dkstat)
library(tidyr)

## Opgave 2.1 – Opdatering af DI’s forbrugertillidsindikator ----

# --- Klargøring af DST & DI - FTI ---
# Hentning af data for forbrugerforventninger
Forventninger <- dst_get_data("FORV1", query = list(INDIKATOR = "*", Tid = "*"))

# Datokolonne laves (kan muligvis kigge ind i om Tid bare skal reformateres)
Forventninger$Dato <- as.Date(Forventninger$TID, tz = "Europe/Copenhagen")

# Behold kun rækker fra og med 1. januar 2000
Forventninger <- Forventninger[Forventninger$Dato >= as.Date("2000-01-01"), ]

# Placér hver måned i sit kvartal. Kvartalet får datoen på kvartalets første dag
Forventninger$Kvartal <- as.Date(cut(Forventninger$Dato, "quarter"))

# Beregn gennemsnittet af value for hver kombination af kvartal og indikator
KV_lang <- aggregate(value ~ Kvartal + INDIKATOR, data = Forventninger, FUN = mean)

# Gør tabellen "bred": én række pr. kvartal og én kolonne pr. indikator
KV <- reshape(KV_lang, idvar = "Kvartal", timevar = "INDIKATOR", direction = "wide")

# # reshape sætter "value." foran alle kolonnenavne, så det fjernes her
names(KV) <- sub("^value\\.", "", names(KV))

# Sortér rækkerne så kvartalerne står i kronologisk rækkefølge
#KV <- KV[order(KV$Kvartal), ]

# Nulstil rækkenumrene
#rownames(KV) <- NULL

# DI-FTI's fire spørgsmål
F2  <- KV[[grep("^F2 ",  colnames(KV))]]   # Familiens økonomi i dag vs. for et år siden
F4  <- KV[[grep("^F4 ",  colnames(KV))]]   # Danmarks økonomi i dag vs. for et år siden
F9  <- KV[[grep("^F9 ",  colnames(KV))]]   # Fordelagtigt at købe forbrugsgoder nu
F10 <- KV[[grep("^F10 ", colnames(KV))]]   # Forbrugsgoder de næste 12 måneder

# DI-FTI er det simple gennemsnit af de fire spørgsmål
DI_FTI <- (F2 + F4 + F9 + F10) / 4

# DST's forbrugertillidsindikator
DST_FTI <- KV[[grep("^F1 ", colnames(KV))]]

# Samling af begge indikatorer med kvartalsdatoen i én tabel
fti <- data.frame(Kvartal = KV$Kvartal, DST_FTI = DST_FTI, DI_FTI = DI_FTI)

# --- Forbrugsdata ---
# Tjek af variabel navne for forbrugsdata
primeta <- dst_meta("NKN1")
primeta$variables
primeta$values$TRANSAKT
primeta$values$PRISENHED
primeta$values$SÆSON
primeta$values$Tid

# Hentning af forbrugsdata
Fork <- dst_get_data("NKN1", query = list(TRANSAKT = "*", PRISENHED = "*", SÆSON = "*", Tid = "*"))

##### Måske skal P31S14D eller P31S1MD bruges #####
# Behold husholdningernes forbrugsudgifter (P31S14D), kædede værdier i mia. kr. (LKV_M), sæsonkorrigeret (Y)
fk <- Fork[grepl("^P31S14D", Fork$TRANSAKT) &
             grepl("^LKV_M ", Fork$PRISENHED) &
             grepl("^Y ", Fork$SÆSON), ]

# Sortér efter tid
#fk <- fk[order(fk$TID), ]

# ---- Årlig realvækst pr. kvartal ----
# Forbruget som almindelig vektor og antal kvartaler
x <- fk$value
n <- length(x)

# Vækst i pct. for hvert kvartal i forhold til samme kvartal året før (4 kvartaler tidligere)
vaekst <- (x[5:n] / x[1:(n - 4)] - 1) * 100

# Saml kvartalsdato og vækst i en tabel
vaekst <- data.frame(Kvartal = as.Date(fk$TID[5:n], tz = "Europe/Copenhagen"),
                     Forbrug_vaekst = vaekst)

# Sæt vækst og indikatorer sammen via kvartalsdatoen
d <- merge(fti, vaekst, by = "Kvartal")

# Estimationsperiode: 1. kvt. 2000 til nyeste forbrugsdata 
d_est <- d[d$Kvartal >= as.Date("2000-01-01"), ]

model_DI  <- lm(Forbrug_vaekst ~ DI_FTI,  data = d_est)
model_DST <- lm(Forbrug_vaekst ~ DST_FTI, data = d_est)

Summary_DI <- summary(model_DI)
Summary_DST <- summary(model_DST)

# 3.1 --------------------------
# Ligningen for de estimerede værdier er følgende:
# y^ = B^0 + B^1 * X

# DI
DI_B0 <- Summary_DI$coefficients[1,1]
DI_B1 <- Summary_DI$coefficients[2,1]

y_pred_DI <- DI_B0 + DI_B1 * d_est$DI_FTI

# DST
DST_B0 <- Summary_DST$coefficients[1,1]
DST_B1 <- Summary_DST$coefficients[2,1]

y_pred_DST <- DST_B0 + DST_B1 * d_est$DST_FTI

# Sammenligning med predict funktionen
sum(y_pred_DI - predict(model_DI)) # Skal give liste af 0
sum(y_pred_DST - predict(model_DST)) # Skal give liste af 0

# 3.2 -----------------------------------------------------------------------

# Residuals = y - y^
y <- d_est$Forbrug_vaekst
Res_DI <- y - y_pred_DI
Res_DST <- y - y_pred_DST

tid_resid <- data.frame(
  tid = rep(d_est$Kvartal, 2),
  residual = c(Res_DI, Res_DST),
  model = c(rep("DI's forbrugertillidsindikator", length(Res_DI)), 
            rep("DST's forbrugertillidsindikator", length(Res_DST)))
)

# Én tydelig orange nuance til hver model: lys til DI og mørk til DST
farver_model <- c("DI's forbrugertillidsindikator" = "#FDAE6B", 
                  "DST's forbrugertillidsindikator" = "#A63603")

# Graf: ét punkt for hvert kvartal, med tiden på x-aksen og farve efter model
ggplot(tid_resid, aes(x = tid, y = residual, colour = model)) +
  # Stiplet linje ved 0: her ville punkterne ligge, hvis modellen ramte præcist
  geom_hline(yintercept = 0, linetype = "dashed", colour = "grey50") +
  # Ét punkt for hvert kvartal (farven kommer fra modellen)
  geom_point(size = 2, alpha = 0.9) +
  # De to modeller ved siden af hinanden
  facet_wrap(~ model) +
  # Brug de to orange farver (ingen forklaringsboks, da modelnavnet står over hver graf)
  scale_colour_manual(values = farver_model, guide = "none") +
  # X-aksen starter i 2000: årstal for hvert 4. år og en gitterlinje for hvert år
  scale_x_date(limits = c(as.Date("2000-01-01"), as.Date("2026-12-31")), 
               breaks = seq(as.Date("2000-01-01"), as.Date("2024-01-01"), by = "4 years"), 
               minor_breaks = seq(as.Date("2000-01-01"), as.Date("2026-01-01"), by = "1 year"), 
               date_labels = "%Y", expand = c(0, 0)) +
  # Y-aksen: et tal for hvert 2. procentpoint og en gitterlinje for hvert procentpoint
  scale_y_continuous(limits = c(-8, 8), breaks = seq(-8, 8, 2), minor_breaks = seq(-8, 8, 1)) +
  # Titel, akser og kilde
  labs(title = "DI model har færre udsving fra den faktiske realvækst", 
       subtitle = "Modellerne får generelt større udsving under kriser", 
       x = "År", y = "Residual (procentpoint)", 
       caption = "  De stiplede linjer viser hvad den faktiske realvækst er
  Kilde: Danmarks Statistik (FORV1 og NKN1) og egne beregninger") +
  # Enkelt tema
  theme_minimal() +
  # Fed skrift på modelnavne, kilden til venstre, mørk ramme om hver graf og luft imellem
  theme(strip.text = element_text(face = "bold"), 
        plot.caption = element_text(hjust = 0), 
        panel.border = element_rect(colour = "grey30", fill = NA, linewidth = 0.8), 
        panel.spacing = unit(2, "lines"))

# 3.3 -----------------------------------------------------------------------

y_pred_DI <- predict(model_DI)
y_pred_DST <- predict(model_DST)


# SSR = (y - y^)^2
y <- d_est$Forbrug_vaekst
RSS_DI <- sum((y - y_pred_DI)^2)
TSS_DI <- sum((y - mean(y))^2)

RSS_DST <- sum((y - y_pred_DST)^2)
TSS_DST <- sum((y - mean(y))^2)
TSS_DI

# 3.4 ------------------------------------------------------------------------

# r^2 = 1 - (RSS/TSS)
r_squared_DI <- 1 - (RSS_DI / TSS_DI)
r_squared_DST <- 1 - (RSS_DST / TSS_DST)

r_squared_DI
Summary_DI$r.squared

r_squared_DST
Summary_DST$r.squared
