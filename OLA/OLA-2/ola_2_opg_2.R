library(dkstat)
library(tidyr)
library(ggplot2)

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

# --- Sammenligning: artiklens periode 2000K1-2016K2 ---
d1 <- d[d$Kvartal >= as.Date("2000-01-01") & d$Kvartal <= as.Date("2016-04-01"), ]
# Forklaringsgrad (R²) for DI-FTI og DST's FTI. Artiklen: 0,54 og 0,42
summary(lm(Forbrug_vaekst ~ DI_FTI,  data = d1))$r.squared
summary(lm(Forbrug_vaekst ~ DST_FTI, data = d1))$r.squared
# Korrelation for DI-FTI og DST's FTI. Artiklen: 0,73 og 0,65
cor(d1$Forbrug_vaekst, d1$DI_FTI)
cor(d1$Forbrug_vaekst, d1$DST_FTI)

# --- Sammenligning: hele perioden 2000K1-nu ---
d2 <- d[d$Kvartal >= as.Date("2000-01-01"), ]
summary(lm(Forbrug_vaekst ~ DI_FTI,  data = d2))$r.squared
summary(lm(Forbrug_vaekst ~ DST_FTI, data = d2))$r.squared
cor(d2$Forbrug_vaekst, d2$DI_FTI)
cor(d2$Forbrug_vaekst, d2$DST_FTI)

# --- Sammenligning: efter artiklen frem til nyeste data 2016K3-nu ---
d3 <- d[d$Kvartal >= as.Date("2016-07-01"), ]
summary(lm(Forbrug_vaekst ~ DI_FTI,  data = d3))$r.squared
summary(lm(Forbrug_vaekst ~ DST_FTI, data = d3))$r.squared
cor(d3$Forbrug_vaekst, d3$DI_FTI)
cor(d3$Forbrug_vaekst, d3$DST_FTI)


## Opgave 2.2 – Forudsigelser af forbruget ----
# Estimationsperiode: 1. kvt. 2000 til nyeste forbrugsdata 
d_est <- d[d$Kvartal >= as.Date("2000-01-01"), ]

# Simpel lineær regression af forbrugsvæksten på hver indikator
model_DI  <- lm(Forbrug_vaekst ~ DI_FTI,  data = d_est)
model_DST <- lm(Forbrug_vaekst ~ DST_FTI, data = d_est)

# Estimerede koefficienter (skæring og hældning) for begge modeller
coef(model_DI)
coef(model_DST)

# Indikatorernes værdier for det kvartal, som skal forudsiges
ny <- fti[fti$Kvartal == as.Date("2026-07-01"), ]
ny

# Forudsigelse af den årlige realvækst i kvartalet med hver model
predict(model_DI,  newdata = ny)
predict(model_DST, newdata = ny)

# Samme beregning i hånden: skæring + hældning × indikatorens værdi
coef(model_DI)[1]  + coef(model_DI)[2]  * ny$DI_FTI
coef(model_DST)[1] + coef(model_DST)[2] * ny$DST_FTI

# Forudsigelse med usikkerhedsinterval (95 pct.)
predict(model_DI,  newdata = ny, interval = "prediction")
predict(model_DST, newdata = ny, interval = "prediction")


# --- Data til plottet ---
# Faktisk realvækst i estimationsperioden
faktisk <- data.frame(kvartal = d_est$Kvartal,
                      vaekst  = d_est$Forbrug_vaekst,
                      serie   = "Faktisk")

# Modellernes tilpassede værdier i estimationsperioden (DI-modellen)
di_model <- data.frame(kvartal = d_est$Kvartal,
                       vaekst  = predict(model_DI, newdata = d_est),
                       serie   = "DI_model")

# Modellernes tilpassede værdier i estimationsperioden (DST-modellen)
dst_model <- data.frame(kvartal = d_est$Kvartal,
                        vaekst  = predict(model_DST, newdata = d_est),
                        serie   = "DST_model")

# Samling af de tre serier i én tabel
plot_df <- rbind(faktisk, di_model, dst_model)

# Kun modellernes linjer (den faktiske vækst tegnes som søjler)
modeller <- plot_df[plot_df$serie != "Faktisk", ]

# Prognosepunkterne for kvartalet i ny (3. kvt. 2026): ét punkt pr. model
prognose <- data.frame(kvartal = ny$Kvartal,
                       vaekst  = c(predict(model_DI,  newdata = ny),
                                   predict(model_DST, newdata = ny)),
                       serie   = c("DI_model", "DST_model"))

# Sidste tilpassede punkt for hver model, så den prikkede linje kan starte dér
sidste <- plot_df[plot_df$kvartal == max(d_est$Kvartal) & plot_df$serie != "Faktisk", ]

# Linjestykket fra sidste tilpassede punkt til prognosen
prognose_linje <- rbind(sidste, prognose)

# Tekst til søjlerne i forklaringsboksen
txt_faktisk <- "Faktisk realvækst (kædede værdier)"


# --- Data til plottet ---
# --- Plot for forventede realvækst --
# Forudsigelser med 95 %-prædiktionsinterval fra begge modeller
pi_DI  <- predict(model_DI,  newdata = ny, interval = "prediction")
pi_DST <- predict(model_DST, newdata = ny, interval = "prediction")

# Saml i én tabel og rund af til to decimaler
interval <- data.frame(model = c("Model med DI-FTI", "Model med DST-FTI"),
                       fit = round(c(pi_DI[, "fit"], pi_DST[, "fit"]), 2),
                       lwr = round(c(pi_DI[, "lwr"], pi_DST[, "lwr"]), 2),
                       upr = round(c(pi_DI[, "upr"], pi_DST[, "upr"]), 2))

# Lås rækkefølgen, så DI står øverst
interval$model <- factor(interval$model, levels = rev(interval$model))

# Hjælpefunktion: tal med to decimaler og komma
komma <- function(x) formatC(x, format = "f", digits = 2, decimal.mark = ",")

p_interval <- ggplot(interval, aes(y = model)) +
  # Stiplet linje ved 0 pct.
  geom_vline(xintercept = 0, linetype = "dashed", colour = "#7F2704") +
  # Bjælken for intervallet
  geom_segment(aes(x = lwr, xend = upr, yend = model, colour = model), linewidth = 14, alpha = 0.3, lineend = "round") +
  # Punktet for forudsigelsen
  geom_point(aes(x = fit, colour = model), size = 7) +
  # Tallet for forudsigelsen under punktet, til venstre for nul-linjen
  geom_text(aes(x = fit, label = paste0(komma(fit), " %"), colour = model), vjust = 2.6, hjust = 1, size = 5.5, fontface = "bold") +
  # Grænserne for intervallet i hver ende
  geom_text(aes(x = lwr, label = komma(lwr)), hjust = 1.8, size = 4.5, colour = "#7F2704") +
  geom_text(aes(x = upr, label = paste0("+", komma(upr))), hjust = -0.8, size = 4.5, colour = "#7F2704") +
  # Orange til DI, mørkebrun til DST
  scale_colour_manual(values = c("Model med DI-FTI" = "#F16913", "Model med DST-FTI" = "#7F2704"), guide = "none") +
  # Plads til tallene i begge ender
  scale_x_continuous(limits = c(-7, 6), breaks = seq(-6, 4, 2)) +
  labs(x = "Årlig realvækst i husholdningernes forbrug (pct.)", y = NULL) +
  theme_minimal() +
  # Mørkebrun tekst, ingen vandrette gitterlinjer, gennemsigtig baggrund
  theme(panel.grid.major.y = element_blank(), panel.grid.minor = element_blank(),
        axis.text.y = element_text(size = 14, face = "bold", colour = "#7F2704"),
        axis.text.x = element_text(size = 11, colour = "#7F2704"),
        axis.title.x = element_text(size = 11, colour = "#7F2704"),
        plot.background = element_rect(fill = "transparent", colour = NA),
        panel.background = element_rect(fill = "transparent", colour = NA))


# Faktisk realvækst i estimationsperioden
faktisk <- data.frame(kvartal = d_est$Kvartal,
                      vaekst  = d_est$Forbrug_vaekst,
                      serie   = "Faktisk")

# Modellernes tilpassede værdier i estimationsperioden (DI-modellen)
di_model <- data.frame(kvartal = d_est$Kvartal,
                       vaekst  = predict(model_DI, newdata = d_est),
                       serie   = "DI_model")

# Modellernes tilpassede værdier i estimationsperioden (DST-modellen)
dst_model <- data.frame(kvartal = d_est$Kvartal,
                        vaekst  = predict(model_DST, newdata = d_est),
                        serie   = "DST_model")

# Samling af de tre serier i én tabel
plot_df <- rbind(faktisk, di_model, dst_model)

# Kun modellernes linjer (den faktiske vækst tegnes som søjler)
modeller <- plot_df[plot_df$serie != "Faktisk", ]

# Prognosepunkterne for kvartalet i ny (3. kvt. 2026): ét punkt pr. model
prognose <- data.frame(kvartal = ny$Kvartal,
                       vaekst  = c(predict(model_DI,  newdata = ny),
                                   predict(model_DST, newdata = ny)),
                       serie   = c("DI_model", "DST_model"))

# Sidste tilpassede punkt for hver model, så den prikkede linje kan starte dér
sidste <- plot_df[plot_df$kvartal == max(d_est$Kvartal) & plot_df$serie != "Faktisk", ]

# Linjestykket fra sidste tilpassede punkt til prognosen
prognose_linje <- rbind(sidste, prognose)

# Tekst til søjlerne i forklaringsboksen
txt_faktisk <- "Faktisk realvækst (kædede værdier)"


# --- Plot for udvikling af realvækst ---
ggplot() +
  # Søjler for faktisk realvækst, én pr. kvartal (bredde i dage)
  geom_col(data = faktisk, aes(kvartal, vaekst, fill = txt_faktisk), width = 80) +
  # Stiplet linje ved 0, ovenpå søjlerne
  geom_hline(yintercept = 0, linetype = "dashed", colour = "grey50") +
  # Linjer for de to modeller
  geom_line(data = modeller, aes(kvartal, vaekst, colour = serie), linewidth = 0.8) +
  # Prikket linje fra sidste tilpassede punkt ind i prognosen
  geom_line(data = prognose_linje, aes(kvartal, vaekst, colour = serie),
            linetype = "dotted", linewidth = 0.8) +
  # Punkter for prognosen
  geom_point(data = prognose, aes(kvartal, vaekst, colour = serie), size = 3) +
  # Farve til søjlerne
  scale_fill_manual(values = setNames("grey75", txt_faktisk), name = NULL) +
  # Farver og tekster til modellerne
  scale_colour_manual(values = c(DI_model = "#F16913", DST_model = "#7F2704"),
                      labels = c(DI_model = "Model med DI-FTI", DST_model = "Model med DST-FTI"),
                      name = NULL) +
  # X-aksen: et årstal hvert andet år, fra 2000
  scale_x_date(breaks = seq(as.Date("2000-01-01"), as.Date("2026-01-01"), by = "2 years"),
               date_labels = "%Y", expand = c(0.01, 0)) +
  # Y-aksen: tal for hver 2. pct.-point, tynde hjælpelinjer for hver 1.
  scale_y_continuous(breaks = seq(-8, 10, by = 2),
                     minor_breaks = seq(-8, 10, by = 1)) +
  # Titel, akser og kilde
  labs(title = "Modellerne forudsiger nulvækst i 3. kvartal 2026, men har undervurderet væksten siden 2024",
       subtitle = "Faktisk årlig realvækst i husholdningernes forbrug (søjler) mod modeller med DI-FTI og DST-FTI. Punkterne er prognosen for 3. kvt. 2026",
       x = "År", y = "Pct.",
       caption = "Kilde: Danmarks Statistik (FORV1 og NKN1) og egne beregninger") +
  # Enkelt tema
  theme_minimal() +
  # Fed titel, forklaring i bunden og kilden i venstre side
  theme(plot.title = element_text(face = "bold"),
        legend.position = "bottom",
        plot.caption = element_text(hjust = 0, colour = "grey40", size = 9))

