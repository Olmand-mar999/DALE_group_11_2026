# Opg. 4.1: Illustration af forbrugertillid ----

# Pakker
library(dkstat)
library(tidyr)
library(ggplot2)

# Hent data fra DST
Forventninger <- dst_get_data("FORV1", query = list(INDIKATOR = "*", Tid = "*"))

# Datokolonne laves (kan muligvis kigge ind i om Tid bare skal reformateres)
Forventninger$Dato <- as.Date(Forventninger$TID, tz = "Europe/Copenhagen")

# Giver 1996 mening?
maanedlig_check <- aggregate(           # Beregn en værdi pr. gruppe (her: pr. år)
  !is.na(value) ~ format(TID, "%Y"),    # Tæl rækker hvor value IKKE er NA, grupperet efter årstal
  data = Forventninger[Forventninger$INDIKATOR == "F1 Forbrugertillidsindikatoren", ],  # Brug kun rækker for F1
  FUN = sum                             # Læg TRUE-værdierne sammen = antal måneder med data
)
names(maanedlig_check) <- c("aar", "antal_maaneder_med_data")  # Giv kolonnerne læsbare navne
maanedlig_check                         # Vis resultatet: et fuldt år har 12 måneder med data

# Behold kun rækker fra og med 1. januar 1996
Forventninger <- Forventninger[Forventninger$Dato >= as.Date("1996-01-01"), ]

# Placér hver måned i sit kvartal. Kvartalet får datoen på kvartalets første dag
Forventninger$Kvartal <- as.Date(cut(Forventninger$Dato, "quarter"))

# Beregn gennemsnittet af value for hver kombination af kvartal og indikator
KV_lang <- aggregate(value ~ Kvartal + INDIKATOR, data = Forventninger, FUN = mean)

# Gør tabellen "bred": én række pr. kvartal og én kolonne pr. indikator
KV <- reshape(KV_lang, idvar = "Kvartal", timevar = "INDIKATOR", direction = "wide")

# # reshape sætter "value." foran alle kolonnenavne, så det fjernes her
names(KV) <- sub("^value\\.", "", names(KV))

# Sortér rækkerne så kvartalerne står i kronologisk rækkefølge
KV <- KV[order(KV$Kvartal), ]

# Nulstil rækkenumrene
rownames(KV) <- NULL

# PLot
ggplot(KV, aes(x = Kvartal, y = `F1 Forbrugertillidsindikatoren`)) +
  geom_line(color = "#F39C12", linewidth = 0.55) +
  geom_hline(yintercept = 0, color = "black", linetype = "dashed", linewidth = 0.4) +
  scale_x_date(date_breaks = "2 years", date_labels = "%Y", expand = c(0, 0)) +
  labs(title = "DST's forbrugertillidsindikator", x = "År", y = "Forbrugertillidsindikator") +
  theme_minimal() +
  theme(plot.title = element_text(size = 16, hjust = 0.5))


# Opg 4.2: Gennemsnit af underspørgsmål----

# Se på kolonnenavne og find frem til hvilken et spørgsmål, som underspørgsmålet hænger sammen med.
colnames(KV)
# F9 passer bedst, da der er snak om forbrugsgoder i øjeblikket

# Behold data fra 1. kvt. 2000 og frem
KV_4_2 <- KV[KV$Kvartal >= as.Date("2000-01-01"), ]

# Træk F9 ud som vektor via kolonnens kode
F9 <- KV_4_2[[grep("^F9 ", colnames(KV_4_2))]]

# Gennemsnit af F9
mean(F9, na.rm = TRUE)

# Udregning af range og spredning
range(F9, na.rm = TRUE)
sd(F9, na.rm = TRUE)


# Antal kvartaler med positivt nettotal
sum(F9 > 0, na.rm = TRUE)

# Hvilket kvartal var forbrugerne mest positive (højeste F9)?
KV_4_2$Kvartal[which.max(F9)]

# Hvilket kvartal var de mest negative (laveste F9)?
KV_4_2$Kvartal[which.min(F9)]


# Plot F9 over tid med nullinjen som reference
ggplot(KV_4_2, aes(x = Kvartal, y = F9)) +
  geom_line(color = "#F39C12", linewidth = 0.55) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(title = "F9: Fordelagtigt at anskaffe større forbrugsgoder", x = "År", y = "Nettotal") +
  theme_minimal()

# Opg. 4.3: De 11 grupper af forbrug ----

# Se tabellens variable og værdier
formeta <- dst_meta("NAHC21")
formeta$variables
formeta$values$FORMAAAL
formeta$values$PRISENHED
formeta$values$Tid

# Hent alle formål, begge prisenheder og alle år
my_query <- list(FORMAAAL = "*", PRISENHED = "*", Tid = "*")
Forbrug <- dst_get_data("NAHC21", query = my_query)

# Tjek kolonnenavne og teksten i prisenhed
names(Forbrug)
unique(Forbrug$PRISENHED)

# Hvad brugte danskerne flest penge på i 2022 (løbende priser)
f22 <- Forbrug[format(Forbrug$TID, "%Y") == "2022" &
                 Forbrug$PRISENHED == "V Løbende priser" &
                 !grepl("I alt", Forbrug$FORMAAAL), ]


# Kategori med højeste forbrug i 2022
f22[which.max(f22$value), ]


# Plot til hvad danskerne brugte flest penge på i 2022
# # Hjælpefunktion: fjern koden foran gruppenavnet og ombryd lange navne
ryd <- function(x) sapply(sub("^CP[A-Z] ", "", x),
                          function(s) paste(strwrap(s, 28), collapse = "\n"),
                          USE.NAMES = FALSE)

## Læsbart gruppenavn til akserne
f22$gruppe <- ryd(f22$FORMAAAL)

## Søjlediagram sorteret efter størrelse, vandret så de lange navne kan være på y-aksen
ggplot(f22, aes(x = reorder(gruppe, value), y = value / 1000)) +
  geom_col(fill = "#F39C12") +
  coord_flip() +
  labs(title = "Husholdningernes forbrug i 2022", x = NULL, y = "Mia. kr. (løbende priser)") +
  theme_minimal()


# Behold kædede værdier (2020-priser) uden "I alt"
lan <- Forbrug[Forbrug$PRISENHED == "LAN 2020-priser, kædede værdier" &
                 !grepl("I alt", Forbrug$FORMAAAL), ]
lan$aar <- format(lan$TID, "%Y")


# Værdier for 2020 og 2023 side om side pr. gruppe
v20 <- lan[lan$aar == "2020", c("FORMAAAL", "value")]
v23 <- lan[lan$aar == "2023", c("FORMAAAL", "value")]
vaekst <- merge(v20, v23, by = "FORMAAAL", suffixes = c("_2020", "_2023"))

# Procentvis vækst fra 2020 til 2023, størst først
vaekst$pct <- (vaekst$value_2023 / vaekst$value_2020 - 1) * 100
vaekst[order(-vaekst$pct), ]

# Plot til hvilken gruppe steg mest fra 2020 - 2023
## Læsbart gruppenavn til akserne
vaekst$gruppe <- ryd(vaekst$FORMAAAL)

## Søjler for procentvis vækst, farvet efter om væksten er positiv eller negativ
ggplot(vaekst, aes(x = reorder(gruppe, pct), y = pct, fill = pct > 0)) +
  geom_col(show.legend = FALSE) +
  scale_fill_manual(values = c("TRUE" = "#F39C12", "FALSE" = "grey60")) +
  coord_flip() +
  labs(title = "Realvækst i forbrug 2020-2023", x = NULL, y = "Vækst i pct. (2020-priser, kædede værdier)") +
  theme_minimal()


# Opg. 4.4 ----

# ---- Indikatorer (fra KV i 4.1) ----
# De fire spørgsmål, som DI-FTI består af
F2  <- KV[[grep("^F2 ",  colnames(KV))]]
F4  <- KV[[grep("^F4 ",  colnames(KV))]]
F9  <- KV[[grep("^F9 ",  colnames(KV))]]
F10 <- KV[[grep("^F10 ", colnames(KV))]]

# Tabel med kvartalsdato, DST's FTI (F1) og DI-FTI (gennemsnit af de fire spørgsmål)
fti <- data.frame(Kvartal = KV$Kvartal,
                  DST_FTI = KV[[grep("^F1 ", colnames(KV))]],
                  DI_FTI  = (F2 + F4 + F9 + F10) / 4)

# ---- Forbrugsdata: 11 grupper pr. kvartal ----
# Hent alle grupper, prisenheder, sæsonvarianter og kvartaler
formeta <- dst_meta("NKHC21")
formeta$variables
formeta$values$FORMAAAL
formeta$values$PRISENHED
formeta$values$SÆSON
formeta$values$Tid

my_query <- list(
  FORMAAAL = "*",
  PRISENHED = "2020-priser, kædede værdier",
  SÆSON = "Sæsonkorrigeret",
  Tid = "*"
)

gr <- dst_get_data("NKHC21", query = my_query)

gr$TID <- as.Date(gr$TID, tz = "Europe/Copenhagen")

# Én kolonne pr. gruppe
bred <- reshape(gr[, c("TID", "FORMAAAL", "value")],
                idvar = "TID", timevar = "FORMAAAL", direction = "wide")

which(is.na(bred))

# Fjern "value." og behold kun gruppens kode (CPA, CPB, ...) som kolonnenavn
names(bred) <- sub("^value\\.", "", names(bred))
#names(bred)[-1] <- sub(" .*", "", names(bred)[-1])

# Sortér kronologisk
#bred <- bred[order(bred$Kvartal), ]

# ---- Årlig vækst for hver gruppe (løkke over grupperne) ----

# Rækken med CPT i alt fjernes
bred <- bred[-2]

# Navnene på de 11 grupper
y_navne <- names(bred)[-1]
n <- nrow(bred)

# Saml vækst og indikatorer, og behold fra 1. kvt. 2000
names(bred)[1] = "Kvartal"

# Tabel hvor væksten samles, første 4 kvartaler har ingen vækst
vaekst_gr <- data.frame(Kvartal = bred$Kvartal[5:n])

# Gentag for hver gruppe: vækst i pct. mod samme kvartal året før
for (navn in y_navne) {
  x <- bred[[navn]]
  vaekst_gr[[navn]] <- (x[5:n] / x[1:(n - 4)] - 1) * 100
}


d44 <- merge(vaekst_gr, fti, by = "Kvartal")
d44 <- d44[d44$Kvartal >= as.Date("2000-01-01"), ]



# ---- 22 regressioner i en liste ----
# Tom liste til de 22 summaries
regressioner <- list()

# Ydre løkke: de 11 grupper. Indre løkke: de to indikatorer
for (navn in y_navne) {
  for (indikator in c("DST_FTI", "DI_FTI")) {
    modelnavn <- paste(navn, "~", indikator)
    regressioner[[modelnavn]] <- summary(lm(reformulate(indikator, response = navn), data = d44))
  }
}

# Tjek: skal give 22
length(regressioner)

# ---- Overblik over alle 22 ----
tabel <- data.frame(model     = names(regressioner),
           R2        = sapply(regressioner, function(s) s$r.squared),
           haeldning = sapply(regressioner, function(s) s$coefficients[2, 1]),
           p_vaerdi  = sapply(regressioner, function(s) s$coefficients[2, 4]))
rownames(tabel) <- NULL
