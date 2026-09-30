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


# Opg 4.4: simple lineære regressioner ----
# Antal komplette kvartaler. floor rounds down to lowest integer
År <- floor(nrow(Forventninger) / 12)

# Gruppér hver 12. måned. 
År_grupper <- rep(1:År, each = 12)

# Fjern overskydende måneder der ikke indgår i et fuldt år
År_forventninger <- Forventninger[1:(År * 12), ]

# -1 i koden betyder ikke medtag 1. kolonne
År_forventninger <- aggregate(
  År_forventninger[, -1],
  by = list(År_grupper = År_grupper),
  FUN = function(x) mean(as.numeric(x), na.rm = TRUE)
)

# -1 i koden betyder den ikke medtager tidsperioderne. De indsættes igen.
# Den tager 2026 med, da den har den første måned inkluderet. Den fjernes
År_data <- Forventninger$TID[seq(1, nrow(Forventninger), by = 12)]
År_data <- År_data[1:År]

År_forventninger$År <- År_data

# Flyt år til første kolonne og fjern år_grupper som kolonne
År_forventninger <- År_forventninger[, c(
  "År",
  setdiff(names(År_forventninger), c("År", "År_grupper"))
)]
# -2 er for at få data frem til 2023 som forbrugsdataen har
data_cut <- which(År_forventninger$År=="2000-01-01 CET")
År_forventninger <- År_forventninger[data_cut:(nrow(År_forventninger)-2),]

FTI <- År_forventninger[,c(1,3,4,5,6,7)]
DI_FTI <- År_forventninger[,c(1,3,5,7,11)]
FTI$Gennemsnit <- rowMeans(FTI[, -1], na.rm = TRUE)
DI_FTI$Gennemsnit <- rowMeans(DI_FTI[, -1], na.rm = TRUE)

my_query2 <- list(
  FORMAAAL = "*",
  PRISENHED = "Løbende priser",
  Tid = "*"
)

Forbrugsgrupper_alt <- dst_get_data("NAHC21", query = my_query2)
Forbrugsgrupper_alt <- as.data.frame(pivot_wider(Forbrugsgrupper_alt, 
                              names_from = FORMAAAL, values_from = value))

data_cut2 <- which(Forbrugsgrupper_alt$TID=="2000-01-01 CET")
Forbrugsgrupper_alt <- Forbrugsgrupper_alt[data_cut2:nrow(Forbrugsgrupper_alt),]
Forbrugsgrupper_alt$DI_FTI <- DI_FTI$Gennemsnit
Forbrugsgrupper_alt$FTI <- FTI$Gennemsnit

colnames(Forbrugsgrupper_alt)

# Paste sætter navnene på alle listerne. 
# Så den sætter navnet på kolonnen den laver lm med efter -
# [[]] betyder der arbejdes i en liste.
summary_list <- list()
for (i in 1:11) {
  DI_FTI_model <- lm(Forbrugsgrupper_alt[,(i+3)] ~ Forbrugsgrupper_alt[, 15])
  FTI_model <- lm(Forbrugsgrupper_alt[,(i+3)] ~ Forbrugsgrupper_alt[, 16])
  name = colnames(Forbrugsgrupper_alt[i+3])
  summary_list[[paste0("DI_FTI_", name)]] <- summary(DI_FTI_model)
  summary_list[[paste0("FTI_", name)]] <- summary(FTI_model)
}
names(summary_list)
