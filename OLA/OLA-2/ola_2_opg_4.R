# Opg. 4.1 ----

# Pakker
library(dkstat)
library(tidyr)
library(ggplot2)

# Hent data fra DST
Forventninger <- dst_get_data("FORV1", query = list(INDIKATOR = "*", Tid = "*"))

# Datokollone laves (kan muligvis kigge ind i om Tid bare skal reformateres)
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


# Opg 4.2----

colnames(KV_forventninger)
# Det er F9 der er det spørgsmål de mener.

KV_forventninger$Kvartal

# Gem data fra 2000 og frem
data_cut <- which(Forventninger[,1]=="2000-03-01 CET")
Forventninger_2000 <- Forventninger[data_cut:nrow(Forventninger),]

round(mean(Forventninger_2000$`F9 Anskaffelse af større forbrugsgoder, fordelagtigt for øjeblikket`),2)


# 4.3

#Hent Forbrugsgruppe data
formeta <- dst_meta("NAHC21")
formeta$variables
formeta$values$FORMAAAL
formeta$values$PRISENHED
formeta$values$Tid

my_query <- list(
  FORMAAAL = "*",
  PRISENHED = "Løbende priser",
  Tid = c("2020", "2021", "2022", "2023")
)

Forbrugsgrupper <- dst_get_data("NAHC21", query = my_query)

Forbrugsgrupper <- pivot_wider(Forbrugsgrupper, names_from = FORMAAAL, 
                             values_from = value)

# Max værdi
max_val <- max(Forbrugsgrupper[3,4:14])
which(Forbrugsgrupper[3,]==max_val)
Forbrugsgrupper[3,7]

# Værdi (2023 - værdi 2020) / værdi 2020 * 100

# Skal være numeric da class ellers er data.frame
Forbrug_stigning <- data.frame(
  Forbrugsgruppe = colnames(Forbrugsgrupper)[4:ncol(Forbrugsgrupper)],
  Forbrug_stigning = as.numeric(
    (Forbrugsgrupper[4, 4:ncol(Forbrugsgrupper)] -
       Forbrugsgrupper[1, 4:ncol(Forbrugsgrupper)]) /
      Forbrugsgrupper[1, 4:ncol(Forbrugsgrupper)] * 100),
  Penge_stigning = as.numeric(Forbrugsgrupper[4, 4:ncol(Forbrugsgrupper)] -
                                Forbrugsgrupper[1, 4:ncol(Forbrugsgrupper)])
)

ggplot(Forbrug_stigning, 
       aes(x = reorder(Forbrugsgruppe, Forbrug_stigning),
           y = Forbrug_stigning)) +
  geom_col(fill = "steelblue") +
  geom_text(
    aes(label = paste0(round(Forbrug_stigning, 1), "%")),
    hjust = -0.1,
    size = 3
  ) +
  coord_flip() +
  scale_y_continuous(
    breaks = seq(0, 40, by = 10),
    limits = c(0, 45)
  ) +
  labs(
    title = "Procentvis stigning i forbrug 2020-2023",
    x = NULL,
    y = "Stigning (%)"
  ) +
  theme_minimal()


# Opg 4.4
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
