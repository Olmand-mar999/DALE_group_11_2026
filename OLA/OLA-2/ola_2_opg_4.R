# Opg. 4.1 ----

library(dkstat)
library(tidyr)

#Hent Forbrugerforventninger data
formeta <- dst_meta("FORV1")
formeta$variables
formeta$values$INDIKATOR
formeta$values$Tid

my_query <- list(
  INDIKATOR = "*",
  Tid = "*"
)

Forventninger <- dst_get_data("FORV1", query = my_query)

Forventninger <- pivot_wider(Forventninger, names_from = INDIKATOR, 
                             values_from = value)

# Gem data fra 2000 og frem
data_cut <- which(Forventninger[,1]=="1996-01-01 CET")
Forventninger <- Forventninger[data_cut:nrow(Forventninger),]

# Feature engineering, kvartaler

# Antal komplette kvartaler. floor rounds down to lowest integer
kvartaler <- floor(nrow(Forventninger) / 3)

# Gruppér hver 3. måned. Giver grupper der slutter med værdien 106.
KV_grupper <- rep(1:kvartaler, each = 3)

# Fjern overskydende måneder der ikke indgår i et fuldt kvartal
KV_forventninger <- Forventninger[1:(kvartaler * 3), ]

# -1 i koden betyder ikke medtag 1. kolonne
KV_forventninger <- aggregate(
  KV_forventninger[, -1],
  by = list(KV_grupper = KV_grupper),
  FUN = function(x) mean(as.numeric(x), na.rm = TRUE)
)

# -1 i koden betyder den ikke medtager perioderne. De indsættes igen.
KV_måneder <- Forventninger$TID[seq(3, nrow(Forventninger), by = 3)]
KV_forventninger$Kvartal <- KV_måneder

# Flyt Kvartal til første kolonne og fjern KV_grupper som kolonne
KV_forventninger <- KV_forventninger[, c(
  "Kvartal",
  setdiff(names(KV_forventninger), c("Kvartal", "KV_grupper"))
)]

# Lav plot af tillidsindikatoren
library(ggplot2)

ggplot(
  KV_forventninger,
  aes(
    x = Kvartal,
    y = `F1 Forbrugertillidsindikatoren`
  )
) +
  geom_line(
    color = "#F39C12",
    linewidth = 0.55
  ) +
  geom_hline(
    yintercept = 0,
    color = "black",
    linetype = "dashed",
    linewidth = 0.4
  ) +
  scale_x_datetime(
    limits = as.POSIXct(c("1996-01-01", "2026-12-31")),
    date_breaks = "2 years",
    date_labels = "%Y",
    expand = c(0, 0)
  ) +
  scale_y_continuous(
    breaks = c(-30, -20, -10, 0, 10),
    limits = c(-35, 15)
  ) +
  labs(
    title = "DST's forbrugertillidsindikator",
    x = "År",
    y = "Forbrugertillidsindikator"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(
      size = 16,
      hjust = 0.5
    ),
    axis.title.x = element_text(size = 11),
    axis.title.y = element_text(size = 11),
    axis.text = element_text(size = 9),
    panel.grid.major = element_line(
      color = "#E5E5E5",
      linewidth = 0.4
    ),
    panel.grid.minor = element_line(
      color = "#EEEEEE",
      linewidth = 0.3
    )
  )

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

# -1 i koden betyder den ikke medtager perioderne. De indsættes igen.
År_data <- Forventninger$TID[seq(1, nrow(Forventninger), by = 12)]
År_data <- År_data[1:År]

År_forventninger$År <- År_data

# Flyt Kvartal til første kolonne og fjern KV_grupper som kolonne
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
