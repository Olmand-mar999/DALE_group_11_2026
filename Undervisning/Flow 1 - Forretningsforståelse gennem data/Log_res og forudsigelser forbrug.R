library(dkstat)
library(tidyr)

#Hent Forbrugerforventninger data-----------------------------------------------
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
data_cut <- which(Forventninger[,1]=="2000-01-01 CET")
Forventninger <- Forventninger[data_cut:nrow(Forventninger),]

# Feature engineering, kvartaler og FTI og DI-FTI-------------------------------

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

# Gem FTI og DI-FTI spørgsmål i eget dataframe
colnames(KV_forventninger)

FTI <- KV_forventninger[,c(1,3,4,5,6,7)]
DI_FTI <- KV_forventninger[,c(1,3,5,7,11)]

# Tilføj kolonne med simpelt gennemsnit af spørgsmålene
FTI$Gennemsnit <- rowMeans(FTI[, -1], na.rm = TRUE)
DI_FTI$Gennemsnit <- rowMeans(DI_FTI[, -1], na.rm = TRUE)

#Hent husholdningernes forbrugsudgifter-----------------------------------------
primeta <- dst_meta("NKN1")
primeta$variables
primeta$values$TRANSAKT
primeta$values$PRISENHED
primeta$values$SÆSON
primeta$values$Tid

my_query2 <- list(
  TRANSAKT = "P.31 Husholdningernes forbrugsudgifter",
  PRISENHED = "2020-priser, kædede værdier, (mia. kr.)",
  SÆSON = "Sæsonkorrigeret",
  Tid = "*"
)

Forbrug <- dst_get_data("NKN1", query = my_query2)
Forbrug <- Forbrug[,4:5]
colnames(Forbrug) <- c("Kvartal","Forbrug")

# Gem data fra 1999 og frem (1999 skal bruges til feature engineering)
data_cut2 <- which(Forbrug[,1]=="1999-01-01 CET")
Forbrug <- Forbrug[data_cut2:nrow(Forbrug),]

# Pakken henter kvartaler som den første måned i kvartalet
# For at det skal matche forventningsdataen skal det laves om til den sidste måned

Forbrug$Kvartal <- seq(
  from = as.POSIXct("1999-03-01", tz = "Europe/Copenhagen"),
  by = "3 months",
  length.out = nrow(Forbrug)
)


# Feature engineering-----------------------------------------------------------

## Grunden til at der skal være 4 NA'er, er at funktionen returner
# 4 værdier mindre end der er i datasættet, så der der nødt til at blive lavet
# 4 tomme rækker så den ikke giver fejl
Forbrug$Forbrug <- c(
  rep(NA, 4),
  diff(log(Forbrug$Forbrug), lag = 4) * 100
)
Forbrug <- Forbrug[5:nrow(Forbrug),]

#Merge FTI og DI-FTI med forbrugsdata------------------------------------------

FTI_col = merge(Forbrug, FTI, by = "Kvartal")
DI_FTI_col = merge(Forbrug, DI_FTI, by = "Kvartal")

# Plot--------------------------------------------
library(ggplot2)

ggplot(
  Forbrug,
  aes(
    x = Kvartal,
    y = Forbrug
  )
) +
  geom_line(color = "orange", linewidth = 1) +
  scale_x_datetime(
    date_breaks = "2 year",
    date_labels = "%Y"
  ) +
  labs(
    x = "Kvartaler",
    y = "Forbrug",
    title = "Udvikling i forbrug"
  ) +
  theme_minimal() +
  theme(
    panel.grid.major = element_line(color = "grey90"),
    panel.grid.minor = element_line(color = "grey95")
  )

library(dplyr)

data1 <- Forbrug %>%
  arrange(Kvartal) %>%   # sørg for at data er sorteret kronologisk efter tid
  mutate(
    gruppe = ifelse(Forbrug >= 0, "OP", "NED")
  )
table(data1$gruppe)
data_val <- table(data1$gruppe)
data_val <- round(prop.table(data_val) * 100, 1)

bp <- barplot(
  data_val,
  ylim = c(0, 100),
  ylab = "Procent",
  xlab = "",
  main = "Fordeling af kvartalvis realvækst",
  names.arg = c("Negativ", "Positiv"),
  las = 1,
  col = c("#D55E00", "#F4A261")
)
abline(0,0)

mtext(
  "Kilde: Danmarks Statistik, NKN1, P.31 Husholdningernes forbrugsudgifter",
  side = 1,
  line = 4,
  adj = 0
)

# logistisk regression med simpelt gennemsnit

DI_FTI_col$Dummy <- ifelse(Forbrug$Forbrug > 0, 1, 0)
log_model <- glm(Dummy ~ Gennemsnit, family = "binomial", data = DI_FTI_col)
DI_FTI_col$log_pred <- ifelse(predict(log_model) > 0, 1, 0)

new_row <- nrow(FTI_col)*3 + 1
DI_FTI_sidst <- Forventninger[new_row:nrow(Forventninger),c(1,3,5,7,11)]
DI_FTI_sidst$Gennemsnit <- rowMeans(DI_FTI_sidst[, -1], na.rm = TRUE)
DI_FTI_sidst <- data.frame(Gennemsnit = mean(DI_FTI_sidst$Gennemsnit))
DI_FTI_pred <- predict(log_model, newdata = DI_FTI_sidst)
DI_FTI_pred
ifelse(DI_FTI_pred > 0.5, "OP", "NED")

# Hvor ofte har den logistiske regression ret
validation <- ifelse(DI_FTI_col$Dummy == DI_FTI_col$log_pred, "Rigtig", "Forkert")
validation_table <- table(validation)
validation_table
validation_table[2]/sum(validation_table)

library(caret)
conf_matrix <- confusionMatrix(as.factor(DI_FTI_col$Dummy), as.factor(DI_FTI_col$log_pred))
conf_matrix
log_pred
DI_FTI_col$Dummy

for (i in 1:12){
  data1 <- Forventninger %>%
    arrange(Forventninger$TID) %>%   # sørg for at data er sorteret kronologisk efter tid
    mutate(
      gruppe = factor(ifelse(Forventninger[,(i+2)] >= 0, "OP", "NED"),levels = c("NED", "OP"))
    )
  table(data1$gruppe)
  data_val <- table(data1$gruppe)
  data_val <- round(prop.table(data_val) * 100, 1)
  name = colnames(Forventninger[i+2])
  
  bp <- barplot(
    data_val,
    ylim = c(0, 100),
    ylab = "Procent",
    xlab = "",
    main = paste0("Udvikling i spørgsmålet", "\n", name),
    names.arg = c("Negativ", "Positiv"),
    las = 1,
    col = c("#D55E00", "#F4A261"),
    cex.main = 0.9
  )
  abline(0,0)
  
  mtext(
    "Kilde: Danmarks Statistik, FORV1",
    side = 1,
    line = 4,
    adj = 0
  )
}
