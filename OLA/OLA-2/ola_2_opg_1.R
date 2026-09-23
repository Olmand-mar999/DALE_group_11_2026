library(dkstat)
library(dplyr)
library(tidyr)

#1.1
alltable = dkstat::dst_get_tables()

regmeta <- dst_meta("POSTNR1")
regmeta$variables
regmeta$values$PNR20
regmeta$values$KØN
regmeta$values$ALDER
regmeta$values$CIVILTILSTAND
regmeta$values$Tid

# * betyder alle muligheder. Kan ikke lide civiltilstand af en eller anden grund.
my_query <- list(
  PNR20 = "*",
  KØN = "I alt",
  ALDER = "Alder i alt",
  TID = "2026"
)

POSTNR1 <- dst_get_data("POSTNR1", query = my_query)
POSTNR1 <- POSTNR1[2:nrow(POSTNR1),]
POSTNR1$PNR20 <- substr(POSTNR1$PNR20,1,4)
colnames(POSTNR1)[1] <- "postnr"
colnames(POSTNR1)[5] <- "Indbyggertal"

#1.2
steps <- seq(min(POSTNR1$Indbyggertal),max(POSTNR1$Indbyggertal),length.out = 6)
steps


POSTNR1 <- POSTNR1 %>%
  mutate(
    by_data = case_when(
      Indbyggertal < 7000 ~ "landsby",
      Indbyggertal < 12000 ~ "lille by",
      Indbyggertal < 35000 ~ "almindelig by",
      Indbyggertal < 65000 ~ "større by",
      Indbyggertal >= 65000 ~ "storby"
    )
  )

#1.3
data <- read.csv("https://raw.githubusercontent.com/Olmand-mar999/DALE_group_11_2026/main/Dataset/boligsiden.csv",header = T)
data <- data[2:nrow(data),]
for (i in which(is.na(data$liggetid)))
  data$liggetid[i] <- "0 dag"

# Alle andre rækker med NA fjernes nu
data <- na.omit(data)

# Lav pris til numeric
data$pris <- sub(" kr\\.", "", data$pris)
data$pris <- sub("\\.", "", data$pris)
data$pris <- sub("\\.", "", data$pris)
data$pris <- as.numeric(data$pris)

# Lav liggetid til numerisk
data$liggetid <- sub(" dag","",data$liggetid)
data$liggetid <- as.numeric(data$liggetid)

# Fjern punktummer i kvmpris og mdudg da R læser det som et komma
data$kvmpris <- sub("\\.", "", data$kvmpris)
data$kvmpris <- as.numeric(data$kvmpris)
data$mdudg <- sub("\\.", "", data$mdudg)
data$mdudg <- as.numeric(data$mdudg)

# Lav opførselsår om til alder på bygning
data$alder <- (2024-data$opført)

col_data <- merge(data, POSTNR1, by = "postnr")

# Gør så der ikke er nogen postnumre der går igen.
col_data_unik <- col_data %>%
  distinct(postnr, .keep_all = TRUE)

# Siden der har været postnumre der går igen, vil by_data ikke længere passe.
# Den fjernes
col_data_unik <- col_data_unik[,1:17]

# Samler summen af alle postnumre der hører til samme by
col_data_unik <- col_data_unik %>%
  group_by(by) %>%
  summarise(
    Indbyggertal = sum(Indbyggertal, na.rm = TRUE)
  )

# by_data fjernes og tilføjes derfor igen så de passer
col_data_unik <- col_data_unik[,1:2]

col_data_unik <- col_data_unik %>%
  mutate(
    by_data = case_when(
      Indbyggertal < 7000 ~ "landsby",
      Indbyggertal < 12000 ~ "lille by",
      Indbyggertal < 35000 ~ "almindelig by",
      Indbyggertal < 65000 ~ "større by",
      Indbyggertal >= 65000 ~ "storby"
    )
  )

# Spørg Baum om det gør noget vi kun har by, indbyggertal og bystørrelse
# som de eneste kolonner i endelig tabel
# Giver vel ikke mening at inkludere resten

sum(col_data_unik$Indbyggertal)

#1.4
library(ggplot2)

plot_data <- col_data_unik %>%
  group_by(by_data) %>%
  summarise(Indbyggertal = sum(Indbyggertal, na.rm = TRUE)) %>%
  mutate(procent = Indbyggertal / sum(Indbyggertal) * 100) %>%
  mutate(by_data = factor(by_data, 
                          levels = c("landsby", "lille by", "almindelig by", "større by", "storby")))

ggplot(plot_data, aes(x = by_data, y = procent, fill = by_data)) +
  geom_col() +
  scale_fill_brewer(palette = "Oranges") +
  labs(
    title = "Andel af indbyggertal fordelt på bytype",
    x = "Bykategori",
    y = "Andel af indbyggertal (%)",
    fill = "Bytype"
  ) +
  theme_minimal()

