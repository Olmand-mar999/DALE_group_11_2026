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


POSTNR1$bycat <- cut(POSTNR1$Indbyggertal,
                     breaks = c(0, 1000, 5000, 20000, 100000, Inf),
                     labels = c("landsby", "lille by", "almindelig by", "større by", "storby"),
                     right  = FALSE)

#1.3
data <- read.csv("https://raw.githubusercontent.com/Olmand-mar999/DALE_group_11_2026/main/Dataset/boligsiden.csv",
                 encoding = "UTF-8")
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
col_data_unik <- col_data[!duplicated(col_data$postnr), ]

# Siden der har været postnumre der går igen, vil by_data ikke længere passe.
# Den fjernes
col_data_unik <- col_data_unik[,1:17]

# Finder alle de bynavne der går igen flere gange
antal_byer <- table(col_data_unik$by)
gentagne_byer <- names(antal_byer[antal_byer > 1])

# Viser pænt med postnumre og alfabetisk hvilke byer der går igen
resultat <- col_data_unik[col_data_unik$by %in% gentagne_byer, c("by", "postnr")]
resultat <- resultat[order(resultat$by), ]
resultat
#jerslev, noerre, nykoebing, soender, store, viby
unik_liste <- c("jerslev", "noerre", "nykoebing", "soender", "store", "viby")
# Det er de byer hvor der findes flere byer med det samme navn
by_summer <- col_data_unik[!col_data_unik$by %in% unik_liste,]
by_ikke_summer <- col_data_unik[col_data_unik$by %in% unik_liste,]
by_ikke_summer

# Samler summen af alle postnumre der hører til samme by
by_summer <- aggregate(
  Indbyggertal ~ by,
  data = by_summer,
  FUN = sum
)

colnames(by_ikke_summer)
col_data_ny <- rbind(by_summer, by_ikke_summer[,c(5,17)])

# by_data tilføjes igen. Den blev fjernet i processen med at summere bynavne
col_data_ny$bycat <- cut(col_data_ny$Indbyggertal,
                         breaks = c(0, 1000, 5000, 20000, 100000, Inf),
                         labels = c("landsby", "lille by", "almindelig by", "større by", "storby"),
                         right  = FALSE)

# Spørg Baum om det gør noget vi kun har by, indbyggertal og bystørrelse
# som de eneste kolonner i endelig tabel
# Giver vel ikke mening at inkludere resten

sum(col_data_ny$Indbyggertal)

#1.4
library(ggplot2)

plot_data <- col_data_ny %>%
  group_by(bycat) %>%
  summarise(Indbyggertal = sum(Indbyggertal, na.rm = TRUE)) %>%
  mutate(procent = Indbyggertal / sum(Indbyggertal) * 100) %>%
  mutate(bycat = factor(bycat, 
                        levels = c("landsby", 
                                   "lille by", 
                                   "almindelig by", 
                                   "større by", 
                                   "storby")))

ggplot(plot_data, aes(x = bycat, y = procent, fill = bycat)) +
  geom_col() +
  scale_fill_brewer(palette = "Oranges") +
  labs(
    title = "Andel af indbyggertal fordelt på bytype",
    x = "Bykategori",
    y = "Andel af indbyggertal (%)",
    fill = "Bytype"
  ) +
  theme_minimal()

