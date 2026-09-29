library(dkstat)
library(dplyr)
library(tidyr)

#1.1
alltable = dkstat::dst_get_tables()

regmeta <- dst_meta("BY3")
regmeta$variables
regmeta$values$BYER
regmeta$values$FOLKARTAET
regmeta$values$Tid

my_query <- list(
  BYER = "*",
  FOLKARTAET = "Folketal",
  TID = "2026"
)

# Hent data
BY3 <- dst_get_data("BY3", query = my_query)

# Sætter value til indbyggertal
colnames(BY3)[colnames(BY3) == "value"] <- "Indbyggertal"

# Fjern rækker der ikke har et indbyggertal
BY3 <- BY3[BY3$Indbyggertal != 0,]

# Fjern rækker der ikke er bynavne
BY3 <- BY3[!grepl("Uden fast bopæl", BY3$BYER),]
BY3 <- BY3[!grepl("Landdistrikter", BY3$BYER),]

sub(" \\(.*\\)", "", BY3$BYER)

# BY3 renses for æ,ø,å og store bogstaver
clean_function <- function(x) {
  x <- tolower(x)                   # Laver store bogstaver til små
  x <- gsub(" \\(.*\\)", "", x)     # Fjerner alt i parentes
  x <- gsub("[0-9]","", x)          # Fjerner tal
  x <- gsub("æ", "ae", x)
  x <- gsub("å", "aa", x)
  x <- gsub("ø", "oe", x)
  x <- sub("-","", x)
  trimws(x)
}

BY3$BYER <- clean_function(BY3$BYER)

# Der er byer der optræder flere gange. De samles med et samlet indbyggertal
BY3 <- aggregate(Indbyggertal ~ BYER, data = BY3, FUN = sum)
sum(duplicated(BY3$BYER)) # Test om det virkede. Skal give 0

#1.2

# Inspiration fra danmarks statistik BY2 til at vælge inddelingen
dst_meta("BY2")$values$BYST

BY3$bycat <- cut(BY3$Indbyggertal,
                 breaks = c(0, 1000, 5000, 20000, 100000, Inf),
                 labels = c("landsby", "lille by", "almindelig by", "større by", "storby"),
                 right  = FALSE)

#1.3
data <- read.csv("boligsiden.csv",header = T)
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
data$alder <- (2026-data$opført)

# Der er 3 rækker der har indsat postnummer som bynavn. Det rettes
# Det er rækkerne 273, 1749, 2040
which(data$postnr < 100)
data$postnr[c(273, 1749, 2040)] <- data$by[c(273, 1749, 2040)]
data$by[c(273, 1749, 2040)] <- c("moeldrup", "kibaek", "hilleroed")
data$postnr[2040]

# BYER i vores BY3 data omnavngives til by så merge fungerer
colnames(BY3)[colnames(BY3) == "BYER"] <- "by"

# Merger vores data
col_data <- merge(data, BY3, by = "by", all.x = T)

# Gemmer NA rækker i sit eget og fjerner duplicates
na_rows <- col_data[which(is.na(col_data$Indbyggertal)),]

# Fjerner NA og duplicates fra vores plot data
col_data_na_free <- na.omit(col_data)

# Der er markant forskel på den gennemsnitlige kvadratmeter pris vi mangler data på
# og den data vi har data på.
mean(col_data_na_free$kvmpris)
mean(na_rows$kvmpris)

# Vi er derfor nødt til at indbyggertallet på de byer vi ikke har data på
# Derfor henter vi postnummer data fra danmarks statistik og merger med det.
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
POSTNR1$by_kommune <- substr(POSTNR1$PNR20,11,nchar(POSTNR1$PNR20))
POSTNR1$PNR20 <- substr(POSTNR1$PNR20,1,4)
colnames(POSTNR1)[1] <- "postnr"
colnames(POSTNR1)[5] <- "Ant_indbyg"

new_col <- merge(na_rows, POSTNR1, by = "postnr")
new_col$Indbyggertal <- new_col$Ant_indbyg

new_col$bycat <- cut(new_col$Indbyggertal,
                     breaks = c(0, 1000, 5000, 20000, 100000, Inf),
                     labels = c("landsby", "lille by", "almindelig by", "større by", "storby"),
                     right  = FALSE)

all_data <- rbind(col_data_na_free[,c("by", "Indbyggertal", "bycat", "kvmpris")], 
                  new_col[,c("by", "Indbyggertal", "bycat", "kvmpris")])
which(is.na(all_data))


library(ggplot2)

plot_df <- aggregate(kvmpris ~ bycat, data = all_data, FUN = mean)
plot_df$antal <- aggregate(kvmpris ~ bycat, data = all_data, FUN = length)$kvmpris
plot_df$label <- paste0(plot_df$bycat, "\n(n = ", plot_df$antal, ")")

ggplot(plot_df, aes(x = reorder(label, kvmpris), y = kvmpris)) +
  geom_col(fill = "orange", width = 0.7) +
  geom_text(aes(label = format(round(kvmpris), big.mark = ".", decimal.mark = ",")),
            vjust = -0.5, size = 3.5) +
  scale_y_continuous(labels = scales::label_number(big.mark = ".", decimal.mark = ","),
                     expand = expansion(mult = c(0, 0.08))) +
  labs(title = "Storby har den højeste pris pr kvm i kr",
       subtitle = "Boliger til salg, byer kategoriseret efter DST's byområder 2026",
       x = "Bykategori", y = "Kr. pr. m²",
       caption = "Kilde: Boligsiden og Danmarks Statistik (BY3)") +
  theme_minimal() +
  theme(panel.grid.major.x = element_blank())
