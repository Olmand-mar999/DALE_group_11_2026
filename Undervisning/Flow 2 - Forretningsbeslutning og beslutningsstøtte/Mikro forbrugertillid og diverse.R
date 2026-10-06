library(dkstat)
library(caret)

# 3.1---------------------------------------------------------------------------

# Hentning af forbrugsdata
Forbrugsdata <- dst_get_data("NKN1", query = list(TRANSAKT = "*", PRISENHED = "*", SÆSON = "*", Tid = "*"))

##### Måske skal P31S14D eller P31S1MD bruges #####
# Behold husholdningernes forbrugsudgifter (P31S14D), kædede værdier i mia. kr. (LKV_M), sæsonkorrigeret (Y)
Dummy_data <- Forbrugsdata[grepl("^P31S14D", Forbrugsdata$TRANSAKT) &
                             grepl("^LKV_M ", Forbrugsdata$PRISENHED) &
                             grepl("^Y ", Forbrugsdata$SÆSON), ]

# ---- Årlig realvækst pr. kvartal ----
# Forbruget som almindelig vektor og antal kvartaler
x <- Dummy_data$value
n <- length(x)

# Vækst i pct. for hvert kvartal i forhold til samme kvartal året før (4 kvartaler tidligere)
Dummy <- (x[5:n] / x[1:(n - 4)] - 1) * 100

# Saml kvartalsdato og vækst i en tabel
Dummy <- data.frame(Kvartal = as.Date(Dummy_data$TID[5:n], tz = "Europe/Copenhagen"),
                    Forbrug_vaekst = Dummy)

# Behold kun rækker fra og med 1. kvartal 1998
Dummy <- Dummy[Dummy$Kvartal >= as.Date("1998-01-01"), ]

# Find hvor ofte den kvartalvise årlige realvækst falder (er negativ)
# Væksten laves om til binære værdier, 0 for negativ og 1 for positiv
Dummy$binary <- ifelse(Dummy$Forbrug_vaekst > 0, 1, 0)

# Lav en tabel der indeholder fordelingen
table_data <- table(Dummy$binary)

# Lav plot der viser fordelingen
bp <- barplot(
  table_data,
  ylim = c(0, 100),
  ylab = "Procent",
  xlab = "",
  main = "Realvæksten er positiv næsten 75% af gangene over tid ",
  names.arg = c("Negativ", "Positiv"),
  las = 1,
  col = c("#D55E00", "#F4A261")
)
abline(0,0)

mtext(
  "Kilde: Danmarks Statistik, NKN1, P.31 Husholdningernes forbrugsudgifter",
  side = 1,
  line = 4,
  adj = 0)

# 3.2  Hent forbrugertillidsundersøgelsen---------------------------------------
Forventninger <- dst_get_data("FORV1", query = list(INDIKATOR = "*", Tid = "*"))

# Tidskolonne sættes til datoformat
Forventninger$TID <- as.Date(Forventninger$TID, tz = "Europe/Copenhagen")

# Behold kun rækker fra og med 1. januar 1998
Forventninger <- Forventninger[Forventninger$TID >= as.Date("1998-01-01"), ]

# Placér hver måned i sit kvartal. Kvartalet får datoen på kvartalets første dag
Forventninger$Kvartal <- as.Date(cut(Forventninger$TID, "quarter"))

# Beregn gennemsnittet af value for hver kombination af kvartal og indikator
KV_lang <- aggregate(value ~ Kvartal + INDIKATOR, data = Forventninger, FUN = mean)

# Gør tabellen "bred": én række pr. kvartal og én kolonne pr. indikator
KV <- reshape(KV_lang, idvar = "Kvartal", timevar = "INDIKATOR", direction = "wide")

# # reshape sætter "value." foran alle kolonnenavne, så det fjernes her
names(KV) <- sub("^value\\.", "", names(KV))

# DI-FTI's fire spørgsmål
F2  <- KV[[grep("^F2 ",  colnames(KV))]]   # Familiens økonomi i dag vs. for et år siden
F4  <- KV[[grep("^F4 ",  colnames(KV))]]   # Danmarks økonomi i dag vs. for et år siden
F9  <- KV[[grep("^F9 ",  colnames(KV))]]   # Fordelagtigt at købe forbrugsgoder nu
F10 <- KV[[grep("^F10 ", colnames(KV))]]   # Forbrugsgoder de næste 12 måneder

# DI-FTI er det simple gennemsnit af de fire spørgsmål
DI_FTI <- data.frame(DI = (F2 + F4 + F9 + F10) / 4)

#Tilføj kvartaler til DI
DI_FTI$Kvartal <- KV$Kvartal

# 3.2 Logistisk regression------------------------------------------------------

# Sæt vækst og indikatorer sammen via kvartalsdatoen
data_col <- merge(DI_FTI, Dummy, by = "Kvartal")

# Lav logistisk regression og gem de forudsagte værdier som o eller 1
log_model_DI  <- glm(binary ~ DI, family = "binomial", data = data_col)
data_col$log_binary <- ifelse(predict(log_model_DI) > 0, 1, 0)

# Gem DI-tillidsindikatoren for 3. kvartal
Forud_DI <- DI_FTI[DI_FTI$Kvartal == as.Date("2026-07-01"), ]
Forud_DI <- data.frame(DI = Forud_DI[,1])
Forud_DI # -13.308

# Nu kendes den forudsagte værdi. 
# Den logistiske regression kan nu bruges til at forudsige om den forudsagte værdi
# Vil være positiv eller negativ
Forud_vaekst <- predict(log_model_DI, newdata = Forud_DI)
Forud_vaekst # 0.058
ifelse(Forud_vaekst > 0.5, "OP", "NED")

# 3.3---------------------------------------------------------------------------

# Confusion matrix tager kun faktorer som argument
data_col$log_binary <- as.factor(data_col$log_binary)
data_col$binary <- as.factor(data_col$binary)

# Der laves en confusion matrice for at se hvor god modellen er til at ramme rigtigt
conf_matrix <- confusionMatrix(data_col$binary, data_col$log_binary)
conf_matrix

# Modellen finder 1,1 81 gange og der er 18 gange den ikke gør.
accurracy <- 81/(81+18)
accurracy <- conf_matrix$table[2,2]/(conf_matrix$table[2,2] + conf_matrix$table[1,2])
accurracy
# Den gætter rigtigt at den stiger 81,8% af gangene
# Samlet set har den en accurracy på 79,8%

# 3.4 forbedring af model-------------------------------------------------------

# Hvordan kan den forbedres?












# Alt relevant til undervisning d. 6/10

# Fjerde kvartal
test <- seq(4,nrow(DI_FTI),4)
mean(DI_FTI[test,1])

# Tredje kvartal
test2 <- seq(4,nrow(DI_FTI),4)
test2 <- test2-1
mean(DI_FTI[test2,1])

# Hvor mange gange er fjerde kvartal højere end tredje kvartal
j <- sum(DI_FTI[test, 1] > DI_FTI[test2, 1])
j

# Lav alle kombinationer af alle spørgsmål. Skal F11 fjernes?
my_numbers <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12)

# Get combinations for all possible group sizes (1 through 12)
all_combinations <- lapply(1:length(my_numbers), function(x) combn(my_numbers, x, simplify = FALSE))

# Unlist to get a single flat list containing all 31 unique combinations
all_combinations <- unlist(all_combinations, recursive = FALSE)


# Er artiklens forbrugertillidsindikator med mikro bedre end vores?
# Alle spørgsmål
F2  <- KV[[grep("^F2 ",  colnames(KV))]]   # Familiens økonomi i dag vs. for et år siden
F3  <- KV[[grep("^F3 ",  colnames(KV))]]   # Familiens økonomiske  situation om et år, sammenlignet med i dag
F4  <- KV[[grep("^F4 ",  colnames(KV))]]   # Danmarks økonomi i dag vs. for et år siden
F5  <- KV[[grep("^F5 ",  colnames(KV))]]   # Danmarks økonomiske situation om et år, sammenlignet med i dag
F6  <- KV[[grep("^F6 ",  colnames(KV))]]   # Priser i dag, sammenlignet med for et år siden
F7  <- KV[[grep("^F7 ",  colnames(KV))]]   # Priser om et år, sammenlignet med i dag
F8  <- KV[[grep("^F8 ",  colnames(KV))]]   # Arbejdsløsheden om et år, sammenlignet med i dag
F9  <- KV[[grep("^F9 ",  colnames(KV))]]   # Fordelagtigt at købe forbrugsgoder nu
F10 <- KV[[grep("^F10 ", colnames(KV))]]   # Forbrugsgoder de næste 12 måneder
F11 <- KV[[grep("^F11 ",  colnames(KV))]]  # Anser det som fornuftigt at spare op i den nuværende økonomiske situation
F12 <- KV[[grep("^F12 ",  colnames(KV))]]  # Regner med at kunne spare op i de kommende 12 måneder
F13 <- KV[[grep("^F13 ",  colnames(KV))]]  # Familiens økonomiske situation lige nu: kan spare/penge slår til/ bruger mere end man tjener
colnames(KV)


# Mikro spørgsmål fra artiklen
Artikel <- data.frame(Kvartal = KV$Kvartal,
                      Q1 = F2,
                      Q2 = F3,
                      Q8 = F9,
                      Q9 = F10)
# Tilføj gennemsnit
Artikel$Gennemsnit <- rowMeans(Artikel[,2:ncol(Artikel)])

# Vores vurdering af mikrospørgsmål
vores_vurdering <- data.frame(Kvartal = KV$Kvartal,
                              F2 = F2,
                              F3 = F3,
                              F9 = F9,
                              F10 = F10,
                              F12 = F12,
                              F13 = F13)

vores_vurdering$Gennemsnit <- rowMeans(vores_vurdering[,2:ncol(vores_vurdering)])

Artikel_merge <- merge(Artikel, Dummy[,-3], by = "Kvartal")
Vores_merge <- merge(vores_vurdering, Dummy[,-3], by = "Kvartal")

model_artikel <- lm(Forbrug_vaekst ~ Gennemsnit, data = Artikel_merge)
summary_artikel <- summary(model_artikel)
summary_artikel$r.squared

model_vores <- lm(Forbrug_vaekst ~ Gennemsnit, data = Vores_merge)
summary_vores <- summary(model_vores)
summary_vores$r.squared

# Kan vores være bedre, siden deres er fra 2009? Der kan være sket meget siden da
