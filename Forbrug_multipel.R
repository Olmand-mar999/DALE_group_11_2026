library(readxl)
library(tidyr)
library(dplyr)
library(zoo)
library(ggplot2)
options(scipen = 999)


# Få tillidsindikatoren ind
ftillid <- read_excel("Forbruger_alt_og_gammel.xlsx", 
                      skip = 1)

ftillid_ny <- as.data.frame(t(ftillid))
ftillid_ny[1,1] <- "Periode"
names(ftillid_ny) <- ftillid_ny[1,]
ftillid_ny <- ftillid_ny[2:nrow(ftillid_ny),]
row_2000 <- which(ftillid_ny[,1]=="2000M01")
ftillid_2000 <- ftillid_ny[row_2000:nrow(ftillid_ny),]

# Samler og giver alle værdierne tilhørende kvartaler

# Erstat ".." med NA
ftillid_2000[ftillid_2000 == ".."] <- NA

# Antal komplette kvartaler. floor rounds down to lowest integer
n_kvartaler <- floor(nrow(ftillid_2000) / 3)

# Behold kun komplette kvartaler
ftillid_2000_trim <- ftillid_2000[1:(n_kvartaler * 3), ]

# Gruppér hver 3. måned
gruppe <- rep(1:n_kvartaler, each = 3)

# Beregn gennemsnit for hver gruppe
nyt_df <- aggregate(
  ftillid_2000_trim[, -1],
  by = list(gruppe = gruppe),
  FUN = function(x) mean(as.numeric(x), na.rm = TRUE)
)

# Tag marts, juni, september og december fra Periode-kolonnen
kvartalsdatoer <- ftillid_2000$Periode[seq(3, nrow(ftillid_2000), by = 3)]

# Sæt kvartalerne ind
nyt_df$Periode <- kvartalsdatoer

# Flyt Periode til første kolonne
nyt_df <- nyt_df[, c(
  "Periode",
  setdiff(names(nyt_df), c("Periode", "gruppe"))
)]

# Få privatforbruget ind
p_forbrug <- read_excel("Privatforbrug_alt3.xlsx", 
                        skip = 1)
p_forbrug_ny <- as.data.frame(t(p_forbrug))
p_forbrug_ny <- p_forbrug_ny[4:nrow(p_forbrug_ny),]

names(p_forbrug_ny) <- c("Periode", "Privat_forbrug")

p_forbrug_ny$Privat_forbrug <- as.numeric(p_forbrug_ny$Privat_forbrug)

## Grunden til at der skal være 4 NA'er, er at funktionen returner
# 4 værdier mindre end der er i datasættet, så der der nødt til at blive lavet
# 4 tomme rækker så den ikke giver fejl
# Der er indhentet data startende fra 1999 i stedet for 2000,
# da jeg skal bruge 1999 til at regne tilbage for at finde værdierne for 2000.
p_forbrug_ny$Privat_forbrug <- c(
  rep(NA, 4),
  diff(log(p_forbrug_ny$Privat_forbrug), lag = 4) * 100
)

p_forbrug_ny <- p_forbrug_ny[5:nrow(p_forbrug_ny),]
nyt_df$Periode <- p_forbrug_ny$Periode

collected_data <- merge(p_forbrug_ny, 
                        nyt_df, by = "Periode")

names(collected_data) <- gsub(" ", ".", names(collected_data))
names(collected_data) <- gsub(",", ".", names(collected_data))
names(collected_data)

# 4b. MULTIPLE REGRESSION
lmm.forbrug.ftillid.ftillid <- lm(Privat_forbrug ~
                                    Familiens.økonomiske.situation.i.dag..sammenlignet.med.for.et.år.siden +
                                    Familiens.økonomiske..situation.om.et.år..sammenlignet.med.i.dag +
                                    Danmarks.økonomiske.situation.i.dag..sammenlignet.med.for.et.år.siden +
                                    Danmarks.økonomiske.situation.om.et.år..sammenlignet.med.i.dag +
                                    Anskaffelse.af.større.forbrugsgoder..fordelagtigt.for.øjeblikket,
                                  data = collected_data)
summary(lmm.forbrug.ftillid.ftillid)

coefs <- summary(lmm.forbrug.ftillid.ftillid)$coef

regressionstabel <- data.frame(
  Variabel = rownames(coefs),
  Estimate = coefs[, "Estimate"],
  Std.Error = coefs[, "Std. Error"],
  t.value = coefs[, "t value"],
  p.value = coefs[, "Pr(>|t|)"],
  Signifikans = ifelse(coefs[, "Pr(>|t|)"] < 0.001, "***",
                ifelse(coefs[, "Pr(>|t|)"] < 0.01, "**",
                ifelse(coefs[, "Pr(>|t|)"] < 0.05, "*",
                ifelse(coefs[, "Pr(>|t|)"] < 0.1, ".", "")))),
  row.names = NULL
)

regressionstabel
summary(lmm.forbrug.ftillid.ftillid)$r.squared ## Giver R2

signifikant_tabel <- regressionstabel[c(1, 4, 6), ]
