# =====================================================================
# 0. OPSÆTNING OG INDSTILLINGER
# =====================================================================
library(dkstat)
options(scipen = 999)

# ---- Indstillinger: ret kun her ----
tidszone         <- "Europe/Copenhagen"
startdato        <- as.Date("2000-01-01")   # første kvartal der bruges (forbrug og forventninger)
kvartaler_pr_aar <- 4                       # bruges til årlig vækst

# Forbrugsserien: husholdningernes forbrug (P31S14D), kædede værdier (LKV_M), sæsonkorrigeret (Y)
# Måske skal P31S14D eller P31S1MD bruges
forbrug_transakt <- "^P31S14D"
forbrug_enhed    <- "^LKV_M "
forbrug_saeson   <- "^Y "

# Spørgsmål der indgår i alle kombinationer (F11 er IKKE med, som i den oprindelige kode)
# F2  Familiens økonomi i dag vs. for et år siden
# F3  Familiens økonomiske situation om et år, sammenlignet med i dag
# F4  Danmarks økonomi i dag vs. for et år siden
# F5  Danmarks økonomiske situation om et år, sammenlignet med i dag
# F6  Priser i dag, sammenlignet med for et år siden
# F7  Priser om et år, sammenlignet med i dag
# F8  Arbejdsløsheden om et år, sammenlignet med i dag
# F9  Fordelagtigt at købe forbrugsgoder nu
# F10 Forbrugsgoder de næste 12 måneder
# F11 Fornuftigt at spare op i den nuværende situation
# F12 Regner med at kunne spare op i de kommende 12 måneder
# F13 Familiens økonomiske situation lige nu: kan spare / penge slår til / bruger mere end man tjener
spoergsmaal  <- c("F2", "F3", "F4", "F5", "F6", "F7", "F8", "F9", "F10", "F12", "F13")
vend_fortegn <- c("F6", "F7", "F8")   # "stigning" er dårligt (priser og arbejdsløshed)

antal_top      <- 5     # antal bedste indikatorer der vises i sektion 6-7
antal_modeller <- 20    # antal bedste indikatorer der får en forudsigelse i sektion 8
top_graense    <- 10    # "top N" i stabilitetstesten (sektion 9)
test_start     <- as.Date("2020-01-01")   # stabilitetstesten køres tilbage til dette kvartal


# =====================================================================
# 1. FORBRUGSDATA
# =====================================================================

# ---- Hentning af forbrugsdata ----
Forbrugsdata <- dst_get_data("NKN1", query = list(TRANSAKT = "*", PRISENHED = "*", SÆSON = "*", Tid = "*"))

Forbrugsdata <- Forbrugsdata[grepl(forbrug_transakt, Forbrugsdata$TRANSAKT) &
                               grepl(forbrug_enhed,    Forbrugsdata$PRISENHED) &
                               grepl(forbrug_saeson,   Forbrugsdata$SÆSON), ]

# Sortér efter tid, da væksten regnes ud fra rækkernes position
Forbrugsdata <- Forbrugsdata[order(Forbrugsdata$TID), ]

# ---- Årlig realvækst pr. kvartal ----
x <- Forbrugsdata$value
n <- length(x)

# Vækst i pct. i forhold til samme kvartal året før
Forbrugsdata_vaekst <- (x[(kvartaler_pr_aar + 1):n] / x[1:(n - kvartaler_pr_aar)] - 1) * 100

# Saml kvartalsdato og vækst i en tabel
Forbrugsdata <- data.frame(Kvartal = as.Date(Forbrugsdata$TID[(kvartaler_pr_aar + 1):n], tz = tidszone),
                           Forbrug_vaekst = Forbrugsdata_vaekst)

# Behold kun rækker fra og med startdato
Forbrugsdata <- Forbrugsdata[Forbrugsdata$Kvartal >= startdato, ]


# =====================================================================
# 2. FORBRUGERTILLIDSUNDERSØGELSEN (FORVENTNINGER)
# =====================================================================

# ---- Hentning ----
Forventninger <- dst_get_data("FORV1", query = list(INDIKATOR = "*", Tid = "*"))

# ---- Tid og afgrænsning ----
Forventninger$TID <- as.Date(Forventninger$TID, tz = tidszone)
Forventninger <- Forventninger[Forventninger$TID >= startdato, ]

# ---- Aggregering til kvartaler ----
# Hver måned placeres i sit kvartal. Kvartalet får datoen på kvartalets første dag
Forventninger$Kvartal <- as.Date(cut(Forventninger$TID, "quarter"))

# Antal måneder med data i hvert kvartal (3 = helt kvartal, 1-2 = delvist)
maaneder_i_kv <- aggregate(TID ~ Kvartal, data = Forventninger, FUN = function(d) length(unique(d)))
maaneder_i_kv <- setNames(maaneder_i_kv$TID, format(maaneder_i_kv$Kvartal))

# Gennemsnit af value for hver kombination af kvartal og indikator
KV_lang <- aggregate(value ~ Kvartal + INDIKATOR, data = Forventninger, FUN = mean)

# ---- Fra lang til bred tabel: én række pr. kvartal, én kolonne pr. indikator ----
KV <- reshape(KV_lang, idvar = "Kvartal", timevar = "INDIKATOR", direction = "wide")
names(KV) <- sub("^value\\.", "", names(KV))   # reshape sætter "value." foran kolonnenavnene
KV <- KV[order(KV$Kvartal), ]                  # sikrer kronologisk rækkefølge


# =====================================================================
# 3. SPØRGSMÅL FRA FORBRUGERTILLIDSUNDERSØGELSEN
# =====================================================================

# ---- Udtræk spørgsmålene og saml dem i F_total ----
# Kvartalsdatoen bruges som rækkenavn, så vi senere kan matche mod forbrugsdata
F_total <- data.frame(row.names = format(KV$Kvartal))

for (sp in spoergsmaal) {
  kolonne <- grep(paste0("^", sp, " "), colnames(KV))   # mellemrummet sikrer at F1 ikke matcher F10
  stopifnot(length(kolonne) == 1)                       # stop hvis spørgsmålet ikke findes præcis én gang
  F_total[[sp]] <- KV[[kolonne]]
}

# ---- Vend fortegn ----
stopifnot(all(vend_fortegn %in% spoergsmaal))
F_total[vend_fortegn] <- -F_total[vend_fortegn]


# =====================================================================
# 4. KENDTE INDIKATORER
# =====================================================================

# Spørgsmålene skal stå i samme rækkefølge som i F_total (F2, F3, ..., F10, F12, F13)
kendte <- c(MCCI       = "F2+F3+F9+F10",           # Artiklens mikrospørgsmål
            EU_CCI     = "F3+F5+F8+F12",           # Europas indikator
            Alle_mikro = "F2+F3+F9+F10+F12+F13",   # Alle mikrospørgsmål
            DI         = "F2+F4+F9+F10",
            DST        = "F2+F3+F4+F5+F9")


# =====================================================================
# 5. ALLE KOMBINATIONER AF INDIKATORER
# =====================================================================

# ---- Generér alle kombinationer og beregn gennemsnit for hver ----
indikator_liste <- data.frame(row.names = rownames(F_total))

for (antal in 1:ncol(F_total)) {
  kombinationer <- combn(colnames(F_total), antal, simplify = FALSE)   # alle kombinationer med 'antal' spørgsmål
  
  for (kombi in kombinationer) {
    navn <- paste(kombi, collapse = "+")   # navn = de spørgsmål der indgår, fx "F4+F10+F12"
    indikator_liste[[navn]] <- as.numeric(rowMeans(F_total[, kombi, drop = FALSE]))
  }
}

# Dobbelttjek: en tilfældig kombination genberegnes manuelt. Skal give TRUE
test_navn <- sample(colnames(indikator_liste), 1)
forventet <- rowMeans(F_total[, strsplit(test_navn, "+", fixed = TRUE)[[1]], drop = FALSE])
all.equal(indikator_liste[[test_navn]], as.numeric(forventet))

# ---- Klargør forbrugsdata og match mod indikatorerne ----
Forbrug <- data.frame(realvaekst = Forbrugsdata$Forbrug_vaekst,
                      row.names = format(Forbrugsdata$Kvartal))

# Kvartaler hvor vi har både indikatorer og forbrugsdata (i kronologisk orden)
faelles_kv <- intersect(rownames(F_total), rownames(Forbrug))

faelles <- data.frame(Kvartal = as.Date(faelles_kv),
                      realvaekst = Forbrug[faelles_kv, "realvaekst"],
                      indikator_liste[faelles_kv, , drop = FALSE],
                      check.names = FALSE, row.names = NULL)


# =====================================================================
# 6. DEN BEDSTE KOMBINATION AF INDIKATORER
# =====================================================================

# Alle kolonner undtagen Kvartal og realvaekst er indikatorer
indikator_kolonner <- setdiff(colnames(faelles), c("Kvartal", "realvaekst"))
M <- as.matrix(faelles[, indikator_kolonner])
y <- faelles$realvaekst

# Korrelation og R^2 for alle indikatorer
korrelation <- cor(M, y)[, 1]
forklaringsgrader <- data.frame(korrelation = korrelation, R = korrelation^2)

# Sortér fra bedst til dårligst og gem de bedste
rangeret <- forklaringsgrader[order(forklaringsgrader$R, decreasing = TRUE), ]
top_indikatorer <- rangeret[1:antal_top, ]


# =====================================================================
# 7. SAMMENLIGNING AF INDIKATORER
# =====================================================================

if (!all(kendte %in% rownames(forklaringsgrader))) {
  stop("En kendt indikator findes ikke. Tjek navne og at spørgsmålene står i samme rækkefølge som i F_total")
}

# Kendte indikatorer og de bedste fra søgningen i samme tabel
top_navne  <- rownames(top_indikatorer)
alle_navne <- c(kendte, setNames(top_navne, top_navne))

Kendte_indikatorer <- forklaringsgrader[unname(alle_navne), ]
rownames(Kendte_indikatorer) <- names(alle_navne)
Kendte_indikatorer

# Hvor ligger DI i forhold til de bedste?
forklaringsgrader[kendte[["DI"]], ]
which(rownames(rangeret) == kendte[["DI"]])    # DI's placering (1 = bedst)


# =====================================================================
# 8. FORUDSIGELSER AF DE BEDSTE MODELLER
# =====================================================================

# ---- Vælg indikatorer: de bedste + DI ----
navne <- unique(c(rownames(rangeret)[1:antal_modeller], kendte[["DI"]]))

# ---- Kvartaler der mangler forbrugsdata = de næste kvartaler der skal forudsiges ----
# Findes automatisk. Der er ingen kvartaler eller årstal i koden
nye_kv <- setdiff(rownames(F_total), faelles_kv)
if (length(nye_kv) == 0) stop("Ingen nye kvartaler at forudsige")

# ---- Loop: estimér en model pr. indikator og forudsig alle de nye kvartaler ----
forudsagte_vaerdier <- data.frame()

for (navn in navne) {
  # Eget dataframe, så navnet på variablen i modellen altid er 'indikator'
  data <- data.frame(realvaekst = faelles$realvaekst,
                     indikator  = indikator_liste[faelles_kv, navn])
  model <- lm(realvaekst ~ indikator, data = data)
  
  # Indikatorens værdier for de nye kvartaler
  ny_data <- data.frame(indikator = indikator_liste[nye_kv, navn])
  
  # Er indikatoren en af de kendte (fx DI)?
  kendt_navn <- names(kendte)[match(navn, kendte)]
  if (is.na(kendt_navn)) kendt_navn <- ""
  
  forudsagte_vaerdier <- rbind(
    forudsagte_vaerdier,
    data.frame(Indikator = navn,
               Kendt_navn = kendt_navn,
               Kvartal = nye_kv,
               Maaneder = as.integer(maaneder_i_kv[nye_kv]),   # hvor mange måneder forudsigelsen bygger på
               R2 = forklaringsgrader[navn, "R"],
               Forudsagt = as.numeric(predict(model, newdata = ny_data)))
  )
}

forudsagte_vaerdier <- forudsagte_vaerdier[order(forudsagte_vaerdier$Kvartal, -forudsagte_vaerdier$R2), ]
forudsagte_vaerdier


# =====================================================================
# 9. STABILITET AF RANGERING OVER TID
# =====================================================================

# Find rækken for startkvartalet og tjek at den findes
start_raekke <- which(faelles$Kvartal == test_start)
stopifnot(length(start_raekke) == 1)
antal_KV <- nrow(faelles) - start_raekke   # antal kvartaler der fjernes bagfra, i alt

# ---- Loop: beregn R^2 og rang for alle indikatorer, når ét kvartal fjernes ad gangen ----
alle_kvartaler <- data.frame()

for (i in 1:antal_KV) {
  data_cut <- nrow(faelles) - i   # fjern ét kvartal mere for hver iteration
  
  R_2      <- cor(M[1:data_cut, ], y[1:data_cut])[, 1]^2
  sorteret <- sort(R_2, decreasing = TRUE)   # bedst først
  
  samlet <- data.frame(Rang = 1:length(sorteret),
                       Indikator = names(sorteret),
                       R_vaerdi = unname(sorteret),
                       Fjernede_kvartaler = i)
  alle_kvartaler <- rbind(alle_kvartaler, samlet)
}

# ---- Gennemsnitlig rang, R^2 og antal gange i top N ----
gennemsnitlig_rangering <- aggregate(cbind(Rang, R_vaerdi) ~ Indikator,
                                     data = alle_kvartaler, FUN = mean)

# 1 hvis indikatoren var i top N i den periode, ellers 0. Summen giver antal gange (0 hvis aldrig, ingen NA)
alle_kvartaler$I_top <- as.integer(alle_kvartaler$Rang <= top_graense)
top_antal <- aggregate(I_top ~ Indikator, data = alle_kvartaler, FUN = sum)
gennemsnitlig_rangering <- merge(gennemsnitlig_rangering, top_antal, by = "Indikator")

colnames(gennemsnitlig_rangering) <- c("Indikator", "Gennemsnitlig_rang", "Gennemsnitlig_R2",
                                       paste("Antal gange i top", top_graense))

gennemsnitlig_rangering <- gennemsnitlig_rangering[order(gennemsnitlig_rangering$Gennemsnitlig_rang), ]
head(gennemsnitlig_rangering, 20)