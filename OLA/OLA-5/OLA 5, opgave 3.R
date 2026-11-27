###############################################################################
#  OLA 5 – Opgave 3: Spørgeskema og Cumulative Link Models
#  Gruppe 11 – Ellen, Emma, Marcus og Yusuf
#
#  Spørgsmål: "Hvordan ser du mulighederne for at låne penge til din virksomhed?"
#
#  Scriptet er delt op i:
#    0. Opsætning (pakke, farver, indlæsning af data)
#    1. Opgave 3.1 – søjlediagram over svarene
#    2. Opgave 3.2 – Cumulative Link Model (CLM)
#    3. Opgave 3.3 – svarene opdelt efter en forklarende variabel
#
#  Alt er skrevet i base R. Den eneste ekstra pakke er 'ordinal', som
#  indeholder funktionen clm() til Cumulative Link Models.
###############################################################################


# =============================================================================
# 0. OPSÆTNING
# =============================================================================

# ---- Pakke ------------------------------------------------------------------
# install.packages("ordinal")   # kør kun én gang (husk anførselstegn!)
library(ordinal)                # skal køres hver gang R startes


# ---- Farver (kun brun og lyserød) -------------------------------------------
# Negative svar = brune nuancer, positive svar = lyserøde nuancer.
# Navnene gør, at hver svarkategori altid får samme farve i alle figurer.
farver <- c("Meget dårlige" = "#4A2F26",   # mørkebrun
            "Dårlige"       = "#8A5444",   # mellembrun
            "Neutrale"      = "#C9A397",   # lys brun-rosa
            "Gode"          = "#E8B4B0",   # lyserød
            "Meget gode"    = "#C77A7E",   # dyb rosa
            "Ved ikke"      = "#CDB4AA")   # lys gråbrun

# Kildeangivelse, som skrives nederst på alle plots
kilde <- paste0("Kilde: Egne beregninger på data fra ",
                "'regnskaber_industri_transport_byg_5_25000_ansatte_anonym' ",
                "(spørgsmålet er fiktivt)")

# Tekstfarve inde i søjlerne: hvid på mørke farver, brun på lyse farver
tekstfarve <- c("Meget dårlige" = "white",
                "Dårlige"       = "white",
                "Neutrale"      = "#3B2620",
                "Gode"          = "#3B2620",
                "Meget gode"    = "white")


# ---- Hjælpefunktion: baggrund med firkanter (gitter) ------------------------
# Tegner lyserøde gitterlinjer på hvid baggrund, så plottet får "firkanter"
# bagved. Kaldes efter et tomt plot og før søjlerne tegnes.
baggrund <- function(antal_lodrette = 12, farve = "#EBC9C5") {
  u <- par("usr")                                   # plottets kanter: x1, x2, y1, y2
  abline(h = axTicks(2), col = farve, lwd = 1)      # vandrette linjer ved y-akse-tallene
  abline(v = seq(u[1], u[2], length.out = antal_lodrette + 1),
         col = farve, lwd = 1)                      # lodrette linjer
}

# ---- Indlæs data ------------------------------------------------------------
# read.csv2() passer til danske CSV-filer:
#   - kolonner adskilt med semikolon (;)
#   - decimalkomma (fx 47,64)
# fileEncoding = "latin1" sørger for, at æ, ø og å bliver læst korrekt.
# check.names = FALSE beholder de originale kolonnenavne med mellemrum.
#
# Findes filen ikke i R's arbejdsmappe (se getwd()), åbner der et vindue,
# hvor man selv klikker på CSV-filen.
fil <- "regnskaber_industri_transport_byg_5_25000_ansatte_anonym.csv"
if (!file.exists(fil)) {
  message("CSV-filen blev ikke fundet i ", getwd(), " – vælg den i vinduet")
  fil <- file.choose()
}

df <- read.csv2(fil, fileEncoding = "latin1", check.names = FALSE)

dim(df)   # antal rækker (virksomheder) og kolonner (variable): 4484 x 228


# ---- Svarvariablen ----------------------------------------------------------
# Spørgsmålet ligger i datasættets første kolonne.
# trimws() fjerner eventuelle mellemrum før/efter teksten.
svar_raa <- trimws(df[[1]])

# Se hvilke svar der findes, og hvor mange af hver
table(svar_raa, useNA = "ifany")

# 22 virksomheder har svaret "Dårlig" i stedet for "Dårlige".
# Det er samme svar, så de slås sammen:
svar_raa[svar_raa == "Dårlig"] <- "Dårlige"

# Den rigtige rækkefølge fra dårligst til bedst.
# "Ved ikke" er ikke med, fordi det ikke hører hjemme på skalaen.
niveauer <- c("Meget dårlige", "Dårlige", "Neutrale", "Gode", "Meget gode")


# ---- Forklarende variable ---------------------------------------------------
analyse_aar <- 2026   # vi ser på virksomhederne fra 2026

# OBS: Datasættets nyeste regnskaber er fra 2021, men 2021-tallene mangler
# for ca. 89 % af virksomhederne. Derfor bruges regnskabstal fra 2020.

# Etableringsår: datoen står som "dd-mm-åååå" -> lav om til dato -> tag året
etab_aar <- as.numeric(format(as.Date(df$Etableringsdato, format = "%d-%m-%Y"), "%Y"))

# Branchekoden er 6 cifre. De første 2 cifre fortæller hovedbranchen.
# sprintf("%06d", ...) sikrer foranstillede nuller, fx 25110 -> "025110"
branche2 <- as.numeric(substr(sprintf("%06d", as.integer(df[["Branchekode primær"]])), 1, 2))

# Inddel i sektorer ud fra branchekoden (Danmarks Statistiks DB07):
#   10-33 = Industri, 41-43 = Byggeri, 49-53 = Transport, resten = Øvrige
sektor <- rep("Øvrige", nrow(df))
sektor[branche2 >= 10 & branche2 <= 33] <- "Industri"
sektor[branche2 >= 41 & branche2 <= 43] <- "Byggeri"
sektor[branche2 >= 49 & branche2 <= 53] <- "Transport"

# Saml de variable, vi skal bruge, i én ny og overskuelig data.frame
data <- data.frame(
  svar_raa  = svar_raa,
  soliditet = df[["Soliditetsgrad 2020 (%)"]],    # egenkapital / aktiver i %
  afkast    = df[["Afkastningsgrad 2020 (%)"]],   # resultat / aktiver i %
  ansatte   = df[["Antal ansatte Cvr-nr."]],
  alder     = analyse_aar - etab_aar,             # virksomhedens alder i 2026
  sektor    = sektor
)

summary(data)   # hurtigt overblik: min, max, median og manglende værdier (NA)


# =============================================================================
# 1. OPGAVE 3.1 – FORDELINGEN AF SVARENE
# =============================================================================

# factor() med levels sikrer, at søjlerne kommer i den rigtige rækkefølge
# (ellers ville R sortere dem alfabetisk).
antal  <- table(factor(data$svar_raa, levels = c(niveauer, "Ved ikke")))
andele <- prop.table(antal)     # antal -> andele (summer til 1)

round(100 * andele, 1)          # andele i procent med 1 decimal

# Plottet vises i RStudios Plots-vindue.
# Gem det evt. via Export -> Save as Image i Plots-vinduet.
par(mar = c(5.5, 5, 5, 1))      # margener: (bund, venstre, top, højre)

# barplot() returnerer søjlernes x-positioner (bp), så vi kan sætte tekst over
# Trin 1: tomt plot (usynlige søjler), så akserne bliver sat op
barplot(100 * andele, col = NA, border = NA, axes = FALSE, names.arg = "",
        ylim = c(0, 55))
# Trin 2: baggrund med firkanter
baggrund()
# Trin 3: de rigtige søjler tegnes oven på baggrunden (add = TRUE)
bp <- barplot(100 * andele,
              add    = TRUE,
              col    = farver,  # farve pr. søjle
              border = NA,      # ingen kant om søjlerne
              ylim   = c(0, 55),
              las    = 1,       # vandrette tal på y-aksen
              ylab   = "Andel af besvarelser (%)",
              cex.names = 0.9)  # lidt mindre tekst under søjlerne
title(ylab = "Andel af besvarelser (%)")   # y-aksens titel

# Procent og antal over hver søjle (decimal.mark = "," giver dansk komma)
text(bp, 100 * andele, pos = 3, offset = 0.4,   # pos = 3: teksten placeres lige over søjlen
     labels = paste0(format(round(100 * andele, 1), decimal.mark = ","),
                     " %\n(n = ", antal, ")"),
     cex = 0.8, col = "#3B2620")

# Titel og undertitel (adj = 0 = venstrestillet)
title("Hvordan ser du mulighederne for at låne penge til din virksomhed?",
      adj = 0, line = 2.6, cex.main = 1.1, col.main = "#3B2620")
mtext(paste0("Virksomheder i industri, transport og byggeri med 5+ ansatte (n = ",
             sum(antal), ")"),
      side = 3, adj = 0, line = 1.2, cex = 0.85, col = "#6B4334")

# Kilde nederst til venstre
mtext(kilde, side = 1, adj = 0, line = 4, cex = 0.7, col = "#6B4334")


# =============================================================================
# 2. OPGAVE 3.2 – CUMULATIVE LINK MODEL
# =============================================================================
# En CLM bruges, når svaret er ORDNET (har en rækkefølge), men afstanden
# mellem kategorierne ikke er et tal. Modellen beregner, hvordan de
# forklarende variable påvirker oddsen for at svare i en mere positiv kategori.

# ---- Hjælpefunktion: trim ekstreme værdier ---------------------------------
# Nogle nøgletal er meget ekstreme (fx soliditet på -1000 %). For at enkelte
# outliers ikke skal styre modellen, sættes alt under 1 %-percentilen til
# 1 %-percentilen og alt over 99 %-percentilen til 99 %-percentilen.
# (Det kaldes winsorisering.)
trim <- function(x, p = c(0.01, 0.99)) {
  q <- quantile(x, p, na.rm = TRUE)   # find 1 %- og 99 %-percentilen
  pmin(pmax(x, q[1]), q[2])           # klip værdier udenfor
}


# ---- Klargør data til modellen ---------------------------------------------
# Behold kun svar på skalaen (dvs. fjern "Ved ikke")
model_data <- data[data$svar_raa %in% niveauer, ]

# ordered = TRUE fortæller R, at kategorierne har en rækkefølge
model_data$svar <- factor(model_data$svar_raa, levels = niveauer, ordered = TRUE)

model_data$soliditet <- trim(model_data$soliditet)
model_data$afkast    <- trim(model_data$afkast)

# Log af antal ansatte: forskellen på 5 og 10 ansatte betyder mere end
# forskellen på 1000 og 1005. Log gør effekten "pr. procent større".
model_data$log_ansatte <- log(model_data$ansatte)

# Byggeri sættes som referencegruppe, så de andre sektorer sammenlignes med den
model_data$sektor <- relevel(factor(model_data$sektor), ref = "Byggeri")

# Fjern virksomheder med manglende værdier i de variable, vi bruger
model_data <- model_data[complete.cases(model_data[, c("soliditet", "afkast", "alder")]), ]
nrow(model_data)   # antal virksomheder i modellen: 4423


# ---- Valg af forklarende variable (inspireret af artiklerne) ---------------
#  - Soliditetsgrad: høj egenkapitalandel = mindre risiko for banken
#  - Afkastningsgrad: virksomheder der tjener penge er mere kreditværdige
#  - Størrelse (log antal ansatte): større virksomheder har bedre adgang
#  - Alder: ældre virksomheder har længere historik over for banken
#  - Sektor: industri, transport og byggeri


# ---- Byg modellerne trin for trin ------------------------------------------
# link = "logit" giver en ordinal logistisk regression (proportional odds)
m0 <- clm(svar ~ 1, data = model_data, link = "logit")            # nulmodel (ingen variable)
m1 <- clm(svar ~ soliditet, data = model_data, link = "logit")
m2 <- clm(svar ~ soliditet + afkast, data = model_data, link = "logit")
m3 <- clm(svar ~ soliditet + afkast + log_ansatte + alder + sektor,
          data = model_data, link = "logit")                       # fuld model

summary(m3)
# Sådan læses summary():
#  - Coefficients: positiv Estimate = mere positivt svar (bedre lånemuligheder)
#  - Pr(>|z|): p-værdi. Under 0,05 = signifikant effekt
#  - Threshold coefficients: "skillelinjerne" mellem svarkategorierne


# ---- Sammenlign modellerne --------------------------------------------------
# anova() laver likelihood-ratio tests: forbedrer hver ny model den forrige?
anova(m0, m1, m2, m3)

# AIC: jo lavere, jo bedre model (straffer for mange variable)
AIC(m0, m1, m2, m3)

# drop1(): hvad sker der, hvis vi fjerner én variabel ad gangen?
# Ikke-signifikant p-værdi = variablen bidrager ikke og kan fjernes.
drop1(m3, test = "Chi")   # afkastningsgrad er ikke signifikant (p = 0,17)


# ---- Endelig model: uden afkastningsgrad -----------------------------------
m4 <- clm(svar ~ soliditet + log_ansatte + alder + sektor,
          data = model_data, link = "logit")

anova(m4, m3)   # ingen signifikant forskel -> den simple model foretrækkes
summary(m4)


# ---- Odds ratios ------------------------------------------------------------
# exp(koefficient) = odds ratio (OR)
#   OR > 1: højere værdi giver større odds for et mere positivt svar
#   OR < 1: højere værdi giver mindre odds for et mere positivt svar
# confint() giver 95 %-konfidensintervaller. Indeholder intervallet ikke 1,
# er effekten signifikant.
round(exp(cbind(OR = coef(m4)[names(m4$beta)], confint(m4))), 3)
# Eksempel på fortolkning:
#   soliditet OR = 1,007 -> hvert procentpoint højere soliditet øger oddsen
#   for et mere positivt svar med ca. 0,7 %


# ---- Test af modellens antagelse -------------------------------------------
# CLM antager "proportional odds": at en variabel har samme effekt ved alle
# svartrin (fx både fra Dårlige -> Neutrale og fra Gode -> Meget gode).
# Signifikant p-værdi = antagelsen holder IKKE for den variabel.
nominal_test(m4)   # antagelsen holder ikke for log_ansatte


# ---- Udvidet model: partial proportional odds ------------------------------
# nominal = ~ log_ansatte lader størrelse have sin egen effekt ved hvert
# svartrin, mens de øvrige variable stadig har én fælles effekt.
m5 <- clm(svar ~ soliditet + alder + sektor, nominal = ~ log_ansatte,
          data = model_data, link = "logit")

anova(m4, m5)   # signifikant -> m5 passer bedre til data
summary(m5)


# ---- Forudsagte sandsynligheder ---------------------------------------------
# Hvad er sandsynligheden for hvert svar for en "typisk" byggevirksomhed
# med forskellig størrelse? (øvrige variable sat til medianen)
nye_data <- data.frame(
  soliditet   = median(model_data$soliditet),
  log_ansatte = log(c(5, 10, 25, 50, 100, 250)),
  alder       = median(model_data$alder),
  sektor      = factor("Byggeri", levels = levels(model_data$sektor))
)

pred <- predict(m4, newdata = nye_data, type = "prob")$fit
round(cbind(ansatte = exp(nye_data$log_ansatte), pred), 3)
# Fx: 5 ansatte -> 6 % "Meget gode", 250 ansatte -> 68 % "Meget gode"


# =============================================================================
# 3. OPGAVE 3.3 – FORKLARENDE VARIABEL VS. SVAR
# =============================================================================

# ---- Hjælpefunktion: stablet 100 %-søjlediagram ----------------------------
# Funktionen tegner ét diagram, hvor hver søjle er en gruppe (fx antal
# ansatte), og farverne viser, hvordan svarene fordeler sig i gruppen.
# Den bruges to gange nedenfor, så koden ikke skal skrives to gange.
stablet_plot <- function(gruppe, svar, titel, undertitel, xlab) {
  
  tab <- table(svar, gruppe)                    # rækker = svar, kolonner = grupper
  pct <- 100 * prop.table(tab, margin = 2)      # margin = 2: procent inden for hver gruppe
  navn <- paste0(colnames(tab), "\n(n = ", colSums(tab), ")")   # gruppenavn + antal
  
  par(mar = c(6.5, 5, 5, 9), xpd = TRUE)          # stor højre margen til forklaringen
  
  # Tomt plot -> baggrund med firkanter -> søjlerne oven på (add = TRUE)
  barplot(pct, col = NA, border = NA, axes = FALSE, names.arg = rep("", ncol(pct)),
          space = 0.4)
  par(xpd = FALSE)          # baggrunden må ikke gå ud over plotområdet
  baggrund()
  par(xpd = TRUE)           # tekst og forklaring må gerne stå i margenen
  
  bp <- barplot(pct,
                add       = TRUE,
                col       = farver[rownames(pct)],
                border    = "white",
                las       = 1,
                names.arg = navn,
                ylab      = "Andel af virksomheder (%)",
                cex.names = 0.85,
                space     = 0.4)                # afstand mellem søjlerne
  title(ylab = "Andel af virksomheder (%)")     # y-aksens titel
  mtext(xlab, side = 1, line = 3.6, col = "#3B2620")
  
  # Placér procenttal midt i hvert farvefelt (kun felter på mindst 4 %)
  midt <- apply(pct, 2, function(x) cumsum(x) - x / 2)
  for (j in seq_len(ncol(pct))) {
    vis <- pct[, j] >= 4
    text(bp[j], midt[vis, j], paste0(round(pct[vis, j]), " %"),
         col = tekstfarve[rownames(pct)[vis]], cex = 0.75)
  }
  
  # Forklaring (legend) til højre for diagrammet
  legend(par("usr")[2] + 0.2, 100,
         legend = rev(rownames(pct)),
         fill   = rev(farver[rownames(pct)]),
         border = NA, bty = "n",
         title  = "Lånemuligheder", cex = 0.8)
  
  title(titel, adj = 0, line = 2.6, cex.main = 1.1, col.main = "#3B2620")
  mtext(undertitel, side = 3, adj = 0, line = 1.2, cex = 0.85, col = "#6B4334")
  mtext(kilde, side = 1, adj = 0, line = 5.2, cex = 0.7, col = "#6B4334")   # kilde
  round(pct, 1)   # returnér procenttabellen, så den også kan ses i konsollen
}


# ---- Data til figurerne (uden "Ved ikke") ----------------------------------
med_svar <- data[data$svar_raa %in% niveauer, ]
svar_fak <- factor(med_svar$svar_raa, levels = niveauer)


# ---- Figur 3.3: svar efter antal ansatte -----------------------------------
# Størrelse er den stærkeste forklarende variabel i modellen.
# cut() deler antal ansatte op i grupper: (4,9] = 5-9 ansatte osv.
str_gruppe <- cut(med_svar$ansatte,
                  breaks = c(4, 9, 19, 49, 99, Inf),
                  labels = c("5-9", "10-19", "20-49", "50-99", "100+"))

stablet_plot(str_gruppe, svar_fak,
             "Jo større virksomhed, desto bedre ser den lånemulighederne",
             "Svarfordeling efter antal ansatte ('Ved ikke' udeladt)",
             "Antal ansatte")


# ---- Ekstra figur: svar efter soliditetsgrad -------------------------------
# right = FALSE: grænsen hører til gruppen ovenover (fx 20 % er i "20-40 %")
sol_gruppe <- cut(med_svar$soliditet,
                  breaks = c(-Inf, 20, 40, 60, Inf),
                  labels = c("Under 20 %", "20-40 %", "40-60 %", "Over 60 %"),
                  right  = FALSE)

stablet_plot(sol_gruppe, svar_fak,
             "Lånemuligheder efter soliditetsgrad",
             "Svarfordeling efter soliditetsgrad 2020 ('Ved ikke' udeladt)",
             "Soliditetsgrad (egenkapital / samlede aktiver)")

# Alle plots kan ses i Plots-vinduet – brug pilene til at bladre mellem dem.
# TIP: Gør Plots-vinduet bredt (eller klik Zoom), så forklaringen til højre kan ses.


# =============================================================================
# 4. SIDEQUEST – LÅNEMULIGHEDS-BEREGNER
# =============================================================================
# Funktionen bruger den endelige model (m4) til at forudsige, hvordan en
# virksomhed med de valgte egenskaber vil svare på spørgsmålet.
# Den returnerer sandsynligheden for hvert svar og tegner et søjlediagram.

laaneberegner <- function(ansatte, soliditet, alder, sektor = "Byggeri") {
  
  # 1) Lav en "ny virksomhed" med samme variabelnavne som i modellen
  ny <- data.frame(
    soliditet   = soliditet,
    log_ansatte = log(ansatte),            # modellen bruger log(ansatte)
    alder       = alder,
    sektor      = factor(sektor, levels = levels(model_data$sektor))
  )
  
  # 2) predict() giver sandsynligheden for hvert af de fem svar
  p <- predict(m4, newdata = ny, type = "prob")$fit[1, ]
  names(p) <- niveauer
  
  # 3) Tegn resultatet
  par(mar = c(5.5, 5, 5, 1), xpd = FALSE)
  barplot(100 * p, col = NA, border = NA, axes = FALSE, names.arg = "", ylim = c(0, 100))
  baggrund()
  bp <- barplot(100 * p, add = TRUE, col = farver[niveauer], border = NA,
                las = 1, ylim = c(0, 100), cex.names = 0.9)
  title(ylab = "Sandsynlighed (%)", xlab = "Forudsagt svar")
  text(bp, 100 * p, paste0(format(round(100 * p, 1), decimal.mark = ","), " %"),
       pos = 3, offset = 0.4, cex = 0.85, col = "#3B2620")
  title(paste0("Mest sandsynlige svar: ", niveauer[which.max(p)]),
        adj = 0, line = 2.6, cex.main = 1.1, col.main = "#3B2620")
  mtext(paste0(ansatte, " ansatte · soliditet ", soliditet, " % · ", alder,
               " år · ", sektor),
        side = 3, adj = 0, line = 1.2, cex = 0.85, col = "#6B4334")
  mtext(paste(kilde, "· model m4"), side = 1, adj = 0, line = 4.3, cex = 0.7, col = "#6B4334")
  
  round(p, 3)   # returnér sandsynlighederne
}

# ---- Prøv beregneren --------------------------------------------------------
laaneberegner(ansatte = 6,   soliditet = 25, alder = 18, sektor = "Byggeri")    # lille byggefirma
laaneberegner(ansatte = 250, soliditet = 55, alder = 45, sektor = "Transport")  # stor transportvirksomhed

# Interaktiv udgave med skydeknapper: se filen "beregner_app.R"
