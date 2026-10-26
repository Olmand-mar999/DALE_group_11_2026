# =====================================================================
# 0. OPSÆTNING
# =====================================================================
library(dkstat)
library(pls)
options(scipen = 999)

# ---- Indstillinger ----
tidszone         <- "Europe/Copenhagen"
startdato        <- as.Date("2000-01-01")   # første kvartal der bruges (forbrug og forventninger)
Q_pr_aar <- 4                       # bruges til årlig vækst
antal_top <- 5 # hvor mange af de bedste indikatorer vi gemmer

# Spørgsmål der indgår i alle kombinationer
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


# Kendte indikatorer
kendte <- c(MCCI       = "F2+F3+F9+F10",           # Artiklens mikrospørgsmål
            EU_CCI     = "F3+F5+F8+F12",           # Europas indikator
            Alle_mikro = "F2+F3+F9+F10+F12+F13",   # Alle mikrospørgsmål
            DI         = "F2+F4+F9+F10",
            DST        = "F2+F3+F4+F5+F9")

# =====================================================================
# 1. FORBRUGSDATA
# =====================================================================

# ---- Hentning af forbrugsdata ----
Forbrugsdata <- dst_get_data("NKN1", query = list(TRANSAKT = "*", PRISENHED = "*", SÆSON = "*", Tid = "*"))

##### Måske skal P31S14D eller P31S1MD bruges #####
# Husholdningernes forbrugsudgifter (P31S14D), kædede værdier i mia. kr. (LKV_M), sæsonkorrigeret (Y)
Forbrugsdata <- Forbrugsdata[grepl("^P31S14D", Forbrugsdata$TRANSAKT) &
                               grepl("^LKV_M ", Forbrugsdata$PRISENHED) &
                               grepl("^Y ", Forbrugsdata$SÆSON), ]

# ---- Årlig realvækst pr. kvartal ----
# Forbruget som almindelig vektor og antal kvartaler
x <- Forbrugsdata$value
n <- length(x)

# Vækst i pct. i forhold til samme kvartal året før
Forbrugsdata_vaekst <- (x[(Q_pr_aar + 1):n] / x[1:(n - Q_pr_aar)] - 1) * 100

# Saml kvartalsdato og vækst i en tabel
Forbrugsdata <- data.frame(Kvartal = as.Date(Forbrugsdata$TID[(Q_pr_aar + 1):n], tz = tidszone),
                           Forbrug_vaekst = Forbrugsdata_vaekst)

# Behold kun rækker fra og med startdato
Forbrugsdata <- Forbrugsdata[Forbrugsdata$Kvartal >= startdato, ]


# =====================================================================
# 2. FORBRUGERTILLIDSUNDERSØGELSEN (FORVENTNINGER)
# =====================================================================

# ---- Hentning ----
Forventninger <- dst_get_data("FORV1", query = list(INDIKATOR = "*", Tid = "*"))

# ---- Tid og afgrænsning ----
# Tidskolonne sættes til datoformat
Forventninger$TID <- as.Date(Forventninger$TID, tz = tidszone)

# Behold kun rækker fra og med 1. januar 2000
Forventninger <- Forventninger[Forventninger$TID >= startdato, ]

# ---- Aggregering til kvartaler ----
# Placér hver måned i sit kvartal. Kvartalet får datoen på kvartalets første dag
Forventninger$Kvartal <- as.Date(cut(Forventninger$TID, "quarter"))

# Beregn gennemsnittet af value for hver kombination af kvartal og indikator
KV_lang <- aggregate(value ~ Kvartal + INDIKATOR, data = Forventninger, FUN = mean)

# ---- Fra lang til bred tabel ----
# Gør tabellen "bred": én række pr. kvartal og én kolonne pr. indikator
KV <- reshape(KV_lang, idvar = "Kvartal", timevar = "INDIKATOR", direction = "wide")

# # reshape sætter "value." foran alle kolonnenavne, så det fjernes her
names(KV) <- sub("^value\\.", "", names(KV))


# =====================================================================
# 3. SPØRGSMÅL FRA FORBRUGERTILLIDSUNDERSØGELSEN
# =====================================================================

# ---- Udtræk af de enkelte spørgsmål ----

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

# ---- Vend fortegn ----
# Spørgsmål hvor "stigning" er dårligt, og som derfor vendes (priser og arbejdsløshed)
F6 <- F6 * (-1)
F7 <- F7 * (-1)
F8 <- F8 * (-1)

# ---- Saml spørgsmålene ----
# Saml alle spørgsmål i et dataframe
F_total <- data.frame(
  F2 = F2,
  F3 = F3,
  F4 = F4,
  F5 = F5,
  F6 = F6,
  F7 = F7,
  F8 = F8,
  F9 = F9,
  F10 = F10,
  F12 = F12,
  F13 = F13
)

# Brug kvartalsdatoen som rækkenavn, så vi senere kan matche mod forbrugsdata
rownames(F_total) <- format(KV$Kvartal)


# =====================================================================
# 4. DATA TIL REGRESSION
# =====================================================================

Forbrug <- data.frame(realvaekst = Forbrugsdata$Forbrug_vaekst,
                      row.names = format(Forbrugsdata$Kvartal))

# Gemmer rownames for alle kvartaler der er i både spørgsmålene og forbruget
faelles_kv <- intersect(rownames(F_total), rownames(Forbrug))

# Gemmer rownames for alle kvartaler der skal forudsiges
nye_kv     <- setdiff(rownames(F_total), faelles_kv)

# Dataframe der samler realvæksten og spørgsmålene
faelles <- data.frame(Kvartal = as.Date(faelles_kv),
                      realvaekst = Forbrug[faelles_kv, "realvaekst"],
                      F_total[faelles_kv, ],
                      row.names = NULL)

# =====================================================================
# 5. LINEÆRE OG MULTIPLE REGRESSIONER FOR ALLE SPØRGSMÅL
# =====================================================================

# Definer spørgsmål
alle_sp <- setdiff(names(faelles), c("Kvartal", "realvaekst"))
beholdte <- alle_sp

# ---- Lineær regression for hvert enkelt spørgsmål ----
enkelt <- data.frame()
for (sp in alle_sp) {
  s <- summary(lm(reformulate(sp, response = "realvaekst"), data = faelles))
  enkelt <- rbind(enkelt, data.frame(
    Spoergsmaal = sp,
    Koefficient = s$coefficients[2, "Estimate"],
    p_vaerdi    = s$coefficients[2, "Pr(>|t|)"],
    R2          = s$r.squared
  ))
}
enkelt

# ---- Multipel lineær regression ----
multi <- lm(realvaekst ~ ., data = faelles[, -1])
summary(multi)

# Signifikans niveau
signifikans <- 0.05

# Bliver ved med at fjerne insignifikante spørgsmål indtil alle tilbageværende er signifikante
repeat {
  # multipel regression laves med de spørgsmål der beholdes
  multi_ren <- lm(
    realvaekst ~ .,
    data = faelles[, c("realvaekst", beholdte)]
  )
  # gemmer alle p-værdierne
  p_vaerdier <- summary(multi_ren)$coefficients[-1, "Pr(>|t|)"]
  
  # Stop, hvis alle tilbageværende spørgsmål er signifikante
  if (all(p_vaerdier < signifikans)) break
  
  beholdte <- setdiff(beholdte, names(which.max(p_vaerdier)))
}

beholdte            # Spørgsmål, der er tilbage
summary(multi_ren)  # Resultat af den endelige regression


# =====================================================================
# 6. PCA REGRESSION
# =====================================================================

# Lav PCA fit [, -1] fjerner Kvartal
# Scale skal være TRUE for at den ikke prøver at lave samme skala for realvæksten og spørgsmålene
# . betyder alle andre variable end realvækst i faelles[, -1]
pcr.fit <- pcr(realvaekst ~ ., data = faelles[, -1], scale = TRUE)
summary(pcr.fit)

# =====================================================================
# 7. VÆGTE
# =====================================================================

vaegte <- loadings(pcr.fit)[, 1]^2          # kvadrerede loadings på 1. hovedkomponent
sort(vaegte, decreasing = TRUE)             # alle vægte, højest først
sort(vaegte, decreasing = TRUE)[1:3]        # de tre højeste

# =====================================================================
# 8. FORUDSIGELSE
# =====================================================================

# Vægtet indikator = sum af (vægt x svar), og forbrugsvæksten regresseres på den
# %*% laver matrix multiplikation og tager summen af alle rækker efter multiplikationen
faelles_ind <- data.frame(realvaekst = faelles$realvaekst,
                          indikator  = as.numeric(as.matrix(faelles[, names(vaegte)]) %*% vaegte))
model <- lm(realvaekst ~ indikator, data = faelles_ind)

ny <- data.frame(indikator = as.numeric(as.matrix(F_total[nye_kv, names(vaegte)]) %*% vaegte))

forudsagt <- data.frame(Kvartal = nye_kv, Forudsagt = predict(model, newdata = ny, ncomp = 1))
forudsagt

# =====================================================================
# 9. OPGAVE 2 – BEREGNINGER OG GRAFER
# =====================================================================


library(ggplot2)

# ---- Farver, tema og hjælpefunktion (samme stil som opgave 1) ----
groen_moerk <- "#1B5E20"
groen_mid   <- "#558B2F"
groen_lys   <- "#8BC34A"
groen_bleg  <- "#C5E1A5"
graa        <- "#8C8C8C"

tema_opgave <- theme_minimal(base_size = 12) +
  theme(plot.title            = element_text(face = "bold", size = 14),
        plot.subtitle         = element_text(colour = "grey30", margin = margin(b = 10)),
        plot.title.position   = "plot",
        plot.caption          = element_text(hjust = 0, colour = "grey40", size = 9,
                                             margin = margin(t = 10)),
        plot.caption.position = "plot",
        panel.grid.minor      = element_blank(),
        panel.grid.major.x    = element_blank(),
        axis.title            = element_text(colour = "grey30"),
        plot.margin           = margin(12, 16, 12, 12))

fmt <- function(x, d = 2) format(round(x, d), nsmall = d, decimal.mark = ",")

# Loadings hentes direkte fra PCA-modellen
loadings.pcr.fit <- pcr.fit$loadings

# Spørgsmålene i PCA-regressionen hentes direkte fra modellen (virker med 5 eller 11 spørgsmål)
pca_sp  <- rownames(loadings.pcr.fit)
n_sp    <- length(pca_sp)
sp_navn <- if (setequal(pca_sp, c("F2", "F3", "F4", "F5", "F9"))) "DST's fem spørgsmål" else paste0("de ", n_sp, " spørgsmål")
sp_liste <- paste0(paste(head(pca_sp, -1), collapse = ", "), " og ", tail(pca_sp, 1))
kilde   <- "Kilde: Danmarks Statistik, statistikbanken.dk (NKN1 og FORV1) samt egne beregninger"

# ---------------------------------------------------------------------
# 9. Graf 6: Forklaret variation pr. komponent
# ---------------------------------------------------------------------
ve_x <- as.numeric(explvar(pcr.fit))                                    # % af variationen i spørgsmålene
r2_y <- as.numeric(drop(R2(pcr.fit, estimate = "train", intercept = FALSE)$val)) * 100  # kumulativ % af realvæksten

g6 <- data.frame(komp = factor(seq_along(ve_x)), x = ve_x, y = r2_y)

graf6 <- ggplot(g6, aes(x = komp)) +
  geom_col(aes(y = x), fill = groen_bleg, width = 0.6) +
  geom_text(aes(y = x, label = paste0(fmt(x, 0), " %")), vjust = -0.5, size = 3.5, colour = groen_mid) +
  geom_line(aes(y = y, group = 1), colour = groen_moerk, linewidth = 1.1) +
  geom_point(aes(y = y), colour = groen_moerk, size = 3) +
  geom_text(aes(y = y, label = paste0(fmt(y, 0), " %"),
                hjust = ifelse(komp == "1", -0.35, 0.5),      # 1. etiket til højre, så den ikke rammer linjen
                vjust = ifelse(komp == "1", 0.5, -1)),
            size = 3.5, colour = groen_moerk, fontface = "bold") +
  scale_y_continuous(name = "Pct.", limits = c(0, ceiling(max(ve_x, r2_y) * 1.15 / 10) * 10),
                     breaks = seq(0, 100, 10), expand = expansion(mult = c(0, 0))) +
  labs(title = paste0("1. komponent samler ", fmt(ve_x[1], 0), " pct. af variationen i ", sp_navn,
                      " og forklarer ", fmt(r2_y[1], 0), " pct. af realvæksten"),
       subtitle = paste0("Søjler: andel af variationen i spørgsmålene pr. komponent.\n",
                         "Linje: samlet andel af realvæksten, der forklares med 1, 2, … komponenter."),
       x = "Komponent",
       caption = paste0("Anm.: PCA-regression af den årlige realvækst i husholdningernes forbrugsudgift ",
                        "på ", sp_liste, " (standardiserede).\n", kilde)) +
  tema_opgave + 
  theme(plot.background  = element_rect(fill = NA, colour = NA),
        legend.background = element_rect(fill = NA, colour = NA),
        legend.box.background = element_rect(fill = NA, colour = NA))

print(graf6)
#ggsave(
#  "graf6.png",
#  plot = graf6,
#  bg = "transparent",
#  width = 12,
#  height = 7,
#  dpi = 300
#)


# ---------------------------------------------------------------------
# 9. Graf 7: Vægte på 1. komponent
# ---------------------------------------------------------------------
load1 <- loadings.pcr.fit[, 1]
if (sum(load1) < 0) load1 <- -load1        # komponentens retning er vilkårlig: vend, så de fleste er positive
vaegte <- data.frame(sp = names(load1), loading = as.numeric(load1), vaegt = as.numeric(load1)^2)
vaegte <- vaegte[order(vaegte$vaegt), ]
vaegte$sp <- factor(vaegte$sp, levels = vaegte$sp)
top3   <- rev(as.character(vaegte$sp))[1:3]
vaegte$farve <- ifelse(vaegte$sp %in% top3, groen_moerk, groen_bleg)
print(vaegte[order(-vaegte$vaegt), c("sp", "loading", "vaegt")])   # tabel til opgaven

sp_tekst <- c(F2  = "F2 Familiens økonomi vs. for et år siden",
              F3  = "F3 Familiens økonomi om et år",
              F4  = "F4 Danmarks økonomi vs. for et år siden",
              F5  = "F5 Danmarks økonomi om et år",
              F6  = "F6 Priser vs. for et år siden (vendt)",
              F7  = "F7 Priser om et år (vendt)",
              F8  = "F8 Arbejdsløshed om et år (vendt)",
              F9  = "F9 Fordelagtigt at købe forbrugsgoder nu",
              F10 = "F10 Køb af forbrugsgoder næste 12 mdr.",
              F12 = "F12 Regner med at kunne spare op",
              F13 = "F13 Familiens økonomi lige nu")
lab_fun <- function(x) ifelse(x %in% names(sp_tekst), sp_tekst[x], x)

graf7 <- ggplot(vaegte, aes(x = vaegt, y = sp)) +
  geom_col(fill = vaegte$farve, width = 0.65) +
  geom_vline(xintercept = 1 / n_sp, colour = graa, linetype = "dashed", linewidth = 0.4) +
  geom_text(aes(label = paste0(fmt(vaegt * 100, 0), " %", ifelse(loading < 0, " (negativ loading)", ""))),
            hjust = -0.1, size = 3.4) +
  scale_y_discrete(labels = lab_fun) +
  scale_x_continuous(name = "Vægt (kvadreret loading på 1. komponent)",
                     labels = function(v) paste0(fmt(v * 100, 0), " %"),
                     expand = expansion(mult = c(0, 0.3))) +
  labs(title = paste0(paste(top3, collapse = ", "), " har de højeste vægte i PCA-indikatoren"),
       subtitle = paste0("Vægtene summerer til 100 pct. Mørkegrøn: de tre spørgsmål med højest vægt.\n",
                         "Stiplet linje: vægten ved lige vægte (", fmt(100 / n_sp, 0), " pct.)."),
       y = NULL,
       caption = paste0("Anm.: Vægt = loading² på 1. komponent i PCA-regressionen. ",
                        "F6, F7 og F8 har vendt fortegn.\n", kilde)) +
  tema_opgave +
  theme(panel.grid.major.y = element_blank(),
        panel.grid.major.x = element_line(colour = "grey90"),
        panel.background = element_rect(fill = NA, colour = NA),
        plot.background  = element_rect(fill = NA, colour = NA),
        legend.background = element_rect(fill = NA, colour = NA),
        legend.box.background = element_rect(fill = NA, colour = NA))

print(graf7)
#ggsave(
#  "graf7.png",
#  plot = graf7,
#  bg = "transparent",
#  width = 12,
#  height = 7,
#  dpi = 300
#)



# ---------------------------------------------------------------------
# 9. PCA-modellen: test af 2025K3 + forudsigelse af de nyeste kvartaler + Graf 8
# ---------------------------------------------------------------------
kv_navn <- function(d) {
  d <- as.Date(d)
  paste0(format(d, "%Y"), "K", (as.numeric(format(d, "%m")) - 1) %/% 3 + 1)
}

# Vægte = loading², men med loadingens fortegn. Ellers ville et spørgsmål med negativ
# loading tælle med den forkerte vej i indikatoren.
w <- (load1^2 * sign(load1))[pca_sp]

# PCA-indikator = vægtet sum af spørgsmålene (alle kvartaler, også dem uden forbrugsdata)
pca_ind <- data.frame(Kvartal    = as.Date(rownames(F_total)),
                      indikator  = as.numeric(as.matrix(F_total[, pca_sp]) %*% w),
                      realvaekst = Forbrug[rownames(F_total), "realvaekst"])

# ---- Model B: PCA-modellen på alle data til nyeste kvartal ----
data_alle <- pca_ind[!is.na(pca_ind$realvaekst), ]
model_pca <- lm(realvaekst ~ indikator, data = data_alle)
summary(model_pca)

pca_ind$forudsagt <- as.numeric(predict(model_pca, newdata = pca_ind))
sidste_kv <- max(data_alle$Kvartal)                       # seneste kvartal med forbrugsdata
nye       <- pca_ind[pca_ind$Kvartal > sidste_kv, ]       # kvartaler der forudsiges
nye_pi    <- predict(model_pca, newdata = nye, interval = "prediction")
print(data.frame(Kvartal = kv_navn(nye$Kvartal), PCA_indikator = round(nye$indikator, 1),
                 Forudsagt = round(nye_pi[, "fit"], 1),
                 Nedre_95 = round(nye_pi[, "lwr"], 1), Oevre_95 = round(nye_pi[, "upr"], 1)))

# Gennemsnitlig fejl (faktisk minus forudsagt) før og efter 2022
data_alle$fejl <- data_alle$realvaekst - as.numeric(predict(model_pca, newdata = data_alle))
fejl_foer  <- mean(data_alle$fejl[data_alle$Kvartal <  as.Date("2022-01-01")])
fejl_efter <- mean(data_alle$fejl[data_alle$Kvartal >= as.Date("2022-01-01")])
cat("Gns. fejl før 2022:", fmt(fejl_foer, 1), " – fra 2022:", fmt(fejl_efter, 1), "procentpoint\n")

titel8 <- if (fejl_efter > 1) {
  "PCA-modellen undervurderer forbruget systematisk efter 2022"
} else if (fejl_efter < -1) {
  "PCA-modellen overvurderer forbruget systematisk efter 2022"
} else {
  "PCA-modellen følger forbruget også efter 2022"
}

# Data til grafen
hist_df  <- pca_ind[pca_ind$Kvartal <= sidste_kv, ]
frem_df  <- rbind(pca_ind[pca_ind$Kvartal == sidste_kv, ], nye)   # stiplet linje fra sidste kendte kvartal
nye_lab  <- nye[nrow(nye), ]
slut     <- max(pca_ind$Kvartal)

graf8 <- ggplot(hist_df, aes(x = Kvartal)) +
  annotate("rect", xmin = as.Date("2022-01-01"), xmax = slut + 60,
           ymin = -Inf, ymax = Inf, fill = "grey95") +
  geom_col(aes(y = realvaekst), fill = groen_bleg, width = 80, na.rm = TRUE) +
  geom_hline(yintercept = 0, colour = "grey40", linewidth = 0.3) +
  geom_line(aes(y = forudsagt), colour = groen_moerk, linewidth = 1.1) +
  # Forudsigelse af de nyeste kvartaler (stiplet)
  geom_line(data = frem_df, aes(y = forudsagt), colour = groen_moerk,
            linewidth = 1.1, linetype = "dashed") +
  geom_point(data = nye, aes(y = forudsagt), colour = groen_moerk, size = 3.5) +
  # Etiket med alle forudsagte kvartaler, placeret over punkterne med hvid baggrund
  annotate("label", x = nye_lab$Kvartal, y = nye_lab$forudsagt + 1.5,
           label = paste(paste0(kv_navn(nye$Kvartal), ": ", fmt(nye$forudsagt, 1), " pct."), collapse = "\n"),
           hjust = 1, vjust = 0, colour = groen_moerk, fontface = "bold", size = 3.4,
           fill = "white") +
  # Testen af 2025K3 (model estimeret til og med 2025K2)
  scale_x_date(date_breaks = "2 years", date_labels = "%Y", expand = expansion(mult = c(0.01, 0.02))) +
  scale_y_continuous(name = "Årlig realvækst, pct.") +
  labs(title = titel8,
       subtitle = paste0("Søjler: faktisk årlig realvækst. Linje: forudsagt med PCA-modellen, stiplet = ",
                         paste(kv_navn(nye$Kvartal), collapse = " og "), ".\n",
                         "Gns. fejl (faktisk minus forudsagt): ", fmt(fejl_foer, 1),
                         " procentpoint før 2022 og ", fmt(fejl_efter, 1), " procentpoint fra 2022 (gråt felt)."),
       x = NULL,
       caption = paste0("Anm.: PCA-indikatoren er en vægtet sum af de ", n_sp, " spørgsmål med vægtene fra graf 7. ",
                        "Linjen er estimeret på 2000K1–", kv_navn(sidste_kv), ".\n",
                        kilde)) +
  tema_opgave +
  theme(panel.background = element_rect(fill = NA, colour = NA),
        plot.background  = element_rect(fill = NA, colour = NA),
        legend.background = element_rect(fill = NA, colour = NA),
        legend.box.background = element_rect(fill = NA, colour = NA))

print(graf8)
#ggsave(
#  "graf8.png",
#  plot = graf8,
#  bg = "transparent",
#  width = 12,
#  height = 7,
#  dpi = 300
#)
