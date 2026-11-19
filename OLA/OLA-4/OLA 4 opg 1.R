library(chromote)
library(rvest)
library(stringr)
library(dplyr)
library(logr)

# ============================================================
# 0. SCRAPE bilbasen.dk
# ============================================================

baseurl <- "https://www.bilbasen.dk/brugt/bil/vw?fuel=3&includeengroscvr=true&includeleasing=false&page="

#options(chromote.headless = FALSE)
#browser_sesh <- ChromoteSession$new()

#alle_sider <- list() 

#for (i in 1:136) {
#  side_url <- str_c(baseurl, i)
#  browser_sesh$Page$navigate(side_url)
#  Sys.sleep(sample(3:7,1,replace = F))
#  alle_sider[[i]] <- browser_sesh$Runtime$evaluate("document.documentElement.outerHTML")$result$value
#  print(str_c("Side ", i, " hentet"))
#}

#browser_sesh$close()
#saveRDS(alle_sider, "bilbasen_alle_sider.rds")

alle_sider_fra_Bilbasen_VW <- readRDS("html_fuckery/bilbasen_alle_sider.rds")

# Funktion der laver én dataframe ud fra én sides HTML
lav_df <- function(html_tekst) {
  side  <- read_html(html_tekst)
  biler <- html_elements(side, "article[class*='Listing_listing']")   # én pr. bil
  
  data.frame(
    model    = html_text2(html_element(biler, "[class*='Listing_makeModel']")),
    pris_txt = html_text2(html_element(biler, "[class*='Listing_price']")),
    detaljer = sapply(biler, function(b) {
      paste(html_text2(html_elements(b, "[class*='ListingDetails_listItem']")),
            collapse = " | ")
    }),
    lokation = html_text2(html_element(biler, "[class*='Listing_location']")),
    link     = html_attr(html_element(biler, "a"), "href"),
    logo     = sub("\\?.*", "", html_attr(html_element(biler, "img[class*='dealerLogoCard']"), "src")),
    stringsAsFactors = FALSE
  )
}

# Kør på alle sider og saml til én dataframe
df_liste <- lapply(alle_sider_fra_Bilbasen_VW, lav_df)
bilbasen_df <- do.call(rbind, df_liste)

# gem alle unikke biler
bilbasen_df <- unique(bilbasen_df)

# ID: tallet til sidst i linket ".../58-pro-performance-family-5d/7031939" -> 7031939
bilbasen_df$id <- as.numeric(sub(".*/([0-9]+).*", "\\1", bilbasen_df$link))

bilbasen_df <- distinct(bilbasen_df, id, .keep_all = TRUE)
bilbasen_df$scrapedate <- Sys.Date()

# Pris som tal: "249.900 kr." -> 249900
bilbasen_df$pris <- as.numeric(gsub("[^0-9]", "", bilbasen_df$pris_txt))

# Kilometer: tag tallet foran "km |" eller "km" sidst (ikke "km rækkevidde")
km_match <- regexpr("[0-9.]+ km( \\||$)", bilbasen_df$detaljer)
bilbasen_df$km <- NA
bilbasen_df$km[km_match != -1] <- as.numeric(gsub("[^0-9]", "",
                                                  regmatches(bilbasen_df$detaljer, km_match)))

# Årgang: "3/2019" -> 2019
bilbasen_df$aargang <- ifelse(grepl("[0-9]{1,2}/[0-9]{4}", bilbasen_df$detaljer),
                              as.numeric(sub(".*?[0-9]{1,2}/([0-9]{4}).*", "\\1",
                                             bilbasen_df$detaljer)),NA)

# Rækkevidde for elbiler: "420 km rækkevidde" -> 420
bilbasen_df$raekkevidde <- ifelse(grepl("[0-9]+ km rækkevidde", bilbasen_df$detaljer),
                                  as.numeric(sub(".*?([0-9]+) km rækkevidde.*", "\\1",
                                                 bilbasen_df$detaljer)),NA)

#saveRDS(bilbasen_df, "html_fuckery/bilbasen.rds")

print(alle_sider_fra_Bilbasen_VW[[6]])

# ============================================================
# 1. SCRAPE autoscout24.de
# ============================================================

baseurl_as24 <- "https://www.autoscout24.de/lst/volkswagen/ft_elektro?cy=D&damaged_listing=exclude&desc=0&ocs_listing=include&powertype=kw&sort=standard&ustate=N%2CU&page="

#options(chromote.headless = FALSE)
#browser_sesh <- ChromoteSession$new()

#as24_sider <- list()

#for (i in 1:200) {
#  as24side_url <- str_c(baseurl_as24, i, "&atype=C&search_id=222tiht5zxj&source=listpage_pagination")
#  browser_sesh$Page$navigate(as24side_url)
#  Sys.sleep(sample(3:7, 1))
#  as24_sider[[i]] <- browser_sesh$Runtime$evaluate("document.documentElement.outerHTML")$result$value
#  print(str_c("Side ", i, " hentet"))
#}

#browser_sesh$close()
#saveRDS(as24_sider, "autoscout24_alle_sider.rds")

as24_sider <- readRDS("html_fuckery/autoscout24_alle_sider.rds")

lav_df_as24 <- function(html_tekst) {
  side  <- read_html(html_tekst)
  biler <- html_elements(side, "article[data-guid]")             # én pr. bil
  
  forhandler_a <- html_element(biler, "a[href*='/haendler/']")   # link til forhandlerside
  
  data.frame(
    id              = html_attr(biler, "data-guid"),
    model           = html_attr(biler, "data-model"),
    pris_eur        = as.numeric(html_attr(biler, "data-price")),
    km              = as.numeric(html_attr(biler, "data-mileage")),
    aargang         = as.numeric(sub(".*-", "", html_attr(biler, "data-first-registration"))),  # "08-2023" -> 2023
    postnr          = html_attr(biler, "data-listing-zip-code"),
    forhandler      = html_text2(forhandler_a),
    forhandler_link = html_attr(forhandler_a, "href"),
    stringsAsFactors = FALSE
  )
}

# Kør på alle sider og saml til én dataframe
as24_liste <- lapply(as24_sider, lav_df_as24)
as24_df <- do.call(rbind, as24_liste)

# gem alle unikke biler
as24_df <- unique(as24_df)

# Gem prisen i danske kroner også
as24_df$pris_uden_afgift <- as24_df$pris_eur * 7.46

# Bestem afgiften. Rå afgift efter de almindelige trin (2026-satser)
raa_afgift <- function(pris) {
  0.25 * pmin(pris, 76400) +
    0.85 * pmax(0, pmin(pris, 237400) - 76400) +
    1.50 * pmax(0, pris - 237400) - 25500
}

# Elbil: kun den del af værdien over grænsen giver afgift, ganget med indfasningsprocenten
# indfas = 0.48, grænse = 366300
afgift_elbil <- function(pris) {
  0.48 * pmax(0, raa_afgift(pris) - raa_afgift(366300))
}

as24_df$afgift     <- afgift_elbil(as24_df$pris_uden_afgift)
as24_df$pris  <- as24_df$pris_uden_afgift + as24_df$afgift


# ============================================================
# 2. Sammenligning mellen bilbasen og autoscout24
# ============================================================

# sørg for modelnavnene er ens 
# bilbasen skriver "VW ID.3" og autoscout skriver "id.3" for samme bil
ens_model <- function(x) {
  x <- tolower(x)
  x <- sub("\n.*", "", x)                    # kun første linje
  x <- gsub("^(vw|volkswagen) ", "", x)      # fjern mærket
  x <- gsub(" ", "", x)                      # fjern mellemrum
}

bb <- data.frame(kilde = "Bilbasen",    model = ens_model(bilbasen_df$model),
                 aargang = bilbasen_df$aargang, km = bilbasen_df$km, pris = bilbasen_df$pris)
as <- data.frame(kilde = "AutoScout24", model = ens_model(as24_df$model),
                 aargang = as24_df$aargang,     km = as24_df$km,     pris = as24_df$pris)

# Gennemsnit og antal pr. model + årgang
bb_gns <- aggregate(cbind(pris, km) ~ model + aargang, data = bb, FUN = mean)
bb_gns$antal <- aggregate(pris ~ model + aargang, data = bb, FUN = length)$pris

as_gns <- aggregate(cbind(pris, km) ~ model + aargang, data = as, FUN = mean)
as_gns$antal <- aggregate(pris ~ model + aargang, data = as, FUN = length)$pris

# Side om side. Siden både bilbasen og autoscout har samme kolonne navne
# bruges suffixes til at give kolonnerne nye forskellige navne
sammenligning <- merge(bb_gns, as_gns, by = c("model", "aargang"),
                       suffixes = c("_bilbasen", "_as24"))

# Forskel i kroner og procent (positiv = dyrere i Danmark)
sammenligning$forskel_kr  <- round(sammenligning$pris_bilbasen - sammenligning$pris_as24)
sammenligning$forskel_pct <- round(100 * sammenligning$forskel_kr / sammenligning$pris_as24, 1)

library(ggplot2)
#Plot 1

moerk_lilla <- "#5b2a9e"   # dyrere i Danmark
lys_lilla   <- "#b89be6"   # billigere i Danmark
min_antal   <- 5

plot_data <- sammenligning %>%
  filter(antal_bilbasen >= min_antal, antal_as24 >= min_antal) %>%
  mutate(model = recode(model, "id.3" = "ID.3", "id.4" = "ID.4", "id.5" = "ID.5",
                        "id.7" = "ID.7", "id.buzz" = "ID. Buzz", "id.polo" = "ID. Polo"),
         vaegt = antal_bilbasen + antal_as24) %>%
  group_by(model) %>%
  summarise(forskel_pct = weighted.mean(forskel_pct, vaegt),
            forskel_kr  = weighted.mean(forskel_kr, vaegt),
            antal       = sum(vaegt)) %>%
  mutate(retning = ifelse(forskel_pct > 0, "Dyrere i Danmark", "Billigere i Danmark"),
         navn    = paste0(model, "  (", antal, " biler)"),
         label   = paste0(sub("\\.", ",", sprintf("%+.1f", forskel_pct)), " %  (",
                          ifelse(forskel_kr > 0, "+", "−"),
                          format(abs(round(forskel_kr, -3)), big.mark = ".", decimal.mark = ","),
                          " kr.)"))

p <- ggplot(plot_data, aes(x = forskel_pct, y = reorder(navn, forskel_pct), fill = retning)) +
  geom_col(width = 0.65) +
  geom_vline(xintercept = 0, colour = "grey40") +
  geom_text(aes(label = label, hjust = ifelse(forskel_pct >= 0, -0.08, 1.08)), size = 3.6) +
  scale_fill_manual(values = c("Dyrere i Danmark" = moerk_lilla,
                               "Billigere i Danmark" = lys_lilla), name = NULL) +
  scale_x_continuous(labels = function(x) paste0(ifelse(x > 0, "+", ""), x, " %"),
                     expand = expansion(mult = 0.35)) +
  labs(title = "Brugte VW-elbiler er typisk dyrere i Danmark end i Tyskland",
       subtitle = paste0("Gns. prisforskel pr. model, vægtet efter antal biler. Tysk pris inkl. beregnet dansk afgift.\n",
                         "Kun model/årgang med mindst ", min_antal, " biler på begge sider."),
       x = "Prisforskel, Bilbasen vs. AutoScout24", y = NULL) +
  theme_minimal(base_size = 12) +
  theme(plot.title = element_text(face = "bold"),
        axis.text.y = element_text(face = "bold", colour = "black"),
        panel.grid.major.y = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "top")

p
ggsave("prisforskel.png", p, width = 10, height = 5.5, dpi = 300, bg = "white")

#PLOT 2

library(ggplot2)
library(dplyr)
library(scales)

moerk_lilla <- "#5b2a9e"   # Bilbasen (Danmark)
graa        <- "#a9a6b8"   # AutoScout24 (Tyskland)

valgt_model   <- "id.3"
valgt_aargang <- 2023

punkter <- samlet_el %>%
  filter(model == valgt_model, aargang == valgt_aargang, !is.na(km), !is.na(pris))

km_grid <- seq(0, quantile(punkter$km, 0.98), length.out = 100)
linjer <- expand.grid(km = km_grid,
                      kilde = factor(c("AutoScout24", "Bilbasen"), levels = levels(samlet_el$kilde)),
                      model = valgt_model, aargang = valgt_aargang)
linjer$pris <- predict(model_lm2, newdata = linjer)

forskel <- coef(model_lm2)["kildeBilbasen"]
pil_km  <- 90000
pil_y   <- predict(model_lm2, newdata = data.frame(
  km = pil_km, model = valgt_model, aargang = valgt_aargang,
  kilde = factor(c("AutoScout24", "Bilbasen"), levels = levels(samlet_el$kilde))))

kr  <- label_number(big.mark = ".", decimal.mark = ",", suffix = " kr.")
kmf <- label_number(big.mark = ".", decimal.mark = ",", suffix = " km")

p2 <- ggplot(punkter, aes(x = km, y = pris, colour = kilde)) +
  geom_point(alpha = 0.35, size = 1.6) +
  geom_line(data = linjer, linewidth = 1.6) +
  annotate("segment", x = pil_km, xend = pil_km, y = pil_y[1], yend = pil_y[2],
           arrow = arrow(ends = "both", length = unit(0.15, "cm")), colour = "black") +
  annotate("label", x = pil_km + 3000, y = mean(pil_y), hjust = 0, label.size = 0,
           label = paste0("+", kr(round(forskel, -2)), "\ni Danmark"), fontface = "bold", size = 4) +
  scale_colour_manual(values = c("Bilbasen" = moerk_lilla, "AutoScout24" = graa),
                      labels = c("Bilbasen" = "Bilbasen (Danmark)",
                                 "AutoScout24" = "AutoScout24 (Tyskland, inkl. afgift)"),
                      name = NULL) +
  scale_x_continuous(labels = kmf, limits = c(0, quantile(punkter$km, 0.98))) +
  scale_y_continuous(labels = kr) +
  labs(title = "Samme bil koster ca. 24.000 kr. mere i Danmark",
       subtitle = paste0("VW ID.3, årgang ", valgt_aargang,
                         ". Punkter = biler, linjer = regressionsmodellen."),
       x = "Kilometertal", y = "Pris") +
  theme_minimal(base_size = 12) +
  theme(plot.title = element_text(face = "bold"),
        panel.grid.minor = element_blank(),
        legend.position = "top")

p2
ggsave("regression_id3.png", p2, width = 10, height = 6, dpi = 300, bg = "white")

#plot 3
# ============================================================
# Figur 8: Merpris i Danmark ved forskellige indfasningsprocenter
# Kræver at følgende allerede findes fra scriptet:
#   as24_df, bb, raa_afgift(), ens_model()
# ============================================================


moerk_lilla <- "#5b2a9e"
lys_lilla   <- "#b89be6"

elbiler <- c("id.3", "id.4", "id.5", "id.7", "id.buzz", "e-up!", "id.polo")

# Andel af tyske biler, der betaler 0 kr. i afgift (forklarer figuren)
andel_0_afgift <- mean(as24_df$afgift == 0, na.rm = TRUE)
andel_0_afgift

# Afgift for elbil med valgfri indfasningsprocent
afgift_elbil_x <- function(pris, indfas) {
  indfas * pmax(0, raa_afgift(pris) - raa_afgift(366300))
}

# Kør regressionen for hver indfasningsprocent
indfas_vaerdier <- seq(0.40, 1.00, by = 0.04)

scenarier <- lapply(indfas_vaerdier, function(indfas) {
  as_x <- data.frame(kilde   = "AutoScout24",
                     model   = ens_model(as24_df$model),
                     aargang = as24_df$aargang,
                     km      = as24_df$km,
                     pris    = as24_df$pris_uden_afgift +
                       afgift_elbil_x(as24_df$pris_uden_afgift, indfas))
  data_x <- rbind(bb, as_x) %>% filter(model %in% elbiler)
  data_x$kilde <- relevel(factor(data_x$kilde), ref = "AutoScout24")
  fit <- lm(pris ~ km + factor(aargang) + model + kilde, data = data_x)
  data.frame(indfas     = indfas,
             merpris_dk = unname(coef(fit)["kildeBilbasen"]),
             lav        = confint(fit)["kildeBilbasen", 1],
             hoej       = confint(fit)["kildeBilbasen", 2])
})
scenarier <- do.call(rbind, scenarier)
scenarier

# Merpris i dag (48 %) og ved fuld afgift (100 %)
merpris_i_dag <- scenarier$merpris_dk[abs(scenarier$indfas - 0.48) < 0.001]
merpris_fuld  <- scenarier$merpris_dk[abs(scenarier$indfas - 1.00) < 0.001]
merpris_i_dag
merpris_fuld

kr <- label_number(big.mark = ".", decimal.mark = ",", suffix = " kr.")

p3 <- ggplot(scenarier, aes(x = indfas, y = merpris_dk)) +
  geom_hline(yintercept = 0, colour = "grey40") +
  geom_ribbon(aes(ymin = lav, ymax = hoej), fill = lys_lilla, alpha = 0.4) +
  geom_line(colour = moerk_lilla, linewidth = 1.4) +
  geom_point(data = data.frame(indfas = c(0.48, 1.00),
                               merpris_dk = c(merpris_i_dag, merpris_fuld)),
             colour = moerk_lilla, size = 3.5) +
  annotate("text", x = 0.48, y = merpris_i_dag, vjust = -1.3, fontface = "bold",
           label = paste0("I dag (48 %): ", kr(round(merpris_i_dag, -2)))) +
  annotate("text", x = 1.00, y = merpris_fuld, vjust = -1.3, hjust = 0.9, fontface = "bold",
           label = paste0("Fuld afgift: ", kr(round(merpris_fuld, -2)))) +
  scale_x_continuous(labels = percent, breaks = seq(0.4, 1, by = 0.1)) +
  scale_y_continuous(labels = kr, limits = c(0, NA)) +
  labs(title = "Import kan betale sig, selv hvis elbiler betaler fuld afgift",
       subtitle = paste0("Merpris i Danmark for samme bil (regression) ved forskellige indfasningsprocenter. ",
                         "Lilla bånd = 95 %-konfidensinterval.\n",
                         percent(andel_0_afgift, accuracy = 1),
                         " af de tyske biler ligger under bundfradraget og betaler derfor 0 kr. i afgift."),
       x = "Indfasningsprocent for registreringsafgift på elbiler",
       y = "Merpris i Danmark") +
  theme_minimal(base_size = 12) +
  theme(plot.title = element_text(face = "bold"),
        panel.grid.minor = element_blank())

p3
ggsave("figur8_afgift.png", p3, width = 10, height = 6, dpi = 300, bg = "white")
# ============================================================
# 3. Hentning af tilbud fra power.dk/kampagne/aktuelt/
# ============================================================

power_sesh <- ChromoteSession$new()
power_sesh$Page$navigate("https://www.power.dk/kampagne/aktuelt/")
Sys.sleep(8)

# Henter alt dataen - gør det via et api kald og java script
raa <- powersesh$Runtime$evaluate("
(async () => {
  const urls = [...new Set(performance.getEntriesByType('resource')
    .map(e => e.name)                                        // keep only the URL of each request
    .filter(n => n.includes('/api/v2/products?ids=')))];     // keep only the product API calls
  const svar = [];
  for (const u of urls) svar.push(await (await fetch(u)).text());  // fetch each reply as text
  return JSON.stringify({urls: urls, svar: svar});
})()", awaitPromise = TRUE, returnByValue = TRUE, timeout = 60)$result$value

power_sesh$close()
#saveRDS(raa, "html_fuckery/power_data.rds")

# kategori_tjek indeholder alle kategori navne
res  <- jsonlite::fromJSON(raa)
kategori_tjek <- lapply(res$svar, jsonlite::fromJSON)     # one data frame per API call

# cols indeholder alle vores valgte kategorinavne
cols <- c("productId","title","price","previousPrice","categoryName","url")
dele <- lapply(res$svar, function(s) { d <- jsonlite::fromJSON(s); if (length(d)) d[, cols] })
power_df <- do.call(rbind, dele)
power_df <- power_df[!duplicated(power_df$productId), ]


# ============================================================
# 4. Forsøgt webscraping af power
# ============================================================

base <- "https://www.power.dk/kampagne/aktuelt/"
browser_sesh <- ChromoteSession$new()
browser_sesh$Page$navigate(base)

Sys.sleep(10)

data <- browser_sesh$Runtime$evaluate("document.documentElement.outerHTML")$result$value

browser_sesh$close()

side <- read_html(data)

# Undersøg om klassen nu findes
str_detect(data, "product-item-arguments-container")
str_detect(data, "savings-label power-yellow-500 mobile-variant")
str_detect(data, "price-content h-100-p")

# sammenlign med om klasser kan findes for biler
biler <- readRDS("html_fuckery/bilbasen_alle_sider.rds")[1]
str_detect(biler, "Listing_makeModel__7yqgs")

# h2, h3 og h4 er alle underoverskrifter
side %>%
  html_elements("h2, h3, h4") %>%
  html_text2()

# Den sidste underoverskrift den henter, er verificer du er et menneske
