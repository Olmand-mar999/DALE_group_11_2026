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

alle_sider_fra_Bilbasen_VW <- readRDS("bilbasen_alle_sider.rds")

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

#saveRDS(bilbasen_df, "bilbasen.rds")

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

as24_sider <- readRDS("autoscout24_alle_sider.rds")

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


# ============================================================
# 3. Hentning af tilbud fra power.dk/kampagne/aktuelt/
# ============================================================

power_sesh <- ChromoteSession$new()
power_sesh$Page$navigate("https://www.power.dk/kampagne/aktuelt/")
Sys.sleep(8)

raa <- power_sesh$Runtime$evaluate("
(async () => {
  const urls = [...new Set(performance.getEntriesByType('resource')
    .map(e => e.name)
    .filter(n => n.includes('/api/v2/products?ids=')))];
  const svar = [];
  for (const u of urls) svar.push(await (await fetch(u)).text());
  return JSON.stringify({urls: urls, svar: svar});
})()", awaitPromise = TRUE, returnByValue = TRUE, timeout_ = 60)$result$value

power_sesh$close()
saveRDS(raa, "power_data.rds")

res  <- jsonlite::fromJSON(raa)
cols <- c("productId","title","price","previousPrice","categoryName","url")
dele <- lapply(res$svar, function(s) { d <- jsonlite::fromJSON(s); if (length(d)) d[, cols] })
power_df <- do.call(rbind, dele)
power_df <- power_df[!duplicated(power_df$productId), ]


# ============================================================
# 4. Forsøgt webscraping af power
# ============================================================

#base <- "https://www.power.dk/kampagne/aktuelt/"
#browser_sesh <- ChromoteSession$new()

#browser_sesh$Page$navigate(base)

#Sys.sleep(10)

#data <- browser_sesh$Runtime$evaluate("document.documentElement.outerHTML")$result$value

#browser_sesh$close()

#side <- read_html(data)

# h2, h3 og h4 er alle underoverskrifter
#side %>%
#  html_elements("h2, h3, h4") %>%
#  html_text2()

