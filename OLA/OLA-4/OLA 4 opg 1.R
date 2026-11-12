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


# ============================================================
# 1. SCRAPE autoscout24.de
# ============================================================

baseurl_as24 <- "https://www.autoscout24.de/lst/volkswagen/ft_elektro?cy=D&damaged_listing=exclude&desc=0&ocs_listing=include&powertype=kw&sort=standard&ustate=N%2CU&page="

#options(chromote.headless = FALSE)
#browser_sesh <- ChromoteSession$new()

#as24_sider <- list()

#for (i in 1:200) {
#  browser_sesh$Page$navigate(str_c(baseurl_as24, i, "&atype=C&search_id=222tiht5zxj&source=listpage_pagination"))
#  Sys.sleep(sample(3:7, 1))
#  as24_sider[[i]] <- browser_sesh$Runtime$evaluate("document.documentElement.outerHTML")$result$value
#  print(str_c("Side ", i, " hentet"))
#}

#browser_sesh$close()
saveRDS(as24_sider, "autoscout24_alle_sider.rds")

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
    saelger         = html_attr(biler, "data-seller-type"),      # "d" = forhandler, "p" = privat
    postnr          = html_attr(biler, "data-listing-zip-code"),
    forhandler      = html_text2(forhandler_a),
    forhandler_link = html_attr(forhandler_a, "href"),
    link            = paste0("https://www.autoscout24.de", html_attr(html_element(biler, "a"), "href")),
    stringsAsFactors = FALSE
  )
}

as24_df <- do.call(rbind, lapply(as24_sider, lav_df_as24))

as24_df$pris <- as24_df$pris_eur * 7.46


# ============================================================
# 2. Sammenligning mellen bilbasen og autoscout24
# ============================================================

# Gør modelnavnene ens: "VW ID.3" og "id.3" -> "id.3"
ens_model <- function(x) {
  x <- tolower(x)
  x <- sub("\n.*", "", x)                    # kun første linje
  x <- gsub("^(vw|volkswagen) ", "", x)      # fjern mærket
  gsub(" ", "", x)                           # fjern mellemrum
}

bb <- data.frame(kilde = "Bilbasen",    model = ens_model(bilbasen_df$model),
                 aargang = bilbasen_df$aargang, km = bilbasen_df$km, pris = bilbasen_df$pris)
as <- data.frame(kilde = "AutoScout24", model = ens_model(as24_df$model),
                 aargang = as24_df$aargang,     km = as24_df$km,     pris = as24_df$pris)

# Tjek at modelnavnene matcher
table(bb$model)
table(as$model)

# Gennemsnit og antal pr. model + årgang
bb_gns <- aggregate(cbind(pris, km) ~ model + aargang, data = bb, FUN = mean)
bb_gns$antal <- aggregate(pris ~ model + aargang, data = bb, FUN = length)$pris

as_gns <- aggregate(cbind(pris, km) ~ model + aargang, data = as, FUN = mean)
as_gns$antal <- aggregate(pris ~ model + aargang, data = as, FUN = length)$pris

# Side om side
sammenligning <- merge(bb_gns, as_gns, by = c("model", "aargang"),
                       suffixes = c("_bilbasen", "_as24"))

# Forskel i kroner og procent (positiv = dyrere i Danmark)
sammenligning$forskel_kr  <- round(sammenligning$pris_bilbasen - sammenligning$pris_as24)
sammenligning$forskel_pct <- round(100 * sammenligning$forskel_kr / sammenligning$pris_as24, 1)

sammenligning <- sammenligning[order(sammenligning$model, sammenligning$aargang), ]
View(sammenligning)

