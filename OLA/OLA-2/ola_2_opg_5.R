# Opg. 5.1
library(stringr)
library(dplyr)
library(eurostat)
library(restatapi)
library(ggplot2)


allTabs <- get_eurostat_toc()
clean_restatapi_cache()

# Find det data vi har brug for
# filtrer efter household
datatabs <- allTabs |>
  filter(str_detect(title, regex("household", ignore_case = TRUE)) &
      str_detect(title, regex("consumption", ignore_case = TRUE)) &
        str_detect(title, regex("quarterly", ignore_case = TRUE))
  )

# Viser hvilke valgmuligheder der er for de forskellige kolonner
#forbrug <- get_eurostat_data(
#  id = "namq_10_fcs")
#unique(forbrug$geo)
#unique(forbrug$na_item)
#unique(forbrug$unit)
#unique(forbrug$s_adj)

forbrug <- get_eurostat_data(
  id = "namq_10_fcs",
  filters = list(
    geo = c("DK", "BE", "NL", "SE", "AT", "DE", "FR", "IT", "ES"),
    na_item = "P31_S14",  # P.31 kædede værdier
    unit = "CLV20_MNAC",  #	Chain-linked volume, millioner euro, referenceår 2020
    s_adj = "SCA"         # Seasonal and calendar adjusted
  ),
  #select_freq = "Q",
  #name = FALSE,
  stringsAsFactors = FALSE
)

# Gem data fra 1999 og frem
forbrug <- forbrug |>
  filter(as.character(time) >= "1999-Q1")

# Fjern kolonner vi ikke har brug for længere
forbrug <- forbrug[,-c(2,3,4)]

# Gør tabellen "bred": én række pr. kvartal og én kolonne pr. indikator
forbrug_wide <- reshape(forbrug, idvar = "time", timevar = "geo", direction = "wide")

# # reshape sætter "values." foran alle kolonnenavne, så det fjernes her
names(forbrug_wide) <- sub("^values\\.", "", names(forbrug_wide))

# Vækst i pct. for hvert kvartal i forhold til samme kvartal året før (4 kvartaler tidligere)
x <- forbrug_wide[,-1]
n <- nrow(x)
realvaekst <- (x[5:n,] / x[1:(n - 4)] - 1) * 100
realvaekst <- cbind(forbrug_wide$time[5:n], realvaekst)

# Sæt navn på tidskolonnen
names(realvaekst)[1] <- "Tid"

# Opg 5.2-----------------------------------------------------------------------

# Find den gennemsnitlige realvækst for alle
mean_data <- colMeans(realvaekst[,-1])

mean_df <- data.frame(Land = names(realvaekst)[-1],
                      gennemsnit = round(mean_data,2))


# Opg 5.3-----------------------------------------------------------------------

# Corona fjernes som en outlier. 
# Tidsperioden under corona har vi defineret ud fra artiklen
# Covid-19-epidemien i Danmark, 2020-2022 fra Danmarks nationalleksikon
datacut_2020 <- which(realvaekst$Tid == "2020-Q1")
datacut_2022 <- which(realvaekst$Tid == "2022-Q2")
realvaekst_coronafri <- realvaekst[-(datacut_2020:datacut_2022),]

mean_coronafri <- colMeans(realvaekst_coronafri[,-1])

mean_coronafri_df <- data.frame(Land = names(realvaekst_coronafri)[-1],
                      gennemsnit = round(mean_coronafri,2))

# Opg 5.4-----------------------------------------------------------------------

# data under coronaperiden gemmes
realvaekst_corona <- realvaekst[(datacut_2020:datacut_2022),]

mean_corona <- colMeans(realvaekst_corona[,-1])

mean_corona_df <- data.frame(Land = names(realvaekst_corona)[-1],
                                gennemsnit = round(mean_corona,2))
