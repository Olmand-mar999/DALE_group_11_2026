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

# Barplot over gennemsnitlig realvækst
ggplot(mean_df, aes(x = Land, y = gennemsnit)) +
  geom_col(fill = "steelblue") +
  labs(
    title = "Gennemsnitlig realvækst i husholdningernes forbrug",
    x = "Land",
    y = "Gennemsnitlig realvækst (%)"
  ) +
  theme_minimal()


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

# Barplot over gennemsnitlig realvækst
ggplot(mean_coronafri_df, aes(x = Land, y = gennemsnit)) +
  geom_col(fill = "orange") +
  labs(
    title = "Gennemsnitlig realvækst i husholdningernes forbrug",
    x = "Land",
    y = "Gennemsnitlig realvækst (%)"
  ) +
  theme_minimal()

# Opg 5.4-----------------------------------------------------------------------

# data under coronaperiden gemmes
realvaekst_corona <- realvaekst[(datacut_2020:datacut_2022),]

mean_corona <- colMeans(realvaekst_corona[,-1])

mean_corona_df <- data.frame(Land = names(realvaekst_corona)[-1],
                                gennemsnit = round(mean_corona,2))

# Barplot over gennemsnitlig realvækst
ggplot(mean_corona_df, aes(x = Land, y = gennemsnit)) +
  geom_col(fill = "orange") +
  labs(
    title = "Gennemsnitlig realvækst i husholdningernes forbrug",
    x = "Land",
    y = "Gennemsnitlig realvækst (%)"
  ) +
  theme_minimal()


library(tidyr)
library(forcats)

library(zoo)

corona_slut <- as.Date(as.yearqtr(realvaekst$Tid[datacut_2022], format = "%Y-Q%q"), 
                       frac = 1)

corona_start <- tilDato(realvaekst$Tid[datacut_2020])
corona_slut  <- tilDato(realvaekst$Tid[datacut_2022])

# Samlet tabel med de tre gennemsnit pr. land (join på Land, ikke på rækkefølge)
gns <- mean_df |>
  rename(hele = gennemsnit) |>
  left_join(rename(mean_coronafri_df, uden = gennemsnit), by = "Land") |>
  left_join(rename(mean_corona_df,    under = gennemsnit), by = "Land")

# Graf 1 (5.1 + 5.3): Tidsserie pr. land med corona-perioden og gennemsnittene ----
lang <- realvaekst |>
  pivot_longer(-Tid, names_to = "Land", values_to = "vaekst") |>
  mutate(dato = tilDato(Tid))

ggplot(lang, aes(dato, vaekst)) +
  annotate("rect", xmin = corona_start, xmax = corona_slut,
           ymin = -Inf, ymax = Inf, alpha = 0.15) +
  geom_hline(yintercept = 0, linewidth = 0.3) +
  geom_line(colour = "grey30") +
  geom_hline(data = gns, aes(yintercept = hele,  linetype = "Hele perioden"),
             colour = "firebrick") +
  geom_hline(data = gns, aes(yintercept = uden, linetype = "Uden corona"),
             colour = "steelblue") +
  facet_wrap(~Land) +
  labs(x = NULL, y = "Årlig realvækst i privatforbrug (pct.)",
       linetype = "Gennemsnit",
       title = "Realvækst pr. kvartal, med corona-perioden skraveret")

# Graf 2 (5.2): Højeste gennemsnitlige vækst, hele perioden ----
ggplot(gns, aes(fct_reorder(Land, hele), hele)) +
  geom_col(fill = "orange") +
  geom_text(aes(label = round(hele, 2)), hjust = -0.1) +
  coord_flip() +
  labs(
    x = NULL,
    y = "Gns. årlig realvækst (pct.)",
    title = "Gennemsnitlig realvækst, hele perioden",
    caption = "Kilde: Eurostat, namq_10_fcs og egne beregninger"
  ) +
  theme_minimal() +
  theme(
    panel.background = element_rect(fill = "white", colour = NA),
    plot.background = element_rect(fill = "white", colour = NA)
  )

# Graf 3 (5.3): Coronaens effekt på gennemsnittet ----

effekt <- gns |> mutate(effekt = uden - hele)   # positiv = corona trak gennemsnittet ned

ggplot(effekt, aes(fct_reorder(Land, effekt), effekt)) +
  geom_col(fill = "orange") +
  geom_text(
    aes(
      label = round(effekt, 2),
      hjust = ifelse(effekt < 0, 1.1, -0.1)
    )
  ) +
  coord_flip() +
  labs(
    x = NULL,
    y = "Forskel i gns. realvækst (procentpoint)",
    title = "Ændring i gennemsnit hvis corona (2020-Q1 - 2022-Q2) fratrækkes perioden",
    caption = "Kilde: Eurostat, namq_10_fcs og egne beregninger"
  ) +
  theme_minimal() +
  theme(
    panel.background = element_rect(fill = "white", colour = NA),
    plot.background = element_rect(fill = "white", colour = NA)
  )



# Graf 4 (5.4): Gennemsnit uden vs. under corona ----
ggplot(gns, aes(y = fct_reorder(Land, under))) +
  geom_vline(xintercept = 0, linewidth = 0.3) +
  geom_segment(aes(x = uden, xend = under, yend = Land), colour = "grey60") +
  geom_point(aes(x = uden,  colour = "Uden corona"),  size = 3) +
  geom_point(aes(x = under, colour = "Under corona"), size = 3) +
  labs(x = "Gns. årlig realvækst (pct.)", y = NULL, colour = NULL,
       title = "Gennemsnitlig realvækst uden og under corona")

# Graf: Gennemsnitlig realvækst under corona
ggplot(mean_corona_df, aes(fct_reorder(Land, gennemsnit), gennemsnit)) +
  geom_col(fill = "orange") +
  geom_text(
    aes(
      label = round(gennemsnit, 2),
      hjust = ifelse(gennemsnit < 0, 1.1, -0.1)
    )
  ) +
  coord_flip() +
  labs(
    x = NULL,
    y = "Gns. årlig realvækst (pct.)",
    title = "Gennemsnitlig realvækst under corona (2020-Q1 - 2022-Q2)",
    caption = "Kilde: Eurostat, namq_10_fcs og egne beregninger"
  ) +
  theme_minimal() +
  theme(
    panel.background = element_rect(fill = "white", colour = NA),
    plot.background = element_rect(fill = "white", colour = NA)
  )
