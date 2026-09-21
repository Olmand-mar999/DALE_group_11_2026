library(readxl)
library(tidyr)
library(dplyr)
library(zoo)
library(ggplot2)
options(scipen = 999)

# Få tillidsindikatoren ind
ftillid <- read_excel("Forbrugerforventninger_nettotal.xlsx", 
                                           skip = 1)
ftillid_ny <- as.data.frame(t(ftillid))
names(ftillid_ny) <- c("Periode", "Tillidsindikator")
ftillid_ny <- ftillid_ny[2:nrow(ftillid_ny),]

# Giver alle værdierne en tilhørende time series der starter fra 2000 1. måned
ftillidts <- ts(
  as.numeric(ftillid_ny$Tillidsindikator), start = c(2000,1), frequency = 12
)

# Samler og giver alle værdierne tilhørende kvartaler
ftillidq <- aggregate(ftillidts, nfrequency = 4, FUN = mean)

# Perioden samler time series delen
# Tillidsindikatoren tager værdierne gemt for hver time series punkt.
ftillid_df <- data.frame(
  Periode = as.yearqtr(time(ftillidq)),
  Tillidsindikator = as.numeric(ftillidq)
)

# Få privatforbruget ind
p_forbrug <- read_excel("Forbrugsudgift.privatforbrug2.xlsx", 
                        skip = 1)
p_forbrug_ny <- as.data.frame(t(p_forbrug))
p_forbrug_ny <- p_forbrug_ny[4:nrow(p_forbrug_ny),]

names(p_forbrug_ny) <- c("Periode", "Real_vækst")

p_forbrug_ny$Real_vækst <- as.numeric(p_forbrug_ny$Real_vækst)

## Grunden til at der skal være 4 NA'er, er at funktionen returner
# 4 værdier mindre end der er i datasættet, så der der nødt til at blive lavet
# 4 tomme rækker så den ikke giver fejl
# Der er indhentet data startende fra 1999 i stedet for 2000,
# da jeg skal bruge 1999 til at regne tilbage for at finde værdierne for 2000.
p_forbrug_ny$Real_vækst <- c(
  rep(NA, 4),
  diff(log(p_forbrug_ny$Real_vækst), lag = 4) * 100
)

p_forbrug_ny <- p_forbrug_ny[5:nrow(p_forbrug_ny),]
## Laver forbrug til kvartaler så det matcher tillidsindikatoren
p_forbrug_ny$Periode <- ts(p_forbrug_ny$Periode, start = c(2000,1), frequency = 4)
## Laver forbrug til ts fra 2000 af
p_forbrug_ny$Periode <- as.yearqtr(time(p_forbrug_ny$Periode))

collected_data <- merge(ftillid_df, p_forbrug_ny, by = "Periode")

cor_model <- cor(collected_data$Real_vækst, 
                 collected_data$Tillidsindikator)

lm_model <- lm(collected_data$Real_vækst ~ 
                 collected_data$Tillidsindikator,
               data = collected_data)
cor_model
summary(lm_model)


## Graf tyvstjålet fra chatgpt
ggplot(collected_data, aes(x = Periode)) +
  
  # Årlig realvækst i privatforbrug
  geom_col(
    aes(y = Real_vækst * 3),
    fill = "#0099CC",
    width = 0.20
  ) +
  
  # Forbrugertillidsindikator
  geom_line(
    aes(y = Tillidsindikator),
    color = "#555555",
    linewidth = 1.1
  ) +
  
  # Venstre akse
  scale_y_continuous(
    name = "Nettotal",
    
    # Højre akse
    sec.axis = sec_axis(
      ~ . / 3,
      name = "Årlig realvækst i privatforbrug, pct."
    )
  ) +
  
  # X-akse
  scale_x_yearqtr(
    format = "%y",
    n = 11
  ) +
  
  labs(
    title = "Forbrugertillid og årlig realvækst i privatforbruget",
    x = NULL
  ) +
  
  theme_minimal() +
  theme(
    plot.title = element_text(
      size = 16,
      face = "bold"
    ),
    axis.title.y = element_text(
      color = "#555555"
    ),
    axis.title.y.right = element_text(
      color = "#0099CC"
    ),
    axis.text.x = element_text(
      angle = 0
    ),
    panel.grid.minor = element_blank(),
    legend.position = "none"
  )
