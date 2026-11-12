
# Forstå din data og hvilken type den er----------------------------------------
typeof()
## Fortæller hvilken type data dit objekt er, string, int osv.
# Characters are strings, doubles are integers
class()
# Typeof fortæller datatypen, mens class fortæller hvad du arbejder med
# Altså fx om du har en matrice, et array en dataframe eller tibble
# Kan også bruges på en bestemt kolonne i et dataframe

Sys.time()
# Giver tid og data

structure(1:10)
str(1:10) 
# Giver overblik over data. str giver et kortere og mere overskueligt overblik
# over hvad din data indeholder


# Arrays----------------------------------------------------------

# Giver et array med tallene fra 1-100
liste <- 1:100

# Giver en liste fra 1-100 med et step på 5. Kan også bruges med kommatal
liste <- seq(1,100,5)
liste <- seq(1,100,0.5)
liste

# Gentag en talrække eller string et vidst antal gange
liste <- rep(1:5,5)
liste <- rep("ABC",5)

# Gentag hvert element i en række et bestemt antal gange
liste <- rep(1:5, each=5)
liste <- c(rep("A", each=5),rep("B", each=5),rep("C", each=5))

# opstil en tilfældig rækkefølge ud fra en liste
# Hvis det samme tal må blive gentaget tilføjes replace = T
# Hvis der skal tilføjes en probability tilføjes prob = probability liste
liste <- sample(1:100, 100)
liste <- sample(1:100, 5, replace = T)
prob_list <- c(0.9, 0.025, 0.025, 0.025, 0.025)
liste <- sample(1:5, 3, prob = prob_list, replace=T)

# Sorter array efter størrelse
liste <- c(1,7,5,2,8)
sort(liste)

# Returner værdierne i omvendt rækkefølge
liste <- c(1,7,5,2,8)
rev(liste)

# Hvor mange gange opstår hvert tal
liste <- c(1:11, 1:17, 9:13)
table(liste)

# Hvor mange unikke værdier eller strings er der i et array
liste <- c("apple", "apple", "Orange", "Orange", "banana")
unique(liste)


# Order-------------------------------------------------------------------------

# Brug dataframe fra dplyr

library(dplyr)
biler <- mtcars

# Sorter biler efter mpg
biler[order(biler$mpg),]
biler[order(biler$mpg, decreasing = TRUE),][1:5,]

# Dataframes--------------------------------------------------------------------

# Lav et dataframe
data <- data.frame(
  Kol_1 = 1:100,
  Kol_2 = sample(1:100,100),
  Kol_3 = sample(c(rep("Apple", each=5),
                   rep("Banana", each=5),
                   rep("Orange", each=89),
                   rep("Coconut", each=1)),100)
)

# Tilføj kolonne med sorterede værdier fra kol_2. Derefter i omvendt rækkefølge
data$Kol_4 <- sort(data$Kol_2)
data$Kol_4 <- rev(data$Kol_4)

# Tilføj kolonne Kol_1/Kol_2. Dog nu med mutate
library(dplyr)
data <- mutate(data, Kol_5 = Kol_1/Kol_2)

# Find kolonne der har indeholder Coconut
data$Kol_3["Coconut"]
table(data$Kol_3)


# vælg kolonner baseret på navne------------------------------------------------

# Brug dplyr eksempel på dataframe
library(dplyr)
biler <- mtcars

# Udvælg specifikke kolonner baseret på navne
biler[,c("mpg","hp")]

# Jeg vil gerne fjerne 2 kolonner ud fra deres navn
disp_col <- which(colnames(biler)=="disp")
carb_col <- which(colnames(biler)=="carb")
biler[,-c(disp_col,carb_col)]

biler[, names(biler) != "disp" & names(biler) != "carb"]

select(biler, -carb & -disp) # kræver dplyr pakke


# Aggregate, cut og ifelse------------------------------------------------------

library(dplyr)
biler <- mtcars

min(biler$disp)
max(biler$disp)

# Giver alle biler en true eller false værdi. Kan dog kun bruges til 2 muligheder
biler$high_map <- ifelse(biler$mpg>20, "True", "False")

# Inddeler alle mpg værdier efter en given size. Kan have lige så mange muligheder som ønsket
biler$size <- cut(biler$disp,
                  breaks = c(70, 150, 200, 300, 400, 500),
                  labels = c("mikro", "lille", "mellem", "stor", "giga"))

# brug cut men uden breaks og labels
biler$tal <- cut(biler$mpg, 6)
aggregate(mpg ~ tal, data = biler, FUN = mean)

# Aggregate kan bruges til at samle værdier.
# Det der står efter ~ er det den opdeler data efter
# Her bruger jeg mean til at bestemme mean af mpg for de 2 kategorier af high_map
aggregate(mpg ~ high_map, data = biler, FUN = mean)

# Her bruger jeg mean til at bestemme mean af mpg for alle kategorier af size
aggregate(disp ~ size, data = biler, FUN = mean)

# am betyder om der er automat gear eller ej.
# Find den gennemsnitlige værdi for begge typer og se hvad der er bedst
# Antager 0 betyder ikke automat og 1 betyder automat
aggregate(mpg ~ am, data = biler, FUN = mean)

# Er der forskel på antal cylindere og gear i forhold til automatgear eller ej
# Eksempel på hvordan man kigger på 2 variabler samtidig
aggregate(cbind(mpg, cyl) ~ am, data = biler, FUN = mean)

# Er der forskel på hestekræfter i forhold til antal cylindere
aggregate(hp ~ cyl, data = biler, FUN = mean)

# FUN kan også være min, max, median, sd osv.

# Functions---------------------------------------------------------------------



# loops-------------------------------------------------------------------------



# group by, cut-----------------------------------------------------------------

# base R metoden for inddeling i grupper er via cut metoden
# Dplyr metoden er via Group by

library(dplyr)
biler <- mtcars

# Giver alle biler en true eller false værdi. Kan dog kun bruges til 2 muligheder
biler$high_map <- ifelse(biler$mpg>20, "True", "False")

# Inddeler alle mpg værdier efter en given size. Kan have lige så mange muligheder som ønsket
biler$size <- cut(biler$disp,
                  breaks = c(70, 150, 200, 300, 400, 500),
                  labels = c("mikro", "lille", "mellem", "stor", "giga"))

# brug cut men uden breaks og labels
biler$tal <- cut(biler$mpg, 6)

?group_by

# which argumenter--------------------------------------------------------------



# filter------------------------------------------------------------------------

# base R metode
biler[biler$hp > 100,]

# dplyr metode
filter(biler, hp > 100)

min(biler$hp)
# Skal kun give 1 værdi
biler[biler$hp <= 52,]
# Skal ikke give nogen værdi
biler[biler$hp < 52,]
# Skal kun give 1 værdi
biler[biler$hp == 52,]

max(biler$hp)
# Skal give 0 rækker
biler[biler$hp > 335,]
# Skal give 1 række
biler[biler$hp >= 335,]

# skal opfylde 2 krav, og argument (første giver 0, da ingen har 5 cylindere)
biler[biler$cyl == 5 & biler$mpg > 20,]
biler[biler$cyl == 4 & biler$mpg > 20,]

# Skal opfylde 1 af 2 krav. Eller argument
biler[biler$cyl == 4 | biler$mpg <= 15,]

# enten 4 eller 6 cylindre
biler[biler$cyl == 4 |  biler$cyl == 6,]


# sub og gsub-------------------------------------------------------------------

# [A-Z]+
# [0-9]+

# sub fjerner kun det første tilfælde, gsub fjerner alle

data <- "jfdkgnl nvfw45348a fhs00"
sub(" ", "", data)
gsub(" ", "", data)

gsub("[A-Z]+","", data)
gsub("[a-z]+","", data)
gsub("[0-9]+","", data)

gsub("[^a-z]","", data)


# stringr and string extract and stuff------------------------------------------


# basic plotting----------------------------------------------------------------





# Join and merge dataframes-----------------------------------------------------

library(dplyr)
biler <- mtcars

biler1 <- biler[, c("mpg", "cyl", "disp", "hp")]
biler2 <- biler[, c("hp", "drat", "wt", "qsec")][1:20,]

# som det kan ses skal det de merges ud fra være en liste af unikke værdier.
# ellers går det galt
biler3 <- merge(biler1, biler2, by = "hp")

biler1$tal <- 1:nrow(biler1)
biler2$tal <- 1:nrow(biler2)
biler4 <- merge(biler1, biler2, by = "tal")

# rækker skal være lige lange for left_join
biler1 <- biler[, c("mpg", "cyl", "disp", "hp")]
biler2 <- biler[, c("hp", "drat", "wt", "qsec")]
biler1$tal <- 1:nrow(biler1)
biler2$tal <- 1:nrow(biler2)

biler4 <- left_join(biler1, biler2, by = "tal")



# lapply, sapply----------------------------------------------------------------

library(dplyr)
biler <- mtcars

# returnerer gennemsnittet af alle kolonner
lapply(biler, FUN = mean)
lapply(biler, mean)

lapply(biler, FUN = min)
lapply(biler, FUN = max)
lapply(biler[,c("mpg","cyl")], FUN = mean)



# API's-------------------------------------------------------------------------



# reshape og pivot_wider



# hvilket directory arbejder jeg i----------------------------------------------
getwd()



# basic argumenter (mean, min) osv----------------------------------------------


# json og httr og content-------------------------------------------------------

# skal kunne forklare hvad json pakken gør og hvad httr pakken gør


