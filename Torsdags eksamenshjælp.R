#### FORBEREDELSE: - indlæs pakken dplyr og det indbyggede datasæt: mtcars ####
# Install packages()
library(dplyr)

biler <- mtcars

#### OPGAVE 1: UNDERSØG DATASÆT ####

# 1.1 - vis de første 6 rækker i datasættet
biler[1:6,]
head(biler,6)

# 1.2 - undersøg datasættets struktur
biler
structure(biler)
str(biler)
class(biler$mpg)
typeof(biler$hp)

# 1.3 - Find antallet af rækker og kolonner
dim(biler)
ncol(biler)
nrow(biler)

# 1.4 - Beregn gennemsnittet af mpg
mean(biler$mpg)

# 1.5 - find højeste og laveste værdi af hp
min(biler$hp)
max(biler$hp)


#### OPGAVE 2: UDVÆLG BESTEMTE BILER ####

# 2.1 - Udvælg kolonnen mpg
mpg <- biler$mpg

# 2.2 - Udvælg kolonnerne mpg og hp
mphp <- biler[,c("mpg","hp")]


# 2.3 - vis de første fem biler
biler[1:5,]

head(mphp,5)

# 2.4 - find de biler med mere end 150 hestekræfter
biler[biler$hp > 150,]

# 2.5 - find de biler med mindst 6 cylindre
biler[biler$cyl >= 6,]

#### ekstra opgave: Find biler, der både har mere end 150 hestekræfter og mindst 6 cylindre
biler[biler$cyl >= 6 & biler$hp > 150,]


#### OPGAVE 3: FILTRING ####

# 3.1 - find alle biler med mere end 20 mgp
biler[biler$mpg > 20,]

# 3.2 -  find alle biler med otte cylindre
biler[biler$cyl == 8,]

# 3.3 - find alle biler med mindst 150 hestekræfter
biler[biler$hp >= 150,]

# 3.4 - find alle biler med 5 cylindre og mere end 20 mpg
sort(biler$cyl)
biler[biler$cyl == 5 & biler$mpg > 20,]

# 3.5 - find biler med enten 4 eller 8 cylindre
biler[biler$cyl == 4 |  biler$cyl == 8,]      # | means or

filter(biler, cyl %in% c(4,8))


#### OPGAVE 4: UDVÆLG KOLONNER OG SORTÉR ####

# 4.1 - udvælg kun mpg,hp,wt og cyl
biler[,c("mpg","hp","wt","cyl")]

select(biler, mpg,hp,wt,cyl)

# 4.2 - sortér bilerne efter mpg fra lavest til højest
biler[order(biler$mpg),]

arrange(biler,mpg)

# 4.3 - sortér bilerne efter hp fra højest til lavest
biler[order(biler$hp, decreasing = TRUE),]

arrange(biler,desc(hp))

# 4.4 - find de 5 biler med højeste brændstoføkonomi
biler[order(biler$mpg, decreasing = TRUE),][1:5,]

arrange(biler,mpg) %>% head(n = 5)

# 4.5 - udvælg alle variabler undtagen disp og carb
disp_col <- which(colnames(biler)=="disp")
carb_col <- which(colnames(biler)=="carb")
biler[,-c(disp_col,carb_col)]

biler[, names(biler) != "disp" & names(biler) != "carb"]

select(biler, -carb & -disp)


#### OPGAVE 5: OPRET NYE VARIABLER ####

# 5.1 - opret "hp_kw", der omregner hestekræfter til kilowatt- brug omregningen:
#  kW = hp * 0,7457
biler_ny <- biler
biler_ny$hp_kw <- biler_ny$hp * 0.7457

# 5.2 - opret "weight_kg", der omregner bilens vægt til kilogram.Variablen wt er angivet i 1.000 pund
# kg = wt * 1000 * 0,453592
biler_ny$weight_kg <- biler_ny$wt * 1000 * 0.453592

# 5.3 Opret"high_mgp", som er TRUE, hvis bilen har mere end 20 mpg, og ellers FALSE
biler_ny$high_map <- biler_ny$mpg>20

biler_ny$high_map <- ifelse(biler_ny$mpg>20, "True", "False")


# 5.4 - vis de første 10 biler med de nye variabler
biler_ny[1:10,]


### OPGAVE 6 - DESKRIPTIV STATISTIK ####

#Beregn følgende for mpg:
#Gennemsnit
mean(biler$mpg)

#Median
median(biler$mpg)

#Standardafvigelse
sd(biler$mpg)

#Minimum
min(biler$mpg)

#Maximum
max(biler$mpg)

#Antal observationer
nrow(biler)

#Ekstra udfordring: Beregn første og tredje kvartal
