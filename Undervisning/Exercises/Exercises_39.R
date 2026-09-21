## Exercise 1.1:
# load data from url
url= "https://raw.githubusercontent.com/cphstud/RIntroData/refs/heads/master/bilbasen.csv"
dfbilbase=read.csv(url)
dfbilbase$age=2022-dfbilbase$year

## Exercise 1.3:
# The code below creates a numeric variable holding the displacement
library(dplyr)
library(tidyverse)
dfbilbase <- dfbilbase |> mutate(
  disp=str_extract(maketype," [0-9],[0-9]+ "),
  disp=as.numeric(str_replace(disp,",",".")))

na_rows <- which(is.na(dfbilbase$disp))
table_na <- dfbilbase[na_rows,]
dfbilbase$maketype[na_rows] <- str_replace(table_na$maketype,"i","")

dfbilbase <- dfbilbase |> mutate(
  disp=str_extract(maketype," [0-9],[0-9]+ "),
  disp=as.numeric(str_replace(disp,",",".")))

# Fjerner i'et i capri, og ellers er der 2 tilbage
na_rows <- which(is.na(dfbilbase$disp))
table_na <- dfbilbase[na_rows,]

# check the rows holding NA's in "disp", identify the cause, fix it with gsub
# and then recreate the "disp" variable
dfb=dfbilbase |> filter()
dfbilbase$maketype=gsub("","",dfbilbase$maketype)


## Exercise 1.4:
# create a subset where the rows with NA in mpg and disp are removed
dfbclean <- dfbilbase |> filter()
dfbclean <- dfbclean |> filter()

# Create a subset of columns with numeric variables
# and remove variables with no relevance for lm
dfbilbaseNum = dfbclean |> select(where()) |> 
  select(-c())


## Exercise 1.5:
# The code below creates a correlation-plot 
library(corrplot)
cordfc=cor(dfbilbaseNum)
corrplot(cordfc,method = c("color"), addCoef.col = 'black')

# explain from the formula for correlation why year ~ price is positive 
# and age ~ price is negative (as you would expect)

# is there an enginerelated explanation for mpg ~ disp?


## Exercise 2.1:
# nested for-loop: Den "lille" tabel
# generate a 10 x 10 matrix with row and colnames 1:10
# each row must contain the related tabel
# so row 2 must be:
# [2,]    2    4    6    8   10   12   14   16   18    20

lilleTabel=matrix()

for (i in (1:1)) {
  for(i in (1:1)) {
    lilleTabel[]=1*1
  }
}


## Exercise 2.2:
# while-loop
# use the code from base R cheatsheet for the while-loop and apply the following:
# after the current minute ends the loop must end and print the date and time
library(lubridate)
condition=T
now=Sys.time()
sec=second(now)

while (condition) {
  Sys.sleep(1)
  # create code to exit when the minute changes
  if (sec) {
    condition=F
    print()
  } 
}


## Exercise 2.3:
# while-loop on bilbasen
# create a while-loop that runs until the mean of samples size 30 from bilbasen
# price is stable - i.e does not change more than a given tolerance between 
# iterations. Collect each samplemean in a vector
# The loop must have a safety-part. The code is like in the cheat-sheet.
condition=T
tolerance=1
safetylimit=10000
counter=1
diff=100000000
meancollv=NULL

prevmean=mean(sample(dfbclean$price,30))
meancollv[counter]=prevmean

while (condition) {
  Sys.sleep(0.1)
  # safety
  if (unsafe) {
    condition=F
    break
  }
  thismean=mean(colv)
  diff=abs(prevmean-thismean)
  if (belowtol) {
    condition=F
    print(thismean)
  } else {
    print(str_c("Go on ",diff , " on mean ", thismean))
    prevmean=thismean
  } 
}

hist(colv, breaks=30)
hist(dfbclean$price, breaks=30)
mean(colv)
sd(colv)
mean(dfbclean$price)
sd(dfbclean$price)/sqrt(30)
length(colv)

## Exercise 2.4:
# Plots
# calculate the mean price within each make using both base R aggregate and
# tidyverse group_by and summarise
dfcarB <- aggregate()
dfcarT <- dfbclean |> group_by()

# now plot with ggplot
library(ggplot2)
ggplot(dfcarB, aes(x=x, y=y))+
  geom_()

# do the same but now for region
dfcarBR <- aggregate()
dfcarTR <- dfbclean |> group_by()

ggplot(dfcarBR, aes(x=x, y=y)+
  geom_()

# do the same but now for price calculated per region and make
dfcarBM <- aggregate()

ggplot(dfcarBM, aes(x=x, y=y))+
  geom_()+
  facet_wrap(~make)

ggplot(dfcarBM, aes(x=x, y=y, fill=z))+
  geom_()+
  facet_wrap(~make)


## Exercise 3.1:
# DST
# undersøg "INDKP109" og hent relevante tal ud for hver landsdel

# Bilbasen
# Tilpas region så data kan merges med DST
