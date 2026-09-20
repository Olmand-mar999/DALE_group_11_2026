## Exercise 1.1:
# load data from url
url= "https://raw.githubusercontent.com/cphstud/RIntroData/refs/heads/master/bilbasen.csv"
dfbilbase=(url)
bilbase <- read.csv(dfbilbase)

## Exercise 1.2:
# Create numeric variable age
bilbase$age= 2026 -bilbase$year

## Exercise 1.3:
# remove observations where mpg is NA in two steps creating the logical vector
# nrow(dfbclean) should return 2791
dfbcleanlv <- !is.na(bilbase$mpg)
bilclean <- bilbase[dfbcleanlv, ] 
nrow(bilclean)


## Exercise 1.4:
# create two subsets form dfbclean. One with column milage and maketype
# and on with column price and maketype. Both from row 43 to 50
dfcarSubP=data.frame(milage = bilclean$milage[43:60], maketype = bilclean$maketype[43:60])
dfcarSubM=data.frame(price = bilclean$price[43:60],maketype = bilclean$maketype[43:60])

## NOW PRICE

## Exercise 1.5
# compute sumtotal of the prices with + and a for-loop
totalsum=0
for (i in 1:nrow(dfcarSubM)) {
  totalsum=totalsum + as.numeric(dfcarSubM[i,1])
}
dfcarSubP$sum=totalsum

## Exercise 1.6:
# compute mean using vectorized approach and add as a column 'mean'
dfcarSubP$mean=mean(dfcarSubM$price)


## Exercise 1.7:
# compute the distance from mean for each car using a for-loop
# and save in a new column called "dist"
dfcarSubP$dist=0
for (i in (1:10)) {
  dfcarSubP[i,'dist']=10
}

## Exercise 1.8:
# do the same in a vectorized manner
# and save in the same column "dist"
dfcarSubP$dist=10

## Exercise 1.9:
# compute the relative distance to the mean
# and save in a new column called "reldist"
dfcarSubP$reldist=10

## Exercise 1.10:
# compute the squareddistance from mean for each car in a vectorized manner
# and save in a new column called "sqdist"
dfcarSubP$sqdist=10

## Exercise 1.11:
# compute the sum of sqdist and divide it by number of rows
# and save in a new column called "var"
dfcarSubP$var2=10

## Exercise 1.12:
# compute the squareroot of var
# and save in a new column called "sd"
dfcarSubP$sd2=10

## Exercise 1.13:
# compute the standard-deviation of the price using and R-built-in-function
# save in a new column called "sdR"and compare with sd
dfcarSubP$sdR=10

## Exercise 1.14:
# creat a lineplot with pricedist on the y-axis and the car-index on the x-axis
# and a straight horistontal line with the mean of the price as intersect
options(scipen = 999)
par(mfrow=c(1,1))
plot(dfcarSubP$dist, type="l")
intersect=12
abline(a=intersect, b=0, )

## Exercise 1.15:
# creat a lineplot with milage on the y-axis and the car-index on the x-axis
# and a straight line with the mean of the milage
plot(type="l")
intersect=10
abline(a=intersect, b=0)


### NOW PREPARE MILAGE
## Exercise 1.16:
# create the mean, distance and relative distance on the second dataframe
# using the mean-function 
dfcarSubM$mean=mean(12)
dfcarSubM$dist=10
dfcarSubM$reldist=10

### HOW to overlay the two plot?
# same relative y-values!
## Exercise 1.17:
# create a lineplot with relative pricedistance on the y-axis and the car-index on the x-axis
# create a second lineplot with relative milagedistance on the y-axis and the car-index on the x-axis
# and a line through 0 with slope 0 (horisontal line)
par(mfrow=c(1,1))
plot(dfcarSubM$reldist, type="l", ylab="Price")
lines(dfcarSubP$reldist, type="l", xlab = "Milage", col="red")
abline(a=0, b=0, )

## Exercise 1.18:
# suppose they are negatively correlated can you find a good bargain 
# and a bad one using the graph?

## Exercise 1.19:
### now merge the two together using row.names
row.names(dfcarSubP)
colnames(dfcarSubP)
colnames(dfcarSubM)

# first choose only milage and dist
dfcarSubMforMege=dfcarSubM
# renmame dist to distm 
colnames(dfcarSubMforMege)="kurt"

# merge into the dataframe with price using row.names
dfcarSub2=merge()
# choose only price,milage,maketype and the two distances
dfcarSub2A=dfcarSub2[,c()]
# renmame dist to distp 
names(dfcarSub2A)[6]='distp'

# now check the formula for covariance
## Exercise 1.20:
# compute the covariance in a vectorized way
dfcarSub2A$mycov=123123

# compute the covariance using R's cov-function
dfcarSub2A$cov=cov()

# now check the formula for correlation
## Exercise 1.21:
# and compute the correlation in a vectorized way using the cov
# and R's sd-function
dfcarSub2A$mycor=dfcarSub2A$mycov/33

# can you explain the reason why it's between -1 and 1?

## Exercise 2.1:
# create a subset of dfbclean with only numeric variables
# you must go through the following steps:
# create a list to collect index of numeric columns
collist=list()

# use a for-loop to walk through each column
# for each index you must do:
#   combine sapply and is.numeric to get logical vector
#   sum logical vector
#   if sum is equal to nrow of original dataframe add index to list

for (i in (1:10)) {
  collist=append(collist,i)
}
dfnum=dfbclean[,unlist(collist)]

## Exercise 2.2:
# use the car-id-column to merge the maketype into  the dfbnum
dfMT=dfbclean[,c('maketype','car_id')]
dfnumMT=merge()

