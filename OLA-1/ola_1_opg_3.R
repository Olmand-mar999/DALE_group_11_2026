# Opg. 3.1
roll_dice <- function(t) {
  die <- 1:6
  sample(die,size=t,replace=TRUE)
}

result_throws <- roll_dice(25000)

fives <- sum(result_throws==5)
fives_chance <- fives/length(result_throws)
fives_chance_theo <- 1/6


# Opg. 3.2
library(ggplot2)

roll_dice2 <- function() {
  die <- 1:6
  result_throw <- sample(die,size=6,replace=TRUE)
  sum(result_throw)
}

result_throws2 <- replicate(10000,roll_dice2())

barplot(table(result_throws2),
        main = "Sum of 6 die (10.000 throws)",
        xlab = "Sum", ylab = "Count")


# Opg. 3.3
roll_dice3 <- function() {
  die <- 1:6
  result_throw2 <- sample(die,size=6,replace=TRUE)
  sum(result_throw2)
}

result_throws3 <- replicate(1000000,roll_dice3())

barplot(table(result_throws3),
        main = "Sum of 6 die (1.000.000 throws)",
        xlab = "Sum", ylab = "Count")


# Opg. 3.4
x_fixed <- c(1,2,3,5,6)
x <- sample(x_fixed)

y <- 2:6

cbind(y,x)