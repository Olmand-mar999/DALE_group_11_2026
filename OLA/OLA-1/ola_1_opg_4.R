# Opg. 4.1
Klasse <- rep(c("A","B","C","D"),each=9)
Uge <- seq(1,9)
Score <- sample(1:100, 36, replace = TRUE)

df <- data.frame(Klasse,Uge,Score)


# Opg. 4.2
i_list = 1:36

Klasse3 <- c()
Uge3 <- c()
Score3 <- c()

for (i in i_list) {
  if (i %% 3 == 0) {
    Klasse2 <- (df$Klasse[i])
    Uge2 <- (df$Uge[i])
    Score2 <- (sum(df$Score[(i-2):i]/3))
    Klasse3 <- c(Klasse3,Klasse2)
    Uge3 <- c(Uge3,Uge2)
    Score3 <- c(Score3,Score2)
    print(i)
  } else {
    
  }
}


df2 <- data.frame(Klasse3,Uge3,Score3)


# Opg. 4.3
library(tidyverse)

df3 <- pivot_wider(df2,names_from = Klasse3,values_from = Score3)
df3 <- rename(df3, Uge = Uge3)
