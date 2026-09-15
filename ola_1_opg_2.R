# Opg. 2.1
file <- "~/Desktop/Study/CPH_business/Dataanalyse/DALprojects/Docs/OLA/OLA_1/boligsiden.csv"
boligsiden <- read.csv(file, colClasses = "character")
clean_num <- function(x) {
  x <- gsub("kr\\.", "", x)   # strip " kr." (only pris has this)
  x <- gsub("\\.", "", x)     # strip the thousands-separator dot
  as.numeric(trimws(x))
}
boligsiden$pris      <- clean_num(boligsiden$pris)
boligsiden$kvmpris   <- clean_num(boligsiden$kvmpris)
boligsiden$mdudg     <- clean_num(boligsiden$mdudg)
boligsiden$grund     <- clean_num(boligsiden$grund)
boligsiden$størrelse <- clean_num(boligsiden$størrelse)
boligsiden$værelser  <- clean_num(boligsiden$værelser)
boligsiden$opført    <- clean_num(boligsiden$opført)


# Opg. 2.2


# Opg. 2.3
model_størrelse <- lm(kvmpris ~ størrelse, data = boligsiden)
model_grund     <- lm(kvmpris ~ grund, data = boligsiden)
model_værelser  <- lm(kvmpris ~ værelser, data = boligsiden)
model_mdudg     <- lm(kvmpris ~ mdudg, data = boligsiden)
model_opført    <- lm(kvmpris ~ opført, data = boligsiden)

summary(model_størrelse)
summary(model_grund)
summary(model_værelser)
summary(model_mdudg)
summary(model_opført)


# Opg. 2.4