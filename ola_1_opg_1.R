# Opg. 1.1

file <- "https://raw.githubusercontent.com/Olmand-mar999/DALE_group_11_2026/main/boligsiden.csv"
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

boligsiden
View(boligsiden)
nrow(boligsiden)
ncol(boligsiden)
colnames(boligsiden)
str(boligsiden)
summary(boligsiden)

boligsiden[which(boligsiden$vej == "egevej" & boligsiden$vejnr == 20 & boligsiden$postnr == 7100),]
boligsiden[which(boligsiden$vej == "tousvej" & boligsiden$vejnr == 106 & boligsiden$postnr == 8230),]


# Opg. 1.2
boligsiden[sample(nrow(boligsiden), 2), ]


# Opg. 1.3
colSums(is.na(boligsiden))

## Tjek for at N/A's på vej, er fuld missing adresse
vej_na <- boligsiden[is.na(boligsiden$vej), ]
sum(is.na(vej_na$vejnr))
sum(is.na(vej_na$postnr))
sum(is.na(vej_na$by))

## Redegøring for manglende ID
har_addresse <- boligsiden[!is.na(boligsiden$vej), ]
vej_dup_tjek <- duplicated(har_addresse[, c("vej","vejnr","postnr")]) | 
  duplicated(har_addresse[, c("vej","vejnr","postnr")], fromLast = TRUE)
duplicate_vej_tjek <- har_addresse[vej_dup_tjek, ]
