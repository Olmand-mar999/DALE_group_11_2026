# Opg. 2.1
alltable = dkstat::dst_get_tables()

forv1_meta <- dst_meta("FORV1", lang = "da")
forv1_meta$variables 
forv1_meta$values$INDIKATOR

forv1 <- dst_get_data("FORV1",
                      query = list(INDIKATOR = "*", Tid = "*"),
                      lang = "da", meta_data = forv1_meta)

colnames(forv1) <- c("indikator", "tid", "nettotal")
forv1$kode <- sub(" .*", "", forv1$indikator)

## Hvilke år har data for alle måneder
maanedlig_check <- aggregate(!is.na(nettotal) ~ format(tid, "%Y"),
                             data = forv1[forv1$indikator == "F1 Forbrugertillidsindikatoren", ],
                             FUN = sum)
names(maanedlig_check) <- c("aar", "antal_maaneder_med_data")
maanedlig_check

## Fjern data fra før 1996
forv1_96 <- subset(forv1, tid >= as.Date("1996-01-01"))


