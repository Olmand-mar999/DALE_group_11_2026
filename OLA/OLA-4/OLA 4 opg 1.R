library(chromote)
library(rvest)
library(stringr)
library(dplyr)
library(logr)

baseurl <- "https://www.bilbasen.dk/brugt/bil/vw?fuel=3&includeengroscvr=true&includeleasing=false&page="

options(chromote.headless = FALSE)
browser_sesh <- ChromoteSession$new()

alle_sider <- list() 

for (i in 1:136) {
  side_url <- str_c(baseurl, i)
  browser_sesh$Page$navigate(side_url)
  Sys.sleep(sample(3:7,1,replace = F))
  alle_sider[[i]] <- browser_sesh$Runtime$evaluate("document.documentElement.outerHTML")$result$value
  print(str_c("Side ", i, " hentet"))
}

browser_sesh$close()
saveRDS(alle_sider, "bilbasen_alle_sider.rds")

alle_sider_fra_Bilbasen_VW <- readRDS("bilbasen_alle_sider.rds")

for (i in 1:length(alle_sider_fra_Bilbasen_VW)) {
  side_i_Bilbasen_VW <- read_html(alle_sider_fra_Bilbasen_VW[[i]])
  bil_liste <- side_i_Bilbasen_VW |> html_elements("article") 
  for (bil in bil_liste) {
  }
}




# start af log fil
log_open(file_name = "carlog")

# opsætning og start af chromote (browser)
options(chromote.headless = FALSE)
b <- ChromoteSession$new()

# Bruges til at lukke sessionen eller hele browseren
#b$close()
#b$parent$close()

# find startlink
baseurl="https://www.bilbasen.dk/brugt/bil/vw?fuel=3&includeengroscvr=true&includeleasing=false&page="


cardf=as.data.frame(matrix(data="",nrow = 0,ncol = 2))
cardf <- data.frame(prices = character(), make_model = character())
# colnames(cardf)=carname

for (i in (1:136)) {
  # Bygger url'en
  url=paste0(baseurl,i)
  
  # Fortæller chromote at gå til url
  b$Page$navigate(url)
  
  # Beder systemet om at "vente" enten 3, 7 eller 1 sekund for at få aktivitet til at virke mindre som en bot
  Sys.sleep(sample(3:7,1,replace = F))
  
  # Kører JavaScript i browseren, som returnerer hele sidens HTML, som den ser ud efter JavaScript har kørt. Resultatet hentes ud som en tekststreng.
  html <- b$Runtime$evaluate("document.documentElement.outerHTML")$result$value
  
  # Beder systemet om at "vente" igen.
  Sys.sleep(1)
  
  
  page <- read_html(html)   # 
  carlist=page %>% html_elements("article")
  log_print(url)
  
  tryCatch({
    prices <- page %>% html_nodes("[class*='Listing_price']") %>% html_text()
    make_model <- page %>% html_nodes("[class*='Listing_makeModel']") %>% html_text()
    
    tmpdf = data.frame(
      prices=prices,
      make_model=make_model
    )
    
    cardf=rbind(cardf, tmpdf)
  }, error = function(e){
    log_print(str_c("Error at iteration", i, ":", conditionMessage(e)))
    return(i)
  }
  )
}

b$close()
saveRDS(cardf, "ford.rds")