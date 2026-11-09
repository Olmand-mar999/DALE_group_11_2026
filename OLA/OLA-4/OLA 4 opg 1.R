library(chromote)
library(rvest)
library(dplyr)
library(logr)


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
# carname=readRDS("carnames.rds")

cardf <- data.frame(prices = character(), make_model = character())
cardf=as.data.frame(matrix(data="",nrow = 0,ncol = 2))
colnames(cardf)=carname

for (i in (1:limit)) {
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