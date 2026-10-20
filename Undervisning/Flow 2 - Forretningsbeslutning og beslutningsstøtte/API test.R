library(httr)
library(jsonlite)

testurl <- "https://api.smk.dk/api/v1/art?object_number=KMS1"

apiraw <- httr::GET(testurl)

# Check status code. Skal være 200
apiraw$status_code

apitext <- httr::content(apiraw, as="text")

apijson <- fromJSON(apitext)

# Gør til dataframe
data <- apijson$items

