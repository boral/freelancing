rm( list = ls() )

library(httr)
library(jsonlite)
options(stringsAsFactors = FALSE)

# url for local testing
# url <- "http://127.0.0.1:6531"

# url = "http://159.65.40.84"


response <- httr::POST("http://159.65.40.84", 
                       
                       body=list(df=mtcars, cp = 0.05), encode="json")
httr::content(response)


#.. http://159.65.40.84/date/
