rm( list = ls() )

library(httr)
library(jsonlite)
options(stringsAsFactors = FALSE)

# url for local testing
# url <- "http://127.0.0.1:6531"

url = "http://159.65.40.84"


# create example body
#body1 = list( 'dependent' = 'mpg', 'independent' = c( 'cyl', 'wt', 'hp' ), 'data' = mtcars )

body = fromJSON('json_data.json' )

#body = list( 'msg' = 'hidfs' )

body_json = toJSON( body )

# set API path
path <- 'date/date'

# send POST Request to API
raw.result <- POST(url = url, path = path, body = body_json, encode = 'raw')

# check status code
raw.result$status_code

# check response
fromJSON(rawToChar(raw.result$content))


#.. http://159.65.40.84/date/date
