# plumber.R

#* Echo the parameter that was sent in
#* @get /echo
function(){
  list(msg = 'Hello World')
}

#* Model
#* @param df
#* @param cp
#* @post /date
function(req, df, cp){
  
  tryCatch({
    
    library( rpart ) ; library( dplyr ) ; library( jsonlite )
    
    df = as.data.frame( df )
    
    fit = rpart( mpg ~ ., data = df, control =  rpart.control( cp = as.numeric( cp ) ) )
    
    toJSON( fit$control )
    
  }, error = function(e) {
    
    return( jsonlite::toJSON( list('error' = e), force = T ) )
  })
  
}



