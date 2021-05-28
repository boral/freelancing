# plumber.R

#* Echo the parameter that was sent in
#* @get /echo
function(){
  list(msg = 'Hello World')
}

#* Model
#* @post /date
function(req){
  
  tryCatch({
  
    library( rpart ) ; library( dplyr ) ; library( jsonlite )
    
    body = jsonlite::fromJSON( req$postBody )
    
    if( is.data.frame( body$data ) ){
      
      df = body$data
      
    } else{
      
      df = body$data[[1]]
      
    }
    
    fit = rpart( mpg ~ ., data = df, control =  rpart.control( cp = body$cp ) )

    toJSON( fit$control )
  
  }, error = function(e) {
    
    return( jsonlite::toJSON( list('error' = e), force = T ) )
  })
  
}


# plumberDeploy::do_deploy_api('ubuntu', "date", "D:\\projects\\Sakhu\\.api", 8000, docs = T, overwrite = T )
