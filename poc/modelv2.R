# plumber.R

#* Echo the parameter that was sent in
#* @get /echo
function(){
  list(msg = 'Hello World')
}

#* Model
#* @post /data
#* @get /data
function(req){

  library( rpart ) ; library( dplyr ) ; library( jsonlite )

  # body = jsonlite::fromJSON( 'json_data.json' )

  body = jsonlite::fromJSON( req$postBody )

  if( is.data.frame( body$data ) ){

    df = body$data

  } else{

    df = body$data[[1]]

  }

  fit = rpart( mpg ~ ., data = df, control =  rpart.control( cp = body$cp ) )

  # fit = rpart( mpg ~ ., data = mtcars, control =  rpart.control( cp = 0.5 ) )

  toJSON( fit$control )
  
  # list( rawbody = req$postBody )
  
  
  ## Send a message with the location of the file
  # paste0("File rows : ", nrow( df ), "\n")
  
}



