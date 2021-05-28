rm( list = ls() )

library( dplyr ) ; library( jsonlite )

json_data = list( 'cp' = 0.8, 'data' = mtcars )

json_out = toJSON( json_data )

json_out

write( json_out, 'json_data.json' )
