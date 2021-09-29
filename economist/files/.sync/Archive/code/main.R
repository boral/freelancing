rm( list = ls() )

library( data.table ) ; library( dplyr )

setwd( 'D:\\projects\\economist\\data' )

df = fread( 'Aggregate data.csv', header = T )

unique_series_names = unique( df$SeriesName )

year_cols = which( !is.na( as.numeric( names( df ) ) ) )

edf = df %>% select( names( df )[ c( which( 'SeriesName' == names( df ) ), year_cols ) ] )

tdf = NULL

for( i in 1:length( unique_series_names ) ){
  
  mdf = edf %>% filter( SeriesName == unique_series_names[i] )
  
  tdf[[i]] = data.table( t( colSums( mutate_all( mdf[ , -1 ], function(x) as.numeric( as.character(x) ) ), na.rm = T ) ) ) %>%
    
              mutate( 'Indicator' = unique_series_names[i], .before = names( mdf[ , -1 ] )[1] )
  
}

output = bind_rows( tdf )


