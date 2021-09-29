rm( list = ls() )

library( data.table ) ; library( dplyr ) ; library( lookup )

#setwd( 'D:\\projects\\economist\\data' )

df = fread( 'Aggregate data.csv', header = T )

mapdf = fread( 'mapping_table.csv' )

year_cols = which( !is.na( as.numeric( names( df ) ) ) )

edf_0 = df %>% select( names( df )[ c( which( 'SeriesName' == names( df ) ), year_cols ) ] ) %>%
  
             mutate( function_name = lookup( edf$SeriesName, mapdf$SeriesName, mapdf$Function ), .after = 'SeriesName' )

edf = edf_0[ edf$function_name != '', ]

unique_series_names = unique( edf$SeriesName )

tdf = NULL

for( i in 1:length( unique_series_names ) ){
  
  mdf_0 = edf %>% filter( SeriesName == unique_series_names[i] )
  
  mdf = mdf_0 %>% select( -c( 'SeriesName', 'function_name' ) )
  
  if( mdf_0$function_name[1] == 'sum' ){
  
  tdf[[i]] = data.table( t( colSums( mutate_all( mdf, function(x) as.numeric( as.character(x) ) ), na.rm = T ) ) ) %>%
    
              mutate( 'Indicator' = unique_series_names[i], .before = names( mdf )[1] )
  
  } else{ tdf[[i]] = NULL }
  
}

output = bind_rows( tdf )


