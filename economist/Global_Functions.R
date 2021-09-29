#..... Computation for Exact_Function ....

#.... unique_dependent_series_ids = unique_level_1_dependent_series_ids ; mapdf_0 = mapdf ; independent_output_0 = independent_output

Exact_Function = function( unique_dependent_series_ids, mapdf_0, independent_output_0 ) {
  
  kdf = NULL
  
  for( j in 1:length( unique_dependent_series_ids ) ){
    
    sdf = mapdf_0 %>% filter( SeriesID == unique_dependent_series_ids[j] )
    
    if( sdf$Function == 'exact_formula' ){
      
      dependency_components = unlist( strsplit( sdf$Depends_On, ',' ) )
      
      dependencies_present_indicators = dependency_components %in% independent_output_0$SeriesID
      
      if( all( dependencies_present_indicators ) ){
        
        dependency_df_0 = independent_output_0 %>% filter( SeriesID %in% dependency_components ) %>% 
          
          t() %>% as.data.frame()
        
        dependency_df = dependency_df_0[ -1, ] %>% mutate_all( function(x) as.numeric( as.character(x) ) )
        
        names( dependency_df ) = dependency_df_0[ 1, ]
        
        dependency_df = dependency_df %>% mutate( curr_metric = eval( parse( text = sdf$Exact_Formula ) ) )
        
        kdf[[j]] = data.table( t( dependency_df$curr_metric ) ) %>% `colnames<-`( rownames( dependency_df ) ) %>%
          
          mutate( 'SeriesID' = unique_dependent_series_ids[j], .before = 1 )
        
      } else{ 
        
        kdf[[j]] = NULL 
        
        cat( 'Dependencies :', paste0( dependency_components[ !dependencies_present_indicators ], collapse = ', ' ), 'are absent for computing ', unique_dependent_series_ids[j], '\n' )
        
      }
      
    } else{ kdf[[j]] = NULL }
    
  }
  
  dependent_output = bind_rows( kdf )
  
  return( dependent_output )
  
}


#..... Computation for Weighted_GM ....

#.... summary_df_0 = summary_output_4 ; mapdf_01 = mapdf ; data_0 = df

Weighted_GM = function( summary_df_0, mapdf_01, data_0 ){
  
  wgm_info_df = mapdf_01 %>% filter( Function == 'weighted_gm' )
  
  wgm_ids = unique( wgm_info_df$SeriesID )
  
  dependency_ids = unique( wgm_info_df$Depends_On )
  
  #.... Weight creation ......
  
  df_weight = NULL
  
  for( i in 1:length( dependency_ids ) ){
    
    df_year = data_0 %>% filter( SeriesID == dependency_ids[i] ) %>% select( c( 'SeriesID', 'Country', '2010' ) )   #.... 2010 is baseline here
    
    df_year$`2010` = as.numeric( df_year$`2010` )
    
    df_weight[[i]] = data.frame( SeriesID = dependency_ids[i], Country = df_year$Country,
                                 
                                 weights = sapply( df_year$`2010`/sum( df_year$`2010` ), function( x ){ ifelse( is.numeric( x ), x, 0 ) } ) )
    
  }
  
  df_weights = bind_rows( df_weight ) %>% mutate( country_series = paste0( .$SeriesID, '_', .$Country ) )
  
  
  #.... Geometric mean creation ....
  
  series_df = NULL
  
  for( j in 1:nrow( wgm_info_df ) ){
    
    series_df_0 = data_0 %>% select( -c( 'SeriesName', 'Source', 'definition', 'Notes', 'Currency', 'UoM' ) ) %>%
      
                           mutate( country_series = paste0( .$SeriesID, '_', .$Country ), .after = 'Country' ) %>%
      
                           mutate( weights = lookup( .$country_series, df_weights$country_series, df_weights$weights ), .after = 'Country' ) %>%
      
                           filter( SeriesID == wgm_info_df %>% filter( SeriesID == wgm_info_df$SeriesID[j] ) %>% select( Depends_On ) %>% unlist() ) %>%
      
                           select( -c( 'SeriesID', 'Country', 'country_series' ) ) %>%
      
                           mutate_all( function( x ) as.numeric( as.character( x ) ) )
    
    series_df_1 = log( series_df_0 %>% select( -c( 'weights' ) ) )*series_df_0$weights
    
    series_df[[j]] = data.table( exp( t( colSums( series_df_1 ) ) ) ) %>% mutate( SeriesID = wgm_info_df$SeriesID[j], .before = 1 )
    
  }
  
  df_gm = bind_rows( series_df )
  
  return( df_gm )
  
}


#..... Percent Change Computation ....

#.... summary_df_1 = summary_output_5 ; mapdf_02 = mapdf

Percent_Change = function( summary_df_1, mapdf_02 ){

  pchg_info_df = mapdf_02 %>% filter( Function == 'percent_change' )

  gdf_0 = summary_df_1 %>% filter( SeriesID %in% pchg_info_df$Depends_On )
  
  gdf_1 = gdf_0 %>% select( -c( 'SeriesID' ) ) %>% as.data.frame()
  
  pcg_df = NULL
  
  for( i in 2:length( names( gdf_1 ) ) ){
    
    pcg_df[[i]] = data.frame( ( ( gdf_1[ , i ]/gdf_1[ , i-1 ] ) - 1 )*100 )
    
    names( pcg_df[[i]] ) = names( gdf_1 )[i]
    
  }
  
  pcg_df[[1]] = data.frame( rep( NA, nrow( gdf_1 ) ) ) ; names( pcg_df[[1]] ) = names( gdf_1 )[1]   #... for first year
  
  pcg_df_all = bind_cols( pcg_df )
  
  pcg_df_all = pcg_df_all %>% mutate( DependentSeriesID = gdf_0$SeriesID, .before = names( gdf_1 )[1] ) %>%
    
                              mutate( SeriesID = lookup( .$DependentSeriesID, pchg_info_df$Depends_On, pchg_info_df$SeriesID ), .before = 'DependentSeriesID' )
  
  pcg_df_all_1 = pcg_df_all %>% select( - 'DependentSeriesID' )
  
  cat( 'SeriesIDs : ', paste0( setdiff( pchg_info_df$SeriesID, pcg_df_all_1$SeriesID ), collapse = ', ' ), ' are not present as their dependencies are absent.' )
  
  return( pcg_df_all_1 )

}

