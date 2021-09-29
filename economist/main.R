setwd( 'D:\\projects\\economist' )

source( 'Source_of_Source.R' )

df = fread( 'Aggregate data.csv', header = T )

mapdf_0 = read.xlsx( 'mapping_table.xlsx' )

mapdf = mapdf_0[!duplicated( mapdf_0 ), ]

year_cols = which( !is.na( as.numeric( names( df ) ) ) )

edf_0 = df %>% select( names( df )[ c( which( 'SeriesID' == names( df ) ), year_cols ) ] ) %>%
  
             mutate( function_name = lookup( df$SeriesID, mapdf$SeriesID, mapdf$Function ),
                     
                     exact_formula = lookup( df$SeriesID, mapdf$SeriesID, mapdf$Exact_Formula ),
                     
                     dependent = lookup( df$SeriesID, mapdf$SeriesID, mapdf$Dependency_Level ),
                     
                     .after = 'SeriesID' )

edf = edf_0[ edf_0$function_name != '', ]

unique_independent_series_ids = unique( edf %>% filter( dependent == 0 ) %>% select( SeriesID ) %>% unlist() )

tdf = NULL

for( i in 1:length( unique_independent_series_ids ) ){
  
  mdf_0 = edf %>% filter( SeriesID == unique_independent_series_ids[i] )
  
  mdf = mdf_0 %>% select( -c( 'SeriesID', 'function_name', 'exact_formula', 'dependent' ) )
  
  if( mdf_0$function_name[1] == 'sum' ){
    
    tdf[[i]] = data.table( t( colSums( mutate_all( mdf, function(x) as.numeric( as.character(x) ) ), na.rm = T ) ) ) %>%
      
      mutate( 'SeriesID' = unique_independent_series_ids[i], .before = names( mdf )[1] )
    
  } else{ tdf[[i]] = NULL }
  
}

independent_output = bind_rows( tdf )


#..... Exact Function - Dependent Series Computation ...

#... Level 1 dependency ....

unique_level_1_dependent_series_ids = unique( mapdf %>% filter( Dependency_Level == 1 ) %>% select( SeriesID ) %>% unlist() )

level_1_dependent_df = Exact_Function( unique_dependent_series_ids = unique_level_1_dependent_series_ids, mapdf_0 = mapdf, independent_output_0 = independent_output )

summary_output_1 = bind_rows( independent_output, level_1_dependent_df )

#... Level 2 dependency ....

unique_level_2_dependent_series_ids = unique( mapdf %>% filter( Dependency_Level == 2 ) %>% select( SeriesID ) %>% unlist() )

level_2_dependent_df = Exact_Function( unique_dependent_series_ids = unique_level_2_dependent_series_ids, mapdf_0 = mapdf, independent_output_0 = summary_output_1 )

summary_output_2 = bind_rows( summary_output_1, level_2_dependent_df )

#... Level 3 dependency ....

unique_level_3_dependent_series_ids = unique( mapdf %>% filter( Dependency_Level == 3 ) %>% select( SeriesID ) %>% unlist() )

level_3_dependent_df = Exact_Function( unique_dependent_series_ids = unique_level_3_dependent_series_ids, mapdf_0 = mapdf, independent_output_0 = summary_output_2 )

summary_output_3 = bind_rows( summary_output_2, level_3_dependent_df )

#... Level 4 dependency ....

unique_level_4_dependent_series_ids = unique( mapdf %>% filter( Dependency_Level == 4 ) %>% select( SeriesID ) %>% unlist() )

level_4_dependent_df = Exact_Function( unique_dependent_series_ids = unique_level_4_dependent_series_ids, mapdf_0 = mapdf, independent_output_0 = summary_output_3 )

summary_output_4 = bind_rows( summary_output_3, level_4_dependent_df )

#..... Weighted GM Computation ...
  
weighted_gm_output = Weighted_GM( summary_output_4, mapdf, df )

summary_output_5 = bind_rows( summary_output_4, weighted_gm_output )

#..... Percent Change Computation ....

percent_change_output = Percent_Change( summary_output_5, mapdf )

summary_output_6 = bind_rows( summary_output_5, percent_change_output ) %>% 
  
  mutate( SeriesName = lookup( .$SeriesID, mapdf$SeriesID, mapdf$SeriesName ), .after = 'SeriesID' )







