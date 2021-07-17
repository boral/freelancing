rm( list = ls() ) ; options( error = NULL )

library( dplyr ) ; library( lubridate ) ; library( stringr ) ; library( tidyr )

setwd( 'D:\\projects\\Kevin\\milestone_2' )

input_df = read.csv( 'subcate_data_input_sample.csv', stringsAsFactors = F )

hub_subcate_df_0 = read.csv( 'hub_subcate_displayed_lookup.csv', stringsAsFactors = F, skip = 1 )

hub_subcate_df = hub_subcate_df_0 %>% select( -X.1 )

hub_subcate_df[ is.na( hub_subcate_df ) ] = 0

hub_subcate_month_df = read.csv( 'hub_subcate_displayed_bymonth_lookup.csv', stringsAsFactors = F )

names( hub_subcate_month_df ) = c( 'month', 'hub', 'subcat' )

#..... Function to clean and compute subhub summary ....

#... df0 = input_df[1:10000,] ; df1 = hub_subcate_df ; df2 = hub_subcate_month_df

Subhub_Summary = function( df0, df1, df2 ){
  
  df = df0 %>% filter( audit == 1 ) %>%      #.... removing cases where audit is 0
    
             select( 'ï..weight', 'month', 'categoryincidence', 'cate_code' ) %>%   #... selecting only required columns
    
             mutate( categoryincidence = str_replace_all( categoryincidence, '\\|', ',' ) ) %>%    #.... replacing | with
    
       left_join( ., df1, by = c( 'cate_code' = 'X' ) ) %>%    #.... adding subslot info
    
       #..... adding display slots indicator
    
       bind_cols( .,
                  
                  apply( ., MARGIN = 1, FUN = 
                           
                           function( x ){
                             
                             subcats = df2 %>% filter( month == x[['month']] & hub == x[['cate_code']] ) %>% select( subcat ) %>% unlist()
                             
                             presence_ind_df = as.numeric( x[ names( x ) %>% startsWith( 'Slot' ) ] %in% subcats ) %>% t() %>% as.data.frame()
                             
                             names( presence_ind_df ) = paste0( 'disp_slot_', 1:40 )
                             
                             return( presence_ind_df )
                             
                           } ) %>% bind_rows() ) %>%
    
    #..... adding selected slots indicator
    
    bind_cols( .,
               
               apply( ., MARGIN = 1, FUN = 
                        
                        function( x ){
                          
                          selected_ind_df = as.numeric( x[ names( x ) %>% startsWith( 'Slot' ) ] %in% as.numeric( unlist( strsplit( x[['categoryincidence']], ',' ) ) ) ) %>% 
                            
                            t() %>% as.data.frame()
                          
                          names( selected_ind_df ) = paste0( 'selected_slot_', 1:40 )
                          
                          return( selected_ind_df )
                          
                        } ) %>% bind_rows() )
  
  
  
}

rr = Subhub_Summary( df0, df1, df2 )


