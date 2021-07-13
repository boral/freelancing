rm( list = ls() ) ; options( error = NULL )

library( dplyr ) ; library( lubridate ) ; library( stringr ) ; library( tidyr )

setwd( 'D:\\projects\\Kevin' )

input_df = read.csv( 'hub_data_input_sample.csv', stringsAsFactors = F )

#.... Function to clean and compute hub summary ....

#.... df_0 = input_df

Hub_Summary = function( df_0 ){
  
  items = paste0( 'c', 1:21 ) ; items_with_none = c( items, 'cate1_none', 'cate2_none' )
  
  df = df_0 %>% filter( audit != 0 ) %>%      #.... removing cases where audit is 0
    
    mutate( month = ( interval( ymd( "2012-01-01" ), ymd( date( dateR ) ) ) ) %/% months( 1 ),   #... adding month
            
            cate_display = str_replace_all( cate_display, 'c31', 'none' ),   #... fixing c31 with none
            
            cate_selected = str_replace_all( cate_selected, 'ccate', 'cate' )   #... fixing ccate with cate
            
            ) %>%
    
    #.... splitting items for screen 1 and 2
    
    bind_cols( ., str_split_fixed( df$cate_display, fixed('|'), n = 2 ) %>% as.data.frame() %>% rename( 'screen_1' = 'V1', 'screen_2' = 'V2' ) ) %>%
    
    #.... getting display list for each item
    
    bind_cols( ., 
               
               apply( df, MARGIN = 1, FUN = 
                        
                        function(x){ paste0( ifelse( items %in% unlist( str_split( x['screen_1'], ',' ) ), 1, '' ), 
                                             
                                             ifelse( items %in% unlist( str_split( x['screen_2'], ',' ) ), 2, '' ) ) } ) %>% t() %>% 
                 
                 as.data.frame() %>%  `colnames<-`( paste0( 'disp_', items ) ) ) %>%
    
    #.... getting selected list for each item
    
    bind_cols( ., 
               
               apply( df, MARGIN = 1, FUN = 
                        
                        function(x){ ifelse( items_with_none %in% unlist( str_split( x['cate_selected'], ',' ) ), 3, '' ) } ) %>% t() %>% 
                 
                 as.data.frame() %>%  `colnames<-`( paste0( 'chosen_', items_with_none ) ) )
  
  
  
}