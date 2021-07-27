rm( list = ls() ) ; options( error = NULL )

library( dplyr )

setwd( 'D:\\projects\\Kevin\\combined\\test_files' )

input_df_0 = read.csv( 'IN_hub_95.csv', stringsAsFactors = F )

set.seed( 123 )

sv_new = audit_1 = NULL

attach( input_df_0 )

for( i in 1:nrow( input_df_0 ) ){
  
  if( sv[i] == 21 ){
    
    sv_new[i] = switch( as.character( age[i] ), 
                        
                        '2' = { ifelse( runif(1) <= 0.86, 25, 21 ) },
                        
                        '3' = { ifelse( runif(1) <= 0.87, 25, 21 ) },
                        
                        '4' = { ifelse( runif(1) <= 0.81, 25, 21 ) },
                        
                        21
                        
                      )
    
  } else{
    
          if( sv[i] == 26 ){
            
            sv_new[i] = switch( as.character( age[i] ), 
                                
                                '2' = { ifelse( runif(1) <= 0.71, 29, 26 ) },
                                
                                '3' = { ifelse( runif(1) <= 0.74, 29, 26 ) },
                                
                                '4' = { ifelse( runif(1) <= 0.64, 29, 26 ) },
                                
                                26
                                
            )
            
          } else{ sv_new[i] = sv[i] }    #... No change when sv is not equal to 21 or 26
    
  }
  
  audit_1[i] = ifelse( any( is.na( lg[i] ), is.na( pv[i] ), is.na( gd[i] ), is.na( age[i] ), is.na( imc[i] ), is.na( dateR[i] ),
                          
                          age[i] < 1, age[i] > 6, sv_new[i] == 31, sv_new[i] == 36 ), 0, 1 )
  
}

detach( input_df_0 )

df = input_df_0 %>% select( -c( 'ï..token_hub', 'audit', 'weight', 'cate_display', 'cate_selected', 'dateR' ) ) %>% 
  
                    mutate( sv_new = sv_new, audit = audit_1 )


#.... Weight Computation Recursive Function .....

#.... hub_df = df ; num_iter = 10

Compute_Weight = function( hub_df, num_iter = 10 ){
  
  hub_df$weight = hub_df$audit
  
  vars = c( 'lg_pv', 'gd', 'age', 'imc', 'orig_lto', 'new_panel' )
  
  df_gender = df_age = df_income = NULL
  
  for( j in 1:num_iter ){
    
    #.... For gender ....
    
    df_gender_0 = hub_df %>% group_by( gd ) %>% summarise( sum_weight = sum( weight ) ) %>% 
      
                                mutate( perc_weight = round( 100*sum_weight/sum( sum_weight ) ) )
    
    df_gender_0 = df_gender_0[ !is.na( df_gender_0$gd ), ]
    
    gender_exp = data.frame( gd = 1:2, expctd_perc = c( 50, 50 ) )
    
    df_gender_1 = left_join( df_gender_0, gender_exp, by = 'gd' ) %>% 
      
                    mutate( diff_perc = perc_weight - expctd_perc, 
                            
                            adj = ifelse( abs( diff_perc ) < 5, 1, ( expctd_perc/perc_weight )^(1/3) ) )
    
    df_gender[[j]] = df_gender_1
    
    
    #.... For age .....
    
    df_age_0 = hub_df %>% group_by( age ) %>% summarise( sum_weight = sum( weight ) ) %>% 
      
                             mutate( perc_weight = round( 100*sum_weight/sum( sum_weight ) ) )
    
    df_age_0 = df_age_0[ !is.na( df_age_0$age ), ]
    
    age_exp = data.frame( age = 1:6, expctd_perc = c( 0, 12, 16, 24, 34, 15 ) )
    
    df_age_1 = left_join( df_age_0, age_exp, by = 'age' ) %>%
      
                  mutate( diff_perc = perc_weight - expctd_perc, 
              
                          adj = ifelse( abs( diff_perc ) < 5, 1, ( expctd_perc/perc_weight )^(1/3) ) )
    
    df_age[[j]] = df_age_1
    
    
    #.... For income .....
    
    df_income_0 = hub_df %>% group_by( imc ) %>% summarise( sum_weight = sum( weight ) ) %>% 
      
                         mutate( perc_weight = round( 100*sum_weight/sum( sum_weight ) ) )
    
    df_income_0 = df_income_0[ !is.na( df_income_0$imc ), ]
    
    imc_exp = data.frame( imc = 1:5, expctd_perc = c( 18, 25, 25, 32, 0 ) )
    
    df_income_1 = left_join( df_income_0, imc_exp, by = 'imc' ) %>%
      
                         mutate( diff_perc = perc_weight - expctd_perc, 
              
                                  adj = ifelse( abs( diff_perc ) < 5, 1, ( expctd_perc/perc_weight )^(1/3) ) )
    
    df_age[[j]] = df_income_1
    
    
    #.... For orig lto blend ..... age = 2:4 .....
    
    
    
  }

}







