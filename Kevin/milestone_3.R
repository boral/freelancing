rm( list = ls() ) ; options( error = NULL )

library( dplyr )

setwd( 'D:\\projects\\Kevin\\combined\\test_files' )

input_df_0 = read.csv( 'IN_hub_95.csv', stringsAsFactors = F )

set.seed( 123 )

sv_new = NULL

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
  
}

detach( input_df_0 )

df = input_df_0 %>% select( -c( 'audit', 'weight' ) ) %>% mutate( sv_new = sv_new )

#.... need to vectorize any()

# df$audit = ifelse( any( df$lg == '', df$pv == '', df$gd == '', df$age == '', df$imc == '', df$dateR == '', 
#                     
#                     df$age < 1, df$age > 6, df$sv_new == 31, df$sv_new == 36 ), 0, 1 )


#.... Weight Computation Recursive Function .....

#.... hub_df = df

Compute_Weight = function( hub_df ){
  
  hub_df$weight = hub_df$audit
  
}







