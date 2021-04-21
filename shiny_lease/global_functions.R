#..... Function to generate cash flows ....

#.... lease_df_input = tail( my_lease_df_stored, 1 )

Generate_Cash_Flows = function( lease_df_input ){
  
  if( nrow( lease_df_input ) > 0 ){
    
  lease_payment_freq = switch( lease_df_input$`Lease_Payment_Frequency`,
                               
                               'monthly' = 'month', 'quarterly' = 'quarter', 'half_yearly' = '6 months', 'yearly' = 'year'
                               
                               )
  
  cashflow_df = data.table( LeaseId = lease_df_input$`Lease_ID`, 
                            
                            Date = seq.Date( as.Date( lease_df_input$`Rent_Effective_Date`, format="%d-%m-%Y" ),
                                             
                                             as.Date( lease_df_input$`End_Date`, format="%d-%m-%Y" ), lease_payment_freq ), Type = 'Rent' ) %>% mutate( 
                            
                            'Lease_Rental' = lease_df_input$Rent*( ( 1 + lease_df_input$`Escalation_Percent`/100 )^( as.integer( ( Date - as.Date( lease_df_input$`Rent_Effective_Date`, format="%d-%m-%Y" ) ) )%/%365 ) ), 
                            
                            'Upfront_Cost' = 0 ) %>% as.data.frame()
  
  if( lease_df_input$`Lease_Payable_At` == 'end' ){
    
    cashflow_df$Date = LastDayInMonth( cashflow_df$Date )
    
  } else{ cashflow_df$Date = lubridate::floor_date( cashflow_df$Date, 'month' ) }
  
  first_row = data.table( LeaseId = lease_df_input$`Lease ID`, Date = as.Date( lease_df_input$`Initial_Application_Date`, format="%d-%m-%Y" ),
                          
                          Type = 'Initial direct costs', 'Lease_Rental' = 0, 'Upfront_Cost' = lease_df_input$`Upfront_Payments` ) %>% as.data.frame()
  
  cashflow_df_1 = bind_rows( first_row, cashflow_df )
  
  #..... Dismantling costs and residual guarantee rows ....
  
  dismantling_df = data.table( LeaseId = lease_df_input$`Lease ID`, Date = rep( as.Date( lease_df_input$`Date_of_Dismantling`, format="%d-%m-%Y" ), 2 ),
                               
                               Type = c( 'Dismantling costs', 'Residual Value Guarantee' ), 
                               
                               'Lease_Rental' = c( lease_df_input$`Dismantling_Cost`, lease_df_input$`Residual_Guarantee_Payments` ), 
                               
                               'Upfront_Cost' = rep.int( 0, 2 ) ) %>% as.data.frame()
  
  cashflow_df_2 = bind_rows( cashflow_df_1, dismantling_df )
  
  #... correcting rent for proportional number of days in month ...
  
  cashflow_df_2$`Lease_Rental`[2] = round( cashflow_df_2$`Lease_Rental`[2]*( cashflow_df_2$Date[2] - cashflow_df_2$Date[1] + 1 )/lubridate::days_in_month( cashflow_df_2$Date[2] ) )
  
  cashflow_df_2$Total = cashflow_df_2$`Lease_Rental` + cashflow_df_2$`Upfront_Cost`
  
  cashflow_df_2[["Discount_Factor"]] = exp( -( lease_df_input$`Incremental_Borrowing_Rate_perc`/(100*365) )*( as.integer( cashflow_df_2$Date - cashflow_df_2$Date[1] ) ) )
  
  cashflow_df_2[["Present_Value"]] = round( cashflow_df_2$Total*cashflow_df_2[["Discount_Factor"]] )
  
  cashflow_df_2$Amortization = round( c( NA, diff( cashflow_df_2$Date ) )/as.integer( ( as.Date( lease_df_input$`End_Date`, format="%d-%m-%Y" ) - as.Date( lease_df_input$`Rent_Effective_Date`, format="%d-%m-%Y" ) ) )*sum( cashflow_df_2[["Present_Value"]] ) )
  
  cashflow_df_2 = cashflow_df_2 %>% mutate( 'Debit_Account' = 'Lease_Liability', 'Credit_Account' = 'Bank_Account', Amout = Total )
  
  #.... Total row ....
  
  total_df = data.table( LeaseId = 'Total', Date = lease_df_input$`End Date`, Type = '', 
                         
                         'Lease_Rental' = sum( cashflow_df_2$`Lease_Rental` ), 'Upfront_Cost' = lease_df_input$`Upfront_Payments`, 
                         
                         'Total' = sum( cashflow_df_2$Total ), 'Discount_Factor' = 0, 'Present_Value' = sum( cashflow_df_2$`Present_Value` ),
                         
                         'Amortization' = sum( cashflow_df_2$Amortization ) )
  
  cashflow_df_3 = bind_rows( cashflow_df_2, total_df )
  
  cashflow_df_3$Date[ nrow( cashflow_df_3 ) ] = ''
  
  cashflow_df_3[['Discount_Factor']][ nrow( cashflow_df_3 ) ] = ''
  
  return( cashflow_df_3 )
  
  } else{ return( data.frame() ) }
  
}


#..... Function to generate lease deposit ....

#.... my_advances_df_input = tail( my_advances_df_stored, 1 )  ;  lease_df_input = tail( my_lease_df_stored, 1 )

Generate_Lease_Deposit = function( my_advances_df_input, lease_df_input ){
  
  if( ( nrow( lease_df_input ) > 0 ) & ( nrow( my_advances_df_input ) > 0 ) ){
    
    lease_payment_freq = switch( lease_df_input$`Lease_Payment_Frequency`,
                                 
                                 'monthly' = 'month', 'quarterly' = 'quarter', 'half_yearly' = '6 months', 'yearly' = 'year'
                                 
                                 )
    
    lease_deposit_df = data.table( LeaseId = lease_df_input$`Lease_ID`, 
                              
                              Date = seq.Date( as.Date( lease_df_input$`Rent_Effective_Date`, format="%d-%m-%Y" ),
                                               
                                               as.Date( lease_df_input$`End_Date`, format="%d-%m-%Y" ), lease_payment_freq ), 'Lease Deposit' = -1 ) 
      
    if( lease_df_input$`Lease_Payable_At` == 'end' ){
      
      lease_deposit_df$Date = LastDayInMonth( lease_deposit_df$Date )
      
    } else{ lease_deposit_df$Date = lubridate::floor_date( lease_deposit_df$Date, 'month' ) }
    
    lease_deposit_df = lease_deposit_df %>%
      
      mutate( 'Installment' = 1:nrow( lease_deposit_df ),
        
              'Discount Factor' = exp( -( ( my_advances_df_input$Risk_Free_Rate + my_advances_df_input$Credit_Spread )/( 100*365 ) )*( as.integer( ( Date - as.Date( lease_df_input$`Rent_Effective_Date`, format="%d-%m-%Y" ) ) ) ) ), 
              
              'Right to Use deposit' = -1, 'Debit Account' = 'Deposit - Asset', 'Credit Account' = 'Interest Income', 'Present Value' = -1 ) %>% as.data.frame() # %>%
      
          #    dplyr::select( LeaseId, Date, Installment, 'Lease Deposit', 'Discount Factor', 'Present Value', 'Right to Use deposit', 'Debit Account', 'Credit Account' )
    
    first_row = data.table( LeaseId = lease_df_input$`Lease_ID`, Date = as.Date( lease_df_input$`Initial_Application_Date`, format="%d-%m-%Y" ),
                            
                            Installment = 0, 'Lease Deposit' = my_advances_df_input$Lease_Advance ) %>%
                            
                            mutate( 'Discount Factor' = exp( -( ( my_advances_df_input$Risk_Free_Rate + my_advances_df_input$Credit_Spread )/( 100*365 ) )*( as.integer( ( tail( lease_deposit_df$Date, 1 ) - as.Date( lease_df_input$`Rent_Effective_Date`, format="%d-%m-%Y" ) ) ) ) ) ) %>%
                            
                            mutate( 'Present Value' = round( .$'Lease Deposit'*.$'Discount Factor' ), 'Amortization' = 0, 'Imputed interest on deposit' = '',
                                    
                                    'Debit Account' = 'Deposit - Asset', 'Credit Account' = 'Bank Account' ) %>% 
      
                            mutate( 'Carrying value of deposit' = .$'Present Value', Amount = .$'Lease Deposit',
                                    
                                    'Right to Use deposit' = round( .$'Lease Deposit' - .$'Present Value' ) ) %>% as.data.frame()
    
    lease_deposit_df_1 = bind_rows( first_row, lease_deposit_df )
    
    lease_deposit_df_1$date_diff = c( NA, diff( lease_deposit_df_1$Date ) )
    
    car_val_deposit = NULL
    
    for( i in 1:nrow( lease_deposit_df_1 ) ){
      
      if( i == 1 ){ car_val_deposit[i] = lease_deposit_df_1$`Carrying value of deposit`[1] } else{
        
        car_val_deposit[i] = car_val_deposit[i-1]*exp( ( ( my_advances_df_input$Risk_Free_Rate + my_advances_df_input$Credit_Spread )/( 100*365 ) )*lease_deposit_df_1$date_diff[i] )
        
      }
      
    }
    
    lease_deposit_df_1$`Carrying value of deposit` = round( car_val_deposit )
    
    lease_deposit_df_1$`Imputed interest on deposit` = c( -1, round( diff( lease_deposit_df_1$`Carrying value of deposit` ) ) )
    
    lease_deposit_df_1$Amortization[ is.na( lease_deposit_df_1$Amortization ) ] = round( head( lease_deposit_df_1$`Right to Use deposit`, 1 )/tail( lease_deposit_df_1$Installment, 1 ) )
    
    lease_deposit_df_1$Amount = lease_deposit_df_1$`Imputed interest on deposit`
    
    lease_deposit_df_1$Amount[1] = lease_deposit_df_1$`Lease Deposit`[1]
    
    lease_deposit_df_2 = lease_deposit_df_1 %>% dplyr::select( LeaseId, Date, Installment, 'Lease Deposit', 'Discount Factor', 'Present Value',
                                                        
                                                        'Carrying value of deposit', 'Imputed interest on deposit', 'Right to Use deposit',
                                                        
                                                        'Amortization', 'Debit Account', 'Credit Account', 'Amount' )
    
    #.... Total row ....
    
    total_row = data.table( LeaseId = '', Date = lease_deposit_df_2$Date[1], Installment = -1, 'Lease Deposit' = lease_deposit_df_1$`Lease Deposit`[1],
                            
                            'Discount Factor' = -1, 'Present Value' = lease_deposit_df_2$`Present Value`[1], 'Carrying value of deposit' = -1, 
                            
                            'Imputed interest on deposit' = sum( as.numeric( lease_deposit_df_2$`Imputed interest on deposit` ), na.rm = T ),
                            
                            'Right to Use deposit' = -1, 'Amortization' = sum( as.numeric( lease_deposit_df_2$Amortization, na.rm = T ) ),
                            
                            'Debit Account' = '', 'Credit Account' = '', Amount = -1 )
    
    lease_deposit_df_final = bind_rows( lease_deposit_df_2, total_row )
    
    lease_deposit_df_final$Installment[ lease_deposit_df_final$Installment == -1 ] = ''
    
    lease_deposit_df_final$`Lease Deposit`[ lease_deposit_df_final$`Lease Deposit` == -1 ] = ''
    
    lease_deposit_df_final$`Present Value`[ lease_deposit_df_final$`Present Value` == -1 ] = ''
    
    lease_deposit_df_final$`Carrying value of deposit`[ nrow( lease_deposit_df_final ) ] = ''
    
    lease_deposit_df_final$`Right to Use deposit`[ lease_deposit_df_final$`Right to Use deposit` == -1 ] = ''
    
    lease_deposit_df_final$`Imputed interest on deposit`[1] = ''
    
    return( lease_deposit_df_final )
    
  } else{
    
    return( data.frame() )
    
  }
  
}



