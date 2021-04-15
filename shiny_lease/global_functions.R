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


#..... Function to generate asset right to use ....

#.... lease_df_input = tail( my_lease_df_stored, 1 )

# Generate_Asset_Right_to_Use




