#..... Function to generate cash flows ....

#.... lease_df_input = tail( my_lease_df_stored, 1 )

Generate_Cash_Flows = function( lease_df_input ){
  
  lease_payment_freq = switch( lease_df_input$`Lease Payment Frequency`,
                               
                               'monthly' = 'month', 'quarterly' = 'quarter', 'half_yearly' = '6 months', 'yearly' = 'year'
                               
                               )
  
  cashflow_df = data.table( LeaseId = lease_df_input$`Lease ID`, 
                            
                            Date = seq.Date( lease_df_input$`Rent Effective Date`, 
                                             
                                             lease_df_input$`End Date`, lease_payment_freq ), Type = 'Rent' ) %>% mutate( 
                            
                            'Lease Rental' = lease_df_input$Rent*( ( 1 + lease_df_input$`Escalation Percent (%)`/100 )^( as.integer( ( Date - as.Date( as.character( lease_df_input$`Rent Effective Date` ) ) ) )%/%365 ) ), 
                            
                            'Upfront Cost' = 0 )
  
  if( lease_df_input$`Lease Payable At` == 'end' ){
    
    cashflow_df$Date = LastDayInMonth( cashflow_df$Date )
    
  } else{ cashflow_df$Date = lubridate::floor_date( cashflow_df$Date, 'month' ) }
  
  first_row = data.table( LeaseId = lease_df_input$`Lease ID`, Date = lease_df_input$`Initial Application Date`,
                          
                          Type = 'Initial direct costs', 'Lease Rental' = 0, 'Upfront Cost' = lease_df_input$`Upfront Payments` )
  
  cashflow_df_1 = bind_rows( first_row, cashflow_df )
  
  #..... Dismantling costs and residual guarantee rows ....
  
  dismantling_df = data.table( LeaseId = lease_df_input$`Lease ID`, Date = rep( lease_df_input$`Date of Dismantling`, 2 ),
                               
                               Type = c( 'Dismantling costs', 'Residual Value Guarantee' ), 
                               
                               'Lease Rental' = c( lease_df_input$`Dismantling Cost`, lease_df_input$`Residual Guarantee Payments` ), 
                               
                               'Upfront Cost' = rep.int( 0, 2 ) )
  
  cashflow_df_2 = bind_rows( cashflow_df_1, dismantling_df )
  
  #... correcting rent for proportional number of days in month ...
  
  cashflow_df_2$`Lease Rental`[2] = round( cashflow_df_2$`Lease Rental`[2]*( cashflow_df_2$Date[2] - cashflow_df_2$Date[1] + 1 )/lubridate::days_in_month( cashflow_df_2$Date[2] ) )
  
  cashflow_df_2$Total = cashflow_df_2$`Lease Rental` + cashflow_df_2$`Upfront Cost`
  
  cashflow_df_2[["Discount Factor"]] = exp( -( lease_df_input$`Incremental Borrowing Rate (%)`/(100*365) )*( cashflow_df_2$Date - cashflow_df_2$Date[1] ) )
  
  cashflow_df_2[["Present Value"]] = round( cashflow_df_2$Total*cashflow_df_2[["Discount Factor"]] )
  
  cashflow_df_2$Amortization = round( c( NA, diff( cashflow_df_2$Date ) )/( lease_df_input$`End Date` - lease_df_input$`Rent Effective Date` )*sum( cashflow_df_2[["Present Value"]] ) )
  
  cashflow_df_2 = cashflow_df_2 %>% mutate( 'Debit Account' = 'Lease Liability', 'Credit Account' = 'Bank Account', Amout = Total )
  
  #.... Total row ....
  
  
  
  return( cashflow_df_2 )
  
}


