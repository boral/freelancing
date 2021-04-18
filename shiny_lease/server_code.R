#...... Server ......

#..... Asset class ....

RV_assetclass = reactiveValues( my_asset_class_final = my_asset_class_stored )

output$my_asset_class_df = DT::renderDT({
  
  RV_assetclass$my_asset_class_final
  
})

observeEvent( input$asset_class_submit_button, {
  
  my_asset_class_df_0 = data.table( 'Asset Class' = input$asset_class_input )
  
  #..... Read my asset class df ...
  
  if( !file.exists( 'my_asset_class_df_stored.csv' ) ){
    
    my_asset_class_stored = data.frame()
    
  } else{
    
    my_asset_class_stored = fread( 'my_asset_class_df_stored.csv' )
    
  }
  
  my_asset_class_df_updated = bind_rows( my_asset_class_stored, my_asset_class_df_0 )
  
  fwrite( my_asset_class_df_updated, 'my_asset_class_df_stored.csv' )
  
  RV_assetclass$my_asset_class_final = fread( 'my_asset_class_df_stored.csv' )
  
  updateSelectInput( session, 'add_lease_asset_class', 'Asset Class', choices = my_asset_class_df_updated[['Asset Class']] )
  
})

#..... Generating lease table ....

RV = reactiveValues( my_lease_df_final = my_lease_df_stored )

output$my_lease_df = DT::renderDT({
  
  RV$my_lease_df_final
  
})

observeEvent( input$add_lease_submit_button, {
  
  my_lease_df_input = data.table( 'Company' = input$add_lease_company, 'Valuation_Date' = input$add_lease_valuation_date,
                                  
                                  'Initial_Application_Date' = input$add_lease_initial_application_date, 'Lease_ID' = input$add_lease_lease_id,
                                  
                                  'Asset_Class' = input$add_lease_asset_class, 'Lessor_Name' = input$add_lease_lessor_name,
                                  
                                  'Address' = input$add_lease_address, 'Start_Date' = input$add_lease_start_date,
                                  
                                  'End_Date' = input$add_lease_end_date, 'Upfront_Payments' = input$add_lease_upfront_payments,
                                  
                                  'Rent_Effective_Date' = input$add_lease_rent_effective_date, 'Rent' = input$add_lease_rent,
                                  
                                  'GST_perc' = input$add_lease_gst, 'Incremental_Borrowing_Rate_perc' = input$add_lease_incremental_borrowing_rate,
                                  
                                  'Escalation_Percent' = input$add_lease_escalation_percent, 'Escalation_Tenure' = input$add_lease_escalation_tenure,
                                  
                                  'Lease_Payment_Frequency' = input$add_lease_lease_payment_freq, 'Lease_Payable_At' = input$add_lease_lease_payable_at,
                                  
                                  'Dismantling_Cost' = input$add_lease_dismantling_cost, 'Date_of_Dismantling' = input$add_lease_date_of_dismantling,
                                  
                                  'Residual_Guarantee_Payments' = input$add_lease_residual_guarantee_payments,
                                  
                                  'Avail_Exemption' = input$add_lease_avail_exemption, 'Exemption_Type' = input$add_lease_exemption_type,
                                  
                                  'Transition_Method' = input$add_lease_transition_method, 'Credit_Rating' = input$add_lease_credit_rating,
                                  
                                  'Use_Cash_Flow' = input$add_lease_use_cash_flow )
  
  updated_my_lease_df = bind_rows( RV$my_lease_df_final, my_lease_df_input )
  
  write.csv( updated_my_lease_df, 'my_lease_df_stored.csv', stringAsFactors = F )

  RV$my_lease_df_final = fread( 'my_lease_df_stored.csv' )
  
})

observeEvent( input$my_lease_upload_button, {

  uploaded_my_lease_df = read.csv( input$my_lease$datapath )

  fwrite( uploaded_my_lease_df, 'my_lease_df_stored.csv' )

  RV$my_lease_df_final = fread( 'my_lease_df_stored.csv' )

})


#..... Generating cashflow tables ....


RV_cashflow = reactiveValues( my_cashflow_df_final = my_cashflow_df_stored )

output$my_cash_flows_df = DT::renderDT({

  RV_cashflow$my_cashflow_df_final

})

observeEvent( input$add_lease_submit_button, { 
  
  RV_cashflow$my_cashflow_df_final = Generate_Cash_Flows( tail( RV$my_lease_df_final, 1 ) )

})

observeEvent( input$my_lease_upload_button, {

  RV_cashflow$my_cashflow_df_final = Generate_Cash_Flows( tail( RV$my_lease_df_final, 1 ) )

})


#..... Generating Asset - Right to use table ....

#... asset_right_of_use_df_0 = my_cashflow_df_stored

output$asset_right_of_use_df = DT::renderDT({

  asset_right_of_use_df_0 = RV_cashflow$my_cashflow_df_final
  
  asset_right_of_use_df_1_firstrow = data.table( LeaseID = asset_right_of_use_df_0$LeaseId[2], Installment = asset_right_of_use_df_0$Date[1],
                                        
                                        'Date' = 0, 'Right to Use Balance' = tail( asset_right_of_use_df_0$Present_Value, 1 ),
                                        
                                        'Amortisation of RTU' = 0, 'Right to use Closing Balance' = tail( asset_right_of_use_df_0$Present_Value, 1 ),
                                        
                                        'Debit Account' = 'Right to Use', 'Credit Account' = 'Lease Liability' )
  
  asset_right_of_use_df_1_next_rows = tail( asset_right_of_use_df_0, -1 ) %>% filter( Type == 'Rent' )
  
  asset_right_of_use_df_2 = data.table( LeaseID = asset_right_of_use_df_1_next_rows$LeaseId, Installment = asset_right_of_use_df_1_next_rows$Date,
                                        
                                        Date = seq.int( 1, nrow( asset_right_of_use_df_1_next_rows ), 1 ),
                                        
                                        'Amortisation of RTU' = asset_right_of_use_df_1_next_rows$Amortization,
                                        
                                        'Debit Account' = 'Depreciation', 'Credit Account' = 'Right to Use' ) %>%
    
                           mutate( 'Right to use Closing Balance' = tail( asset_right_of_use_df_0$Present_Value, 1 ) - cumsum( .$'Amortisation of RTU' ) ) %>%
  
                           mutate( 'Right to Use Balance' = shift( .$'Right to use Closing Balance' ) )
  
  asset_right_of_use_df_2[[ 'Right to use Closing Balance' ]][ nrow( asset_right_of_use_df_2 ) ] = 0
  
  asset_right_of_use_df_2[[ 'Right to Use Balance' ]][1] = tail( asset_right_of_use_df_0$Present_Value, 1 )
  
  asset_right_of_use_df_2$Date = seq.int( 1, nrow( asset_right_of_use_df_2 ), 1 )
  
  asset_right_of_use_df_3 = asset_right_of_use_df_2 %>% dplyr::select( names( asset_right_of_use_df_1_firstrow ) )
  
  asset_right_of_use_df_4 = bind_rows( asset_right_of_use_df_1_firstrow, asset_right_of_use_df_3 )
  
  asset_right_of_use_df_4$Amount = asset_right_of_use_df_4[[ 'Right to Use Balance' ]]
  
  #.... Total row ....
  
  total_df = data.table( LeaseID = 'Total', Installment = asset_right_of_use_df_4$Installment[1], Date = NA, 
                         
                         'Right to Use Balance' = NA, 'Amortisation of RTU' = sum( asset_right_of_use_df_4[["Amortisation of RTU"]] ), 
                         
                         'Right to use Closing Balance' = NA, 'Debit Account' = NA, 
                         
                         'Credit Account' = '', 'Amount' = NA )
  
  asset_right_of_use_df_5 = bind_rows( asset_right_of_use_df_4, total_df )
  
  asset_right_of_use_df_5$Installment[ nrow( asset_right_of_use_df_5 ) ] = ''
  
  asset_right_of_use_df_5
    
})


#..... Generating Liability table ....

#... lease_liability_df_0 = my_cashflow_df_stored

output$lease_liability_df = DT::renderDT({
  
  lease_liability_df_0 = RV_cashflow$my_cashflow_df_final
  
  lease_liability_df_1_firstrow = data.table( LeaseID = lease_liability_df_0$LeaseId[2], Installment = lease_liability_df_0$Date[1],
                                                 
                                                 'Date' = 0, 'Lease Liability Opening Balance' = tail( lease_liability_df_0$Present_Value, 1 ),
                                                 
                                                 'Finance Cost' = 0, 'Lease Payments' = head( lease_liability_df_0$Present_Value, 1 ),
                                                 
                                                 'Debit Account' = '', 'Credit Account' = '' ) %>%
    
                                  mutate( 'Balance Liability' = .$'Lease Liability Opening Balance' + .$'Finance Cost' - .$'Lease Payments' ) %>%
    
                                  dplyr::select( 'LeaseID', 'Installment', 'Date', 'Lease Liability Opening Balance', 'Finance Cost',
                   
                                                 'Lease Payments', 'Balance Liability', 'Debit Account', 'Credit Account' )
  
  lease_liability_df_1_next_rows = tail( lease_liability_df_0, -1 ) %>% filter( Type == 'Rent' )
  
  lease_liability_df_2 = data.table( LeaseID = lease_liability_df_1_next_rows$LeaseId, Installment = lease_liability_df_1_next_rows$Date,
                                        
                                        Date = seq.int( 1, nrow( lease_liability_df_1_next_rows ), 1 ),
                                        
                                        'Lease Payments' = lease_liability_df_1_next_rows$Lease_Rental,
                                        
                                        'Debit Account' = 'Interest Expense', 'Credit Account' = 'Lease Liability' ) %>%
    
                        mutate( installment_diff = as.numeric( c( lease_liability_df_0$Date[2] - lease_liability_df_0$Date[1], diff( Installment ) ) ) )
  
  lease_liab_open_bal = finance_cost = bal_liab = NULL
  
  incremental_borrowing_rate_0 = ifelse( is.na( input$add_lease_incremental_borrowing_rate ), my_lease_df_stored$Incremental_Borrowing_Rate_perc, input$add_lease_incremental_borrowing_rate )
  
  for( i in 1:nrow( lease_liability_df_2 ) ){
    
    if( i == 1 ){
      
      lease_liab_open_bal[i] = lease_liability_df_1_firstrow$`Balance Liability`
      
    } else{
      
      lease_liab_open_bal[i] = bal_liab[i-1]
      
    }
    
    finance_cost[i] = lease_liab_open_bal[i]*exp( ( incremental_borrowing_rate_0/( 100*365 ) )*lease_liability_df_2$installment_diff[i] ) - lease_liab_open_bal[i]
    
    bal_liab[i] = lease_liab_open_bal[i] + finance_cost[i] - lease_liability_df_2$`Lease Payments`[i]
    
  }
  
  lease_liability_df_3  = lease_liability_df_2 %>% mutate( 'Lease Liability Opening Balance' = lease_liab_open_bal,
                                                           
                            'Finance Cost' = finance_cost, 'Balance Liability' = bal_liab ) %>% dplyr::select( -installment_diff ) %>%
    
                          dplyr::select( 'LeaseID', 'Installment', 'Date', 'Lease Liability Opening Balance', 'Finance Cost',
                                         
                                         'Lease Payments', 'Balance Liability', 'Debit Account', 'Credit Account' )
  
  lease_liability_df_4 = bind_rows( lease_liability_df_1_firstrow, lease_liability_df_3 )
  
  #.... Last 2 rows ...
  
  lease_liab_last_2_rows = data.table( LeaseID = '', Installment = tail( lease_liability_df_0, 3 )$Date[1:2],
                                       
                                       Date = -1, 
                                       
                                       'Lease Liability Opening Balance' = c( tail( lease_liability_df_4$`Balance Liability`, 1 ), tail( lease_liability_df_0, 2 )$Lease_Rental[1] ),
                                       
                                       'Finance Cost' = rep( 0, 2 ), 'Lease Payments' = tail( lease_liability_df_0, 3 )$Lease_Rental[1:2],
                                       
                                       'Debit Account' = '', 'Credit Account' = ''  ) %>%
    
                          mutate( 'Balance Liability' = .$'Lease Liability Opening Balance' + .$'Finance Cost' - .$'Lease Payments' ) %>%
    
                         dplyr::select( 'LeaseID', 'Installment', 'Date', 'Lease Liability Opening Balance', 'Finance Cost',
                   
                                        'Lease Payments', 'Balance Liability', 'Debit Account', 'Credit Account' )
  
  lease_liability_df_5 = bind_rows( lease_liability_df_4, lease_liab_last_2_rows )
  
  #..... Total row .....
  
  total_row = data.table( LeaseID = '', Installment = Sys.Date(), Date = -1, 'Lease Liability Opening Balance' = -1, 'Finance Cost' = -1,
                          
                          'Lease Payments' = sum( lease_liability_df_5$`Lease Payments`, na.rm = T ), 'Balance Liability' = -1, 'Debit Account' = '', 'Credit Account' = '' )
  
  lease_liability_df_6 = bind_rows( lease_liability_df_5, total_row )
  
  lease_liability_df_6$`Lease Liability Opening Balance` = round( lease_liability_df_6$`Lease Liability Opening Balance` )
  
  lease_liability_df_6$`Finance Cost` = round( lease_liability_df_6$`Finance Cost` )
  
  lease_liability_df_6$`Balance Liability` = round( lease_liability_df_6$`Balance Liability` )
  
  lease_liability_df_6$Date[ which( lease_liability_df_6$Date == -1 ) ] = ''
  
  lease_liability_df_6$`Lease Liability Opening Balance`[ which( lease_liability_df_6$`Lease Liability Opening Balance` == -1 ) ] = ''
  
  lease_liability_df_6$`Finance Cost`[ which( lease_liability_df_6$`Finance Cost` == -1 ) ] = ''
  
  lease_liability_df_6$`Balance Liability`[ which( lease_liability_df_6$`Balance Liability` == -1 ) ] = ''
  
  return( lease_liability_df_6 )
  
})




