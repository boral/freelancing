#...... Server ......

sensor_names_to_update = observeEvent( input$data_submit_button, {
  
  folder_path = as.character( input$data_path )
  
  if( !dir.exists( folder_path ) || folder_path == '' ){

    sendSweetAlert( session = session, 'Oops!', 'Folder path does not exists. Please check.', type = 'info' )
    
  } else{
    
    file_names_0 = list.files( folder_path, full.names = T )
    
    mag_numbers = read.csv( file_names_0[1], stringsAsFactors = F ) %>% select( 'Mag' ) %>% unlist() %>% unique()
    
    str( mag_numbers )
    
    updateSelectInput( session, 'start_mag_no', choices = mag_numbers, selected = mag_numbers[1] )
    
    updateSelectInput( session, 'end_mag_no', choices = mag_numbers, selected = mag_numbers[2] )
    
  }
  
} )

RV = reactiveValues( my_lease_df_final = my_lease_df_stored )

# my_lease_df_0 = eventReactive( input$add_lease_submit_button, {
#   
#   my_lease_df_input = data.table( 'Company' = input$add_lease_company, 'Valuation Date' = input$add_lease_valuation_date,
#                                   
#                                   'Initial Application Date' = input$add_lease_initial_application_date, 'Lease ID' = input$add_lease_lease_id,
#                                   
#                                   'Asset Class' = input$add_lease_asset_class, 'Lessor Name' = input$add_lease_lessor_name,
#                                   
#                                   'Address' = input$add_lease_address, 'Start Date' = input$add_lease_start_date,
#                                   
#                                   'End Date' = input$add_lease_end_date, 'Upfront Payments' = input$add_lease_upfront_payments,
#                                   
#                                   'Rent Effective Date' = input$add_lease_rent_effective_date, 'Rent' = input$add_lease_rent,
#                                   
#                                   'GST (%)' = input$add_lease_gst, 'Incremental Borrowing Rate (%)' = input$add_lease_incremental_borrowing_rate,
#                                   
#                                   'Escalation Percent (%)' = input$add_lease_escalation_percent, 'Escalation Tenure' = input$add_lease_escalation_tenure,
#                                   
#                                   'Lease Payment Frequency' = input$add_lease_lease_payment_freq, 'Lease Payable At' = input$add_lease_lease_payable_at,
#                                   
#                                   'Avail Exemption' = input$add_lease_avail_exemption, 'Exemption Type' = input$add_lease_exemption_type,
#                                   
#                                   'Transition Method' = input$add_lease_transition_method, 'Credit Rating' = input$add_lease_credit_rating,
#                                   
#                                   'Use Cash Flow' = input$add_lease_use_cash_flow )
#   
#   updated_my_lease_df = bind_rows( my_lease_df_stored, my_lease_df_input )
#   
#   fwrite( updated_my_lease_df, 'my_lease_df_stored.csv' )
#   
# })

output$my_lease_df = DT::renderDT({
  
  RV$my_lease_df_final
  
})

observeEvent( input$add_lease_submit_button, {
  
  my_lease_df_input = data.table( 'Company' = input$add_lease_company, 'Valuation Date' = input$add_lease_valuation_date,
                                  
                                  'Initial Application Date' = input$add_lease_initial_application_date, 'Lease ID' = input$add_lease_lease_id,
                                  
                                  'Asset Class' = input$add_lease_asset_class, 'Lessor Name' = input$add_lease_lessor_name,
                                  
                                  'Address' = input$add_lease_address, 'Start Date' = input$add_lease_start_date,
                                  
                                  'End Date' = input$add_lease_end_date, 'Upfront Payments' = input$add_lease_upfront_payments,
                                  
                                  'Rent Effective Date' = input$add_lease_rent_effective_date, 'Rent' = input$add_lease_rent,
                                  
                                  'GST (%)' = input$add_lease_gst, 'Incremental Borrowing Rate (%)' = input$add_lease_incremental_borrowing_rate,
                                  
                                  'Escalation Percent (%)' = input$add_lease_escalation_percent, 'Escalation Tenure' = input$add_lease_escalation_tenure,
                                  
                                  'Lease Payment Frequency' = input$add_lease_lease_payment_freq, 'Lease Payable At' = input$add_lease_lease_payable_at,
                                  
                                  'Avail Exemption' = input$add_lease_avail_exemption, 'Exemption Type' = input$add_lease_exemption_type,
                                  
                                  'Transition Method' = input$add_lease_transition_method, 'Credit Rating' = input$add_lease_credit_rating,
                                  
                                  'Use Cash Flow' = input$add_lease_use_cash_flow )
  
  updated_my_lease_df = bind_rows( RV$my_lease_df_final, my_lease_df_input )
  
  fwrite( updated_my_lease_df, 'my_lease_df_stored.csv' )

  RV$my_lease_df_final = fread( 'my_lease_df_stored.csv' )
  
})




