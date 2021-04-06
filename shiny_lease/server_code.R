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
  
  my_lease_df_input = data.table( 'Company' = input$add_lease_company, 'Valuation Date' = input$add_lease_valuation_date,
                                  
                                  'Initial Application Date' = input$add_lease_initial_application_date, 'Lease ID' = input$add_lease_lease_id,
                                  
                                  'Asset Class' = input$add_lease_asset_class, 'Lessor Name' = input$add_lease_lessor_name,
                                  
                                  'Address' = input$add_lease_address, 'Start Date' = input$add_lease_start_date,
                                  
                                  'End Date' = input$add_lease_end_date, 'Upfront Payments' = input$add_lease_upfront_payments,
                                  
                                  'Rent Effective Date' = input$add_lease_rent_effective_date, 'Rent' = input$add_lease_rent,
                                  
                                  'GST (%)' = input$add_lease_gst, 'Incremental Borrowing Rate (%)' = input$add_lease_incremental_borrowing_rate,
                                  
                                  'Escalation Percent (%)' = input$add_lease_escalation_percent, 'Escalation Tenure' = input$add_lease_escalation_tenure,
                                  
                                  'Lease Payment Frequency' = input$add_lease_lease_payment_freq, 'Lease Payable At' = input$add_lease_lease_payable_at,
                                  
                                  'Dismantling Cost' = input$add_lease_dismantling_cost, 'Date of Dismantling' = input$add_lease_date_of_dismantling,
                                  
                                  'Residual Guarantee Payments' = input$add_lease_residual_guarantee_payments,
                                  
                                  'Avail Exemption' = input$add_lease_avail_exemption, 'Exemption Type' = input$add_lease_exemption_type,
                                  
                                  'Transition Method' = input$add_lease_transition_method, 'Credit Rating' = input$add_lease_credit_rating,
                                  
                                  'Use Cash Flow' = input$add_lease_use_cash_flow )
  
  updated_my_lease_df = bind_rows( RV$my_lease_df_final, my_lease_df_input )
  
  fwrite( updated_my_lease_df, 'my_lease_df_stored.csv' )

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







