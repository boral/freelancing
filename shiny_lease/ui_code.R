#.... UI .....

app_header = dashboardHeader( title = 'Lease Dashboard' )

app_sidebar =  dashboardSidebar( collapsed = F,
                                       
                                sidebarMenu(

                              #    menuItem( strong( '  Admin' ), tabName = 'admin' ),
                                  
                                  menuItem( strong( '  Asset Class' ), tabName = 'asset_class' ),
                                  
                                  menuItem( strong( '  Lease' ), startExpanded = TRUE,
                                            
                                           menuSubItem( 'My Lease', tabName = 'my_lease', selected = T )
                                  ),
                                  
                                  menuItem( strong( '  Advances' ), startExpanded = TRUE,
                                            
                                            menuSubItem( 'My Advances', tabName = 'my_advances', selected = T )
                                  ),
                                  
                                  menuItem( strong( '  Cash Flows' ), startExpanded = TRUE,
                                            
                                            menuSubItem( 'My Cash Flows', tabName = 'my_cash_flows' )
                                  ),
                                  
                                  menuItem( strong( '  Calculations' ), startExpanded = TRUE,
                                            
                                            menuSubItem( 'Lease Deposits', tabName = 'lease_deposits' ),
                                            
                                            menuSubItem( 'Asset Right-of-Use', tabName = 'asset_right_of_use' ),
                                            
                                            menuSubItem( 'Lease Liability', tabName = 'lease_liability' ),
                                            
                                            menuSubItem( 'Right to Use Deposit', tabName = 'right_to_use_deposit' )
                                            
                                  )
                                         
                                )
                                       
)

app_body =  dashboardBody(
  
#  useSweetAlert(),  #.... Set up sweetalert
  
  tabItems(
    
    #..... graph tab ....
    
    tabItem( tabName = 'admin',
             
             fluidRow(
               
               box( title = 'Valuation Date', status = 'success', collapsible = T, width = 12, solidHeader = T,
                    
                    column( 2, dateInput( 'valuation_data_input', 'Valuation Date' ) ), br(),
                    
                    column( 2, checkboxInput( 'active_checkbox', label = 'Active', value = F ) ),
                    
                    column( 2, checkboxInput( 'latest_checkbox', label = 'Latest', value = F ) ),
                    
                    column( 2, align = 'left',
                            
                            actionButton( 'admin_submit_button', 'SUBMIT' ),
                            
                            tags$style( "#admin_submit_button { vertical-align: middle; height: 30px; width: 70%; font-size: 15px;color: white;background-color:#1B618D;border-color: #374645 }" )
                            
                    )
                    
               )
               
             ),

             fluidRow(

               box( title = 'My Valuation Dates', status = 'success', collapsible = T, width = 12, solidHeader = T,

                    DT::dataTableOutput( outputId = "my_data_table" ) )

             )
             

    ),
    
    tabItem( tabName = 'asset_class',
             
             fluidRow(
               
               box( title = 'Asset Class', status = 'success', collapsible = T, width = 12, solidHeader = T,
                    
                    column( 4, textInput( 'asset_class_input', 'Asset Class' ) ), br(),
 
                    column( 2, align = 'left',
                            
                            actionButton( 'asset_class_submit_button', 'SUBMIT' ),
                            
                            tags$style( "#asset_class_submit_button { vertical-align: middle; height: 30px; width: 70%; font-size: 15px;color: white;background-color:#1B618D;border-color: #374645 }" )
                            
                    )
                    
               )
               
             ),
             
             fluidRow(
               
               box( title = 'My Asset Classes', status = 'success', collapsible = T, width = 12, solidHeader = T,
                    
                    column( 12, DT::dataTableOutput( outputId = "my_asset_class_df" ), style = "height:500px; overflow-y: scroll;overflow-x: scroll;" )
                    
               )
               
             )
             
    ),
    
    tabItem( tabName = 'my_lease',
             
             fluidRow(
               
               box( title = 'Add Lease', status = 'success', collapsible = T, width = 12, solidHeader = T, collapsed = T,
                    
                    column( 2, textInput( 'add_lease_company', 'Company' ) ),
                    
                    column( 2, dateInput( 'add_lease_valuation_date', 'Valuation Date', format = 'dd-mm-yyyy' ) ),
                    
                    column( 2, dateInput( 'add_lease_initial_application_date', 'Initial Application Date', format = 'dd-mm-yyyy' ) ),
                    
                    column( 2, textInput( 'add_lease_lease_id', 'Lease ID' ) ),
                    
                    column( 2, selectInput( 'add_lease_asset_class', 'Asset Class', choices = my_asset_class_stored[['Asset Class']] ) ),
                    
                    column( 2, textInput( 'add_lease_lessor_name', 'Lessor Name' ) ),
                    
                    column( 2, textAreaInput( 'add_lease_address', 'Address' ) ),
                    
                    column( 2, dateInput( 'add_lease_start_date', 'Start Date', format = 'dd-mm-yyyy' ) ),
                    
                    column( 2, dateInput( 'add_lease_end_date', 'End Date', format = 'dd-mm-yyyy' ) ),
                    
                    column( 2, numericInput( 'add_lease_upfront_payments', 'Upfront Payments', value = 0 ) ),
                    
                    column( 2, dateInput( 'add_lease_rent_effective_date', 'Rent Effective Date', format = 'dd-mm-yyyy' ) ),
                    
                    column( 2, numericInput( 'add_lease_rent', 'Rent', value = 0 ) ),
                    
                    column( 2, numericInput( 'add_lease_gst', 'GST (%)', value = 0 ) ),
                    
                    column( 2, numericInput( 'add_lease_incremental_borrowing_rate', 'Incremental Borrowing Rate (%)', value = NULL ) ),
                    
                    column( 2, numericInput( 'add_lease_escalation_percent', 'Escalation Percent (%)', value = 0 ) ),
                    
                    column( 2, numericInput( 'add_lease_escalation_tenure', 'Escalation Tenure', value = 0 ) ),
                    
                    column( 2, selectInput( 'add_lease_lease_payment_freq', 'Lease Payment Frequency', 
                                            
                                            choices = c( 'Monthly' = 'monthly', 'Quarterly' = 'quarterly', 
                                                         
                                                         'Half Yearly' = 'half_yearly', 'Yearly' = 'yearly' ) ) ),
                    
                    column( 2, selectInput( 'add_lease_lease_payable_at', 'Lease Payable At', 
                                            
                                            choices = c( 'End' = 'end', 'Beginning' = 'beginning' ) ) ),
                    
                    column( 2, numericInput( 'add_lease_dismantling_cost', 'Dismantling Cost', value = 0 ) ),
                    
                    column( 2, dateInput( 'add_lease_date_of_dismantling', 'Date of Dismantling', format = 'dd-mm-yyyy' ) ),
                    
                    column( 2, numericInput( 'add_lease_residual_guarantee_payments', 'Residual Guarantee Payments', value = 0 ) ),
                    
                    column( 2, selectInput( 'add_lease_avail_exemption', 'Avail Exemption', 
                                            
                                            choices = c( 'No' = 'no', 'Yes' = 'yes' ) ) ),
                    
                    column( 2, selectInput( 'add_lease_exemption_type', 'Exemption Type', 
                                            
                                            choices = c( 'Short Term' = 'short_term', 'Low Value' = 'low_value', 'NA' = 'NA' ) ) ),
                    
                    column( 2, selectInput( 'add_lease_transition_method', 'Transition Method', 
                                            
                                            choices = c( 'Modified 1' = 'modified_1' ) ) ),
                    
                    column( 2, textInput( 'add_lease_credit_rating', 'Credit Rating' ) ),
                    
                    column( 2, selectInput( 'add_lease_use_cash_flow', 'Use Cash Flow', choices = c( 'No' = 'no', 'Yes' = 'yes' ) ) ), br(),
                    
                    column( 2, align = 'left',
                            
                            actionButton( 'add_lease_submit_button', 'SUBMIT' ),
                            
                            tags$style( "#add_lease_submit_button { vertical-align: middle; height: 30px; width: 70%; font-size: 15px;color: white;background-color:#1B618D;border-color: #374645 }" )
                            
                    )
                    
               )
               
             ),
             
             fluidRow(
               
               box( title = 'Upload My Lease', status = 'success', width = 12, solidHeader = T, collapsible = T,
                    
                    column( 8, fileInput( 'my_lease', 'Upload lease file' ) ), br(),
                    
                    column( 2, align = 'left',
                            
                            actionButton( 'my_lease_upload_button', 'SUBMIT' ),
                            
                            tags$style( "#my_lease_upload_button { vertical-align: middle; height: 30px; width: 70%; font-size: 15px;color: white;background-color:#1B618D;border-color: #374645 }" )
                            
                    )
                    
               )
               
             ),
             
             fluidRow(
               
               box( title = 'My Lease', status = 'success', collapsible = T, width = 12, solidHeader = T,
                    
                    column( 12, DT::dataTableOutput( outputId = "my_lease_df" ), style = "height:500px; overflow-y: scroll;overflow-x: scroll;" )
                    
                  )
               
             )
             
    ),
    
    tabItem( tabName = 'my_advances',
             
             fluidRow(
               
               box( title = 'Upload My Advances', status = 'success', width = 12, solidHeader = T, collapsible = T,
                    
                    column( 8, fileInput( 'my_advance', 'Upload Advance file' ) ), br(),
                    
                    column( 2, align = 'left',
                            
                            actionButton( 'my_advance_upload_button', 'SUBMIT' ),
                            
                            tags$style( "#my_advance_upload_button { vertical-align: middle; height: 30px; width: 70%; font-size: 15px;color: white;background-color:#1B618D;border-color: #374645 }" )
                            
                    )
                    
               )
               
             ),
             
             fluidRow(
               
               box( title = 'My Advance', status = 'success', collapsible = T, width = 12, solidHeader = T,
                    
                    column( 12, DT::dataTableOutput( outputId = "my_advance_df" ), style = "height:500px; overflow-y: scroll;overflow-x: scroll;" )
                    
               )
               
             )
             
    ),
    
    tabItem( tabName = 'my_cash_flows',
             
             fluidRow(
               
               box( title = 'My Cash Flows', status = 'success', collapsible = T, width = 12, solidHeader = T,
                    
                    DT::dataTableOutput( outputId = "my_cash_flows_df" ) )
               
             )
             
    ),
    
    tabItem( tabName = 'asset_right_of_use',
             
             fluidRow(
               
               box( title = 'Asset Right-of-Use', status = 'success', collapsible = T, width = 12, solidHeader = T,
                    
                    DT::dataTableOutput( outputId = "asset_right_of_use_df" ) )
               
             )
             
    ),
    
    tabItem( tabName = 'lease_liability',
             
             fluidRow(
               
               box( title = 'Lease Liability', status = 'success', collapsible = T, width = 12, solidHeader = T,
                    
                    DT::dataTableOutput( outputId = "lease_liability_df" ) )
               
             )
             
    ),
    
    tabItem( tabName = 'lease_deposits',
             
             fluidRow(
               
               box( title = 'Lease Deposits', status = 'success', collapsible = T, width = 12, solidHeader = T,
                    
                    DT::dataTableOutput( outputId = "lease_deposits_df" ) )
               
             )
             
    ),
    
    tabItem( tabName = 'right_to_use_deposit',
             
             fluidRow(
               
               box( title = 'Right to Use deposit', status = 'success', collapsible = T, width = 12, solidHeader = T,
                    
                    DT::dataTableOutput( outputId = "right_to_use_deposit_df" ) )
               
             )
             
    )
    
  ))


dashboardPage( app_header, app_sidebar, app_body, skin = 'yellow' )


