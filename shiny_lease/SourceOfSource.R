rm( list = ls() )

library( shiny ) ; library( shinydashboard ) ; library( dplyr ) ; library( data.table )

library( shinyWidgets ) ; library( DT ) ; library( plotly ) ; library( bsts )


#.... Scripts .....

source( 'global_functions.R' )

#..... Read my asset class df ...

if( !file.exists( 'my_asset_class_df_stored.csv' ) ){
  
  my_asset_class_stored = data.frame()
  
} else{
  
  my_asset_class_stored = fread( 'my_asset_class_df_stored.csv' )
  
}


#..... Read my lease df ...

if( !file.exists( 'my_lease_df_stored.csv' ) ){
  
  my_lease_df_stored = data.frame()
  
} else{
  
  my_lease_df_stored = fread( 'my_lease_df_stored.csv' )
  
}

my_cashflow_df_stored = Generate_Cash_Flows( tail( my_lease_df_stored, 1 ) )

