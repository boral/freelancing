rm( list = ls() )

library( shiny ) ; library( shinydashboard ) ; library( dplyr ) ; library( data.table )

library( shinyWidgets ) ; library( DT ) ; library( plotly )

#..... Read my lease df ...

if( !file.exists( 'my_lease_df_stored.csv' ) ){
  
  my_lease_df_stored = data.frame()
  
} else{
  
  my_lease_df_stored = fread( 'my_lease_df_stored.csv' )
  
}