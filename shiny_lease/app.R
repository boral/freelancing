source( 'SourceOfSource.R' )

ui = source( file.path( "ui_code.R" ), local = T )$value   #..... ui logic
  

server = function( input, output, session ){
  
  #..... Include server logic for each tab .....
  
  source( file.path( "server_code.R" ), local = T )$value    #..... server logic
  
}

shinyApp( ui = ui, server = server )

