# Increase the maximum upload size to 160 MB 
options(shiny.maxRequestSize = 100*1024^2)

#install and/or load the required packages
library(leaflet)
library(dplyr)
library(shiny)


##################################################################################################################################
# UI
##################################################################################################################################
ui <- fluidPage(
  useShinyjs(),  # Enable shinyjs features
  
  #plot title
  titlePanel("UK risk of algal blooms"),
  

)

##################################################################################################################################
# SERVER
##################################################################################################################################

server <- function(input, output, session) {
  
  
}


#run the app
shinyApp(ui = ui, server = server)
