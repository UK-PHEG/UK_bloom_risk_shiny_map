# Increase the maximum upload size to 160 MB 
options(shiny.maxRequestSize = 100*1024^2)

#install and/or load the required packages
library(leaflet)
library(dplyr)
library(shiny)
library(raster)
library(tools)
library(RColorBrewer)
library(ncdf4)
library(shinyjs)

#load the supporting functions
source("app_functions.R")

# List NC files, open and combine
nc_files <- list.files("data/", full.names = T)

r_list <- list()
for (file in nc_files){
  r <- raster::raster(file, varname="CHL")
  
  r[r==0] <- NA

  # Extract the string after the last underscore and before '.nc'
  season <- sub(".*_(.*?)\\.nc", "\\1", file)
  
  r_list[[season]] <- r
  rm(file, r, season)
}

# Remove data no longer needed
rm(nc_files)

# assign classes for raster values
classes <- data.frame(num = sort(unique(unlist(lapply(r_list, values)))),
                      cat = c("Regular risk", 
                              "Sporadic risk", 
                              "Regular and sporadic risk"
                              )
                      )

##################################################################################################################################
# UI
##################################################################################################################################
ui <- fluidPage(
  useShinyjs(),  # Enable shinyjs features
  
  #plot title
  titlePanel("UK risk of algal blooms"),
  
  selectInput('season', 'Select season', choices = c("Please make a selection", tools::toTitleCase(unique(names(r_list)))
                                                              )
              ),
  
  leafletOutput("map1")
)

##################################################################################################################################
# SERVER
##################################################################################################################################

server <- function(input, output, session) {
  
  # React to the selection of Season by selecting the correct raster
  r_season <- reactive({
    if (!is.null(input$season) &&
      input$season != "Please make a selection") {
      raster_temp <- r_list[[tolower(input$season)]]
      print(paste("Selected season:", input$season))
      return(raster_temp)
    } else {
      return(NULL)
    }
  })
  
  # Generate map1
  output$map1 <- renderLeaflet({
    if (!is.null(r_season())) {
      debug_msg("Generating map1")

      # Create a color palette
      pal <- colorFactor(palette = brewer.pal(length(unique(r_season()[])), "Set1"), 
                         domain = r_season()[],
                         na.color = "transparent")
      print(classes)
      leaflet_temp <- generate_map(r_season(), pal, classes)
      
    } else {
      leaflet_temp <- NULL
    }
    return(leaflet_temp)
  })
  
}


#run the app
shinyApp(ui = ui, server = server)
