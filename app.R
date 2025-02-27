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
                      cat = c("Risk of regular blooms", 
                              "Risk of sporadic blooms", 
                              "Risk of regular and sporadic blooms"
                              )
                      )

##################################################################################################################################
# UI
##################################################################################################################################
ui <- fluidPage(
  useShinyjs(), # Enable shinyjs features
  
  # plot title
  titlePanel("UK risk of algal blooms"),
  
  fluidRow(
    column(
      width = 3, # Adjust width as needed
      selectInput(
        'season',
        label = NULL,
        choices = c("Please select a season", tools::toTitleCase(unique(names(r_list))))
      )
    ),
    column(
      width = 3, # Adjust width as needed
      checkboxInput("regular_checkbox", "Regular risk areas", value = TRUE)
    ),
    column(
      width = 3, # Adjust width as needed
      checkboxInput("sporadic_checkbox", "Sporadic risk areas", value = TRUE)
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
      input$season != "Please select a season") {
      debug_msg("Filtering raster data to selected season")
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
      pal <- colorFactor(palette = c("#38B000", "#CBFE33", "#004B24"),
                         #palette = c("#FFC100", "#FF7400", "#FF0000"),
                         #palette = brewer.pal(length(unique(r_season()[])), "Set1"), 
                         domain = r_season()[],
                         na.color = "transparent")
      print(classes)
      
      #filter the raster input to respond to checkboxes
      if(input$regular_checkbox == TRUE & input$sporadic_checkbox == TRUE){
        r_temp <- r_season()
      } else if (input$regular_checkbox == TRUE & input$sporadic_checkbox == FALSE){
        r_temp <- r_season()
        r_temp[r_temp == 2] <- NA
        r_temp[r_temp == 3] <- 1
      } else if (input$regular_checkbox == FALSE & input$sporadic_checkbox == TRUE){
        r_temp <- r_season()
        r_temp[r_temp == 1] <- NA
        r_temp[r_temp == 3] <- 2
      } else {
        r_temp <- NULL
      }
      
      # function for generating the leaflet map
      leaflet_temp <- generate_map(r_temp, pal, classes)
      
    } else {
      
      leaflet_temp <- NULL
    }
    return(leaflet_temp)
  })
  
}

#run the app
shinyApp(ui = ui, server = server)
