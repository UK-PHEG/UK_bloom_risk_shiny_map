# Increase the maximum upload size to 160 MB 
options(shiny.maxRequestSize = 100*1024^2)

#install and/or load the required packages
library(leaflet)
library(dplyr)
library(shiny)
library(raster)
library(tools)
library(ncdf4)
library(shinyjs)

#load the supporting functions
source("app_functions.R")

#define order of seasons
seasons <- c("winter", "spring", "summer", "autumn")
seasons_labels <- seasons
names(seasons_labels) <- tools::toTitleCase(seasons)

# List NC files, open and combine
nc_files <- list.files("data/", full.names = T)

# Create a mapping between partial matches for season in filename
order_mapping <- sapply(nc_files, function(x) {
  grep(sub(".*_(.*?)\\.nc", "\\1", x), seasons)
})

# Reorder filenames vector based on the mapping
nc_files <- nc_files[order(order_mapping)]

# assign classes for raster values
cat <- c("Risk of regular blooms", 
         "Risk of sporadic blooms")
classes <- data.frame(num = 1:length(cat),
                      cat = cat
)

# Remove variables no longer needed
rm(cat)

##################################################################################################################################
# UI
##################################################################################################################################
ui <- fluidPage(
  useShinyjs(), # Enable shinyjs features
  
  # Plot title
  titlePanel("UK Risk of Algal Blooms"),
  
  # Single input for mutually exclusive season selection
  fluidRow(
    column(12, 
           radioButtons(
             inputId = "season",
             label = "Select a season",
             choices = seasons_labels,
             selected = seasons[1],  # Default selection
             inline = TRUE  # Display buttons in a row
           )
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
    if (!is.null(input$season)) {
      debug_msg("Loading raster data for selected season")
      
      # Select the relevant file for the season selected
      file <- find_matching_string(nc_files, seasons, input$season)
      
      # Load the NC file and convert to raster
      r <- raster::raster(file, varname="CHL")
      
      # Convert all raster values where x==0 to is.na(x)
      r[r==0] <- NA
      
      return(r)
    } else {
      return(NULL)
    }
  })
  
  # Generate map1
  output$map1 <- renderLeaflet({
    if (!is.null(r_season())) {
      debug_msg("Generating map1")
      
      # Create a color palette
      pal <- colorFactor(palette = c("#38B000", "#CBFE33"),
                         #palette = c("#FFC100", "#FF7400"),
                         #palette = brewer.pal(length(unique(classes$num)), "Set1"), 
                         domain = unique(classes$num),
                         na.color = "transparent")
      print(classes)
      
      # function for generating the leaflet map
      leaflet_temp <- generate_map(r_season(), pal, classes)
      
    } else {
      
      leaflet_temp <- NULL
    }
    return(leaflet_temp)
  })
  
}

#run the app
shinyApp(ui = ui, server = server)
