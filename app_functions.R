#generate the base map function
generate_map <- function(x, pal, classes) {
  
  # Get raster extent
  ext <- extent(x)
  
  # Create polygon coordinates from raster extent
  polygon_coords <- list(list(
    c(ext@xmin, ext@ymin),
    c(ext@xmax, ext@ymin),
    c(ext@xmax, ext@ymax),
    c(ext@xmin, ext@ymax),
    c(ext@xmin, ext@ymin) # Close the polygon
  ))
  
  #filter the raster input to respond to layer selection from addLayersControls
  x_regular <- x
  x_regular[x_regular == 2] <- NA
  x_regular[x_regular == 3] <- 1
  
  x_sporadic <- x
  x_sporadic[x == 1] <- NA
  x_sporadic[x == x_sporadic] <- 2
  
  # generate leaflet
  temp_map <- leaflet(options = leafletOptions(zoomControl = FALSE)) %>%
    addProviderTiles(providers$CartoDB.Positron) %>%
    addRasterImage(x_regular, group = classes$cat[classes$num==1], colors = pal, opacity = 0.6) %>%
    addRasterImage(x_sporadic, group = classes$cat[classes$num==2], colors = pal, opacity = 0.6)
    addPolygons(
      lng = unlist(lapply(polygon_coords[[1]], function(coord) coord[1])),
      lat = unlist(lapply(polygon_coords[[1]], function(coord) coord[2])),
      fill = FALSE, # No fill
      color = "gray", # Outline color
      weight = 1, # Outline weight
      opacity = 0.5 # Outline opacity
    ) %>%
    addLegend(pal = pal, values=values(x),labFormat = labelFormat(
      transform = function(x){classes[which(classes["num"]== x),2]}
      )
    ) %>%
    htmlwidgets::onRender("function(el, x) {
        L.control.zoom({ position: 'bottomright' }).addTo(this);
      }") %>%
    addLayersControl(overlayGroups = unique(classes$cat))
  
  return(temp_map)
}

# display debugging messages in R (if local) 
# and in the console log (if running in shiny)
debug_msg <- function(...) {
  is_local <- Sys.getenv('SHINY_PORT') == ""
  in_shiny <- !is.null(shiny::getDefaultReactiveDomain())
  txt <- toString(list(...))
  if (is_local) message(txt)
  if (in_shiny) shinyjs::runjs(sprintf("console.debug(\"%s\")", txt))
}
