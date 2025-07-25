#function for matching the relevant files to seasons
find_matching_string <- function(a, b, target_b) {
  if (!(target_b %in% b)) return(NULL)
  a[grep(target_b, a, fixed = TRUE)]
}

# functio for generating the map
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
  
  # Define layers based on new classification
  x_regular <- x
  x_regular[!(x %in% c(1, 3, 5, 7))] <- NA
  x_regular[!is.na(x_regular)] <- 1
  
  x_sporadic <- x
  x_sporadic[!(x %in% c(2, 3, 4, 5, 6, 7))] <- NA
  x_sporadic[!is.na(x_sporadic)] <- 2
  
  x_sporadic_extreme <- x
  x_sporadic_extreme[!(x %in% c(4, 5, 6, 7))] <- NA
  x_sporadic_extreme[!is.na(x_sporadic_extreme)] <- 3
  
  # Generate leaflet map
  temp_map <- leaflet(options = leafletOptions(zoomControl = FALSE)) %>%
    addProviderTiles(providers$CartoDB.Positron) %>%
    addRasterImage(x_regular, group = classes$cat[classes$num == 1], colors = pal, opacity = 0.6) %>%
    addRasterImage(x_sporadic, group = classes$cat[classes$num == 2], colors = pal, opacity = 0.6) %>%
    addRasterImage(x_sporadic_extreme, group = classes$cat[classes$num == 3], colors = pal, opacity = 0.6) %>%
    addPolygons(
      lng = unlist(lapply(polygon_coords[[1]], function(coord) coord[1])),
      lat = unlist(lapply(polygon_coords[[1]], function(coord) coord[2])),
      fill = FALSE,
      color = "gray",
      weight = 1,
      opacity = 0.5
    ) %>%
    addLegend(pal = pal, values = unique(classes$num), labFormat = labelFormat(
      transform = function(x) { classes[which(classes$num == x), 2] }
    )) %>%
    htmlwidgets::onRender("function(el, x) {
        L.control.zoom({ position: 'bottomright' }).addTo(this);
      }") %>%
    addLayersControl(overlayGroups = unique(classes$cat),
                     options = layersControlOptions(collapsed = FALSE)) %>%
    htmlwidgets::onRender("function(el, x) {
      setTimeout(function() {
        var offset = 150;
        var mapHeight = window.innerHeight - offset;
        el.style.height = mapHeight + 'px';
      }, 100);
    }")
  
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