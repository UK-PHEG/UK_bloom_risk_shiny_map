#generate the base map function
generate_map <- function(x){
  
  r_temp <- x
  
  #generate leaflet
  temp_map <- leaflet() %>%
    addProviderTiles(providers$CartoDB.PositronNoLabels) %>%
    addRasterImage(r_temp, opacity = 0.8) %>%
    addLegend(pal = colorNumeric("viridis", values(r_temp), na.color = "transparent"),
              values = values(r_temp),
              title = "Bloom risk")
  
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