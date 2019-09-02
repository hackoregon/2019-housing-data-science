#### Utility functions ####

## Make a Gi* map depicting spatial clustering around a variable
make_gimap <- function(df.sf, df.sf.var, method = "k", k.neighbor = 5, distance = 1.6) {
  p_load(tidyverse, spdep, spdplyr, leaflet, sp, sf, RColorBrewer)
  
  block_summary.sf <- df.sf %>% st_transform(4269) %>%
    filter(!is.na(!!df.sf.var))
  block_summary.sp <- as(block_summary.sf, "Spatial")
  
  if(method == "d") { # distance in km
    d <- dnearneigh(coordinates(block_summary.sp), d1 = 0, d2 = distance, longlat = TRUE)
    spatial_weights <- nb2listw(include.self(d)) ## the include.self() function makes it Gi* instead of just Gi
  } 
  
  if(method == "k") { # k-nearest neighbor
    k <- knearneigh(coordinates(block_summary.sp), k = k.neighbor, longlat = TRUE)
    # k <- knearneigh(st_coordinates(block_summary.sf)[,1:2], k = k.neighbor, longlat = TRUE)
    spatial_weights <- nb2listw(include.self(knn2nb(k)))
  } 
  
  if(method == "q") { # queen
    ## TODO build out queen adjacency. Not very relevent unless it's aggregated to block groups
    spatial_weights <- nb2listw(include.self(poly2nb(block_summary.sf)))
    # break()
  } 
  
  if(method == "r") { # rook
    ## TODO build out rook adjacency. Not very relevent unless it's aggregated to block groups
    spatial_weights <- nb2listw(include.self(poly2nb(block_summary.sf, queen = FALSE)))
    # break()
  }
  
  df.sf.var.vector <- (block_summary.sf%>%select(!!df.sf.var)%>%st_set_geometry(NULL)%>%as.data.frame(.))[,]
  g <- localG(df.sf.var.vector, spatial_weights)
  block_summary.sf$g <- g
  cols <- rev(brewer.pal(7, 'RdYlBu'))
  pal <- colorBin(palette = cols, domain = block_summary.sf$g, 
                  bins = c(min(block_summary.sf$g), -2.58, -1.96, -1.65, 
                           1.65, 1.96, 2.58, max(block_summary.sf$g)))
  popup <- paste0("<strong>", as.character(df.sf.var[[2]]),": </strong>", 
                  df.sf.var.vector,
                  "<br/>", 
                  "<strong>Gi* z-score:</strong> ", 
                  as.character(round(block_summary.sf$g, 2)))
  
  gimap <- leaflet(block_summary.sf) %>%
    addProviderTiles('CartoDB.Positron') %>%
    addPolygons(data = block_summary.sf, fillColor = ~pal(g), stroke = NA, weight = 0.7, fillOpacity = 0.6, 
                popup = popup, smoothFactor = 0.1) %>%
    addLegend(pal = pal, values = block_summary.sf$g, title = "Gi* z-score")
  
  return(gimap)
}