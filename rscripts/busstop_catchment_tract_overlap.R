# this script 
# 1) creates catchment zones for trimet stops
# 2) gives some sample code for visualizing those zones via leaflet. 
# 3) intersects zones with tracts and creates data.frame with proportions of each 
#     tract in each zone. 

require('tidyverse')
require('tibble')
require('sf')
require('rgdal')
require('dismo')
require('rgeos')
require('maptools')
require('leaflet')
require('RColorBrewer')

### CREATE CATCHMENT ZONES ### 

setwd("cb_2017_41_tract_500k/")
oregon_tracts <- rgdal::readOGR("cb_2017_41_tract_500k.shp")
trimet_tracts <- subset(oregon_tracts, COUNTYFP %in% c('051','005','067'))
setwd("../")

setwd("tm_route_stops/")
my_stops_shp <- rgdal::readOGR("tm_route_stops.shp")
setwd("../")

# store each crs for future use, then transform stops to the tract crs
oregon_census_projection <- proj4string(oregon_tracts)
trimet_stops_projection <- proj4string(my_stops_shp)

my_stops_shp <- spTransform(my_stops_shp, CRSobj = oregon_census_projection)

# set catchment_zone as a function that takes a line number, direction and buffer radius 
# and return catchment polygons for that line/direction/radius
catchment_zone <- function(stops_shp= my_stops_shp, route, direction, buffer_ft, planar_ft_crs = trimet_stops_projection, gcs_crs = oregon_census_projection) { 
  cur_stops <- stops_shp %>% 
    subset(rte == route & dir == direction)
  
  # in case there are duplicates, remove them. 
  # Note that this happens when lines circle back over themself and hit the same stop twice in different stop order
  cur_stops_dedup <- cur_stops %>% remove.duplicates    
  
  # create the complete set of voronoi polygons for all stops (deduped)
  vor_complete <- dismo::voronoi(cur_stops_dedup, ext = extent(trimet_tracts)) %>% 
    sp::spChFIDs(row.names(cur_stops_dedup)) # changes the IDs on the polygons to match the row names
  
  # create buffers around each stop (deduped)
  cur_buffers <- cur_stops_dedup  %>% 
    sf::st_as_sf() %>% 
    sf::st_transform(planar_ft_crs) %>% 
    sf::st_buffer(dist = buffer_ft)  %>% 
    as('Spatial') %>%
    spTransform(gcs_crs) 
  
  # iterate through voronoi's and buffer's and just grab overlapping areas. (maybe can do this without map / reduce, but I couldn't figure out how, even with byid = TRUE in gIntersection, or anyway in st_intersection)
  cur_catchment <- purrr::map(1:nrow(vor_complete), function(x)  {
    rgeos::gIntersection(vor_complete[x,], cur_buffers[x,], id = row.names(vor_complete)[x]) %>% 
      SpatialPolygonsDataFrame(vor_complete@data[x,])
  }) %>% reduce(rbind)
  
  # cur_catchment should have polygons for unique stops - this next line adds back in duplicates (sometimes routes/direction hit same stop twice at different times in route path)
  cur_catchment_with_dups <- merge(subset(cur_catchment, select='stop_id'),cur_stops@data, duplicateGeoms = TRUE)
  
  # return cur_catchment (unique per stop_id), catchment_with_dups, and cur_stops points 
  return(list(catchment = cur_catchment, catchment_with_dups =cur_catchment_with_dups, stops = cur_stops))
  }

# populate list of spdf's of catchment-zones. In this case for half mile radii
czs <- list()
for(i in unique(my_stops_shp@data$rte)){
  for(j in unique(subset(my_stops_shp, rte == i)@data$dir)) {
    czs[[paste0('r', i,'_d', j)]] <- catchment_zone(stops_shp = my_stops_shp, route = i, direction = j, buffer_ft = 2640)
  }
}

# mapping function for set of routes / directions
leaflet_transit_catchment <- function(routes, seperate_stops = TRUE, limit_by_direction = c(NA,'0','1'), start_hidden = TRUE, color_code_routes=FALSE){
  if(color_code_routes & length(routes) >= 9){stop("can only color code up to 8 routes")}
  limit_by_direction <- match.arg(limit_by_direction)
  m <- leaflet() %>% 
    addTiles() %>% 
    addPolygons(data = trimet_tracts, weight = 1, highlightOptions = highlightOptions( weight = 3, opacity=1, bringToFront = FALSE), group="tracts", color = 'black', fill= "light grey", fillOpacity = 0.1, label = ~GEOID) %>% 
    fitBounds(-122.78, 45.51, -122.6, 45.58)

  group_names <- character()
  for(name_i in sort(names(czs)[grep(paste0('r(', paste0(routes, collapse='|'),')_d', ifelse(!is.na(limit_by_direction),limit_by_direction,'')), names(czs))])){
    if(seperate_stops) { 
      cur_group_names <- paste(gsub('^r','Line ',name_i), c('stops','catchment'))
    } else {
      cur_group_names <- paste(gsub('^r','Line ',name_i), c('',''))
    }
    cur_col <- brewer.pal(9,'Set1')[grep(sub('_.*','_',name_i) , paste0('r',routes,'_'))]
    m <- m %>% 
      addCircleMarkers(data = czs[[name_i]]$stops, radius = 2, weight = 3, fillOpacity = 1, 
                       color = cur_col, group=cur_group_names[1]) %>%  
      addPolygons(data = czs[[name_i]]$catchment, weight = 2, color = cur_col, fillColor = cur_col,
                  highlightOptions = highlightOptions( weight = 4, opacity=1, bringToFront = FALSE), 
                  group=cur_group_names[2], label=~paste0('line: ',rte,'(d', dir,') - ', stop_name))
    group_names <- c(group_names, unique(cur_group_names))
      
  }
  if(start_hidden) {m <- m %>% hideGroup(group_names)}
  m <- m %>% 
    addLayersControl(
      overlayGroups = c("tracts",group_names),
      options = layersControlOptions(collapsed = FALSE)
    ) 
  m
}

### TAKE A LOOK AT THE ZONES VIA LEAFLET  ###

# sample usage of mapping function
# one route; both directions; hide/show points and zones separately; default lines hidden
leaflet_transit_catchment(4)
# four routes; one direction; hide/show points and zones together; default show all at start; color coded
leaflet_transit_catchment(c(4, 44,6,8), 
                          seperate_stops = FALSE, 
                          limit_by_direction = "0",
                          start_hidden = FALSE,
                          color_code_routes = TRUE)

# many routes; one direction; hide/show points and zones together; default hide all at start
leaflet_transit_catchment(c(4,24,72, 44,6,85,35,75,8, 17,70), 
                          seperate_stops = FALSE, 
                          limit_by_direction = "1")

### IF ZONES LOOKING OKAY, THEN PROCEED TO DO TRACT OVERLAPS  ###

# rbind all the catchment zones into single spdf 
czs_all <- map(czs, function(x) {x$catchment}) %>% reduce(rbind)

# compute the intersection of each catchment zone with each tract - this will let us see 
# proportion of each tract in each zone -- which we'll do next 
czs_tract_intersect <- gIntersection(czs_all, trimet_tracts, byid = TRUE)
czs_tract_intersect_proj <- spTransform(czs_tract_intersect, trimet_stops_projection)
# create dataframe with overlap of each 
cz_tract_df <- data.frame(ids = row.names(czs_tract_intersect_proj),
                          area = gArea(czs_tract_intersect_proj, byid=TRUE)) %>% 
  separate(ids, c('cz_rowname', 'tract_rowname'), ' ')
cz_tract_df <- cz_tract_df %>% 
  dplyr::left_join(tibble::rownames_to_column(czs_all@data), by = c('cz_rowname' = 'rowname')) %>% 
  dplyr::left_join(tibble::rownames_to_column(trimet_tracts@data), by = c('tract_rowname' = 'rowname')) %>% 
  dplyr::select(stop_id, rte, dir, GEOID, area) %>% 
  group_by(stop_id, rte, dir) %>% 
  mutate(total_area = sum(area), area_proportion = area / total_area) %>% 
  ungroup()
  
# write the overlap dataframe so we can just use that instead of rebuilding all of this in the future. 

write.csv(cz_tract_df,"busstop_catchment_zones_tracts_overlap.csv", row.names = FALSE)
saveRDS(czs_all, "busstop_catchment_zone_shp.RDS")
