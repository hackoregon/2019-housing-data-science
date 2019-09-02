# This script computes census data for bus-stop catchment zones.  
# Imputs include:
# a weighting dataframe: the cz_tract_df - which includes catchment zone and proportion in each tract
# an attribute data_frame - in this case our census data. 
# The function to compute census variables for catchment zones currently can calculate using a weighted 
# mean approach; or by grabbing the census value for the tract with the largest area in the catchment 
# zone - which is potentially useful for some categorical fields (like gentrification stage). It 
# should be relatively easy to extend the function to handle other field calculation methods. 

require(tidyverse)

### LOAD DATA ### 
census_data <- read.csv("NCDB_Sample_Database_All_Tracts_12apr2019.csv")
census_data_pop <- read.csv("NCDB_Sample_Population_10jun2019.csv")
census_data<- census_data %>% dplyr::left_join(census_data_pop)
cz_tract_df <- read.csv("busstop_catchment_zones_tracts_overlap.csv")

### DEFINE FIELDS TO APPEND ###
# all 2017 fields that are tract level (not 'Met' fields which are Metro area versions, and not TotalPopulation)
fields_to_append <- names(census_data[,which(grepl('_17$',names(census_data)) & !grepl('^(Met|TotalPopulation)',names(census_data)))]) 

### DEFINE FUNCTION TO CALCULATE CATCHMENT ZONE VALUES ### 

calc_catchment_param_value <- function(weighting_df, attribute_df, attribute_name, 
                                       weighting_geoid_fieldname = 'GEOID',
                                       weighting_value_fieldname = 'area_proportion',
                                       weighting_group_name=c('stop_id', 'rte','dir'),
                                       attribute_geoid_fieldname = 'Geo_FIPS',
                                       calc_method = c('weighted_mean','value_of_largest_matching_tract')
){
  stopifnot(TRUE); # add things here later
  calc_method = match.arg(calc_method)
  
  output_df <- weighting_df %>% 
    left_join(attribute_df[,c(attribute_geoid_fieldname,attribute_name)], 
              by = setNames(nm = weighting_geoid_fieldname,attribute_geoid_fieldname))  # https://stackoverflow.com/questions/28399065/dplyr-join-on-by-a-b-where-a-and-b-are-variables-containing-strings

  if(calc_method == 'weighted_mean'){
    output_df <- output_df %>%
      group_by_(.dots = weighting_group_name) %>% 
      mutate(total_proportion = sum(!!rlang::sym(weighting_value_fieldname))) %>% 
      summarize(!!attribute_name := sum(!!rlang::sym(attribute_name) * !!rlang::sym(weighting_value_fieldname)) / total_proportion) 

  } else if (calc_method == 'value_of_largest_matching_tract'){
    output_df <-output_df %>% 
      group_by_(.dots = weighting_group_name) %>% 
      mutate(biggest_area_tract_rank = min_rank(desc(area_proportion))) %>% 
      filter(biggest_area_tract_rank==1) %>% 
      summarize(!!attribute_name := !!rlang::sym(attribute_name))
  }
  
  if(sum(is.na(output_df[,attribute_name])) > 0){
    warning(paste('computed', attribute_name, 'has', sum(is.na(output_df[,attribute_name])), 'NA value(s). Your areas of interest in your weighting dataframe may not all have matching values in attribute dataframe'))
  }
  return(output_df)
}


### APPLY THE FUNCTION TO ALL FIELDS FROM CENSUS WE WANT APPENDED TO CATCHMENT ZONES ### 
cz_tract_df_appended <- cz_tract_df %>% distinct(stop_id, rte, dir) 
for (ff in fields_to_append){
  cz_tract_df_appended <- left_join(cz_tract_df_appended, 
                                    calc_catchment_param_value(cz_tract_df, census_data, ff))
}
write.csv(cz_tract_df_appended, 'busstop_catchment_zone_censusfield_appended.csv')


### APPEND DATA TO SHAPEFILE ### 
busstop_catchment_zone_shp_appended <- readRDS('busstop_catchment_zone_shp.RDS')
if(nrow(busstop_catchment_zone_shp_appended@data) != nrow(cz_tract_df_appended)){
  warning("Your attribute field doesn't have the same number of rows are your shapefile. This needs to be fixed to proceed")
} 
busstop_catchment_zone_shp_appended@data <- busstop_catchment_zone_shp_appended@data %>% 
  left_join(cz_tract_df_appended)

# write as geojson file for easy sharing. 
busstop_catchment_zone_shp_appended %>% 
  geojsonio::geojson_json() %>% 
  geojsonio::geojson_write(file = "busstop_catchment_zone_with_census_attribs.geojson")


