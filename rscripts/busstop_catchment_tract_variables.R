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
tract_gentrification <- read.csv("portland_gentrification_displacement_typology_by_tract_id.csv")
census_data<- census_data %>% dplyr::left_join(census_data_pop)
cz_tract_df <- read.csv("busstop_catchment_zones_tracts_overlap.csv")

tract_gentrification <- tract_gentrification %>%
  select(GEOID, human_name) %>% 
  mutate(human_name = as.character(human_name), GEOID = as.character(GEOID),
    gentr_stage_group = dplyr::case_when(
      as.character(human_name) %in% c('Susceptible', 'Early: Type 1','Early: Type 2') ~ 'Early',
      as.character(human_name) %in% c('Dynamic') ~ 'Middle',
      as.character(human_name) %in% c('Late: Type 1','Late: Type 2','Continued Loss') ~ 'Late'
  ))

# clean gentrification data so that Portland tracts not in gentrification classification are 'None'; compared to NA for tracts outside of Portland. 
portland_tracts <- c('41051001202', '41051001301', '41051001302', '41051001400', '41051001500', '41051001601', '41051001602', 
                           '41051001701', '41051001702', '41051001801', '41051001802', '41051001900', '41051002000', '41051002100', 
                           '41051002401', '41051002402', '41051002501', '41051002502', '41051002600', '41051002701', '41051002702', 
                           '41051002801', '41051002802', '41051002901', '41051002902', '41051003000', '41051003100', '41051003200', 
                           '41051003301', '41051003302', '41051003401', '41051003402', '41051003501', '41051003502', '41051003601', 
                           '41051003602', '41051003603', '41051003701', '41051005100', '41051005200', '41051005500', '41051005600', 
                           '41051005700', '41051005800', '41051005900', '41051007300', '41051003702', '41051003801', '41051003802', 
                           '41051003803', '41051003901', '41051003902', '41051004001', '41051004002', '41051004101', '41051004102', 
                           '41051004200', '41051980000', '41051004500', '41051004601', '41051004602', '41051004700', '41051004800', 
                           '41051004900', '41051005000', '41051006300', '41051006402', '41051006801', '41051007400', '41051007500', 
                           '41051007600', '41051007700', '41051007900', '41051008001', '41051008002', '41051008100', '41051008201', 
                           '41051008202', '41051006802', '41051006900', '41051007000', '41051007201', '41051008600', '41051008902', 
                           '41051008901', '41051009201', '41051008301', '41051008302', '41051008400', '41051008500', '41051008700', 
                           '41051009000', '41051009102', '41051009202', '41051009302', '41051009803', '41051009400', '41051009501', 
                           '41051009701', '41051009301', '41051006001', '41051006601', '41051006002', '41051006100', '41051006200', 
                           '41051006501', '41051006502', '41051006602', '41051006701', '41051006702', '41051007202', '41051002903', 
                           '41051007800', '41051008800', '41051009502', '41051004300', '41051000200', '41051000301', '41051010600', 
                           '41051002303', '41051002203', '41051000302', '41051000401', '41051000402', '41051000501', '41051000502', 
                           '41051000601', '41051000602', '41051000701', '41051000702', '41051000801', '41051000802', '41051000901', 
                           '41051000902', '41051001000', '41051001101', '41051001102', '41051001201', '41051009101', '41051009804', 
                           '41051009702', '41051006404', '41051006403', '41051000100', '41051002201', '41051002301', '41051002202', 
                           '41051002302', '41051004400', '41051006401', '41051005300', '41051005400', '41051009300', '41051009802', 
                           '41051009100', '41051008900', '41051009500')

tract_gentrification <- data.frame(GEOID = portland_tracts, stringsAsFactors = FALSE) %>% 
  left_join(tract_gentrification) %>% 
  mutate(GEOID = as.numeric(GEOID))
tract_gentrification[is.na(tract_gentrification)] = 'None'

### DEFINE FIELDS TO APPEND ###
# all 2017 fields that are tract level (not 'Met' fields which are Metro area versions, and not TotalPopulation)
fields_to_append <- names(census_data[,which(grepl('_17$',names(census_data)) & !grepl('^(Met|TotalPopulation)',names(census_data)))]) 
# add 2010 MedInc, MedRentVal, PovRate
fields_to_append <- c(fields_to_append, 'MedInc_10', 'MedRentVal_10', 'PovRate_10')

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
      summarize(!!attribute_name := sum(!!rlang::sym(attribute_name) * 
                                          !!rlang::sym(weighting_value_fieldname)) / mean(total_proportion))

  } else if (calc_method == 'value_of_largest_matching_tract'){
    output_df <- output_df %>% 
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
cz_tract_df_appended <- left_join(cz_tract_df_appended, 
            calc_catchment_param_value(weighting_df = cz_tract_df, 
                                       attribute_df = tract_gentrification, 
                                       attribute_name ='gentr_stage_group',
                                       attribute_geoid_fieldname = 'GEOID',
                                       calc_method = 'value_of_largest_matching_tract'))

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


