###  SETUP CONNECTION TO HOUSING 2019 STAGING DB ### 
source("~/Documents/R/postgis_connect.R")
con_pg <- make_postgis_con(mydb = 'housing-2019-staging', myuser = 'housing2019', host = 'housing-2019-staging.caicgny9d8nv.us-west-2.rds.amazonaws.com')


## READ IN OREGON AND WASHINGTON TRACTS ### 
setwd("cb_2017_41_tract_500k/")
or_tracts <- rgdal::readOGR("cb_2017_41_tract_500k.shp")
setwd("../cb_2017_53_tract_500k/")
wa_tracts <- rgdal::readOGR("cb_2017_53_tract_500k.shp")
setwd("../")


### UPLOAD TRACT GEOMETRY TABLES TO DB ### 
# this is a bit of a hack - rather than the annoying of rbinding the two tract spatialpolygondataframes
# I just create the table with the first one (or_tracts), then reassign the data and write again with append = TRUE 
orwa_census_tracts_geo <- sf::st_as_sf(or_tracts)
sf::st_write(obj = orwa_census_tracts_geo, dsn = con_pg)  
orwa_census_tracts_geo <- sf::st_as_sf(wa_tracts)
sf::st_write(obj = orwa_census_tracts_geo, dsn = con_pg, append=TRUE)  


### LOAD add_geo_to_table FUNCTION FOR ADDING GEOMETRY TO TABLES ### 
source("add_geofield_to_table_function.R")


### ADD GEO FOR EACH TABLE TABLE TRACT ### 
add_geo_to_table(
  table_name = 'public.median_household_income_by_race_2017',
  geotable_name = 'public.orwa_census_tracts_geo',
  table_geoid_fieldname = 'GEOID',
  geotable_geoid_fieldname = 'GEOID',
  table_geom_fieldname = 'tract_geom',
  geotable_geom_fieldname = 'geometry',
  con = con_pg
)

add_geo_to_table(
  table_name = 'public."SC2_hmda_approval_by_race_2013t2017"',
  geotable_name = 'public.orwa_census_tracts_geo',
  table_geoid_fieldname = 'GEOID',
  geotable_geoid_fieldname = 'GEOID',
  table_geom_fieldname = 'tract_geom',
  geotable_geom_fieldname = 'geometry',
  con = con_pg,
  force_update = 1
)

add_geo_to_table(
  table_name = 'public.race_by_tenure_1990t2017',
  geotable_name = 'public.orwa_census_tracts_geo',
  table_geoid_fieldname = 'tract_fips',
  geotable_geoid_fieldname = 'GEOID',
  table_geom_fieldname = 'tract_geom',
  geotable_geom_fieldname = 'geometry',
  con = con_pg
)

add_geo_to_table(
  table_name = 'public.total_loans',
  geotable_name = 'public.orwa_census_tracts_geo',
  table_geoid_fieldname = 'tct_fips',
  geotable_geoid_fieldname = 'GEOID',
  table_geom_fieldname = 'tract_geom',
  geotable_geom_fieldname = 'geometry',
  con = con_pg
)

add_geo_to_table(
  table_name = 'public.api_ncdbsampleyearly',
  geotable_name = 'public.orwa_census_tracts_geo',
  table_geoid_fieldname = 'fips_code_id',
  geotable_geoid_fieldname = 'GEOID',
  table_geom_fieldname = 'tract_geom',
  geotable_geom_fieldname = 'geometry',
  con = con_pg
)




### THIS IS OLD CODE FOR THE msa_tracts
# before I put all WA tracts into the same table I used this for msa tracts. 
# msa_tracts <- geojsonio::geojson_read('census_tract_boundaries.geojson',what='sp')
# msa_census_tracts_geo <- sf::st_as_sf(msa_tracts)
# sf::st_write(obj = msa_census_tracts_geo, dsn = con_pg)  
