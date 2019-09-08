source("~/Documents/R/postgis_connect.R")
con_pg <- make_postgis_con(mydb = 'housing-2019-staging', myuser = 'housing2019', host = 'housing-2019-staging.caicgny9d8nv.us-west-2.rds.amazonaws.com')
setwd('rscripts/')

setwd("HOLC_Portland/")
holc_portland_redlining_shp <- rgdal::readOGR("HOLC_Portland.shp")
# name your R sf object whatever you want your table to be in the db: 
holc_portland_redlining <- sf::st_as_sf(holc_portland_redlining_shp)
sf::st_write(obj = holc_portland_redlining, dsn = con_pg)  

