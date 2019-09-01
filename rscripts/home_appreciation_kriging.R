# This script generates spatial estimates across Portland of inflation-adjusted home 
# apprecation from roughly 1990 to the present. It then writes these estimates to the db
# for access via the API. 

# The analysis uses RLIS (tax-lot data) from the table's taxlots.home_sales_prev_sale
# and taxlots.tl_2017. Specifically it uses at home sales in 2016-2017 sold most recently 
# before that between 1987 and 1993. It limits sales to houses whose buildsqft had not 
# substantially changed between 1997 -- the initial year of data --  and 2017. Houses that 
# were sold in the intervening years were excluded to mitigate the impact of major remodels / 
# house flipping on appreciation. 

# the differences between inflation-adjusted sale prices was used as the basis a Ordinary Kriging. 
# Spatial Dependence of the data was modeled via a semivariogram function, and this function 
# served as a input to generate the spatial model of home appreciation during this period for 
# there sorts of houses (i.e. those not sold ~1990 and again ~2016 without intervening sales)  

require(postGIStools)
require(geoR)
require(tidyverse)

# make database connection
source("~/Documents/R/postgis_connect.R")
con_pg <- make_postgis_con(mydb = 'housing-2019-staging', myuser = 'housing2019', host = 'housing-2019-staging.caicgny9d8nv.us-west-2.rds.amazonaws.com')

# pull data (spatial points data frame) needed for analysis
appreciation_for_kriging <- postGIStools::get_postgis_query(con_pg,
       "SELECT DISTINCT hs.tlid_clean, sale_year, sale_price, prev_sale_price, 
        prev_sale_price * prev_sale_infl.inflation_factor_2017 prev_sale_price_infl_adj, 
        sale_price - prev_sale_price appreciation_dollars,  
        (sale_price * sale_infl.inflation_factor_2017) - (prev_sale_price * prev_sale_infl.inflation_factor_2017) appreciation_dollars_infl_adj,  
        ST_PointOnSurface(shape) house_point
        FROM taxlots.home_sales_prev_sale hs
        JOIN public.cpi_u_rs_1950 prev_sale_infl ON(prev_sale_infl.year = prev_sale_year)
        JOIN public.cpi_u_rs_1950 sale_infl ON(sale_infl.year = sale_year)
        JOIN taxlots.tl_2017 USING(tlid_clean)
        WHERE abs((hs.bldgsqft/prev_bldgsqft::float)-1) < 0.1 
        AND prev_bldgsqft >0 AND prev_sale_price > 0    
        AND sale_year BETWEEN 2016 AND 2017 AND prev_sale_year BETWEEN 1987 AND 1993
        ", geom_name = 'house_point')


# check for duplicate points - since geo-data operations like kriging don't handle those elegantly

if(!is.null(geoR::dup.coords(coordinates(appreciation_for_kriging)))){
  warning('There are dupe records that need to be cleaned up')
} else {
  print('DUP CHECK: all good on the duplicate coordinate front. On to converting to geodata')
}

# convert spatial points data frame to geodata - which is the class geoR needs for kriging 
appr_geo <- coordinates(appreciation_for_kriging) %>% 
  cbind(appreciation_dollars = appreciation_for_kriging@data$appreciation_dollars) %>% 
  geoR::as.geodata()

appr_geo_infl <- coordinates(appreciation_for_kriging) %>% 
  cbind(appreciation_dollars = appreciation_for_kriging@data$appreciation_dollars_infl_adj) %>% 
  geoR::as.geodata()

# the semivariogram function needed for kriging is meant to model local spatial variation, not 
# global spatial variation -- so generally the semivariogram is visualized using half the maximum
# distance between geodata points. 

# get half the max distance
half_geo_dist<-summary(appr_geo)$distances.summary[2]/2

# build and plot the semivariogram for the 
appreciation.vario<-geoR::variog(appr_geo,max.dist=half_geo_dist)

plot(appreciation.vario,pch=16,xlab="Distance (feet)",ylab="Semivariogram",pty="m")
title("Semivariogram (retricted < 1/2 max distance)")

# plot confirms spatial dependence. To quantify the function used by kriging we 
# eyeball a set of parameter estimates for a spherical semivariogram

# Range (where on x axis the semivariogram levels off) is about 21,000
# Nugget (where semivariogram might hit the x axis at 0 distance) is about 100,000,000
# Sill (where the semivariogram levels off at its max) is about 420,000,000
# Partial Sill (Sill minus Nugget) is about 320,000,000


# Using those estimated parameters as a starting point use WLS approach to build model/function
# of the semivariogram.  Use your eyeballed estimates as the initial parameter 
# estimates.  I choose a spherical function.  You are welcome to select another function
# that appears to fit the shape, just make the appropriate change in the command below.
# Enter (partial sill,range) in ini.cov.pars, and nugget as nugget. 
appreciation.vario.wls<-geoR::variofit(appreciation.vario,ini.cov.pars=c(3.2e10,21000),cov.model="spherical",
                                       nugget=1e10,weights="cressie")

# plot to check the fit of the model
plot(appreciation.vario,pch = 16, xlab="Distance (feet)",ylab="Semivariogram",pty="m")
title("Fitted WLS Semivariogram for Appreciation")
lines(appreciation.vario.wls)


### fit inflation adjusted semivariogram
appreciation_infl.vario<-geoR::variog(appr_geo_infl,max.dist=half_geo_dist)

# plot the semivariogram
plot(appreciation_infl.vario,pch=16,xlab="Distance (feet)",ylab="Semivariogram",pty="m")
title("Semivariogram (retricted < 1/2 max distance)")

# Parameter estimates for a spherical semivariogram
# Range about 21,000
# Nugget about 100,000,000
# Sill about 380,000,000
# Partial Sill about 280,000,000

# model creation
appreciation_infl.vario.wls<-geoR::variofit(appreciation.vario,ini.cov.pars=c(2.8e10,21000),cov.model="spherical",
                                            nugget=1e10,weights="cressie")

# check the model visually
plot(appreciation_infl.vario,pch=16,xlab="Distance (feet)",ylab="Semivariogram",pty="m")
title("Fitted WLS Semivariogram for Inflation-Adj. Appreciation")
lines(appreciation_infl.vario.wls)


# Kriging will predict values at a grid of points in the study area. 
# In this case I'm giving 5% padding in every direction from the extent of the geodata points.  
# this grid will be used both for the inflation-adjusted, and raw kriging models. 

easting_bounds <- list(max = max(appr_geo$coords[,1]), min = min(appr_geo$coords[,1]))
northing_bounds <- list(max = max(appr_geo$coords[,2]), min = min(appr_geo$coords[,2]))
bound_padding <- list(
  easting = .05 * (easting_bounds$max - easting_bounds$min),
  northing = .05 * (northing_bounds$max - northing_bounds$min)
)

grid<-expand.grid(easting=seq(easting_bounds$min - bound_padding$easting,easting_bounds$max + bound_padding$easting, len = 100),
                  northing=seq(northing_bounds$min - bound_padding$northing,northing_bounds$max + bound_padding$northing, len = 100)
)

# do the Ordinary Kriging, using the grid we just created, and the variogram we fitted above. Kriging can take a minute or two
krige.appr <- geoR::krige.conv(appr_geo,locations=grid, krige=geoR::krige.control(obj.model=appreciation.vario.wls))
krige.appr.infl_adj <- geoR::krige.conv(appr_geo_infl,locations=grid, krige=geoR::krige.control(obj.model=appreciation_infl.vario.wls))

# you can see what the predicted values krige.appr$predict and the errors with sqrt(krige.appr$krige.var)

# this geodata can be visualized with ggplot, but to visualize in leaflet and subsequently upload to database for API access 
# we go through a bunch of data transformations. 

# convert to spatial pixels data frame 
krige.spatpixelsdf <- SpatialPixelsDataFrame(
  points = as.matrix(grid, ncol=2) , 
  data = data.frame(appreciation_estimates = krige.appr$predict), 
  proj4string = CRS(proj4string(appreciation_for_kriging)))

krige_infl.spatpixelsdf <- SpatialPixelsDataFrame(
  points = as.matrix(grid, ncol=2) , 
  data = data.frame(appreciation_estimates = krige.appr.infl_adj$predict), 
  proj4string = CRS(proj4string(appreciation_for_kriging)))

# the original models predicted for the entire grid, but the relavent area is just Portland 
# which is where the data all come from. So we grab city boundaries which we'll use to clip the pixels/grid data
# city boundaries from: https://gis-pdx.opendata.arcgis.com/datasets/city-boundaries?geometry=-123.637%2C45.386%2C-121.697%2C45.723

setwd("~/Documents/R/CivicHousing/City_Boundaries/")
portland_boundary <- rgdal::readOGR("City_Boundaries.shp") %>% 
  subset(CITYNAME == 'Portland') %>%
  spTransform(proj4string(appreciation_for_kriging))
portland_boundary@data <- data.frame(CITYNAME = 'Portland') # mainly this gets rid of a bunch of factors variables with empty records from other cities that mess up the 'over' command below
setwd("../")

krige.spatpixelsdf.clipped <- krige.spatpixelsdf[which(!is.na(over(krige.spatpixelsdf,portland_boundary))),]
krige_infl.spatpixelsdf.clipped <- krige_infl.spatpixelsdf[which(!is.na(over(krige_infl.spatpixelsdf,portland_boundary))),]

# convert to raster for visualization in leaflet 
krige.raster.clipped <- raster::raster(krige.spatpixelsdf.clipped)
krige_infl.raster.clipped <- raster::raster(krige_infl.spatpixelsdf.clipped)

# set color palettes for leaflet
pal = leaflet::colorNumeric(RColorBrewer::brewer.pal(7,'YlGnBu'), krige.spatpixelsdf.clipped@data$appreciation_estimates,
                            na.color = "transparent") 

pal_infl = leaflet::colorNumeric(RColorBrewer::brewer.pal(7,'YlGnBu'), krige_infl.spatpixelsdf.clipped@data$appreciation_estimates,
                                 na.color = "transparent") 

## visualize and explore via leaflet (raw and inflation-adjusted)

leaflet::leaflet() %>% leaflet::addProviderTiles("CartoDB.Positron") %>%
  leaflet::addRasterImage(krige.raster.clipped,colors = pal, opacity = 0.6) %>%
  leaflet::addLegend(pal = pal, values =raster::values(krige.raster.clipped), bins = 6, labFormat = leaflet::labelFormat(prefix = "$"), title = "Appreciation") %>%
  leaflet::addControl("Median $ increase in sale price for homes sold 1987-93 and again 2015-16", position = "bottomleft")

leaflet::leaflet() %>% leaflet::addProviderTiles("CartoDB.Positron") %>%
  leaflet::addRasterImage(krige_infl.raster.clipped,colors = pal_infl, opacity = 0.6) %>%
  leaflet::addLegend(pal = pal_infl, values =raster::values(krige_infl.raster.clipped), bins = 6, labFormat = leaflet::labelFormat(prefix = "$"), title = "Appreciation") %>%
  leaflet::addControl("Median $ increase in inflation-adjusted sale price for homes sold 1987-93 and again 2015-16", position = "bottomleft")

# if everything looks good, move on to convert to spatial polygons data frame for uploaded to db. 
krige_infl.spolydf <- as(krige_infl.spatpixelsdf.clipped,'SpatialPolygons') %>% 
  SpatialPolygonsDataFrame(data = krige_infl.spatpixelsdf.clipped@data,match.ID = FALSE)


# this was a slight detour to geojson to create a gist file. The url from the raw gist file can 
# be plugged into the storybook maps to make sure it loads quickly enough. 
# oddly the default projection didn't render right, so spTansform to GCS was needed to get it to work. 
krige_infl.geojson <- geojsonio::geojson_json(krige_infl.spolydf)
krige_infl2.geojson <- geojsonio::geojson_json(spTransform(krige_infl.spolydf,CRS("+init=epsg:4326")))

geojsonio::geojson_write(krige_infl1.geojson, "~/Documents/R/CivicHousing/krig_infl1.geojson")
geojsonio::geojson_write(krige_infl2.geojson, "~/Documents/R/CivicHousing/krig_infl2.geojson")

# final steps before loading to database are converting to sf and renaming to what we want the table name to be.
krige_infl2.sf <- sf::st_as_sf(spTransform(krige_infl.spolydf,CRS("+init=epsg:4326")))

home_inflation_kriging <- krige_infl2.sf

# then write to db
sf::st_write(obj = home_inflation_kriging, dsn = con_pg)  