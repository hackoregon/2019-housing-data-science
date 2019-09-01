# This scripts takes the raw taxlot data from Portland (provided by Nick K and uploaded to db by Nick E)
# and transforms it in various ways to make it more useful for longitudinal analysis. Specifically: 
# 1) append census block fips to each taxlot record; 
# 2) standardize projection - which was mostly consistent, but changed for a few years in early 2000's
# 3) standardize tlid format - which changed multiple times. 
# 4) Pull out home sales into own set of tables. 

# KNOWN BUG: the st_intersect(st_pointonsurface(...),...) joining the taxlots MULTIPOLYGON to the census blocks 
# creates duplicate rows for same tlid with different blocks, when different parts of the multipolygon are in 
# different blocks -- which isn't super common but happens. For some reason I haven't yet tracked down this  
# happened way more in earlier years, and not at all recently (~1k cases '97-'02, ~100 or less '03 - '11, none
# starting 2012.(for these more recent years even though taxlots that still appear to be multi-polygons, they 
# only have one st_pointonsurface and are therefore associated with one block. 
# For example tlid_clean = '1S1E26CB-01000' has 
# plot(get_postgis_query(con_pg, "select shape FROM taxlots.tl2_2017 where tlid_clean = '1S1E26CB-01000' limit 1", geom_name = 'shape'))
# KNOWN FIX: do something like this to only intersect largest part of multi-polygon's pointonsurface to block: 
# https://gis.stackexchange.com/questions/217237/function-to-generate-a-centroid-on-the-largest-polygon
# just haven't gotten to it. 

###  SETUP CONNECTION TO HOUSING 2019 STAGING DB ### 
source("~/Documents/R/postgis_connect.R")
con_pg <- make_postgis_con(mydb = 'housing-2019-staging', myuser = 'housing2019', host = 'housing-2019-staging.caicgny9d8nv.us-west-2.rds.amazonaws.com')


### CREATE TAXLOTS SCHEMA ### 
create_taxlots_schema = 0
if(create_taxlots_schema){dbGetQuery(con_pg, 'create schema taxlots')}; 


### MAKE CENSUS BLOCKS TABLE FOR METRO COUNTIES ### 
# note: all 2010 oregon census blocks were previously uploaded by Nick E to public.tl_2010_41_tabblock10 
create_metro_census_blocks_table = 0
if(create_metro_census_blocks_table){
  # create census blocks table for relevant counties in Oregon
  dbGetQuery(con_pg, "CREATE TABLE taxlots.census_blocks AS 
             SELECT * FROM public.tl_2010_41_tabblock10
             WHERE countyfp10 IN ('005', '051', '067');")
  
  # add make the polygon field a geometry(MultiPolygon) field and re-project it to same projection as taxlots tables
  dbGetQuery(con_pg, "ALTER TABLE taxlots.census_blocks 
            ALTER COLUMN wkb_geometry TYPE geometry(MultiPolygon, 2913) 
            USING ST_Transform(ST_SetSRID(wkb_geometry,4269),2913)")

  # create index on geometry field for faster querying
  dbGetQuery(con_pg, "CREATE INDEX ix_spatial_cb ON taxlots.census_blocks USING GIST(wkb_geometry);")
} 


### ADJUST TAXLOT PROJECTIONS FOR 3 YEARS THAT USED A DIFFERENT PROJECTION ### 
# to avoid touching the original tables, I created duplicate taxlot tables for these 3 years 
# with srid 2913 to match the others
for(yr in 2001:2003){ 
  dbGetQuery(con_pg,paste0("CREATE TABLE aug_", yr,"_srid2913 as 
    select * from public.aug_", yr));
  
  dbGetQuery(con_pg, paste0("ALTER TABLE aug_", yr,"_srid2913
    ALTER COLUMN shape TYPE geometry(MultiPolygon, 2913)
           USING ST_Transform(ST_SetSRID(shape,2269),2913)"));
  
  }
  

### CREATE TAXLOT TABLES FOR EACH YEAR IN THE taxlots SCHEMA - WITH CENSUS BLOCK APPENDED
for(yr in 1997:2017){
  # ID Which source table to use -- this is done here because names aren't totally standard 
  # due to the 3 years where the raw data had different projections. 
  src_table <- paste0("public.aug_", yr, ifelse(yr %in% 2001:2003 , '_srid2913', ''))
  
  # add index on PointOnSurface of polygons in source tables -- to make intersect with census blocks faster.
  dbGetQuery(con_pg, paste0("CREATE INDEX IF NOT EXISTS ix_pos_spatial_aug_", yr," ON ",src_table , " USING GIST (ST_PointOnSurface(shape));")) 
  
  # populated table, with census-block-fip column, then add geometry and create index on each tl's polygon
  dbGetQuery(con_pg, paste0("CREATE TABLE taxlots.tl2_", yr, " AS 
                            SELECT tl.*, cb.geoid10 blockfip
                            FROM ",src_table , " tl
                            LEFT JOIN taxlots.census_blocks cb 
                                ON ST_intersects(ST_PointOnSurface(tl.shape),cb.wkb_geometry) "));
  
  dbGetQuery(con_pg, paste0("ALTER TABLE taxlots.tl2_", yr, "  
                            ALTER COLUMN shape TYPE geometry(MultiPolygon, 2913)"));

  dbGetQuery(con_pg, paste0("CREATE INDEX ix_spatial_tl2_", yr," ON taxlots.tl2_", yr, " USING GIST (shape);")) 
}

  
### STANDARDIZE FORMAT OF TLID

# to determine clean tlid I confirmed that all Multnomah County tlid's in most recent year 2017 had standard format with prefix then white space, then '-' and 5 digit numeric portion. 
  # looking back at previous years it seems this was standard practice back to 2010. 2001 - 2009 had same format but without the '-'
  # 1998-2000 had no '-' and no leading 0's in what would become the 5 digit final portion. 
  # 1997 had just r_code as the tlid and no tlid in the data. 
  # it also wasn't clear to me if amount of white space was consistent over time
  # so to normalize - I added 1998 tlids to 1997 based on r_code join. 
  # then I stripped white space where there was already a '-' 
  # and where there was no '-' I stripped white space and replaced it with '-' followed by however many leading 0s were needed to make the portion after the '-' 5 digits. 
  
# add tlid_clean for each year, index it, and populate it. 
for(yr in 1997:2017){
  
  dbGetQuery(con_pg, paste0("ALTER TABLE taxlots.tl2_", yr, " 
                            ADD COLUMN tlid_clean varchar(55);"));
  
  dbGetQuery(con_pg, paste0("CREATE INDEX IF NOT EXISTS ix_tlid_clean2_", yr, " ON taxlots.tl2_", yr, " (tlid_clean)"));
  
  # note that one backslash of the double \\ in \\s+ is to escape in R -- to run sql directly needs to be just \s+ 
  # special case for 1997, since source data had tlid as r_code. 
  if(yr == 1997) { 
    dbGetQuery(con_pg, paste0(
      "UPDATE taxlots.tl2_", yr, " o
       SET tlid_clean = REGEXP_REPLACE(n.tlid, '\\s+', '-' || left('00000', 5 - LENGTH((REGEXP_SPLIT_TO_ARRAY(n.tlid, '\\s+'))[2]))) -- replace whitespace with '-' and then enough leading 0's needed to make 5 digits
       FROM taxlots.tl2_1998 n
       WHERE o.tlid = n.rno"))
    } else {
     dbGetQuery(con_pg, paste0("UPDATE taxlots.tl2_", yr, " 
                             SET tlid_clean = CASE
                                WHEN tlid LIKE '%-%' THEN REGEXP_REPLACE(tlid, '\\s+', '') -- replace white space with ''
                                ELSE REGEXP_REPLACE(tlid, '\\s+', '-' || left('00000', 5 - LENGTH((REGEXP_SPLIT_TO_ARRAY(tlid, '\\s+'))[2]))) -- replace whitespace with '-' and then enough leading 0's needed to make 5 digits
                                END
                             WHERE county = 'M' AND TRIM(prop_code) != ''"));
    }
 }
  
  


### HOME SALES - JUST FOR PORTLAND, SINCE EARLY YEARS OF DATA IS JUST FOR PORTLAND ### 

# create table with importnat fields from home sales, and add index to tlid_clean
dbGetQuery(con_pg, "CREATE TABLE taxlots.home_sales (
           tlid_clean VARCHAR(55), year INTEGER, 
           saledate VARCHAR(6), saleprice FLOAT,
           yearbuilt SMALLINT, bldgsqft INTEGER,
           blockfip VARCHAR(15))")
dbGetQuery(con_pg, paste0("CREATE INDEX ix_tlid_clean_home_sales ON taxlots.home_sales (tlid_clean)"));
  
# populate home_sales table with new home-sale data each year. (standardizing saledate format, since was 4-digit sale yymm, before becoming 6-digit yyyymm) 
for(yr in 1997:2017){
  dbGetQuery(con_pg, paste0("INSERT INTO taxlots.home_sales
             SELECT tl.tlid_clean, ", yr, ",
             case when length(tl.saledate::varchar)=4 then '19' else left(tl.saledate::varchar,2) END || right(tl.saledate::varchar,4) saledate, tl.saleprice, tl.yearbuilt, tl.bldgsqft, tl.blockfip
             FROM taxlots.tl2_", yr, " tl
             LEFT JOIN taxlots.home_sales o ON(tl.tlid_clean =o.tlid_clean AND o.year < ", yr, "  
                       AND CASE WHEN LENGTH(tl.saledate::varchar)=4 THEN '19' ELSE left(tl.saledate::varchar,2)  END || RIGHT(tl.saledate::varchar,4)  = o.saledate)
             WHERE o.tlid_clean IS NULL -- to strip out rows where sale date is the same.  
             AND trim(tl.saledate::varchar) !='' AND tl.saledate::numeric !=0 AND tl.saledate IS NOT NULL -- sale date switched from smallint with 0's to char fields with ' ' type values at some point.
             AND tl.county = 'M' AND tl.sitecity LIKE 'PORTLAND%'
             AND tl.tlid_clean !=''
             AND ", case_when(
                      yr %in% 1997:1999 ~ "tl.prop_code = 'B'",
                      yr %in% 2000:2003 ~ "tl.prop_code IN('141', '741', '151')",
                      yr == 2004 ~ "tl.prop_code IN('101', '171')",
                      yr > 2004 ~ "tl.prop_code = '101'",
                      TRUE ~ 'NULL')
              )) 
  print(paste('done with', yr))
}  

## This next section is for basic QA of where each years new sales are showing up. 
qa_house_sales <- function() {
sales_sample<- postGIStools::get_postgis_query(con_pg, "select *, ST_Centroid(shape) as home_pnt from taxlots.tl_2017 tl JOIN taxlots.home_sales3 USING(tlid_clean) ORDER BY RANDOM() LIMIT 2000", geom_name = 'home_pnt') %>% 
  spTransform(CRS("+init=epsg:4326"))  
m <- leaflet::leaflet() %>% leaflet::addTiles() %>% leaflet::fitBounds(-122.8,45.4,-122.45,45.6)
for (yr in 1997:2017){ 
  m <- m %>% leaflet::addCircleMarkers(data = subset(sales_sample,year == yr), 
                              radius = 1, label = ~saleprice.1, 
                              group = paste0('home sales recorded: ', yr))
}
m %>% leaflet::addLayersControl(baseGroups = paste0('home sales recorded: ', 1997:2017), options = leaflet::layersControlOptions(collapsed = FALSE))
}

### uncomment this next line to see visualization from function above
# qa_house_sales()

# NOTE FROM QA: the apparent dip in homesales recorded in 2000 and spike in 2001 is because almost all 1999 and 2000 home sales were reported in the 2001 taxlots, but not in 2000. 


# create a table that has for each sale in the dataset when the previous sale was and for how much
dbGetQuery(con_pg, "CREATE TABLE taxlots.home_sales_prev_sale AS 
  SELECT * 
  FROM (
      SELECT tlid_clean, year taxlot_year, 
      left(saledate, 4)::integer sale_year, 
      right(saledate,2)::integer sale_month, 
      saleprice sale_price, yearbuilt, bldgsqft, blockfip,
      lag(year) OVER (PARTITION BY tlid_clean ORDER BY saledate::numeric) as prev_taxlot_year,
      lag(left(saledate, 4)::integer) OVER (PARTITION BY tlid_clean ORDER BY saledate::numeric) as prev_sale_year,
      lag(right(saledate, 2)::integer) OVER (PARTITION BY tlid_clean ORDER BY saledate::numeric) as prev_sale_month,
      lag(saleprice) OVER (PARTITION BY tlid_clean ORDER BY saledate::numeric) as prev_sale_price,
      lag(yearbuilt) OVER (PARTITION BY tlid_clean ORDER BY saledate::numeric) as prev_yearbuilt,
      lag(bldgsqft) OVER (PARTITION BY tlid_clean ORDER BY saledate::numeric) as prev_bldgsqft
      FROM taxlots.home_sales3
  ) as t 
  WHERE prev_sale_year IS NOT NULL AND 
      NOT (prev_sale_year = sale_year AND prev_sale_month = sale_month)"
)
  