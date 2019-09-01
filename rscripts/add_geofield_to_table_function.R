### FUNCTION TO ADD GEOMETRY TO TABLE ### 
# this function takes two tables - one that needs geometry added and one with geometry - along with
# the field-names for the join (geoids), and a fielnames for the geometries. In cases where 
# geometry field already exists in destination table, force_update = 1 will lead to updating that 
# existing geometry field with matches to the table. 

# parameters: 
# table_name is the table that needs geometry appended to it. Schema name must be included.
# geotable_name is the table with geometries that need to be appened to the table 'table_name'. Schema name must be included.
# table_geoid_fieldname is the name of the field in the first table that will be used for the join
# geotable_geoid_fieldname is the name of the field in the second table that will be used for the join
# table_geom_fieldname is the name of the new field that will be created in table 1 and 
# will store the geometry. If this fieldname already exists in the table you can set force=1 to update 
# this field based on the join to your geotable.
# geotable_geom_fieldname is the name of the geometry fieldname in the second table
# con is the database connection needed
# force_update = 0. If your geometry column already exists in table 1, you'll need to set force = 1 to overwrite. 
# where parameter can be used to specifies which rows in table 1 or table to to append geometry field to. For example if 
# you have different geo-fields for different years but shared geoid's across years, you can specify 'table.year = 1990' 
# in your where clause and only those rows will be updated. You can then run the function again with 'table.year = 2000' etc.
# Use the table name (no alias) when you refer to any fields the 'where' (i.e. 'total_lons.year = 1990', not just 'year = 1990')
require(tidyverse)
add_geo_to_table <- function(table_name, geotable_name, table_geoid_fieldname, geotable_geoid_fieldname, table_geom_fieldname, geotable_geom_fieldname, con = con_pg, force_update = 0, where = 'TRUE') {
  if ( (tolower(table_name) != table_name & !grepl('"',table_name)) ||
       (tolower(geotable_name) != geotable_name & !grepl('"',geotable_name)) ){ warning('your table_name or geotable_name have upper cases letters that might cause you problems. Either change them in db or quote them like this: table_name = schema_name."table_name".')}
  if ( ! (grepl('\\.',table_name) & grepl('\\.',geotable_name))){ stop("table_name and geotable_name must include schema (i.e. '[[schema]].[[table]]').")}
  
  # get the srid of the geometry that will be appended
  srid <- DBI::fetch(DBI::dbSendQuery(con, paste0("SELECT ST_SRID(",geotable_geom_fieldname,") FROM ",geotable_name," LIMIT 1")), n=-1)[1,1]
  geometry_type <- DBI::fetch(DBI::dbSendQuery(con, paste0("SELECT ST_GEOMETRYTYPE(",geotable_geom_fieldname,") FROM ",geotable_name," LIMIT 1")), n=-1)[1,1] %>% sub('^ST_','',.) %>% toupper()
  # check if geom table already exists 
  table_geom_fieldname_exists <- 
    fetch(DBI::dbSendQuery(con, paste0("SELECT COUNT(*) FROM INFORMATION_SCHEMA.COLUMNS WHERE TABLE_SCHEMA ||'.'|| TABLE_NAME = '", gsub('"','',table_name), "' AND COLUMN_NAME = '",table_geom_fieldname,"'")), n=-1)[1,1]
  if(table_geom_fieldname_exists){ 
    if(force_update == 0){
      stop('your table_geom_fieldname already exists in your table. Set force_update = 1 to update anyway')
    } else {
      warning('your table_geom_fieldname already exists in your table. Since force_update = 1, we will update it.')
    } 
  } else {
    other_geom_fields <- 
      DBI::fetch(DBI::dbSendQuery(con, paste0("select f_geometry_column from geometry_columns WHERE f_table_schema || '.' || f_table_name =  '", gsub('"','',table_name), "'")), n=-1)
    if(dim(other_geom_fields)[2] > 0 ) {
      warning(paste0('The following geometry column(s) already exist(s): ',paste(other_geom_fields[,1],collapse = ', '),'... Adding additional geometry column'))
    }
    print('adding geometry column to table 1')
    DBI::dbGetQuery(con, paste0("ALTER TABLE ", table_name," ADD COLUMN ", table_geom_fieldname," geometry(",geometry_type,",",srid,")"))
  }

  # populate geometry column 
  DBI::dbGetQuery(con, paste0("UPDATE ",table_name, 
                              ' SET "', table_geom_fieldname,'"',
                                  '= ', geotable_name, '."', geotable_geom_fieldname, '"',
                              " FROM ", geotable_name,
                              ' WHERE ', geotable_name, '."', geotable_geoid_fieldname,'"::varchar',
                                  ' = ', table_name, '."',table_geoid_fieldname ,'"::varchar',
                              ' AND ', where));
  
}
