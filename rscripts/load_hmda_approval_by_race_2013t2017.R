##### Load libraries, set working directory #####
if(!require(pacman)){install.packages("pacman");library(pacman)}
p_load(tidyverse, RPostgreSQL)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

##### Posting to PostGRES database #####
# source the postgresql password
source("../server_password.R") # pw <- {'PASSWORD'}

homeownership_by_race_2017_3 <- readRDS("../data/processed/homeownership_by_race_2017_HMDA_summary.RDS")

# loads the PostgreSQL driver
drv <- dbDriver("PostgreSQL")
# creates a connection to the postgres database
# note that "con" will be used later in each connection to the database
con <- dbConnect(drv, dbname = "housing-2019-staging",
                 host = "housing-2019-staging.caicgny9d8nv.us-west-2.rds.amazonaws.com", port = 5432,
                 user = "housing2019", password = pw)
rm(pw) # removes the password

# dbSendQuery(con, "DROP TABLE SC2_hmda_approval_by_race_2013t2017")

# if the table doesn't exist, create it, otherwise notify in console
if (!(dbExistsTable(con, "SC2_hmda_approval_by_race_2013t2017"))) {
  dbWriteTable(con, "SC2_hmda_approval_by_race_2013t2017", homeownership_by_race_2017_3, row.names = FALSE)
} else {
  print("Table already in database")
}
