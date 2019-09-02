# this script uploads two types of inflation data to the housing staging db. 

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))


# setup db connection - enter pwd at promp
source("postgis_connect.R")
con_pg <- make_postgis_con(mydb = 'housing-2019-staging', myuser = 'housing2019', pwd = pw,
                           host = 'housing-2019-staging.caicgny9d8nv.us-west-2.rds.amazonaws.com')

# Inflation data 1: 
# A table from Nick Kobel (CPI-U-RS - research series for adjusting census data, as recommended by the census).
cpi_u_rs_1950 <- rio::import("../data/external/cpi-u-rs_1950-current.xlsx")
dbWriteTable(con_pg, "cpi_u_rs_1950", cpi_u_rs_1950, row.names=FALSE)

# Inflation data 2: 
# annual inflation data from https://data.bls.gov/pdq/SurveyOutputServlet
# with computes cumalative inflation from each year through 2018
# there's something weird where it doesn't mesh with inflation calculator here: https://data.bls.gov/cgi-bin/cpicalc.pl?cost1=100.00&year1=201712&year2=201812
# not sure if discrepancy is because I'm using annual rather than monthly, or sometime about time-windows, or number of significant digits, or something else. 

bls_inflation <- read.csv("../data/external/bls_annual_inflation.csv") %>% 
  rename(inflation = Annual, year = Year) %>% 
  arrange(desc(year)) %>% 
  mutate(cumulative_inflation = cumprod(1 + (inflation / 100)) ) 

dbWriteTable(con_pg, "bls_inflation", bls_inflation, row.names=FALSE)

