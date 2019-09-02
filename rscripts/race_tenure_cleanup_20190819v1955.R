##### Load libraries, set working directory #####
if(!require(pacman)){install.packages("pacman");library(pacman)}
p_load(tidyverse, tidycensus)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

pdx_msa_definition <- c('41005', '41009', '41051', '41067', '41071', '53011', '53059')
pdxcnty <- substr(pdx_msa_definition, start = 3, stop = 5)

### Data came from NHGIS from an extract from Nick Kobel; includes three tables at the
## Census tract level (standardized to 2010): Tables CN1 and CY5 and CZ6
race_tenure <- read_csv("../data/raw/nhgis0026_csv.zip")

race_tenure.pdx <- race_tenure %>%
  mutate(county_fips = paste0(STATEA, COUNTYA),
         tract_fips = paste0(county_fips, TRACTA)) %>%
  filter(county_fips %in% pdx_msa_definition) %>%
  mutate(source = "NHGIS, Decennial Census",
         total_hh = CN1AA + CN1AB,
         total_own = CN1AA,
         share_own = total_own / total_hh, # Share of households that are owners
         
         total_hh_white = CY5AA + CY5AG,
         total_own_white = CY5AA,
         share_own_white = total_own_white / total_hh_white, # Share of white households that are owners
         
         total_hh_black = CY5AB + CY5AH,
         total_own_black = CY5AB,
         share_own_black = total_own_black / total_hh_black, # Share of Black households that are owners
         
         total_hh_aian = CY5AC + CY5AI, # American Indian/Alaska Native
         total_own_aian = CY5AC,
         share_own_aian = total_own_aian / total_hh_aian,
         
         total_hh_api = CY5AD + CY5AJ, # Asian/Pacific Islander
         total_own_api = CY5AD,
         share_own_api = total_own_api / total_hh_api,
         
         total_hh_other = CY5AE + CY5AK,
         total_own_other = CY5AE,
         share_own_other = total_own_other / total_hh_other,
         
         total_hh_multi = CY5AF + CY5AL,
         total_own_multi = CY5AF,
         share_own_multi = total_own_multi / total_hh_multi,
         
         total_hh_hisp = CZ6AB + CZ6AD,
         total_own_hisp = CZ6AB,
         share_own_hisp = total_own_hisp / total_hh_hisp,
         
         share_total_own_white = total_own_white / total_own, # Share of total homeowners that were white
         share_total_own_black = total_own_black / total_own, # Share of total homeowners that were Black, etc.
         share_total_own_aian = total_own_aian / total_own, # American Indian/Alaska Native
         share_total_own_api = total_own_api / total_own, # Asian/Pacific Islander
         share_total_own_other = total_own_other / total_own,
         share_total_own_multi = total_own_multi / total_own,
         share_total_own_hisp = total_own_hisp / total_own) %>%
  select(DATAYEAR, source, tract_fips:share_total_own_hisp)


homeownership_by_race_2017 <- get_acs(geography = "tract",
                                      state = c("OR", "WA"),
                                      county = pdxcnty,
                                      survey = "acs5",
                                      year = 2017,
                                      output = 'wide',
                                      cache_table = TRUE,
                                      geometry = FALSE,
                                      variables = c('B25003_001', 'B25003_002', 'B25003_003',
                                                    'B25003A_001', 'B25003A_002', 'B25003A_003',
                                                    'B25003B_001', 'B25003B_002', 'B25003B_003',
                                                    'B25003C_001', 'B25003C_002', 'B25003C_003',
                                                    'B25003D_001', 'B25003D_002', 'B25003D_003',
                                                    'B25003E_001', 'B25003E_002', 'B25003E_003',
                                                    'B25003F_001', 'B25003F_002', 'B25003F_003',
                                                    'B25003G_001', 'B25003G_002', 'B25003G_003',
                                                    'B25003H_001', 'B25003H_002', 'B25003H_003',
                                                    'B25003I_001', 'B25003I_002', 'B25003I_003'))

homeownership_by_race_2017_2 <- homeownership_by_race_2017 %>%
  mutate(county = substr(GEOID, start = 1, stop = 5)) %>%
  filter(county %in% pdx_msa_definition) %>%
  mutate(DATAYEAR = 2017,
         source = "ACS, 5-year estimates",
         tract_fips = GEOID,
         total_hh = B25003_001E,
         total_own = B25003_002E,
         share_own = total_own / total_hh, # Share of households that are owners
         
         total_hh_white = B25003A_001E,
         total_own_white = B25003A_002E,
         share_own_white = total_own_white / total_hh_white, # Share of white households that are owners
         
         total_hh_black = B25003B_001E,
         total_own_black = B25003B_002E,
         share_own_black = total_own_black / total_hh_black, # Share of Black households that are owners
         
         total_hh_aian = B25003C_001E, # American Indian/Alaska Native
         total_own_aian = B25003C_002E,
         share_own_aian = total_own_aian / total_hh_aian,
         
         total_hh_api = B25003D_001E + B25003E_001E, # Asian/Pacific Islander
         total_own_api = B25003D_002E + B25003E_002E,
         share_own_api = total_own_api / total_hh_api,
         
         total_hh_other = B25003F_001E,
         total_own_other = B25003F_002E,
         share_own_other = total_own_other / total_hh_other,
         
         total_hh_multi = B25003G_001E,
         total_own_multi = B25003G_002E,
         share_own_multi = total_own_multi / total_hh_multi,
         
         total_hh_hisp = B25003I_001E,
         total_own_hisp = B25003I_002E,
         share_own_hisp = total_own_hisp / total_hh_hisp,
         
         share_total_own_white = total_own_white / total_own, # Share of total homeowners that were white
         share_total_own_black = total_own_black / total_own, # Share of total homeowners that were Black, etc.
         share_total_own_aian = total_own_aian / total_own, # American Indian/Alaska Native
         share_total_own_api = total_own_api / total_own, # Asian/Pacific Islander
         share_total_own_other = total_own_other / total_own,
         share_total_own_multi = total_own_multi / total_own,
         share_total_own_hisp = total_own_hisp / total_own) %>%
  select(DATAYEAR:share_total_own_hisp)

homeownership_by_race_2011 <- get_acs(geography = "tract",
                                      state = "or",
                                      county = pdxcnty,
                                      survey = "acs5",
                                      year = 2011,
                                      output = 'wide',
                                      cache_table = TRUE,
                                      geometry = FALSE,
                                      variables = c('B25003_001', 'B25003_002', 'B25003_003',
                                                    'B25003A_001', 'B25003A_002', 'B25003A_003',
                                                    'B25003B_001', 'B25003B_002', 'B25003B_003',
                                                    'B25003C_001', 'B25003C_002', 'B25003C_003',
                                                    'B25003D_001', 'B25003D_002', 'B25003D_003',
                                                    'B25003E_001', 'B25003E_002', 'B25003E_003',
                                                    'B25003F_001', 'B25003F_002', 'B25003F_003',
                                                    'B25003G_001', 'B25003G_002', 'B25003G_003',
                                                    'B25003H_001', 'B25003H_002', 'B25003H_003',
                                                    'B25003I_001', 'B25003I_002', 'B25003I_003')) %>%
  rbind(., get_acs(geography = "tract",
                   state = "wa",
                   county = pdxcnty,
                   survey = "acs5",
                   year = 2011,
                   output = 'wide',
                   cache_table = TRUE,
                   geometry = FALSE,
                   variables = c('B25003_001', 'B25003_002', 'B25003_003',
                                 'B25003A_001', 'B25003A_002', 'B25003A_003',
                                 'B25003B_001', 'B25003B_002', 'B25003B_003',
                                 'B25003C_001', 'B25003C_002', 'B25003C_003',
                                 'B25003D_001', 'B25003D_002', 'B25003D_003',
                                 'B25003E_001', 'B25003E_002', 'B25003E_003',
                                 'B25003F_001', 'B25003F_002', 'B25003F_003',
                                 'B25003G_001', 'B25003G_002', 'B25003G_003',
                                 'B25003H_001', 'B25003H_002', 'B25003H_003',
                                 'B25003I_001', 'B25003I_002', 'B25003I_003')))

homeownership_by_race_2011_2 <- homeownership_by_race_2011 %>%
  mutate(county = substr(GEOID, start = 1, stop = 5)) %>%
  filter(county %in% pdx_msa_definition) %>%
  mutate(DATAYEAR = 2011,
         source = "ACS, 5-year estimates",
         tract_fips = GEOID,
         total_hh = B25003_001E,
         total_own = B25003_002E,
         share_own = total_own / total_hh, # Share of households that are owners
         
         total_hh_white = B25003A_001E,
         total_own_white = B25003A_002E,
         share_own_white = total_own_white / total_hh_white, # Share of white households that are owners
         
         total_hh_black = B25003B_001E,
         total_own_black = B25003B_002E,
         share_own_black = total_own_black / total_hh_black, # Share of Black households that are owners
         
         total_hh_aian = B25003C_001E, # American Indian/Alaska Native
         total_own_aian = B25003C_002E,
         share_own_aian = total_own_aian / total_hh_aian,
         
         total_hh_api = B25003D_001E + B25003E_001E, # Asian/Pacific Islander
         total_own_api = B25003D_002E + B25003E_002E,
         share_own_api = total_own_api / total_hh_api,
         
         total_hh_other = B25003F_001E,
         total_own_other = B25003F_002E,
         share_own_other = total_own_other / total_hh_other,
         
         total_hh_multi = B25003G_001E,
         total_own_multi = B25003G_002E,
         share_own_multi = total_own_multi / total_hh_multi,
         
         total_hh_hisp = B25003I_001E,
         total_own_hisp = B25003I_002E,
         share_own_hisp = total_own_hisp / total_hh_hisp,
         
         share_total_own_white = total_own_white / total_own, # Share of total homeowners that were white
         share_total_own_black = total_own_black / total_own, # Share of total homeowners that were Black, etc.
         share_total_own_aian = total_own_aian / total_own, # American Indian/Alaska Native
         share_total_own_api = total_own_api / total_own, # Asian/Pacific Islander
         share_total_own_other = total_own_other / total_own,
         share_total_own_multi = total_own_multi / total_own,
         share_total_own_hisp = total_own_hisp / total_own) %>%
  select(DATAYEAR:share_total_own_hisp)

race_by_tenure_1990t2017 <- rbind(race_tenure.pdx, homeownership_by_race_2011_2, homeownership_by_race_2017_2)

saveRDS(race_by_tenure_1990t2017, "../data/processed/race_by_tenure_1990t2017.RDS")
