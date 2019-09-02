##### Load libraries, set working directory #####
if(!require(pacman)){install.packages("pacman");library(pacman)}
p_load(tidyverse, tigris, tidycensus, rio, viridis, sf, mapview, plotly)
options(tigris_use_cache = T); options(tigris_class = "sf")

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

##### Define geographies and get geometries #####
pdx_msa_definition <- c('41005', '41009', '41051', '41067', '41071', '53011', '53059')

pdxcnty <- pdx_msa_definition%>%substr(., 3,5)

msa_geo <- tigris::core_based_statistical_areas(cb = TRUE) %>% filter(GEOID == "38900")

places_in_msa <- (tigris::places(state = c("or", "wa"), cb = TRUE) %>%
                    filter(lengths(st_intersects(., msa_geo)) > 0) %>%
                    st_set_geometry(NULL) %>%
                    select(GEOID))[,]

##### Download and summarize ACS data #####
# acs17 <- load_variables(year = 2017, dataset = "acs5", cache = T)

make_race_explicit <- function(acs_query) {
  acs_query <- acs_query %>%
    mutate(hh_income_total = B19013_001E,
           hh_income_white = B19013A_001E,
           hh_income_black = B19013B_001E,
           hh_income_aian = B19013C_001E,
           hh_income_asian = B19013D_001E,
           hh_income_nhpi = B19013E_001E,
           hh_income_other = B19013F_001E,
           hh_income_multi = B19013G_001E,
           hh_income_whitenh = B19013H_001E,
           hh_income_hisp = B19013I_001E) %>%
    select(GEOID, NAME, hh_income_total:hh_income_hisp)
  return(acs_query)
}

tract_income_query <- get_acs(geography = "tract",
                              state = c("OR", "WA"),
                              county = pdxcnty,
                              survey = "acs5",
                              year = 2017,
                              output = 'wide',
                              cache_table = TRUE,
                              variables = c("B19013_001", "B19013A_001", "B19013B_001",
                                            "B19013C_001", "B19013D_001", "B19013E_001",
                                            "B19013F_001", "B19013G_001", "B19013H_001",
                                            "B19013I_001"))

tract_median_household_income <- tract_income_query %>%
  filter(substr(GEOID,1,5) %in% pdx_msa_definition) %>%
  make_race_explicit(.) %>%
  mutate(sumlevel = "tract")

msa_income_query <- get_acs(
  geography = "metropolitan statistical area/micropolitan statistical area",
  year = 2017,
  survey = "acs5",
  output = "wide",
  cache_table = TRUE,
  variables = c("B19013_001", "B19013A_001", "B19013B_001",
                "B19013C_001", "B19013D_001", "B19013E_001",
                "B19013F_001", "B19013G_001", "B19013H_001",
                "B19013I_001"))

msa_median_household_income <- msa_income_query %>% filter(GEOID == "38900") %>%
  make_race_explicit(.) %>%
  mutate(sumlevel = "msa")

place_income_query <- get_acs(
  geography = "place",
  state = c("or", "wa"),
  year = 2017,
  survey = "acs5",
  output = "wide",
  cache_table = TRUE,
  variables = c("B19013_001", "B19013A_001", "B19013B_001",
                "B19013C_001", "B19013D_001", "B19013E_001",
                "B19013F_001", "B19013G_001", "B19013H_001",
                "B19013I_001"))

place_median_household_income <- place_income_query %>%
  filter(GEOID %in% places_in_msa) %>%
  make_race_explicit(.) %>%
  mutate(sumlevel = "place")

median_household_income_by_race_2017 <- rbind(msa_median_household_income, 
                                              place_median_household_income, 
                                              tract_median_household_income) %>%
  select(GEOID, sumlevel, NAME, hh_income_total:hh_income_hisp)

saveRDS(median_household_income_by_race_2017, "../data/processed/median_household_income_by_race_2017.RDS")




##### Data viz exploration #####

mhhinc <- median_household_income_by_race_2017 %>%
  filter(NAME %in% c("Portland-Vancouver-Hillsboro, OR-WA Metro Area",
                     "Portland city, Oregon")) %>%
  select(NAME, hh_income_total:hh_income_hisp) %>%
  rename(Total = hh_income_total,
         White = hh_income_white,
         Black = hh_income_black,
         `Native American` = hh_income_aian,
         `Asian` = hh_income_asian,
         `Pacific Isalnder` = hh_income_nhpi,
         `Another race` = hh_income_other,
         `Two or more races` = hh_income_multi,
         `White, not Latino` = hh_income_whitenh,
         Latino = hh_income_hisp) %>%
  gather(., c(Total:Latino), key = "Race", value = "Median household income")

mhhinc %>%
  filter(NAME == "Portland-Vancouver-Hillsboro, OR-WA Metro Area") %>%
  ggplot(aes(y = `Median household income`, x = reorder(Race, `Median household income`), fill = Race)) +
  geom_bar(position = "dodge", stat = "identity") +
  coord_flip() #+
  # facet_wrap(~NAME)
  
  
  
  