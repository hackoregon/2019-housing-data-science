## This file produces three cleaned datasets
##   1) Cleaned/recoded HMDA data by race for 2007-2017
##   2) HMDA summaries joined to ACS data for 2007-2011 (sf)
##   3) HMDA summaries joined to ACS data for 2013-2017 (sf)

##### Load libraries, set working directory #####
if(!require(pacman)){install.packages("pacman");library(pacman)}
p_load(tidyverse, tigris, tidycensus, rio, viridis, sf, mapview, mapview)
options(tigris_use_cache = T); options(tigris_class = "sf")

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

pdx_msa_definition <- c('41005', '41009', '41051', '41067', '41071', '53011', '53059')
pdxcnty <- substr(pdx_msa_definition, start = 3, stop = 5)

#### Get tracts and homeownership rates ####
## Get tracts for OR and WA and filter for Portland MSA
tracts <- tigris::tracts(state = 'or', cb = T, year = 2017) %>%
  rbind(., tigris::tracts(state = 'wa', cb = T, year = 2017)) %>%
  filter(paste0(STATEFP, COUNTYFP) %in% pdx_msa_definition)

## Load 2013-2017 homeownership rates 
homeownership_by_race_2017_2 <- readRDS("../data/processed/race_by_tenure_1990t2017.RDS") %>%
  filter(DATAYEAR == 2017) %>%
  # left_join(., tracts, by = c("tract_fips" = "GEOID")) %>%
  dplyr::inner_join(tracts, ., by = c("GEOID" = "tract_fips")) %>%
  select(GEOID, DATAYEAR:share_total_own_hisp, ALAND, AWATER, geometry)

## Load 2007-2011 homeownership rates 
homeownership_by_race_2011_2 <- readRDS("../data/processed/race_by_tenure_1990t2017.RDS") %>%
  filter(DATAYEAR == 2011) %>%
  # left_join(., tracts, by = c("tract_fips" = "GEOID")) %>%
  inner_join(tracts, ., by = c("GEOID" = "tract_fips")) %>%
  select(GEOID, DATAYEAR:share_total_own_hisp, ALAND, AWATER, geometry)

# mapview(homeownership_by_race_2017_2, burst = TRUE, hide = TRUE)

#### Load HMDA data ####
## Load in HMDA data. This HMDA data was downloaded from CPFB in 2018 by Nick Kobel at the Portland Bureau
## of Planning and Sustainability. He exported HMDA data for Oregon and Washington for all years using
## CPFB's platform and then filtered the dataset for those whose census tracts fell in the 7-county Portland MSA
hmda_lar_pdx_2007t2017 <- readRDS("../data/raw/hmda_lar_pdx_2007-2017.RDS")

msa_tracts <- (tracts %>% select(GEOID) %>% st_set_geometry(NULL) %>% unique(.))[,]

### Create inclusive race variables indicating whether any applicant/co-applicant's racial categories included the category
hmda_pdx_working <- hmda_lar_pdx_2007t2017 %>%
  mutate(race_white = case_when(str_detect(co_applicant_race_name_1, "White") ~ TRUE,
                                str_detect(co_applicant_race_name_2, "White") ~ TRUE,
                                str_detect(co_applicant_race_name_3, "White") ~ TRUE,
                                str_detect(co_applicant_race_name_4, "White") ~ TRUE,
                                str_detect(co_applicant_race_name_5, "White") ~ TRUE,
                                str_detect(applicant_race_name_1, "White") ~ TRUE,
                                str_detect(applicant_race_name_2, "White") ~ TRUE,
                                str_detect(applicant_race_name_3, "White") ~ TRUE,
                                str_detect(applicant_race_name_4, "White") ~ TRUE,
                                str_detect(applicant_race_name_5, "White") ~ TRUE,
                                TRUE ~ FALSE),
         race_black = case_when(str_detect(co_applicant_race_name_1, "Black") ~ TRUE,
                                str_detect(co_applicant_race_name_2, "Black") ~ TRUE,
                                str_detect(co_applicant_race_name_3, "Black") ~ TRUE,
                                str_detect(co_applicant_race_name_4, "Black") ~ TRUE,
                                str_detect(co_applicant_race_name_5, "Black") ~ TRUE,
                                str_detect(applicant_race_name_1, "Black") ~ TRUE,
                                str_detect(applicant_race_name_2, "Black") ~ TRUE,
                                str_detect(applicant_race_name_3, "Black") ~ TRUE,
                                str_detect(applicant_race_name_4, "Black") ~ TRUE,
                                str_detect(applicant_race_name_5, "Black") ~ TRUE,
                                TRUE ~ FALSE),
         race_aian = case_when(str_detect(co_applicant_race_name_1, "Indian") ~ TRUE,
                                str_detect(co_applicant_race_name_2, "Indian") ~ TRUE,
                                str_detect(co_applicant_race_name_3, "Indian") ~ TRUE,
                                str_detect(co_applicant_race_name_4, "Indian") ~ TRUE,
                                str_detect(co_applicant_race_name_5, "Indian") ~ TRUE,
                                str_detect(applicant_race_name_1, "Indian") ~ TRUE,
                                str_detect(applicant_race_name_2, "Indian") ~ TRUE,
                                str_detect(applicant_race_name_3, "Indian") ~ TRUE,
                                str_detect(applicant_race_name_4, "Indian") ~ TRUE,
                                str_detect(applicant_race_name_5, "Indian") ~ TRUE,
                                TRUE ~ FALSE),
         race_asian = case_when(str_detect(co_applicant_race_name_1, "Asian") ~ TRUE,
                               str_detect(co_applicant_race_name_2, "Asian") ~ TRUE,
                               str_detect(co_applicant_race_name_3, "Asian") ~ TRUE,
                               str_detect(co_applicant_race_name_4, "Asian") ~ TRUE,
                               str_detect(co_applicant_race_name_5, "Asian") ~ TRUE,
                               str_detect(applicant_race_name_1, "Asian") ~ TRUE,
                               str_detect(applicant_race_name_2, "Asian") ~ TRUE,
                               str_detect(applicant_race_name_3, "Asian") ~ TRUE,
                               str_detect(applicant_race_name_4, "Asian") ~ TRUE,
                               str_detect(applicant_race_name_5, "Asian") ~ TRUE,
                               TRUE ~ FALSE),
         race_nhpi = case_when(str_detect(co_applicant_race_name_1, "Hawaiian") ~ TRUE,
                                str_detect(co_applicant_race_name_2, "Hawaiian") ~ TRUE,
                                str_detect(co_applicant_race_name_3, "Hawaiian") ~ TRUE,
                                str_detect(co_applicant_race_name_4, "Hawaiian") ~ TRUE,
                                str_detect(co_applicant_race_name_5, "Hawaiian") ~ TRUE,
                                str_detect(applicant_race_name_1, "Hawaiian") ~ TRUE,
                                str_detect(applicant_race_name_2, "Hawaiian") ~ TRUE,
                                str_detect(applicant_race_name_3, "Hawaiian") ~ TRUE,
                                str_detect(applicant_race_name_4, "Hawaiian") ~ TRUE,
                                str_detect(applicant_race_name_5, "Hawaiian") ~ TRUE,
                                TRUE ~ FALSE),
         race_api = case_when(race_asian == TRUE ~ TRUE,
                              race_nhpi == TRUE ~ TRUE,
                              TRUE ~ FALSE),
         race_multi = case_when(str_detect(co_applicant_race_recoded, "Two") ~ TRUE,
                               str_detect(applicant_race_recoded, "Two") ~ TRUE,
                               TRUE ~ FALSE),
         race_hisp = case_when(co_applicant_ethnicity_name == "Hispanic or Latino" ~ TRUE,
                               applicant_ethnicity_name == "Hispanic or Latino" ~ TRUE,
                               TRUE ~ FALSE),
         race_whitenh = case_when(str_detect(co_applicant_race_recoded, "White") ~ TRUE,
                                str_detect(applicant_race_recoded, "White") ~ TRUE,
                                TRUE ~ FALSE),
         race_poc = case_when(race_black == TRUE ~ TRUE,
                              race_aian == TRUE ~ TRUE,
                              race_asian == TRUE ~ TRUE,
                              race_nhpi == TRUE ~ TRUE,
                              race_multi == TRUE ~ TRUE,
                              race_hisp == TRUE ~ TRUE,
                              TRUE ~ FALSE))

saveRDS(hmda_pdx_working, "../data/processed/hmda_pdx_working_2007t2017.RDS")

#### 2017 HMDA summary ####
## Subset of successful home purchase loans with complete racial demographic information
approved_loan_universe <- hmda_pdx_working %>%
  filter(as_of_year >= 2013,
         loan_purpose_name == "Home purchase",
         action_taken_name == "Loan originated",
         owner_occupancy_name == "Owner-occupied as a principal dwelling",
         !(race_whitenh == FALSE & race_poc == FALSE)) %>%
  rename(GEOID = tract_fips)

total_loans <- approved_loan_universe %>% group_by(GEOID) %>% summarize(loans_total = n())
total_white <- approved_loan_universe %>% filter(race_white == TRUE) %>% group_by(GEOID) %>% summarize(loans_white = n())
total_black <- approved_loan_universe %>% filter(race_black == TRUE) %>% group_by(GEOID) %>% summarize(loans_black = n())
total_aian <- approved_loan_universe %>% filter(race_aian == TRUE) %>% group_by(GEOID) %>% summarize(loans_aian = n())
total_asian <- approved_loan_universe %>% filter(race_asian == TRUE) %>% group_by(GEOID) %>% summarize(loans_asian = n())
total_nhpi <- approved_loan_universe %>% filter(race_nhpi == TRUE) %>% group_by(GEOID) %>% summarize(loans_nhpi = n())
total_api <- approved_loan_universe %>% filter(race_api == TRUE) %>% group_by(GEOID) %>% summarize(loans_api = n())
total_multi <- approved_loan_universe %>% filter(race_multi == TRUE) %>% group_by(GEOID) %>% summarize(loans_multi = n())
total_whitenh <- approved_loan_universe %>% filter(race_whitenh == TRUE) %>% group_by(GEOID) %>% summarize(loans_whitenh = n())
total_hisp <- approved_loan_universe %>% filter(race_hisp == TRUE) %>% group_by(GEOID) %>% summarize(loans_hisp = n())
total_poc <- approved_loan_universe %>% filter(race_poc == TRUE) %>% group_by(GEOID) %>% summarize(loans_poc = n())


## Join HMDA data summaries by race to demographic data on existing homeownership by race
homeownership_by_race_2017_3 <- homeownership_by_race_2017_2 %>%
  left_join(., total_loans, by = "GEOID") %>% left_join(., total_white, by = "GEOID") %>%
  left_join(., total_black, by = "GEOID") %>% left_join(., total_aian, by = "GEOID") %>%
  left_join(., total_api, by = "GEOID") %>%
  left_join(., total_multi, by = "GEOID") %>% left_join(., total_hisp, by = "GEOID") %>%
  mutate(loans_share_white = loans_white / loans_total,
         loans_share_black = loans_black / loans_total,
         loans_share_aian = loans_aian / loans_total,
         loans_share_api = loans_api / loans_total,
         loans_share_multi = loans_multi / loans_total,
         loans_share_hisp = loans_hisp / loans_total,
         lq_white = loans_share_white / share_total_own_white,
         lq_black = loans_share_black / share_total_own_black,
         lq_aian = loans_share_aian / share_total_own_aian,
         lq_api = loans_share_api / share_total_own_api,
         lq_multi = loans_share_multi / share_total_own_multi,
         lq_hisp = loans_share_hisp / share_total_own_hisp,
         lq_white.brks = cut(lq_white, breaks = c(-1, 0.5, 0.85, 1.15, 1.5, 100), labels = c("Not represented (< 0.50)", "Under-represented (0.50 - 0.84)", "Acceptable range (0.85-1.14)", "Slightly over-represented (1.15-1.49)", "Over-represented (1.50-3.00)")),
         lq_black.brks = cut(lq_black, breaks = c(-1, 0.5, 0.85, 1.15, 1.5, 100), labels = c("Not represented (< 0.50)", "Under-represented (0.50 - 0.84)", "Acceptable range (0.85-1.14)", "Slightly over-represented (1.15-1.49)", "Over-represented (1.50-3.00)")),
         lq_aian.brks = cut(lq_aian, breaks = c(-1, 0.5, 0.85, 1.15, 1.5, 100), labels = c("Not represented (< 0.50)", "Under-represented (0.50 - 0.84)", "Acceptable range (0.85-1.14)", "Slightly over-represented (1.15-1.49)", "Over-represented (1.50-3.00)")),
         lq_api.brks = cut(lq_api, breaks = c(-1, 0.5, 0.85, 1.15, 1.5, 100), labels = c("Not represented (< 0.50)", "Under-represented (0.50 - 0.84)", "Acceptable range (0.85-1.14)", "Slightly over-represented (1.15-1.49)", "Over-represented (1.50-3.00)")),
         lq_multi.brks = cut(lq_multi, breaks = c(-1, 0.5, 0.85, 1.15, 1.5, 100), labels = c("Not represented (< 0.50)", "Under-represented (0.50 - 0.84)", "Acceptable range (0.85-1.14)", "Slightly over-represented (1.15-1.49)", "Over-represented (1.50-3.00)")),
         lq_hisp.brks = cut(lq_hisp, breaks = c(-1, 0.5, 0.85, 1.15, 1.5, 100), labels = c("Not represented (< 0.50)", "Under-represented (0.50 - 0.84)", "Acceptable range (0.85-1.14)", "Slightly over-represented (1.15-1.49)", "Over-represented (1.50-3.00)")),
         share_hh_white = total_hh_white / total_hh,
         share_hh_black = total_hh_black / total_hh,
         share_hh_aian = total_hh_aian / total_hh,
         share_hh_api = total_hh_api / total_hh,
         share_hh_multi = total_hh_multi / total_hh,
         share_hh_hisp = total_hh_hisp / total_hh,
         lq_hh_white = loans_share_white / share_hh_white,
         lq_hh_black = loans_share_black / share_hh_black,
         lq_hh_aian = loans_share_aian / share_hh_aian,
         lq_hh_api = loans_share_api / share_hh_api,
         lq_hh_multi = loans_share_multi / share_hh_multi,
         lq_hh_hisp = loans_share_hisp / share_hh_hisp,
         lq_hh_white.brks = cut(lq_hh_white, breaks = c(-1, 0.5, 0.85, 1.15, 1.5, 100), labels = c("Not represented (< 0.50)", "Under-represented (0.50 - 0.84)", "Acceptable range (0.85-1.14)", "Slightly over-represented (1.15-1.49)", "Over-represented (1.50-3.00)")),
         lq_hh_black.brks = cut(lq_hh_black, breaks = c(-1, 0.5, 0.85, 1.15, 1.5, 100), labels = c("Not represented (< 0.50)", "Under-represented (0.50 - 0.84)", "Acceptable range (0.85-1.14)", "Slightly over-represented (1.15-1.49)", "Over-represented (1.50-3.00)")),
         lq_hh_aian.brks = cut(lq_hh_aian, breaks = c(-1, 0.5, 0.85, 1.15, 1.5, 100), labels = c("Not represented (< 0.50)", "Under-represented (0.50 - 0.84)", "Acceptable range (0.85-1.14)", "Slightly over-represented (1.15-1.49)", "Over-represented (1.50-3.00)")),
         lq_hh_api.brks = cut(lq_hh_api, breaks = c(-1, 0.5, 0.85, 1.15, 1.5, 100), labels = c("Not represented (< 0.50)", "Under-represented (0.50 - 0.84)", "Acceptable range (0.85-1.14)", "Slightly over-represented (1.15-1.49)", "Over-represented (1.50-3.00)")),
         lq_hh_multi.brks = cut(lq_hh_multi, breaks = c(-1, 0.5, 0.85, 1.15, 1.5, 100), labels = c("Not represented (< 0.50)", "Under-represented (0.50 - 0.84)", "Acceptable range (0.85-1.14)", "Slightly over-represented (1.15-1.49)", "Over-represented (1.50-3.00)")),
         lq_hh_hisp.brks = cut(lq_hh_hisp, breaks = c(-1, 0.5, 0.85, 1.15, 1.5, 100), labels = c("Not represented (< 0.50)", "Under-represented (0.50 - 0.84)", "Acceptable range (0.85-1.14)", "Slightly over-represented (1.15-1.49)", "Over-represented (1.50-3.00)"))) %>%
  select(GEOID:share_total_own_hisp, loans_total:lq_hh_hisp.brks, ALAND, AWATER, geometry) %>%
  mutate_each(funs(replace(., is.infinite(.), 0)), share_own:lq_hisp) %>%
  mutate_each(funs(replace(., is.na(.), 0)), share_own:lq_hisp)

saveRDS(homeownership_by_race_2017_3, "../data/processed/homeownership_by_race_2017_HMDA_summary.RDS")

#### 2007 - 2011 HMDA summary ####
## Subset of successful home purchase loans with complete racial demographic information
approved_loan_universe_2011 <- hmda_pdx_working %>%
  filter(as_of_year >= 2007 & as_of_year <= 2011,
         loan_purpose_name == "Home purchase",
         action_taken_name == "Loan originated",
         owner_occupancy_name == "Owner-occupied as a principal dwelling",
         !(race_whitenh == FALSE & race_poc == FALSE)) %>%
  rename(GEOID = tract_fips)

total_loans <- approved_loan_universe_2011 %>% group_by(GEOID) %>% summarize(loans_total = n())
total_white <- approved_loan_universe_2011 %>% filter(race_white == TRUE) %>% group_by(GEOID) %>% summarize(loans_white = n())
total_black <- approved_loan_universe_2011 %>% filter(race_black == TRUE) %>% group_by(GEOID) %>% summarize(loans_black = n())
total_aian <- approved_loan_universe_2011 %>% filter(race_aian == TRUE) %>% group_by(GEOID) %>% summarize(loans_aian = n())
total_asian <- approved_loan_universe_2011 %>% filter(race_asian == TRUE) %>% group_by(GEOID) %>% summarize(loans_asian = n())
total_nhpi <- approved_loan_universe_2011 %>% filter(race_nhpi == TRUE) %>% group_by(GEOID) %>% summarize(loans_nhpi = n())
total_api <- approved_loan_universe_2011 %>% filter(race_api == TRUE) %>% group_by(GEOID) %>% summarize(loans_api = n())
total_multi <- approved_loan_universe_2011 %>% filter(race_multi == TRUE) %>% group_by(GEOID) %>% summarize(loans_multi = n())
total_whitenh <- approved_loan_universe_2011 %>% filter(race_whitenh == TRUE) %>% group_by(GEOID) %>% summarize(loans_whitenh = n())
total_hisp <- approved_loan_universe_2011 %>% filter(race_hisp == TRUE) %>% group_by(GEOID) %>% summarize(loans_hisp = n())
total_poc <- approved_loan_universe_2011 %>% filter(race_poc == TRUE) %>% group_by(GEOID) %>% summarize(loans_poc = n())


## Join HMDA data summaries by race to demographic data on existing homeownership by race
homeownership_by_race_2011_3 <- homeownership_by_race_2011_2 %>%
  left_join(., total_loans, by = "GEOID") %>% left_join(., total_white, by = "GEOID") %>%
  left_join(., total_black, by = "GEOID") %>% left_join(., total_aian, by = "GEOID") %>%
  left_join(., total_api, by = "GEOID") %>%
  left_join(., total_multi, by = "GEOID") %>% left_join(., total_hisp, by = "GEOID") %>%
  mutate(loans_share_white = loans_white / loans_total,
         loans_share_black = loans_black / loans_total,
         loans_share_aian = loans_aian / loans_total,
         loans_share_api = loans_api / loans_total,
         loans_share_multi = loans_multi / loans_total,
         loans_share_hisp = loans_hisp / loans_total,
         lq_white = loans_share_white / share_total_own_white,
         lq_black = loans_share_black / share_total_own_black,
         lq_aian = loans_share_aian / share_total_own_aian,
         lq_api = loans_share_api / share_total_own_api,
         lq_multi = loans_share_multi / share_total_own_multi,
         lq_hisp = loans_share_hisp / share_total_own_hisp,
         lq_white.brks = cut(lq_white, breaks = c(-1, 0.5, 0.85, 1.15, 1.5, 100), labels = c("Not represented (< 0.50)", "Under-represented (0.50 - 0.84)", "Acceptable range (0.85-1.14)", "Slightly over-represented (1.15-1.49)", "Over-represented (1.50-3.00)")),
         lq_black.brks = cut(lq_black, breaks = c(-1, 0.5, 0.85, 1.15, 1.5, 100), labels = c("Not represented (< 0.50)", "Under-represented (0.50 - 0.84)", "Acceptable range (0.85-1.14)", "Slightly over-represented (1.15-1.49)", "Over-represented (1.50-3.00)")),
         lq_aian.brks = cut(lq_aian, breaks = c(-1, 0.5, 0.85, 1.15, 1.5, 100), labels = c("Not represented (< 0.50)", "Under-represented (0.50 - 0.84)", "Acceptable range (0.85-1.14)", "Slightly over-represented (1.15-1.49)", "Over-represented (1.50-3.00)")),
         lq_api.brks = cut(lq_api, breaks = c(-1, 0.5, 0.85, 1.15, 1.5, 100), labels = c("Not represented (< 0.50)", "Under-represented (0.50 - 0.84)", "Acceptable range (0.85-1.14)", "Slightly over-represented (1.15-1.49)", "Over-represented (1.50-3.00)")),
         lq_multi.brks = cut(lq_multi, breaks = c(-1, 0.5, 0.85, 1.15, 1.5, 100), labels = c("Not represented (< 0.50)", "Under-represented (0.50 - 0.84)", "Acceptable range (0.85-1.14)", "Slightly over-represented (1.15-1.49)", "Over-represented (1.50-3.00)")),
         lq_hisp.brks = cut(lq_hisp, breaks = c(-1, 0.5, 0.85, 1.15, 1.5, 100), labels = c("Not represented (< 0.50)", "Under-represented (0.50 - 0.84)", "Acceptable range (0.85-1.14)", "Slightly over-represented (1.15-1.49)", "Over-represented (1.50-3.00)")),
         share_hh_white = total_hh_white / total_hh,
         share_hh_black = total_hh_black / total_hh,
         share_hh_aian = total_hh_aian / total_hh,
         share_hh_api = total_hh_api / total_hh,
         share_hh_multi = total_hh_multi / total_hh,
         share_hh_hisp = total_hh_hisp / total_hh,
         lq_hh_white = loans_share_white / share_hh_white,
         lq_hh_black = loans_share_black / share_hh_black,
         lq_hh_aian = loans_share_aian / share_hh_aian,
         lq_hh_api = loans_share_api / share_hh_api,
         lq_hh_multi = loans_share_multi / share_hh_multi,
         lq_hh_hisp = loans_share_hisp / share_hh_hisp,
         lq_hh_white.brks = cut(lq_hh_white, breaks = c(-1, 0.5, 0.85, 1.15, 1.5, 100), labels = c("Not represented (< 0.50)", "Under-represented (0.50 - 0.84)", "Acceptable range (0.85-1.14)", "Slightly over-represented (1.15-1.49)", "Over-represented (1.50-3.00)")),
         lq_hh_black.brks = cut(lq_hh_black, breaks = c(-1, 0.5, 0.85, 1.15, 1.5, 100), labels = c("Not represented (< 0.50)", "Under-represented (0.50 - 0.84)", "Acceptable range (0.85-1.14)", "Slightly over-represented (1.15-1.49)", "Over-represented (1.50-3.00)")),
         lq_hh_aian.brks = cut(lq_hh_aian, breaks = c(-1, 0.5, 0.85, 1.15, 1.5, 100), labels = c("Not represented (< 0.50)", "Under-represented (0.50 - 0.84)", "Acceptable range (0.85-1.14)", "Slightly over-represented (1.15-1.49)", "Over-represented (1.50-3.00)")),
         lq_hh_api.brks = cut(lq_hh_api, breaks = c(-1, 0.5, 0.85, 1.15, 1.5, 100), labels = c("Not represented (< 0.50)", "Under-represented (0.50 - 0.84)", "Acceptable range (0.85-1.14)", "Slightly over-represented (1.15-1.49)", "Over-represented (1.50-3.00)")),
         lq_hh_multi.brks = cut(lq_hh_multi, breaks = c(-1, 0.5, 0.85, 1.15, 1.5, 100), labels = c("Not represented (< 0.50)", "Under-represented (0.50 - 0.84)", "Acceptable range (0.85-1.14)", "Slightly over-represented (1.15-1.49)", "Over-represented (1.50-3.00)")),
         lq_hh_hisp.brks = cut(lq_hh_hisp, breaks = c(-1, 0.5, 0.85, 1.15, 1.5, 100), labels = c("Not represented (< 0.50)", "Under-represented (0.50 - 0.84)", "Acceptable range (0.85-1.14)", "Slightly over-represented (1.15-1.49)", "Over-represented (1.50-3.00)"))) %>%
  select(GEOID:share_total_own_hisp, loans_total:lq_hh_hisp.brks, ALAND, AWATER, geometry) %>%
  mutate_each(funs(replace(., is.infinite(.), 0)), share_own:lq_hisp) %>%
  mutate_each(funs(replace(., is.na(.), 0)), share_own:lq_hisp)

saveRDS(homeownership_by_race_2011_3, "../data/processed/homeownership_by_race_2011_HMDA_summary.RDS")
