##### Load libraries, set working directory #####
if(!require(pacman)){install.packages("pacman");library(pacman)}
p_load(tidyverse, tigris, tidycensus, rio, viridis, sf, mapview, mapview, data.table)
options(tigris_use_cache = T); options(tigris_class = "sf")

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

pdx_msa_definition <- c('41005', '41009', '41051', '41067', '41071', '53011', '53059')

cpi <- rio::import("../data/external/cpi-u-rs_1950-current.xlsx") %>% select(year, inflation_factor_2018)

## Get median HH income by race for 1990
get_median_hh_income_by_race_1990_county_agg <- function(geoid_vector) {
  # Helper function 1
  get_income_value_keys <- function(dframe) {
    return_list_of_lists <- list(c(dframe[[1]], 0),
                                 c(dframe[[2]], 5000),
                                 c(dframe[[3]], 10000),
                                 c(dframe[[4]], 15000),
                                 c(dframe[[5]], 25000),
                                 c(dframe[[6]], 35000),
                                 c(dframe[[7]], 50000),
                                 c(dframe[[8]], 75000),
                                 c(dframe[[9]], 100000))
    return(return_list_of_lists)
  }
  
  # Helper function 2
  get_pareto_median <- function(list_of_lists) {
    
    # Make sure list is sorted in ascending order
    data_list <- list_of_lists[order(sapply(list_of_lists, '[[', 2))]
    
    # Pull out elements into separate lists to make things clearer later on.
    counts <- lapply(data_list, '[[', 1)
    bases <- lapply(data_list, '[[', 2)
    
    # Calculate the "widths" of each group as the difference between the bases
    widths = list()
    for (i in seq_along(bases[1:length(names(bases))-1])){
      val = bases[[i + 1]] - bases[[i]]
      widths <- append(widths, val)
    }
    
    # The last element returns an error because i + 1 is out of bounds. 
    # Just append 1, since there is no effective upper bound.
    # Could be fixed with a tryCatch().
    widths <- append(widths, 1)
    
    # Find the group containing the median, which will be the group at which
    # the cumulative counts is greater than the sum of all of the counts divided by 2
    target = Reduce("+",counts) / 2
    cumulative_counts = 1
    index = 1
    while (cumulative_counts <= target) {
      cumulative_counts = cumulative_counts + counts[[index]]
      index = index + 1
    }
    index = index - 1
    
    # Calculate the sum of all the groups prior to the one that contains the median
    previous_groups_sum = cumulative_counts - counts[[index]]
    
    # Last, estimate the median
    return(bases[[index]] + ((Reduce("+",counts) / 2 - previous_groups_sum) / counts[[index]]) * widths[[index]])
  }
  
  race_income_vars_90<- c('P0820001', 'P0820002', 'P0820003', 'P0820004', 'P0820005', 'P0820006', 'P0820007', 'P0820008', 
                          'P0820009', 'P0820010', 'P0820011', 'P0820012', 'P0820013', 'P0820014', 'P0820015', 'P0820016', 
                          'P0820017', 'P0820018', 'P0820019', 'P0820020', 'P0820021', 'P0820022', 'P0820023', 'P0820024', 
                          'P0820025', 'P0820026', 'P0820027', 'P0820028', 'P0820029', 'P0820030', 'P0820031', 'P0820032', 
                          'P0820033', 'P0820034', 'P0820035', 'P0820036', 'P0820037', 'P0820038', 'P0820039', 'P0820040', 
                          'P0820041', 'P0820042', 'P0820043', 'P0820044', 'P0820045')
  
  incgroups <- c('Less than $5,000', '$5,000-$9,999', '$10,000-$14,999', '$15,000-$24,999', 
                 '$25,000-$34,999', '$35,000-$49,999', '$50,000-$74,999', '$75,000-$99,999', '$100,000 or more')
  
  raceinc <- c(paste("White,", incgroups), 
               paste("Black,", incgroups),
               paste("AIAN,", incgroups),
               paste("API,", incgroups),
               paste("Other,", incgroups))
  
  # Query the data
  hhincome_api_query <- get_decennial(geography = "county", 
                                      variables = race_income_vars_90, 
                                      year = 1990, sumfile = "sf3", state = c("OR", "WA"), 
                                      geometry = F) 
  
  hhincome <- hhincome_api_query %>%
    mutate(variable = str_replace(variable, 'P0820', '')) %>%
    arrange(GEOID, variable) %>%
    mutate(group = rep(raceinc, length(unique(GEOID)))) %>%
    separate(group, into = c("race", "income"), sep = ", ", remove = F) %>%
    mutate(income = ordered(income, levels = c('Less than $5,000', '$5,000-$9,999', '$10,000-$14,999', 
                                               '$15,000-$24,999', '$25,000-$34,999', '$35,000-$49,999', 
                                               '$50,000-$74,999', '$75,000-$99,999', '$100,000 or more')),
           poc_rec = case_when(race != "White" ~ "POC",
                               TRUE ~ "White")) %>%
    filter(GEOID %in% geoid_vector) %>%
    select(race, income, value) %>%
    group_by(race, income) %>%
    summarize_all(sum) %>%
    ungroup() %>%
    spread(key = income, value = value)
  
  pull_median <- function(racevar, income_data) {
    ret <- income_data %>%
      filter(race == !!racevar) %>%
      select(-race) %>%
      get_income_value_keys(.) %>%
      get_pareto_median(.)
    return(ret)
  }
  
  aian <- pull_median(quo("AIAN"), hhincome)
  api <- pull_median(quo("API"), hhincome)
  black <- pull_median(quo("Black"), hhincome)
  other <- pull_median(quo("Other"), hhincome)
  white <- pull_median(quo("White"), hhincome)
  
  output <- rbind(aian, api, black, other, white) %>%
    as.data.frame(.) 
  
  setDT(output, keep.rownames = TRUE)[]
  
  output$year <- 1990
  output$source <- "Decennial Census"
  
  names(output) <- c("race", "median_income", "year", "source")
  
  return(output)
}

med_income_1990 <- get_median_hh_income_by_race_1990_county_agg(pdx_msa_definition)

## Get median HH income by race for 2000
get_median_hh_income_by_race_2000_county_agg <- function(geoid_vector) {
  # Helper function 1
  get_income_value_keys <- function(dframe) {
    return_list_of_lists <- list(c(dframe[[1]], 0),
                                 c(dframe[[2]], 10000),
                                 c(dframe[[3]], 15000),
                                 c(dframe[[4]], 20000),
                                 c(dframe[[5]], 25000),
                                 c(dframe[[6]], 30000),
                                 c(dframe[[7]], 35000),
                                 c(dframe[[8]], 40000),
                                 c(dframe[[9]], 45000),
                                 c(dframe[[10]], 50000),
                                 c(dframe[[11]], 60000),
                                 c(dframe[[12]], 75000),
                                 c(dframe[[13]], 100000),
                                 c(dframe[[14]], 125000),
                                 c(dframe[[15]], 150000),
                                 c(dframe[[16]], 200000))
    return(return_list_of_lists)
  }
  
  # Helper function 2
  get_pareto_median <- function(list_of_lists) {
    
    # Make sure list is sorted in ascending order
    data_list <- list_of_lists[order(sapply(list_of_lists, '[[', 2))]
    
    # Pull out elements into separate lists to make things clearer later on.
    counts <- lapply(data_list, '[[', 1)
    bases <- lapply(data_list, '[[', 2)
    
    # Calculate the "widths" of each group as the difference between the bases
    widths = list()
    for (i in seq_along(bases[1:length(names(bases))-1])){
      val = bases[[i + 1]] - bases[[i]]
      widths <- append(widths, val)
    }
    
    # The last element returns an error because i + 1 is out of bounds. 
    # Just append 1, since there is no effective upper bound.
    # Could be fixed with a tryCatch().
    widths <- append(widths, 1)
    
    # Find the group containing the median, which will be the group at which
    # the cumulative counts is greater than the sum of all of the counts divided by 2
    target = Reduce("+",counts) / 2
    cumulative_counts = 1
    index = 1
    while (cumulative_counts <= target) {
      cumulative_counts = cumulative_counts + counts[[index]]
      index = index + 1
    }
    index = index - 1
    
    # Calculate the sum of all the groups prior to the one that contains the median
    previous_groups_sum = cumulative_counts - counts[[index]]
    
    # Last, estimate the median
    return(bases[[index]] + ((Reduce("+",counts) / 2 - previous_groups_sum) / counts[[index]]) * widths[[index]])
  }
  
  # Name variables to gather
  race_income_vars_00 <- c('P151A001', 'P151A002', 'P151A003', 'P151A004', 'P151A005', 'P151A006', 'P151A007', 'P151A008', 
                           'P151A009', 'P151A010', 'P151A011', 'P151A012', 'P151A013', 'P151A014', 'P151A015', 'P151A016', 
                           'P151A017', 'P151B001', 'P151B002', 'P151B003', 'P151B004', 'P151B005', 'P151B006', 'P151B007', 
                           'P151B008', 'P151B009', 'P151B010', 'P151B011', 'P151B012', 'P151B013', 'P151B014', 'P151B015', 
                           'P151B016', 'P151B017', 'P151C001', 'P151C002', 'P151C003', 'P151C004', 'P151C005', 'P151C006', 
                           'P151C007', 'P151C008', 'P151C009', 'P151C010', 'P151C011', 'P151C012', 'P151C013', 'P151C014', 
                           'P151C015', 'P151C016', 'P151C017', 'P151D001', 'P151D002', 'P151D003', 'P151D004', 'P151D005', 
                           'P151D006', 'P151D007', 'P151D008', 'P151D009', 'P151D010', 'P151D011', 'P151D012', 'P151D013', 
                           'P151D014', 'P151D015', 'P151D016', 'P151D017', 'P151E001', 'P151E002', 'P151E003', 'P151E004', 
                           'P151E005', 'P151E006', 'P151E007', 'P151E008', 'P151E009', 'P151E010', 'P151E011', 'P151E012', 
                           'P151E013', 'P151E014', 'P151E015', 'P151E016', 'P151E017', 'P151F001', 'P151F002', 'P151F003', 
                           'P151F004', 'P151F005', 'P151F006', 'P151F007', 'P151F008', 'P151F009', 'P151F010', 'P151F011', 
                           'P151F012', 'P151F013', 'P151F014', 'P151F015', 'P151F016', 'P151F017', 'P151G001', 'P151G002', 
                           'P151G003', 'P151G004', 'P151G005', 'P151G006', 'P151G007', 'P151G008', 'P151G009', 'P151G010', 
                           'P151G011', 'P151G012', 'P151G013', 'P151G014', 'P151G015', 'P151G016', 'P151G017', 'P151H001', 
                           'P151H002', 'P151H003', 'P151H004', 'P151H005', 'P151H006', 'P151H007', 'P151H008', 'P151H009', 
                           'P151H010', 'P151H011', 'P151H012', 'P151H013', 'P151H014', 'P151H015', 'P151H016', 'P151H017', 
                           'P151I001', 'P151I002', 'P151I003', 'P151I004', 'P151I005', 'P151I006', 'P151I007', 'P151I008', 
                           'P151I009', 'P151I010', 'P151I011', 'P151I012', 'P151I013', 'P151I014', 'P151I015', 'P151I016', 
                           'P151I017')
  
  # Define income groups
  incgroups <- c('Less than $10,000', '$10,000 to $14,999', '$15,000 to $19,999', '$20,000 to $24,999',
                 '$25,000 to $29,999', '$30,000 to $34,999', '$35,000 to $39,999', '$40,000 to $44,999',
                 '$45,000 to $49,999', '$50,000 to $59,999', '$60,000 to $74,999', '$75,000 to $99,999',
                 '$100,000 to $124,999', '$125,000 to $149,999', '$150,000 to $199,999', '$200,000 or more')
  
  # Make vector of race-income combinations
  raceinc <- c(paste("White,", incgroups), paste("Black,", incgroups), paste("AIAN,", incgroups),
               paste("Asian,", incgroups), paste("NHPI,", incgroups), paste("Other,", incgroups),
               paste("Multi,", incgroups), paste("Hispanic,", incgroups), paste("White NH,", incgroups))
  
  # Query the data
  hhincome_api_query <- get_decennial(geography = "county", 
                                      variables = race_income_vars_00, 
                                      year = 2000, sumfile = "sf3", state = c("OR", "WA"), 
                                      geometry = F)
  
  # Clean the data. 0) filter only for intersted tracts; 1) add state-county fips;
  # 2) clean up/remove variable name and race code;
  # 3) filter out "total HH"; 4) arrange by GEOID, race code and variable number
  # Create group column that gives human-readable names to race codes and variable names
  # More cleanup. 1) Separate group into two columns: race and income; 2) make income ordered factor;
  # 3) remove group and race_code variables; 4) select only variables interested in
  hhincome <- hhincome_api_query %>%
    filter(GEOID %in% geoid_vector) %>%
    mutate(variable = str_replace(variable, 'P151', ''),
           race_code = substr(x = variable, start = 1, stop = 1),
           variable = substr(x = variable, start = 2, stop = 4)) %>%
    filter(variable != "001") %>%
    arrange(GEOID, race_code, variable) %>%
    mutate(group = rep(raceinc, length(unique(GEOID)))) %>%
    separate(group, into = c("race", "income"), sep = ", ", remove = F) %>%
    mutate(income = ordered(income, levels = incgroups),
           race_rec = race,
           race_rec = case_when(race == "Asian" | race == "NHPI" ~ "API",
                                race == "Other" | race == "Multi" ~ "Other",
                                TRUE ~ race),
           poc_rec = case_when(race == "White" ~ "White",
                               race != "White NH" ~ "POC",
                               TRUE ~ "White NH")) # This line recodes `race_rec` to `race` for all other instances
  
  ### Return income for race codes that match 1990 values
  
  hhincome_1990_compatible <- hhincome %>%
    select(race_rec, income, value) %>%
    group_by(race_rec, income) %>%
    summarize_all(sum) %>%
    ungroup() %>%
    spread(key = income, value = value)
  
  
  
  pull_median <- function(racevar, income_data) {
    ret <- income_data %>%
      filter(race_rec == !!racevar) %>%
      select(-race_rec) %>%
      get_income_value_keys(.) %>%
      get_pareto_median(.)
    return(ret)
  }
  
  aian <- pull_median(quo("AIAN"), hhincome_1990_compatible)
  api <- pull_median(quo("API"), hhincome_1990_compatible)
  black <- pull_median(quo("Black"), hhincome_1990_compatible)
  other <- pull_median(quo("Other"), hhincome_1990_compatible)
  white <- pull_median(quo("White"), hhincome_1990_compatible)
  hisp <- pull_median(quo("Hispanic"), hhincome_1990_compatible)
  whitenh <- pull_median(quo("White NH"), hhincome_1990_compatible)
  
  output <- rbind(aian, api, black, other, white, hisp, whitenh) %>%
    as.data.frame(.) 
  
  setDT(output, keep.rownames = TRUE)[]
  
  output$year <- 2000
  output$source <- "Decennial Census"
  
  names(output) <- c("race", "median_income", "year", "source")
  
  return(output)
}

med_income_2000 <- get_median_hh_income_by_race_2000_county_agg(pdx_msa_definition)

## Get median HH income by race for ACS
get_median_hh_income_by_race_acs_county_agg <- function(geoid_vector, acs_year = 2017) {
  
  get_income_value_keys <- function(dframe) {
    return_list_of_lists <- list(c(dframe[[1]], 0),
                                 c(dframe[[2]], 10000),
                                 c(dframe[[3]], 15000),
                                 c(dframe[[4]], 20000),
                                 c(dframe[[5]], 25000),
                                 c(dframe[[6]], 30000),
                                 c(dframe[[7]], 35000),
                                 c(dframe[[8]], 40000),
                                 c(dframe[[9]], 45000),
                                 c(dframe[[10]], 50000),
                                 c(dframe[[11]], 60000),
                                 c(dframe[[12]], 75000),
                                 c(dframe[[13]], 100000),
                                 c(dframe[[14]], 125000),
                                 c(dframe[[15]], 150000),
                                 c(dframe[[16]], 200000))
    return(return_list_of_lists)
  }
  
  get_pareto_median <- function(list_of_lists) {
    
    # Make sure list is sorted in ascending order
    data_list <- list_of_lists[order(sapply(list_of_lists, '[[', 2))]
    
    # Pull out elements into separate lists to make things clearer later on.
    counts <- lapply(data_list, '[[', 1)
    bases <- lapply(data_list, '[[', 2)
    
    # Calculate the "widths" of each group as the difference between the bases
    widths = list()
    for (i in seq_along(bases[1:length(names(bases))-1])){
      val = bases[[i + 1]] - bases[[i]]
      widths <- append(widths, val)
    }
    
    # The last element returns an error because i + 1 is out of bounds. 
    # Just append 1, since there is no effective upper bound.
    # Could be fixed with a tryCatch().
    widths <- append(widths, 1)
    
    # Find the group containing the median, which will be the group at which
    # the cumulative counts is greater than the sum of all of the counts divided by 2
    target = Reduce("+",counts) / 2
    cumulative_counts = 1
    index = 1
    while (cumulative_counts <= target) {
      cumulative_counts = cumulative_counts + counts[[index]]
      index = index + 1
    }
    index = index - 1
    
    # Calculate the sum of all the groups prior to the one that contains the median
    previous_groups_sum = cumulative_counts - counts[[index]]
    
    # Last, estimate the median
    return(bases[[index]] + ((Reduce("+",counts) / 2 - previous_groups_sum) / counts[[index]]) * widths[[index]])
  }
  
  race_income_vars_2010 <- c('B19001A_001', 'B19001A_002', 'B19001A_003', 'B19001A_004', 'B19001A_005', 'B19001A_006', 'B19001A_007', 
                             'B19001A_008', 'B19001A_009', 'B19001A_010', 'B19001A_011', 'B19001A_012', 'B19001A_013', 'B19001A_014', 
                             'B19001A_015', 'B19001A_016', 'B19001A_017', 'B19001B_001', 'B19001B_002', 'B19001B_003', 'B19001B_004', 
                             'B19001B_005', 'B19001B_006', 'B19001B_007', 'B19001B_008', 'B19001B_009', 'B19001B_010', 'B19001B_011', 
                             'B19001B_012', 'B19001B_013', 'B19001B_014', 'B19001B_015', 'B19001B_016', 'B19001B_017', 'B19001C_001', 
                             'B19001C_002', 'B19001C_003', 'B19001C_004', 'B19001C_005', 'B19001C_006', 'B19001C_007', 'B19001C_008', 
                             'B19001C_009', 'B19001C_010', 'B19001C_011', 'B19001C_012', 'B19001C_013', 'B19001C_014', 'B19001C_015', 
                             'B19001C_016', 'B19001C_017', 'B19001D_001', 'B19001D_002', 'B19001D_003', 'B19001D_004', 'B19001D_005', 
                             'B19001D_006', 'B19001D_007', 'B19001D_008', 'B19001D_009', 'B19001D_010', 'B19001D_011', 'B19001D_012', 
                             'B19001D_013', 'B19001D_014', 'B19001D_015', 'B19001D_016', 'B19001D_017', 'B19001E_001', 'B19001E_002', 
                             'B19001E_003', 'B19001E_004', 'B19001E_005', 'B19001E_006', 'B19001E_007', 'B19001E_008', 'B19001E_009', 
                             'B19001E_010', 'B19001E_011', 'B19001E_012', 'B19001E_013', 'B19001E_014', 'B19001E_015', 'B19001E_016', 
                             'B19001E_017', 'B19001F_001', 'B19001F_002', 'B19001F_003', 'B19001F_004', 'B19001F_005', 'B19001F_006', 
                             'B19001F_007', 'B19001F_008', 'B19001F_009', 'B19001F_010', 'B19001F_011', 'B19001F_012', 'B19001F_013', 
                             'B19001F_014', 'B19001F_015', 'B19001F_016', 'B19001F_017', 'B19001G_001', 'B19001G_002', 'B19001G_003', 
                             'B19001G_004', 'B19001G_005', 'B19001G_006', 'B19001G_007', 'B19001G_008', 'B19001G_009', 'B19001G_010', 
                             'B19001G_011', 'B19001G_012', 'B19001G_013', 'B19001G_014', 'B19001G_015', 'B19001G_016', 'B19001G_017', 
                             'B19001H_001', 'B19001H_002', 'B19001H_003', 'B19001H_004', 'B19001H_005', 'B19001H_006', 'B19001H_007', 
                             'B19001H_008', 'B19001H_009', 'B19001H_010', 'B19001H_011', 'B19001H_012', 'B19001H_013', 'B19001H_014', 
                             'B19001H_015', 'B19001H_016', 'B19001H_017', 'B19001I_001', 'B19001I_002', 'B19001I_003', 'B19001I_004', 
                             'B19001I_005', 'B19001I_006', 'B19001I_007', 'B19001I_008', 'B19001I_009', 'B19001I_010', 'B19001I_011', 
                             'B19001I_012', 'B19001I_013', 'B19001I_014', 'B19001I_015', 'B19001I_016', 'B19001I_017')
  
  incgroups <- c('Less than $10,000', '$10,000 to $14,999', '$15,000 to $19,999', '$20,000 to $24,999', 
                 '$25,000 to $29,999', '$30,000 to $34,999', '$35,000 to $39,999', '$40,000 to $44,999', 
                 '$45,000 to $49,999', '$50,000 to $59,999', '$60,000 to $74,999', '$75,000 to $99,999', 
                 '$100,000 to $124,999', '$125,000 to $149,999', '$150,000 to $199,999', '$200,000 or more')
  
  raceinc <- c(paste("White,", incgroups), 
               paste("Black,", incgroups),
               paste("AIAN,", incgroups),
               paste("Asian,", incgroups),
               paste("NHPI,", incgroups),
               paste("Other,", incgroups),
               paste("Multi,", incgroups),
               paste("White NH,", incgroups),
               paste("Hispanic,", incgroups))
  
  # Query the data
  hhincome_api_query <- get_acs(geography = "county", variables = race_income_vars_2010, 
                                year = acs_year, state = c("OR", "WA"), survey = 'acs5')
  
  # Clean the data. 0) filter only for intersted tracts; 1) add state-county fips;
  # 2) clean up/remove variable name and race code;
  # 3) filter out "total HH"; 4) arrange by GEOID, race code and variable number
  # Create group column that gives human-readable names to race codes and variable names
  # More cleanup. 1) Separate group into two columns: race and income; 2) make income ordered factor;
  # 3) remove group and race_code variables; 4) select only variables interested in
  hhincome <- hhincome_api_query %>%
    filter(GEOID %in% geoid_vector) %>%
    mutate(variable = str_replace(variable, 'B19001', ''),
           race_code = substr(x = variable, start = 1, stop = 1),
           variable = substr(x = variable, start = 3, stop = 5)) %>%
    filter(variable != "001") %>%
    mutate(group = rep(raceinc, length(unique(GEOID)))) %>%
    separate(group, into = c("race", "income"), sep = ", ", remove = F) %>%
    mutate(income = ordered(income, levels = incgroups),
           race_rec = race,
           race_rec = case_when(race == "Asian" | race == "NHPI" ~ "API",
                                race == "Other" | race == "Multi" ~ "Other",
                                TRUE ~ race),
           poc_rec = case_when(race == "White" ~ "White",
                               race != "White NH" ~ "POC",
                               TRUE ~ "White NH")) # This line recodes `race_rec` to `race` for all other instances
  
  ### Return income for race codes that match 1990 values
  hhincome_1990_compatible <- hhincome %>%
    select(race_rec, income, estimate) %>%
    group_by(race_rec, income) %>%
    summarize_all(sum) %>%
    ungroup() %>%
    spread(key = income, value = estimate)
  
  pull_median <- function(racevar, income_data) {
    ret <- income_data %>%
      filter(race_rec == !!racevar) %>%
      select(-race_rec) %>%
      get_income_value_keys(.) %>%
      get_pareto_median(.)
    return(ret)
  }
  
  aian <- pull_median(quo("AIAN"), hhincome_1990_compatible)
  api <- pull_median(quo("API"), hhincome_1990_compatible)
  black <- pull_median(quo("Black"), hhincome_1990_compatible)
  other <- pull_median(quo("Other"), hhincome_1990_compatible)
  white <- pull_median(quo("White"), hhincome_1990_compatible)
  hisp <- pull_median(quo("Hispanic"), hhincome_1990_compatible)
  whitenh <- pull_median(quo("White NH"), hhincome_1990_compatible)
  
  output <- rbind(aian, api, black, other, white, hisp, whitenh) %>%
    as.data.frame(.) 
  
  setDT(output, keep.rownames = TRUE)[]
  
  output$year <- acs_year
  output$source <- "5-year ACS"
  
  names(output) <- c("race", "median_income", "year", "source")
  
  return(output)
}

med_income_2010 <- get_median_hh_income_by_race_acs_county_agg (pdx_msa_definition, acs_year = 2010)
med_income_2012 <- get_median_hh_income_by_race_acs_county_agg (pdx_msa_definition, acs_year = 2012)
med_income_2017 <- get_median_hh_income_by_race_acs_county_agg (pdx_msa_definition, acs_year = 2017)


med_income_by_race <- rbind(med_income_1990, med_income_2000, med_income_2010, med_income_2012, med_income_2017) %>%
  left_join(., cpi, by = "year") %>%
  mutate(adjusted_median_income = median_income * inflation_factor_2018,
         aggregation = "7-county MSA",
         race_explicit = case_when(race == "aian" ~ "Native American",
                                   race == "api" ~ "Asian/Pacific Islander",
                                   race == "black" ~ "Black",
                                   race == "other" ~ "Another race",
                                   race == "white" ~ "White",
                                   race == "hisp" ~ "Hispanic or Latino",
                                   race == "whitenh" ~ "White, not Hispanic",
                                   TRUE ~ "ERR")) %>%
  filter(year != 2012, race != "whitenh")

saveRDS(med_income_by_race, "../data/processed/median_household_income_by_race_1990t2017_MSA.RDS")
med_income_by_race <- readRDS("../data/processed/median_household_income_by_race_1990t2017_MSA.RDS")

med_income_by_race %>%
  ggplot(aes(x = year, y = adjusted_median_income, color = race_explicit)) +
  scale_y_continuous(labels=scales::dollar_format(prefix="$")) +
  geom_line(size = 2, alpha = 0.8) +
  labs(x = "Year", y = "Income", color = "Race",
       title = "Median household income by race",
       subtitle = "Portland region, 1990 to 2017, adjusted for inflation",
       caption = "Source: Decennial Census 1990, 2000; ACS 2006-10 and 2013-17. Adjusted for inflation to 2018$ using CPI-U-RS")
                 

