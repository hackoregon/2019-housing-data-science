##### Load libraries, set working directory #####
if(!require(pacman)){install.packages("pacman");library(pacman)}
p_load(tidyverse, tigris, tidycensus, rio, viridis, sf, mapview, plotly, RColorBrewer)
options(tigris_use_cache = T); options(tigris_class = "sf")

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

source("../data/external/utility_functions.R")

pdx_msa_definition <- c('41005', '41009', '41051', '41067', '41071', '53011', '53059')

hmda_pdx_working_07t17 <- readRDS("../data/processed/hmda_pdx_working_2007t2017.RDS")
homeownership_by_race_2017_3 <- readRDS("../data/processed/homeownership_by_race_2017_HMDA_summary.RDS")
homeownership_by_race_2011_3 <- readRDS("../data/processed/homeownership_by_race_2011_HMDA_summary.RDS")

## TODO figure out how to make and map longitudinal dataset
## TODO reconcile tract boundary changes since 2007-2009 or 2011(?) HMDA uses older tract boundaries

# mapview(homeownership_by_race_2017_3 %>% select(total_hh:lq_hisp.brks), burst = TRUE, hide = TRUE)

pal <- colorRampPalette(brewer.pal(9, "RdYlBu"))

mapview(homeownership_by_race_2017_3 %>% select(lq_white.brks:lq_hisp.brks), 
        col.regions = pal(5), burst = TRUE, hide = TRUE)
# mapview(homeownership_by_race_2011_3 %>% select(lq_white.brks:lq_hisp.brks), col.regions = pal(5), burst = TRUE, hide = TRUE)


## Boxplot showing distribution of location quotients
ggplotly((homeownership_by_race_2017_3 %>%
            st_set_geometry(NULL) %>%
            select(GEOID, lq_white:lq_hisp) %>%
            gather(., c(lq_white:lq_hisp), key = "race", value = "lq") %>%
            ggplot(aes(x = race, y = lq)) +
            geom_boxplot()))

## These Gi* maps (share of loans that went to specific race groups) show the 
## changing geography of 
make_gimap(homeownership_by_race_2017_3, quo(loans_share_black), method = "d", distance = 2.4) # 2.4km = 1.5mi

