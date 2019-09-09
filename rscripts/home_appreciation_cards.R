# This script does some querying and vizualization of different aspects of home appreciation based on 
# home sales in the taxlot data. 
# the end of the script generates and uploads the portland_home_appreciation_annually_since_1990ish table 
# which is used by story-card #9. 

require(tidyverse)
require(leaflet)
source("postgis_connect.R")
con_pg <- make_postgis_con(mydb = 'housing-2019-staging', myuser = 'housing2019', host = 'housing-2019-staging.caicgny9d8nv.us-west-2.rds.amazonaws.com')

appreciation_dd_full <- 
  fetch(dbSendQuery(con_pg,
                    "SELECT sale_year, prev_sale_year, sale_year - prev_sale_year appreciation_yrs, 
                    avg(100*((sale_price/prev_sale_price::float) - 1)) appreciation_pct_mean, 
                    100*((SUM(sale_price)/SUM(prev_sale_price)::float) - 1) appreciation_pct_weighted_avg, 
                    percentile_cont(0.25) WITHIN GROUP (ORDER BY 100*((sale_price/prev_sale_price::float) - 1)) appreciation_pct_25th, 
                    percentile_cont(0.75) WITHIN GROUP (ORDER BY 100*((sale_price/prev_sale_price::float) - 1)) appreciation_pct_75th,
                    percentile_cont(0.5) WITHIN GROUP (ORDER BY 100*((sale_price/prev_sale_price::float) - 1)) appreciation_pct_med,
                    count(*) 
                    FROM taxlots.home_sales_prev_sale 
                    WHERE abs((bldgsqft/prev_bldgsqft::float)-1) < 0.1 
                    AND prev_bldgsqft >0 AND prev_sale_price > 0  
                    AND sale_year >= 1997
                    GROUP BY 1,2,3"), n = -1)


appreciation_since_1990ish <- 
  fetch(dbSendQuery(con_pg,
                    "SELECT sale_year, 
                    avg(100*((sale_price/prev_sale_price::float) - 1)) appreciation_pct_mean, 
                    100*((SUM(sale_price)/SUM(prev_sale_price)::float) - 1) appreciation_pct_weighted_avg, 
                    percentile_cont(0.25) WITHIN GROUP (ORDER BY 100*((sale_price/prev_sale_price::float) - 1)) appreciation_pct_25th, 
                    percentile_cont(0.75) WITHIN GROUP (ORDER BY 100*((sale_price/prev_sale_price::float) - 1)) appreciation_pct_75th,
                    percentile_cont(0.5) WITHIN GROUP (ORDER BY 100*((sale_price/prev_sale_price::float) - 1)) appreciation_pct_med,
                    count(*) 
                    FROM taxlots.home_sales_prev_sale 
                    WHERE abs((bldgsqft/prev_bldgsqft::float)-1) < 0.1 
                    AND prev_bldgsqft >0 AND prev_sale_price > 0  
                    AND sale_year >= 1997 AND prev_sale_year BETWEEN 1987 AND 1993
                    GROUP BY 1 ORDER BY 1"), n = -1)

appreciation_since_1990ish %>% 
  ggplot(aes(x = sale_year)) + 
  ylim(0,max(appreciation_since_1990ish$appreciation_pct_75th)) + 
  geom_line(aes(y = appreciation_pct_med), color = 'blue')+
  geom_line(aes(y = appreciation_pct_75th), color = 'blue', linetype = 'dashed')+
  geom_line(aes(y = appreciation_pct_25th), color = 'blue', linetype = 'dashed' )+
  labs(title="Appreciation for houses last sold between 1987 and 1993", x="Sale Year", y="Appreciation % Since Previous Sale",subtitle = 'Median Appreciation with 25th-75th Percentile Range') +
  scale_y_continuous(labels = function(x) {paste0(x, '%')}) + 
  theme(axis.text.y = element_text(angle = 90, hjust = 0.5))

appreciation_dollars_since_1990ish <- 
  fetch(dbSendQuery(con_pg,
                    "SELECT '1987-1993' prev_sale_year, sale_year, 
                    avg(sale_price - prev_sale_price) raw_appreciation_mean, 
                    percentile_cont(0.25) WITHIN GROUP (ORDER BY sale_price - prev_sale_price) raw_appreciation_25th, 
                    percentile_cont(0.75) WITHIN GROUP (ORDER BY sale_price - prev_sale_price) raw_appreciation_75th,
                    percentile_cont(0.5) WITHIN GROUP (ORDER BY sale_price - prev_sale_price) raw_appreciation_med,
                    avg(sale_price * sale_infl.inflation_factor_2018 - prev_sale_price * prev_sale_infl.inflation_factor_2018) adj_appreciation_mean, 
                    percentile_cont(0.25) WITHIN GROUP (ORDER BY sale_price * sale_infl.inflation_factor_2018 - prev_sale_price * prev_sale_infl.inflation_factor_2018) adj_appreciation_25th, 
                    percentile_cont(0.75) WITHIN GROUP (ORDER BY sale_price * sale_infl.inflation_factor_2018 - prev_sale_price * prev_sale_infl.inflation_factor_2018) adj_appreciation_75th,
                    percentile_cont(0.5) WITHIN GROUP (ORDER BY sale_price * sale_infl.inflation_factor_2018 - prev_sale_price * prev_sale_infl.inflation_factor_2018) adj_appreciation_med,
                    count(*) 
                    FROM taxlots.home_sales_prev_sale 
                    JOIN public.cpi_u_rs_1950 prev_sale_infl ON(prev_sale_infl.year = prev_sale_year)
                    JOIN public.cpi_u_rs_1950 sale_infl ON(sale_infl.year = sale_year)
                    WHERE abs((bldgsqft/prev_bldgsqft::float)-1) < 0.1 
                    AND prev_bldgsqft >0 AND prev_sale_price > 0  
                    AND sale_year >= 1997 AND prev_sale_year BETWEEN 1987 AND 1993
                    GROUP BY 1,2 ORDER BY 1,2"), n = -1)


appreciation_dollars_since_1990ish %>% 
  ggplot(aes(x = sale_year)) + 
  ylim(0,max(appreciation_dollars_since_1990ish$raw_appreciation_75th)) + 
  geom_line(aes(y = raw_appreciation_med), color = 'blue')+
  geom_line(aes(y = raw_appreciation_25th), color = 'blue', linetype = 'dashed')+
  geom_line(aes(y = raw_appreciation_75th), color = 'blue', linetype = 'dashed' )+
  labs(title="Per-house appreciation for houses last sold between 1987 and 1993", x="Sale Year", y="Appreciation Dollars Since Previous Sale",subtitle = "Median Appreciation $'s with 25th-75th Percentile Range") + 
  scale_y_continuous(labels = function(x) {paste0('$', x/1000, 'K')}) + 
  theme(axis.text.y = element_text(angle = 90, hjust = 0.5))


appreciation_dollars_since_1990ish %>%
  ggplot(aes(x = sale_year)) + 
  geom_line(aes(y = adj_appreciation_med), color = 'blue')+
  geom_line(aes(y = adj_appreciation_25th), color = 'blue', linetype = 'dashed')+
  geom_line(aes(y = adj_appreciation_75th), color = 'blue', linetype = 'dashed' )+
  labs(title="Per-house appreciation for houses last sold between 1987 and 1993", x="Sale Year", y="Appreciation (in 2018-Dollars) Since Previous Sale",subtitle = "Median Appreciation $'s - adjusted for inflation - with 25th-75th Percentile Range") + 
  scale_y_continuous(limits = c(0,max(appreciation_dollars_since_1990ish$adj_appreciation_75th)),
                      labels = function(x) {paste0('$', x/1000, 'K')}) + 
  theme(axis.text.y = element_text(angle = 90, hjust = 0.5))
ggsave()

portland_home_appreciation_annually_since_1990ish <- appreciation_dollars_since_1990ish 

dbWriteTable(con_pg, "portland_home_appreciation_annually_since_1990ish", portland_home_appreciation_annually_since_1990ish, row.names=FALSE)

# possible future extensions: 
# Toggle for types of sales. (i.e. all houses sold and resold without taxlot change - vs just those sold once at the start and once at the end). 
# Toggle for Geographical area (by census characteristic, or being in Albina, or who knows what else)
