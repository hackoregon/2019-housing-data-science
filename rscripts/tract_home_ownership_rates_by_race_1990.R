# this scripts takes raw tract-level-race-by-tenure data and aggregates it in various ways. 
# at the end it creates the multnomah_home_ownership_by_race and portland_home_ownership_by_race 
# tables used in the cards. 

source("~/postgis_connect.R")
con_pg <- make_postgis_con(mydb = 'housing-2019-staging', myuser = 'housing2019', host = 'housing-2019-staging.caicgny9d8nv.us-west-2.rds.amazonaws.com')

dd.tenure_by_race <- fetch(dbSendQuery(con_pg, 
#  'select tract_fips, total_hh_white, share_own_white, total_hh_black, share_own_black from race_by_tenure_1990t2010 where "DATAYEAR" = 1990 AND total_hh_black > 100'
  'select "DATAYEAR" yr, tract_fips, total_hh_white, share_own_white, total_hh_black, share_own_black from race_by_tenure_1990t2017
    WHERE tract_fips IN(SELECT tract_fips FROM race_by_tenure_1990t2017 where "DATAYEAR" = 1990 AND total_hh_black > 100)'
), n=-1)

dd.tenure_by_race.sp <- merge(oregon_tracts, dd.tenure_by_race) %>% subset(!is.na(total_hh_white))

dd.tenure_by_race %>% 
  group_by(yr) %>%
  summarize(          share_own_white = sum( total_hh_white * share_own_white) / sum(total_hh_white),
                      share_own_black = sum( total_hh_black * share_own_black) / sum(total_hh_black),
                      total_hh_white = sum(total_hh_white),
          total_hh_black = sum(total_hh_black)
  )

ownership_by_race <- dbGetQuery(con_pg, paste0('
     select "DATAYEAR" yr, 
     sum(total_hh_white) white_households, sum(total_own_white) white_ownership_count, sum(total_own_white)/sum(total_hh_white) white_rate, 
     sum(total_hh_black) black_households, sum(total_own_black) black_ownership_count, sum(total_own_black)/sum(total_hh_black) black_rate,
     sum(total_hh_aian) asian_households, sum(total_own_aian) asian_ownership_count, sum(total_own_aian)/sum(total_hh_aian) asian_rate, 
     sum(total_hh_hisp) hisp_households, sum(total_own_hisp) hisp_ownership_count, sum(total_own_hisp)/sum(total_hh_hisp) hisp_rate
     from race_by_tenure_1990t2017 
     WHERE "DATAYEAR"!=2011 
     AND left(tract_fips,5) = ',"'41051'",'
     GROUP BY 1 ORDER BY 1;'))
ownership_by_race %>% 
  dplyr::select(yr, ends_with('_rate')) %>% 
  tidyr::gather(race, home_ownership_rate,-yr ) %>%
  mutate(race = sub('_rate','',race)) %>% 
  ggplot(aes(x=yr, y=home_ownership_rate, color = race)) +
    geom_line() + 
    geom_point() +
    scale_y_continuous(limits = c(0,0.8), labels = function(x) {paste0(x*100, '%')}) + 
    theme(axis.text.y = element_text(angle = 90, hjust = 0.5))+
    labs(title="Home ownership by race in Mult Co 1990 to 2017", x="Census Year", y="Home Ownership Race") 
ggsave("home_ownership_by_race.png")

ownership_by_race %>% 
  dplyr::select(yr, ends_with('_rate')) %>% 
  tidyr::gather(race, home_ownership_rate,-yr ) %>%
  mutate(race = sub('_rate','',race)) %>% 
  filter(race %in% c('black','white'), yr == 1990) %>% 
  ggplot(aes(y=home_ownership_rate, x = race, fill = race)) +
  geom_col(width = 0.5) + 
  scale_y_continuous(limits = c(0,0.8), labels = function(x) {paste0(x*100, '%')}) + 
  theme(axis.text.y = element_text(angle = 90, hjust = 0.5), legend.position = "none") +
  labs(title="Home ownership by race in Multnomah County - 1990", x="", y="Home Ownership Rate") 
ggsave("home_ownership_by_race_1990.png")



ownership_by_race %>% 
  dplyr::select(yr, ends_with('_households')) %>% 
  tidyr::gather(race, number_of_households,-yr ) %>% 
  mutate(race = sub('_households','',race)) %>% 
  ggplot(aes(x=yr, y=number_of_households, color = race)) +
  geom_line() + 
  geom_point() +
  scale_y_continuous(labels = function(x) {paste0(x/1000, 'K')}) + 
  theme(axis.text.y = element_text(angle = 90, hjust = 0.5))+
  labs(title="Number of households by race 1990 to 2017", x="Census Year", y="Number of Households") + 
  facet_wrap(~race, scales = 'free_y') 
  
  


with(ownership_by_race, plot(yr, white_rate, ylim = c(0,1), type = 'b'))
with(ownership_by_race, points(yr, black_rate, ylim = c(0,1), type='b'))



multnomah_home_ownership_by_race <- 
  (ownership_by_race %>% 
                                       dplyr::select(yr, ends_with('_rate')) %>% 
                                       tidyr::gather(race, home_ownership_rate,-yr ) %>%
                                       mutate(race = sub('_rate','',race)) 
) %>% 
  inner_join(ownership_by_race %>% 
               dplyr::select(yr, ends_with('_count')) %>% 
               tidyr::gather(race, home_ownership_count,-yr ) %>%
               mutate(race = sub('_ownership_count','',race)) 
  ) %>% 
  inner_join(ownership_by_race %>% 
               dplyr::select(yr, ends_with('_households')) %>%
               tidyr::gather(race, household_count, -yr ) %>%
               mutate(race = sub('_households','',race)) 
  ) %>% 
  cbind(geoname = 'MultnomahCounty') %>% 
  cbind(fips=41051)
dbWriteTable(con_pg, 'multnomah_home_ownership_by_race',multnomah_home_ownership_by_race, row.names=FALSE)


portland_tracts <-  c('41051001202', '41051001301', '41051001302', '41051001400', '41051001500', '41051001601', '41051001602', 
                                              '41051001701', '41051001702', '41051001801', '41051001802', '41051001900', '41051002000', '41051002100', 
                                              '41051002401', '41051002402', '41051002501', '41051002502', '41051002600', '41051002701', '41051002702', 
                                              '41051002801', '41051002802', '41051002901', '41051002902', '41051003000', '41051003100', '41051003200', 
                                              '41051003301', '41051003302', '41051003401', '41051003402', '41051003501', '41051003502', '41051003601', 
                                              '41051003602', '41051003603', '41051003701', '41051005100', '41051005200', '41051005500', '41051005600', 
                                              '41051005700', '41051005800', '41051005900', '41051007300', '41051003702', '41051003801', '41051003802', 
                                              '41051003803', '41051003901', '41051003902', '41051004001', '41051004002', '41051004101', '41051004102', 
                                              '41051004200', '41051980000', '41051004500', '41051004601', '41051004602', '41051004700', '41051004800', 
                                              '41051004900', '41051005000', '41051006300', '41051006402', '41051006801', '41051007400', '41051007500', 
                                              '41051007600', '41051007700', '41051007900', '41051008001', '41051008002', '41051008100', '41051008201', 
                                              '41051008202', '41051006802', '41051006900', '41051007000', '41051007201', '41051008600', '41051008902', 
                                              '41051008901', '41051009201', '41051008301', '41051008302', '41051008400', '41051008500', '41051008700', 
                                              '41051009000', '41051009102', '41051009202', '41051009302', '41051009803', '41051009400', '41051009501', 
                                              '41051009701', '41051009301', '41051006001', '41051006601', '41051006002', '41051006100', '41051006200', 
                                              '41051006501', '41051006502', '41051006602', '41051006701', '41051006702', '41051007202', '41051002903', 
                                              '41051007800', '41051008800', '41051009502', '41051004300', '41051000200', '41051000301', '41051010600', 
                                              '41051002303', '41051002203', '41051000302', '41051000401', '41051000402', '41051000501', '41051000502', 
                                              '41051000601', '41051000602', '41051000701', '41051000702', '41051000801', '41051000802', '41051000901', 
                                              '41051000902', '41051001000', '41051001101', '41051001102', '41051001201', '41051009101', '41051009804', 
                                              '41051009702', '41051006404', '41051006403', '41051000100', '41051002201', '41051002301', '41051002202', 
                                              '41051002302', '41051004400', '41051006401', '41051005300', '41051005400', '41051009300', '41051009802', 
                                              '41051009100', '41051008900', '41051009500')

pdx_ownership_by_race <- dbGetQuery(con_pg, paste0('
     select "DATAYEAR" yr, 
      sum(total_hh_white) white_households, sum(total_own_white) white_ownership_count, sum(total_own_white)/sum(total_hh_white) white_rate, 
      sum(total_hh_black) black_households, sum(total_own_black) black_ownership_count, sum(total_own_black)/sum(total_hh_black) black_rate,
      sum(total_hh_aian) asian_households, sum(total_own_aian) asian_ownership_count, sum(total_own_aian)/sum(total_hh_aian) asian_rate, 
      sum(total_hh_hisp) hisp_households, sum(total_own_hisp) hisp_ownership_count, sum(total_own_hisp)/sum(total_hh_hisp) hisp_rate
      from race_by_tenure_1990t2017 
      WHERE "DATAYEAR"!=2011 
      AND tract_fips IN (',paste0("'",portland_tracts,"'", collapse = ','),')
      GROUP BY 1 ORDER BY 1;'))


portland_home_ownership_by_race <- 
  (pdx_ownership_by_race %>% 
     dplyr::select(yr, ends_with('_rate')) %>% 
     tidyr::gather(race, home_ownership_rate,-yr ) %>%
     mutate(race = sub('_rate','',race)) 
  ) %>% 
  inner_join(pdx_ownership_by_race %>% 
               dplyr::select(yr, ends_with('_count')) %>% 
               tidyr::gather(race, home_ownership_count,-yr ) %>%
               mutate(race = sub('_ownership_count','',race)) 
  ) %>% 
  inner_join(pdx_ownership_by_race %>% 
               dplyr::select(yr, ends_with('_households')) %>%
               tidyr::gather(race, household_count, -yr ) %>%
               mutate(race = sub('_households','',race)) 
  ) %>% 
  cbind(geoname = 'Portland') %>% 
  cbind(fips=NA)
dbWriteTable(con_pg, 'portland_home_ownership_by_race',portland_home_ownership_by_race, row.names=FALSE)
