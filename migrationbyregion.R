library(tidyverse)
library(srvyr)

######################################
######### Migration to MN   ##########
######### From Other States ##########
######################################

#read in inmigration data
if (file.exists("./caches/inmigration.rda")) {
  load("./caches/inmigration.rda")
} else {
  source("clean.R")
}

#get inmigration by geographic and age group
inmigration_by_group <- inmigration %>%
  #create dummy variable to identify people who moved
  mutate(moved_states = ifelse(!(MIGPLAC1 %in% c(0,27)) & MIGPLAC1<100, 1, 0)) %>%
  #rename replicate weights flag so it doesn't get used as a replicate weight
  rename(repwtflag = REPWTP) %>% 
  as_survey_rep(type="BRR", repweights=starts_with("REPWTP"), weights=PERWT) %>%
  group_by(geogroup, agegroup) %>%
  summarise(pct_moved_in = survey_mean(moved_states), moved_in = survey_total(moved_states)) 


######################################
######### Migration from MN ##########
######################################

#Get total population for each region
population_by_region <- inmigration %>%
  #rename replicate weights flag so it doesn't get used as a replicate weight
  rename(repwtflag = REPWTP) %>% 
  as_survey_rep(type="BRR", repweights=starts_with("REPWTP"), weights=PERWT) %>% 
  group_by(geogroup, agegroup) %>% 
  summarise(population = survey_total())

if (file.exists("./caches/outmigration.rda")) {
  load("./caches/outmigration.rda")
} else {
  source("clean.R")
}

outmigration_by_group <- outmigration %>%
  #rename replicate weights flag so it doesn't get used as a replicate weight
  rename(repwtflag = REPWTP) %>% 
  as_survey_rep(type="BRR", repweights=starts_with("REPWTP"), weights=PERWT)  %>%
  group_by(geogroup, agegroup) %>%
  summarise(moved_out = survey_total()) %>% 
  left_join(population_by_region) %>%
  mutate(pct_moved_out = moved_out/population, 
         pct_moved_out_se = sqrt(moved_out_se^2 + ((pct_moved_out/population)^2)*population_se^2)/(population)
         ) %>% 
  select(-population, -population_se)



######################################
###### Write Totals out To CSV #######
######################################

netmig <- left_join(inmigration_by_group, outmigration_by_group) %>%
  mutate(net_migration = moved_in-moved_out )
write.csv(netmig, "netmigration.csv")

pct_inout_byregion <- left_join(inmigration_by_group, outmigration_by_group) %>%
  mutate(geogroup = ifelse(geogroup=="Greater MN", "Greater Minnesota", geogroup),
         geogroup = ifelse(geogroup=="Metro", "Other Metro Counties", geogroup)) %>%
  select(-moved_in, -moved_in_se, -moved_out, -moved_out_se) %>% 
  gather(direction, mig, pct_moved_in, pct_moved_out) %>%
  mutate(se = ifelse(direction=="pct_moved_in", pct_moved_in_se, pct_moved_out_se)) %>%
  select(-pct_moved_in_se, -pct_moved_out_se) %>%
  mutate(geogroup = factor(geogroup, levels=c("Ramsey","Hennepin","Other Metro Counties","Greater Minnesota")),
         direction = factor(direction, levels=c("pct_moved_out", "pct_moved_in")),
         mig = mig *1000,
         se = se * 1000)


total_inout_byregion <- left_join(inmigration_by_group, outmigration_by_group) %>%
  mutate(geogroup = ifelse(geogroup=="Greater MN", "Greater Minnesota", geogroup),
         geogroup = ifelse(geogroup=="Metro", "Other Metro Counties", geogroup)) %>%
  select(-pct_moved_in, -pct_moved_in_se, -pct_moved_out, -pct_moved_out_se) %>% 
  gather(direction, mig, moved_in, moved_out) %>%
  mutate(se = ifelse(direction=="moved_in", moved_in_se, moved_out_se)) %>%
  select(-moved_in_se, -moved_out_se) %>%
  mutate(geogroup = factor(geogroup, levels=c("Ramsey","Hennepin","Other Metro Counties","Greater Minnesota")),
         direction = factor(direction, levels=c("moved_out", "moved_in")))



save(pct_inout_byregion,
     total_inout_byregion,
     file = "./caches/inout_byregion.rda")

