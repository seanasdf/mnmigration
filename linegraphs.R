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


#get inmigration by geographic group and age 
inmigration_by_age <- inmigration %>%
  #creat dummy variable to identify people who moved
  mutate(moved_states = ifelse(!(MIGPLAC1 %in% c(0,27)) & MIGPLAC1<100, 1, 0),
         AGE = as.character(AGE)) %>%
  #rename replicate weights flag so it doesn't get used as a replicate weight
  rename(repwtflag = REPWTP) %>% 
  as_survey_rep(type="BRR", repweights=starts_with("REPWTP"), weights=PERWT) %>%
  group_by(geogroup, AGE) %>%
  summarise(pct_moved_in = survey_mean(moved_states))


######################################
######### Migration from MN ##########
######### to Other States ############
######################################

#Get total population for each region
population_by_region <- inmigration %>%
  mutate(AGE = as.character(AGE)) %>% 
  #rename replicate weights flag so it doesn't get used as a replicate weight
  rename(repwtflag = REPWTP) %>% 
  as_survey_rep(type="BRR", repweights=starts_with("REPWTP"), weights=PERWT) %>% 
  group_by(geogroup, AGE) %>% 
  summarise(population = survey_total())

if (file.exists("./caches/outmigration.rda")) {
  load("./caches/outmigration.rda")
} else {
  source("clean.R")
}

outmigration_by_age <- outmigration %>%
  mutate(AGE = as.factor(AGE)) %>% 
  #rename replicate weights flag so it doesn't get used as a replicate weight
  rename(repwtflag = REPWTP) %>% 
  as_survey_rep(type="BRR", repweights=starts_with("REPWTP"), weights=PERWT)  %>%
  group_by(geogroup, AGE) %>%
  summarise(moved_out = survey_total()) %>% 
  left_join(population_by_region) %>%
  mutate(pct_moved_out = moved_out/population, 
         pct_moved_out_se = sqrt(moved_out_se^2 + ((pct_moved_out/population)^2)*population_se^2)/(population)
  ) %>% 
  select(-population, -population_se, -moved_out, -moved_out_se)

######################################
##### Merge In and Outmigration ######
######################################
regional_inout <- left_join(inmigration_by_age, outmigration_by_age) %>%
  mutate(geogroup = ifelse(geogroup=="Greater MN", "Greater Minnesota", geogroup),
         geogroup = ifelse(geogroup=="Metro", "Other Metro Counties", geogroup)) %>%
  gather(direction, mig, pct_moved_in, pct_moved_out) %>%
  mutate(se = ifelse(direction=="pct_moved_in", pct_moved_in_se, pct_moved_out_se)) %>%
  select(-pct_moved_in_se, -pct_moved_out_se) %>%
  mutate(geogroup = factor(geogroup, levels=c("Ramsey","Hennepin","Other Metro Counties","Greater Minnesota")),
         direction = factor(direction, levels = c("pct_moved_out", "pct_moved_in"))) 

save(regional_inout,file="./caches/regionalinout.rda")
