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


#get inmigration by geographic and age 
inmigration_by_age <- inmigration %>%
  #creat dummy variable to identify people who moved
  mutate(moved_states = ifelse(!(MIGPLAC1 %in% c(0,27)) & MIGPLAC1<100, 1, 0),
         AGE = as.factor(AGE)) %>%
  #rename replicate weights flag so it doesn't get used as a replicate weight
  rename(repwtflag = REPWTP) %>% 
  as_survey_rep(type="BRR", repweights=starts_with("REPWTP"), weights=PERWT) %>%
  group_by(AGE) %>%
  summarise(pct_moved_in = survey_mean(moved_states))


######################################
######### Migration from MN ##########
######### to Other States ############
######################################
#Get total population for each region
population_by_age <- inmigration %>%
  mutate(AGE = as.character(AGE),
         one = 1) %>% 
  #rename replicate weights flag so it doesn't get used as a replicate weight
  rename(repwtflag = REPWTP) %>% 
  as_survey_rep(type="BRR", repweights=starts_with("REPWTP"), weights=PERWT) %>% 
  group_by(AGE) %>% 
  #this is throwing an error when I do it with an empty call to survey_total, but this works
  summarise(population = survey_total(one))


if (file.exists("./caches/outmigration.rda")) {
  load("./caches/outmigration.rda")
} else {
  source("clean.R")
}

outmigration_by_age <- outmigration %>%
  mutate(AGE = as.factor(AGE),
         one = 1) %>% 
  #rename replicate weights flag so it doesn't get used as a replicate weight
  rename(repwtflag = REPWTP) %>% 
  as_survey_rep(type="BRR", repweights=starts_with("REPWTP"), weights=PERWT)  %>%
  group_by(AGE) %>%
  summarise(moved_out = survey_total(one)) %>% 
  left_join(population_by_age) %>%
  mutate(pct_moved_out = moved_out/population, 
         pct_moved_out_se = sqrt(moved_out_se^2 + ((pct_moved_out/population)^2)*population_se^2)/(population)
  ) %>% 
  select(-population, -population_se, -moved_out, -moved_out_se)

######################################
##### Merge In and Outmigration ######
######################################
state_inout_byage <- left_join(inmigration_by_age, outmigration_by_age) %>%
  gather(direction, mig, pct_moved_in, pct_moved_out) %>%
  mutate(se = ifelse(direction=="pct_moved_in", pct_moved_in_se, pct_moved_out_se)) %>%
  select(-pct_moved_in_se, -pct_moved_out_se) %>%
  mutate(direction = factor(direction, levels = c("pct_moved_out", "pct_moved_in")), 
         mig = mig * 1000, 
         se= se *1000) 

save(state_inout_byage, file="./caches/stateinout.rda")
