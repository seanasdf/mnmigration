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
  summarise(moved_in = survey_total(moved_states))


######################################
######### Migration from MN ##########
######### to Other States ############
######################################
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
  summarise(moved_out = survey_total(one)) 

######################################
##### Merge In and Outmigration ######
######################################
netmig_mn <- left_join(inmigration_by_age, outmigration_by_age) %>%
  filter(AGE>0) %>% 
  mutate(netmig = moved_in-moved_out,
         se = sqrt(moved_in_se^2 + moved_out_se^2)) %>% 
  select(AGE, netmig, se)

save(netmig_mn,file= "./caches/netmig_mn.rda")
