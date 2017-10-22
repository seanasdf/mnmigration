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
inmigration_regions <- inmigration %>%
  #creat dummy variable to identify people who moved
  mutate(moved_states = ifelse(!(MIGPLAC1 %in% c(0,27)) & MIGPLAC1<100, 1, 0),
         AGE = as.factor(AGE)) %>%
  #rename replicate weights flag so it doesn't get used as a replicate weight
  rename(repwtflag = REPWTP) %>% 
  as_survey_rep(type="BRR", repweights=starts_with("REPWTP"), weights=PERWT) %>%
  group_by(agegroup, geogroup) %>%
  summarise(moved_in = survey_total(moved_states))

#get inmigration by geographic and age 
inmigration_state <- inmigration %>%
  #creat dummy variable to identify people who moved
  mutate(moved_states = ifelse(!(MIGPLAC1 %in% c(0,27)) & MIGPLAC1<100, 1, 0),
         AGE = as.factor(AGE)) %>%
  #rename replicate weights flag so it doesn't get used as a replicate weight
  rename(repwtflag = REPWTP) %>% 
  as_survey_rep(type="BRR", repweights=starts_with("REPWTP"), weights=PERWT) %>%
  group_by(agegroup) %>%
  summarise(moved_in = survey_total(moved_states))


inmigration_age <- inmigration_state %>% 
  mutate(geogroup="Statewide") %>% 
  rbind(inmigration_regions)


######################################
######### Migration from MN ##########
######### to Other States ############
######################################
if (file.exists("./caches/outmigration.rda")) {
  load("./caches/outmigration.rda")
} else {
  source("clean.R")
}

outmigration_regions <- outmigration %>%
  mutate(AGE = as.factor(AGE),
         one = 1) %>% 
  #rename replicate weights flag so it doesn't get used as a replicate weight
  rename(repwtflag = REPWTP) %>% 
  as_survey_rep(type="BRR", repweights=starts_with("REPWTP"), weights=PERWT)  %>%
  group_by(agegroup, geogroup) %>%
  summarise(moved_out = survey_total(one)) 

outmigration_state <- outmigration %>%   
  mutate(AGE = as.factor(AGE), one = 1) %>% 
  #rename replicate weights flag so it doesn't get used as a replicate weight
  rename(repwtflag = REPWTP) %>% 
  as_survey_rep(type="BRR", repweights=starts_with("REPWTP"), weights=PERWT)  %>%
  group_by(agegroup) %>%
  summarise(moved_out = survey_total(one)) 


outmigration_age <- outmigration_state %>% 
  mutate(geogroup="Statewide") %>% 
  rbind(outmigration_regions)



######################################
##### Merge In and Outmigration ######
######################################
netmigration_age <- left_join(inmigration_age, outmigration_age) %>%
  mutate(netmig = moved_in-moved_out,
         se = sqrt(moved_in_se^2 + moved_out_se^2),
         geogroup = case_when(geogroup=="Greater MN" ~ "Greater Minnesota",
                              geogroup=="Metro" ~ "Other Metro Counties",
                              TRUE ~ geogroup),
         geogroup = factor(geogroup, levels=c("Ramsey","Hennepin","Other Metro Counties","Greater Minnesota","Statewide"))) %>% 
  select(agegroup, geogroup, netmig, se) %>% 
  filter(agegroup %in% c("18 to 21", "22 to 29"))


save(netmigration_age, file="./caches/netmig_agegroup.rda")


######################################
######### Inmigration, Total #########
######################################
#read in inmigration data
if (file.exists("./caches/inmigration.rda")) {
  load("./caches/inmigration.rda")
} else {
  source("clean.R")
}

#get inmigration by geographic and age 
inmigration_total <- inmigration %>%
  #creat dummy variable to identify people who moved
  mutate(moved_states = ifelse(!(MIGPLAC1 %in% c(0,27)) & MIGPLAC1<100, 1, 0),
  AGE = as.factor(AGE)) %>%
  #rename replicate weights flag so it doesn't get used as a replicate weight
  rename(repwtflag = REPWTP) %>% 
  as_survey_rep(type="BRR", repweights=starts_with("REPWTP"), weights=PERWT) %>%
  summarise(moved_in = survey_total(moved_states))


######################################
######### Outmigration, Total ########
######################################
if (file.exists("./caches/outmigration.rda")) {
  load("./caches/outmigration.rda")
} else {
  source("clean.R")
}


outmigration_total <- outmigration %>%   
  mutate(AGE = as.factor(AGE), one = 1) %>% 
  #rename replicate weights flag so it doesn't get used as a replicate weight
  rename(repwtflag = REPWTP) %>% 
  as_survey_rep(type="BRR", repweights=starts_with("REPWTP"), weights=PERWT)  %>%
  summarise(moved_out = survey_total(one)) 

