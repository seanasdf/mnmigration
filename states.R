library(tidyverse)
library(srvyr)

######################################
####### Make a list of all ###########
#######states moved to or from MN ####
######################################

#read in inmigration data
if (file.exists("./caches/inmigration.rda")) {
  load("./caches/inmigration.rda")
} else {
  source("clean.R")
}

#read in outmigration data
if (file.exists("./caches/outmigration.rda")) {
  load("./caches/outmigration.rda")
} else {
  source("clean.R")
}


#make a list of all of the states people moved from
statelist_in <- unique(inmigration$MIGPLAC1)
statelist_in <- statelist_in[statelist_in<100]

statelist_out <- unique(outmigration$STATEFIP)
statelist_out <- statelist_out[statelist_out<100]

statelist <- unique(c(statelist_in, statelist_out))

remove(statelist_in,statelist_out)

######################################
######### Migration to MN   ##########
######### From Other States ##########
######################################


#Load cached inmigration analysis (if it exists) or run the analysis
if (file.exists("./caches/inmig_bystate.rda")) {
  load("./caches/inmig_bystate.rda")
} else {
  # Get count of how many people moved to MN from another state
  state_inmigration <- inmigration %>%
    rename(repwtflag = REPWTP) %>% 
    #create dummy variable to identify people who moved
    mutate(moved_states = ifelse(!(MIGPLAC1 %in% c(0,27)) & MIGPLAC1<100, 1, 0)) %>%
    as_survey_rep(type="BRR", repweights=starts_with("REPWTP"), weights=PERWT) %>%
    group_by(geogroup, agegroup, moved_states)
  
  #For each state in the analysis, 
  state_inmigration_analysis <- map_df(statelist, function(st) {
    summarise(state_inmigration,
              pct = survey_mean(MIGPLAC1 == st,
                                vartype = "ci", 
                                level=.9)) %>% 
      mutate(state = st)
  })  %>% 
    filter(moved_states==1 & state>0 & state !=27) %>% 
    select(-moved_states)
  
  save(state_inmigration_analysis, file="./caches/inmig_bystate.rda")
}

######################################
######### Migration From MN ##########
######### To Other States ############
######################################

# get pct of those who moved away going to each state.
state_outmigration <- outmigration %>% 
  mutate(STATEFIP = as.factor(STATEFIP)) %>% 
  rename(repwtflag = REPWTP) %>% 
  as_survey_rep(type="BRR", repweights=starts_with("REPWTP"), weights=PERWT)  %>%
  group_by(geogroup, agegroup)
  
state_outmigration_analysis <-  map_df(statelist, function(st) {
  summarise(state_outmigration,
            pct = survey_mean(STATEFIP == st,
                              vartype = "ci", 
                              level=.9)) %>% 
    mutate(state = st)
}) %>% 
  filter(state>0 & state !=27)

######################################
######### Merge in and Out ###########
#########    Migration     ###########
######################################


#get top 10 inmigration states for each subgroup
top_states_in <- state_inmigration_analysis %>% 
  arrange(geogroup, agegroup, -pct) %>% 
  group_by(agegroup, geogroup) %>% 
  mutate(rank = row_number()) %>% 
  filter(rank < 11) %>% 
  mutate(direction="Moved to MN")

#get top 10 outmigration states for each subgroup
top_states_out <- state_outmigration_analysis %>% 
  arrange(geogroup, agegroup, -pct) %>% 
  group_by(agegroup, geogroup) %>% 
  mutate(rank = row_number()) %>% 
  filter(rank < 11) %>% 
  mutate(direction="Moved from MN")


#read in FIPS codes for the states
library(readxl)
statenames <- read_excel("migrationplaces.xlsx")


#merge together in and out migration
top_states <- rbind(top_states_in, top_states_out) %>% 
  left_join(statenames) %>%
  ungroup() %>% 
  mutate(geogroup = ifelse(geogroup=="Metro", "Other Metro Counties", geogroup),
         geogroup = ifelse(geogroup=="Greater MN", "Greater Minnesota", geogroup),
         geogroup = factor(geogroup, 
                           levels=c("Ramsey","Hennepin","Other Metro Counties","Greater Minnesota")))

save(top_states, file="statetop10.rda")
