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

# Get count of how many people moved to MN from another state
state_inmigration <- inmigration %>%
  #create dummy variable to identify people who moved
  mutate(moved_states = ifelse(!(MIGPLAC1 %in% c(0,27)) & MIGPLAC1<100, 1, 0),
         MIGPLAC1=as.factor(MIGPLAC1)) %>%
  as_survey_rep(type="BRR", repweights=starts_with("REPWTP"), weights=PERWT) %>%
  group_by(geogroup, agegroup, moved_states, MIGPLAC1) %>%
  summarise(pct=survey_mean(vartype = "ci", level=.9)) %>%
  filter(as.numeric(MIGPLAC1) < 100 & (moved_states==1))

# #Try using Survey
# library(survey)
# # Get count of how many people moved to MN from another state
# state_inmigration <- inmigration %>%
#   #create dummy variable to identify people who moved
#   mutate(moved_states = ifelse(!(MIGPLAC1 %in% c(0,27)) & MIGPLAC1<100, 1, 0),
#          MIGPLAC1=as.factor(MIGPLAC1)) %>%
#   as_survey_rep(type="BRR", repweights=starts_with("REPWTP"), weights=PERWT)
# 
# state_inmigration_analysis <- svyby(~MIGPLAC1
#                                     ~geogroup, 
#                                     state_inmigration,
#                                     svytable)

######################################
######### Migration From MN ##########
######### To Other States ############
######################################
if (file.exists("./caches/outmigration.rda")) {
  load("./caches/outmigration.rda")
} else {
  source("clean.R")
}

# for people who moved away from Minnesota, get number who are students
state_outmigration <- outmigration %>% 
  mutate(STATEFIP = as.factor(STATEFIP)) %>% 
  as_survey_rep(type="BRR", repweights=starts_with("REPWTP"), weights=PERWT)  %>%
  group_by(geogroup, agegroup, STATEFIP) %>%
  summarise(moved_from_mn = survey_total(vartype = "ci", level=.9)) %>% 
  mutate(STATEFIP = as.numeric(as.character(STATEFIP))) 

######################################
######### Merge in and Out ###########
#########    Migration     ###########
######################################

library(readxl)
migrationplaces <- read_excel("migrationplaces.xlsx")


#merge inmigration and outmigration, keep relevant columns, make long for graphing
state_migration <- left_join(state_inmigration, state_outmigration, 
                               by=c("geogroup",
                                    "agegroup",
                                    "MIGPLAC1" = "STATEFIP")) %>% 
  left_join(migrationplaces) %>% 
  filter(!MIGPLAC1 %in% c(0,27)) %>%
  mutate(totmig = moved_to_mn + moved_from_mn) 

most_migration <- filter(state_migration, geogroup=="Ramsey") %>% 
  group_by(agegroup, Place) %>%
  summarise(totmig = sum(totmig)) %>% 
  arrange(-totmig) %>% 
  group_by(agegroup) %>% 
  mutate(rank = min_rank(-totmig)) %>% 
  arrange(agegroup, rank) %>% 
  select(agegroup, Place, rank)


state_migration <- gather(state_migration,
                          variable,value,
                          moved_to_mn, moved_from_mn) %>%
  mutate(direction = ifelse(variable == "moved_to_mn", "Inmigration", "Outmigration"),
         err_low = ifelse(direction == "Inmigration", moved_to_mn_low, moved_from_mn_low),
         err_upp = ifelse(direction == "Inmigration", moved_to_mn_upp, moved_from_mn_upp)) %>% 
  select(geogroup, agegroup, Place, variable, value, direction, err_low, err_upp) %>% 
  left_join(most_migration, by=c("agegroup", "Place")) %>% 
  filter(rank<15)

######################################
######### Graph top states ###########
######################################
library(ggplot2)
states_18_outmigration <- filter(state_migration, 
                                 agegroup=="18 to 23") %>% 
  ggplot(aes(x=Place,
             y=value,
             fill=direction)) 

  geom_bar(stat="identity",position="dodge") +
  coord_flip() +
  facet_wrap(~geogroup, ncol=2)

states_18_outmigration
+
  scale_fill_brewer(labels=c("Left for Another State", 
                             "Moved to Minnesota From Another State"), 
                    palette="Set1",
                    guide=guide_legend(title="Direction of Migration")) +
  scale_y_continuous(labels=scales::percent) +
  geom_errorbar(aes(ymin=lower_error, ymax=upper_error), 
                width = .2,
                position=position_dodge(.9)) +
  labs(title = "Share of 18-23 Year-olds Moving to and From Minnesota that Were Students",
       y="Percent of 18-23 Year-olds who Moved that Were Students",
       x="",
       caption = "Source: MN House Research. Error bars represent 90% confidence intervals. 
                   2015 American Community Survey 5-year Estimates. IPUMS-USA, University of Minnesota.")

ggsave("./plots/students_18.png", student_18,width=8,height=6) 
