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

# for people who moved, get student status
student_inmigration <- inmigration %>%
  #create dummy variable to identify people who moved
  mutate(moved_states = ifelse(!(MIGPLAC1 %in% c(0,27)) & MIGPLAC1<100, 1, 0),
         student = case_when(SCHOOL == 1 ~ "Not a Student",
                             SCHOOL == 2 ~ "Student",
                             SCHOOL %in% c(0, 9) ~ "Missing/NA"),
         student = as.factor(student)) %>%
  #rename replicate weights flag so it doesn't get used as a replicate weight
  rename(repwtflag = REPWTP) %>% 
  as_survey_rep(type="BRR", repweights=starts_with("REPWTP"), weights=PERWT) %>%
  group_by(geogroup, agegroup, moved_states) %>% 
  summarise(pct_student = survey_mean(student=="Student", proportion = TRUE, vartype = "ci", level=.9),
            missing_student = survey_mean(student=="Missing/NA")) %>%
  filter(moved_states ==1) %>%
  select(-moved_states)

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
student_outmigration <- outmigration %>% 
  mutate(student = case_when(SCHOOL == 1 ~ "Not a Student",
                             SCHOOL == 2 ~ "Student",
                             SCHOOL %in% c(0, 9) ~ "Missing/NA"),
         student = as.factor(student)) %>% 
  #rename replicate weights flag so it doesn't get used as a replicate weight
  rename(repwtflag = REPWTP) %>% 
  as_survey_rep(type="BRR", repweights=starts_with("REPWTP"), weights=PERWT)  %>%
  group_by(geogroup, agegroup) %>%
  summarise(pct_student = survey_mean(student=="Student", proportion = TRUE, vartype = "ci", level=.9),
            missing_student = survey_mean(student=="Missing/NA"))

######################################
######### Merge in and Out ###########
#########    Migration     ###########
######################################
library(stringr)

#merge inmigration and outmigration, keep relevant columns, make long for graphing
student_migration <- left_join(student_inmigration, student_outmigration, 
                               by=c("geogroup", "agegroup"),
                               suffix=c("_in", "_out")) %>% 
  mutate(geogroup = ifelse(geogroup=="Greater MN", "Greater Minnesota", geogroup),
         geogroup = ifelse(geogroup=="Metro", "Other Metro Counties", geogroup)) %>% 
  filter(agegroup %in% c("18 to 21", "22 to 29")) %>%
  select(-missing_student_in, -missing_student_se_in, -missing_student_out, -missing_student_se_out) %>%
  gather(key=variable, value=value, pct_student_in, pct_student_out) %>%
  mutate(direction=ifelse(grepl("_in", variable), "Inmigration", "Outmigration"),
         upper_error = ifelse(direction == "Inmigration", pct_student_upp_in, pct_student_upp_out),
         lower_error = ifelse(direction == "Inmigration", pct_student_low_in, pct_student_low_out)) %>%
  select(-variable,-pct_student_low_in, -pct_student_upp_in, -pct_student_low_out, -pct_student_upp_out) %>% 
  mutate(geogroup = factor(geogroup, levels=c("Ramsey","Hennepin","Other Metro Counties","Greater Minnesota")))


save(student_migration, file="./caches/students.rda")

