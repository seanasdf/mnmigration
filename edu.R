library(tidyverse)
library(srvyr)

######################################
######### Migration to MN   ##########
######### From Other States ##########
######################################

#cache the survey analysis by education, because it takes a while.
if (file.exists("./caches/educanalysis.rda")) {
  load("./caches/educanalysis.rda")
} else {
  
  if (file.exists("./caches/inmigration.rda")) {
    load("./caches/inmigration.rda")
  } 
  else {source("clean.R")}
  
  library(readxl)
  educ_groups <- read_excel("educ_cats.xlsx")
  
  # for people who moved, get educational attainment
  educ_inmigration <- inmigration %>%
    left_join(educ_groups) %>%
    mutate(moved_states = ifelse(!(MIGPLAC1 %in% c(0,27)) & MIGPLAC1<100, 1, 0)) %>% 
    #rename replicate weights flag so it doesn't get used as a replicate weight
    rename(repwtflag = REPWTP) %>% 
    as_survey_rep(type="BRR", repweights=starts_with("REPWTP"), weights=PERWT) %>%
    group_by(geogroup, agegroup, moved_states) %>% 
    summarise(missing_edu = survey_mean(Group=="NA/Missing", proportion = TRUE, vartype = "ci", level=.9),
              less_hs = survey_mean(Group=="Less than HS Diploma", proportion = TRUE, vartype = "ci", level=.9),
              hs_ged = survey_mean(Group=="High School/GED", proportion = TRUE, vartype = "ci", level=.9), 
              some_college = survey_mean(Group=="Some College, No Degree", proportion = TRUE, vartype = "ci", level=.9),
              associate = survey_mean(Group=="Associate's Degree", proportion = TRUE, vartype = "ci", level=.9),
              bachelors = survey_mean(Group=="Bachelor's Degree", proportion = TRUE, vartype = "ci", level=.9),
              advanced = survey_mean(Group=="Advanced Degree", proportion = TRUE, vartype = "ci", level=.9)) %>%
  filter(moved_states==1) %>%
  select(-moved_states)

  save(educ_inmigration, file="./caches/educanalysis.rda")
}


######################################
######### Migration From MN ##########
######### To Other States ############
######################################
if (file.exists("./caches/outmigration.rda")) {
  load("./caches/outmigration.rda")
} else {
  source("clean.R")
}

library(readxl)
educ_groups <- read_excel("educ_cats.xlsx")


# for people who moved away from Minnesota, get number who are students
educ_outmigration <- outmigration %>%
  left_join(educ_groups) %>% 
  as_survey_rep(type="BRR", repweights=starts_with("REPWTP"), weights=PERWT)  %>%
  group_by(geogroup, agegroup) %>%
  summarise(missing_edu = survey_mean(Group=="NA/Missing", proportion = TRUE, vartype = "ci", level=.9),
            less_hs = survey_mean(Group=="Less than HS Diploma", proportion = TRUE, vartype = "ci", level=.9),
            hs_ged = survey_mean(Group=="High School/GED", proportion = TRUE, vartype = "ci", level=.9), 
            some_college = survey_mean(Group=="Some College, No Degree", proportion = TRUE, vartype = "ci", level=.9),
            associate = survey_mean(Group=="Associate's Degree", proportion = TRUE, vartype = "ci", level=.9),
            bachelors = survey_mean(Group=="Bachelor's Degree", proportion = TRUE, vartype = "ci", level=.9),
            advanced = survey_mean(Group=="Advanced Degree", proportion = TRUE, vartype = "ci", level=.9))

######################################
######### Merge in and Out ###########
#########    Migration     ###########
######################################
library(stringr)

#merge inmigration and outmigration, keep relevant columns, make long for graphing
educ_migration <- left_join(educ_inmigration, educ_outmigration, 
                               by=c("geogroup", "agegroup"),
                               suffix=c("_in", "_out")) %>% 
  mutate(geogroup = ifelse(geogroup=="Greater MN", "Greater Minnesota", geogroup),
         geogroup = ifelse(geogroup=="Metro", "Other Metro Counties", geogroup)) %>% 
  filter(agegroup %in% c("18 to 21", "22 to 29")) %>% 
  gather(key = variable, value=value, missing_edu_in, missing_edu_out,
                                      less_hs_in, less_hs_out, 
                                      hs_ged_in, hs_ged_out,
                                      some_college_in, some_college_out,
                                      associate_in, associate_out,
                                      bachelors_in, bachelors_out,
                                      advanced_in, advanced_out) %>%
  mutate(direction=ifelse(grepl("_in", variable), "Inmigration", "Outmigration")) %>% 
  mutate(lowerror = case_when(variable == "missing_edu_in" ~ missing_edu_low_in,
                              variable == "missing_edu_out" ~ missing_edu_low_out,
                              variable == "less_hs_in" ~ less_hs_low_in,
                              variable == "less_hs_out" ~ less_hs_low_out,
                              variable == "hs_ged_in" ~ hs_ged_low_in,
                              variable == "hs_ged_out" ~ hs_ged_low_out,
                              variable == "some_college_in" ~ some_college_low_in,
                              variable == "some_college_out" ~ some_college_low_out,
                              variable == "associate_in" ~ associate_low_in,
                              variable == "associate_out" ~ associate_low_out,
                              variable == "bachelors_in" ~ bachelors_low_in,
                              variable == "bachelors_out" ~ bachelors_low_out,
                              variable == "advanced_in" ~ advanced_low_in,
                              variable == "advanced_out" ~ advanced_low_out), 
         upperror = case_when(variable == "missing_edu_in" ~ missing_edu_upp_in,
                              variable == "missing_edu_out" ~ missing_edu_upp_out,
                              variable == "less_hs_in" ~ less_hs_upp_in,
                              variable == "less_hs_out" ~ less_hs_upp_out,
                              variable == "hs_ged_in" ~ hs_ged_upp_in,
                              variable == "hs_ged_out" ~ hs_ged_upp_out,
                              variable == "some_college_in" ~ some_college_upp_in,
                              variable == "some_college_out" ~ some_college_upp_out,
                              variable == "associate_in" ~ associate_upp_in,
                              variable == "associate_out" ~ associate_upp_out,
                              variable == "bachelors_in" ~ bachelors_upp_in,
                              variable == "bachelors_out" ~ bachelors_upp_out,
                              variable == "advanced_in" ~ advanced_upp_in,
                              variable == "advanced_out" ~ advanced_upp_out),
         variable = case_when(grepl("advanced", variable) ~ "Advanced Degree",
                              grepl("associate", variable) ~ "Associate's Degree",
                              grepl("bachelors", variable) ~ "Bachelor's Degree",
                              grepl("hs_ged", variable) ~ "HS Diploma/GED",
                              grepl("less_hs", variable) ~ "Less than HS Diploma",
                              grepl("missing", variable) ~ "Missing/NA",
                              grepl("some_college", variable) ~ "Some College, No Degree"
         )
  ) %>%
  select(-matches("_low_"), -matches("_upp_")) %>% 
  mutate(geogroup = factor(geogroup, levels=c("Ramsey","Hennepin","Other Metro Counties","Greater Minnesota")),
         direction = factor(direction, levels=c("Outmigration","Inmigration")),
         variable = factor(variable, levels = c("Missing/NA",
                                                "Less than HS Diploma",
                                                "HS Diploma/GED",
                                                "Some College, No Degree",
                                                "Associate's Degree",
                                                "Bachelor's Degree",
                                                "Advanced Degree"))
  )

save(educ_migration, file="./caches/educresults.rda")
  

######################################
######### Organized Data for #########
######### manual inspection ##########
######################################
educanalysis <- educ_migration %>% 
  filter(agegroup == "22 to 29") %>% 
  arrange(geogroup, direction)

######################################
######### Estimate Education #########
####Levels of the 22 to 29 YO Pop ####
######################################
load("./caches/inmigration.rda")
library(readxl)
library(srvyr)
educ_groups <- read_excel("educ_cats.xlsx")

if (file.exists("./caches/educ_noregion.rda")) {
  load("./caches/educ_noregion.rda")
} else {
  # for people who moved, get educational attainment
  educ_noregion <- inmigration %>%
    left_join(educ_groups) %>%
    mutate(moved_states = ifelse(!(MIGPLAC1 %in% c(0,27)) & MIGPLAC1<100, 1, 0)) %>% 
    #rename replicate weights flag so it doesn't get used as a replicate weight
    rename(repwtflag = REPWTP) %>% 
    as_survey_rep(type="BRR", repweights=starts_with("REPWTP"), weights=PERWT) %>%
    group_by(agegroup) %>% 
    summarise(missing_edu = survey_mean(Group=="NA/Missing", proportion = TRUE, vartype = "ci", level=.9),
              less_hs = survey_mean(Group=="Less than HS Diploma", proportion = TRUE, vartype = "ci", level=.9),
              hs_ged = survey_mean(Group=="High School/GED", proportion = TRUE, vartype = "ci", level=.9), 
              some_college = survey_mean(Group=="Some College, No Degree", proportion = TRUE, vartype = "ci", level=.9),
              associate = survey_mean(Group=="Associate's Degree", proportion = TRUE, vartype = "ci", level=.9),
              bachelors = survey_mean(Group=="Bachelor's Degree", proportion = TRUE, vartype = "ci", level=.9),
              advanced = survey_mean(Group=="Advanced Degree", proportion = TRUE, vartype = "ci", level=.9)) %>% 
    select(-contains("upp"), -contains("low"))
  
  save(educ_noregion, file="./caches/educ_noregion.rda")
}
