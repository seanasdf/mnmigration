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
  filter(agegroup %in% c("18 to 23", "24 to 29")) %>%
  select(-missing_student_in, -missing_student_se_in, -missing_student_out, -missing_student_se_out) %>%
  gather(key=variable, value=value, pct_student_in, pct_student_out) %>%
  mutate(direction=ifelse(grepl("_in", variable), "Inmigration", "Outmigration"),
         upper_error = ifelse(direction == "Inmigration", pct_student_upp_in, pct_student_upp_out),
         lower_error = ifelse(direction == "Inmigration", pct_student_low_in, pct_student_low_out)) %>%
  select(-variable,-pct_student_low_in, -pct_student_upp_in, -pct_student_low_out, -pct_student_upp_out) %>% 
  mutate(geogroup = factor(geogroup, levels=c("Ramsey","Hennepin","Other Metro Counties","Greater Minnesota")))

######################################
####### Graph Student Status #########
######################################

library(ggplot2)
library(showtext)

#add roboto font from google
font.add.google("Roboto", "roboto")
showtext.auto()

#create default text
migration_text <- element_text(family="roboto", 
                               size=30, 
                               face="plain", 
                               color="black",
                               lineheight = 0.4
)

#create graph theme
theme_migration <-  theme(
  panel.background = element_blank(),
  axis.ticks.x=element_blank(),
  axis.ticks.y=element_blank(),
  axis.title.x=element_blank(),
  axis.title.y=migration_text,
  axis.text.x=migration_text,
  axis.text.y=migration_text,
  legend.text=migration_text,
  plot.caption = migration_text,
  legend.title=element_blank(),
  legend.position="bottom",
  plot.title =migration_text,
  panel.grid.major.y = element_line(color="#d9d9d9"),
  panel.grid.major.x = element_blank()
) + 
  theme(plot.title = element_text(size=36, hjust = 0.5),
        plot.caption = element_text(size=24))


student_18 <- filter(student_migration, agegroup=="18 to 23") %>%
              mutate(direction = factor(direction, levels=c("Outmigration","Inmigration"))) %>% 
              ggplot(aes(x=geogroup,
                         y=value,
                         fill=direction)) +
              theme_migration +
              geom_bar(stat="identity",position="dodge") +
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

student_24 <- filter(student_migration, agegroup=="24 to 29") %>%
  mutate(direction = factor(direction, levels=c("Outmigration","Inmigration"))) %>% 
  ggplot(aes(x=geogroup,
             y=value,
             fill=direction)) +
  theme_migration +
  geom_bar(stat="identity",position="dodge") +
  scale_fill_brewer(labels=c("Left for Another State", 
                             "Moved to Minnesota From Another State"), 
                    palette="Set1",
                    guide=guide_legend(title="Direction of Migration")) +
  scale_y_continuous(labels=scales::percent) +
  geom_errorbar(aes(ymin=lower_error, ymax=upper_error), 
                width = .2,
                position=position_dodge(.9)) +
  labs(title = "Share of 24-29 Year-olds Moving to and From Minnesota that Were Students",
       y="Percent of 24-29 Year-olds who Moved that Were Students",
       x="",
       caption = "Source: MN House Research. Error bars represent 90% confidence intervals. 
       2015 American Community Survey 5-year Estimates. IPUMS-USA, University of Minnesota.")

ggsave("./plots/students_24.png", student_24,width=8,height=6) 

