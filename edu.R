library(tidyverse)
library(srvyr)

######################################
######### Migration to MN   ##########
######### From Other States ##########
######################################

#cache the survey analysis by education, because it takes a while.
if (file.exists("educanalysis.rda")) {
  load("educanalysis.rda")
} else {
  
  if (file.exists("inmigration.rda")) {
    load("inmigration.rda")
  } 
  else {source("clean.R")}
  
  library(readxl)
  educ_groups <- read_excel("educ_cats.xlsx")
  
  # for people who moved, get educational attainment
  educ_inmigration <- inmigration %>%
    left_join(educ_groups) %>%
    mutate(moved_states = ifelse(!(MIGPLAC1 %in% c(0,27)) & MIGPLAC1<100, 1, 0)) %>% 
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

  save(educ_inmigration, file="educanalysis.rda")
}


######################################
######### Migration From MN ##########
######### To Other States ############
######################################
if (file.exists("outmigration.rda")) {
  load("outmigration.rda")
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
  filter(agegroup %in% c("18 to 23", "24 to 29")) %>% 
  gather(key = variable, value=value, missing_edu_in, missing_edu_out,
                                      less_hs_in, less_hs_out, 
                                      hs_ged_in, hs_ged_out,
                                      some_college_in, some_college_out,
                                      associate_in, associate_out,
                                      bachelors_in, bachelors_out,
                                      advanced_in, advanced_out) %>%
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
  mutate(direction=ifelse(grepl("_in", variable), "Inmigration", "Outmigration")) %>%
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


educ_18 <- filter(educ_migration, agegroup=="18 to 23")  %>% 
  ggplot(aes(x=direction,
             y=value,
             fill=variable)) +
  theme_migration +
  geom_bar(stat="identity",position="fill") +
  coord_flip() +
  facet_wrap(~geogroup, ncol=2) +
  scale_fill_brewer(palette="Set1")

educ_18

ggsave("educ_18.png", educ_18,width=8,height=6) 


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

ggsave("students_18.png", student_18,width=8,height=6) 

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

ggsave("students_24.png", student_24,width=8,height=6) 

