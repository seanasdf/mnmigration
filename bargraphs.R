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

#get inmigration by geographic and age group
inmigration_by_group <- inmigration %>%
  #create dummy variable to identify people who moved
  mutate(moved_states = ifelse(!(MIGPLAC1 %in% c(0,27)) & MIGPLAC1<100, 1, 0)) %>%
  #rename replicate weights flag so it doesn't get used as a replicate weight
  rename(repwtflag = REPWTP) %>% 
  as_survey_rep(type="BRR", repweights=starts_with("REPWTP"), weights=PERWT) %>%
  group_by(geogroup, agegroup) %>%
  summarise(pct_moved_in = survey_mean(moved_states), moved_in = survey_total(moved_states)) 


######################################
######### Migration from MN ##########
######################################

#Get total population for each region
population_by_region <- inmigration %>%
  #rename replicate weights flag so it doesn't get used as a replicate weight
  rename(repwtflag = REPWTP) %>% 
  as_survey_rep(type="BRR", repweights=starts_with("REPWTP"), weights=PERWT) %>% 
  group_by(geogroup, agegroup) %>% 
  summarise(population = survey_total())

if (file.exists("./caches/outmigration.rda")) {
  load("./caches/outmigration.rda")
} else {
  source("clean.R")
}

outmigration_by_group <- outmigration %>%
  #rename replicate weights flag so it doesn't get used as a replicate weight
  rename(repwtflag = REPWTP) %>% 
  as_survey_rep(type="BRR", repweights=starts_with("REPWTP"), weights=PERWT)  %>%
  group_by(geogroup, agegroup) %>%
  summarise(moved_out = survey_total()) %>% 
  left_join(population_by_region) %>%
  mutate(pct_moved_out = moved_out/population, 
         pct_moved_out_se = sqrt(moved_out_se^2 + ((pct_moved_out/population)^2)*population_se^2)/(population)
         ) %>% 
  select(-population, -population_se)



######################################
###### Write Totals out To CSV #######
######################################

netmig <- left_join(inmigration_by_group, outmigration_by_group) %>%
  mutate(net_migration = moved_in-moved_out )
write.csv(netmig, "netmigration.csv")


######################################
##### Graph Percent Migration ########
######################################

pct_netmig <- left_join(inmigration_by_group, outmigration_by_group) %>%
  mutate(geogroup = ifelse(geogroup=="Greater MN", "Greater Minnesota", geogroup),
         geogroup = ifelse(geogroup=="Metro", "Other Metro Counties", geogroup)) %>%
  select(-moved_in, -moved_in_se, -moved_out, -moved_out_se) %>% 
  gather(direction, mig, pct_moved_in, pct_moved_out) %>%
  mutate(se = ifelse(direction=="pct_moved_in", pct_moved_in_se, pct_moved_out_se)) %>%
  select(-pct_moved_in_se, -pct_moved_out_se) %>%
  mutate(geogroup = factor(geogroup, levels=c("Ramsey","Hennepin","Other Metro Counties","Greater Minnesota")),
         direction = factor(direction, levels=c("pct_moved_out", "pct_moved_in")),
         mig = mig *1000,
         se = se * 1000)

####Create ggplot2 theme for plots####
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
#create theme
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
  panel.grid.major.y = element_line(color="#d9d9d9")
  ) + 
  theme(plot.title = element_text(size=36, hjust = 0.5),
        plot.caption = element_text(size=24))


####Actually plot the thing for 18 to 23 year olds####
netmig18_pct <- filter(pct_netmig, agegroup=="18 to 23") %>%
  ggplot(aes(x=geogroup, y=mig, fill=direction)) +
  theme_migration +
  geom_bar(stat="identity", position="dodge") +
  scale_fill_brewer(labels=c("Left MN for Another State", "Moved to MN from Another State"), 
                    palette="Set1")+
  labs(title = "Average Annual Migration between Minnesota and Other States, 2011-2015\nPersons Ages 18 to 23",
       caption = "Source: MN House Research. Error bars represent 90% confidence intervals.\n 2015 American Community Survey 5-year Estimates. IPUMS-USA, University of Minnesota.",
       y="Persons Moving per 1,000 Persons Ages 18 to 23") +
  geom_errorbar(aes(ymin=mig-1.645*se, ymax=mig+1.645*se), 
                width = .2,
                position=position_dodge(.9))

ggsave("./plots/netmig18_pct.png", netmig18_pct,width=8,height=6) 

####Actually plot the thing for 24 to 29 year olds####
netmig24_pct <- filter(pct_netmig, agegroup=="24 to 29") %>%
  ggplot(aes(x=geogroup, y=mig, fill=direction)) +
  geom_bar(stat="identity", position="dodge") +
  theme_migration +
  scale_fill_brewer(labels=c("Left MN for Another State", "Moved to MN from Another State"), 
                    palette="Set1")+
  labs(title = "Average Annual Migration between Minnesota and Other States, 2011-2015\nPersons Ages 24 to 29",
       caption = "Source: MN House Research. Error bars represent 90% confidence intervals. 
       2015 American Community Survey 5-year Estimates. IPUMS-USA, University of Minnesota.",
       y="Persons Moving per 1,000 Persons Ages 24 to 29") +
  geom_errorbar(aes(ymin=mig-1.645*se, ymax=mig+1.645*se), 
                width = .2,
                position=position_dodge(.9))


ggsave("./plots/netmig24_pct.png", netmig24_pct,width=8,height=6) 


######################################
##### Graph total Migration ########
######################################


total_netmig <- left_join(inmigration_by_group, outmigration_by_group) %>%
  mutate(geogroup = ifelse(geogroup=="Greater MN", "Greater Minnesota", geogroup),
         geogroup = ifelse(geogroup=="Metro", "Other Metro Counties", geogroup)) %>%
  select(-pct_moved_in, -pct_moved_in_se, -pct_moved_out, -pct_moved_out_se) %>% 
  gather(direction, mig, moved_in, moved_out) %>%
  mutate(se = ifelse(direction=="moved_in", moved_in_se, moved_out_se)) %>%
  select(-moved_in_se, -moved_out_se) %>%
  mutate(geogroup = factor(geogroup, levels=c("Ramsey","Hennepin","Other Metro Counties","Greater Minnesota")),
         direction = factor(direction, levels=c("moved_out", "moved_in")))

####Actually plot the thing for 18 to 23 year olds####
netmig18_tot <- filter(total_netmig, agegroup=="18 to 23") %>%
  ggplot(aes(x=geogroup, y=mig, fill=direction)) +
  theme_migration +
  geom_bar(stat="identity", position="dodge") +
  scale_fill_brewer(labels=c("Left MN for Another State", "Moved to MN from Another State"), 
                    palette="Set1")+
  labs(title = "Average Annual Migration between Minnesota and Other States, 2011-2015\nPersons Ages 18 to 23",
       caption = "Source: MN House Research. Error bars represent 90% confidence intervals.\n 2015 American Community Survey 5-year Estimates. IPUMS-USA, University of Minnesota.",
       y="Total Individuals Ages 18 to 23 Who Moved") +
  scale_y_continuous(labels=scales::comma) +
  geom_errorbar(aes(ymin=mig-1.645*se, ymax=mig+1.645*se), 
                width = .2,
                position=position_dodge(.9))

ggsave("./plots/netmig18_tot.png", netmig18_tot,width=8,height=6) 

####Actually plot the thing for 24 to 29 year olds####
netmig24_tot <- filter(total_netmig, agegroup=="24 to 29") %>%
  ggplot(aes(x=geogroup, y=mig, fill=direction)) +
  theme_migration +
  geom_bar(stat="identity", position="dodge") +
  scale_fill_brewer(labels=c("Left MN for Another State", "Moved to MN from Another State"), 
                    palette="Set1")+
  labs(title = "Average Annual Migration between Minnesota and Other States, 2011-2015\nPersons Ages 24 to 29",
       caption = "Source: MN House Research. Error bars represent 90% confidence intervals.\n 2015 American Community Survey 5-year Estimates. IPUMS-USA, University of Minnesota.",
       y="Total Individuals Ages 18 to 23 Who Moved") +
  scale_y_continuous(labels=scales::comma) +
  geom_errorbar(aes(ymin=mig-1.645*se, ymax=mig+1.645*se), 
                width = .2,
                position=position_dodge(.9))


ggsave("./plots/netmig24_tot.png", netmig24_tot,width=8,height=6) 


