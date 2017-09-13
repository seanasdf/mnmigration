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

######################################
##### Graph Percent Migration ########
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
  axis.title.x=migration_text,
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

#Statewide net migration graph, all ages
netmig_linegraph <- netmig_mn %>% 
  mutate(AGE = as.numeric(AGE),
         posneg = ifelse(netmig>0, "Positive", "Negative")) %>% 
  ggplot(aes(x=AGE, y=netmig, color=posneg)) +
  scale_color_brewer(palette="Set1") +
  geom_point() +
  geom_errorbar(aes(ymin=netmig-1.645*se, ymax=netmig+1.645*se), 
                width = .1) +
  theme_migration +
  labs(title = "Average Annual Net Migration between Minnesota and Other States by Age, 2011-2015",
       caption = "Source: MN House Research.
       2015 American Community Survey 5-year Estimates. IPUMS-USA, University of Minnesota.",
       y="Net Migration of Individuals of Given Age",
       x="Age") +
  scale_y_continuous(labels=scales::comma) +
  guides(fill=FALSE) +
  theme(legend.position="none")
netmig_linegraph

ggsave("./plots/netmig.png", netmig_linegraph,width=8,height=6) 


