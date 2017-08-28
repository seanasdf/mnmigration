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
  summarise(pct_moved_in = survey_mean(moved_states))


######################################
######### Migration from MN ##########
######### to Other States ############
######################################
#Get total population for each region
population_by_age <- inmigration %>%
  mutate(AGE = as.character(AGE),
         one = 1) %>% 
  #rename replicate weights flag so it doesn't get used as a replicate weight
  rename(repwtflag = REPWTP) %>% 
  as_survey_rep(type="BRR", repweights=starts_with("REPWTP"), weights=PERWT) %>% 
  group_by(AGE) %>% 
  #this is throwing an error when I do it with an empty call to survey_total, but this works
  summarise(population = survey_total(one))


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
  summarise(moved_out = survey_total(one)) %>% 
  left_join(population_by_age) %>%
  mutate(pct_moved_out = moved_out/population, 
         pct_moved_out_se = sqrt(moved_out_se^2 + ((pct_moved_out/population)^2)*population_se^2)/(population)
  ) %>% 
  select(-population, -population_se, -moved_out, -moved_out_se)

######################################
##### Merge In and Outmigration ######
######################################
pct_netmig <- left_join(inmigration_by_age, outmigration_by_age) %>%
  gather(direction, mig, pct_moved_in, pct_moved_out) %>%
  mutate(se = ifelse(direction=="pct_moved_in", pct_moved_in_se, pct_moved_out_se)) %>%
  select(-pct_moved_in_se, -pct_moved_out_se) %>%
  mutate(direction = factor(direction, levels = c("pct_moved_out", "pct_moved_in"))) 

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


#actually graph the thing
#first, broken out by region
state_linegraph <- pct_netmig %>% 
  mutate(AGE = as.numeric(AGE)) %>% 
  ggplot(aes(x=AGE, y=mig, group=direction, colour=direction)) +
  geom_point(shape=1) +
  geom_line() +
  theme_migration +
  theme(strip.text.x = migration_text,
        strip.background=element_rect(color="white", fill="white")) +
  scale_color_brewer(labels=c("Left MN for Another State", "Moved to MN from Another State"), 
                     palette="Set1") +
  labs(title = "Average Annual Migration between Minnesota and Other States by Age, 2011-2015",
       caption = "Source: MN House Research.
       2015 American Community Survey 5-year Estimates. IPUMS-USA, University of Minnesota.",
       y="Percent of Population in Age Group",
       x="Age") +
  scale_y_continuous(labels=scales::percent) 

ggsave("./plots/statelinegraph.png", state_linegraph,width=8,height=6) 

#actually graph the thing
#first, broken out by region
state_linegraph_young <- pct_netmig %>% 
  mutate(AGE = as.numeric(AGE)) %>% 
  filter(AGE>17 & AGE<32) %>% 
  ggplot(aes(x=AGE, y=mig, group=direction, colour=direction)) +
  geom_point(shape=1) +
  geom_line() +
  theme_migration +
  theme(strip.text.x = migration_text,
        strip.background=element_rect(color="white", fill="white")) +
  scale_color_brewer(labels=c("Left MN for Another State", "Moved to MN from Another State"), 
                     palette="Set1") +
  labs(title = "Average Annual Migration between Minnesota and Other States by Age, 2011-2015",
       caption = "Source: MN House Research.
       2015 American Community Survey 5-year Estimates. IPUMS-USA, University of Minnesota.",
       y="Percent of Population in Age Group",
       x="Age") +
  scale_y_continuous(labels=scales::percent) 

ggsave("./plots/statelinegraph_young.png", state_linegraph_young,width=8,height=6) 

#second, create another graph--with error bars
state_linegraph_young_witherrors <- state_linegraph_young +
  geom_errorbar(aes(ymin=mig-1.645*se, ymax=mig+1.645*se), 
                width = .2) +
  labs(caption = "Source: MN House Research. Error bars represent 90% confidence intervals.
       2015 American Community Survey 5-year Estimates. IPUMS-USA, University of Minnesota.")
ggsave("./plots/state_linegraph_young_witherrors.png", state_linegraph_young_witherrors,width=8,height=6) 


