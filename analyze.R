library(tidyverse)
library(srvyr)

######################################
######### Migration to MN   ##########
######### From Other States ##########
######################################

#read in inmigration data
if (file.exists("inmigration.rda")) {
  load("inmigration.rda")
} else {
  source("clean.R")
}


#get inmigration by geographic and age group
inmigration_by_group <- inmigration %>%
  #creat dummy variable to identify people who moved
  mutate(moved_states = ifelse(!(MIGPLAC1 %in% c(0,27)) & MIGPLAC1<100, 1, 0)) %>%
  as_survey_rep(type="BRR", repweights=starts_with("REPWTP"), weights=PERWT) %>%
  group_by(geogroup, agegroup) %>%
  summarise(pct_moved_in = survey_mean(moved_states), moved_in = survey_total(moved_states)) 


######################################
######### Migration from MN ##########
######################################

#Get total population for each region
population_by_region <- inmigration %>%
  as_survey_rep(type="BRR", repweights=starts_with("REPWTP"), weights=PERWT) %>% 
  group_by(geogroup, agegroup) %>% 
  summarise(population = survey_total())

#get N for each region
regional_n <- inmigration %>% group_by(geogroup, agegroup) %>% summarise(n=n())

population_by_region <- left_join(population_by_region, regional_n)


if (file.exists("outmigration.rda")) {
  load("outmigration.rda")
} else {
  source("clean.R")
}

outmigration_by_group <- outmigration %>% 
  as_survey_rep(type="BRR", repweights=starts_with("REPWTP"), weights=PERWT)  %>%
  group_by(geogroup, agegroup) %>%
  summarise(moved_out = survey_total()) %>% 
  left_join(population_by_region) %>%
  mutate(pct_moved_out = moved_out/population, 
         pct_moved_out_se = sqrt((pct_moved_out*(1-pct_moved_out)/n))
         ) %>% 
  select(-population, population_se)



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
  select(-moved_in, -moved_in_se, -moved_out, -moved_out_se, -n) %>% 
  gather(direction, mig, pct_moved_in, pct_moved_out) %>%
  mutate(se = ifelse(direction=="pct_moved_in", pct_moved_in_se, pct_moved_out_se)) %>%
  select(-pct_moved_in_se, -pct_moved_out_se) %>%
  mutate(geogroup = factor(geogroup, levels=c("Ramsey","Hennepin","Other Metro Counties","Greater Minnesota")),
         direction = factor(direction, levels=c("pct_moved_out", "pct_moved_in")))

####Create ggplot2 theme for plots####
library(ggplot2)
library(showtext)

#add roboto font from google
font.add.google("Roboto", "roboto")

#create default text
migration_text <- element_text(family="roboto", 
                               size=24, 
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
  theme(plot.title = element_text(size=35, hjust = 0.5))


####Actually plot the thing for 18 to 23 year olds####
netmig18_pct <- filter(pct_netmig, agegroup=="18 to 23") %>%
  ggplot(aes(x=geogroup, y=mig, fill=direction)) +
  theme_migration +
  geom_bar(stat="identity", position="dodge") +
  scale_y_continuous(labels=scales::percent) +
  scale_fill_brewer(labels=c("Left MN for Another State", "Moved to MN from Another State"), 
                    palette="Set1")+
  labs(title = "Average Annual Migration between Minnesota and Other States, 2011-2015\nPersons Aged 18 to 23",
       caption = "Source: MN House Research/MN State Demographer.\n Error bars represent 90% confidence intervals. 2015 American Community Survey 5-year Estimates. IPUMS-USA, University of Minnesota.",
       y="Percent of Population in Age Group") +
  geom_errorbar(aes(ymin=mig-1.645*se, ymax=mig+1.645*se), 
                width = .2,
                position=position_dodge(.9))

ggsave("netmig18_pct.png", netmig18_pct,width=8,height=6) 

####Actually plot the thing for 24 to 29 year olds####
netmig24_pct <- filter(pct_netmig, agegroup=="24 to 29") %>%
  ggplot(aes(x=geogroup, y=mig, fill=direction)) +
  geom_bar(stat="identity", position="dodge") +
  theme_migration +
  scale_y_continuous(labels=scales::percent) +
  scale_fill_brewer(labels=c("Left MN for Another State", "Moved to MN from Another State"), 
                    palette="Set1")+
  labs(title = "Average Annual Migration between Minnesota and Other States, 2011-2015\nPersons Aged 24 to 29",
       caption = "Source: MN House Research/MN State Demographer. Error bars represent 90% confidence intervals. 
       2015 American Community Survey 5-year Estimates. IPUMS-USA, University of Minnesota.",
       y="Percent of Population in Age Group") +
  geom_errorbar(aes(ymin=mig-1.645*se, ymax=mig+1.645*se), 
                width = .2,
                position=position_dodge(.9))


ggsave("netmig24_pct.png", netmig24_pct,width=8,height=6) 



# ######################################
# ###### Graph Total Migration #########
# ######################################
# 
# netmig_long <- left_join(inmigration_by_group, outmigration_by_group) %>%
#   mutate(geogroup = ifelse(geogroup=="Greater MN", "Greater Minnesota", geogroup),
#          geogroup = ifelse(geogroup=="Metro", "Other Metro Counties", geogroup)) %>%
#   select(-pct_moved_in, -pct_moved_in_se, -pct_moved_out, -pct_moved_out_se) %>% 
#   gather(direction, mig, moved_in, moved_out) %>%
#   mutate(se = ifelse(direction=="moved_in", moved_in_se, moved_out_se)) %>%
#   select(-moved_in_se, -moved_out_se) %>%
#   mutate(geogroup = factor(geogroup, levels=c("Ramsey","Hennepin","Other Metro Counties","Greater Minnesota")),
#          direction = factor(direction, levels=c("moved_out", "moved_in")))
# 
# 
# library(ggplot2)
# barplot17_totals <- filter(netmig_long, agegroup=="17 to 23") %>%
#   ggplot(aes(x=geogroup, y=mig, fill=direction)) +
#   geom_bar(stat="identity", position="dodge") +
#   geom_text(aes(x=geogroup, y=mig, label=scales::comma(mig)),
#             position=position_dodge(width=0.9), vjust=-0.25) +
#   theme(
#     panel.background = element_blank(),
#     axis.ticks.x=element_blank(),
#     axis.ticks.y=element_blank(),
#     axis.title.x=element_blank(),
#     axis.title.y=element_blank(),
#     axis.text.x=element_text(size=10, color="black"),
#     axis.text.y=element_blank(),
#     legend.title=element_blank(),
#     legend.position="bottom",
#     plot.caption=element_text(size=8, hjust=.5),
#     plot.title = element_text(hjust = 0.5)
#   ) +
#   scale_y_continuous(labels=scales::comma) +
#   scale_fill_discrete(labels=c("Left MN for Another State", "Moved to MN from Another State")) +
#   labs(title = "Average Annual Migration between Minnesota and Other States, 2011-2015\nPersons Aged 17 to 23",
#        caption = "Source: House Research. 2015 American Community Survey 5-year Estimates. IPUMS-USA, University of Minnesota.") 
# 
# ggsave("ages17to23_totals.png", barplot17_totals,width=8,height=6) 
# 
# 
# library(ggplot2)
# barplot17_pct <- filter(netmig_long, agegroup=="17 to 23") %>%
#   ggplot(aes(x=geogroup, y=mig, fill=direction)) +
#   geom_bar(stat="identity", position="dodge") +
#   geom_text(aes(x=geogroup, y=mig, label=scales::comma(mig)),
#             position=position_dodge(width=0.9), vjust=-0.25) +
#   theme(
#     panel.background = element_blank(),
#     axis.ticks.x=element_blank(),
#     axis.ticks.y=element_blank(),
#     axis.title.x=element_blank(),
#     axis.title.y=element_blank(),
#     axis.text.x=element_text(size=10, color="black"),
#     axis.text.y=element_blank(),
#     legend.title=element_blank(),
#     legend.position="bottom",
#     plot.caption=element_text(size=8, hjust=.5),
#     plot.title = element_text(hjust = 0.5)
#   ) +
#   scale_y_continuous(labels=scales::comma) +
#   scale_fill_discrete(labels=c("Left MN for Another State", "Moved to MN from Another State")) +
#   labs(title = "Average Annual Migration between Minnesota and Other States, 2011-2015\nPersons Aged 17 to 23",
#        caption = "Source: House Research. 2015 American Community Survey 5-year Estimates. IPUMS-USA, University of Minnesota.") 
# 
# 
# 
# 
# barplot17_errorbars <- filter(netmig_long, agegroup=="17 to 23") %>%
#   ggplot(aes(x=geogroup, y=mig, fill=direction)) +
#   geom_bar(stat="identity", position="dodge") +
#   geom_errorbar(aes(ymin=mig-se, ymax=mig+se), 
#                 width = .2,
#                 position=position_dodge(.9)) +
#   theme(
#     panel.background = element_blank(),
#     axis.ticks.x=element_blank(),
#     axis.ticks.y=element_blank(),
#     axis.title.x=element_blank(),
#     axis.title.y=element_blank(),
#     axis.text.x=element_text(size=10, color="black"),
#     axis.text.y=element_blank(),
#     legend.title=element_blank(),
#     legend.position="bottom",
#     plot.caption=element_text(size=8, hjust=.5),
#     plot.title = element_text(hjust = 0.5)
#   ) +
#   scale_fill_discrete(labels=c("Left MN for Another State", "Moved to MN from Another State")) +
#   labs(title = "Average Annual Migration between Minnesota and Other States, 2011-2015\nPersons Aged 17 to 23",
#        caption = "Source: House Research. 2015 American Community Survey 5-year Estimates. IPUMS-USA, University of Minnesota.") 
# 
# ggsave("ages17to23_errorbars.png", barplot17_errorbars,width=8,height=6) 
# 
# 
# barplot24 <- filter(netmig_long, agegroup=="24 to 30") %>%
#   ggplot(aes(x=geogroup, y=mig, fill=direction)) +
#   geom_bar(stat="identity", position="dodge") +
#   geom_text(aes(x=geogroup, y=mig, label=scales::comma(mig)),
#             position=position_dodge(width=0.9), vjust=-0.25) +
#   theme(
#     panel.background = element_blank(),
#     axis.ticks.x=element_blank(),
#     axis.ticks.y=element_blank(),
#     axis.title.x=element_blank(),
#     axis.title.y=element_blank(),
#     axis.text.x=element_text(size=10, color="black"),
#     axis.text.y=element_blank(),
#     legend.title=element_blank(),
#     legend.position="bottom",
#     plot.caption=element_text(size=8, hjust=.5),
#     plot.title = element_text(hjust = 0.5)
#   ) +
#   scale_fill_discrete(labels=c("Left MN for Another State", "Moved to MN from Another State")) +
#   labs(title = "Average Annual Migration between Minnesota and Other States, 2011-2015\nPersons Aged 24 to 30",
#        caption = "Source: House Research. 2015 American Community Survey 5-year Estimates. IPUMS-USA, University of Minnesota.") 
# 
# ggsave("ages24to30_totals.png", barplot24,width=8,height=6)
# 
# barplot24_errorbars <- filter(netmig_long, agegroup=="24 to 30") %>%
#   ggplot(aes(x=geogroup, y=mig, fill=direction)) +
#   geom_bar(stat="identity", position="dodge") +
#   geom_errorbar(aes(ymin=mig-se, ymax=mig+se), 
#                 width = .2,
#                 position=position_dodge(.9))+
#   theme(
#     panel.background = element_blank(),
#     axis.ticks.x=element_blank(),
#     axis.ticks.y=element_blank(),
#     axis.title.x=element_blank(),
#     axis.title.y=element_blank(),
#     axis.text.x=element_text(size=10, color="black"),
#     axis.text.y=element_blank(),
#     legend.title=element_blank(),
#     legend.position="bottom",
#     plot.caption=element_text(size=8, hjust=.5),
#     plot.title = element_text(hjust = 0.5)
#   ) +
#   scale_y_continuous(labels=scales::comma) +
#   scale_fill_discrete(labels=c("Left MN for Another State", "Moved to MN from Another State")) +
#   labs(title = "Average Annual Migration between Minnesota and Other States, 2011-2015\nPersons Aged 24 to 30",
#        caption = "Source: House Research. 2015 American Community Survey 5-year Estimates. IPUMS-USA, University of Minnesota.") 
# 
# 
# ggsave("ages24to30_errorbars.png", barplot24_errorbars,width=8,height=6)
# 
# ######################################
# ####### Graph PCT Migration ##########
# ######################################
# 
# migration_percents_long <- left_join(inmigration_by_group, outmigration_by_group) %>%
#   mutate(geogroup = ifelse(geogroup=="Greater MN", "Greater Minnesota", geogroup),
#          geogroup = ifelse(geogroup=="Metro", "Other Metro Counties", geogroup)) %>%
#   select(-moved_in, -moved_in_se, -moved_out, -moved_out_se) %>% 
#   gather(direction, mig, pct_moved_in, pct_moved_out) %>%
#   mutate(se = ifelse(direction=="pct_moved_in", pct_moved_in_se, pct_moved_out_se)) %>%
#   select(-pct_moved_in_se, -pct_moved_out_se) %>%
#   mutate(geogroup = factor(geogroup, levels=c("Ramsey","Hennepin","Other Metro Counties","Greater Minnesota")),
#          direction = factor(direction, levels=c("pct_moved_out", "pct_moved_in")))
# 
# 
# barplot24_pct_errorbars <- filter(migration_percents_long, agegroup=="24 to 30") %>%
#   ggplot(aes(x=geogroup, y=mig, fill=direction)) +
#   geom_bar(stat="identity", position="dodge") 
# 
# 
# +
#   geom_errorbar(aes(ymin=mig-se, ymax=mig+se), 
#                 width = .2,
#                 position=position_dodge(.9))+
#   theme(
#     panel.background = element_blank(),
#     axis.ticks.x=element_blank(),
#     axis.ticks.y=element_blank(),
#     axis.title.x=element_blank(),
#     axis.title.y=element_blank(),
#     axis.text.x=element_text(size=10, color="black"),
#     axis.text.y=element_blank(),
#     legend.title=element_blank(),
#     legend.position="bottom",
#     plot.caption=element_text(size=8, hjust=.5),
#     plot.title = element_text(hjust = 0.5)
#   ) +
#   scale_y_continuous(labels=scales::comma) +
#   scale_fill_discrete(labels=c("Left MN for Another State", "Moved to MN from Another State")) +
#   labs(title = "Average Annual Migration between Minnesota and Other States, 2011-2015\nPersons Aged 24 to 30",
#        caption = "Source: House Research. 2015 American Community Survey 5-year Estimates. IPUMS-USA, University of Minnesota.") 
# 
# 
# ggsave("ages24to30_errorbars.png", barplot24_errorbars,width=8,height=6)