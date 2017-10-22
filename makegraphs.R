
######################################
##### Graph Migration Rates  #########
######################################

load("./caches/inout_byregion.rda")

if (!exists("theme_migration")) {
  source("theme.R")
}


####Migration rate for 18 to 21 year olds####
netmig18_pct <- filter(pct_inout_byregion, agegroup=="18 to 21") %>%
  ggplot(aes(x=geogroup, y=mig, fill=direction)) +
  theme_migration +
  geom_bar(stat="identity", position="dodge") +
  scale_fill_brewer(labels=c("Left MN for Another State", "Moved to MN from Another State"), 
                    palette="Set1")+
  labs(title = "Figure 5: Average Annual Migration between Minnesota and Other States, 2011-2015\nIndividuals Ages 18 to 21",
       caption = caption_witherrors,
       y="Individuals Moving per 1,000 Individuals Ages 18 to 21") +
  geom_errorbar(aes(ymin=mig-1.645*se, ymax=mig+1.645*se), 
                width = .2,
                position=position_dodge(.9))

ggsave("./plots/netmig18_pct.png", netmig18_pct,width=8,height=6) 

####Migration rate for 22 to 29 year olds####
netmig22_pct <- filter(pct_inout_byregion, agegroup=="22 to 29") %>%
  ggplot(aes(x=geogroup, y=mig, fill=direction)) +
  geom_bar(stat="identity", position="dodge") +
  theme_migration +
  scale_fill_brewer(labels=c("Left MN for Another State", "Moved to MN from Another State"), 
                    palette="Set1")+
  labs(title = "Figure 6: Average Annual Migration between Minnesota and Other States, 2011-2015\nIndividuals Ages 22 to 29",
       caption = caption_witherrors,
       y="Individuals Moving per 1,000 Individuals Ages 22 to 29") +
  geom_errorbar(aes(ymin=mig-1.645*se, ymax=mig+1.645*se), 
                width = .2,
                position=position_dodge(.9))


ggsave("./plots/netmig22_pct.png", netmig22_pct,width=8,height=6) 


####Total Migration for 18 to 21 year olds####
netmig18_tot <- filter(total_inout_byregion, agegroup=="18 to 21") %>%
  ggplot(aes(x=geogroup, y=mig, fill=direction)) +
  theme_migration +
  geom_bar(stat="identity", position="dodge") +
  scale_fill_brewer(labels=c("Left MN for Another State", "Moved to MN from Another State"), 
                    palette="Set1")+
  labs(title = "Average Annual Migration between Minnesota and Other States, 2011-2015\nIndividuals Ages 18 to 21",
       caption = caption_witherrors,
       y="Total Individuals Ages 18 to 21 Who Moved") +
  scale_y_continuous(labels=scales::comma) +
  geom_errorbar(aes(ymin=mig-1.645*se, ymax=mig+1.645*se), 
                width = .2,
                position=position_dodge(.9))

ggsave("./plots/netmig18_tot.png", netmig18_tot,width=8,height=6) 

####Total migration for 22 to 29 year olds####
netmig22_tot <- filter(total_inout_byregion, agegroup=="22 to 29") %>%
  ggplot(aes(x=geogroup, y=mig, fill=direction)) +
  theme_migration +
  geom_bar(stat="identity", position="dodge") +
  scale_fill_brewer(labels=c("Left MN for Another State", "Moved to MN from Another State"), 
                    palette="Set1")+
  labs(title = "Average Annual Migration between Minnesota and Other States, 2011-2015\nIndividuals Ages 22 to 29",
       caption = caption_witherrors,
       y="Total Individuals Ages 22 to 29 Who Moved") +
  scale_y_continuous(labels=scales::comma) +
  geom_errorbar(aes(ymin=mig-1.645*se, ymax=mig+1.645*se), 
                width = .2,
                position=position_dodge(.9))


ggsave("./plots/netmig22_tot.png", netmig22_tot,width=8,height=6) 

######################################
### Statewide In- and outmigration ###
######################################

load("./caches/stateinout.rda")

#run theme script if necessary
if (!exists("theme_migration")) {
  source("theme.R")
}
#Get statewide line graph of migration by age.
state_linegraph <- state_inout_byage %>%
  mutate(AGE = as.numeric(AGE)) %>% 
  filter(AGE > 0 & AGE < 91 ) %>% 
  ggplot(aes(x=AGE, y=mig, group=direction, colour=direction)) +
  geom_line() +
  theme_migration +
  theme(strip.text.x = migration_text,
        strip.background=element_rect(color="white", fill="white"),
        axis.title.x=migration_text) +
  expand_limits(y=0) +
  scale_color_brewer(labels=c("Left MN for Another State", "Moved to MN from Another State"), 
                     palette="Set1") +
  labs(title = "Figure 1: Average Annual Migration between Minnesota and Other States by Age, 2011-2015",
       caption = caption_noerrors,
       y="Individuals Migrating per 1,000 Indidivuals of Given Age",
       x="Age") +
  scale_y_continuous(labels=scales::comma) 

ggsave("./plots/statelinegraph.png", state_linegraph,width=8,height=6) 

#limit line graph to migration of young people
state_linegraph_young <- state_inout_byage %>% 
  mutate(AGE = as.numeric(AGE)) %>% 
  filter(AGE>17 & AGE<30) %>% 
  ggplot(aes(x=AGE, y=mig, group=direction, colour=direction)) +
  geom_point(shape=1) +
  geom_line() +
  theme_migration +
  theme(strip.text.x = migration_text,
        strip.background=element_rect(color="white", fill="white"),
        axis.title.x=migration_text) +
  expand_limits(y=0) +
  scale_color_brewer(labels=c("Left MN for Another State", "Moved to MN from Another State"), 
                     palette="Set1") +
  labs(title = "Figure 2: Average Annual Migration between Minnesota and Other States, 2011-2015",
       caption = caption_noerrors,
       y="Individuals Migrating per 1,000 Indidivuals of Given Age",
       x="Age") +
  scale_y_continuous(labels=scales::comma) +
  scale_x_continuous(limits = c(18,29),breaks = c(18,20,22,24,26,28))

ggsave("./plots/statelinegraph_young.png", state_linegraph_young,width=8,height=6) 

#create another graph--with error bars
state_linegraph_young_witherrors <- state_linegraph_young +
  geom_errorbar(aes(ymin=mig-1.645*se, ymax=mig+1.645*se), 
                width = .2) +
  labs(caption = caption_witherrors)
ggsave("./plots/state_linegraph_young_witherrors.png", state_linegraph_young_witherrors,width=8,height=6) 

######################################
### Regional in- and outmigration ####
######################################
load("./caches/regionalinout.rda")

#run theme script if necessary
if (!exists("theme_migration")) {
  source("theme.R")
}

#actually graph the thing
#first, broken out by region
linegraphs <- regional_inout %>% 
  mutate(AGE = as.numeric(AGE)) %>% 
  filter(AGE > 17 & AGE < 30) %>%
  mutate(mig = mig*1000, se=se*1000) %>% 
  ggplot(aes(x=AGE, y=mig, group=direction, colour=direction)) +
  geom_point(shape=1) +
  geom_line() +
  facet_wrap(~geogroup, ncol=2) +
  theme_migration +
  theme(strip.text.x = migration_text,
        strip.background=element_rect(color="white", fill="white")) +
  scale_color_brewer(labels=c("Left MN for Another State", "Moved to MN from Another State"), 
                     palette="Set1") +
  labs(title = "Figure 4: Average Annual Migration between Minnesota and Other States by Age and Region, 2011-2015",
       caption =caption_noerrors,
       y="Individuals Migrating per 1,000 Indidivuals of Given Age",
       x="Age") +
  scale_y_continuous(labels=scales::comma) +
  scale_x_continuous(limits = c(18,29),breaks = c(18,20,22,24,26,28))

ggsave("./plots/linegraphs.png", linegraphs,width=8,height=6) 


#second, create another graph--with error bars
linegraphs_witherrors <- linegraphs +
  geom_errorbar(aes(ymin=mig-1.645*se, ymax=mig+1.645*se), 
                width = 0) +
  labs(caption = caption_witherrors)
ggsave("./plots/linegraphs_errorbars.png", linegraphs_witherrors,width=8,height=6) 



######################################
####### Statewide Net Migration ######
######################################
load("./caches/netmig_mn.rda")

#run theme script if necessary
if (!exists("theme_migration")) {
  source("theme.R")
}

#Statewide net migration graph, all ages
netmig_linegraph <- netmig_mn %>% 
  mutate(AGE = as.numeric(AGE),
         posneg = ifelse(netmig>0, "Positive", "Negative")) %>% 
  ggplot(aes(x=AGE, y=netmig, color=posneg)) +
  scale_color_brewer(palette="Set1") +
  geom_point() +
  geom_errorbar(aes(ymin=netmig-1.645*se, ymax=netmig+1.645*se), 
                width = 0) +
  theme_migration +
  labs(title = "Average Annual Net Migration between Minnesota and Other States by Age, 2011-2015",
       caption = caption_noerrors,
       x="Age") +
  scale_y_continuous(labels=scales::comma, limits = c(-4500,4500), breaks = c(-4000, -2000,0, 2000, 4000)) +
  guides(fill=FALSE) +
  theme(legend.position="none")


ggsave("./plots/netmig.png", netmig_linegraph,width=8,height=6) 



#Statewide net migration graph, young people only
netmig_linegraph_young <- netmig_mn %>% 
  mutate(AGE = as.numeric(AGE),
         posneg = ifelse(netmig>0, "Positive", "Negative")) %>% 
  filter(AGE > 17 & AGE < 30) %>% 
  ggplot(aes(x=AGE, y=netmig, color=posneg)) +
  scale_color_brewer(palette="Set1") +
  geom_point() +
  geom_errorbar(aes(ymin=netmig-1.645*se, ymax=netmig+1.645*se), 
                width = 0) +
  theme_migration +
  labs(title = "Figure 3: Average Annual Net Migration between Minnesota and Other States by Age, 2011-2015",
       caption = caption_witherrors,
       y="Net Migration of Individuals of Given Age",
       x="Age") +
  scale_y_continuous(labels=scales::comma, limits = c(-4500,4500), breaks = c(-4000, -2000,0, 2000, 4000)) +
  guides(fill=FALSE) +
  theme(legend.position="none",
        axis.text.x = migration_text) +
  scale_x_continuous(limits = c(18,29),breaks = c(18,20,22,24,26,28)) +
  geom_hline(yintercept = 0, size=.3)


ggsave("./plots/netmig_young.png", netmig_linegraph_young,width=8,height=6) 


######################################
#####Regional Percent Migration#######
######################################
load("./caches/netmig_regions.rda")

#run theme script if necessary
if (!exists("theme_migration")) {
  source("theme.R")
}

#Statewide net migration graph, all ages
regional_netmig_linegraph <- netmig_regions %>% 
  mutate(AGE = as.numeric(AGE),
         posneg = ifelse(netmig>0, "Positive", "Negative")) %>%
  filter(AGE > 17 & AGE <31) %>% 
  ggplot(aes(x=AGE, y=netmig, color=posneg)) +
  geom_point() +
  scale_color_brewer(palette="Set1") +
  geom_errorbar(aes(ymin=netmig-1.645*se, ymax=netmig+1.645*se), 
                width = 0) +
  facet_wrap(~geogroup, ncol=2) +
  theme(strip.text.x = migration_text,
        strip.background=element_rect(color="white", fill="white")) +
  theme_migration +
  labs(title = "Average Annual Net Migration between Minnesota and Other States by Age, 2011-2015",
       caption = "2015 American Community Survey 5-year Estimates. IPUMS-USA, University of Minnesota.
       Source: MN House Research. Error bars represent 90% confidence intervals.",
       y="Net Migration of Individuals of Given Age",
       x="Age") +
  scale_y_continuous(labels=scales::comma, limits = c(-2500,2500), breaks = c(-2000,-1000,0,1000,2000)) +
  guides(fill=FALSE) +
  scale_x_continuous(limits = c(18,29),breaks = c(18,20,22,24,26,28))

ggsave("./plots/netmig_byregion.png", regional_netmig_linegraph,width=8,height=6) 

######################################
##### Agregroup Net Migration ########
######################################
load("./caches/netmig_agegroup.rda")

#run theme script if necessary
if (!exists("theme_migration")) {
  source("theme.R")
}


#rename statewide add linebreak to geogroup labels for later
levels(netmigration_age$geogroup) <- gsub("Statewide", "Statewide Total", levels(netmigration_age$geogroup))
levels(netmigration_age$geogroup) <- gsub(" ", "\n", levels(netmigration_age$geogroup))


netmig_regions <- netmigration_age %>%
  mutate(posneg = ifelse(netmig>=0, "pos", "neg"),
         errortop = netmig + 1.645*se,
         errorbot = netmig - 1.645*se) %>% 
  ggplot(aes(x=geogroup, y=netmig, fill=(geogroup=="Statewide\nTotal"))) +
  geom_bar(stat="identity") +
  facet_wrap(~agegroup, 
             labeller = labeller(agegroup = c("18 to 21" = "18 to 21-year-olds", 
                                              "22 to 29" = "22 to 29-year-olds"))) +
  geom_errorbar(aes(ymin=errorbot, ymax=errortop), 
                width = .1) +
  theme_migration +
  theme(plot.title = element_text(size=32, hjust = 0.5),
        strip.text.x =  element_text(family="roboto", 
                                     size=28, 
                                     face="plain", 
                                     color="black",
                                     lineheight = 0.4),
        strip.background=element_rect(color="white", fill="white"),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(color="#d9d9d9"),
        axis.text.x = element_text(family="roboto", 
                                   size=24, 
                                   face="plain", 
                                   color="black"),
        plot.caption= element_text(size=20))+
  labs(title = "Figure 7: Total Net Migration Between Minnesota and Other States, 2011-2015",
       y="",
       x="",
       caption = paste0("\n",caption_witherrors)) +
  scale_fill_manual(values = c("#377eb8", "#4daf4a"),
                    labels = c("Geographic Subgroup", "Statewide Total")) +
  geom_text(aes(label=ifelse(netmig>0,scales::comma(netmig),""), y=errortop), 
            vjust = -1,
            family="roboto", 
            size=10, 
            color="black") +
  geom_text(aes(label=ifelse(netmig<0,scales::comma(netmig),""), y=errorbot), 
            vjust = 1.7,
            family="roboto", 
            size=10, 
            color="black") +
  scale_y_continuous(labels=scales::comma) 


ggsave("./plots/netmig_agegroup.png", netmig_regions,width=8,height=6) 

######################################
####### Graph Student Status #########
######################################
load("./caches/students.rda")

#run theme script if necessary
if (!exists("theme_migration")) {
  source("theme.R")
}

student_18 <- filter(student_migration, agegroup=="18 to 21") %>%
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
  scale_y_continuous(labels=scales::percent,
                     limits=c(0,1)) +
  geom_errorbar(aes(ymin=lower_error, ymax=upper_error), 
                width = .2,
                position=position_dodge(.9)) +
  labs(title = "Figure 8: Share of 18 to 21-year-olds Moving to and From Minnesota that Were Students",
       y="Percent of 18 to 21-year-olds who Moved that Were Students",
       x="",
       caption = caption_witherrors)

ggsave("./plots/students_18.png", student_18,width=8,height=6) 

student_22 <- filter(student_migration, agegroup=="22 to 29") %>%
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
  scale_y_continuous(labels=scales::percent,
                     limits=c(0,1)) +
  geom_errorbar(aes(ymin=lower_error, ymax=upper_error), 
                width = .2,
                position=position_dodge(.9)) +
  labs(title = "Figure 9: Share of 22 to 29-year-olds Moving to and From Minnesota that Were Students",
       y="Percent of 22 to 29-year-olds who Moved that Were Students",
       x="",
       caption = caption_witherrors)

ggsave("./plots/student_22.png", student_22,width=8,height=6) 


######################################
######### Graph Birthplace  ##########
######################################
load("./caches/birthplace.rda")

#run theme script if necessary
if (!exists("theme_migration")) {
  source("theme.R")
}

bp_22 <- 
  filter(bp_inmigration_long, agegroup=="22 to 29" & Birthplace !="pct_other") %>%
  mutate(Birthplace = fct_relevel(Birthplace,c("Ramsey",
                                               "Hennepin",
                                               "Other Metro Counties",
                                               "Greater Minnesota"))) %>%
  ggplot(aes(x=geogroup,
             y=value, 
             fill=Birthplace)) +
  coord_flip() +
  scale_fill_brewer(palette = "Set1",
                    guide = guide_legend(reverse = TRUE,
                                         title = "Place of Birth"),
                    labels = c("Another Country",
                               "Another State/Territory",
                               "Minnesota")) +
  geom_bar(stat="identity", position="stack") +
  geom_text(aes(label = ifelse(value>=.05, paste0(round(value*100,1),'%'), "")), 
            position=position_stack(vjust=0.5),
            family="roboto", 
            size=12) +
  scale_y_continuous(labels=scales::percent) +
  theme_migration +
  theme(panel.grid.major.y = element_blank(),
        panel.grid.major.x = element_blank(),
        axis.title.x = migration_text,
        axis.title = element_text(size=32)) +
  labs(title = "Figure 10: Birthplace of 22 to 29-year-olds who\nMigrated to Minnesota from Another State, 2011-2015",
       y="\nPercent of 18 to 22-year-olds who Moved",
       x="",
       caption = caption_noerrors) 

ggsave("./plots/bp_22.png", bp_22,width=8,height=6) 

######################################
#### Graph Country of Birth for FB####
######################################
load("./caches/bp_foreignborn.rda")

#run theme script if necessary
if (!exists("theme_migration")) {
  source("theme.R")
}

bp_foreignborn_graph <- 
    bp_foreignborn_pct %>% 
    mutate(errortop = pct_of_movers + 1.645*pct_of_movers_se,
           errorbot = pct_of_movers - 1.645*pct_of_movers_se, 
           Birthplace = ifelse(Birthplace=="Cambodia (Kampuchea)", "Cambodia", Birthplace),
           Birthplace = fct_reorder(as.factor(Birthplace), pct_of_movers)) %>% 
    arrange(-pct_of_movers) %>% 
    filter(row_number() < 16) %>% 
    ggplot(aes(x=Birthplace,
               y=pct_of_movers)) +
    geom_bar(fill = "#377eb8", 
             stat="identity") +
    coord_flip() +
    theme_migration +
    theme(
      panel.grid.major.x = element_line(color="#d9d9d9"),
      panel.grid.major.y = element_blank()
    ) +
    scale_y_continuous(labels=scales::percent,
                       limits = c(0,.38)) +
    geom_errorbar(aes(ymin=errorbot, ymax=errortop), 
                  width = .1) +
    geom_text(aes(label=scales::percent(pct_of_movers), y=errortop), 
              hjust = -.15,
              family="roboto", 
              size=10, 
              color="black") +
    labs(title = "Figure 11: Birthplace of Foreign-born 22 to 29-year-olds\n who Moved to Minnesota From Another State, 2011-2015",
       y="\nPercent of Foreign Born Individual who Moved",
       x="",
       caption = caption_witherrors) 
  
  ggsave("./plots/bp_foreignborn.png", bp_foreignborn_graph,width=8,height=6) 

######################################
####### Graph Educ. Attainment #######
######################################
load("./caches/educresults.rda")

#run theme script if necessary
if (!exists("theme_migration")) {
  source("theme.R")
}

#Graph education levels for 22 to 29 year olds
educ_22 <- filter(educ_migration, 
                  agegroup=="22 to 29" & variable !="Missing/NA")  %>%
  mutate(direction = ifelse(direction=="Inmigration", "Moved to Minnesota", "Moved from Minnesota")) %>% 
  ggplot(aes(x=direction,
             y=value,
             fill=variable)) +
  theme_migration +
  theme(strip.text.x = migration_text,
        strip.background=element_rect(color="white", fill="white"),
        panel.grid.major.y = element_blank(),
        panel.grid.major.x = element_blank(),
        axis.ticks.x=element_blank(),
        axis.ticks.y=element_blank(),
        axis.text.y=element_blank(),
        axis.text.x=element_text(size=30)) +
  geom_bar(stat="identity",position="fill") +
  #coord_flip() +
  scale_fill_brewer(palette="Paired") +
  scale_y_continuous(labels=scales::percent,
                     trans = "reverse") +
  facet_wrap(~geogroup, ncol=2) +
  labs(title = "Figure 12: Educational Attainment of 22 to 29-year-olds who Migated between Minnesota and Another State",
       y="Percent of 22 to 29-year-olds who Moved",
       x="",
       caption = caption_noerrors) +
  geom_text(aes(label = ifelse(value>=.06, paste0(round(value*100,1),'%'), "")), 
            position=position_stack(vjust=0.5),
            size =10) 

ggsave("./plots/educ_22.png", educ_22,width=8,height=6) 

######################################
######### Graph top states ###########
######################################
load("statetop10.rda")

#run theme script if necessary
if (!exists("theme_migration")) {
  source("theme.R")
}

states_18_outmigration <- filter(top_states, 
                                 agegroup=="18 to 21") %>%
  mutate(barlabel=paste0(statename,
                         ", " ,
                         as.character(format(pct*100,digits=2)),
                         "%")) %>% 
  ggplot(aes(x=reorder(rank, pct),
             y=pct,
             fill=rev(direction))) +
  geom_bar(stat="identity") +
  coord_flip() +
  facet_grid(direction~geogroup) +
  geom_text(aes(label=ifelse(pct<.25, barlabel,"")),
            size=11,
            hjust=-.02) +
  geom_text(aes(label=ifelse(pct>=.25, barlabel,"")),
            size=11,
            hjust=1.02,
            color="white") +
  theme_migration +
  theme(plot.title = element_text(size=48, hjust = 0.5),
        plot.subtitle = element_text(size=36, hjust = 0.5),
        plot.caption = element_text(size=24),
        strip.text.x = element_text(size=36),
        axis.ticks.x=element_blank(),
        axis.ticks.y=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.major.x = element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        strip.background=element_rect(color="white", fill="white"),
        strip.text.y = element_blank()) +
  labs(caption = caption_noerrors,
       title="Figure 13: Top 10 States for Migration to and from Minnesota Regions, 2011-2015",
       subtitle="Share of 18-21 Year Olds Who Moved") +
  scale_fill_brewer(guide = guide_legend(reverse=TRUE),
                    palette = "Set1")


ggsave("./plots/states_18.png", states_18_outmigration,width=11,height=8.5) 


states_22_outmigration <- filter(top_states, 
                                 agegroup=="22 to 29") %>%
  mutate(barlabel=paste0(statename,
                         ", " ,
                         as.character(format(pct*100,digits=2)),
                         "%")) %>% 
  ggplot(aes(x=reorder(rank, pct),
             y=pct,
             fill=rev(direction))) +
  geom_bar(stat="identity") +
  coord_flip() +
  facet_grid(direction~geogroup) +
  geom_text(aes(label=ifelse(pct<.15, barlabel,"")),
            size=11,
            hjust=-.02) +
  geom_text(aes(label=ifelse(pct>=.15, barlabel,"")),
            size=11,
            hjust=1.02,
            color="white") +
  theme_migration +
  theme(plot.title = element_text(size=48, hjust = 0.5),
        plot.subtitle = element_text(size=36, hjust = 0.5),
        plot.caption = element_text(size=24),
        strip.text.x = element_text(size=36),
        axis.ticks.x=element_blank(),
        axis.ticks.y=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.major.x = element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        strip.background=element_rect(color="white", fill="white"),
        strip.text.y = element_blank()) +
  labs(caption = caption_noerrors,
       title="Figure 14: Top 10 States for Migration to and from Minnesota Regions, 2011-2015",
       subtitle="Share of 22 to 29 Year Olds Who Moved") +
  scale_fill_brewer(guide = guide_legend(reverse=TRUE),
                    palette = "Set1")


ggsave("./plots/states_22.png", states_22_outmigration,width=11,height=8.5) 
