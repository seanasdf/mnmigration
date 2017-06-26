library(dplyr)
library(readr)
library(purrr)
library(srvyr)

######################################
######### Migration to MN ############
######################################

#using srvyr
inmigration <- read_csv("usa_00010.csv.gz") %>%
      mutate(geogroup =ifelse(PUMARES2MIG==14, "Hennepin", 
                              ifelse(PUMARES2MIG==13, "Ramsey", "All Other Counties")),
             agegroup = ifelse(AGE<17, "16 and Younger",
                               ifelse(AGE>=17 & AGE<24, "17 to 23",
                                      ifelse(AGE>=24 & AGE<31, "24 to 30", "31 and Over")
                                      )
                               ) 
             ) %>%
      as_survey(weights=PERWT) %>%
      group_by(geogroup, agegroup) %>%
      summarise(moved_in = survey_total(!(MIGPLAC1 %in% c(0,27)) & MIGPLAC1<100))


######################################
######### Migration from MN ##########
######################################

outmigration <- read_csv("usa_00009.csv.gz") %>%
      filter(STATEFIP != 27) %>%
      mutate(geogroup =ifelse(MIGPUMA1==1400, "Hennepin", 
                              ifelse(MIGPUMA1==1300, "Ramsey", "All Other Counties")),
             agegroup = ifelse(AGE<17, "16 and Younger",
                               ifelse(AGE>=17 & AGE<24, "17 to 23",
                                      ifelse(AGE>=24 & AGE<31, "24 to 30", "31 and Over")
                               )
             )) %>%
      as_survey(weights=PERWT)  %>%
      group_by(geogroup, agegroup) %>%
      summarise(moved_out = survey_total())

######################################
######### Combine and Analyze ########
######################################
netmig <- left_join(inmigration, outmigration) %>%
      mutate(net_migration = moved_in-moved_out) %>%
      select(-moved_in_se, -moved_out_se)

write.csv(netmig, "netmigration.csv")

library(tidyr)
netmig_long <- left_join(inmigration, outmigration) %>%
      gather(direction, mig, moved_in, moved_out) %>%
      mutate(se = ifelse(direction=="moved_in", moved_in_se, moved_out_se)) %>%
      select(-moved_in_se, -moved_out_se) %>%
      mutate(geogroup = factor(geogroup, levels=c("Hennepin","Ramsey", "All Other Counties")),
             direction = factor(direction, levels=c("moved_out", "moved_in")))


library(ggplot2)
barplot17 <- filter(netmig_long, agegroup=="17 to 23") %>%
      ggplot(aes(x=geogroup, y=mig, fill=direction)) +
      geom_bar(stat="identity", position="dodge") +
      geom_text(aes(x=geogroup, y=mig, label=scales::comma(mig)),
                position=position_dodge(width=0.9), vjust=-0.25) +
      theme(
            panel.background = element_blank(),
            axis.ticks.x=element_blank(),
            axis.ticks.y=element_blank(),
            axis.title.x=element_blank(),
            axis.title.y=element_blank(),
            axis.text.x=element_text(size=10, color="black"),
            axis.text.y=element_blank(),
            legend.title=element_blank(),
            legend.position="bottom",
            plot.caption=element_text(size=8, hjust=.5),
            plot.title = element_text(hjust = 0.5)
      ) +
      scale_y_continuous(labels=scales::comma) +
      scale_fill_discrete(labels=c("Left MN for Another State", "Moved to MN from Another State")) +
      labs(title = "Average Annual Migration between Minnesota and Other States, 2011-2015\nPersons Aged 17 to 23",
           caption = "Source: House Research. 2015 American Community Survey 5-year Estimates. IPUMS-USA, University of Minnesota.") 
ggsave("ages17to23.png", barplot17,width=8,height=6)

library(ggplot2)
barplot24 <- filter(netmig_long, agegroup=="24 to 30") %>%
      ggplot(aes(x=geogroup, y=mig, fill=direction)) +
      geom_bar(stat="identity", position="dodge") +
      geom_text(aes(x=geogroup, y=mig, label=scales::comma(mig)),
                position=position_dodge(width=0.9), vjust=-0.25) +
      theme(
            panel.background = element_blank(),
            axis.ticks.x=element_blank(),
            axis.ticks.y=element_blank(),
            axis.title.x=element_blank(),
            axis.title.y=element_blank(),
            axis.text.x=element_text(size=10, color="black"),
            axis.text.y=element_blank(),
            legend.title=element_blank(),
            legend.position="bottom",
            plot.caption=element_text(size=8, hjust=.5),
            plot.title = element_text(hjust = 0.5)
      ) +
      scale_y_continuous(labels=scales::comma) +
      scale_fill_discrete(labels=c("Left MN for Another State", "Moved to MN from Another State")) +
      labs(title = "Average Annual Migration between Minnesota and Other States, 2011-2015\nPersons Aged 24 to 30",
           caption = "Source: House Research. 2015 American Community Survey 5-year Estimates. IPUMS-USA, University of Minnesota.") 
ggsave("ages24to30.png", barplot24,width=8,height=6)