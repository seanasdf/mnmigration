library(tidyverse)
library(srvyr)

######################################
######### Migration to MN   ##########
######### From Other States ##########
######################################

#load cached dataframe of birthplace migration analysis, if it exists.
if (file.exists("./caches/birtplace_mig.rda")) {
  load("./caches/birtplace_mig.rda")
} else {
  
  #read in inmigration data
  if (file.exists("./caches/inmigration.rda")) {
    load("./caches/inmigration.rda")
  } else {
    source("clean.R")
  }
  
  # for people who moved to mn, get birthplace
  bp_inmigration <- inmigration %>%
    #create dummy variable to identify people who moved
    mutate(moved_states = ifelse(!(MIGPLAC1 %in% c(0,27)) & MIGPLAC1<100, 1, 0),
           birthplace = case_when(BPL == 27 ~ "Minnesota",
                                  BPL <150 & BPL != 27 ~ "Another State/Territory",
                                  BPL >= 150 & BPL <= 900 ~ "Another Country",
                                  BPL > 900 ~ "Other/Missing"),
           birthplace = as.factor(birthplace)) %>%
    #rename replicate weights flag so it doesn't get used as a replicate weight
    rename(repwtflag = REPWTP) %>% 
    as_survey_rep(type="BRR", repweights=starts_with("REPWTP"), weights=PERWT) %>%
    group_by(geogroup, agegroup, moved_states) %>% 
    summarise(pct_mnborn = survey_mean(birthplace=="Minnesota", proportion = TRUE, vartype = "ci", level=.9),
              pct_otherstate = survey_mean(birthplace=="Another State/Territory", proportion = TRUE, vartype = "ci", level=.9),
              pct_fb = survey_mean(birthplace=="Another Country", proportion = TRUE, vartype = "ci", level=.9),
              pct_other = survey_mean(birthplace=="Other/Missing", proportion = TRUE, vartype = "ci", level=.9)
    ) %>% 
    filter(moved_states==1)
  
  save(bp_inmigration, file =  "./caches/birtplace_mig.rda")
}


#make it long and organize it for graphing
bp_inmigration_long <- bp_inmigration %>% 
  gather(key=variable, value=value, pct_mnborn, pct_otherstate, pct_fb, pct_other) %>% 
  mutate(ci_bottom = case_when(variable=="pct_mnborn" ~ pct_mnborn_low,
                            variable=="pct_otherstate" ~ pct_otherstate_low,
                            variable=="pct_fb" ~ pct_fb_low,
                            variable=="pct_other" ~ pct_other_low),
         ci_top = case_when(variable=="pct_mnborn" ~ pct_mnborn_upp,
                             variable=="pct_otherstate" ~ pct_otherstate_upp,
                             variable=="pct_fb" ~ pct_fb_upp,
                             variable=="pct_other" ~ pct_other_upp)) %>% 
  select(-matches("_low"),-matches("_upp")) %>% 
  mutate(variable = factor(variable, 
                           levels = c("pct_mnborn",
                                      "pct_otherstate",
                                      "pct_fb",
                                      "pct_other")),
         geogroup = case_when(geogroup=="Greater MN" ~ "Greater Minnesota", 
                              geogroup=="Metro" ~ "Other Metro Counties",
                              TRUE ~ geogroup),
         geogroup = factor(geogroup, levels=c("Ramsey","Hennepin","Other Metro Counties","Greater Minnesota"))
  ) %>% 
  rename(Birthplace=variable)


######################################
######### Graph Birthplace  ##########
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
  axis.title.y=element_blank(),
  axis.text.x=migration_text,
  axis.text.y=migration_text,
  legend.text=migration_text,
  plot.caption = migration_text,
  legend.title=migration_text,
  legend.position="bottom",
  plot.title =migration_text,
  panel.grid.major.y = element_blank(),
  panel.grid.major.x = element_blank()
) + 
  theme(plot.title = element_text(size=36, hjust = 0.5),
        plot.caption = element_text(size=24))


bp_24 <- 
  filter(bp_inmigration_long, agegroup=="24 to 29" & Birthplace !="pct_other") %>%
  mutate(Birthplace = factor(Birthplace, levels=rev(levels(Birthplace)))) %>%
  arrange(geogroup, Birthplace) %>% 
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
            size =10) +
  scale_y_continuous(labels=scales::percent) +
  theme_migration +
  labs(title = "Place of Birth of 23-29 Year-olds who Moved to Minnesota from Another State, 2011-2015",
       y="Percent of 18-23 Year-olds who Moved",
       x="",
       caption = "2015 American Community Survey 5-year Estimates. IPUMS-USA, University of Minnesota.")



bp_24
              
  
ggsave("./plots/bp_24.png", bp_24,width=8,height=6) 

