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

save(bp_inmigration_long, file="./caches/birthplace.rda")


