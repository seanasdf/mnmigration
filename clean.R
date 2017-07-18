library(tidyverse)
library(readxl)


######################################
######### Migration within ###########
######### Minnesota ##################
######################################

#read in crosswalk for years and PUMAs to Geographic Groupings
pumacrosswalk <- read_excel("./crosswalk/crosswalk_puma_to_mncounty.xlsx") 
names(pumacrosswalk)[3] <- "geogroup_residing"

#read in crosswalk for years and MIGPUMAs to Geographic Groupings
migpumacrosswalk <- read_excel("./crosswalk/migpumacrosswalk.xlsx",
                               sheet = "MigPUMAtoGeogroup") 
names(migpumacrosswalk)[3] <- "geogroup_migration"


#read in extract from IPUMS and join with crosswalk to geographic groupings
in_state <- read_csv("usa_00018.csv.gz") 

in_state <- in_state %>%  
  left_join(pumacrosswalk, by=c("PUMA"="PUMA", "MULTYEAR"="Year")) %>% 
  left_join(migpumacrosswalk, by=c("MIGPUMA1"="MIGPUMA", "MULTYEAR"="Year")) %>%
  mutate(agegroup = case_when(
    AGE<18 ~ "17 and Younger",
    AGE>=18 & AGE<24 ~ "18 to 23", 
    AGE>=24 & AGE<30 ~ "24 to 29",
    AGE>=30 ~ "30 and Over"
    ),
    moved_instate = ifelse(MIGPLAC1 == 27, 1, 0)
  )

save(in_state, file="instate.rda")

######################################
######### Migration to MN   ##########
######### From Other States ##########
######################################

#read in crosswalk for years and PUMAs to Geographic Groupings
pumacrosswalk <- read_excel("./crosswalk/crosswalk_puma_to_mncounty.xlsx") 
names(pumacrosswalk)[3] <- "geogroup"

#read in extract from IPUMS and join with crosswalk to geographic groupings
inmigration <- read_csv("usa_00018.csv.gz") %>% 
  left_join(pumacrosswalk, by=c("PUMA"="PUMA", "MULTYEAR"="Year")) %>%
  mutate(agegroup = case_when(
    AGE<18 ~ "17 and Younger",
    AGE>=18 & AGE<24 ~ "18 to 23", 
    AGE>=24 & AGE<30 ~ "24 to 29",
    AGE>=30 ~ "30 and Over"
    ))

save(inmigration, file="inmigration.rda")


######################################
######### Migration from MN ##########
######### to Other States ############
######################################

#read in crosswalk for years and MIGPUMAs to Geographic Groupings
migpumacrosswalk <- read_excel("./crosswalk/migpumacrosswalk.xlsx",
                               sheet = "MigPUMAtoGeogroup") 


outmigration <- read_csv("usa_00017.csv.gz") %>%
  filter(STATEFIP != 27) %>%
  left_join(migpumacrosswalk, by=c("MIGPUMA1"="MIGPUMA", "MULTYEAR"="Year")) %>%
  mutate(agegroup = case_when(
    AGE<18 ~ "17 and Younger",
    AGE>=18 & AGE<24 ~ "18 to 23", 
    AGE>=24 & AGE<30 ~ "24 to 29",
    AGE>=30 ~ "30 and Over"
  ))

save(outmigration, file="outmigration.rda")

