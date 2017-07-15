library(tidyr)
library(readxl)


######################################
######### Migration to MN   ##########
######### From Other States ##########
######################################

#read in crosswalk for years and PUMAs to Geographic Groupings
pumacrosswalk <- read_excel("./crosswalk/crosswalk_puma_to_mncounty.xlsx") 
names(pumacrosswalk)[3] <- "geogroup"

#read in extract from IPUMS and join with crosswalk to geographic groupings
inmigration <- read_csv("usa_00011.csv.gz") %>% 
  left_join(pumacrosswalk, by=c("PUMA"="PUMA", "MULTYEAR"="Year")) %>%
  mutate(agegroup = case_when(
    AGE<17 ~ "16 and Younger",
    AGE>=17 & AGE<24 ~ "17 to 23", 
    AGE>=24 & AGE<31 ~ "24 to 30",
    AGE>=31 ~ "31 and Over"
  ))

save(inmigration, file="inmigration.rda")


######################################
######### Migration from MN ##########
######### to Other States ############
######################################

#read in crosswalk for years and MIGPUMAs to Geographic Groupings
migpumacrosswalk <- read_excel("./crosswalk/migpumacrosswalk.xlsx",
                               sheet = "MigPUMAtoGeogroup") 


outmigration <- read_csv("usa_00012.csv.gz") %>%
  filter(STATEFIP != 27) %>%
  left_join(migpumacrosswalk, by=c("MIGPUMA1"="MIGPUMA", "MULTYEAR"="Year")) %>%
  mutate(agegroup = case_when(
    AGE<17 ~ "16 and Younger",
    AGE>=17 & AGE<24 ~ "17 to 23", 
    AGE>=24 & AGE<31 ~ "24 to 30",
    AGE>=31 ~ "31 and Over"
  ))

save(outmigration, file="outmigration.rda")

