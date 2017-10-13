library(tidyverse)
library(readxl)

# 
# ######################################
# ######### Migration within ###########
# ######### Minnesota ##################
# ######################################
# 
# #read in crosswalk for years and PUMAs to Geographic Groupings
# pumacrosswalk <- read_excel("./crosswalk/crosswalk_puma_to_mncounty.xlsx") 
# names(pumacrosswalk)[3] <- "geogroup_residing"
# 
# #read in crosswalk for years and MIGPUMAs to Geographic Groupings
# migpumacrosswalk <- read_excel("./crosswalk/migpumacrosswalk.xlsx",
#                                sheet = "MigPUMAtoGeogroup") 
# names(migpumacrosswalk)[3] <- "geogroup_migration"
# 
# 
# #read in extract from IPUMS and join with crosswalk to geographic groupings
# in_state <- read_csv("usa_00018.csv.gz") 
# 
# in_state <- in_state %>%  
#   left_join(pumacrosswalk, by=c("PUMA"="PUMA", "MULTYEAR"="Year")) %>% 
#   left_join(migpumacrosswalk, by=c("MIGPUMA1"="MIGPUMA", "MULTYEAR"="Year")) %>%
#   mutate(agegroup = case_when(
#     AGE<18 ~ "17 and Younger",
#     AGE>=18 & AGE<24 ~ "18 to 23", 
#     AGE>=24 & AGE<30 ~ "24 to 29",
#     AGE>=30 ~ "30 and Over"
#     ),
#     smallagegrouip = case_when(
#       AGE <18 ~ "17 and Younger",
#       AGE %in% c(18,19) ~ "18-19 Years Old",
#       AGE %in% c(20,21) ~ "20-21 Years Old",
#       AGE %in% c(22,23) ~ "22-23 Years Old",
#       AGE %in% c(24,25) ~ "24-25 Years Old",
#       AGE %in% c(26,27) ~ "26-27 Years Old",
#       AGE %in% c(28,29) ~ "28-29 Years Old",
#       AGE %in% c(30,31) ~ "30-31 Years Old",
#     ),
#     moved_instate = ifelse(MIGPLAC1 == 27, 1, 0)
#   )
# 
# save(in_state, file="instate.rda")

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
  mutate(
    agegroup = case_when(
      AGE<18 ~ "17 and Younger",
      AGE>=18 & AGE<22 ~ "18 to 21",
      AGE>=22 & AGE<30 ~ "22 to 29",
      AGE>=30 ~ "30 and Over"),
    smallagegroup = case_when(
      AGE < 18 ~ "17 and Younger",
      AGE %in% c(18,19) ~ "18-19 Years Old",
      AGE %in% c(20,21) ~ "20-21 Years Old",
      AGE %in% c(22,23) ~ "22-23 Years Old",
      AGE %in% c(24,25) ~ "24-25 Years Old",
      AGE %in% c(26,27) ~ "26-27 Years Old",
      AGE %in% c(28,29) ~ "28-29 Years Old",
      AGE %in% c(30,31) ~ "30-31 Years Old",
      AGE > 31 ~ "32 and Over")
    )

save(inmigration, file="./caches/inmigration.rda")


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
  mutate(
    agegroup = case_when(
      AGE<18 ~ "17 and Younger",
      AGE>=18 & AGE<22 ~ "18 to 21",
      AGE>=22 & AGE<30 ~ "22 to 29",
      AGE>=30 ~ "30 and Over"),
    smallagegroup = case_when(
      AGE <18 ~ "17 and Younger",
      AGE %in% c(18,19) ~ "18-19 Years Old",
      AGE %in% c(20,21) ~ "20-21 Years Old",
      AGE %in% c(22,23) ~ "22-23 Years Old",
      AGE %in% c(24,25) ~ "24-25 Years Old",
      AGE %in% c(26,27) ~ "26-27 Years Old",
      AGE %in% c(28,29) ~ "28-29 Years Old",
      AGE %in% c(30,31) ~ "30-31 Years Old",
      AGE > 31 ~ "32 and Over"))

save(outmigration, file="./caches/outmigration.rda")

