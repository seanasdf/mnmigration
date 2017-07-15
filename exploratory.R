library(dplyr)
library(readr)
library(purrr)

######################################
######### Migration to MN ############
######################################

#Read in minnesota data
inmigration <- read_csv("usa_00010.csv.gz") %>%
  mutate(geogroup =ifelse(PUMARES2MIG==14, "Hennepin", 
                          ifelse(PUMARES2MIG==13, "Ramsey", "All Other Counties")),
         agegroup = ifelse(AGE<17, "16 and Younger",
                           ifelse(AGE>=17 & AGE<24, "17 to 23",
                                  ifelse(AGE>=24 & AGE<31, "24 to 30", "31 and Over")
                           )
         ),
         moved = ifelse(!(MIGPLAC1 %in% c(0,27)) & MIGPLAC1<150, 1, 0),
         moved_alt = ifelse(MIGRATE1 == 3, 1, 0)
  )


#get count of state-to-state movers by my measure
sum(inmigration$moved*inmigration$PERWT)

#get count of state-to-state movers by the census measure
sum(inmigration$moved_alt*inmigration$PERWT)

movers <- filter(inmigration, moved==1)

table(movers$agegroup, movers$geogroup)


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
         )) 

table(outmigration$agegroup, outmigration$geogroup)