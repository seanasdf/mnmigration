library(dplyr)
library(readr)
library(purrr)
library(srvyr)

######################################
#########  SET DIRECTORIES  ##########
######################################

#Home machine
#setwd("~/Documents/rstuff/mnmigration")

#work mchine
setwd("i:/user/williams/data assistance/migration wagenius/")

######################################
######### Migration to MN ############
######################################

#using srvyr
mndata <- read_csv("usa_00006.csv.gz") %>%
      mutate(geogroup =ifelse(CITY==4150, "Minneapolis", ifelse(CITY==6110, "St. Paul", "GMN"))) %>%
      as_survey(weights=PERWT) %>%
      group_by(geogroup) %>%
      summarise(movers = survey_total(!(MIGPLAC1 %in% c(0,27)) & MIGPLAC1<100))

mndata

#manual double check
mndata2 <- read_csv("usa_00006.csv.gz") %>%
      mutate(geogroup =ifelse(CITY==4150, "Minneapolis", ifelse(CITY==6110, "St. Paul", "GMN"))) %>%
      filter(!(MIGPLAC1 %in% c(0,27)) & MIGPLAC1<100) %>%
      group_by(geogroup) %>%
      summarise(count=sum(PERWT))

######################################
######### Migration from MN ##########
######################################

otherstatedata <- read_csv("usa_00009.csv.gz") %>%
      filter(STATEFIP != 27) %>%
      mutate()
      group_by(STATEFIP) %>%
      summarise(count=sum(PERWT))

