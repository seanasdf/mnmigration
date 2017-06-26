library(dplyr)
library(readr)
library(purrr)
library(srvyr)

inmigration <- read_csv("usa_00010.csv.gz") %>%
  mutate(geogroup =ifelse(PUMARES2MIG==14, "Hennepin", 
                          ifelse(PUMARES2MIG==13, "Ramsey", "All Other Counties")),
         agegroup = ifelse(AGE<17, "16 and Younger",
                           ifelse(AGE>=17 & AGE<24, "17 to 23",
                                  ifelse(AGE>=24 & AGE<31, "24 to 30", "31 and Over")
                           )
         ),
         moved = ifelse(!(MIGPLAC1 %in% c(0,27) & MIGPLAC1<100), 1, 0) 
  ) %>%
  as_survey_design(weights=PERWT) %>%
  group_by(PUMARES2MIG, agegroup) %>%
  summarise(moved_in = survey_total(moved))

######################################
######### Map Net Migration ##########
######################################

#load packages to use
library(rgdal)
library(ggplot2)


#Read in shapefile for MigPumas
if (!file.exists("shapefile.rda")) {
  shapefile <- readOGR("./shapefile", "ipums_migpuma_pwpuma_2010")
  shapefile <- shapefile[shapefile@data$STATEFIP=="27",]
  save(shapefile, file="shapefile.rda")
} else {
  load("shapefile.rda")
}




mnmap <- fortify(shapefile)

ggplot() + 
  geom_polygon(data=mnmap,
               aes(long, lat, group=group),
               fill="white",
               color="black") +
  coord_equal()