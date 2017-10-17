######################################
##### LOAD THEME AND GGPLOT2 #########
######################################

library(ggplot2)
library(showtext)
library(dplyr)
library(forcats)

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
#create theme
theme_migration <-  theme(
  panel.background = element_blank(),
  axis.ticks.x=element_blank(),
  axis.ticks.y=element_blank(),
  axis.title.x=element_blank(),
  axis.title.y=migration_text,
  axis.text.x=migration_text,
  axis.text.y=migration_text,
  legend.text=migration_text,
  plot.caption = migration_text,
  legend.title=element_blank(),
  legend.position="bottom",
  plot.title =migration_text,
  panel.grid.major.y = element_line(color="#d9d9d9"),
  panel.grid.major.x = element_blank()
) + 
  theme(plot.title = element_text(size=36, hjust = 0.5),
        plot.caption = element_text(size=24))


#create default text for the captions
caption_noerrors <- "Source: MN House Research/State Demographer.
       2015 American Community Survey 5-year Estimates. IPUMS-USA, University of Minnesota."

caption_witherrors <- "Source: MN House Research/State Demographer. Error bars represent 90% confidence intervals.
       2015 American Community Survey 5-year Estimates. IPUMS-USA, University of Minnesota."