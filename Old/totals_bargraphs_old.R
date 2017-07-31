

# ######################################
# ###### Graph Total Migration #########
# ######################################
# 
# netmig_long <- left_join(inmigration_by_group, outmigration_by_group) %>%
#   mutate(geogroup = ifelse(geogroup=="Greater MN", "Greater Minnesota", geogroup),
#          geogroup = ifelse(geogroup=="Metro", "Other Metro Counties", geogroup)) %>%
#   select(-pct_moved_in, -pct_moved_in_se, -pct_moved_out, -pct_moved_out_se) %>% 
#   gather(direction, mig, moved_in, moved_out) %>%
#   mutate(se = ifelse(direction=="moved_in", moved_in_se, moved_out_se)) %>%
#   select(-moved_in_se, -moved_out_se) %>%
#   mutate(geogroup = factor(geogroup, levels=c("Ramsey","Hennepin","Other Metro Counties","Greater Minnesota")),
#          direction = factor(direction, levels=c("moved_out", "moved_in")))
# 
# 
# library(ggplot2)
# barplot17_totals <- filter(netmig_long, agegroup=="17 to 23") %>%
#   ggplot(aes(x=geogroup, y=mig, fill=direction)) +
#   geom_bar(stat="identity", position="dodge") +
#   geom_text(aes(x=geogroup, y=mig, label=scales::comma(mig)),
#             position=position_dodge(width=0.9), vjust=-0.25) +
#   theme(
#     panel.background = element_blank(),
#     axis.ticks.x=element_blank(),
#     axis.ticks.y=element_blank(),
#     axis.title.x=element_blank(),
#     axis.title.y=element_blank(),
#     axis.text.x=element_text(size=10, color="black"),
#     axis.text.y=element_blank(),
#     legend.title=element_blank(),
#     legend.position="bottom",
#     plot.caption=element_text(size=8, hjust=.5),
#     plot.title = element_text(hjust = 0.5)
#   ) +
#   scale_y_continuous(labels=scales::comma) +
#   scale_fill_discrete(labels=c("Left MN for Another State", "Moved to MN from Another State")) +
#   labs(title = "Average Annual Migration between Minnesota and Other States, 2011-2015\nPersons Aged 17 to 23",
#        caption = "Source: House Research. 2015 American Community Survey 5-year Estimates. IPUMS-USA, University of Minnesota.") 
# 
# ggsave("ages17to23_totals.png", barplot17_totals,width=8,height=6) 
# 
# 
# library(ggplot2)
# barplot17_pct <- filter(netmig_long, agegroup=="17 to 23") %>%
#   ggplot(aes(x=geogroup, y=mig, fill=direction)) +
#   geom_bar(stat="identity", position="dodge") +
#   geom_text(aes(x=geogroup, y=mig, label=scales::comma(mig)),
#             position=position_dodge(width=0.9), vjust=-0.25) +
#   theme(
#     panel.background = element_blank(),
#     axis.ticks.x=element_blank(),
#     axis.ticks.y=element_blank(),
#     axis.title.x=element_blank(),
#     axis.title.y=element_blank(),
#     axis.text.x=element_text(size=10, color="black"),
#     axis.text.y=element_blank(),
#     legend.title=element_blank(),
#     legend.position="bottom",
#     plot.caption=element_text(size=8, hjust=.5),
#     plot.title = element_text(hjust = 0.5)
#   ) +
#   scale_y_continuous(labels=scales::comma) +
#   scale_fill_discrete(labels=c("Left MN for Another State", "Moved to MN from Another State")) +
#   labs(title = "Average Annual Migration between Minnesota and Other States, 2011-2015\nPersons Aged 17 to 23",
#        caption = "Source: House Research. 2015 American Community Survey 5-year Estimates. IPUMS-USA, University of Minnesota.") 
# 
# 
# 
# 
# barplot17_errorbars <- filter(netmig_long, agegroup=="17 to 23") %>%
#   ggplot(aes(x=geogroup, y=mig, fill=direction)) +
#   geom_bar(stat="identity", position="dodge") +
#   geom_errorbar(aes(ymin=mig-se, ymax=mig+se), 
#                 width = .2,
#                 position=position_dodge(.9)) +
#   theme(
#     panel.background = element_blank(),
#     axis.ticks.x=element_blank(),
#     axis.ticks.y=element_blank(),
#     axis.title.x=element_blank(),
#     axis.title.y=element_blank(),
#     axis.text.x=element_text(size=10, color="black"),
#     axis.text.y=element_blank(),
#     legend.title=element_blank(),
#     legend.position="bottom",
#     plot.caption=element_text(size=8, hjust=.5),
#     plot.title = element_text(hjust = 0.5)
#   ) +
#   scale_fill_discrete(labels=c("Left MN for Another State", "Moved to MN from Another State")) +
#   labs(title = "Average Annual Migration between Minnesota and Other States, 2011-2015\nPersons Aged 17 to 23",
#        caption = "Source: House Research. 2015 American Community Survey 5-year Estimates. IPUMS-USA, University of Minnesota.") 
# 
# ggsave("ages17to23_errorbars.png", barplot17_errorbars,width=8,height=6) 
# 
# 
# barplot24 <- filter(netmig_long, agegroup=="24 to 30") %>%
#   ggplot(aes(x=geogroup, y=mig, fill=direction)) +
#   geom_bar(stat="identity", position="dodge") +
#   geom_text(aes(x=geogroup, y=mig, label=scales::comma(mig)),
#             position=position_dodge(width=0.9), vjust=-0.25) +
#   theme(
#     panel.background = element_blank(),
#     axis.ticks.x=element_blank(),
#     axis.ticks.y=element_blank(),
#     axis.title.x=element_blank(),
#     axis.title.y=element_blank(),
#     axis.text.x=element_text(size=10, color="black"),
#     axis.text.y=element_blank(),
#     legend.title=element_blank(),
#     legend.position="bottom",
#     plot.caption=element_text(size=8, hjust=.5),
#     plot.title = element_text(hjust = 0.5)
#   ) +
#   scale_fill_discrete(labels=c("Left MN for Another State", "Moved to MN from Another State")) +
#   labs(title = "Average Annual Migration between Minnesota and Other States, 2011-2015\nPersons Aged 24 to 30",
#        caption = "Source: House Research. 2015 American Community Survey 5-year Estimates. IPUMS-USA, University of Minnesota.") 
# 
# ggsave("ages24to30_totals.png", barplot24,width=8,height=6)
# 
# barplot24_errorbars <- filter(netmig_long, agegroup=="24 to 30") %>%
#   ggplot(aes(x=geogroup, y=mig, fill=direction)) +
#   geom_bar(stat="identity", position="dodge") +
#   geom_errorbar(aes(ymin=mig-se, ymax=mig+se), 
#                 width = .2,
#                 position=position_dodge(.9))+
#   theme(
#     panel.background = element_blank(),
#     axis.ticks.x=element_blank(),
#     axis.ticks.y=element_blank(),
#     axis.title.x=element_blank(),
#     axis.title.y=element_blank(),
#     axis.text.x=element_text(size=10, color="black"),
#     axis.text.y=element_blank(),
#     legend.title=element_blank(),
#     legend.position="bottom",
#     plot.caption=element_text(size=8, hjust=.5),
#     plot.title = element_text(hjust = 0.5)
#   ) +
#   scale_y_continuous(labels=scales::comma) +
#   scale_fill_discrete(labels=c("Left MN for Another State", "Moved to MN from Another State")) +
#   labs(title = "Average Annual Migration between Minnesota and Other States, 2011-2015\nPersons Aged 24 to 30",
#        caption = "Source: House Research. 2015 American Community Survey 5-year Estimates. IPUMS-USA, University of Minnesota.") 
# 
# 
# ggsave("ages24to30_errorbars.png", barplot24_errorbars,width=8,height=6)
# 
# ######################################
# ####### Graph PCT Migration ##########
# ######################################
# 
# migration_percents_long <- left_join(inmigration_by_group, outmigration_by_group) %>%
#   mutate(geogroup = ifelse(geogroup=="Greater MN", "Greater Minnesota", geogroup),
#          geogroup = ifelse(geogroup=="Metro", "Other Metro Counties", geogroup)) %>%
#   select(-moved_in, -moved_in_se, -moved_out, -moved_out_se) %>% 
#   gather(direction, mig, pct_moved_in, pct_moved_out) %>%
#   mutate(se = ifelse(direction=="pct_moved_in", pct_moved_in_se, pct_moved_out_se)) %>%
#   select(-pct_moved_in_se, -pct_moved_out_se) %>%
#   mutate(geogroup = factor(geogroup, levels=c("Ramsey","Hennepin","Other Metro Counties","Greater Minnesota")),
#          direction = factor(direction, levels=c("pct_moved_out", "pct_moved_in")))
# 
# 
# barplot24_pct_errorbars <- filter(migration_percents_long, agegroup=="24 to 30") %>%
#   ggplot(aes(x=geogroup, y=mig, fill=direction)) +
#   geom_bar(stat="identity", position="dodge") 
# 
# 
# +
#   geom_errorbar(aes(ymin=mig-se, ymax=mig+se), 
#                 width = .2,
#                 position=position_dodge(.9))+
#   theme(
#     panel.background = element_blank(),
#     axis.ticks.x=element_blank(),
#     axis.ticks.y=element_blank(),
#     axis.title.x=element_blank(),
#     axis.title.y=element_blank(),
#     axis.text.x=element_text(size=10, color="black"),
#     axis.text.y=element_blank(),
#     legend.title=element_blank(),
#     legend.position="bottom",
#     plot.caption=element_text(size=8, hjust=.5),
#     plot.title = element_text(hjust = 0.5)
#   ) +
#   scale_y_continuous(labels=scales::comma) +
#   scale_fill_discrete(labels=c("Left MN for Another State", "Moved to MN from Another State")) +
#   labs(title = "Average Annual Migration between Minnesota and Other States, 2011-2015\nPersons Aged 24 to 30",
#        caption = "Source: House Research. 2015 American Community Survey 5-year Estimates. IPUMS-USA, University of Minnesota.") 
# 
# 
# ggsave("ages24to30_errorbars.png", barplot24_errorbars,width=8,height=6)