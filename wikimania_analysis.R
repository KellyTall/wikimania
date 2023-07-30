library(rvest)
library(tidyverse)
library(stringr)
library(tidyr)
library(lubridate)
library(geomtextpath)
library(scales)
library(svglite)

## run wikimania_web_scrape.R first

all_set <- rbind(first_set, second_set) %>% 
  distinct() %>% 
  separate_wider_delim(date_time, delim=", ", names=c("time", "date")) %>% 
  
  # mutate(time=hm(time),
  #        time = as.character(time)) %>% 
  unite(date_time, c(date,time), sep = ", ") %>% 
  mutate(date_time = dmy_hm(date_time)) %>% 
  arrange(date_time) %>% 
  mutate(byte_size = str_remove_all(bytes, c(" bytes")),
         byte_size = str_remove_all(byte_size, c(",")),
         byte_size= as.numeric(byte_size),
         change = byte_size - lag(byte_size),
         change = case_when(is.na(change)~ 0,
                            TRUE ~ as.numeric(change)),
         cum = cumsum(change)) %>% 
  mutate(date = date(date_time)) %>% 
  group_by(date) %>% 
  add_tally(name="edit_day") %>% 
  group_by(user_name) %>% 
  add_tally(name="user_edits") %>% 
  arrange(desc(user_edits)) 

View(all_set)

users <- all_set %>% 
  group_by(user_name) %>% 
  tally() %>% 
  arrange(desc(n))


View(users_edits_summary_top)

users_edits_summary_top <- all_set %>% 
  group_by(user_name) %>% 
  mutate(firstdate = min(date_time),
         lastdate = max(date_time),
         user_contribution = sum(change),
         days_edit=date(lastdate) - date(firstdate)) %>% 
  group_by(user_name, firstdate, lastdate, user_contribution, days_edit) %>% 
  tally(name="number_edits") %>% 
  ungroup() %>% 
  arrange(desc(number_edits)) %>% 
  slice_head(n = 10) %>% 
  mutate(user_name = as_factor(user_name),
         user_name = fct_reorder(user_name,days_edit),
         user_name = fct_rev(user_name)) %>% 
  mutate(first_asdate = date(firstdate),
         last_asdate = date(lastdate)) %>% 
  mutate(edit_label = paste(user_name, " (", number_edits,"/",days_edit,")", sep = ""))
  


top_ten <- users_edits_top %>% 
  select(user_name) %>% 
  ungroup() %>% 
  reframe(user_name)



users_edits_all_top <- all_set %>% 
  filter(user_name %in% top_ten$user_name) %>% 
  group_by(user_name) %>% 
  mutate(firstdate = min(date_time),
         lastdate = max(date_time),
         user_contribution = sum(change),
         days_edit=date(lastdate) - date(firstdate)) %>% 
  group_by(user_name, firstdate, lastdate, user_contribution, days_edit) %>% 
  add_tally(name="number_edits") %>% 
  ungroup() %>% 
  arrange(desc(number_edits)) %>% 
  mutate(rank = rank(desc(number_edits))) %>% 
  mutate(user_name = as_factor(user_name),
         user_name = fct_reorder(user_name,days_edit),
         user_name = fct_rev(user_name))
  
  View(users_edits_all_top)
  
  

View(users_edits)


##charts


View(all_set)




small_set <- all_set %>% 
  mutate(year = year(date_time)) %>% 
  mutate(date = date(date_time)) %>% 
  filter(year == 2011)


View(small_set)


smaller_set <- small_set %>% 
  filter(date <= dmy("28-02-2011"))

View(smaller_set)

options(scipen = 999)





area_theme_polar <- theme(panel.background= element_rect(fill="black"),
                    axis.line = element_blank(),
                    panel.grid = element_line(colour="#EFEDE4", linewidth = .1),
                    axis.text.x = element_text(vjust = -2, hjust=1.25,size = 8, colour= "#C4C4C4"),
                    plot.background = element_rect(fill="black"),
                    panel.grid.minor = element_blank(),
                    axis.text.y = element_blank()
)


# my_breaks <- my_breaks <- scales::extended_breaks()(smaller_set$change)
# my_breaks <- my_breaks[2:(length(my_breaks)-1)]

area_theme_polar2 <- theme(axis.line = element_blank(),
                          panel.grid = element_line(colour="#C4C4C4", linewidth = .5),
                          axis.text.x = element_text(vjust = -2, hjust=1.25,size = 10, colour= "#FBEDBD"),
                          panel.grid.minor = element_blank(),
                          axis.text.y = element_blank(),
                          legend.position = "none",
                          axis.title = element_blank()
)





polar_chart <- ggplot(smaller_set, aes(date_time, cum)) +
  coord_curvedpolar()+
  geom_area(alpha=.2, fill="#FBEDBD")+
  geom_segment(aes(x=date_time, xend=date_time, y=0, yend=change, colour = change >0))+
  scale_y_continuous(limits=c(-187000, 208700), labels = label_number(suffix = "K", scale = 1e-6))+
  scale_x_datetime(date_labels = "%b %d", date_breaks = "1 day", limits = c(ymd_hms("2011-01-25 00:00:00"),ymd_hms ("2011-02-28 23:59:59")))+
  # scale_x_datetime(date_labels = "%b %d", date_breaks = "1 day")+
  
  scale_colour_manual(
                      name = 'change < 0', 
                      values = setNames(c('#82BF9E', '#DA633B'), c(T, F)))+
  theme_minimal()+
  area_theme_polar2

polar_chart

ggsave("polar_chart.svg", polar_chart, width=33, height = 33, units="cm")



area_theme_cart <- theme(panel.background= element_rect(fill="black"),
                         axis.line = element_line(colour="#EFEDE4", linewidth = .2),
                         panel.grid = element_line(colour="#EFEDE4", linewidth = .1),
                         axis.text.x = element_text(size = 8, colour= "#C4C4C4"),
                         plot.background = element_rect(fill="black")
)

area_theme_cart2 <- theme(axis.line = element_line(colour="#EFEDE4", linewidth = .2),
                         panel.grid = element_line(colour="#EFEDE4", linewidth = .1),
                         axis.text = element_text(size = 10, colour= "#C4C4C4")
)

size_change_all <- ggplot(all_set, aes(date_time, cum)) +
  annotate(geom="rect",xmin=ymd_hms("2011-01-25 13:26:00"), xmax=ymd_hms("2011-02-28 23:29:00"), ymin=-200000, ymax=300000, fill="#C4C4C4", alpha=.3, color="#FBEDBD", linewidth=.2)+
  geom_area(alpha=.2, fill="#FBEDBD")+
  geom_segment(aes(x=date_time, xend=date_time, y=0, yend=change, colour = change >0), linewidth=.5)+
  scale_x_datetime(date_labels = "%Y", date_breaks = "1 year",limits = c(ymd_hms("2011-01-01 00:00:00"),ymd_hms ("2023-05-31 00:00:00")), expand=c(0,0))+
  scale_colour_manual(guide = FALSE,
                    name = 'change < 0', 
                    values = setNames(c('#82BF9E', '#DA633B'), c(T, F)))+
  theme_minimal()+
  area_theme_cart+
  scale_y_continuous( labels = label_number(suffix = "K", scale = 1e-3))
  

size_change_all  

ggsave("cart_chart.svg", size_change_all, width=54, height = 13, units="cm")


edit_theme <- theme(panel.background= element_rect(fill="black"),
                    axis.line = element_line(colour="#EFEDE4", linewidth = .2),
                    panel.grid = element_line(colour="#EFEDE4", linewidth = .1),
                    axis.text.x = element_text(vjust = -1, size = 8, colour= "#C4C4C4"),
                    axis.text.y = element_blank(),
                    strip.text.y = element_blank(),
                    # strip.text.y.left = element_text(angle = 0, hjust = 1),
                    axis.title = element_blank(),
                    plot.background = element_rect(fill="black"),
                    panel.grid.minor.x = element_blank(),
                    legend.position = "none"
)

edit_theme2 <- theme(axis.line = element_line(colour="#EFEDE4", linewidth = .2),
                    panel.grid = element_line(colour="#EFEDE4", linewidth = .1),
                    axis.text.x = element_text(vjust = -1, size = 8, colour= "#C4C4C4"),
                    axis.text.y = element_blank(),
                    strip.text.y = element_blank(),
                    # strip.text.y.left = element_text(angle = 0, hjust = 1),
                    axis.title = element_blank(),
                    panel.grid.minor.x = element_blank(),
                    legend.position = "none",
)

edit_range_gant <- ggplot(users_edits_summary_top) +
  geom_segment(aes(x=firstdate, xend=lastdate, y=0, yend=0), colour = "#917E7E", linewidth = 1)+
  geom_segment(data=users_edits_all_top,aes(x=date_time, xend=date_time, y=-1.5, yend=1.5, colour = change >0), linewidth=.5)+
  facet_grid(user_name ~., switch = "y")+
  theme_minimal()+
  edit_theme+
  scale_y_continuous(limits = c(-2, 4), breaks = 1)+
  geom_text(data=users_edits_summary_top, aes(label=edit_label, x=firstdate, y=3.2), colour="#EFEDE4", hjust=0)+
  scale_x_datetime(limits = c(ymd_hms("2011-01-01 00:00:00"),ymd_hms ("2023-05-31 00:00:00")), expand=c(0,0))


edit_range_gant

ggsave("edit_range_gant.svg", edit_range_gant, width=54, height = 13, units="cm")


# edit_range_point <- ggplot(users_edits_summary_top) +
#   geom_segment(aes(x=firstdate, xend=lastdate, y=1, yend=1), colour = "#917E7E", linewidth = 1)+
#   geom_point(data=users_edits_all_top, aes(date_time, 1), shape=21, size=3, alpha=.5, fill="grey")+
#   geom_point(data=users_edits_all_top, aes(date_time, 1), shape=21, size=3, colour="#C47154" )+
#   facet_grid(user_name ~., switch = "y")+
#   theme_minimal()+
#   edit_theme+
#   scale_y_continuous(limits = c(.2, 2.8), breaks = 1)+
#   geom_text(data=users_edits_summary_top, aes(label=user_name, x=firstdate, y=2.1), colour="white", hjust=0)+
#   scale_x_datetime(limits = c(ymd_hms("2011-01-01 00:00:00"),ymd_hms ("2023-05-31 00:00:00")), expand=c(0,0))

ggsave("edit_range.svg", edit_range, width=54, height = 13, units="cm")


##drafts / sketches

# article_size <- ggplot(all_set, aes(date_time, cum)) +
#   geom_area(alpha=.5)+
#   # scale_y_continuous(limits=c(0, 265500))+
#   labs(y=NULL, x=NULL)+
#   coord_polar()
# 
# article_size
# 
# article_change <- ggplot(all_set, aes(date_time, change))  +
#   geom_segment(aes(x=date_time, xend=date_time, y=0, yend=change))+
#   geom_area()+
#   coord_polar()
# 
# article_change
# 
# size_change <- ggplot(all_set, aes(date_time, cum)) +
#   geom_area(alpha=.5)+
#   geom_segment(aes(x=date_time, xend=date_time, y=0, yend=change, colour = change >0))+
#   coord_polar()
# 
# size_change

# size_change_small <- ggplot(small_set, aes(date_time, cum)) +
#   geom_area()+
#   geom_segment(aes(x=date_time, xend=date_time, y=0, yend=change, colour = change >0))+
#   scale_y_continuous(limits=c(-187000, 280000))+
#   scale_x_datetime(date_labels = "%b %y", date_breaks = "1 month")+
#   coord_polar()
# 
# size_change_small