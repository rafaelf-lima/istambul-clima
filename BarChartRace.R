library(tidyverse)
library(gganimate)
library(lubridate)
library(RColorBrewer)
library(scico)
library(ggstance)
library(ggsci)
#COLUNAS: select() e mutate()
#LINHAS: filter(), distinct() e arrange()
#AGRUPAR: group_by(), summarise() e count()


dft <- read.csv("C:\\Users\\ritad\\Downloads\\Istanbul Weather Data.csv")


view(dft)

dft$DateTime <- dmy(dft$DateTime)

view(dft)

Winter <- c("December", "January", "February")
Spring <- c("March", "April", "May")
Summer <- c("June", "July", "August")
Fall <- c("September", "October", "November")


dftnovo <- dft %>% 
  select(DateTime, MaxTemp, MinTemp) %>% 
  filter(DateTime < '2019-01-01') %>% 
  mutate(DateTime, Month = month(DateTime)) %>% 
  mutate(Month, Month_Name = month.name[Month]) %>% 
  mutate(DateTime, Year = year(DateTime))

view(dftnovo)


dftp <- dftnovo %>%
  group_by(Year, Month_Name) %>%
  summarise(MaxTemp = max(MaxTemp)) %>% 
  mutate(rank = as.integer(rank(-MaxTemp)),
         MaxTemp_lbl = paste0(" ",as.character(MaxTemp))) %>% 
  ungroup() %>% 
  group_by(Year, rank) %>%
  mutate(n = n_distinct(Month_Name)) %>%
  mutate(width = 0.9 / n_distinct(Month_Name)) %>%
  arrange(rank) %>%
  mutate(x = rank + 1.5 * (seq(1, 2 * n() - 1, by = 2) / 2 / n() - 0.5)) %>%
  ungroup()

view(dftp)

dftpn <- dftp %>% 
  mutate(Season = case_when(
    Month_Name == "December" | Month_Name == "January" | Month_Name == "February" ~ "Winter",
    Month_Name == "March" | Month_Name == "April" | Month_Name == "May" ~ "Spring",
    Month_Name == "June" | Month_Name == "July" | Month_Name == "August" ~ "Summer",
    Month_Name == "September" | Month_Name == "October" | Month_Name == "November" ~ "Fall"  
  ))

view(dftpn)

p <- ggplot(dftpn, aes(x = x, group = Month_Name, fill = Season, color = Season))+
    geom_tile(aes(y = MaxTemp/2,
               height = MaxTemp,
               width = 0.9), alpha = 0.9, colour = NA)+
  geom_text(aes(y = 1, label = paste(Month_Name, " ")), vjust = 0.2, hjust = 0, fontface ="bold", colour = "black")+ 
  geom_text(aes(y = MaxTemp, label = paste(MaxTemp_lbl, " ºC"), hjust = 0), fontface ="bold", colour = "black")+
  coord_flip(clip = "off", expand = FALSE)+
  scale_y_continuous(labels = scales::comma, c("Janeiro"))+
  scale_x_reverse()+
  guides(color = "none", fill = "none")+
  scale_fill_igv()+
  labs(title = 'Temperaturas máximas em Istambul - {closest_state}',  
       subtitle  =  "A maior temperatura máxima de cada mês na principal cidade turca (2009-2018)",
       caption  = "@rafaelf_lima")+
  theme(axis.line=element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        legend.position="none",
        panel.background = element_rect(colour = "#FFFADE", fill = "#FFFADE"),
        panel.border=element_blank(),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        plot.title=element_text(size=20, hjust=0, face="bold", color="black"),
        plot.subtitle=element_text(size=12, hjust=0, face="bold", color="black"),
        plot.caption =element_text(size=8, hjust=1, face = "bold" , color="black"),
        plot.background = element_rect(colour = "#FFFADE", fill = "#FFFADE"),
        plot.margin = margin(1,1,1,4, "cm"))+
  transition_states(Year, transition_length = 4, state_length = 1, wrap = FALSE)+
  ease_aes('cubic-in-out')  

p  


animate(p, 200, fps = 20,  width = 1200, height = 1000, 
        renderer = gifski_renderer("gganim.gif"), end_pause = 15, start_pause =  15) 


?shadow_mark