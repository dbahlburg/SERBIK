#17.2.2020
#Dominik Bahlburg
#In this script the properties of the Palmer environmental dataset are visualized.
#Load necessary packages, set working directory and load palmer gerenator-function
library(lubridate)
library(tidyverse)
library(cowplot)
library(here)
source(here('functions','generatePalmer.R'))
# =============================================================================#
# First: create food plot
startDay <- 1
days <- 366
data1 <- generatePalmer(boostWinter = F) %>% 
  mutate(#month = month(date, label = T, abbr = T),
    month = month(date, label = F),
         scenario = 'palmer') %>% 
  group_by(month, scenario) %>% 
  summarise(meanFood = mean(foodConc))
data2 <- generatePalmer(boostWinter = T) %>% 
  mutate(#month = month(date, label = T, abbr = T),
    month = month(date, label = F),
         scenario = 'palmer winter boost') %>% 
  group_by(month, scenario) %>% 
  summarise(meanFood = mean(foodConc)) %>% 
  bind_rows(.,data1)

# Make the plot
food <- ggplot(data2, aes(x=month, y=meanFood, fill = scenario, colour = scenario)) +       
  geom_bar(stat = 'identity', position = 'identity') + # lowest: #b2d4b7 #8aa88f +
  scale_fill_manual(values = c('#8c8c8c','#bdbdbd')) +
  scale_colour_manual(values = c('#8c8c8c','#bdbdbd')) +
  labs(x = '', y = expression(mg~m^3),
       fill = '', colour = '', title = 'chlorophyll a concentration') +
  scale_x_continuous(breaks = c(3,6,9,12), labels = c('Mar','Jun','Sep','Dec')) +
  theme(panel.background = element_rect(fill = NA, colour = '#6b6b6b'),
        strip.background = element_rect(fill = NA),
        axis.text = element_text(size = 14),
        axis.title = element_text(size = 14),
        axis.ticks.x = element_blank(),
        plot.title = element_text(size = 14, hjust = 0.5),
        legend.text = element_text(size = 12),
        legend.key.height = unit(4,'mm'),
        legend.key.width = unit(4,'mm'),
        legend.background = element_rect(fill = NA),
        plot.margin=grid::unit(c(1,1,1,1), "mm"),
        legend.position = c(0.7,0.9))
# =============================================================================#
# Second: create temperature curve
data <- generatePalmer()
temp <- ggplot(data, aes(x = date, y = temperature)) +
  geom_line(size = 0.8, colour = '#8c8c8c')+
  scale_x_date(date_breaks = '3 month', date_labels = '%b', limits = c(as.Date('2010-01-01'),
                                                                       as.Date('2011-01-01'))) +
  labs(x = '', y = expression(C), title = 'water temperature') +
  theme(panel.background = element_rect(fill = NA, colour = '#6b6b6b'),
        strip.background = element_rect(fill = NA),
        axis.text = element_text(size = 14),
        axis.title = element_text(size = 14),
        axis.ticks.x = element_blank(),
        plot.title = element_text(size = 14, hjust = 0.5),
        legend.text = element_text(size = 14),
        legend.background = element_rect(fill = NA),
        plot.margin=grid::unit(c(1,1,1,1), "mm"),
        legend.position = c(0.9,0.9))
# =============================================================================#
# Third: create light curve
light <- ggplot(data, aes(x = date, y = dayLength)) +
  geom_area(fill = '#454545', alpha = 0.35) +
  scale_x_date(date_breaks = '3 month', date_labels = '%b') +
  scale_y_continuous(limits = c(0,24), breaks = c(6,12,18,24)) +
  labs(x = '', y = 'hours', title = 'day length') +
  theme(panel.background = element_rect(fill = NA, colour = '#6b6b6b'),
        strip.background = element_rect(fill = NA),
        axis.text = element_text(size = 14),
        axis.title = element_text(size = 14),
        axis.ticks.x = element_blank(),
        plot.title = element_text(size = 14, hjust = 0.5),
        legend.text = element_text(size = 14),
        legend.background = element_rect(fill = NA),
        plot.margin=grid::unit(c(1,1,1,1), "mm"),
        legend.position = c(0.9,0.9))

palmerPlots <- plot_grid(food, temp, light,
                         nrow = 1, scale = 0.85)

