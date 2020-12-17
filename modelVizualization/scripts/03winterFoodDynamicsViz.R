library(tidyverse)
library(lubridate)
library(cowplot)
setwd('/Users/dominik/Git/PhDThesis/growthModel')
source('functions/growthModel.R')
source('functions/defineConstants.R')


#================================================================================#
inputScenario <- 'palmer'
winterBoost <<- T
lightSwitch <<- 'on'
tempSwitch <<- 'on'
reproScenario <- 'fixedClutchSize'
startDay = 0
years = 6
noDays = years * 365
defineConstants()
modelResults <- krillGrowth() %>% 
  mutate(length = volumetricLength / shapeCorrection) %>% 
  mutate(referenceDate = as.Date(referenceDate, origin = '1970-01-01'),
         scenario = 'regulationOn')
lightSwitch <<- 'off'
tempSwitch <<- 'off'
defineConstants()
winterBoostData <- krillGrowth() %>% 
  mutate(length = volumetricLength / shapeCorrection) %>% 
  mutate(referenceDate = as.Date(referenceDate, origin = '1970-01-01'),
         scenario = 'regulationOff') %>% 
  bind_rows(.,modelResults)

#================================================================================#
#Plot the results
textSize <- 18
stageLines <- tibble(x = c(0, 0),
                     xend = c(6.2,6.2),
                     y = c(2.42,35),
                     yend = c(2.42,35))

growthBoost <- winterBoostData %>% 
  group_by(scenario) %>% 
  mutate(stageColour = ifelse(age/365<2.2 & between(length, 30, 40), abs(length-35)/5, 1),
         transitionPoint = ifelse(stage - lead(stage) != 0, length, NA)) %>% 
  ggplot(.,aes(x = age/365, y = length)) +
  scale_x_continuous(breaks = 0:6, limits = c(0,6.5))+
  scale_y_continuous(limits = c(0,64)) +
  scale_colour_manual(values = c('#797979','#d73c43'),
                      labels = c('seasonal regulation off','seasonal regulation on')) +
  scale_fill_manual(values = c('#404040','#db1a23'),
                    labels = c('seasonal regulation off','seasonal regulation on')) +
  geom_path(aes(colour = scenario, group = scenario), size = 1, alpha= 1) +
  geom_segment(data = stageLines, aes(x = x, xend = xend, y = y, yend = yend),
               linetype = c('22','22'), colour = '#b3b3b3') +
  geom_point(aes(y = transitionPoint, fill = scenario),
             shape = 21, colour = '#404040') +
  labs(x = 'years', y = 'standard length mm') +
  annotate('text', x = c(5.4,5.4),  y = c(4.5, 37),
           label = c(sprintf('embryo \u2192 juvenile'), sprintf('juvenile \u2194 adult')),
           size = 5, colour = '#7d7d7d') +
  labs(x = 'years', y = 'standard length mm') +
  theme(axis.ticks = element_blank(),
        axis.text = element_text(size = textSize),
        axis.title = element_text(size = textSize),
        legend.position = 'none',
        legend.title = element_blank(),
        legend.background = element_blank(),
        legend.key = element_blank(),
        panel.spacing = unit(2, "lines"),
        panel.background = element_rect(fill = NA, colour = '#b8b8b8'))

growthBoost

#Stage dynamics
stageBoost <- winterBoostData %>% 
  mutate(stage = ifelse(scenario == 'regulationOn', stage - 0.05, stage + 0.05),
         year = floor(age/365)) %>% 
  ggplot(.,aes(x = age/365, y = stage,
               group = interaction(scenario,stage,year),
               colour = scenario)) +
  scale_x_continuous(breaks = 0:6)+
  scale_y_continuous(breaks = c(1,2,3), labels = c('embryo','juvenile','adult')) +
  scale_alpha_continuous(range = c(0.75,1), guide = F) +
  scale_colour_manual(values = c('#797979','#d73c43'),
                    guide = F) +
  geom_path(size = 1, alpha = 1) +
  labs(x = 'years', y = '') +
  theme(axis.ticks = element_blank(),
        legend.position = c(0.8,0.15),
        legend.title = element_blank(),
        axis.text = element_text(size = textSize),
        axis.title = element_text(size = textSize),
        legend.background = element_blank(),
        legend.key = element_blank(),
        panel.spacing = unit(2, "lines"),
        panel.background = element_rect(fill = NA, colour = '#b8b8b8'))
stageBoost

#Dynamics in the reproduction Buffer
reproBufferBoost <- winterBoostData %>% 
  ggplot(.,aes(x = age/365, y = reproBuffer,
               group = scenario,
               fill = scenario)
         ) +
  scale_y_continuous(limits = c(0,200), breaks = c(0,100,200)) +
  scale_x_continuous(breaks = 0:6)+
  scale_fill_manual(values = c('#797979','#d73c43'),
                    guide = F) +
  geom_area(alpha = 1) +
  facet_wrap(~scenario, ncol = 1) +
  labs(x = 'years', y = 'dry weight (mg)') +
  theme(axis.ticks = element_blank(),
        axis.text = element_text(size = textSize),
        axis.title = element_text(size = textSize),
        legend.position = c(0.8,0.15),
        strip.text = element_blank(),
        legend.title = element_blank(),
        legend.background = element_blank(),
        legend.key = element_blank(),
        panel.spacing = unit(2, "lines"),
        panel.background = element_rect(fill = NA, colour = '#b8b8b8'))
reproBufferBoost

spawningWindowPoly1 <- tibble(dayMonthYear = as.Date(c('2010-12-30',
                                                       '2010-12-30',
                                                       '2010-12-31',
                                                       '2010-12-31')),
                              year = c(5.25,5.75,5.75,5.25) -0.5,
                              scenario = 1)
spawningWindowPoly2 <- tibble(dayMonthYear = as.Date(c('2010-01-01',
                                                       '2010-01-01',
                                                       '2010-01-11',
                                                       '2010-01-11')),
                              year = c(5.25,5.75,5.75,5.25)-0.5,
                              scenario = 1)

helpLines <- tibble(x = as.Date(c('2010-11-01','2010-12-01','2010-01-01','2010-02-01','2010-03-01')),
                    xend = as.Date(c('2010-11-01','2010-12-01','2010-01-01','2010-02-01','2010-03-01')),
                    y = 0,
                    yend = 6.5,
                    scenario = 'regulationOn')
monthLine <- tibble(x = as.Date(c('2010-01-02','2010-02-02','2010-03-02',
                                  '2010-10-02','2010-11-02','2010-12-02')),
                    xend = as.Date(c('2010-01-30','2010-02-27','2010-03-30',
                                     '2010-10-30','2010-11-30','2010-12-30')),
                    y = 6.5,
                    yend = 6.5,
                    month = c('Jan','Feb','Mar','Oct','Nov','Dec'),
                    scenario = 'regulationOn')

halfCircles <- tibble(x = rep(as.Date(c('2010-01-02','2010-02-02','2010-03-02',
                                    '2010-10-01','2010-11-02','2010-12-02')),times = 6),
                      xend = rep(as.Date(c('2010-01-30','2010-02-27','2010-03-31',
                                       '2010-10-30','2010-11-30','2010-12-30')),times = 6),
                      y = rep(seq(0.5,5.5,1),each = 6),
                      yend = rep(seq(0.5,5.5,1),each = 6),
                      scenario = 'regulationOn',
                      month = rep(c('Jan','Feb','Mar','Oct','Nov','Dec'), times = 6))
notSimulated <- tibble(xmin = as.Date(c('2010-10-01','2010-01-01')),
                       xmax = as.Date(c('2010-12-31','2010-03-30')),
                       ymin = c(0,5.6),
                       ymax = c(0.4,6.4),
                       scenario = 1)

#Spawning:
spawningBoost <- winterBoostData %>% 
  filter(repro == T) %>% 
  mutate(year = round(age/365),
         dayMonthYear = as.Date(paste('2010',
                                      month(referenceDate),
                                      day(referenceDate), sep = '-'
         ))) %>% 
  ggplot(.) +
  geom_rect(data = notSimulated, aes(xmin = xmin, xmax = xmax,
                                     ymin = ymin, ymax = ymax),
            fill = '#e3e3e3') +
  geom_segment(data = monthLine, aes(x = x, xend = xend,
                                     y = y, yend = yend,
                                     group = month),
               colour = '#707070') +
  geom_segment(data = halfCircles, 
               aes(x = x, xend = xend,
                   y = y, yend = yend,
                   group = month),
               colour = '#bababa', alpha = 0.8, 
               size = 0.3, linetype = 'solid')+
  geom_polygon(data = spawningWindowPoly1,
               aes(x = dayMonthYear, y = year),
               fill = '#797979',
               colour = NA, alpha = 1) +
  geom_polygon(data = spawningWindowPoly2,
               aes(x = dayMonthYear, y = year),
               fill = '#797979',
               colour = NA, alpha = 1) +
  scale_x_date(limits = as.Date(c('2010-01-01','2010-12-31')),
               breaks = c(seq(as.Date('2010-01-15'), as.Date('2010-03-15'), by = '1 month'),
                          seq(as.Date('2010-10-15'), as.Date('2010-12-15'), by = '1 month')),
               date_labels = '%b')+
  scale_y_continuous(limits = c(0,6.5)) +
  scale_fill_manual(values = c('#797979','#d73c43'),
                    guide = F) +
  scale_colour_manual(values = c('#797979','#d73c43'),
                      guide = F) +
  geom_tile(aes(x = dayMonthYear, y = year,
                 group = scenario,
                 colour = scenario,
                 fill = scenario),height = 0.5, width = 10, alpha = 1) +
  annotate('text', x = rep(as.Date('2010-04-01'), times = 7),
            y = 0:6, label = as.character(1:7),
           angle = -90,colour = '#bababa',size = 3.6) +
  coord_polar() +
  labs(x = '', y = '') +
  theme(axis.ticks = element_blank(),
        legend.position = 'none',
        legend.title = element_blank(),
        axis.text = element_text(size = textSize),
        axis.title = element_text(size = textSize),
        legend.background = element_blank(),
        legend.key = element_blank(),
        axis.text.y = element_blank(),
        axis.text.x = element_text(angle = c(-15,-45,-75,-285,-315,-345),
                                   size = textSize),
        panel.spacing = unit(2, "lines"),
        panel.grid = element_blank(),
        panel.background = element_rect(fill = NA, colour = NA))
spawningBoost
#================================================================================#
#================================================================================#
#================================================================================#
#Run the model without winter boost
inputScenario <- 'palmer'
winterBoost <<- F
lightSwitch <<- 'on'
tempSwitch <<- 'on'
reproScenario <- 'fixedClutchSize'
startDay = 0
years = 6
noDays = years * 365
defineConstants()
modelResults <- krillGrowth() %>% 
  mutate(length = volumetricLength / shapeCorrection) %>% 
  mutate(referenceDate = as.Date(referenceDate, origin = '1970-01-01'),
         scenario = 'regulationOn')
lightSwitch <<- 'off'
tempSwitch <<- 'off'
defineConstants()
noWinterBoost <- krillGrowth() %>% 
  mutate(length = volumetricLength / shapeCorrection) %>% 
  mutate(referenceDate = as.Date(referenceDate, origin = '1970-01-01'),
         scenario = 'regulationOff') %>% 
  bind_rows(.,modelResults)
#================================================================================#
#Plot the results without winter boost!
growthNoBoost <- 
  noWinterBoost %>% 
  group_by(scenario) %>% 
  mutate(stageColour = ifelse(age/365<2.2 & between(length, 30, 40), abs(length-35)/5, 1),
         #stageColour = ifelse(between(length, 32, 38), rev(exp(-0.05 * abs(length-35)/3)),1)
         transitionPoint = ifelse(stage - lead(stage) != 0, length, NA)) %>% 
  ggplot(.,aes(x = age/365, y = length)) +
  scale_x_continuous(breaks = 0:6, limits = c(0,6.5))+
  scale_y_continuous(limits = c(0,64)) +
  scale_colour_manual(values = c('#797979','#d73c43'),
                      labels = c('seasonal regulation off','seasonal regulation on')) +
  scale_fill_manual(values = c('#404040','#db1a23'),
                      labels = c('seasonal regulation off','seasonal regulation on')) +
  geom_path(aes(colour = scenario, group = scenario), size = 1, alpha= 1) +
  geom_segment(data = stageLines, aes(x = x, xend = xend, y = y, yend = yend),
               linetype = c('22','22'), colour = '#b3b3b3') +
  geom_point(aes(y = transitionPoint, fill = scenario),
             shape = 21, colour = '#404040') +
  # annotate('text', x = c(6.3, 6.3, 6.3),  y = c(4, 23, 50),
  #          label = c('embryo','juvenile','adult'),
  #          size = 3.6, colour = '#7d7d7d', angle = -90) +
  annotate('text', x = c(1.15,1.15),  y = c(4.5, 37),
           label = c(sprintf('embryo \u2192 juvenile'), sprintf('juvenile \u2194 adult')),
           size = 5, colour = '#7d7d7d') +
  labs(x = 'years', y = 'standard length mm') +
  theme(axis.ticks = element_blank(),
        legend.position = 'none',
        axis.text = element_text(size = textSize),
        axis.title = element_text(size = textSize),
        legend.title = element_blank(),
        legend.background = element_blank(),
        legend.key = element_blank(),
        panel.spacing = unit(2, "lines"),
        panel.background = element_rect(fill = NA, colour = '#b8b8b8'))
growthNoBoost

#Stage dynamics
stageNoBoost <- noWinterBoost %>% 
  mutate(stage = ifelse(scenario == 'regulationOn', stage - 0.05, stage + 0.05),
         year = floor(age/365)) %>% 
  ggplot(.,aes(x = age/365, y = stage,
               group = interaction(scenario,stage,year),
               colour = scenario)) +
  scale_x_continuous(breaks = 0:6)+
  scale_y_continuous(breaks = c(1,2,3), labels = c('embryo','juvenile','adult')) +
  scale_alpha_continuous(range = c(0.75,1), guide = F) +
  scale_colour_manual(values = c('#5e5e5e', '#cf1313'),
                      labels = c('seasonal regulation off','seasonal regulation on'),
                      guide = F) +
  geom_path(size = 1, alpha = 0.85) +
  labs(x = 'years', y = '') +
  theme(axis.ticks = element_blank(),
        legend.position = c(0.8,0.15),
        axis.text = element_text(size = textSize),
        axis.title = element_text(size = textSize),
        legend.title = element_blank(),
        legend.background = element_blank(),
        legend.key = element_blank(),
        panel.spacing = unit(2, "lines"),
        panel.background = element_rect(fill = NA, colour = '#b8b8b8'))

#Dynamics in the reproduction Buffer
reproBufferNoBoost <- noWinterBoost %>% 
  ggplot(.,aes(x = age/365, y = reproBuffer,
               group = scenario,
               fill = scenario)
  ) +
  scale_x_continuous(breaks = 0:6)+
  scale_y_continuous(limits = c(0,200), breaks = c(0,100,200)) +
  scale_fill_manual(values = c('#5e5e5e', '#cf1313'),
                    labels = c('seasonal regulation off','seasonal regulation on'),
                    guide = F) +
  geom_area(alpha = 0.85) +
  facet_wrap(~scenario, ncol = 1) +
  labs(x = 'years', y = 'dry weight (mg)') +
  theme(axis.ticks = element_blank(),
        axis.text = element_text(size = textSize),
        axis.title = element_text(size = textSize),
        legend.title = element_blank(),
        legend.background = element_blank(),
        legend.key = element_blank(),
        strip.text = element_blank(),
        legend.position = 'none',
        strip.background = element_blank(),
        panel.spacing = unit(2, "lines"),
        panel.background = element_rect(fill = NA, colour = '#b8b8b8'))


#Spawning:
spawningNoBoost <- noWinterBoost %>% 
  filter(repro == T) %>% 
  mutate(year = round(age/365),
         dayMonthYear = as.Date(paste('2010',
                                      month(referenceDate),
                                      day(referenceDate), sep = '-'
         ))) %>% 
  ggplot(.) +
  geom_rect(data = notSimulated, aes(xmin = xmin, xmax = xmax,
                                     ymin = ymin, ymax = ymax),
            fill = '#e3e3e3') +
  geom_segment(data = monthLine, aes(x = x, xend = xend,
                                     y = y, yend = yend,
                                     group = month),
               colour = '#707070') +
  geom_segment(data = halfCircles, 
               aes(x = x, xend = xend,
                   y = y, yend = yend,
                   group = month),
               colour = '#bababa', alpha = 0.8, 
               size = 0.3, linetype = 'solid')+
  scale_x_date(limits = as.Date(c('2010-01-01','2010-12-31')),
               breaks = c(seq(as.Date('2010-01-15'), as.Date('2010-03-15'), by = '1 month'),
                          seq(as.Date('2010-10-15'), as.Date('2010-12-15'), by = '1 month')),
               date_labels = '%b')+
  scale_y_continuous(limits = c(0,6.5)) +
  scale_fill_manual(values = c('#d73c43','#797979'),
                    guide = F) +
  scale_colour_manual(values = c('#d73c43','#797979'),
                      guide = F) +
  geom_tile(aes(x = dayMonthYear, y = year,
                group = scenario,
                colour = scenario,
                fill = scenario),height = 0.5, width = 10, alpha = 1) +
  annotate('text', x = rep(as.Date('2010-04-01'), times = 7),
           y = 0:6, label = as.character(1:7),
           angle = -90,colour = '#bababa',size = 3.6) +
  coord_polar() +
  labs(x = '', y = '') +
  theme(axis.ticks = element_blank(),
        legend.position = 'none',
        legend.title = element_blank(),
        legend.background = element_blank(),
        legend.key = element_blank(),
        axis.text.y = element_blank(),
        axis.text.x = element_text(angle = c(-15,-45,-75,-285,-315,-345),
                                   size = textSize),
        panel.spacing = unit(2, "lines"),
        panel.grid = element_blank(),
        panel.background = element_rect(fill = NA, colour = NA))


# plot_grid(growthNoBoost,reproBufferNoBoost,spawningNoBoost)
# plot_grid(growthBoost,reproBufferBoost,spawningBoost,nrow=1)

simulationExperiments <- plot_grid(growthNoBoost,growthBoost,
                                   reproBufferNoBoost,reproBufferBoost,
                                   spawningNoBoost,spawningBoost,
          ncol=2,
          scale = 0.9)
ggsave('/Users/dominik/Documents/KRILL_UFZ/Publications/01GrowthModel/plots/Figure4.png', 
       simulationExperiments, width = 15, height = 7)

#Save individual plots
ggsave('/Users/dominik/Documents/KRILL_UFZ/Publications/01GrowthModel/plots/growthNoBoost.svg', 
       growthNoBoost, width = 6, height = 4)
ggsave('/Users/dominik/Documents/KRILL_UFZ/Publications/01GrowthModel/plots/growthBoost.svg', 
       growthBoost, width = 6, height = 4)
ggsave('/Users/dominik/Documents/KRILL_UFZ/Publications/01GrowthModel/plots/reproBufferNoBoost.svg', 
       reproBufferNoBoost, width = 6, height = 4)
ggsave('/Users/dominik/Documents/KRILL_UFZ/Publications/01GrowthModel/plots/reproBufferBoost.svg', 
       reproBufferBoost, width = 6, height = 4)
ggsave('/Users/dominik/Documents/KRILL_UFZ/Publications/01GrowthModel/plots/spawningNoBoost.svg', 
       spawningNoBoost, width = 6, height = 4)
ggsave('/Users/dominik/Documents/KRILL_UFZ/Publications/01GrowthModel/plots/spawningBoost.svg', 
       spawningBoost, width = 6, height = 4)


#Analysis of size-Fluctuations in the "no winter Boost" scenario. The summary table shows the corresponding
#ratio of winter and summer length (and dry mass) for an individual for each year. Results should be meaningful
#from the 3rd year on.
sizeFluctuations <- noWinterBoost %>% 
  mutate(year = ceiling(age/365)) %>% 
  group_by(scenario, year) %>% 
  summarise(minLength = min(length),
            maxLength = max(length),
            minStructure = min(structuralBiomass),
            maxStructure = max(structuralBiomass)) %>% 
  mutate(lengthRel = round(minLength/maxLength * 100),
         weightRel = round(minStructure/maxStructure * 100))
View(sizeFluctuations)




