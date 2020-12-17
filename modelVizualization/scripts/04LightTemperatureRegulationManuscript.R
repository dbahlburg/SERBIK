#24.08.2020
#Dominik Bahlburg
#In this script I create graphs that explain the light- and temperature-dependent regulation 
#of metabolism and feeding activity. 
library(tidyverse)
library(cowplot)
library(ggimage)
library(suncalc)
library(here)

source(here('functions','lightCorrection.R'))
source(here('functions','temperatureCorrection.R'))
source(here('functions','generatePalmer.R'))
source(here('functions','defineConstants.R'))
source(here('functions','feedingResponse.R'))
source(here('functions','growthModel.R'))

legendLine <- tibble(date = seq(from = as.POSIXct('2010-06-20 00:00:00'),
                                to = as.POSIXct('2010-07-15 00:00:00'),
                                length.out = 10), 
                     y = 0.1 * seq(0,0.4, length.out = 10) + 1.28 + rnorm(n = 10, mean = 0, sd = 0.008))

pictograms <- tibble(image = c("explanatoryMaterial/graphics/sunPicto2.png",
                               "explanatoryMaterial/graphics/tempPicto3.png"))


# =============================================================================#
# Create plot that visualizes the effects of day length/latitude on the seasonal regulation
# of metabolism and feeding activity.
startDay <- 1
noDays <- 365
indSize <- 9
defineConstants()

palmer <- generatePalmer(boostWinter = F) %>% 
  mutate(temperatureRegulation = temperatureCorrection(temperature = temperature),
         lightRegulation = lightCorrection(dayLength = dayLength),
         totalRegulation = temperatureRegulation * lightRegulation)

foodReg <- palmer %>%
  rowwise()%>%
  mutate(foodUptake = maxAssimilation * indSize^2 * foodConversion * 
           totalRegulation * feedingFunction(chosenFunction = 'HollingType2',
                                      feedingMode = 0, foodConc = foodConc, 
                                      stage = 3),
         regulation = 'on') 
  
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
foodUptake <- noWinterBoost %>% 
  filter(between(referenceDate, as.Date('2015-01-01'),as.Date('2016-01-01'))) %>% 
  select(date = referenceDate,
         energyDemand = somMaintenance,
         foodUptake = assEnergy,
         regulation = scenario) %>% 
  mutate(deficit = foodUptake - energyDemand)
foodUptake$regulation <- factor(foodUptake$regulation, 
                                labels=c('energy~balance~bold(without)~metabolic~scaling',
                                         'energy~balance~bold(with)~metabolic~scaling'))

foodUptake %>% 
  filter(deficit < 0) %>% 
  group_by(regulation) %>% 
  summarise(totalDef = sum(deficit))

effectPlot <- foodUptake %>% 
  mutate(week = week(date)) %>% 
  group_by(week, regulation) %>% 
  summarise(deficit = sum(deficit),
            date = first(date)) %>% 
  mutate(phase = ifelse(deficit > 0 & month(date) < 6, 'noDeficit1',
                        ifelse(deficit > 0 & month(date) > 6, 'noDeficit2',
                               'deficit'))) %>% 
  ggplot(., aes(x = date, y = deficit, fill = phase, colour = phase)) +
  geom_hline(yintercept = 0, colour = '#737373') +
  geom_segment(aes(xend = date, y = 0, yend = deficit), size = 0.9) +
  geom_point(shape = 21, colour = '#ffffff', size = 1.75) +
  scale_y_continuous(limits = c(-12, 22), breaks = c(-10,0,10,20)) +
  scale_x_date(date_breaks = '3 month', date_labels = '%b', limits = c(as.Date('2015-01-01'),
                                                                       as.Date('2016-01-01'))) +
  scale_fill_manual(values = c('#87597f','#bdbdbd','#bdbdbd'), guide = F) +
  scale_colour_manual(values = c('#87597f','#bdbdbd','#bdbdbd'), guide = F) +
  facet_grid(~regulation, 
             labeller = label_parsed) +
  labs(y = expression(Delta[E[ass]*E[met]]~'in'~mgC),#'difference assimilated energy \n and metabolic costs \n in mgC',
       x = '') +
  theme(panel.background = element_rect(fill = NA, colour = '#6b6b6b'),
        strip.background = element_rect(fill = NA),
        strip.text = element_text(size = 14),
        panel.grid = element_line(colour = NA),
        axis.text = element_text(size = 14),
        axis.title = element_text(size = 14),
        axis.ticks.x = element_blank(),
        plot.title = element_text(size = 14, hjust = 0.5),
        legend.text = element_text(size = 14),
        legend.background = element_rect(fill = NA),
        plot.margin=grid::unit(c(1,1,1,1), "mm"))

regulationPlot <- foodReg %>% 
  mutate(week = week(date)) %>% 
  group_by(week) %>% 
  summarise(totalRegulation = mean(totalRegulation),
            date = as.POSIXct(first(date))) %>% 
  mutate(plotTitle = 'annual metabolic scaling') %>% 
  ggplot(., aes(x = date, y = totalRegulation)) +
  geom_line(data = foodReg,aes(x = as.POSIXct(date), y = lightRegulation),
            colour = '#bdbdbd', size = 0.7, alpha = 0.6) +
  geom_line(data = foodReg,aes(x = as.POSIXct(date), y = temperatureRegulation),
            colour = '#bdbdbd', size = 0.7, alpha = 0.6) +
  geom_segment(aes(xend = date, y = 0, yend = totalRegulation),
               size = 0.9, colour = '#ebc46e') +
  geom_point(shape = 21, colour = '#ffffff',
             size = 1.75, fill = '#ebc46e') +
  facet_grid(~plotTitle)+
  scale_y_continuous(breaks = c(0,0.5,1), limits = c(0,1.4)) +
  scale_x_datetime(date_breaks = '3 month', date_labels = '%b', limits = c(as.POSIXct('2010-01-01'),
                                                                           as.POSIXct('2011-01-01'))) +
  geom_image(data = pictograms, aes(x = as.POSIXct(c('2010-06-21', '2010-07-15')),
                                    y = c(0.45, 0.95),
                                    image = image), 
             size = c(.06,.026), asp = 1.4)+
  labs(x = '', y = 'metabolic scaling factor') +
  geom_segment(aes(x = as.POSIXct('2010-02-15'),
                   xend = as.POSIXct('2010-02-15'),
                   y = 1.255, yend = 1.33),
               size = 1.4, colour = '#ebc46e') +
  geom_point(aes(x = as.POSIXct('2010-02-15'), 
                 y = 1.33),
             shape = 21, colour = '#ffffff',
             size = 2, fill = '#ebc46e') +
  annotate('text', x = as.POSIXct(c('2010-02-28', '2010-07-22')), y = c(1.3, 1.3),
           label = c('total effect', 'single effects'),
           hjust = 0, size = 5) +
  geom_line(data = legendLine, aes(x = date, y = y),
            colour = '#bdbdbd', size = 1.4, alpha = 0.6) +
  geom_segment(aes(x = as.POSIXct('2010-02-01'),
                   xend = as.POSIXct('2010-12-01'),
                   y = 1.23,
                   yend = 1.23),
                   size = 0.3, colour = '#bdbdbd') +
  theme(panel.background = element_rect(fill = NA, colour = '#6b6b6b'),
        strip.background = element_rect(fill = NA),
        strip.text = element_text(size = 14),
        axis.text = element_text(size = 14),
        axis.title = element_text(size = 14),
        axis.ticks.x = element_blank(),
        plot.title = element_text(size = 14, hjust = 0.5),
        panel.grid = element_line(colour = NA),
        legend.text = element_text(size = 14),
        legend.background = element_rect(fill = NA),
        plot.margin=grid::unit(c(1,1,1,1), "mm"))

figure3<-plot_grid(regulationPlot, effectPlot, rel_widths = c(1/3,2/3))

