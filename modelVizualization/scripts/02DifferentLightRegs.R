##============================================================================================##
# 2020-07-03
# Dominik Bahlburg
# The model assumes that metabolic activity of krill is driven by light availability (and temperature)
# Because of data uncertainty, we test three different options of light-dependent regulation of
# metabolism. 
##============================================================================================##
#load packages
library(tidyverse)
library(lubridate)
library(openxlsx)
library(here)
##============================================================================================##
#Set up the model
source(here('functions','growthModel.R'))
source(here('functions','defineConstants.R'))
# =============================================================================
# Model settings. Choose environmental scenario, light and temperature regulation
# and reproduction options Euphausia--1
# Set up the invariate parts of the model
# -----------------------------------------------------------------------------
# Choose an environmental scenario. 
# Options: paradise, palmer
inputScenario <- 'palmer'
# Do you want to correct physiological rates for temperature and/or day length? (Default is 'on')
lightSwitch <- 'on'

tempSwitch <- 'on'

#Which scenario should trigger reproduction (Default is "fixedClutchSize")
reproScenario <- 'fixedClutchSize'
# -----------------------------------------------------------------------------
# =============================================================================
# Initialize birth day of krill and number of years which should be simulated
# -----------------------------------------------------------------------------
#Select the date on which the individual should be born (options: 1-365 with January 1st as day 1) 
#and the number of years for which the model should be executed.
#Temporal resolution of the model is 1 day. 
startDay = 0
years = 6
days = years * 365

# -----------------------------------------------------------------------------
#Call function which loads global variables/predefined constants, parameters
#and initial conditions

runModel <- function(lightMode, winterMode){
  winterBoost <<- winterMode
  defineConstants(defLightMode = lightMode)
  return(krillGrowth(returnWhat = 'all'))}
# ============================================================================= #
# ============================================================================= #
# ============================================================================= #
# Run the model and vary the light-dependent regression functions
# Light modes
winterModes <- c(T,F)
lightModes <- c('spline', 'linRegression', 'arctan')
lightModesResults <- crossing(lightModes, winterModes) %>% 
  rowwise() %>% 
  mutate(modelRun = list(runModel(lightMode = lightModes,
                                  winterMode = winterModes))) %>%
  unnest(modelRun)

#Extract the growth and energy dynamics
krillDynamics <- lightModesResults %>% 
  slice(1:(nrow(lightModesResults)/2)*2-1) %>% 
  unnest(modelRun)%>% 
  mutate(length = volumetricLength / shapeCorrection) %>% 
  mutate(referenceDate = as.Date(referenceDate, origin = '1970-01-01'))

energyDynamics <- lightModesResults %>% 
  slice(1:(nrow(lightModesResults)/2)*2) %>% 
  unnest(modelRun)
# ============================================================================= #
# Clean up environment
rm(list=setdiff(ls(), c('lightModes','krillDynamics','energyDynamics')))

# Calculate summary statistics:
# ============================================================================= #
# 1. Spawning success 
# 1.1 When did they spawn?
spawningDates <- energyDynamics %>% 
  filter(repro == T) 

# 1.2 how often did they spawn and how old at first spawning?
spawningEvents <-  energyDynamics %>% 
  filter(repro == T) %>% 
  group_by(winterModes, lightModes) %>% 
  summarise(spawningEvents = n(),
            ageAtFirstSpawning = min(age))
write.xlsx(spawningEvents, '/Users/dominik/Git/PhDThesis/growthModel/explanatoryMaterial/outputData/lightFunction/ageAndSpawnings.xlsx')

# 1.3 Capable of multiple spawning?
spawningWindows <- tibble(windowStart = as.Date(paste(2010:2016,'10-01',sep = '-')),
       windowEnd = as.Date(paste(2011:2017,'03-31',sep = '-')),
       windowNo = 1:7)

spawningWindowsOccs <-  spawningWindows %>% 
  rowwise() %>% 
  mutate(noEvents = list(spawningDates[between(spawningDates$referenceDate, windowStart, windowEnd),])) %>% 
  unnest(noEvents) %>% 
  select(winterModes, lightModes, windowNo, windowStart, referenceDate, windowEnd, repro) %>% 
  group_by(winterModes, lightModes, windowNo) %>% 
  summarise(multipleSpawning = ifelse(n()>1, 'yes','no'))
write.xlsx(spawningWindowsOccs, '/Users/dominik/Git/PhDThesis/growthModel/explanatoryMaterial/outputData/lightFunction/multipleSpawning.xlsx')

# 1.4 How many eggs?
fecundity <- energyDynamics %>% 
  mutate(referenceDate = lag(referenceDate)) %>% 
  filter(repro == T) %>% 
  left_join(krillDynamics, by = c('winterModes','lightModes','referenceDate')) %>% 
  mutate(noEggs = round(reproBuffer / 0.028)) %>% 
  group_by(winterModes, lightModes) %>% 
  summarise(totalEggs = sum(noEggs))
write.xlsx(fecundity, '/Users/dominik/Git/PhDThesis/growthModel/explanatoryMaterial/outputData/lightFunction/noEggs.xlsx')
# ============================================================================= #
# 2. Growth dynamics:
# 2.1 maximum size and fluctuations, number of days with energy deficit
krillSize <- krillDynamics %>% 
  left_join(., energyDynamics,  by = c('winterModes','lightModes','referenceDate')) %>% 
  group_by(winterModes, lightModes) %>% 
  summarise(maxLength = max(length),
            shrinkageDays = sum(energyDeficit > 0),
            assimilatedStructure = sum(energyDeficit))
write.xlsx(krillSize, '/Users/dominik/Git/PhDThesis/growthModel/explanatoryMaterial/outputData/lightFunction/vitality.xlsx')

# ============================================================================= #
# ============================================================================= #
# Visualize missing pieces
# ============================================================================= #
# 1. Visualize the spawning dates
spawning <- ggplot(spawningDates, aes(x = referenceDate, y = factor(lightModes,
                                                        levels = c('linRegression','arctan','spline')), fill = winterModes)) +
  geom_vline(xintercept = seq(as.Date('2010-01-01'), as.Date('2016-01-01'), by = '12 months'),
             colour = '#bababa', linetype = '13') +
  geom_tile(height = 0.25, width = 15) +
  scale_y_discrete(labels = c('linear regression','arctan','spline')) +
  scale_x_date(limits = c(min(energyDynamics$referenceDate, na.rm = T),
                          max(energyDynamics$referenceDate, na.rm = T)),
               breaks = seq(as.Date('2010-01-01'), as.Date('2016-01-01'), by = '12 months'),
               date_labels = '%b') +
  scale_fill_manual(values = c('#bf5c5c','#4a4a4a')) +
  facet_wrap(~winterModes, ncol = 2, 
             labeller = labeller(winterModes = c('FALSE' = 'Palmer', 'TRUE' = 'Palmer Winter Boost'))) +
  labs(x = '', y = '') +
  theme(strip.background = element_blank(),
        strip.text = element_text(face = 'bold'),
        axis.ticks = element_blank(),
        axis.text.y = element_text(face = 'bold'),
        legend.position = 'none',
        panel.spacing = unit(2, "lines"),
        panel.background = element_rect(fill = NA, colour = '#bfbfbf'))
ggsave('~/Documents/KRILL_UFZ/Publications/01GrowthModel/Plots/lightFunctions/spawning.svg', spawning, width = 8, height = 2.5)

reproDynamics <- krillDynamics %>% 
  mutate(reproBufferAdj = ifelse(lightModes == 'linRegression',
                                 reproBuffer,
                                 ifelse(lightModes == 'arctan',
                                        reproBuffer + 250,
                                        reproBuffer + 500))) %>%
  ggplot(.,aes(x = referenceDate, y = reproBufferAdj, 
               group = lightModes, colour = winterModes)) +
  geom_vline(xintercept = seq(as.Date('2011-01-01'), as.Date('2016-01-01'), by = '12 months'),
             colour = '#bababa', linetype = '13') +
  geom_line(size = .7) +
  geom_segment(aes(x = min(energyDynamics$referenceDate, na.rm = T), y = 0, 
                   xend = min(energyDynamics$referenceDate, na.rm = T), yend = 200),
               colour = '#8a8a8a', size = .35) +
  geom_segment(aes(x = min(energyDynamics$referenceDate, na.rm = T), y = 250, 
                   xend = min(energyDynamics$referenceDate, na.rm = T), yend = 450),
               colour = '#8a8a8a', size = .35) +
  geom_segment(aes(x = min(energyDynamics$referenceDate, na.rm = T), y = 500, 
                   xend = min(energyDynamics$referenceDate, na.rm = T), yend = 700),
               colour = '#8a8a8a', size = .35) +
  annotate('text', label = '0', 
           x = rep(min(energyDynamics$referenceDate, na.rm = T) - 90,times = 3),
           y = c(5,255,505),colour = '#8a8a8a', size = 3.6) +
  annotate('text', label = '200', 
           x = rep(min(energyDynamics$referenceDate, na.rm = T) - 90,times = 3),
           y = c(200,450,700),colour = '#8a8a8a', size = 3.6) +
  scale_colour_manual(values = c('#bf5c5c','#4a4a4a')) +
  scale_y_continuous(breaks = c(30,280,530), labels = c('linear regression','arctan','spline')) +
  scale_x_date(limits = c(min(energyDynamics$referenceDate, na.rm = T)-100,
                          max(energyDynamics$referenceDate, na.rm = T)),
               breaks = seq(as.Date('2011-01-01'), as.Date('2016-01-01'), by = '12 months'),
               date_labels = '%b') +
  facet_wrap(~winterModes, ncol = 2, 
             labeller = labeller(winterModes = c('FALSE' = 'Palmer', 'TRUE' = 'Palmer Winter Boost'))) +
  labs(x = '', y = '') +
  theme(strip.background = element_blank(),
        strip.text = element_text(face = 'bold'),
        axis.ticks = element_blank(),
        axis.text.y = element_text(face = 'bold'),
        legend.position = 'none',
        panel.spacing = unit(2, "lines"),
        panel.background = element_rect(fill = NA, colour = NA))
ggsave('~/Documents/KRILL_UFZ/Publications/01GrowthModel/Plots/lightFunctions/reproBuffer.png', reproDynamics, width = 10, height = 5)


growthDynamicsPlot <- 
  krillDynamics %>% 
  mutate(lengthAdj = ifelse(lightModes == 'linRegression',
                                 length,
                                 ifelse(lightModes == 'arctan',
                                        length + 80,
                                        length + 160))) %>%
  ggplot(.,aes(x = referenceDate, y = lengthAdj, 
               group = lightModes, colour = winterModes)) +
  geom_vline(xintercept = seq(as.Date('2011-01-01'), as.Date('2016-01-01'), by = '12 months'),
             colour = '#bababa', linetype = '13') +
    geom_segment(aes(x = min(energyDynamics$referenceDate, na.rm = T), y = 0, 
                     xend = min(energyDynamics$referenceDate, na.rm = T), yend = 60),
                 colour = '#8a8a8a', size = .35) +
    geom_segment(aes(x = min(energyDynamics$referenceDate, na.rm = T), y = 80, 
                     xend = min(energyDynamics$referenceDate, na.rm = T), yend = 60+80),
                 colour = '#8a8a8a', size = .35) +
    geom_segment(aes(x = min(energyDynamics$referenceDate, na.rm = T), y = 160, 
                     xend = min(energyDynamics$referenceDate, na.rm = T), yend = 60+160),
                     colour = '#8a8a8a', size = .35) +
    annotate('text', label = '0', 
             x = rep(min(energyDynamics$referenceDate, na.rm = T) - 90,times = 3),
             y = c(5,85,165),colour = '#8a8a8a', size = 3.6) +
    annotate('text', label = '60', 
             x = rep(min(energyDynamics$referenceDate, na.rm = T) - 90,times = 3),
             y = c(60,140,220),colour = '#8a8a8a', size = 3.6) +
  geom_line(size = .7) +
  scale_colour_manual(values = c('#bf5c5c','#4a4a4a')) +
  scale_y_continuous(breaks = c(20,100,180), labels = c('linear regression','arctan','spline')) +
  scale_x_date(limits = c(min(energyDynamics$referenceDate, na.rm = T)-100 ,
                          max(energyDynamics$referenceDate, na.rm = T)),
               breaks = seq(as.Date('2011-01-01'), as.Date('2016-01-01'), by = '12 months'),
               date_labels = '%b') +
  facet_wrap(~winterModes, ncol = 2, 
             labeller = labeller(winterModes = c('FALSE' = 'Palmer', 'TRUE' = 'Palmer Winter Boost'))) +
  labs(x = '', y = '') +
  theme(strip.background = element_blank(),
        strip.text = element_text(face = 'bold'),
        axis.ticks = element_blank(),
        axis.text.y = element_text(face = 'bold'),
        legend.position = 'none',
        panel.spacing = unit(2, "lines"),
        panel.background = element_rect(fill = NA, colour = NA))
growthDynamicsPlot

ggsave('~/Documents/KRILL_UFZ/Publications/01GrowthModel/Plots/lightFunctions/growthDynamics.png', growthDynamicsPlot, width = 10, height = 5)



