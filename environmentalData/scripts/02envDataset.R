##============================================================================================##
# 2019-10-25
# Dominik Bahlburg
# The purpose of this script is to create a dataset which contains realistic data for
# day length, water temperature and food availability for a typical krill habitat.
# The dataset will subsequently be used for testing the behaviour of the krill growth model
# under realistic conditions.
##============================================================================================##
# Load necessary packages
library(tidyverse)
library(suncalc)
library(lubridate)
library(cowplot)
##============================================================================================##
# Load datasets from the Palmer LTER program
# the first dataset contains data on primary production
# the second dataset contains data on zooplankton concentration
# the third and fourth dataset contains data on POC as a general measure for food concentration
chlorophyll <- read_csv("~/Git/phdThesis/jagerRavagnan/RStuff/data/chlorophyll.csv")
chlorophyllStation <- read_csv("~/Git/phdThesis/jagerRavagnan/RStuff/data/chlorophyllPalmer.csv")
zooplankton <- read_csv("~/Git/phdThesis/jagerRavagnan/RStuff/data/zooplankton.csv")
cruisePOC <- read_csv("~/Git/phdThesis/jagerRavagnan/RStuff/data/cruisePOC.csv")
stationPOC <- read_csv("~/Git/phdThesis/jagerRavagnan/RStuff/data/stationPOC.csv")
tempPalmer <- read_csv("~/Git/phdThesis/jagerRavagnan/RStuff/data/temperaturePalmer.csv")
##============================================================================================##
## Check out the chlorophyll dataset
chlorophyll <- chlorophyll %>% 
  filter(ChlorophyllMgM3 > 0 & ChlorophyllMgM3 < 100) %>% 
  filter(Depth < 50)

chlorophyllMonthly <- chlorophyll %>% 
  mutate(month = floor_date(DatetimeGMT, "month"),
         month2 = as.POSIXct(paste('2019-',month(DatetimeGMT, label = F),'-01',sep = ''))) %>% 
  group_by(month2) %>% 
  summarise(meanChl = mean(ChlorophyllMgM3, na.rm = T),
            sdChl = sd(ChlorophyllMgM3, na.rm = T),
            meanPhaeo = mean(PhaeopigmentMgM3, na.rm = T),
            sdPhaeo = sd(PhaeopigmentMgM3, na.rm = T),
            samples = n()) %>% 
  rbind(.,tibble(month2 = as.POSIXct('2019-12-01'), meanChl = 0, sdChl = 0, meanPhaeo = 0, sdPhaeo = 0, samples = 0))

chlaStation <- chlorophyllStation %>% 
  mutate(month = as.POSIXct(paste('2019-',month(Date, label = F),'-01',sep = ''))) %>% 
  group_by(month) %>% 
  summarise(meanChla = mean(Chla),
            sdChla = sd(Chla)) 

# chlorophyll %>% 
#   mutate(date = floor_date(DatetimeGMT, unit = 'day')) %>% 
#   filter(date < "2010-09-04") %>% 
#   group_by(date) %>% 
#   summarise(meanChl = mean(ChlorophyllMgM3)) %>% 
#   ggplot(., aes(x = date, y = meanChl)) +
#     geom_point() +
#     scale_x_datetime(date_breaks = '1 month', date_labels = '%b')

#Plot monthly means in barplot
chla <- chlorophyllMonthly %>% 
  select(month = month2, meanChla = meanChl, sdChla = sdChl) %>% 
  mutate(origin = 'cruise') %>% 
  rbind(chlaStation %>% mutate(origin = 'station'))
ggplot(chla, aes(x = month, y = meanChla, group = origin, fill = origin)) +
    geom_bar(stat = 'identity',position = 'dodge')+
    geom_errorbar(aes(ymin = meanChla - sdChla, ymax = meanChla + sdChla), position = "dodge") +
    labs(x = '',y = 'Chlorophyll a ug/L') +
    scale_x_datetime(date_breaks = '1 month', date_labels = '%b') +
    theme(panel.background = element_rect(fill = NA, colour = '#616161'))
    
  

chloro <- ggplot(chlorophyllMonthly, aes(x = month2, y = meanChl)) +
  geom_bar(stat = 'identity', fill = '#699470') +
  labs(x = '',y = 'Chlorophyll a ug/L') +
  scale_x_datetime(date_breaks = '1 month', date_labels = '%b') +
  theme(panel.background = element_rect(fill = NA, colour = '#616161'))
chloro
  
#violin plot
chlorophyll %>% 
  mutate(month = as.POSIXct(paste('2019-',month(DatetimeGMT, label = F),'-01',sep = ''))) %>% 
  ggplot(.,aes(x = as.factor(month), y = ChlorophyllMgM3, fill = as.factor(month))) +
  geom_violin() +
  geom_point()+
  geom_boxplot(width = 0.075, colour = '#c7c7c7',outlier.shape = NA) +
  scale_y_log10(breaks = c(0.1,1,10), labels = c('0.1','1','10'), limits = c(0.05,50))
##============================================================================================##
## Check out the poc dataset from Palmer Station
stationPOC <- stationPOC %>% 
  filter(Depth < 50)
stationPOCMonthly <- stationPOC %>% 
  mutate(month = floor_date(DatetimeGMT, "month"),
         month2 = as.POSIXct(paste('2019-',month(DatetimeGMT, label = F),'-01',sep = ''))) %>% 
  group_by(month2) %>% 
  summarise(meanC = mean(CUgL, na.rm = T))

pocPalmer <- ggplot(stationPOCMonthly, aes(x = month2, y = meanC)) +
  geom_bar(stat = 'identity', fill = '#699470') +
  scale_x_datetime(date_breaks = '1 month', date_labels = '%b') +
  labs(x = '', y = 'C in ug L') +
  ylim(c(0,350))+
  theme(panel.background = element_rect(fill = NA, colour = '#616161'))
##============================================================================================##
## Check out the poc dataset from Cruises
cruisePOC <- cruisePOC %>% 
  filter(Depth < 50)
cruisePOCMonthly <- cruisePOC %>% 
  mutate(month = floor_date(DatetimeGMT, "month"),
         month2 = as.POSIXct(paste('2019-',month(DatetimeGMT, label = F),'-01',sep = ''))) %>% 
  group_by(month2) %>% 
  summarise(meanC = mean(CarbonUgL, na.rm = T)) %>% 
  mutate(origin = 'cruise') %>% 
  rbind(stationPOCMonthly %>% mutate(origin = 'station'))

pocCruise <- ggplot(cruisePOCMonthly, aes(x = month2, y = meanC, fill = origin)) +
  geom_bar(stat = 'identity', position = 'dodge')+
  #geom_bar(stat = 'identity', fill = '#699470') +
  scale_x_datetime(date_breaks = '1 month', date_labels = '%b') +
  labs(x = '', y = 'C in ug L') +
  ylim(c(0,350))+
  theme(panel.background = element_rect(fill = NA, colour = '#616161'))
pocCruise
plot_grid(chloro, pocPalmer,pocCruise, nrow = 3)
##============================================================================================##
## calculate sunlight data
sunData <- tibble(date = seq(ymd("2010-1-1"), ymd("2011-1-1"), by = "day"),
                   lat = -64.055,
                   lon = -64.055) %>% 
  getSunlightTimes(data = .,
                   keep = c('sunrise','sunset')) %>% 
  mutate(dayLength = as.numeric(difftime(sunset, sunrise, units = 'hours'))) %>% 
  rename(month = date)
##============================================================================================##
## process temperature data
tempPalmerSum <- tempPalmer %>% 
  filter(SST > -30) %>% 
  filter(TempAverage > -30) %>% 
  mutate(day = as.Date(paste('2019-',month(Date),'-', day(Date),sep = ''))) %>% 
  group_by(day) %>% 
  summarise(temp = mean(TempAverage, na.rm = T),
            sst = mean(SST, na.rm = T)) %>% 
  filter(!is.na(day))
##============================================================================================##
## create env-dataset
years = 6
envData <- tibble(date = seq(ymd("2010-1-1"), ymd("2010-1-1") + 365 * years, by = "days")) %>% 
  mutate(lat = -64.055,
         lon = -64.055) %>% 
  getSunlightTimes(data = .,
                   keep = c('sunrise','sunset')) %>% 
  mutate(dayLength = as.numeric(difftime(sunset, sunrise, units = 'hours'))) %>% 
  mutate(month = as.POSIXct(paste('2019-',month(date, label = F),'-01',sep = '')),
         day = as.Date(paste('2019-',month(date),'-', day(date),sep = ''))) %>% 
  left_join(chlaStation, by = 'month') %>% 
  left_join(tempPalmerSum, by = 'day') %>% 
  select(-lat, -lon, -sunrise, -sunset, -month, -day) %>% 
  filter(!is.na(sst))
write.csv(envData, 'Git/phdThesis/jagerRavagnan/RStuff/data/envData.csv')

# 
# envData %>% 
#   select(date, sst, temp) %>% 
#   gather(variable, value, -date) %>% 
#   filter(date < '2011-01-01') %>% 
#   ggplot(., aes(x = date, y = value, colour = variable)) +
#     geom_point() +
#   geom_smooth()
#Sun angle which defines night: -0.833 (Steinberg et al. 2015, https://doi.org/10.1016/j.dsr.2015.02.009)
