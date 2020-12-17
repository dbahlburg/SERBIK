#This is the executing script for the krill growth model
#all parameters declared in the beginning are then passed on to the growth function
#The output is a tibble containing the model results
library(tidyverse)
library(lubridate)
library(here)
library(suncalc)

source(here('functions','growthModel.R'))
source(here('functions','defineConstants.R'))
# =============================================================================
# Model settings. Choose environmental scenario, light and temperature regulation
# and reproduction options Euphausia--1
# -----------------------------------------------------------------------------
# Choose an environmental scenario. 
# Options: paradise, palmer
inputScenario <- 'palmer'
winterBoost <<- T
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
noDays = years * 365

# -----------------------------------------------------------------------------
#Call function which loads global variables/predefined constants, parameters
#and initial conditions
defineConstants()
# -----------------------------------------------------------------------------
# ============================================================================= #
# Run the model
modelResults <- krillGrowth() %>% 
  mutate(length = volumetricLength / shapeCorrection) %>% 
  mutate(referenceDate = as.Date(referenceDate, origin = '1970-01-01'))

# ============================================================================= #
# create data set containing date ranges for highlighting certain ranges 
# during the year (winter period and highly productive periods)
# -----------------------------------------------------------------------------
dateRanges <- data.frame(
  startDate = c(seq(as.Date("2010-04-01"), as.Date("2010-04-01") + years * 365, "1 year"),
            seq(as.Date("2010-12-01"), as.Date("2010-12-01") + years * 365, "1 year")),
  endDate = c(seq(as.Date("2010-09-30"), as.Date("2010-09-30") + years* 365, "1 year"),
          seq(as.Date("2011-01-31"), as.Date("2011-01-31") + years* 365, "1 year")),
  group = c(rep('winter', times = length(seq(as.Date("2010-04-01"), as.Date("2010-04-01") + years * 365, "1 year"))),
            rep('summer', times = length(seq(as.Date("2010-09-30"), as.Date("2010-09-30") + years* 365, "1 year"))))
) %>% mutate(refStartDay = as.numeric(difftime(startDate, first(modelResults$referenceDate))),
             refEndDay = as.numeric(difftime(endDate, first(modelResults$referenceDate))),
             startDay = refStartDay /365,
             endDay = refEndDay/365)
# -----------------------------------------------------------------------------
# plot the results
modelResults %>%
  mutate(eggBuffer = ifelse(eggBuffer < 0,0,eggBuffer),
         age = age/365) %>%
  gather(variable, value, -referenceDate, -age) %>%
  ggplot(.,aes(x = age, y = value)) +
    # geom_rect(data = dateRanges, aes(xmin = startDay , xmax = endDay,
    #                                 ymin = 0, ymax = Inf,
    #                                 fill = group),
    #          inherit.aes=FALSE, alpha = 0.25) +
  #scale_x_date(limits = c(), date_breaks = '12 months', date_labels = '%Y') +
  scale_fill_manual(values = c('#e8a554','#878787')) +
    geom_line(colour = '#3b3b3b', size = 0.7)+
    facet_wrap(~variable, scales = 'free') +
  labs(x = 'years')+
  theme(strip.background = element_blank(),
        panel.background = element_rect(fill = NA, colour = '#525252'))
