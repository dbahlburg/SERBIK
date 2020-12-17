library(tidyverse)
setwd('/Users/dominik/Git/PhDThesis/growthModel')
source('functions/growthModel.R')
source('functions/defineConstants.R')
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
days = years * 365

# -----------------------------------------------------------------------------
#Call function which loads global variables/predefined constants, parameters
#and initial conditions
defineConstants()
varNames <- ls()
varValues <- lapply(varNames,get)
variables <- tibble(varName = varNames,
                    varValue = varValues) %>% 
  rowwise() %>% 
  mutate(varType = typeof(varValue)) %>% 
  filter(varType == 'double')
# -----------------------------------------------------------------------------
# ============================================================================= #
# Run the model
modelResults <- krillGrowth() %>% 
  mutate(length = volumetricLength / shapeCorrection) %>% 
  mutate(referenceDate = as.Date(referenceDate, origin = '1970-01-01'))







