library(tidyverse)

setwd('/Users/dominik/Git/PhDThesis/growthModel')
source('functions/growthModel.R')
source('functions/defineConstants.R')
# =============================================================================
# Model settings. Choose environmental scenario, light and temperature regulation
# and reproduction options
# -----------------------------------------------------------------------------
# Choose an environmental scenario. 
# Options: paradise, palmer
inputScenario <- 'palmer'

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
years = 7
days = years * 365

# -----------------------------------------------------------------------------
#Call function which loads global variables/predefined constants, parameters
#and initial conditions
defineConstants()
# -----------------------------------------------------------------------------
# ============================================================================= #
# Run the model
krillGrowth() %>% 
  mutate(length = volumetricLength / shapeCorrection) %>% 
  mutate(referenceDate = as.Date(referenceDate, origin = '1970-01-01'))

