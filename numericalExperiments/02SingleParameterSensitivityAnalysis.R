##============================================================================================##
# 2020-07-10
# Dominik Bahlburg
# Single parameter sensitivity analysis of the ERIK-model. 
# Here we vary all parameters one after another and check their impact on the simulation results.
# In total there are  parameters that we are varying
##============================================================================================##
#load packages
library(tidyverse)
library(lubridate)
library(openxlsx)
##============================================================================================##
#Set up the model
setwd('/Users/dominik/Git/PhDThesis/growthModel')
source('functions/growthModel.R')
##============================================================================================##
# Method: There is a total of x parameters that we want to vary. The question is now by how much each parameter
# should be varied and why. Many parameters are taken from Jager and Ravagnan (2015), others are derived from 
# literature data and others are adjusted. If possible, we tried to calculate confidence intervals (CI) for parameter
# values, especially for those derived from literature data. CI's were estimated via bootstrapping.
# This leads to two different classes of parameters with respective variation scenarios:
# 1. Parameters which were not directly derived from empirical data are varied +- 10% with 20 "steps"
# 2. Parameters which were directly derived from literature are varied within their 95% CI with 20 "steps"
#Which parameter values should vary and how much?
##============================================================================================##
# Parameters which fall into category 1 (+- 10%)
maxAssimilation <<- 0.087;
# volume-specific somatic maintenance flux
Jsm<<-0.0032; #mgDW mm-3 d-1
# yield of assimilates on food (carbon based)
foodConversion<<-0.8; # mgC mg-1C
# yield of structure on assimilates
growthConversion<<-0.8; #mgDW mgDW-1
# yield of assimilates on structure (starvation)
structureConversion<<-0.8; #mgDW mgDW-1
# yield of storage buffer on assimilates
storageConversion<<-0.95; #mgDW mgDW-1
# fraction allocation to soma
#kappaVal <<- 0.75; #
kappaVal <<- 0.7

##============================================================================================##
# Parameters which fall into category 2 (confidence interval)

  initReferenceDate = as.Date(startDay - 1, origin = "2010-01-01")
  initStructuralBiomass = 0.0001
  initEggBuffer = weightEgg
  initReproBuffer = 0
  initVolumetricLength = (initStructuralBiomass/dv)^(1/3);
  initStage = 1
  initAge = 1
  
  krill <<- tibble(referenceDate = initReferenceDate,
                   structuralBiomass = initStructuralBiomass,
                   eggBuffer = initEggBuffer,
                   reproBuffer = initReproBuffer,
                   volumetricLength = initVolumetricLength,
                   stage = initStage,
                   age = initAge)
  
  energyDynamics <<- tibble(referenceDate = initReferenceDate,
                            age = 0,
                            assEnergy = 0,
                            somMaintenance = 0,
                            energyDeficit = 0,
                            reproBufferAss = 0,
                            structBiomassAss = 0,
                            netGrowth = 0,
                            repro = F) 



