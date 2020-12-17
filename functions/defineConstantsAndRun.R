defineConstantsAndRun <- function(X, output, winterBoostMode){
  library(tidyverse)
  library(lubridate)
  setwd('/Users/dominik/Git/PhDThesis/growthModel')
  source('functions/growthModel.R')
  
  
  X <- t(as.numeric(X))
  #-------------------------------------------------------------------------------#
  #Definition of parameters
  #-------------------------------------------------------------------------------#
  
  
  # =============================================================================
  # Model settings. Choose environmental scenario, light and temperature regulation
  # and reproduction options Euphausia--1
  # -----------------------------------------------------------------------------
  # Choose an environmental scenario. 
  # Options: paradise, palmer
  inputScenario <<- 'palmer'
  winterBoost <<- winterBoostMode#T
  # Do you want to correct physiological rates for temperature and/or day length? (Default is 'on')
  lightSwitch <<- 'on'
  tempSwitch <<- 'on'
  
  #Which scenario should trigger reproduction (Default is "fixedClutchSize")
  reproScenario <<- 'fixedClutchSize'
  # -----------------------------------------------------------------------------
  # =============================================================================
  # Initialize birth day of krill and number of years which should be simulated
  # -----------------------------------------------------------------------------
  #Select the date on which the individual should be born (options: 1-365 with January 1st as day 1) 
  #and the number of years for which the model should be executed.
  #Temporal resolution of the model is 1 day. 
  startDay <<- 0
  years  <- 6
  noDays <<- years * 365
  # ============================================================================= #
  # Fixed model parameters 
  # ----------------------------------------------------------------------------- #
  # maximum area-specific assimilation rate (scales assimilation of energy)
  #maxAssimilation <<- 0.044; #mgDW mm-2 d-1
  maxAssimilation <<- X[1]#0.087;
  # volume-specific somatic maintenance flux
  Jsm<<-X[2]#0.0032; #mgDW mm-3 d-1
  # yield of assimilates on food (carbon based)
  foodConversion<<-X[3]#0.8; # mgC mg-1C
  # yield of structure on assimilates
  growthConversion<<-X[4]#0.8; #mgDW mgDW-1
  # yield of assimilates on structure (starvation)
  structureConversion<<-X[5]#0.8; #mgDW mgDW-1
  # yield of storage buffer on assimilates
  storageConversion<<-X[6]#0.95; #mgDW mgDW-1
  # fraction allocation to soma
  kappaVal <<- X[7]#0.7
  # dry weight of a single egg
  weightEgg<<-0.028; #mgDW
  #spawning window (Octobe to March)
  spawningWindow <<- c(10,11,12,1,2,3)
  # physical length at start of juvenile stage
  lengthJuvenile<<-11; #mm
  # physical length at start of adult stage
  lengthAdult<<-35; #mm
  # =============================================================================
  # Conversion factors
  # -----------------------------------------------------------------------------
  # dry weight density
  dv<<-0.22; #mgDW mm-3
  # shape correction coefficient (for Lw > 2mm)
  shapeCorrection<<-0.2;
  # carbon weight per dry weight
  #wC = 0.45; # mgC mgDW-1
  # carbon weight per oxygen volume in respiration
  #wCO = 0.48; #mgC ml-1 O2
  # =============================================================================
  # Choose an environmental scenario. 
  # Options: ideal, palmer
  # -----------------------------------------------------------------------------
  
  #enter initial conditions:
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
                   age = initAge,
                   assEnergy = 0,
                   somMaintenance = 0,
                   energyDeficit = 0,
                   reproBufferAss = 0,
                   structBiomassAss = 0,
                   netGrowth = 0,
                   repro = F)

  #Set the light regulation mode
  lightMode <<- 'linRegression'
  
  modResults <- krillGrowth()
  
  if (output == 'maxSize'){
    max(modResults$volumetricLength, na.rm = T)/shapeCorrection
  } else if (output == 'dayOfMaturity'){
    first(filter(modResults, stage == 3)$age)
  } else if (output == 'noSpawnings'){
    sum(modResults$repro)
  } else if (output == 'noEggs'){
    round(sum(slice(modResults, which(TRUE == modResults$repro) - 1)$reproBuffer/weightEgg))
  }
  
  
}
  
  
  
  
  
  
  
