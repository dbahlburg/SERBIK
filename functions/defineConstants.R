defineConstants <- function(defMaxAssimilation = 0.087,
                            defKappaVal = 0.7,
                            defLightMode = 'linRegression'){
  #-------------------------------------------------------------------------------#
  #Definition of parameters
  #-------------------------------------------------------------------------------#
  # ============================================================================= #
  # Fixed model parameters 
  # ----------------------------------------------------------------------------- #
  # maximum area-specific assimilation rate (scales assimilation of energy)
  #maxAssimilation <<- 0.044; #mgDW mm-2 d-1
  maxAssimilation <<- defMaxAssimilation;
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
  kappaVal <<- defKappaVal
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
  lightMode <<- defLightMode
  }
