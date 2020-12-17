krillGrowth <- function(returnWhat = 'krill'){
  #The core growth function of krill
  source(here('functions','feedingResponse.R'))
  source(here('functions','lightCorrection.R'))
  source(here('functions','generateEnvironment.R'))
  source(here('functions','temperatureCorrection.R'))
  source(here('functions','triggerReproduction.R'))
  
  environment <- generateEnvironment(scenario = inputScenario)
  
  for (index in 2:noDays){
    
    # =============================================================================
    #variables that need to be updated in each timestep:
    # update stage information
    # stage <- ifelse(krill$eggBuffer[index-1]/shapeCorrection > 0,
    #                 1,
    #                 ifelse(krill$eggBuffer[index-1]/shapeCorrection <= 0 & krill$volumetricLength[index-1]/shapeCorrection < lengthAdult,
    #                        2,3))
    
    stage <- ifelse(krill$eggBuffer[index-1] > 0,
                    1,
                    ifelse(krill$volumetricLength[index-1]/shapeCorrection > lengthAdult,
                           3,2))
    # =============================================================================
    # calculate assimilated energy (carbon equivalent)
    assEnergy <- maxAssimilation * krill$volumetricLength[index-1]^2 * 
                  feedingFunction(chosenFunction = 'HollingType2', stage, 
                                  feedingMode = krill$eggBuffer[index-1],
                                  foodConc = environment$foodConc[index-1], k = 1) *
                  temperatureCorrection(switched = tempSwitch, temperature = environment$temperature[index-1])  * 
                  lightCorrection(switched = lightSwitch, dayLength = environment$dayLength[index-1],
                                  mode = lightMode, exactMonth = (environment$dayNo[index-1]/max(environment$dayNo)*11) + 1) * 
                  foodConversion
                  
    #minimum required energy (in carbon equivalent): somatic maintenance
    somMaintenance <- Jsm * krill$volumetricLength[index-1]^3 *
      temperatureCorrection(switched = tempSwitch, temperature = environment$temperature[index-1]) * 
      lightCorrection(switched = lightSwitch, dayLength = environment$dayLength[index-1],
                      mode = lightMode, exactMonth = (environment$dayNo[index-1]/max(environment$dayNo)*11) + 1) 
    
    #calculate potential energy deficit. Because the conversion efficiency of structure into energy is not perfect (0.8),
    #the amount of missing carbon is divided by structureConversion
    energyDeficit <- ifelse(assEnergy * kappaVal - somMaintenance < 0, abs(assEnergy * kappaVal - somMaintenance), 0)/structureConversion
    
    #now calculate where the energy deficit is drawn from: either from the reproduction buffer or from the 
    #structural biomass in case the reproduction buffer is depleted
    reproBufferAss <- ifelse(energyDeficit > 0 & krill$reproBuffer[index-1] >= energyDeficit, energyDeficit, 
                             ifelse(energyDeficit > 0 & krill$reproBuffer[index-1] < energyDeficit, krill$reproBuffer[index-1], 0)
                             )
    structBiomassAss <- ifelse(krill$reproBuffer[index-1] < energyDeficit, abs(krill$reproBuffer[index-1] - energyDeficit),0)
    
    #calculate energy that will be allocated to somatic growth.
    #three scenarios. Each scenario is (de)activated by boolean switches
    #1) enough energy has been assimilated, a fraction covers somMaintenance, the remainder goes into growth (line 1)
    #2) assimilated food (fraction kappa) does not suffice to cover somatic maintenance: energy is drawn from the reproductive branch (growth = 0)
    #3) assimilated food + energy in reproduction buffer do not suffice to cover somatic maintenance: energy is drawn from the structural biomass
    
    growthEnergy <- ifelse(krill$stage[index-1] == 1, 
                           assEnergy - somMaintenance, 
                           ifelse(abs(reproBufferAss + structBiomassAss)>0, 0,
                           ((assEnergy * kappaVal) - somMaintenance) * growthConversion))
    
    #Energy that is allocated to the reproduction buffer.
    #There are three possible scenarios (corresponding with the three mentioned above):
    #1) enough energy has been assimilated to cover somatic maintenance with the kappa-fraction. The remainder goes into reproduction
    #2) kappa * assEnergy is not sufficient to cover somatic maintenance: The missing energy is re-allocated from the reproduction branch of energy allocation and subtracted from reproEnergy
    #3) assimilated energy is not sufficient to cover somatic maintenance: No energy goes into reproduction
    reproEnergy <- (1 - kappaVal) * assEnergy * growthConversion
    
    #update the state variables:
    #Structural biomass
    structuralBiomass <- krill$structuralBiomass[index-1] + growthEnergy - structBiomassAss
    
    #Egg buffer (for embryos).
    #There are three phases of buffer depletion:
    #1) buffer can cover somatic maintenance costs: energy is drawn from buffer
    #2) buffer still has energy but not sufficient to cover somatic maintenance: remaining energy is utilized
    #3) buffer is depleted: it remains zero
    eggBuffer <- ifelse(krill$eggBuffer[index-1] - somMaintenance > 0,
                        krill$eggBuffer[index-1] - assEnergy/storageConversion,
                        0)
                        # krill$eggBuffer[index-1] - krill$eggBuffer[index-1])
    
    #Reproduction Buffer
    reproBuffer <- ifelse(stage == 3 & triggerReproduction(length = krill$volumetricLength[index-1]/shapeCorrection,
                                                           reproState = krill$reproBuffer[index-1],
                                                           currentMonth = month(as.Date(krill$referenceDate[index-1], origin = '1970-01-01'))) == FALSE, 
                          krill$reproBuffer[index-1] + reproEnergy - reproBufferAss,
                          0)
    
    #volumetric length (used in food uptake and metabolic requirements)
    volumetricLength <- (krill$structuralBiomass[index-1]/dv)^(1/3);
    krill <<- krill %>% 
      bind_rows(., tibble(referenceDate = environment$date[index], 
                              structuralBiomass = structuralBiomass, 
                              eggBuffer = eggBuffer, 
                              reproBuffer = reproBuffer, 
                              volumetricLength = volumetricLength, 
                              stage = stage, 
                              age = index,
                assEnergy = assEnergy,
                somMaintenance = somMaintenance,
                energyDeficit = energyDeficit,
                reproBufferAss = reproBufferAss,
                structBiomassAss = structBiomassAss,
                netGrowth = growthEnergy - structBiomassAss, 
                repro = stage == 3 & triggerReproduction(length = krill$volumetricLength[index-1]/shapeCorrection,
                                                         reproState = krill$reproBuffer[index-1],
                                                         currentMonth = month(as.Date(krill$referenceDate[index-1], origin = '1970-01-01')))))

    
  #   energyDynamics <<- energyDynamics %>% 
  #     bind_rows(.,tibble(referenceDate = environment$date[index],
  #                             age = (index-1)/365,
  #                             assEnergy = assEnergy,
  #                             somMaintenance = somMaintenance,
  #                             energyDeficit = energyDeficit,
  #                             reproBufferAss = reproBufferAss,
  #                             structBiomassAss = structBiomassAss,
  #                           netGrowth = growthEnergy - structBiomassAss, 
  #                           repro = stage == 3 & triggerReproduction(length = krill$volumetricLength[index-1]/shapeCorrection,
  #                                                               currentMonth = month(as.Date(krill$referenceDate[index-1], origin = '1970-01-01')))))
   }
  return(krill)
}
