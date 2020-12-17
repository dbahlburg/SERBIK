triggerReproduction <- function(length, reproState, 
                                currentMonth, reproScenario = 'fixedClutchSize'){
  if (reproScenario == 'fixedClutchSize'){
    triggerEnergy <<- (length * 150.83 - 3027.244) * weightEgg
    return(reproState > triggerEnergy & currentMonth %in% spawningWindow)
  }
}