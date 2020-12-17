generateEnvironment <- function(scenario){
  if(scenario == 'paradise'){
    source(here('functions','generateParadise.R'))
    return(generateParadise())
  } else if (scenario == 'palmer'){
    source(here('functions','generatePalmer.R'))
    return(generatePalmer(boostWinter = winterBoost))
  }
  return(envData)
}
