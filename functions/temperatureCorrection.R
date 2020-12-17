#Dominik Bahlburg
#2.12.2019
#This is the temperature correction function based on Arrhenius temperature TA.
#Parameterisation follows estimates from experimental studies where oxygen consumption rates
#were measured as a function of temperature. The unit of the input temperature is Celsius which 
#is converted to Kelvin within the function.
temperatureCorrection <- function(selectedFunction = 'aEnergy', switched = 'on', 
                                  temperature = 0, TA = 7421, T1 = 274.15,
                                  eA = -0.662249,
                                  k = 8.617333262145*10^(-5)){
  if (switched == 'on' & selectedFunction == 'aTemperature') {
    exp(TA/T1 - TA/(temperature+273.15))
  } else if (switched == 'on' & selectedFunction == 'aEnergy'){
    exp(27.660421) * exp(eA/(k * (temperature + 273.15)))/(exp(27.660421) * exp(eA/(k * 273.15)))
    }else if(switched == 'off'){
    1
  }
}

