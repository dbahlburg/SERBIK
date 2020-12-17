#Dominik Bahlburg
#2.12.2019
#This is the auxiliary feeding function for the bioenergetic growth model of Antarctic Krill
#The function allows to switch between different options:
#JagerRavagnan: The default from the Rager & Ravagnan paper assuming optimum food concentrations
#HollingType2: type 2 functional response. Default parameterisation after Groeneveld et al. (2015)
#scaling in stage 1 krill is 0.28 according to the fB parameter in the JagerRavagnan-Model
feedingFunction <- function(chosenFunction, stage, feedingMode, foodConc = 10, k = 0.3){
  if(chosenFunction == 'JagerRavagnan'){
    ifelse(feedingMode > 0,
           0.28,
           1)
  }
    else if(chosenFunction == 'HollingType2'){
      ifelse(feedingMode > 0,
             0.28,
             foodConc / (foodConc + k) )
  }
}