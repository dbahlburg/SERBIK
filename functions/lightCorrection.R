#Dominik Bahlburg
#3.12.2019
#This is the auxiliary function regulating metabolic rates in dependence on light availability.
#Further explanations on how this function was derived can be found in the bookdown-document (chapter 2.4) 
lightCorrection <- function(switched = 'on', mode = 'linRegression', dayLength, exactMonth){
  if(switched == 'on' & mode == 'linRegression'){
    return(0.03703 * dayLength + 0.2224)
  } else if(switched == 'on' & mode == 'spline'){
    return(as.numeric(predict(linSplineMod, tibble(month  = exactMonth))/predict(linSplineMod, tibble(month  = 12))))
  } else if(switched == 'on' & mode == 'arctan'){
    return(0.207 * atan(dayLength - 13.232) + 0.694)
    } else if(switched == 'off'){
    1
  }
}
