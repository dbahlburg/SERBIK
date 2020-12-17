generateParadise <- function(){
  
startDate <- as.Date(startDay - 1, origin = "2010-01-01")
endDate <- startDate + days-1
paradise <- tibble(date = seq(startDate,
                              endDate, by = 'days'),
                   dayNo = as.numeric(strftime(date, format = '%j')),
                   dayLength = rep(24, times = days),
                   temperature = rep(0, times = days),
                   foodConc = rep(100, times = days))
return(paradise)
}