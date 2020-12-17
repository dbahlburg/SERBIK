generatePalmer <- function(boostWinter = F){

  ##============================================================================================##
  ## process continuous chlorophyll data
  chlaStation <- read_csv(here('environmentalData','data','chlorophyllPalmer.csv')) %>% 
    mutate(month = month(Date)) %>% 
    group_by(month) %>% 
    summarize(maxChla = max(Chla),
              noBoost = mean(Chla)) %>% 
    mutate(boost = ifelse(between(month,4,9),maxChla,noBoost)) %>% 
    select(-maxChla) %>% 
    gather(key = chla, value = value, -month) %>% 
    filter(chla == ifelse(boostWinter == T,'boost', 'noBoost')) %>% 
    rename(foodConc = value)
  
  ##============================================================================================##
  ## process temperature data
  tempPalmer <- read_csv(here('environmentalData','data','temperaturePalmer.csv')) %>% 
    filter(SST > -30) %>% 
    filter(TempAverage > -30) %>% 
    mutate(day = as.Date(paste('2019-',month(Date),'-', day(Date),sep = ''))) %>% 
    group_by(day) %>% 
    summarise(temp = mean(TempAverage, na.rm = T),
              sst = mean(SST, na.rm = T),
              observations = n()) %>% 
    filter(!is.na(day)) %>%
    mutate(dayNo = as.numeric(strftime(day, format = '%j'))) %>% 
    select(dayNo, sst)

  #merge everything
  startDate <- as.Date(startDay - 1, origin = "2010-01-01")
  endDate <- startDate + noDays-1
  
  palmerData <- tibble(date = seq(startDate,
                                  endDate, by = 'days')) %>% 
    # mutate(lat = -64.055,
    #        lon = -64.055) %>% 
    mutate(lat = -66.5622,
           lon = 0) %>% 
    getSunlightTimes(data = .,
                     keep = c('sunrise','sunset')) %>% 
    mutate(dayLength = as.numeric(difftime(sunset, sunrise, units = 'hours')),
           dayLength = ifelse(is.na(dayLength), 24,dayLength),
           dayNo = as.numeric(strftime(date, format = '%j')),
           month = month(date)) %>% 
    left_join(chlaStation, by = 'month') %>% 
    left_join(tempPalmer, by = 'dayNo') %>% 
    select(date, dayNo, dayLength, temperature = sst, foodConc) %>% 
    filter(!is.na(temperature)) 
  return(palmerData)
}


