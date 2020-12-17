#This script contains Morris Screening analysis for the krill growth model
#September 3rd, 2020
#Dominik Bahlburg
#------------------------------------------------------------------------------#
#Load packages and functions
library(tidyverse)
library(sensitivity)
library(tictoc)
library(ggrepel)
library(cowplot)

setwd("~/Git/PhDThesis/growthModel")
# source('functions/defineConstantsAndRun.R')
#------------------------------------------------------------------------------#
#Prepare initial matrix containing starting values for screening
# Xtemp <- tibble(maxAssimilation = 0.087,
#             Jsm = 0.0032,
#             foodConversion = 0.8,
#             growthConversion = 0.8,
#             structureConversion = 0.8,
#             storageConversion = 0.95,
#             kappaVal = 0.7)

#Load results (executing function below), tic()/toc() for benchmarking
#The results were calculated for four different model response values and 
#40 screening runs for each response...
morrisScreening <- readRDS("sensitivity/outputData/morrisScreening40Reps.RData")

#Run the morris screening for all model responses and winter boost scenarios...
morrisInput <- expand.grid(output = c('noEggs','maxSize','dayOfMaturity'),
                           winterBoostMode = c(T,F)) %>% 
  mutate(output = as.character(output)) 

# tic()
# morrisScreening <- mapply(FUN = morris, output = morrisInput$output,
#                                  winterBoostMode = morrisInput$winterBoostMode,
#                                  MoreArgs = list(model = defineConstantsAndRun,
#                                                  r = 40,
#                                                  design = list(type = "oat", levels = 6, grid.jump = 3),
#                                                  scale = T,
#                                                  binf = as.numeric(Xtemp) - as.numeric(Xtemp)* 0.1,
#                                                  bsup = as.numeric(Xtemp) + as.numeric(Xtemp)* 0.1,
#                                                  factors = names(Xtemp),
#                                                  loop = T),
#                                  SIMPLIFY = F)
# toc()
#running time: 18799secs or 5.22hours
#saveRDS(morrisScreening, file="sensitivity/outputData/morrisScreening40Reps.RData")

#Extract the relevant data from the list-output of the morris- and mapply-functions, merge data into one tibble
#helper function "extractInfo" which extracts the elementary effects of each variable for single model responses
extractInfo <- function(response){
  info <- as_tibble(morrisScreening[[response]]$ee) %>% 
    mutate(responseCol = morrisInput$output[response],
           winterBoost =  morrisInput$winterBoostMode[response]) %>% 
    relocate(responseCol, winterBoost)
  return(info)}

morrisResults <- bind_rows(mapply(response = 1:6,
                                  FUN = extractInfo, SIMPLIFY = F)) %>% 
  gather(variable, ee, -responseCol, -winterBoost)

#Create two helper tibbles with labels for the plots
varNames <- tibble(variable = unique(morrisResults$variable),
                   varName = c('A[max]', 'J[M[max]]', '\u03b5 [F]', '\u03b5 [S]', '\u03b5 [metab]', '\u03b5 [A]', '\u03ba'))
responseNames <- tibble(responseCol = unique(morrisResults$responseCol),
                   responseName = c('number of eggs','maximum body size','day reaching maturity')) 

#Calculate sigma, mu, mu.star and merge results and labels from above tibbles
morrisResultsPlot <- morrisResults %>% 
  mutate(isNA = ifelse(is.na(ee), 0, 1)) %>% 
  group_by(responseCol, winterBoost, variable) %>% 
  summarise(mu = mean(ee, na.rm = T),
            mu.star = mean(abs(ee), na.rm = T),
            sigma = sd(ee, na.rm = T),
            nIsNA = sum(isNA)) %>% 
  mutate(influential = ifelse(mu.star > 0.1 * max(mu.star), 'yes','no')) %>% 
  left_join(., varNames, by = 'variable') %>% 
  left_join(.,responseNames, by = 'responseCol') 

#--------------------------------------------------------------------------------------#
#--------------------------------------------------------------------------------------#
#Create plots:
#Set plot themes for uniform layout
plotTheme <- 
  theme(panel.background = element_rect(fill = NA, colour = '#6b6b6b'),
        strip.background = element_rect(fill = NA),
        strip.text = element_text(size = 16),
        axis.text = element_text(size = 16),
        axis.title.y = element_text(size = 18, vjust = 0),
        axis.title.x = element_text(size = 18),
        plot.title = element_text(size = 16, hjust = 0.5),
        panel.grid.major = element_line(colour = NA),
        panel.grid.minor = element_line(colour = NA),
        legend.position = 'none',
        plot.margin = margin(t = 30, r = 0, b = 30, l = 0, unit = "pt"),
        panel.spacing = unit(2, "lines")) 
  
labelSize = 6.5 #geom_text_repel label size
#======================================================================================#
#Plot results where winterBoost == T
eggPlotSigmaMu <- morrisResultsPlot %>% 
  filter(responseCol == 'noEggs') %>% 
  filter(winterBoost == T) %>% 
  ggplot(., aes(y = sigma, x = mu.star,
                label = varName, colour = influential)) +
  scale_colour_manual(values = c('#333333','#005f6a')) +
  geom_point() +
  labs(y = expression(sigma), x = expression(mu^'*'), title = 'number of eggs produced') +
  scale_y_continuous(limits = c(0,24000),
                     breaks = c(0,10000,20000)) +
  scale_x_continuous(limits = c(0,24000),
                     breaks = c(0,10000,20000)) +
  geom_text_repel(parse = TRUE, size = labelSize) +
  plotTheme 
eggPlotSigmaMu

eggPlotMuMu <- morrisResultsPlot %>% 
  filter(responseCol == 'noEggs') %>% 
  filter(winterBoost == T) %>% 
  ggplot(., aes(y = mu, x = mu.star,
                label = varName, colour = influential)) +
  scale_colour_manual(values = c('#333333','#005f6a')) +
  geom_hline(yintercept = 0, colour = '#cfcfcf') +
  geom_point() +
  labs(y = expression(mu), x = expression(mu^'*'), title = 'number of eggs produced') +
  scale_x_continuous(limits = c(0,24000),
                     breaks = c(0,10000,20000)) +
  scale_y_continuous(limits = c(-24000, 24000),
                     breaks = c(-20000,0,20000)) +
  geom_text_repel(parse = TRUE, size = labelSize) +
  plotTheme+
  theme(axis.title.y = element_text(vjust = -6))

maxSizePlotSigmaMu <- morrisResultsPlot %>% 
  filter(responseCol == 'maxSize') %>% 
  filter(winterBoost == T) %>% 
  ggplot(., aes(y = sigma, x = mu.star,
                label = varName, colour = influential)) +
  scale_colour_manual(values = c('#333333','#005f6a')) +
  geom_point() +
  labs(y = expression(sigma), x = expression(mu^'*'), title = 'maximum body length') +
  scale_y_continuous(limits = c(0,13),
                     breaks = c(0,6,12)) +
  scale_x_continuous(limits = c(0,13),
  breaks = c(0,6,12)) +
  geom_text_repel(parse = TRUE, size = labelSize) +
  plotTheme

maxSizePlotMuMu <- morrisResultsPlot %>% 
  filter(responseCol == 'maxSize') %>% 
  filter(winterBoost == T) %>% 
  ggplot(., aes(y = mu, x = mu.star,
                label = varName, colour = influential)) +
  scale_colour_manual(values = c('#333333','#005f6a')) +
  geom_hline(yintercept = 0, colour = '#cfcfcf') +
  geom_point() +
  labs(y = expression(mu), x = expression(mu^'*'), title = 'maximum body length') +
  scale_y_continuous(limits = c(-13,13),
                     breaks = c(-12,0,12)) +
  scale_x_continuous(limits = c(0,13),
                     breaks = c(0,6,12)) +
  geom_text_repel(parse = TRUE, size = labelSize) +
  plotTheme+
  theme(axis.title.y = element_text(vjust = -6))

dayOfMaturityPlotSigmaMu <- morrisResultsPlot %>% 
  filter(responseCol == 'dayOfMaturity') %>% 
  filter(winterBoost == T) %>% 
  ggplot(., aes(y = sigma, x = mu.star,
                label = varName, colour = influential)) +
  scale_colour_manual(values = c('#333333','#005f6a')) +
  geom_point() +
  labs(y = expression(sigma), x = expression(mu^'*'), title = 'day of maturity') +
  scale_y_continuous(limits = c(0,200),
                     breaks = c(0,100,200)) +
  scale_x_continuous(limits = c(0,200),
                     breaks = c(0,100,200)) +
  geom_text_repel(parse = TRUE, size = labelSize) +
  plotTheme

dayOfMaturityPlotMuMu <- morrisResultsPlot %>% 
  filter(responseCol == 'dayOfMaturity') %>% 
  filter(winterBoost == T) %>% 
  ggplot(., aes(y = mu, x = mu.star,
                label = varName, colour = influential)) +
  scale_colour_manual(values = c('#333333','#005f6a')) +
  geom_hline(yintercept = 0, colour = '#cfcfcf') +
  geom_point() +
  labs(y = expression(mu), x = expression(mu^'*'), title = 'day of maturity') +
  scale_y_continuous(limits = c(-200,200),
                     breaks = c(-200,0,200)) +
  scale_x_continuous(limits = c(0,200),
                     breaks = c(0,100,200)) +
  geom_text_repel(parse = TRUE, size = labelSize) +
  plotTheme+
  theme(axis.title.y = element_text(vjust = -6))

morrisResults <- plot_grid(eggPlotSigmaMu, eggPlotMuMu, 
          #spawningPlotSigmaMu, spawningPlotMuMu, 
          maxSizePlotSigmaMu,maxSizePlotMuMu, 
          dayOfMaturityPlotSigmaMu,dayOfMaturityPlotMuMu, 
          ncol = 2,align = "v")

ggsave('/Users/dominik/Documents/KRILL_UFZ/Publications/01GrowthModel/Plots/SuppFigure1.png', morrisResults, width = 11, height = 14)
 

#======================================================================================#
#Plot results where winterBoost == F
eggPlotSigmaMu <- morrisResultsPlot %>% 
  filter(responseCol == 'noEggs') %>% 
  filter(winterBoost == F) %>% 
  ggplot(., aes(y = sigma, x = mu.star,
                label = varName, colour = influential)) +
  scale_colour_manual(values = c('#333333','#005f6a')) +
  geom_point() +
  labs(y = expression(sigma), x = expression(mu^'*'), title = 'number of eggs produced') +
  scale_y_continuous(limits = c(0,24000),
                     breaks = c(0,10000,20000)) +
  scale_x_continuous(limits = c(0,24000),
                     breaks = c(0,10000,20000)) +
  geom_text_repel(parse = TRUE, size = labelSize) +
  plotTheme 
eggPlotSigmaMu

eggPlotMuMu <- morrisResultsPlot %>% 
  filter(responseCol == 'noEggs') %>% 
  filter(winterBoost == F) %>% 
  ggplot(., aes(y = mu, x = mu.star,
                label = varName, colour = influential)) +
  scale_colour_manual(values = c('#333333','#005f6a')) +
  geom_hline(yintercept = 0, colour = '#cfcfcf') +
  geom_point() +
  labs(y = expression(mu), x = expression(mu^'*'), title = 'number of eggs produced') +
  scale_x_continuous(limits = c(0,24000),
                     breaks = c(0,10000,20000)) +
  scale_y_continuous(limits = c(-24000, 24000),
                     breaks = c(-20000,0,20000)) +
  geom_text_repel(parse = TRUE, size = labelSize) +
  plotTheme+
  theme(axis.title.y = element_text(vjust = -6))

maxSizePlotSigmaMu <- morrisResultsPlot %>% 
  filter(responseCol == 'maxSize') %>% 
  filter(winterBoost == F) %>% 
  ggplot(., aes(y = sigma, x = mu.star,
                label = varName, colour = influential)) +
  scale_colour_manual(values = c('#333333','#005f6a')) +
  geom_point() +
  labs(y = expression(sigma), x = expression(mu^'*'), title = 'maximum body length') +
  scale_y_continuous(limits = c(0,13),
                     breaks = c(0,6,12)) +
  scale_x_continuous(limits = c(0,13),
                     breaks = c(0,6,12)) +
  geom_text_repel(parse = TRUE, size = labelSize) +
  plotTheme

maxSizePlotMuMu <- morrisResultsPlot %>% 
  filter(responseCol == 'maxSize') %>% 
  filter(winterBoost == F) %>% 
  ggplot(., aes(y = mu, x = mu.star,
                label = varName, colour = influential)) +
  scale_colour_manual(values = c('#333333','#005f6a')) +
  geom_hline(yintercept = 0, colour = '#cfcfcf') +
  geom_point() +
  labs(y = expression(mu), x = expression(mu^'*'), title = 'maximum body length') +
  scale_y_continuous(limits = c(-13,13),
                     breaks = c(-12,0,12)) +
  scale_x_continuous(limits = c(0,13),
                     breaks = c(0,6,12)) +
  geom_text_repel(parse = TRUE, size = labelSize) +
  plotTheme+
  theme(axis.title.y = element_text(vjust = -6))

morrisResults <- plot_grid(eggPlotSigmaMu, eggPlotMuMu, 
                           #spawningPlotSigmaMu, spawningPlotMuMu, 
                           maxSizePlotSigmaMu,maxSizePlotMuMu,
                           ncol = 2,align = "v")

ggsave('/Users/dominik/Documents/KRILL_UFZ/Publications/01GrowthModel/Plots/SuppFigure2.png', morrisResults, width = 11, height = 2/3 * 14)
#--------------------------------------------------------------------------------------#
#--------------------------------------------------------------------------------------#
# Summary tables summarizing influential parameters for both winter food scenarios facilitating comparisons
morrisSorted <- morrisResultsPlot %>% 
  #group_by(winterBoost, responseCol, variable) %>% 
  arrange(winterBoost, responseCol, variable, desc(mu.star)) %>% 
  select(winterBoost, responseCol, variable, mu.star, sigma) %>% 
  mutate(mu.star = round(mu.star,1),
         sigma = round(sigma,1))

View(morrisSorted %>% 
  #filter(responseCol == 'maxSize') %>% 
    arrange(responseCol,winterBoost,desc(mu.star)) %>% 
    group_by(responseCol,winterBoost) %>% 
    mutate(rank = rank(desc(mu.star))) %>% 
    select(winterBoost, responseCol,variable, rank) %>% 
    pivot_wider(names_from = c(responseCol,winterBoost), values_from = rank))
















