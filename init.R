

library(tidyverse)
library(magrittr)
library(broom)

# DATA -------------------------------------------------------------------------
perinatal_data = read.csv('Data/EffectSizes.csv') 
depression = filter(perinatal_data, type == 'Depression')
anxiety = filter(perinatal_data, type == 'Anxiety')
worry = filter(perinatal_data, type == 'Worry')


# SOURCES ----------------------------------------------------------------------
# source('R/effect_sizes.R')
source('R/frequentist.R')