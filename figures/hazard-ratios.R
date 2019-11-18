library(tidyverse)
library(broom)
library(stringr)
library(mice)
library(survival)

if(!exists('GROUP')) GROUP = 'CVD-no_DM2-no'

OUTCOMES = c('d.dementia',
             'd.death')

HRs.mi = list()
HRs.cc = list()
load(sprintf('tables/hazard-ratios_%s.RData', GROUP))

global.mi = global.mi %>% filter(variable %in% OUTCOMES)
global.cc = global.cc %>% filter(variable %in% OUTCOMES)

HRs.mi[['Unadjusted']] = list('global' = global.mi, 'sex' = sex.mi)
HRs.cc[['Unadjusted']] = list('global' = global.cc, 'sex' = sex.cc)

load(sprintf('tables/hazard-ratios-01_%s.RData', GROUP))

global.mi = global.mi %>% filter(variable %in% OUTCOMES)
global.cc = global.cc %>% filter(variable %in% OUTCOMES)

HRs.mi[['Regicor variables']] = list('global' = global.mi, 'sex' = sex.mi)
HRs.cc[['Regicor variables']] = list('global' = global.cc, 'sex' = sex.cc)

# load('tables/hazard-ratios-02.RData')
# 
# global.mi = global.mi %>% filter(variable %in% OUTCOMES)
# global.cc = global.cc %>% filter(variable %in% OUTCOMES)
# 
# HRs.mi[['AIC criterion']] = list('global' = global.mi, 'sex' = sex.mi)
# HRs.cc[['AIC criterion']] = list('global' = global.cc, 'sex' = sex.cc)

load(sprintf('tables/hazard-ratios-03_%s.RData', GROUP))

global.mi = global.mi %>% filter(variable %in% OUTCOMES)
global.cc = global.cc %>% filter(variable %in% OUTCOMES)

HRs.mi[['BIC criterion']] = list('global' = global.mi, 'sex' = sex.mi)
HRs.cc[['BIC criterion']] = list('global' = global.cc, 'sex' = sex.cc)


save(OUTCOMES, HRs.mi, HRs.cc, file = sprintf('figures/hazard-ratios_%s.RData', GROUP))
