library(tidyverse)
library(broom)
library(stringr)
library(mice)
library(survival)
library(data.table)

if(!exists('GROUP')) GROUP = 'ALL'

source('R/global.R')

data = lapply(list.files('imputation/', pattern = 'ABIDEM-imp_*', full.names = TRUE), function(fname){
  load(fname)
  maria.imp
}) %>% bind_rows() %>% tbl_df() %>%
  filter_group(GROUP)

# data$itb_cat = factor(as.character(data$itb_cat), 
                      # levels = ABI_LEVELS)

ending_with = function(.data, end_, ...) .data  %>% 
  select(..., ends_with(end_)) %>%
  rename_at(vars(ends_with(end_)), function(x) str_sub(x, end = -1-nchar(end_)))

data = inner_join(
  data %>% ending_with('.i', .imp, ocip, itb_cat, sex) %>% gather(variable, event, starts_with('d.')),
  data %>% ending_with('.t', .imp, ocip) %>% gather(variable, time, starts_with('d.')), 
  by = c('.imp', 'ocip', 'variable'))

survival = function(.data, ...){
  suppressWarnings(
    models <- .data %>%
      group_by(variable, .imp, ...) %>% 
      do(mod = coxph(Surv(time, event)~itb_cat, data = .)) %>%
      group_by(variable, ...) %>%  
      do(m = pool(as.mira(.$mod))))
  
  if('coxph' %in% class(models$m[[1]])){
    return(models = models %>%
             mutate(m = list(tidy(m))))
  }
  
  if ('mipo' %in% class(models$m[[1]])) {
    HRs = models %>%
      group_by(variable, ...) %>%
      do(summary(.$m[[1]])) %>%
      select(-estimate, -df, -term)
    
    FMI = models %>%
      do(rownames_to_column(.$m$pooled)) 
    
    cbind(FMI, HRs)
  } 
}

data.imp = filter(data, .imp > 0)

global.mi = survival(data.imp)
sex.mi = survival(data.imp, sex)

# data.cc = filter(data, .imp == 0)
# 
# # Apanyo per treure les columnes amb les variables que minteressen
# setDT(global.mi)
# variables = global.mi[, .N, keyby=.(variable, term)]
# variables = c(variables$variable)
# global.cc = try(survival(data.cc), silent = TRUE)
# global.cc = global.cc %>% do(rownames_to_column(.$m))
# global.cc$rowname=variables
# global.cc = global.cc %>% rename(variable = rowname)
# if (GROUP == 'ALL') {
#   genders = c('D', 'D', 'D', 'D', 'D', 'H', 'H', 'H', 'H', 'H')
#   genderlist = c(genders, genders, genders, genders)
#   sex.cc = try(survival(data.cc, sex), silent = TRUE)
#   sex.cc = sex.cc %>% do(rownames_to_column(.$m))
#   sex.cc$sex=genderlist
#   variables = c("d.death", "d.death", "d.death", "d.death", "d.death", "d.death", "d.death", "d.death", "d.death", "d.death",
#                 "d.dementia", "d.dementia", "d.dementia", "d.dementia", "d.dementia", "d.dementia", "d.dementia", "d.dementia", "d.dementia", "d.dementia", 
#                 "d.dementia_alzheimer", "d.dementia_alzheimer",  "d.dementia_alzheimer", "d.dementia_alzheimer", "d.dementia_alzheimer", "d.dementia_alzheimer", "d.dementia_alzheimer", "d.dementia_alzheimer", "d.dementia_alzheimer", "d.dementia_alzheimer",
#                 "d.dementia_vascular", "d.dementia_vascular", "d.dementia_vascular", "d.dementia_vascular", "d.dementia_vascular", "d.dementia_vascular", "d.dementia_vascular", "d.dementia_vascular", "d.dementia_vascular", "d.dementia_vascular")
#   sex.cc$rowname = variables
#   sex.cc = sex.cc %>% rename(variable = rowname)
# }
rm(data, data.imp, data.cc, ending_with)

save.image(file = sprintf('tables/hazard-ratios_%s.RData', GROUP))
