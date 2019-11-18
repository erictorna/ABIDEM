library(tidyverse)
library(broom)
library(stringr)
library(mice)
library(survival)

if(!exists('GROUP')) GROUP = 'CVD-no_DM2-no'

source('R/global.R')

data = lapply(list.files('imputation/', pattern = 'ABIDEM_*', full.names = TRUE), function(fname){
  load(fname)
  maria.imp
}) %>% bind_rows() %>% tbl_df() %>%
  filter_group(GROUP)

data = filter(data, .imp <= 1)
data$itb_cat = factor(as.character(data$itb_cat), 
                      levels = ABI_LEVELS)

ending_with = function(.data, end_, ...) .data  %>% 
  dplyr::select(..., ends_with(end_)) %>%
  rename_at(vars(ends_with(end_)), function(x) str_sub(x, end = -1-nchar(end_)))

data = inner_join(
  data %>% ending_with('.i', .imp, ocip, itb_cat) %>% gather(variable, event, starts_with('d.')),
  data %>% ending_with('.t', .imp, ocip) %>% gather(variable, time, starts_with('d.')), 
  by = c('.imp', 'ocip', 'variable'))

survival = function(.data, ...){
  suppressWarnings(
    models <- .data %>%
      group_by(variable, .imp, ...) %>% 
      do(mod = survfit(Surv(time, event)~itb_cat, data = .)))
  
  models %>%
    tidy(mod)
}

data.imp = filter(data, .imp > 0)

global.mi = survival(data.imp)
# sex.mi = survival(data.imp, sex)

data.cc = filter(data, .imp == 0)

global.cc = survival(data.cc)
#sex.cc = survival(data.cc, sex)

rm(data, data.imp, data.cc, ending_with)

save.image(file = sprintf('figures/cumulative-incidences_%s.RData', GROUP))
