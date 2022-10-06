library(tidyverse)
library(broom)
library(stringr)
library(mice)
library(survival)

if(!exists('GROUP')) GROUP = 'CVD-no_DM2-no'

source('R/global.R')

data = lapply(list.files('imputation/', pattern = 'ABIDEM-imp_*', full.names = TRUE), function(fname){
  load(fname)
  maria.imp
}) %>% bind_rows() %>% tbl_df() %>%
  filter_group(GROUP)

data$itb_cat = factor(as.character(data$itb_cat), 
                      levels = ABI_LEVELS)

ending_with = function(.data, end_, ...) .data  %>% 
  select(..., ends_with(end_)) %>%
  rename_at(vars(ends_with(end_)), function(x) str_sub(x, end = -1-nchar(end_)))

data = inner_join(
  data %>% ending_with('.i', .imp, ocip, itb_cat, sex, age, men, p.smoking, coltot, colhdl, sbp, dbp) %>% gather(variable, event, starts_with('d.')),
  data %>% ending_with('.t', .imp, ocip) %>% gather(variable, time, starts_with('d.')), 
  by = c('.imp', 'ocip', 'variable'))

survival = function(.data, ...){
  .data_gr = .data %>%
    group_by(variable, .imp, ...)
  
  frm = Surv(time, event)~itb_cat+age+men+p.smoking+coltot+colhdl+sbp+dbp
  if('sex' %in% group_vars(.data_gr)){
    frm = update(frm, ~.-men)
  }
  suppressWarnings(
    models <- .data_gr %>% 
      do(mod =  coxph(frm, data = .)) %>%
      group_by(variable, ...) %>%  
      do(m = pool(as.mira(.$mod))))
  
  if('coxph' %in% class(models$m[[1]])){
    return(models %>%
             cbind(pmap_dfr(list(data=as.list(models$m)), ~ tidy(..1))))
  }
  
  HRs = models %>%
    group_by(variable, ...) %>%
    do(summary(.$m[[1]])) %>%
    select(-estimate, -df)
  
  FMI = models %>%
    do(rownames_to_column(.$m$pooled)) 
  
  bind_cols(FMI, HRs)
}

data.imp = filter(data, .imp > 0)

global.mi = survival(data.imp)
sex.mi = survival(data.imp, sex)

data.cc = filter(data, .imp == 0)

global.cc = survival(data.cc)
global.cc$m<-NULL
sex.cc = try(survival(data.cc, sex), silent = TRUE)
sex.cc$m<-NULL

rm(data, data.imp, data.cc, ending_with)

save.image(file = sprintf('tables/hazard-ratios-01_%s.RData', GROUP))
