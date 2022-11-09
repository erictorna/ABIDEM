library(tidyverse)
library(broom)
library(stringr)
library(mice)

if(!exists('GROUP')) GROUP = 'Men'

source('R/global.R')

data = lapply(list.files('imputation/', pattern = 'ABIDEM-imp_*', full.names = TRUE), function(fname){
  load(fname)
  maria.imp
}) %>% bind_rows() %>% tbl_df() %>%
  filter_group(GROUP)

ending_with = function(.data, end_, ...) .data  %>% 
  select(..., ends_with(end_)) %>%
  rename_at(vars(ends_with(end_)), function(x) str_sub(x, end = -1-nchar(end_)))

data = inner_join(
  data %>% ending_with('.i', .imp, ocip, itb_cat) %>% gather(variable, event, starts_with('d.')),
  data %>% ending_with('.t', .imp, ocip) %>% gather(variable, time, starts_with('d.')), 
  by = c('.imp', 'ocip', 'variable'))

incidences = function(.data, ...){
  EVENTS = .data %>%
    filter(!is.na(event)) %>% # S'elimina gent prevalent
    group_by(variable, .imp, ...) %>%
    summarise(
      n_ = n(),
      events_ = sum(event),
      median_time_ = median(time),
      q1_time_ = quantile(time, 0.25),
      q3_time_ = quantile(time, 0.75)) %>%
    group_by(variable, ...) %>%
    summarise(
      n = mean(n_),
      events = mean(events_),
      median_time = mean(median_time_),
      q1_time = mean(q1_time_),
      q3_time = mean(q3_time_))
  
  suppressWarnings(
    models <- .data %>%
      filter(!is.na(event)) %>% # S'elimina gent prevalent
      group_by(variable, .imp, ...) %>%
      do(mod = glm(event~offset(log(time)), data = ., family = 'poisson')) %>%
      group_by(variable, ...) %>%
      do(m = pool(as.mira(.$mod))))
  
  if('glm' %in% class(models$m[[1]])){
    return(full_join(EVENTS, models) %>%
             cbind(pmap_dfr(list(data=as.list(models$m)), ~ tidy(..1))) %>%
             mutate(
               inc = 1000 * exp(estimate),
               inc.lo = 1000 * exp(estimate - 1.96 * std.error),
               inc.hi = 1000 * exp(estimate + 1.96 * std.error)
             ))
  }
  
  INCIDENCES = models %>%
    do(summary(.$m)) %>%
    mutate(
      inc = 1000 * exp(estimate),
      inc.lo = 1000 * exp(estimate - 1.96 * std.error),
      inc.hi = 1000 * exp(estimate + 1.96 * std.error)
    )
  
  FMI = models %>%
    select(variable, ...) %>%
    bind_cols(models %>%
                do(.$m$pooled) %>%
                select(ubar, b, t, dfcom, riv, lambda, fmi))
  
  full_join(EVENTS, bind_cols(FMI, INCIDENCES))
}

data.imp = filter(data, .imp != 0)

global.mi = incidences(data.imp)
itb_cat.mi = incidences(data.imp, itb_cat)

data.cc = filter(data, .imp == 0)

global.cc = incidences(data.cc)
global.cc$m<-NULL
itb_cat.cc = incidences(data.cc, itb_cat)
itb_cat.cc$m<-NULL
rm(data, data.imp, data.cc, ending_with)

save.image(file = sprintf('tables/incidences_%s.RData', GROUP))
