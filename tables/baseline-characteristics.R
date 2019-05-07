library(tidyverse)

if(!exists('GROUP')) GROUP = 'CVD-no_DM2-no'

load('build.data/K.RData')
source('R/global.R')

data = lapply(list.files('imputation/', pattern = 'K-imp_*', full.names = TRUE), function(fname){
  load(fname)
  maria.imp
}) %>% bind_rows(maria %>% mutate(.imp = -1)) %>% tbl_df() %>%
  filter_group(GROUP)

numeric_describe = function(.data, ...){
  .data %>%
    select(.imp, ..., age, itb, weight, height, hba1c, followup = d.death.t, coltot, 
           colhdl, colldl, tg, dbp, sbp, bmi, cre, glu, hr, pp, time_diab) %>%
    gather(variable, value, age:time_diab) %>%
    group_by(.imp, variable, ...) %>%
    summarise(
      n_ = sum(!is.na(value)),
      na_ = mean(is.na(value)),
      mean_ = mean(value, na.rm=TRUE),
      sd_ = sd(value, na.rm=TRUE),
      median_ = median(value, na.rm=TRUE),
      q1_ = quantile(value, 0.25, na.rm=TRUE),
      q3_ = quantile(value, 0.75, na.rm=TRUE)
    ) %>%
    group_by(variable, ...) %>%
    summarise(
      n = mean(n_),
      na = mean(na_),
      mean = mean(mean_),
      sd = sqrt(mean(sd_^2)),
      median = mean(median_),
      q1 = mean(q1_),
      q3 = mean(q3_)
    )
}


dichotomic_describe = function(.data, ...){
  .data %>%
    select(.imp, ..., men, starts_with('p.'), d.death.i, exitusD, exitusT) %>%
    gather(variable, value, men:exitusT) %>%
    group_by(.imp, variable, ...) %>%
    summarise(
      n_ = sum(!is.na(value)),
      na_ = mean(is.na(value)),
      m_ = sum(value, na.rm=TRUE),
      p_ = mean(value, na.rm=TRUE)
    ) %>%
    group_by(variable, ...) %>%
    summarise(
      n = mean(n_),
      na = mean(na_),
      m = mean(m_),
      p = mean(p_)
    )
}

describe = function(data, ...){
  list('numeric' = numeric_describe(data, ...),
       'dichotomic' = dichotomic_describe(data, ...))
}

data.raw = filter(data, .imp == -1)
data.cc = filter(data, .imp == 0)
data.imp = filter(data, .imp > 0)

global.raw = describe(data.raw)
itb_cat.raw = describe(data.raw, itb_cat)

global.cc = describe(data.cc)
itb_cat.cc = describe(data.cc, itb_cat)

global.imp = describe(data.imp)
itb_cat.imp = describe(data.imp, itb_cat)

rm(data, data.cc, data.imp, data.raw, maria)

save.image(file = sprintf('tables/baseline-characteristics_%s.RData', GROUP))
