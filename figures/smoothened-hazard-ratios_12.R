library(tidyverse)
library(stringr)
library(mice)
library(survival)
library(broom)

if(!exists('GROUP')) GROUP = 'CVD-no_DM2-yes'

source('R/global.R')

data = lapply(list.files('imputation/', pattern = 'ABIDEM_*', full.names = TRUE), function(fname){
  load(fname)
  maria.imp
}) %>% bind_rows() %>% tbl_df() %>%
  filter_group(GROUP) %>%
  mutate(time_diab = if_else(is.na(time_diab), 0, time_diab))

ending_with = function(.data, end_, ...) .data  %>% 
  dplyr::select(..., ends_with(end_)) %>%
  rename_at(vars(ends_with(end_)), function(x) str_sub(x, end = -1-nchar(end_)))


data = inner_join(
  data %>% ending_with('.i', .imp, ocip, itb, sex, p.cvd, p.frailty, p.hf,
                       age, men, p.smoking, bmi, sbp, dbp, pp, coltot, colldl, colhdl, tg, hba1c, glu,p.alcohol_high, p.alcohol_low,
                       time_diab, p.b01aa, p.b01ab, p.b01ac, p.b01a_other, p.htn, p.aff, p.copd, p.ckd, p.neoplasms_malignant, p.dm_med, p.c03, p.c07, p.c08, p.c09, p.c02, 
                       p.statin, p.c10nostatin) %>% gather(variable, event, starts_with('d.')),
  data %>% ending_with('.t', .imp, ocip) %>% gather(variable, time, starts_with('d.')), 
  by = c('.imp', 'ocip', 'variable'))


OUTCOMES = c('d.dementia',
             'd.dementia_alzheimer',
             'd.dementia_vascular',
             'd.dementia_unspecified',
             'd.death')

data = data %>%
  filter(variable %in% OUTCOMES)

load(sprintf('tables/hazard-ratios-03_%s.RData', GROUP))
global.mi$rowname = gsub('TRUE', '', global.mi$rowname)

CUTOFF = 1.2
fit_splines = function(.data){
  outcome_ = unique(.data$variable)
  # print(unique(.data$.imp))
  # print(outcome_)
  vrs = global.mi %>%
    filter(!grepl('itb_cat', rowname), variable == outcome_) %>%
    pull(rowname)
  
  df = select(.data, time, event, itb, vrs) %>%
    filter(complete.cases(.))
  frm = as.formula(sprintf("Surv(time, event)~pspline(itb, df = 0, caic = T)+%s",paste(vrs, collapse = '+')))
  m = coxph(frm, data = df, x = TRUE)
  
  df.vrs = rep(list(0), length(vrs)) %>%
    setNames(vrs) %>%
    as_tibble()
  
  ITBs = as_tibble(m$x) %>%
    select(starts_with('pspline')) %>%
    mutate(itb = df$itb) %>%
    distinct() %>%
    arrange(desc(itb))
  
  b = matrix(coef(m), nrow = 1)
  X = ITBs %>% 
    crossing(df.vrs) %>%
    select(-itb) %>%
    as.matrix() %>%
    t()
  X0 = ITBs %>%
    filter(itb == CUTOFF) %>% 
    crossing(df.vrs) %>%
    select(-itb) %>%
    as.matrix() %>%
    t()
  
  y0 = (b %*% (X0 - m$means))[1,]
  y = (b %*% (X - m$means))[1,]
  
  var_m = function(x){
    t(x - X0) %*% vcov(m) %*% (x - X0)
  }
  
  tibble(
    itb = ITBs$itb,
    lHR = y-y0,
    lHR.var = apply(X, 2, var_m)
  )
}

d.res = data %>%
  group_by(.imp, variable) %>%
  do(fit_splines(.))

d.res_pool = d.res %>%
  filter(.imp > 0) %>%
  group_by(variable, itb) %>%
  summarise(
    n = n(),
    b.pool = mean(lHR),
    v.w = sum((lHR-b.pool)^2/(n-1)),
    v.b =  mean(lHR.var),
    v.pool = v.w + v.b + v.w/n,
    se.pool = sqrt(v.pool))


# dplot = filter(d.res_pool, variable == 'd.stroke_h')
# #dplot = filter(d.res_pool, variable == 'd.death')
# 
# ggplot(data = dplot) +
#   geom_ribbon(aes(x=  itb, ymin = exp(b.pool-1.96*se.pool), ymax = exp(b.pool+1.96*se.pool)), alpha = 0.4) +
#   geom_line(aes(x = itb, y = exp(b.pool)), size = 1.3) +
#   geom_vline(xintercept = CUTOFF, linetype = 2, col = 4) +
#   geom_hline(yintercept = 1, linetype = 3, col = 2) +
#   coord_cartesian(ylim = c(0, 3)) + #global.mi$rowname = gsub('TRUE', '', global.mi$rowname)
#   scale_x_continuous(trans = 'log', breaks = c(0.4, 0.5, 0.7, 0.9, 1.1, 1.3,1.6, 2, 2.5)) +
#   theme_minimal() + labs(title = "Haemorragic Stroke", subtitle = 'Cubic splines with smoothing', y = 'Hazard-Ratio', x = 'ABI (log-scale)')


save(d.res, d.res_pool, file = sprintf('figures/smoothened-hazard-ratios_12_%s.RData', GROUP))

#
# ggplot() +
#   geom_line(data=filter(d.res, .imp>0), aes(x = itb, y = exp(lHR), group=.imp)) +
#   facet_wrap(~variable, scale = 'free') +
#   geom_vline(xintercept = 1.1, linetype = 2, col = 4) +
#   geom_hline(yintercept = 1, linetype = 3, col = 2)
# 
# ggplot(data=d.res_pool) +
#   geom_ribbon(aes(x=  itb, ymin = exp(b.pool-1.96*se.pool), ymax = exp(b.pool+1.96*se.pool)), alpha = 0.4) +
#   geom_line(aes(x = itb, y = exp(b.pool))) +
#   facet_wrap(~variable, scale = 'free') +
#   geom_vline(xintercept = 1.1, linetype = 2, col = 4) +
#   geom_hline(yintercept = 1, linetype = 3, col = 2) +
#   coord_cartesian(ylim = c(0.8, 3)) +
#   scale_x_continuous(trans = 'log', breaks = c(0.4, 0.5, 0.7, 0.9, 1.1, 1.3,1.6, 2, 2.5)) +
#   theme_minimal()
# 
#
