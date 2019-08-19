library(tidyverse)
library(stringr)
library(mice)
library(survival)
library(broom)

if(!exists('GROUP')) GROUP = 'CVD-no_DM2-no'

source('R/global.R')

data = lapply(list.files('imputation/', pattern = 'K-imp_*', full.names = TRUE), function(fname){
  load(fname)
  maria.imp
}) %>% bind_rows() %>% tbl_df() %>%
  filter_group(GROUP) %>%
  mutate(time_diab = if_else(is.na(time_diab), 0, time_diab))

# Categories ABI
data = data %>%
  mutate(itb_cat = cut(itb, breaks = c(0.4,0.9,1.3,3), include.lowest = T, right = F))

data$itb_cat = factor(as.character(data$itb_cat), 
                      levels = c('[0.9,1.3)', '[0.4,0.9)', '[1.3,3]'))

ending_with = function(.data, end_, ...) .data  %>% 
  dplyr::select(..., ends_with(end_)) %>%
  rename_at(vars(ends_with(end_)), function(x) str_sub(x, end = -1-nchar(end_)))


data = inner_join(
  data %>% ending_with('.i', .imp, ocip, itb_cat, sex, 
                       age, men, p.smoking, bmi, sbp, dbp, pp, coltot, colldl, colhdl, tg, hba1c, glu,
                       time_diab, p.b01aa, p.b01ab, p.b01ac, p.b01a_other, p.htn, p.aff, p.copd, p.ckd, p.neoplasms_malignant, p.dm_med, p.c03, p.c07, p.c08, p.c09, p.c02, 
                       p.statin, p.c10nostatin) %>% gather(variable, event, starts_with('d.')),
  data %>% ending_with('.t', .imp, ocip) %>% gather(variable, time, starts_with('d.')), 
  by = c('.imp', 'ocip', 'variable'))


# OUTCOMES = c('d.ami',
#              'd.stroke_i',
#              'd.hard',
#              'd.stroke_h',
#              'd.death',
#              'd.nephro_ckd',
#              'd.retinopathy',
#              'd.neuropathy')
# 
# data = data %>%
#   filter(variable %in% OUTCOMES)



library(parallel)
CLUSTER = parallel::makeCluster(10)
clusterEvalQ(CLUSTER, { library(survival) })

fb_cox = function(.data){
  m0 = coxph(Surv(time, event)~itb_cat, data = .data)
  MASS::stepAIC(m0, list(upper = ~itb_cat+age+men+p.smoking+bmi+sbp+dbp+pp+coltot+colldl+colhdl+p.b01aa+p.b01ab+p.b01ac+p.b01a_other+ 
                           tg+hba1c+glu+time_diab+p.htn+p.aff+p.copd+p.ckd+p.neoplasms_malignant+p.dm_med+p.c03+p.c07+p.c08+
                           p.c09+p.c02+p.statin+p.c10nostatin, lower = ~itb_cat), k = log(nrow(.data)))
}

l_data = split(data, list(data$.imp, data$variable))
models = parLapply(cl = CLUSTER, l_data, fb_cox)

sex_interaction = function(mod, data){
  mod.interact = update(mod, .~.+men+men:itb_cat, data=data)
  anova(mod, mod.interact)
}
l_anova = mapply(sex_interaction, models, l_data, SIMPLIFY = FALSE)

l_proportionality = lapply(models, cox.zph)
proportionality = l_proportionality %>%
  lapply(function(ll) as_tibble(as.data.frame(ll$table) %>% rownames_to_column())) %>%
  bind_rows(.id = 'model') %>%
  separate(model, c('imp', 'd', 'outcome'), sep='\\.') %>%
  select(-d)

pv = sapply(l_anova, function(x) x$`P(>|Chi|)`[[2]])
itb_sex_interactions = tibble(
  'var' = names(pv),
  'p-value' = pv
) %>%
  separate(var, c('imp', 'd', 'outcome'), sep='\\.') %>%
  mutate(ds = if_else(imp > 0, 'mi', if_else(imp == 0, 'cc', NA_character_))) %>%
  group_by(ds, outcome) %>%
  summarise(
    `p-value` = median(`p-value`)
  )

l_data = split(data, list(data$.imp, data$variable, data$sex))
models_sex = parLapply(cl = CLUSTER, l_data, fb_cox)

l_proportionality_sex = lapply(models_sex, cox.zph)
stopCluster(CLUSTER)

df = lapply(models , function(.model){
  tibble(
    variable = attr(.model$terms, 'term.labels'))
}) %>% bind_rows(.id = 'id') %>%
  separate(id, c('.imp', 'd', 'outcome'), sep = '\\.', convert = TRUE)

df_sex = lapply(models_sex , function(.model){
  tibble(
    variable = attr(.model$terms, 'term.labels'))
}) %>% bind_rows(.id = 'id') %>%
  separate(id, c('.imp', 'd', 'outcome', 'sex'), sep = '\\.', convert = TRUE)

merge_terms = function(variables){
  if(all(c('sbp','dbp','pp') %in% variables)){
    variables = setdiff(variables, 'sbp')
  }  
  paste(variables, collapse='+')
}

vars.imp = df %>%
  filter(.imp > 0) %>%
  select(outcome, variable) %>%
  distinct() %>%
  group_by(outcome) %>%
  summarise(frm_txt = merge_terms(variable))

vars.cc = df %>%
  filter(.imp == 0) %>%
  select(outcome, variable) %>%
  distinct() %>%
  group_by(outcome) %>%
  summarise(frm_txt = merge_terms(variable))

vars_sex.imp = df_sex %>%
  filter(.imp > 0) %>%
  select(outcome, variable, sex) %>%
  distinct() %>%
  group_by(outcome, sex) %>%
  summarise(frm_txt = merge_terms(variable))

vars_sex.cc = df_sex %>%
  filter(.imp == 0) %>%
  select(outcome, variable, sex) %>%
  distinct() %>%
  group_by(outcome, sex) %>%
  summarise(frm_txt = merge_terms(variable))

survival = function(.data, ..., .frm){
  .data_gr = .data %>%
    group_by(variable, .imp, ...)

  suppressWarnings(
    models <- .data_gr %>% 
      do(mod =  coxph(eval(parse(text = sprintf("Surv(time, event)~%s", .$frm_txt[[1]]))), data = .)) %>%
      group_by(variable, ...) %>%  
      do(m = pool(as.mira(.$mod))))
  
  if('coxph' %in% class(models$m[[1]])){
    return(models %>%
             tidy(m))
  }
  
  HRs = models %>%
    group_by(variable, ...) %>%
    do(summary(.$m[[1]])) %>%
    select(-estimate, -df)
  
  FMI = models %>%
    do(rownames_to_column(.$m$pooled)) 
  
  bind_cols(FMI, HRs)
}

data.imp = filter(data, .imp > 0) %>%
  mutate(
    outcome = gsub('d.', '', variable, fixed = TRUE)
  ) 

global.mi = survival(data.imp %>% left_join(vars.imp, by = 'outcome'))
sex.mi = survival(data.imp %>% left_join(vars_sex.imp, by = c('outcome', 'sex')), sex)
# 

data.cc = filter(data, .imp == 0) %>%
  mutate(
    outcome = gsub('d.', '', variable, fixed = TRUE)
  ) 
 
global.cc = survival(data.cc %>% left_join(vars.cc, by = 'outcome'))
sex.cc = try(survival(data.cc %>% left_join(vars_sex.cc, by = c('outcome', 'sex')), sex), silent = TRUE)

rm(data, data.imp, CLUSTER, l_data, data.cc, models, models_sex, ending_with)

save.image(file = sprintf('tables/hazard-ratios-03-3_categories_%s.RData', GROUP))
