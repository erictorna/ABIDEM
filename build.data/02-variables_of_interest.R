library(dplyr)
load('data/01-diabi_population.RData')

## Definim noves variables
# Only exitus before 2016 are considered
maria = maria %>%
  mutate(
    sel = as.Date('2015-12-31') < dexitus,
    exitus = if_else(sel, 'E', exitus),
    dexitus = if_else(sel, as.Date('2015-12-31'), dexitus),
    exitusE = as.integer(exitus == 'E'),
    exitusD = as.integer(exitus == 'D'),
    exitusT = as.integer(exitus == 'T'))

# General variables
maria = maria %>%
  mutate(
    age2 = age^2, #No hi és a DIABI_LIA
    pp = sbp-dbp,
    bmi_prev = bmi, #IMC provinent del SIDIAP
    bmi = weight/(height^2), #IMC calculat amb el pes i l'altura
    men = as.integer(sex == "H"),
    smoke_text = factor(smoke, labels = c("never", "current", "ex")))
# no calculem el temps a diabetis
    # diab.t = if_else(!is.na(diabetes_a10) & diabetes_a10 <= as.Date(dintro),
    #                  1 + as.numeric(as.Date(dintro) - diabetes_a10), 0)/365.25)

# Definim els esdeveniments CVD
load('data/00a-problems.RData')
cvd = problems %>%
  subset(ocip %in% maria$ocip) %>%
  # ami_atc ja està inclòs en ihd_atc
  # pad ja està inclòs en pad_atc
  subset(disease %in% c('ami', 'angor', 'ihd_atc', 'stroke', 'tia', 'pad_atc')) %>%
  group_by(ocip) %>%
  summarise(
    ind = which.min(date),
    first_cvd = date[ind],
    first_cvd_disease = disease[ind]) %>%
  select(-ind)

maria = maria %>%
  left_join(cvd, by = 'ocip')
# tapply(as.numeric(maria$first_cvd - maria$dintro), maria$first_cvd_disease, summary)

nephropathy = problems %>%
  filter(icd %in% c('E10.2', 'E11.2', 'E13.2', 'E14.2')) %>%
  mutate(disease = 'nephropathy')

problems = problems %>%
  bind_rows(nephropathy)

nephropathy = nephropathy %>%
  group_by(ocip) %>%
  summarise(nephropathy = min(date, na.rm = TRUE)) %>%
  ungroup()

retinopathy = problems %>%
  filter(icd %in% c('E10.3', 'E11.3', 'E13.3', 'E14.3'))  %>%
  mutate(disease = 'retinopathy')

problems = problems %>%
  bind_rows(retinopathy)

retinopathy = retinopathy %>%
  group_by(ocip) %>%
  summarise(retinopathy = min(date, na.rm = TRUE)) %>%
  ungroup()

neuropathy = problems %>%
  filter(icd %in% c('E10.4', 'E11.4', 'E13.4', 'E14.4'))  %>%
  mutate(disease = 'neuropathy')

problems = problems %>%
  bind_rows(neuropathy)

neuropathy = neuropathy %>%
  group_by(ocip) %>%
  summarise(neuropathy = min(date, na.rm = TRUE)) %>%
  ungroup()

maria = maria %>%
  left_join(nephropathy, by = 'ocip') %>%
  left_join(retinopathy, by = 'ocip') %>%
  left_join(neuropathy, by = 'ocip')


# EXPOSURE VARIABLES
maria = maria %>%
  mutate(
    ABI_LOW = as.integer(itb < 0.9),
    DIAB = as.integer(!is.na(diabetes_a10) & diabetes_a10 <= as.Date(dintro)),
    CVD = as.integer(!is.na(first_cvd) & as.Date(first_cvd) <= as.Date(dintro)),
    group = sprintf("DIAB=%d,CVD=%d,ABI.LOW=%d", DIAB, CVD, ABI_LOW))

# Prevalences
maria = maria %>%
  mutate(
    p.diabetes = as.integer(!is.na(diabetes_a10) & diabetes_a10 <= dintro),
    p.hf =  as.integer(!is.na(hf) & hf <= dintro),
    p.aff = as.integer(!is.na(aff) & aff <= dintro),
    p.nephropathy = as.integer(!is.na(nephropathy) & nephropathy <= dintro),
    p.retinopathy = as.integer(!is.na(retinopathy) & retinopathy <= dintro),
    p.neuropathy = as.integer(!is.na(neuropathy) & neuropathy <= dintro),
    #p.alcoholism = as.integer(!is.na(alcoholism) & alcoholism <= dintro),
    p.ami = as.integer (!is.na(ami) & ami <= dintro),
    p.ami_atc = as.integer (!is.na(ami_atc) & ami_atc <= dintro),
    p.angor = as.integer (!is.na(angor) & angor <= dintro),
    p.ihd_atc = as.integer (!is.na(ihd_atc) & ihd_atc <= dintro),
    p.tia = as.integer (!is.na(tia) & tia <= dintro),
    p.stroke = as.integer (!is.na(stroke) & stroke <= dintro),
    p.dyslipidemia = as.integer (!is.na(dyslipidemia) & dyslipidemia <= dintro),
    p.symp_pad = symp_pad,
    p.obesity = as.integer(!is.na(obesity) & obesity <= dintro),
    p.htn = as.integer(!is.na(htn) & htn <= dintro),
    p.arthritis = as.integer(!is.na(arthritis) & arthritis <= dintro),
    p.asthma = as.integer(!is.na(asthma) & asthma <= dintro),
    p.neoplasms_malignant = as.integer(!is.na(neoplasms_malignant) &
                                         neoplasms_malignant <= dintro),
    p.ckd = as.integer(!is.na(ckd) & ckd <= dintro),
    p.copd = as.integer(!is.na(copd) & copd <= dintro),
    p.hypothyroidism = as.integer(!is.na(hypothyroidism) & hypothyroidism <= dintro),
    p.a10 = as.integer(!is.na(a10) & dintro - 365 < a10 & a10 <= dintro),
    p.c02 = as.integer(!is.na(c02) & dintro - 365 < c02 & c02 <= dintro),             
    p.c03 = as.integer(!is.na(c03) & dintro - 365 < c03 & c03 <= dintro),
    p.c07 = as.integer(!is.na(c07) & dintro - 365 < c07 & c07 <= dintro),
    p.c08 = as.integer(!is.na(c08) & dintro - 365 < c08 & c08 <= dintro),
    p.c09 = as.integer(!is.na(c09) & dintro - 365 < c09 & c09 <= dintro),
    p.h02 = as.integer(!is.na(h02) & dintro - 365 < h02 & h02 <= dintro),
    p.n05 = as.integer(!is.na(n05) & dintro - 365 < n05 & n05 <= dintro),
    p.n06 = as.integer(!is.na(n06) & dintro - 365 < n06 & n06 <= dintro),
    p.statin = as.integer(!is.na(statin) & dintro - 365 < statin & statin <= dintro),
    p.c10nostatin = as.integer(!is.na(c10nostatin) & dintro - 365 < c10nostatin &
                                 c10nostatin <= dintro),
    p.aspirin = as.integer(!is.na(aspirin) & dintro - 365 < aspirin & aspirin <= dintro),
    p.hta_med = as.integer(p.c02 == 1 | p.c03 == 1 | p.c07 == 1 | p.c08 == 1 |
                             p.c09 == 1),
    p.dm_med = as.integer(p.a10 == 1),
    p.smoking = as.integer(!((is.na(smoke) & is.na(smoking)) |
                               !is.na(smoke) & smoke == 0 | 
                                (!is.na(smoke) & smoke == 2 &
                                   as.numeric(dintro - dsmoke) > 365))),
    group = if_else(p.cvd == 1, 'CVD-yes', if_else(p.diabetes == 1, 'CVD-no_DM2-yes', 'CVD-no_DM2-no')))

# Variables for incidences
df_incidence = function(EVENT){
  devent = problems %>%
    #seleccionem a problems database els ocips que tenen EVENT
    subset(ocip %in% maria$ocip & disease == EVENT) %>% 
    #els ajuntem amb la base de dades maria.mod, per ocip:
    left_join(maria %>% select(ocip, dintro), by = 'ocip')
  
  suppressWarnings(d.previous <- devent %>%
                     subset(date <= dintro) %>% # Busquem esdeveniment anterior més proper a dintro
                     group_by(ocip) %>%
                     summarise(d.prev = max(date)))
  devent %>%
    # traiem els events anteriors a dintro i de dp de 2015
    subset(dintro < date & date <= as.Date("2015-12-31")) %>% 
    group_by(ocip) %>%
    #aquí agafem la primera data en què ocorre un EVENT i en diem d.EVENT
    summarise(d.event = min(date)) %>% 
    left_join(d.previous, by = 'ocip') %>%
    # 28 instead of 0 if 28 period is required
    subset(is.na(d.prev) | 0 < as.numeric(d.event - d.prev)) %>% 
    select(-d.prev) %>%
    setNames(c('ocip', paste0('d.', EVENT))) #aquí es crea el d.ami, el d.stroke
}


maria = maria %>%
  #left_join(df_incidence(EVENT = 'ami'), by  = 'ocip') %>% mutate(d.ami.i = as.numeric(!is.na(d.ami))) %>%
  #left_join(df_incidence(EVENT = 'angor'), by  = 'ocip') %>% mutate(d.angor.i = as.numeric(!is.na(d.angor)))  %>%
  left_join(df_incidence(EVENT = 'stroke_i'), by  = 'ocip') %>% mutate(d.stroke_i.i = as.numeric(!is.na(d.stroke_i))) %>%
  left_join(df_incidence(EVENT = 'tia'), by  = 'ocip') %>% mutate(d.tia.i = as.numeric(!is.na(d.tia))) %>%
  left_join(df_incidence(EVENT = 'stroke_h'), by  = 'ocip') %>% mutate(d.stroke_h.i = as.numeric(!is.na(d.stroke_h))) %>%
  left_join(df_incidence(EVENT = 'stroke'), by = 'ocip') %>% mutate(d.stroke.i = as.numeric(!is.na(d.stroke))) %>%
  #left_join(df_incidence(EVENT = 'nephropathy'), by = 'ocip') %>% mutate(d.nephropathy.i = as.numeric(!is.na(d.nephropathy))) %>%
  #left_join(df_incidence(EVENT = 'retinopathy'), by = 'ocip') %>% mutate(d.retinopathy.i = as.numeric(!is.na(d.retinopathy))) %>%
  #left_join(df_incidence(EVENT = 'neuropathy'), by = 'ocip') %>% mutate(d.neuropathy.i = as.numeric(!is.na(d.neuropathy))) %>%
  mutate(d.death.i = exitusD)

maria = maria %>%
  mutate(
    d.death.t = as.numeric(dexitus - dintro)/365.25,
    #d.angor.t = as.numeric(if_else(d.angor.i == 1, d.angor, dexitus) - dintro)/365.25,
    #d.ami.t = as.numeric(if_else(d.ami.i == 1, d.ami, dexitus) - dintro)/365.25,
    d.tia.t = as.numeric(if_else(d.tia.i == 1, d.tia, dexitus) - dintro)/365.25,
    d.stroke_i.t = as.numeric(if_else(d.stroke_i.i == 1, d.stroke_i, dexitus) - dintro)/365.25,
    d.stroke_h.t = as.numeric(if_else(d.stroke_h.i == 1, d.stroke_h, dexitus) - dintro)/365.25,
    d.stroke.t = as.numeric(if_else(d.stroke.i == 1, d.stroke, dexitus) - dintro)/365.25
    #d.nephropathy.t = as.numeric(if_else(d.nephropathy.i == 1, d.nephropathy, dexitus) - dintro)/365.25,
    #d.retinopathy.t = as.numeric(if_else(d.retinopathy.i == 1, d.retinopathy, dexitus) - dintro)/365.25,
    #d.neuropathy.t = as.numeric(if_else(d.neuropathy.i == 1, d.neuropathy, dexitus) - dintro)/365.25
  )

# maria = maria %>%
#   mutate(
#     d.nephropathy.i = if_else(p.nephropathy == 1, NA_real_, d.nephropathy.i),
#     d.retinopathy.i = if_else(p.retinopathy == 1, NA_real_, d.retinopathy.i),
#     d.neuropathy.i = if_else(p.neuropathy == 1, NA_real_, d.neuropathy.i)
#   )

maria = maria %>%
  mutate(
    #d.chd.t = pmin(d.ami.t, d.angor.t),
    d.cerebrov.t = pmin(d.stroke_i.t, d.tia.t),
    #d.cvd.t = pmin(d.chd.t, d.cerebrov.t),
    #d.hard.t = pmin(d.ami.t, d.stroke_i.t),
    #d.chd.i = pmax(d.ami.i, d.angor.i),
    d.cerebrov.i = pmax(d.stroke_i.i, d.tia.i)
    #d.cvd.i = pmax(d.chd.i, d.cerebrov.i),
    #d.hard.i = pmax(d.ami.i, d.stroke_i.i)
  )

# Multiple Imputation variables
maria <-maria %>%
  mutate(
    ln_coltot = log(coltot),
    ln_colldl = log(colldl),
    ln_colhdl = log(colhdl),
    ln_tg = log(tg),
    ln_glu = log(glu),
    ln_pp = log(pp),
    ln_sbp = log(sbp))

# Categories ABI
maria = maria %>%
  mutate(itb_cat = cut(itb, breaks = c(0.4,0.5,0.7,0.9,1.1,1.3,max(itb)), include.lowest = T, right = F))

library(lubridate)
# Temps fins a diabetes
maria = maria %>%
  mutate(time_diab = interval(pmax(diabetes_a10, dbirth), dintro)/years(1))

# ### S'inclou alcohol
# alcohol2012 = data.table::fread('data.external/alcohol2012.txt', sep = '@', col.names = c('ocip', 'date', 'code', 'value'))
# alcohol2013 = data.table::fread('data.external/alcohol2013.txt', sep = '@', col.names = c('ocip', 'date', 'code', 'value'))
# alcohol = filter(bind_rows(alcohol2012, alcohol2013), ocip %in% maria$ocip)
# alcohol_prev = alcohol %>%
#   mutate(date = lubridate::ymd(date)) %>%
#   left_join(maria %>% select(ocip, dintro), by = 'ocip') %>%
#   filter(date <= dintro) %>%
#   arrange(ocip, desc(date)) %>%
#   group_by(ocip) %>%
#   slice(1) %>%
#   select(ocip, alcohol = value)
# maria = maria %>%
#   left_join(alcohol_prev, by = 'ocip')
# maria = maria %>%
#   mutate(
#     p.alcohol_risk = p.alcoholism | (!is.na(alcohol) & alcohol == 2),
#     p.alcohol_low = !p.alcoholism  & (!is.na(alcohol) & alcohol == 1)
#   )

save(maria, file = 'data/02-variables_of_interest.RData')
