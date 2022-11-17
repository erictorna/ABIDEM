# CODIS DEMENCIA
library(dplyr)
library(tidyr)

load("data.external/CopyOfmaria.RData")
load('data.external/ecap.RData')
load('data.external/cmbd.RData')
load('data/n06d.RData')

cmbd_long <- cmbd %>%
  select(ocip, d_ingres, d_alta, c_alta, dp:ds9) %>% 
  pivot_longer(cols = dp:ds9)

global <- function(icd){
  # - dementia in Alzheimer’s disease (F00)
  # - vascular dementia (F01)
  # - dementia in other diseases classified elsewhere (F02.8)
  # - unspecified dementia (F03)
  # - Alzheimer’s disease (G30)
  # - Lewy body disease (G31.8)
  # -  frontotemporal dementia (G31.0)
  # - Meds N06D
  (substr(icd, 1, 3) == 'F00') |
    (substr(icd, 1, 3) == 'F01') |
    (substr(icd, 1, 3) == 'F02') |
    (substr(icd, 1, 3) == 'F03') |
    (substr(icd, 1, 3) == 'G30') |
    (substr(icd, 1, 5) == 'G31.0') |
    (substr(icd, 1, 5) == 'G31.8')
}
alzheimer <- function(icd){
  # - dementia in Alzheimer’s disease (F00 i G30)
  (substr(icd, 1, 3) == 'F00') |
    (substr(icd, 1, 3) == 'G30')
}
vascular <- function(icd){
  # - vascular dementia (F01)
  (substr(icd, 1, 3) == 'F01')
}
unspecified <- function(icd){
  # - Inespecífica (F03)
  (substr(icd, 1, 3) == 'F03')
}
other <- function(icd){
  # - Altres: F02, G31.8, G31.0
  (substr(icd, 1, 3) == 'F02') |
    (substr(icd, 1, 5) == 'G31.8') |
    (substr(icd, 1, 5) == 'G31.0')
}

dementia <- ecap %>%
  filter(global(icd)) %>%
  group_by(ocip, icd, dalta) %>%
  summarise(dbaixa = max(dbaixa, na.rm = T)) %>% 
  ungroup %>% 
  unique %>% 
  mutate(subtipus = if_else(alzheimer(icd), 'alzheimer', as.character(NA)),
         subtipus = if_else(vascular(icd), 'vascular', subtipus),
         subtipus = if_else(unspecified(icd), 'inespecifica', subtipus),
         subtipus = if_else(other(icd), 'altres', subtipus),
         font = 'ecap') %>%
  bind_rows(cmbd_long %>%
              filter(substr(value, 1, 3) == 290 |
                       substr(value, 1, 4) %in% c(2941, 2942, 3310, 3311, 33182)) %>%
              select(-name, -d_alta, -c_alta) %>% 
              unique %>% 
              mutate(subtipus = if_else(substr(value, 1, 4) == 3310, 'alzheimer', 'inespecifica'),
                     subtipus = if_else(substr(value, 1, 4) == 2904, 'vascular', subtipus),
                     subtipus = if_else(substr(value, 1, 4) %in% c(3311, 33182), 'altres', subtipus)) %>%
              transmute(ocip,
                        icd = value,
                        dalta = d_ingres,
                        dbaixa = as.Date(NA),
                        subtipus,
                        font = 'cmbd'))

# dementia_alzheimer = ecap %>%
#   filter(alzheimer(icd))
# 
# dementia_vascular = ecap %>%
#   filter(vascular(icd))
# 
# dementia_unspecified = ecap %>%
#   filter(unspecified(icd))
# 
# dementia_other = ecap %>%
#   filter(other(icd))

# Ens quedem amb la última demencia
demen_lst <- dementia %>%
  group_by(ocip) %>%
  summarise(dalta = min(dalta),
            dbaixa = max(dbaixa))
## Eliminem els casos donats de baixa
demen_lst <- demen_lst %>%
  filter(is.na(dbaixa))

## Ajuntem la informació quedant-nos amb el primer registre de cada font
demen_med <- fact_N06D %>%
  group_by(ocip) %>%
  summarise(dat = min(dbegin))

## Afeguim a les demencies la informació de les prescripcions
demen_pres_lst <- demen_lst %>% 
  full_join(demen_med %>%
              select(ocip, dat_med = dat),
            by = 'ocip')
## Ens quedem amb el 1r registre
dementia_global <- demen_pres_lst %>%
  mutate(dat_pbm = dalta,
         dalta = pmin(dat_pbm, dat_med, na.rm = T))

###################################################################################################
# Afeguim subtipus
###################################################################################################
# Subtipus
## Definim la malaltia cerebrovascular
tia_stroke <- ecap %>%
  filter(substr(icd, 1, 3) %in% c(paste0('I6', 0:9), paste0('G4', 5:6))) %>%
  mutate(font = 'ecap') %>% 
  bind_rows(cmbd_long %>%
              filter(substr(value, 1, 3) %in% c(430:435, 438)) %>%
              filter(value %in% c(43300, 43310, 43320, 43330, 43380, 43382, 43390, 43400, 43410, 43411, 43490)) %>%
              transmute(ocip,
                        icd = value,
                        dalta = d_ingres,
                        dbaixa = as.Date(NA),
                        font = 'cmbd')) %>%
  arrange(ocip, dalta)
## Per cada idp i dat ens quedem amb la màxima data de baixa
tia_stroke <- tia_stroke %>%
  group_by(ocip, dalta) %>%
  summarise(dbaixa = max(dbaixa))
## Eliminem els casos amb un sol diagnòstic i donats de baixa
sel_dbaixa <- tia_stroke %>%
  filter(!is.na(dbaixa)) %>%
  pull(ocip) # idp amb dbaixa
sel_dupli <- tia_stroke %>%
  count(ocip) %>%
  filter(1 < n) %>%
  pull(ocip) #idp amb més d'un registre
tia_stroke <- tia_stroke %>%
  filter(!(ocip %in% sel_dbaixa & !(ocip %in% sel_dupli)))
## Per cada idp ens quedem amb la mínima data d'alta i la màxima de baixa
tia_stroke_frst <- tia_stroke %>%
  group_by(ocip) %>%
  summarise(dalta = min(dalta),
            dbaixa = max(dbaixa))

## Definim malaltia PARKINSON
parkinson <- ecap %>%
  filter(substr(icd, 1, 3) %in% paste0('G2', 0:2)) %>%
  bind_rows(cmbd_long %>%
              filter(substr(value, 1, 3) %in% 332) %>%
              filter(value %in% c(43300, 43310, 43320, 43330, 43380, 43382, 43390, 43400, 43410, 43411, 43490)) %>%
              transmute(ocip,
                        icd = value,
                        dalta = d_ingres,
                        dbaixa = as.Date(NA),
                        font = 'cmbd')) %>%
  arrange(ocip, dalta)
## Per cada idp i dat ens quedem amb la màxima data de baixa
parkinson <- parkinson %>%
  group_by(ocip, dalta) %>%
  summarise(dbaixa = max(dbaixa))
## Eliminem els casos amb un sol diagnòstic i donats de baixa
sel_dbaixa <- parkinson %>%
  filter(!is.na(dbaixa)) %>%
  pull(ocip)  # idp amb dbaixa
sel_dupli <- parkinson %>%
  count(ocip) %>%
  filter(1 < n) %>% 
  pull(ocip) #idp amb més d'un registre
parkinson <- parkinson %>%
  filter(!(ocip %in% sel_dbaixa & !(ocip %in% sel_dupli)))
## Per cada idp ens quedem amb la mínima data d'alta i la màxima de baixa
parkinson_frst <- parkinson %>%
  group_by(ocip) %>%
  summarise(dalta = min(dalta),
            dbaixa = max(dbaixa))

## Comencem
## Classifiquem en subtipus
# demen_sub <- dementia %>%
#   mutate(subtipus = if_else(substr(icd, 1, 3) %in% c('F00', 'G30'), 'alzheimer', as.character(NA)),
#          subtipus = if_else(icd == 'F00.2',  'mixta', subtipus),
#          subtipus = if_else(substr(icd, 1, 3) == 'F01', 'vascular', subtipus),
#          subtipus = if_else(substr(icd, 1, 3) == 'F02' | icd %in% c('G31.0', 'G31.8'),
#                             'altres', subtipus),
#          subtipus = if_else(substr(icd, 1, 3) == 'F03', 'inespecifica', subtipus))
# Canviem inespecífica i alzheimer amb ictus o tia anterior a Vascular.
## Afeguim la malaltia cerebrovascular
demen_sub <- dementia %>% #demen_dub %>% 
  left_join(tia_stroke,
            by = 'ocip',
            suffix = c('', '.tia'))
## Marquem els registres amb una TIA entre (dat - 2*10000) < dat.tia & dat.tia < dat
demen_sub <- demen_sub %>%
  group_by(ocip, icd, dalta, dbaixa, subtipus) %>%
  summarise(tia = sum((dalta - 2*365.25) < dalta.tia & dalta.tia <= dalta)) %>%
  ungroup %>% 
  mutate(tia = if_else(is.na(tia), as.integer(0), tia),
         tia = as.numeric(0 < tia))

## Afeguim la malaltia de Parkison
# demen_sub <- [, park := 0]
demen_sub <- demen_sub %>%
  left_join(parkinson,
            by = 'ocip',
            suffix = c('', '.park'))
## Marquem els registres amb una Parkinson anterior
demen_sub <- demen_sub %>%
  group_by(ocip, icd, dalta, dbaixa, subtipus, tia) %>%
  summarise(park = sum(dalta.park <= dalta)) %>%
  ungroup %>% 
  mutate(park = if_else(is.na(park), as.integer(0), park),
         park_codi = as.numeric(0 < park),
         park = park_codi)
# demen_sub <- merge(demen_sub,
#                    park_med,
#                    by = 'idp',
#                    all.x = T)
# demen_sub[, park := park_codi | !is.na(dpark_med)]
demen_sub <- demen_sub %>%
  mutate(subtipus = if_else(subtipus == 'inespecifica' & tia == 1 & park == 0,
                            'vascular', subtipus),
         subtipus = if_else(subtipus == 'inespecifica' & tia == 1 & park == 1,
                            'altres', subtipus))

## Per cada idp, dat i subtipus ens quedem amb la màxima data de baixa
demen_sub <- demen_sub %>% 
  group_by(ocip, dalta, subtipus) %>%
  summarise(dbaixa = max(dbaixa),
            icd = paste(icd, collapse = ' ')) %>%
  ungroup()
## Ordenem per idp i dat
demen_sub <- demen_sub %>% 
  arrange(ocip, dalta)
## Eliminem els registres amb una data de baixa que tenen més d'un diagnòstic en el mateix dia
demen_sub <- demen_sub %>%
  group_by(ocip, dalta) %>%
  mutate(N = n()) %>%
  ungroup %>%
  filter(!(1 < N & !is.na(dbaixa)))
## Ens quedem amb els diagnòstics que no tenen data de baixa
demen_sub <- demen_sub %>%
  mutate(subtipus2 = if_else(is.na(dbaixa), subtipus, '')) %>%
  group_by(ocip) %>%
  mutate(nou_sub = cumsum(subtipus2 != ''),
         nou_sub = if_else(nou_sub == 0, as.integer(1), nou_sub)) %>%
  ungroup
demen_sub <- demen_sub %>%
  group_by(ocip, nou_sub) %>%
  summarise(dalta = min(dalta),
            dbaixa = max(dbaixa),
            subtipus = paste(subtipus2, collapse = '')) %>%
  ungroup
demen_sub <- demen_sub %>%
  filter(is.na(dbaixa))
## Ajuntem els diagnòstics en el mateix dia
demen_sub <- demen_sub %>%
  group_by(ocip, dalta) %>%
  mutate(subtipus = paste(subtipus, collapse = ' '),
         subtipus = if_else(subtipus == 'altres inespecifica', 'altres', subtipus),
         subtipus = if_else(subtipus == 'alzheimer inespecifica', 'alzheimer', subtipus),
         subtipus = if_else(subtipus %in% c('alzheimer inespecifica vascular', 'alzheimer mixta',
                                            'alzheimer mixta vascular', 'alzheimer vascular',
                                            'mixta vascular', 'altres alzheimer inespecifica'), 'mixta', subtipus),
         subtipus = if_else(subtipus == 'inespecifica vascular', 'vascular', subtipus),
         subtipus = if_else(subtipus == 'altres alzheimer vascular', 'altres mixta', subtipus)) %>% 
  ungroup
demen_sub <- demen_sub %>%
  arrange(ocip, dalta)
## Ajuntem els diagnòstics seguits iguals
demen_sub <- demen_sub %>%
  group_by(ocip) %>%
  mutate(diferent = subtipus != lag(subtipus),
         diferent = if_else(is.na(diferent), FALSE, diferent),
         n_demen = cumsum(diferent)) %>%
  ungroup()
demen_sub <- demen_sub %>%
  group_by(ocip, n_demen) %>%
  summarise(dalta = min(dalta),
            subtipus = first(subtipus)) %>%
  ungroup()
## Actualitzem la variable data de baixa
demen_sub <- demen_sub %>%
  group_by(ocip) %>%
  mutate(dbaixa2 = lead(dalta) - 1,
         subtipus_prev = lag(subtipus)) %>%
  ungroup
# demen_sub[, dbaixa2 := year(dbaixa2)*10000 + month(dbaixa2)*100 + mday(dbaixa2)]
demen_sub <- demen_sub %>%
  left_join(maria %>%
              select(ocip, dexitus),
            by = 'ocip') %>%
  mutate(dbaixa2 = pmin(dbaixa2, dexitus, na.rm = T)) %>%
  select(-dexitus) %>%
  ungroup %>%
  arrange(ocip, dalta)
# Creem la història de demencies
demen_sub_hst <- demen_sub %>%
  group_by(ocip) %>%
  summarise(subtipus = paste(subtipus, collapse = ' -> '))
## Ens quedem amb la última demencia
demen_sub_lst <- demen_sub %>%
  group_by(ocip) %>%
  summarise(dalta = min(dalta),
            dbaixa2 = max(dbaixa2),
            subtipus = last(subtipus),
            n_demen = last(n_demen),
            subtipus_prev = last(subtipus_prev)) %>%
  ungroup

## Afeguim els medicaments
demen_sub_pres <- demen_sub %>%
  full_join(fact_N06D %>% 
              select(ocip, agr, treat, dbegin, dend),
            by = 'ocip') %>%
  mutate(agr = if_else(dbaixa2 < dbegin, as.character(NA), agr),
         treat = if_else(dbaixa2 < dbegin, as.integer(NA), treat),
         dbegin = if_else(dbaixa2 < dbegin, as.Date(NA), dbegin),
         dend = if_else(dbaixa2 < dbegin, as.Date(NA), dend),
         # ACOLINE_ant = agr == 'ACOLINE',
         # MEMANTINA_ant = agr == 'MEMANTINA')
         agr = if_else(1 < n_demen & dend < dalta, as.character(NA), agr),
         treat = if_else(1 < n_demen & dend < dalta, as.integer(NA), treat),
         dbegin = if_else(1 < n_demen & dend < dalta, as.Date(NA), dbegin),
         dend = if_else(1 < n_demen & dend < dalta, as.Date(NA), dend),
         ACOLINE = agr == 'ACOLINE',
         MEMANTINA = agr == 'MEMANTINA',
         dalta_ant = dalta,
         dalta = pmin(dalta, dbegin, na.rm = T))
demen_sub_pres <- demen_sub_pres %>%
  group_by(ocip, n_demen) %>%
  summarise(dalta = min(dalta),
            subtipus = first(subtipus),
            dbaixa2 = max(dbaixa2),
            subtipus_prev = first(subtipus_prev),
            # ACOLINE_ant = max(ACOLINE_ant),
            # MEMANTINA_ant = max(MEMANTINA_ant),
            ACOLINE = max(ACOLINE),
            MEMANTINA = max(MEMANTINA)) %>%
  ungroup

# demen_sub_pres[, tia := 0]
## Afeguim la malaltia cerebrovascular
demen_sub_pres <- demen_sub_pres %>%
  left_join(tia_stroke,
            by = 'ocip',
            suffix = c('', '.tia'))
demen_sub_pres <- demen_sub_pres %>%
  group_by(ocip, n_demen, dalta, subtipus, dbaixa2, subtipus_prev, ACOLINE, MEMANTINA) %>%
  summarise(tia = sum((dalta - 2*365.25) < dalta.tia & dalta.tia <= dalta)) %>%
  ungroup
demen_sub_pres <- demen_sub_pres %>%
  mutate(tia = if_else(is.na(tia), as.integer(0), tia),
         tia = as.numeric(0 < tia))
## Afeguim la malaltia de Parkison
# demen_sub_pres[, park := 0]
demen_sub_pres <- demen_sub_pres %>%
  left_join(parkinson,
            by = 'ocip',
            suffix = c('', '.park'))
## Marquem els registres amb una Parkinson anterior
demen_sub_pres <- demen_sub_pres %>%
  group_by(ocip, n_demen, dalta, subtipus, dbaixa2, subtipus_prev, ACOLINE, MEMANTINA, tia) %>% 
  summarise(park = sum(dalta.park <= dalta)) %>%
  ungroup
demen_sub_pres <- demen_sub_pres %>%
  mutate(park = if_else(is.na(park), as.integer(0), park),
         park_codi := as.numeric(0 < park),
         park = park_codi)
# demen_sub_pres <- demen_sub_pres %>%
#   left_join(park_med,
#                         by = 'idp',
#                         all.x = T)
# demen_sub_pres[, park := park_codi | !is.na(dpark_med)]
demen_sub_pres <- demen_sub_pres %>%
  mutate(subtipus = if_else(is.na(n_demen) & tia == 0 & park == 0, 'alzheimer', subtipus),
         subtipus = if_else(is.na(n_demen) & tia == 1 & park == 0, 'vascular', subtipus),
         subtipus = if_else(is.na(n_demen) & tia == 0 & park == 1, 'altres', subtipus),
         subtipus = if_else(is.na(n_demen) & tia == 1 & park == 1, 'altres', subtipus),
         n_demen = if_else((is.na(n_demen) & tia == 0 & park == 0) |
                             (is.na(n_demen) & tia == 1 & park == 0) |
                             (is.na(n_demen) & tia == 0 & park == 1) |
                             (is.na(n_demen) & tia == 1 & park == 1), as.integer(1), n_demen),
         subtipus = if_else(subtipus == 'inespecifica' & !is.na(ACOLINE | MEMANTINA) & tia == 0 & park == 0,
                            'alzheimer', subtipus))

demen_sub_pres_lst <- demen_sub_pres %>%
  group_by(ocip) %>%
  summarise(dalta = min(dalta),
            dbaixa = max(dbaixa2),
            subtipus = last(subtipus),
            n_demen = last(n_demen),
            subtipus_prev = last(subtipus_prev),
            ACOLINE = last(ACOLINE),
            MEMANTINA = last(MEMANTINA)) %>%
  ungroup

dementia_alzheimer = demen_sub_pres_lst %>%
  filter(subtipus == "alzheimer") %>%
  select(ocip, dalta, dbaixa = dbaixa)

dementia_vascular = demen_sub_pres_lst %>%
  filter(subtipus == "vascular") %>%
  select(ocip, dalta, dbaixa = dbaixa)

save(dementia_global, dementia_alzheimer, dementia_vascular,
     file = 'data/dementia_ani.RData')
