FLOWCHART = list()

library(dplyr)
# Carreguem població amb definició de symp_pad
load('data/00b-symptomatic_pad.RData')
load('data/dementia_ani.RData')
dementia_global = dementia_global %>%
  filter(ocip %in% maria$ocip) %>%
  group_by(ocip) %>%
  summarise(dementia_global = min(dalta))

dementia_alzheimer = dementia_alzheimer %>%
  filter(ocip %in% maria$ocip) %>%
  group_by(ocip) %>%
  summarise(dementia_alzheimer = min(dalta))

dementia_vascular = dementia_vascular %>%
  filter(ocip %in% maria$ocip) %>%
  group_by(ocip) %>%
  summarise(dementia_vascular = min(dalta))

# dementia_unspecified = dementia_unspecified %>%
#   filter(ocip %in% maria$ocip) %>%
#   group_by(ocip) %>%
#   summarise(dementia_unspecified = min(dalta))


maria = maria %>%
  left_join(dementia_global, by = 'ocip') %>%
  left_join(dementia_alzheimer, by = 'ocip') %>%
  left_join(dementia_vascular, by = 'ocip') %>%
  # left_join(dementia_unspecified, by = 'ocip') %>%
  mutate(
    dementia_old = dementia,
    dementia = dementia_global
  )

nrow(maria)
#SIDIAP-Q amb ABI: 74.280

# maria %>%
#   count(as.Date("2006-01-01") < dintro) # Afegim 3.636
maria = maria %>%
  # 20181002: Canviem el dintro per les persones amb data anterior a 20060101,
  # els hi posarem la data d'inici d'estudi
  mutate(dintro_prev = dintro,
         dintro = if_else(dintro < as.Date("2006-01-01"), as.Date("2006-01-01"),
                          dintro)) %>% 
  #Select those whose ABI was measured between 2006-2011:
  subset(as.Date("2006-01-01") <= dintro & dintro <= as.Date("2011-12-31"))
nrow(maria)
#SIDIAP-Q amb ABI entre el 2006-2011: 74.280
#20180927 - SIDIAP-Q amb ABI entre el 2006-2011: 70.646

maria <- maria %>%
  # Ens quedem amb els vius i que estiguin vius a partir del 01/01/2006
  subset(dintro < dexitus & as.Date("2006-01-01") < dexitus)# %>%
nrow(maria)
# Condició anterior i vius (post 2006 i post ITB): 73.865
#20180927 - Condició anterior i vius (post 2006 i post ITB): 70.251

# Ens quedem amb la gent entre 35 i 85 anys
maria = maria %>%
  subset(35 <= age & age <= 85)
nrow(maria)
# Condició anterior amb edat entre 35 i 85 anys: 69069

FLOWCHART[['Aged 35-85']] = nrow(maria)

# Ens quedem amb diabètics
maria = maria %>%
  mutate(
    diabetes_a10 = pmin(diabetes, a10, na.rm = TRUE))
maria %>%
  count(!is.na(diabetes_a10) & diabetes_a10 <= dintro)
# maria = maria %>%
#   filter(!is.na(diabetes_a10) & diabetes_a10 <= dintro)
# No diabètics: 19466
# Diabètics: 49603

# Eliminem els diabètics T1
maria = maria %>%
  # Traiem gent amb diabetes T1
  subset(!((!is.na(diabetes) & diabetes <= dintro) &
             !(!is.na(diabetes_t2) & diabetes_t2 <= dintro)))
nrow(maria)
# 68069


FLOWCHART[['DM2 or without DM with ABI']] = nrow(maria)




# Excloem els que claudiquen
# Recopilem informació de claudicació
claudica = scan("data.external/claudica.txt", what = 'character')
maria %>%
  count(!(itb < 0.9 &
            (itb < 0.4 |
               ocip %in% claudica |
               ocip %in% pad_treat_prev_ocip))) # 3360
maria %>%
  count(!(itb < 0.9 &
            (itb < 0.4))) # 593

maria %>%
  subset(!(itb < 0.9 &
            (itb < 0.4))) %>%
  count(!(itb < 0.9 &
            (ocip %in% claudica))) # 257
maria %>%
  subset(!(itb < 0.9 &                  
             (itb < 0.4))) %>%
  subset(!(itb < 0.9 &
            (ocip %in% claudica))) %>%
  count(!(itb < 0.9 &
            (ocip %in% pad_treat_prev_ocip))) # 2510
maria <- maria %>%
  subset(!(itb < 0.9 &
             (itb < 0.4 |
                ocip %in% claudica |
                ocip %in% pad_treat_prev_ocip)))
nrow(maria)   ## 64709

FLOWCHART[['Without PAD symptomatic']] = nrow(maria) 

maria %>%
  count(itb >= 3)  # 2479
maria = maria %>%
  subset(itb < 3)
nrow(maria)
## 62230
FLOWCHART[['ABI < 3']] = nrow(maria)

maria$cvd = apply(maria[,c('ami', 'ami_atc', 'angor', 'ihd_atc', 'stroke', 'tia', 'pad_atc')], 1, min, na.rm=TRUE)
maria = maria %>%
  mutate(
    p.cvd = as.integer(!is.na(cvd) & cvd <= dintro)
  )
# maria %>%
#   count()
# # 9521
maria = maria %>%
  filter(p.cvd == 0)

# nrow(maria)
# 34689
FLOWCHART[['Without CVD']] = nrow(maria)

maria$frailty = apply(maria[,c('plegies', 'transplantament', 'dialisi', 'dementia', 'neoplasms_malignant', 'atdom')], 1, min, na.rm=TRUE)
maria = maria %>%
  mutate(
    p.frailty = as.integer(!is.na(frailty) & frailty <= dintro)
  )

# maria %>%
#   count(!is.na(frailty) & frailty <= dintro)
# maria = maria %>%
#   filter(!(!is.na(frailty) & frailty <= dintro))
# nrow(maria)
# FLOWCHART[['Without CVD and Frailty']] = nrow(maria)



save(maria, FLOWCHART, file = 'data/01-abidem_population.RData')

