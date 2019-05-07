FLOWCHART = list()

library(dplyr)
# Carreguem població amb definició de symp_pad
load('data/00b-symptomatic_pad.RData')

load('DS_hypad_coronary.RData')
HYPAD_OCIP = hypad_imput_coronary[[1]]$ocip

table(HYPAD_OCIP %in% maria$ocip)

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
table(HYPAD_OCIP %in% maria$ocip)

maria <- maria %>%
  # Ens quedem amb els vius i que estiguin vius a partir del 01/01/2006
  subset(dintro < dexitus & as.Date("2006-01-01") < dexitus)# %>%
nrow(maria)
# Condició anterior i vius (post 2006 i post ITB): 73.865
#20180927 - Condició anterior i vius (post 2006 i post ITB): 70.251
table(HYPAD_OCIP %in% maria$ocip)

# Ens quedem amb diabètics
maria = maria %>%
  mutate(
    diabetes_a10 = pmin(diabetes, a10, na.rm = TRUE))

maria = filter(maria,
               ocip %in% HYPAD_OCIP)

# Ens quedem amb la gent entre 35 i 85 anys
maria = maria %>%
  subset(35 <= age & age <= 85)
nrow(maria)

# Excloem els que claudiquen
# Recopilem informació de claudicació
claudica = scan("data.external/claudica.txt", what = 'character')
nrow(maria)
# > 9132
maria <- maria %>%
  subset(!(itb < 0.9 &
             (itb < 0.4 |
                ocip %in% claudica |
                ocip %in% pad_treat_prev_ocip)))
# > 9132 - 8663
# [1] 469

maria$cvd = apply(maria[,c('ami', 'ami_atc', 'angor', 'ihd_atc', 'stroke', 'tia', 'pad_atc')], 1, min, na.rm=TRUE)
maria = maria %>%
  filter(!(!is.na(cvd) & cvd <= dintro))
# > 8663 - 7917
# [1] 746

maria = maria %>%
  subset(itb < 3)
# > 7917 - 7776
# [1] 141

# Total perdua
# > 9132 - 7776
# [1] 1356

save(maria, file = 'data/01-hypad_population.RData')

