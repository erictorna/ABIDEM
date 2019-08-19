library(dplyr)
library(readr)
load('data/00a-diabi_initial_population.RData')

load("data.external/CopyOfpad_cmbd.RData")

# Comprovació de que els pad sempre té un pad_atc previ o al mateix dia
# View(maria %>%
#        left_join(pad_cmbd, by = 'ocip')
#        count(is.na(pad_atc), is.na(dpad_cmbd), is.na(pad), dpad_cmbd <= pad,
#              pad_atc <= pad))
maria = maria %>%                           # Filtrem ocips provinents del MARIA
  left_join(pad_cmbd, by = 'ocip')  %>%                                     # Afegim informació PAD_CMBD
  mutate(pad_atc = pmin(pad_atc, dpad_cmbd, pad, na.rm=TRUE)) # pad_atc és la primera aparició de pad_atc, dpad_cmbd i pad

# 19/09/2018: Comento
# Recollim tractaments PAD
c04_2012 = read_delim('data.external/c04_2012.txt',
                      delim = '@', col_names = FALSE) %>%
  select(ocip = X1, date = X2, atc = X3) %>%
  subset(atc %in% c('C04AD03', 'C04AX20', 'C04AX21'))
c04_2013 = read_delim('data.external/c04_2013.txt',
                      delim = '@', col_names = FALSE) %>%
  select(ocip = X1, date = X2, atc = X3) %>%
  subset(atc %in% c('C04AD03', 'C04AX20', 'C04AX21'))
b01_2012 = read_delim('data.external/b01_2012.txt',
                      delim = '@', col_names = FALSE) %>%
  select(ocip = X1, date = X2, atc = X3) %>%
  subset(atc == 'B01AC23')
b01_2013 = read_delim('data.external/b01_2013.txt',
                      delim = '@', col_names = FALSE) %>%
  select(ocip = X1, date = X2, atc = X3) %>%
  subset(atc == 'B01AC23')
PAD_TREAT = bind_rows(
  c04_2012 %>% subset(ocip %in% maria$ocip),
  c04_2013 %>% subset(ocip %in% maria$ocip),
  b01_2012 %>% subset(ocip %in% maria$ocip),
  b01_2013 %>% subset(ocip %in% maria$ocip))
pad_treat_prev_ocip = PAD_TREAT %>%
  left_join(maria %>% select(ocip, dintro), by = 'ocip') %>%
  mutate(date = as.Date(sprintf("%s01", date), format = '%Y%m%d')) %>%
  subset(date <= dintro) %>%
  count(ocip) %>%
  pull(ocip)

# Recopilem informació de claudicació
claudica = scan("data.external/claudica.txt", what = 'character')

maria = maria %>%
  mutate(
    symp_pad = 0L,
    # persons with history of pad
    # symp_pad = if_else(!is.na(pad_atc) & pad_atc <= dintro, 1L, symp_pad),
    # persons with ABI <0.4
    symp_pad = if_else(itb < 0.4, 1L, symp_pad),
    # <---- CLAUDICA + abi<09 : NEW LINE
    symp_pad = if_else(ocip %in% claudica & itb < 0.9, 1L, symp_pad),
    # persons under PAD related treatment
    symp_pad = if_else(ocip %in% pad_treat_prev_ocip & itb < 0.9, 1L, symp_pad))

save(maria, pad_treat_prev_ocip,
     file = 'data/00b-symptomatic_pad.RData')
