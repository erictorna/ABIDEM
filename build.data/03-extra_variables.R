library(dplyr)
library(tidyr)
library(readr)
library(lubridate)

load('data/02-variables_of_interest.RData')

b01_2012 = read_delim('data.external/b01_2012.txt', delim = '@', col_names = FALSE)
b01_2013 = read_delim('data.external/b01_2013.txt', delim = '@', col_names = FALSE)

b01 = bind_rows(b01_2012, b01_2013) %>%
  select(ocip = X1, date = X2, atc = X3) %>%
  filter(ocip %in% maria[['ocip']]) %>%
  mutate(date = ymd(paste0(date,'28'))) %>%
  left_join(maria %>% select(ocip, dintro)) %>%
  filter(date <= dintro) %>%
  filter(dintro - years(1) <= date) %>%
  count(ocip)

b01a = bind_rows(b01_2012, b01_2013) %>%
  select(ocip = X1, date = X2, atc = X3) %>%
  filter(ocip %in% maria[['ocip']]) %>%
  filter(grepl("^B01A", atc)) %>%
  mutate(date = ymd(paste0(date,'28'))) %>%
  left_join(maria %>% select(ocip, dintro)) %>%
  filter(date <= dintro) %>%
  filter(dintro - years(1) <= date) %>%
  count(ocip)

b01aa = bind_rows(b01_2012, b01_2013) %>%
  select(ocip = X1, date = X2, atc = X3) %>%
  filter(ocip %in% maria[['ocip']]) %>%
  filter(grepl("^B01AA", atc)) %>%
  mutate(date = ymd(paste0(date,'28'))) %>%
  left_join(maria %>% select(ocip, dintro)) %>%
  filter(date <= dintro) %>%
  filter(dintro - years(1) <= date) %>%
  count(ocip)


b01ab = bind_rows(b01_2012, b01_2013) %>%
  select(ocip = X1, date = X2, atc = X3) %>%
  filter(ocip %in% maria[['ocip']]) %>%
  filter(grepl("^B01AB", atc)) %>%
  mutate(date = ymd(paste0(date,'28'))) %>%
  left_join(maria %>% select(ocip, dintro)) %>%
  filter(date <= dintro) %>%
  filter(dintro - years(1) <= date) %>%
  count(ocip)

b01ac = bind_rows(b01_2012, b01_2013) %>%
  select(ocip = X1, date = X2, atc = X3) %>%
  filter(ocip %in% maria[['ocip']]) %>%
  filter(grepl("^B01AC", atc)) %>%
  mutate(date = ymd(paste0(date,'28'))) %>%
  left_join(maria %>% select(ocip, dintro)) %>%
  filter(date <= dintro) %>%
  filter(dintro - years(1) <= date) %>%
  count(ocip)

b01a_other = bind_rows(b01_2012, b01_2013) %>%
  select(ocip = X1, date = X2, atc = X3) %>%
  filter(ocip %in% maria[['ocip']]) %>%
  filter(grepl("^B01A[DEX]", atc)) %>%
  mutate(date = ymd(paste0(date,'28'))) %>%
  left_join(maria %>% select(ocip, dintro)) %>%
  filter(date <= dintro) %>%
  filter(dintro - years(1) <= date) %>%
  count(ocip)

maria = maria %>%
  mutate(
    p.b01 = as.integer(ocip %in% b01$ocip),
    p.b01a = as.integer(ocip %in% b01a$ocip),
    p.b01aa = as.integer(ocip %in% b01aa$ocip),
    p.b01ab = as.integer(ocip %in% b01ab$ocip),
    p.b01ac = as.integer(ocip %in% b01ac$ocip),
    p.b01a_other = as.integer(ocip %in% b01a_other$ocip)
  )

# Algunes variables modificades per l'article de la Lia
maria = maria %>% mutate(p.c10 = ifelse(p.statin == 1 | p.c10nostatin == 1, 1, 0))
maria$p.statin<-NULL
maria$p.c10nostatin<-NULL
library(data.table)
setDT(maria)
maria=maria[maria$age>=50]
save(maria, file = 'data/03-extra_variables.RData')
