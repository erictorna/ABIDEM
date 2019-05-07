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

maria = maria %>%
  mutate(
    p.b01 = as.integer(ocip %in% b01$ocip)
  )

save(maria, file = 'data/03-extra_variables.RData')
