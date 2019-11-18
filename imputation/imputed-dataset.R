library(dplyr)
library(mice)
if(!exists('NIMP')) NIMP = 1

load(sprintf("data/imputation-process-imp_%02d.RData", NIMP))
load('build.data/ABIDEM.RData')

data.imp = tibble(.imp = rep(NIMP, nrow(maria))) %>%
  bind_cols(bind_rows(lapply(imputation, mice::complete), .id = '.category'))

maria.imp = data.imp %>%
  left_join(maria %>% 
              select(-weight, -height, -ln_sbp, -ln_pp, -ln_glu, -ln_coltot, -ln_colhdl, 
                     -ln_colldl, -ln_tg, -hba1c)) %>% 
  mutate(sbp = exp(ln_sbp),
         pp = exp(ln_pp),
         dbp = sbp - pp,
         coltot = exp(ln_coltot),
         colldl = exp(ln_colldl),
         tg = exp(ln_tg),
         bmi = weight/((height/100)^2),
         p.obesity = if_else(bmi >= 30, as.integer(1), p.obesity),
         colhdl = ifelse(is.na(colhdl), coltot - colldl - 0.16*tg, colhdl),
         glu = exp(ln_glu))

save(maria.imp, file = sprintf('data/imputed-dataset-imp_%02d.RData', NIMP))