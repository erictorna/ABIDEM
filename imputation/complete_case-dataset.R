library(dplyr)
library(mice)
NIMP = 0

load(sprintf("data/imputation-process-imp_%02d.RData", NIMP + 1))
load('build.data/ABIDEM.RData')

data.imp = bind_cols(bind_rows(lapply(imputation, mice::complete, action = 'long', include = TRUE), .id = '.category')) %>%
  filter(.imp == 0)

data.imp = data.imp %>%
  filter(complete.cases(data.imp %>% select(-time_diab)))

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
