library(dplyr)
PROJECT = gsub("PROJECT=", "", scan(text = grep("PROJECT=", readLines('global.make'), value = TRUE), what = 'character'))
load(sprintf('build.data/%s.RData', PROJECT))
maria.imp = maria

maria.imp = maria.imp %>%
  mutate(
    time = d.dementia.t,
    event = d.dementia.i
  )
library(survival)
aalen = survfit(coxph(Surv(time, event)~1, data=maria.imp), type='aalen')
maria.imp$na.event = approx(aalen$time, -log(aalen$surv), xout=as.numeric(maria.imp$time))$y

vrs <- c("ocip", 'DIAB', "age", "itb", "weight", "height", "ln_sbp", "ln_pp","ln_glu", "hba1c", 
         "p.hf", "p.diabetes", "p.cvd", "p.b01aa", "p.b01ab", "p.b01ac", "p.b01a_other", 
         "ln_coltot",  "ln_colhdl", "ln_colldl",  "ln_tg", "men", "p.smoking", "p.htn", 
         "time_diab", "p.dyslipidemia",
         "p.arthritis", "p.asthma", "p.copd", "p.aff", "p.neoplasms_malignant", "p.ckd",  "p.cvd",
         "p.hypothyroidism", "p.ami", "p.angor", "p.tia", "p.stroke", 
         "p.a10", "p.c02", "p.c03", "p.c07", "p.c08", "p.alcohol_high", "p.alcohol_low",
         "p.c09", "p.h02", "p.n05", "p.aspirin", 'p.statin', 'p.c10nostatin', 'p.aspirin',
         "event", "na.event", "exitusT", "exitusD")

maria.imp = maria.imp %>%
  select(vrs)

save(maria.imp, file = 'data/03a-mi-dataset.RData')
