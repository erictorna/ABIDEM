# CODIS DEMENCIA

library(dplyr)
load('data.external/ecap.RData')
global = function(icd){
  # - dementia in Alzheimer’s disease (F00)
  # - vascular dementia (F01)
  # - dementia in other diseases classified elsewhere (F02.8)
  # - unspecified dementia (F03)
  # - Alzheimer’s disease (G30)
  # - Lewy body disease (G31.8)
  # -  frontotemporal dementia (G31.0)
  (substr(icd, 1, 3) == 'F00') |
    (substr(icd, 1, 3) == 'F01') |
    # (substr(icd, 1, 5) == 'F02.8') |
    (substr(icd, 1, 3) == 'F02') |
    (substr(icd, 1, 3) == 'F03') |
    (substr(icd, 1, 3) == 'G30') |
    (substr(icd, 1, 5) == 'G31.0') |
    (substr(icd, 1, 5) == 'G31.1') |
    (substr(icd, 1, 5) == 'G31.8')
}
alzheimer = function(icd){
  # - dementia in Alzheimer’s disease (F00 i G30)
  (substr(icd, 1, 3) == 'F00') |
    (substr(icd, 1, 3) == 'G30')
}
vascular = function(icd){
  # - vascular dementia (F01)
  (substr(icd, 1, 3) == 'F01')
}
unspecified = function(icd){
  # - Inespecífica (F03)
  # - Altres: F02.8, G31.8, G31.0
  (substr(icd, 1, 3) == 'F03')
}
other = function(icd){
  # - Altres: F02.8, G31.8, G31.0
  # (substr(icd, 1, 5) == 'F02.8') |
  (substr(icd, 1, 3) == 'F02') |
    (substr(icd, 1, 5) == 'G31.0') |
    (substr(icd, 1, 5) == 'G31.1') |
    (substr(icd, 1, 5) == 'G31.8')
}

dementia_global = ecap %>%
  filter(global(icd))

dementia_alzheimer = ecap %>%
  filter(alzheimer(icd))

dementia_vascular = ecap %>%
  filter(vascular(icd))

dementia_unspecified = ecap %>%
  filter(unspecified(icd))

dementia_other = ecap %>%
  filter(other(icd))

rm(ecap)
save.image(file = 'data/dementia_ani.RData')
