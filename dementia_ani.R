# CODIS DEMENCIA

library(dplyr)
load('data.external/ecap.RData')
dementia_ani = function(icd){
  # - dementia in Alzheimer’s disease (F00)
  # - vascular dementia (F01)
  # - dementia in other diseases classified elsewhere (F02.8)
  # - unspecified dementia (F03)
  # - Alzheimer’s disease (G30)
  # - Lewy body disease (G31.8)
  # -  frontotemporal dementia (G31.0)
  (substr(icd, 1, 3) == 'F00') |
    (substr(icd, 1, 3) == 'F01') |
    (substr(icd, 1, 5) == 'F02.8') |
    (substr(icd, 1, 3) == 'F03') |
    (substr(icd, 1, 3) == 'G30') |
    (substr(icd, 1, 5) == 'G31.8') |
    (substr(icd, 1, 5) == 'G31.0')
}
dementia_ani = ecap %>%
  filter(dementia_ani(icd))

save(dementia_ani, file = 'data/dementia_ani.RData')
