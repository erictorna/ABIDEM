#Filtrem del MARIA i seleccionem gent activa dins l'ICS
library(dplyr)

load("data.external/CopyOfocips.maria.RData")
load("data.external/CopyOfmaria.RData")

nrow(maria) # 74.800
maria <- maria %>%
  # Ens quedem amb els oCIP de partida del JACC
  subset(ocip %in% omaria$ocip)
nrow(maria) # 74.280

# maria <- maria %>%
#   # Ens quedem amb els vius i que estiguin vius a partir del 01/01/2006
#   subset(dintro < dexitus & as.Date("2006-01-01") < dexitus)# %>%
# nrow(maria) #73.865
# # maria <- maria %>%
#   # Traiem individus amb incoherencies en temps fins a diabetes
#   # Jordi 19/09/2018: Comento la part de diabetis
#   # mutate(
#   #   diabetes_a10 = pmin(diabetes, a10, na.rm = TRUE),
#   #   DIAB.t = if_else(!is.na(diabetes_a10) & diabetes_a10 <= as.Date(dintro),
#   #                    1 + as.numeric(as.Date(dintro) - diabetes_a10), 0)/365.25) %>%
#   # subset(DIAB.t < age) %>% #elimina 49 persones
#   # select(-diabetes_a10, -DIAB.t)
# # nrow(maria) #73.865

nrow(problems) # 1.683.498
problems = problems %>%
  # Ens quedem amb els problemes dels oCIP's del projecte. 
  #Però en realitat no cal pq el tornem a generar a 02-variables_of_interest.R
  subset(ocip %in% maria$ocip)
nrow(problems) #1.677.349

save(maria, file = 'data/00a-diabi_initial_population.RData')
save(problems, file = 'data/00a-problems.RData')
