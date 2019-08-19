library(dplyr)
library(tidyr)
library(mice)
if(!exists('NIMP')) NIMP = 1

set.seed(1)
set.seed(.Random.seed[NIMP])

load('data/03b-mi-variable-selection.RData')

impute_category = function(category, D.imp){
  print(paste("Imputing: ", category))
  # Imputació tradicional
  mice(D.imp[[category]], m = 1, maxit = 60, method = "norm",
       predictorMatrix = predictionMatrices[[category]])
}
n_categories = names(D.imp)
imputation = setNames(lapply(n_categories, impute_category, D.imp), n_categories)

save(imputation, file = sprintf("data/imputation-process-imp_%02d.RData", NIMP))
