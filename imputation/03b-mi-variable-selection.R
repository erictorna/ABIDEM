library(dplyr)
library(tidyr)

load('data/03a-mi-dataset.RData')

TO_IMPUTE_TRANS = c("weight", "height", "ln_pp", "ln_sbp", "ln_glu", "ln_coltot", "ln_colhdl",
                    "ln_colldl", "ln_tg", "hba1c")

D.imp = list('all' = maria.imp)

getPredictionMatrix = function(.data){
  set.seed(1)
  .data = .data %>% sample_n(min(5000, nrow(.data)))
  X = .data %>% select_('age', 'men', ~starts_with('m.'), ~starts_with('p.'))
  all_zero = colSums(X) == 0
  all_one = colSums(X) == nrow(X)
  cat(sprintf("All zero or all one:\n%s", paste(names(X)[all_zero|all_one], collapse = '\n')))
  
  EXPLANATORY = X %>% select_(.dots = names(X)[!all_zero])
  vars_to_exp = function(vars) eval(parse(text = sprintf('~ %s', paste(vars, collapse='+'))))
  
  value.prediction = list()
  for(vy in TO_IMPUTE_TRANS){
    print(vy)
    sel = !is.na(.data[[vy]])
    EXPLANATORY.nona = subset(EXPLANATORY, sel)
    y = .data[[vy]][sel]
    vars = names(EXPLANATORY.nona)
    frm_lo = y~1
    frm_hi = eval(parse(text = sprintf('y~ %s', paste(vars, collapse='+'))))
    m = step(lm(frm_lo, data=EXPLANATORY.nona), 
             scope = list(lower=frm_lo, upper =frm_hi),
             direction = 'forward')
    value.prediction[[vy]] = attr(m$terms, 'term.labels')
  }
  
  missingness.prediction = list()
  for(vy in TO_IMPUTE_TRANS){
    print(vy)
    y = is.na(.data[[vy]])
    m = step(glm(y~1, data=EXPLANATORY, family = 'binomial'), 
             scope = eval(parse(text = sprintf('~ %s', paste(names(EXPLANATORY), collapse='+')))),
             direction = 'both')
    vars = names(EXPLANATORY.nona)
    frm_lo = eval(parse(text = sprintf('y~ %s', paste(value.prediction[[vy]], collapse='+'))))
    frm_hi = eval(parse(text = sprintf('y~ %s', paste(vars, collapse='+'))))
    m = step(glm(frm_lo, family='binomial', data=EXPLANATORY), 
             scope = list(lower=frm_lo, upper =frm_hi),
             direction = 'forward')
    missingness.prediction[[vy]] = attr(m$terms, 'term.labels')
  }
  
  numeric.prediction = list(
    'weight' = c('height', "ln_pp", "ln_sbp", "ln_coltot", "ln_colhdl", "ln_colldl", "ln_tg", "ln_glu", "hba1c"),
    'height' = c('weight', "ln_pp", "ln_sbp", "ln_coltot", "ln_colhdl", "ln_colldl", "ln_tg", "ln_glu", "hba1c"),
    "ln_pp" = c('weight', 'height', "ln_sbp", "ln_coltot", "ln_colhdl", "ln_colldl", "ln_tg", "ln_glu", "hba1c"),
    "ln_sbp" = c('weight', 'height', "ln_pp", "ln_coltot", "ln_colhdl", "ln_colldl", "ln_tg", "ln_glu", "hba1c"),
    "ln_coltot" = c('weight', 'height', "ln_pp", "ln_sbp", "ln_colhdl", "ln_colldl", "ln_glu", "hba1c"),
    "ln_colhdl" = c('weight', 'height', "ln_pp", "ln_sbp", "ln_coltot", "ln_colldl", "ln_glu", "hba1c"),
    "ln_colldl" = c('weight', 'height', "ln_pp", "ln_sbp", "ln_coltot", "ln_colhdl", "ln_glu", "hba1c"),
    "ln_tg" = c('weight', 'height', "ln_pp", "ln_sbp", "ln_coltot", "ln_colhdl", "ln_glu", "hba1c"),
    "ln_glu" = c('weight', 'height', "ln_pp", "ln_sbp", "ln_coltot", "ln_colhdl", "hba1c"),
    "hba1c" = c('weight', 'height', "ln_pp", "ln_sbp", "ln_coltot", "ln_colhdl", "ln_colldl", "ln_tg", "ln_glu"))
  
  
  predictionMatrix = matrix(0, ncol = ncol(.data), nrow = ncol(.data), 
                            dimnames = list(names(.data), names(.data)))
  for(vy in TO_IMPUTE_TRANS){
    predictionMatrix[vy, unique(c('na.event', 'event',
                                  value.prediction[[vy]], missingness.prediction[[vy]], numeric.prediction[[vy]]))] = 1
  }
  predictionMatrix
}

predictionMatrices = lapply(D.imp, getPredictionMatrix)

save(D.imp, predictionMatrices,
     file = 'data/03b-mi-variable-selection.RData')
