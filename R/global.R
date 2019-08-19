ABI_LEVELS = c('[1.1,1.3)', '[0.4,0.5)', '[0.5,0.7)', '[0.7,0.9)', '[0.9,1.1)', '[1.3,2.94]')
ABI_LABELS = list(
  '[0.4,0.5)' = "0.4 ≤ ABI < 0.5", 
  '[0.5,0.7)' = "0.5 ≤ ABI < 0.7", 
  '[0.7,0.9)' = "0.7 ≤ ABI < 0.9", 
  '[0.9,1.1)' = "0.9 ≤ ABI < 1.1", 
  '[1.1,1.3)' = "1.1 ≤ ABI < 1.3", 
  '[1.3,2.94]' = "1.3 ≤ ABI < 3")

filter_group = function(.data, GROUP){
  if(GROUP == 'ALL'){
    return(.data)
  }
  if(GROUP == 'CVD-yes_DM2-yes'){
    return(filter(.data, p.cvd == 1 & p.diabetes == 1))
  }
  if(GROUP == 'CVD-yes_DM2-no'){
    return(filter(.data, p.cvd == 1 & p.diabetes == 0))
  }
  .data %>%
    filter(group == GROUP)
}