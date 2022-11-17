ABI_LEVELS = c('[0,0.8)', '[0.8,0.9)', '[0.9,1.0)', '[1.0,1.4)', '[1.4,3]')
ABI_LABELS = list(
  '[0,0.8)' = "0.0 ≤ ABI < 0.8",  
  '[0.8,0.9)' = "0.8 ≤ ABI < 0.9", 
  '[0.9,1.0)' = "0.9 ≤ ABI < 1.0", 
  '[1.0,1.4)' = "1.0 ≤ ABI < 1.4", 
  '[1.4,3]' = "1.4 ≤ ABI < 3")

filter_group = function(.data, GROUP){
  if(GROUP == 'ALL'){
    return(.data)
  }
  if(GROUP == 'Men'){
    return(filter(.data, men == 1))
  }
  if(GROUP == 'Wom'){
    return(filter(.data, men == 0))
  }
  .data %>%
    filter(group == GROUP)
}
