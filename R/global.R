ABI_LEVELS = c('[1.1,1.3)', '[0.4,0.5)', '[0.5,0.7)', '[0.7,0.9)', '[0.9,1.1)', '[1.3,3]')
ABI_LABELS = list(
  '[0.4,0.5)' = "0.4 ≤ ABI < 0.5",
  '[0.5,0.7)' = "0.5 ≤ ABI < 0.7",  
  '[0.7,0.9)' = "0.7 ≤ ABI < 0.9", 
  '[0.9,1.1)' = "0.9 ≤ ABI < 1.1", 
  '[1.1,3]' = "1.1 ≤ ABI < 3")

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
