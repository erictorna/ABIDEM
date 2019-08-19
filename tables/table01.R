library(dplyr)
library(tidyr)
library(flextable)
library(officer)
source('R/global.R')

GROUPS_LABELS = c('CVD-no_DM2-no' = 'No diabetes', 
                  'CVD-no_DM2-yes' = 'Diabetes', 
                  'CVD-yes_DM2-no' = 'No diabetes', 
                  'CVD-yes_DM2-yes' = 'Diabetes')
GROUPS = names(GROUPS_LABELS)
Ns = sapply(GROUPS, function(GROUP){
  load(sprintf('tables/baseline-characteristics_%s.RData', GROUP))
  global.raw$numeric %>% 
    filter(variable == 'age') %>% 
    pull(n)
})
for(group in GROUPS){
  GROUPS_LABELS[group] = paste(GROUPS_LABELS[group], sprintf("n = %d", Ns[group]))
}

get_values = function(type, .variable, m_t = identity, s_t = m_t){
  sapply(GROUPS, get_values_, type, .variable, m_t = identity, s_t = m_t)
}
get_values_ = function(GROUP, type, .variable, m_t = identity, s_t = m_t){
  load(sprintf('tables/baseline-characteristics_%s.RData', GROUP))
  if(type == 'numeric'){
    row = global.imp[[type]] %>%
      filter(variable == .variable) %>%
      transmute(
        text = sprintf("%0.2f (%.2f)", m_t(mean), s_t(sd))
      ) %>% 
      unlist()
  }
  if(type == 'dichotomic'){
    row = global.imp[[type]] %>%
      filter(variable == .variable) %>%
      transmute(
        text = sprintf("%d (%.1f%%)", m, 100 * p)
      ) %>% 
      unlist()
  }
  row
}

# From mmol/mol to %
# (New number/10.929)+2.15
# 
# From % to mmol/mol
# (Old number-2.15)x10.929
info = list(#'N' = as.character(Ns),
  'ABI' = get_values('numeric', 'itb'),
  'Age, years' = get_values('numeric', 'age'),
  'Men' = get_values('dichotomic', 'men'),
  'Smoking habit' = get_values('dichotomic', 'p.smoking'),
  'Height, cm' = get_values('numeric', 'height'),
  'Weight, kg' = get_values('numeric', 'weight'),
  'BMI, kg/m²' = get_values('numeric', 'bmi'),
  'Systolic BP, mmHg' = get_values('numeric', 'sbp'),
  'Diastolic BP, mmHg' = get_values('numeric', 'dbp'),
  'Pulse Pressure, mmHg' = get_values('numeric', 'pp'),
  'Total Cholesterol, mg/dL'  = get_values('numeric', 'coltot'),
  'LDL Cholesterol, mg/dL' = get_values('numeric', 'colldl'),
  'HDL Cholesterol, mg/dL' = get_values('numeric', 'colhdl'),
  'Triglycerides, mg/dL' = get_values('numeric', 'tg'),
  # 'Total Cholesterol, mmol/L'  = get_values('numeric', 'coltot', function(x) x * 0.02586),
  # 'LDL Cholesterol, mmol/L' = get_values('numeric', 'colldl', function(x) x * 0.02586),
  # 'HDL Cholesterol, mmol/L' = get_values('numeric', 'colhdl', function(x) x * 0.02586),
  # 'Triglycerides, mmol/L' = get_values('numeric', 'tg', function(x) x * 0.01129),
  'Glucose, mg/dL' = get_values('numeric', 'glu'),
  'HbA1c, %' = get_values('numeric', 'hba1c'),
  # 'HbA1c, mmol/mol' = get_values('numeric', 'hba1c', m_t = function(x) (x-2.15)*10.929, s_t = function(x) x * 10.929),
  # 'Glucose, mmol/L' = get_values('numeric', 'glu', function(x) x * 0.0555),
  'Diabetes duration' = get_values('numeric', 'time_diab'),
  'Comorbidities' = rep('Comorbidities', 6),
  'Hypertension' = get_values('dichotomic', 'p.htn'),
  'Atrial fibrillation' = get_values('dichotomic', 'p.aff'),
  'Heart failure' = get_values('dichotomic', 'p.hf'),
  'COPD' = get_values('dichotomic', 'p.copd'),
  'CKD' = get_values('dichotomic', 'p.ckd'),
  'Malignant neoplasms' = get_values('dichotomic', 'p.neoplasms_malignant'),
  # 'Complications of diabetes' = rep('Complications of diabetes', 6),
  # 'Nephropathy' = get_values('dichotomic', 'p.nephro_ckd'),
  # 'Retinopathy' = get_values('dichotomic', 'p.retinopathy'),
  # 'Neuropathy' = get_values('dichotomic', 'p.neuropathy'),
  'Medication'  = rep('Medication', 6),
  'Antithrombotic agents' = get_values('dichotomic', 'p.b01'),
  'Antidiabetic therapy' = get_values('dichotomic', 'p.a10'),
  'Diuretics' = get_values('dichotomic', 'p.c03'),
  'Beta-blockers' = get_values('dichotomic', 'p.c07'),
  'Calcium channel blockers' = get_values('dichotomic', 'p.c08'),
  'Agents acting on the \nrenin-angiotensin system' = get_values('dichotomic', 'p.c09'),
  'Other antihypertensives' = get_values('dichotomic', 'p.c02'),
  'Statins' = get_values('dichotomic', 'p.statin'),
  'Other lipid-lowering agent' = get_values('dichotomic', 'p.c10nostatin')
)
K = length(info[[1]])

tab = tibble(
  names(info)) %>%
  bind_cols(as_tibble(lapply(1:K, function(i) sapply(info, nth, i)), .name_repair = 'unique')) 

typology = tibble(
  col_keys = names(tab),
  lab1 = c('', rep(c(sprintf("No previous CVD\nn = %d", Ns['CVD-no_DM2-no'] + Ns['CVD-no_DM2-yes']),
                     sprintf("Previous CVD\nn = %d", Ns['CVD-yes_DM2-no'] + Ns['CVD-yes_DM2-yes'])), c(2,2))),
  lab2 = c('', paste(rep(c('No diabetes', 'Diabetes'), 2), sprintf("n = %d", Ns), sep='\n')))


SUBTITLE = match(c('Comorbidities', 'Medication'), names(info))
tbl = tab %>%
  flextable(col_keys = names(tab)) %>%
  set_header_df(mapping = typology, key = "col_keys" ) %>%
  style(part='header', pr_p = fp_par(text.align = "center")) %>%
  bold(part = 'header') %>%
  merge_h(i = 1, part = 'header') %>%
  add_footer() %>%
  autofit() %>%
  height_all(0.25) %>%
  width(j = 1, 2) %>%
  width(j = 1+1:K, 1.1) %>%
  merge_h(i = SUBTITLE) %>%
  style(i = SUBTITLE, pr_t = fp_text(bold = TRUE, font.size = 11), 
        pr_p = fp_par(text.align = "left", padding.left = 100, padding.bottom = 0, padding.top = 12)) %>%
  #style(i = 21, pr_t = fp_text(bold = TRUE, font.size = 11), pr_p = fp_par(text.align = "left", padding.left = 100, padding.bottom = 0, padding.top = 10)) %>%
  bold(j = 1)
tbl  

heading = fpar(ftext("Table 1. Baseline Characteristics of the Study Population.",
                     fp_text(font.size = 11, bold = TRUE)))
footer1 = fpar(ftext("Values are presented as mean (SD) or N(%).", 
                     fp_text(font.size = 11, bold = FALSE)))
footer2 = fpar(ftext("ABI indicates ankle brachial index; BMI, body mass index; BP, blood pressure; COPD, chronic obstructive pulmonary disease; CKD, chronic kidney disease, HbA1c, glycated hemoglobin A; HDL, high density lipoprotein; LDL, low density lipoprotein; n, number of participants.", 
                     fp_text(font.size = 11, bold = FALSE)))
read_docx() %>%
  body_add_fpar(value = heading) %>%
  body_add_flextable(value = tbl, align = "left") %>%
  body_end_section_landscape() %>%
  body_add_fpar(value = footer1) %>%
  body_add_fpar(value = footer2) %>%  
  print(target = "www/table01.docx")
