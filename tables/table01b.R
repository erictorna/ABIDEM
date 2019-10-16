library(dplyr)
library(tidyr)
library(flextable)
library(officer)
source('R/global.R')

GROUPS_LABELS = c('CVD-no_DM2-no' = 'No diabetes', 
                  'CVD-no_DM2-yes' = 'Diabetes')
GROUPS = names(GROUPS_LABELS)

GROUP = 'CVD-no_DM2-no'

get_values_ = function(GROUP, type, .variable, m_t = identity, s_t = m_t){
  load(sprintf('tables/baseline-characteristics_%s.RData', GROUP))
  if(type == 'numeric'){
    row = itb_cat.imp[[type]] %>%
      ungroup() %>%
      filter(variable == .variable) %>%
      transmute(
        itb_cat,
        text = sprintf("%0.2f (%.2f)", m_t(mean), s_t(sd))
      ) %>% 
      pivot_wider(names_from = itb_cat, values_from = text) %>%
      unlist()
  }
  if(type == 'dichotomic'){
    row = itb_cat.imp[[type]] %>%
      filter(variable == .variable) %>%
      ungroup() %>%
      transmute(
        itb_cat,
        text = sprintf("%d (%.1f%%)", m, 100 * p)
      ) %>% 
      pivot_wider(names_from = itb_cat, values_from = text) %>%
      unlist()
  }
  row
}
get_outcomes_ = function(GROUP, type, .variable){
  load(sprintf('tables/incidences_%s.RData', GROUP))
  if(type == 'events'){
    row = itb_cat.mi %>%
      filter(variable == .variable) %>%
      ungroup() %>% 
      transmute(
        itb_cat, 
        text = sprintf("%d (%.1f%%)",events, 100 * events/n))%>% 
      pivot_wider(names_from = itb_cat, values_from = text) %>%
      unlist()
  }
  if(type == 'incidence'){
    row = itb_cat.mi %>%
      filter(variable == .variable) %>%
      ungroup() %>% 
      transmute(
        itb_cat, 
        text = sprintf("%.1f (%.1f-%.1f)", inc, inc.lo, inc.hi))%>% 
      pivot_wider(names_from = itb_cat, values_from = text) %>%
      unlist()
  }
  row
}
get_values = function(type, .variable, m_t = identity, s_t = m_t){
  unname(get_values_(GROUP, type, .variable, m_t = identity, s_t = m_t))
}
get_outcomes = function(type, .variable){
  unname(get_outcomes_(GROUP, type, .variable))
}
K = 6
info = list(#'N' = as.character(Ns),
  'ABI' = get_values('numeric', 'itb'),
  'Age, years' = get_values('numeric', 'age'),
  'Men' = get_values('dichotomic', 'men'),
  'Smoking habit' = get_values('dichotomic', 'p.smoking'),
  ## Alcohol
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
  #'Diabetes duration' = get_values('numeric', 'time_diab'),
  'Comorbidities' = rep('Comorbidities', K),
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
  'Medications'  = rep('Medications', K),
  'Antidiabetic therapy' = get_values('dichotomic', 'p.a10'),
  'Vitamin K antagonists' = get_values('dichotomic', 'p.b01aa'),
  'Heparin group' = get_values('dichotomic', 'p.b01ab'),
  'Non-heparin platelet aggregation inhibitors' = get_values('dichotomic', 'p.b01ac'),
  'Other antithrombotic agents' = get_values('dichotomic', 'p.b01a_other'),
  'Diuretics' = get_values('dichotomic', 'p.c03'),
  'Beta-blockers' = get_values('dichotomic', 'p.c07'),
  'Calcium channel blockers' = get_values('dichotomic', 'p.c08'),
  'Agents acting on the \nrenin-angiotensin system' = get_values('dichotomic', 'p.c09'),
  'Other antihypertensives' = get_values('dichotomic', 'p.c02'),
  'Statins' = get_values('dichotomic', 'p.statin'),
  'Other lipid-lowering agent' = get_values('dichotomic', 'p.c10nostatin'),
  'Haemorrhagic stroke'  = rep('Haemorrhagic stroke', K),
  'Raw events, %' = get_outcomes('events', 'd.stroke_h'),
  'Incidence rate*' = get_outcomes('incidence', 'd.stroke_h')
)

load(sprintf('tables/baseline-characteristics_%s.RData', GROUP))
Ns = itb_cat.imp$numeric %>% 
  filter(variable == 'age') %>% 
  ungroup() %>%
  select(itb_cat, n) %>%
  pivot_wider(names_from = itb_cat, values_from = n)

tab = as_tibble(do.call('rbind', info), 
                rownames = "variable", .name_repair = ~make.names(1:length(Ns)))

typology = tibble(
  col_keys = names(tab),
  lab1 = c('Variable', '0.4≤ABI<0.5', '0.5≤ABI<0.7', '0.7≤ABI<0.9', '0.9≤ABI<1.1', '1.1≤ABI<1.3', '1.3≤ABI<3'),
  lab2 = c('', sprintf("n = %d", unlist(Ns))))

SUBTITLE = match(c('Comorbidities', 'Medications', 'Haemorrhagic stroke'), names(info))
FONT.SIZE = 8
tbl = tab %>%
  flextable(col_keys = names(tab)) %>%
  set_header_df(mapping = typology, key = "col_keys" ) %>%
  style(pr_t = fp_text(font.size = FONT.SIZE, bold = TRUE), pr_p = fp_par(text.align = "center"), part = 'header') %>%
  style(j = 1, pr_p = fp_par(text.align = "left"), part = 'header') %>%
  style(pr_t = fp_text(font.size = FONT.SIZE), pr_p = fp_par(text.align = "right")) %>%
  style(j = 1, pr_t = fp_text(font.size = FONT.SIZE, bold = TRUE), pr_p = fp_par(padding.left = 6, text.align = "left")) %>%
  style(j = 1, i = c(1:min(SUBTITLE), SUBTITLE), pr_t = fp_text(font.size = FONT.SIZE, bold = TRUE), pr_p = fp_par(padding.left = 0, text.align = "left")) %>%
  merge_h(i = SUBTITLE) %>%
  border_remove() %>%
  autofit() %>%
  width(j = 1, 1.5)
tbl

heading = fpar(ftext(sprintf("Characteristics of the study population without diabetes (n = %d)", sum(Ns)),
                     fp_text(font.size = FONT.SIZE, bold = TRUE)))
footer1 = fpar(ftext("Values are presented as mean (SD) or n (%). Haemorragic stroke is presented as events (%) and incidence rate (95%CI).", fp_text(font.size = FONT.SIZE, bold = FALSE)))
#footer2 = fpar(ftext("* Excluding heparin.", fp_text(font.size = FONT.SIZE, bold = FALSE)))
footer2 = fpar(ftext("* Per 1000 person-years.", fp_text(font.size = FONT.SIZE, bold = FALSE)))
footer3 = fpar(ftext("ABI indicates ankle brachial index; BP, blood pressure; COPD, chronic obstructive pulmonary disease; CKD, chronic kidney disease, HbA1c, glycated hemoglobin A; n, number of participants.", fp_text(font.size = FONT.SIZE, bold = FALSE)))

read_docx() %>%
  body_add_fpar(value = heading) %>%
  body_add_fpar(value = fpar()) %>%
  body_add_flextable(value = tbl, align = "left") %>%
  #body_end_section_landscape() %>%
  body_add_fpar(value = footer1) %>%
  body_add_fpar(value = footer2) %>%
  body_add_fpar(value = footer3) %>%  
  print(target = "www/table01b.docx")

