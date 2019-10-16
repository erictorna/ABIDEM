library(dplyr)
library(tidyr)
library(flextable)
library(officer)
source('R/global.R')

GROUPS_LABELS = c('CVD-no_DM2-no' = 'No diabetes', 
                  'CVD-no_DM2-yes' = 'Diabetes')
GROUPS = names(GROUPS_LABELS)

load(sprintf("tables/hazard-ratios_%s.RData", GROUPS[1]))
no.raw = global.mi %>% 
  ungroup() %>%
  filter(variable == 'd.stroke_h') %>% 
  transmute(
    variable,
    var = rowname,
    hr = exp(estimate),
    lo = exp(estimate - 1.96 * std.error),
    hi = exp(estimate + 1.96 * std.error))
load(sprintf("tables/hazard-ratios-03_%s.RData", GROUPS[1]))
no.adj = global.mi %>% 
  ungroup() %>%
  filter(variable == 'd.stroke_h') %>% 
  transmute(
    variable,
    var = rowname,
    hr = exp(estimate),
    lo = exp(estimate - 1.96 * std.error),
    hi = exp(estimate + 1.96 * std.error))

load(sprintf("tables/hazard-ratios_%s.RData", GROUPS[2]))
yes.raw = global.mi %>% 
  ungroup() %>%
  filter(variable == 'd.stroke_h') %>% 
  transmute(
    variable,
    var = rowname,
    hr = exp(estimate),
    lo = exp(estimate - 1.96 * std.error),
    hi = exp(estimate + 1.96 * std.error))
load(sprintf("tables/hazard-ratios-03_%s.RData", GROUPS[2]))
yes.adj = global.mi %>% 
  ungroup() %>%
  filter(variable == 'd.stroke_h') %>% 
  transmute(
    variable,
    var = rowname,
    hr = exp(estimate),
    lo = exp(estimate - 1.96 * std.error),
    hi = exp(estimate + 1.96 * std.error))

info = bind_rows(
  no.raw %>% mutate(diab = 'No diabetes', model = 'raw'),
  no.adj %>% mutate(diab = 'No diabetes', model = 'adj'),
  yes.raw %>% mutate(diab = 'Diabetes', model = 'raw'),
  yes.adj %>% mutate(diab = 'Diabetes', model = 'adj')) %>%
  filter(stringr::str_sub(var, 1, 7) == 'itb_cat')

tab1 = info %>%
  mutate(text = ifelse(is.na(hr) | !is.finite(hi), '-', sprintf("%0.1f (%0.1f - %0.1f)", hr, lo, hi))) %>%
  pivot_wider(id_cols = c('diab', 'model'), names_from = 'var', values_from = c('text'))

tab = info %>%
  mutate(text = ifelse(is.na(hr) | !is.finite(hi), '-', sprintf("%0.1f (%0.1f - %0.1f)", hr, lo, hi))) %>%
  pivot_wider(id_cols = c('var', 'diab'), names_from = c('model'), values_from = c('text')) %>%
  select(diab, var, raw, adj) %>%
  bind_rows(tibble(diab = c('No diabetes', 'Diabetes'), var = "[1.1,1.3)", raw = '1 (Ref.)', adj = '1 (Ref.)')) %>%
  mutate(var = factor(gsub('itb_cat', '', var), levels = names(ABI_LABELS), labels = ABI_LABELS)) %>%
  arrange(diab, var)

typology = tibble(
  col_keys = names(tab),
  lab1 = c('', 'ABI category', 'Unadjusted hazard ratios', 'Adjusted hazard ratios'))


FONT.SIZE = 10
tbl = tab %>%
  flextable(col_keys = names(tab)) %>%
  set_header_df(mapping = typology, key = "col_keys" ) %>%
  style(pr_t = fp_text(font.size = FONT.SIZE, bold = TRUE), pr_p = fp_par(text.align = "center"), part = 'header') %>%
  style(j = 1, pr_p = fp_par(text.align = "left"), part = 'header') %>%
  style(pr_t = fp_text(font.size = FONT.SIZE), pr_p = fp_par(text.align = "right")) %>%
  style(j = 1:2, pr_t = fp_text(font.size = FONT.SIZE, bold = TRUE), pr_p = fp_par(padding.left = 6, text.align = "left")) %>%
  border_remove() %>%
  autofit() %>%
  merge_v() %>%
  padding(i = 7, padding.top = 10)
tbl

heading = fpar(ftext("Supplementary Table 4. Unadjusted and adjusted hazard ratios for haemorragic stroke by diabetic condition and ABI category",
                     fp_text(font.size = FONT.SIZE, bold = TRUE)))
footer1 = fpar(ftext("Hazard ratios and 95% condidence interval are presented.", fp_text(font.size = FONT.SIZE, bold = FALSE)))

read_docx() %>%
  body_add_fpar(value = heading) %>%
  body_add_fpar(value = fpar()) %>%
  body_add_flextable(value = tbl, align = "left") %>%
  #body_end_section_landscape() %>%
  body_add_fpar(value = footer1) %>%
  #body_add_fpar(value = footer4) %>%
  print(target = "www/sup-table03a.docx")

