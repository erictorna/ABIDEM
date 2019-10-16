library(dplyr)
library(tidyr)
library(flextable)
library(officer)
source('R/global.R')

load(sprintf("tables/hazard-ratios_%s.RData", 'ALL'))
all.raw = global.mi %>% 
  ungroup() %>%
  filter(variable == 'd.stroke_h') %>% 
  transmute(
    variable,
    var = rowname,
    hr = exp(estimate),
    lo = exp(estimate - 1.96 * std.error),
    hi = exp(estimate + 1.96 * std.error))
load(sprintf("tables/hazard-ratios-03_%s.RData", 'ALL'))
all.adj = global.mi %>% 
  ungroup() %>%
  filter(variable == 'd.stroke_h') %>% 
  transmute(
    variable,
    var = rowname,
    hr = exp(estimate),
    lo = exp(estimate - 1.96 * std.error),
    hi = exp(estimate + 1.96 * std.error))



info = bind_rows(
  all.raw %>% mutate(model = 'raw'),
  all.adj %>% mutate(model = 'adj')) %>%
  filter(stringr::str_sub(var, 1, 7) == 'itb_cat')

tab1 = info %>%
  mutate(text = ifelse(is.na(hr) | !is.finite(hi), '-', sprintf("%0.1f (%0.1f - %0.1f)", hr, lo, hi))) %>%
  pivot_wider(id_cols = c('model'), names_from = 'var', values_from = c('text'))

tab = info %>%
  mutate(text = ifelse(is.na(hr) | !is.finite(hi), '-', sprintf("%0.1f (%0.1f - %0.1f)", hr, lo, hi))) %>%
  pivot_wider(id_cols = c('var'), names_from = c('model'), values_from = c('text')) %>%
  select(var, raw, adj) %>%
  bind_rows(tibble(var = "[1.1,1.3)", raw = '1 (Ref.)', adj = '1 (Ref.)')) %>%
  mutate(var = factor(gsub('itb_cat', '', var), levels = names(ABI_LABELS), labels = ABI_LABELS)) %>%
  arrange(var)

typology = tibble(
  col_keys = names(tab),
  lab1 = c('ABI category', 'Unadjusted hazard ratios', 'Adjusted hazard ratios'))


FONT.SIZE = 10
tbl = tab %>%
  flextable(col_keys = names(tab)) %>%
  set_header_df(mapping = typology, key = "col_keys" ) %>%
  style(pr_t = fp_text(font.size = FONT.SIZE, bold = TRUE), pr_p = fp_par(text.align = "center"), part = 'header') %>%
  style(j = 1, pr_p = fp_par(text.align = "left"), part = 'header') %>%
  style(pr_t = fp_text(font.size = FONT.SIZE), pr_p = fp_par(text.align = "right")) %>%
  style(j = 1, pr_t = fp_text(font.size = FONT.SIZE, bold = TRUE), pr_p = fp_par(padding.left = 6, text.align = "left")) %>%
  border_remove() %>%
  autofit() %>%
  merge_v()
tbl

heading = fpar(ftext("Supplementary Table 3. Unadjusted and adjusted hazard ratios for haemorragic stroke and ABI category",
                     fp_text(font.size = FONT.SIZE, bold = TRUE)))
footer1 = fpar(ftext("Hazard ratios and 95% condidence interval are presented.", fp_text(font.size = FONT.SIZE, bold = FALSE)))

read_docx() %>%
  body_add_fpar(value = heading) %>%
  body_add_fpar(value = fpar()) %>%
  body_add_flextable(value = tbl, align = "left") %>%
  #body_end_section_landscape() %>%
  body_add_fpar(value = footer1) %>%
  #body_add_fpar(value = footer4) %>%
  print(target = "www/sup-table03.docx")

