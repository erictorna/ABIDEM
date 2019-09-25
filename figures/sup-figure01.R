library(dplyr)
library(tidyr)
library(flextable)
library(officer)
library(ggplot2)
source('R/global.R')

GROUPS_LABELS = c('CVD-no_DM2-no' = 'No diabetes', 
                  'CVD-no_DM2-yes' = 'Diabetes')
GROUPS = names(GROUPS_LABELS)

load(sprintf('figures/hazard-ratios_%s.RData', GROUPS[1]))
dat.no = HRs.cc$`BIC criterion`$global %>%
  mutate(diab = 'No diabetes') %>%
  filter(stringr::str_sub(term, 1, 7) == 'itb_cat' & variable == 'd.stroke_h')

load(sprintf('figures/hazard-ratios_%s.RData', GROUPS[2]))
dat.yes = HRs.cc$`BIC criterion`$global %>%
  mutate(diab = 'Diabetes') %>%
  filter(stringr::str_sub(term, 1, 7) == 'itb_cat' & variable == 'd.stroke_h')

dat = bind_rows(dat.no, dat.yes) %>%
  transmute(
    diab,
    abi = gsub('itb_cat', '', term),
    hr = exp(estimate),
    lo = exp(estimate - 1.96 * std.error),
    hi = exp(estimate + 1.96 * std.error)
  ) %>%
  bind_rows(expand_grid(diab = c('No diabetes', 'Diabetes'), abi = '[1.1,1.3)', hr = 1, lo = 1, hi = 1)) %>%
  mutate(abi = factor(abi, levels = names(ABI_LABELS), labels = ABI_LABELS)) %>%
  mutate(
    hr = ifelse(is.finite(hi), hr, NA),
    lo = ifelse(is.finite(hi), lo, NA),
    hi = ifelse(is.finite(hi), hi, NA)
  )
dat

# labb = as_labeller(c(' '='', OUTCOMES)) labeller = labb, 
p1 = ggplot(data = dat) +
  geom_hline(aes(yintercept = 1), col = 'black', linetype = 2) +
  geom_errorbar(aes(x = abi, ymin = lo, ymax = hi), width = 0.2) +
  geom_point(aes(x = abi, y = hr), shape = 18, size = 2) +
  facet_wrap(~diab, ncol = 1, scales = 'free_x') +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    axis.title.x = element_text(color = 'black', face = 'bold'),
    axis.text.x = element_text(angle = 0, color = 'black'),
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    strip.text = element_text(face = 'bold', hjust = 0, color = 'white', size = 12),
    legend.justification = "top"
  ) +
  labs(y = 'Hazard ratios (log-scale)', x = '', col = 'Models') +
  scale_color_manual(breaks = c('Unadjusted', 'Adjusted'), values = c('grey', 'black')) +
  scale_y_continuous(trans = 'log', breaks = c(0.25, 1, 4, 16, 64, 256)) +
  coord_flip()
p1

p2 = ggplot() +
  geom_text(data = dat %>%
              mutate(text = ifelse(hr==1, 'Reference', sprintf("%0.1f (%0.1f - %0.1f)", hr, lo, hi))), aes(x = abi, y = 0.8+0.3, label = text), size = 4, hjust = 0) +
  #geom_hline(yintercept = 1, col = 'red', linetype = 2) +
  #geom_errorbar(aes(x = abi, ymin = lo, ymax = hi, col=method), position =  position_dodge(width=0.7), width = 0.2) +
  #geom_point(aes(x = abi, y = haz, col = method), shape = 18, size = 2, position =  position_dodge(width=0.7)) +
  facet_wrap(~diab, ncol = 1, scales = 'free_y') +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    axis.title.x = element_text(color = 'white', face = 'bold'),
    axis.text.x = element_text(angle = 0, color = 'white'),
    axis.text.y = element_text(face = 'bold', angle = 0, colour = 'black', size = 12),
    strip.text = element_text(face = 'bold', hjust = 1, color = 'black', size = 12)
  ) +
  labs(y = 'Hazard ratios', x = '', col = '') +
  coord_flip(ylim = c(1.1,1.1))
p2


library(ggplotify)
library(grid)
svg(filename = 'www/sup-figure01.svg', width = 6.5, height = 5.8)
grid.newpage()
c1 = 0.65
c2 = 0.35
vpa_ <- viewport(width = c1, height = 1, x = c1/2, y = 0.5)
vpb_ <- viewport(width = c2, height = 1, x = c1+c2/2-0.05, y = 0.5)
print(p2, vp = vpa_)
print(p1, vp = vpb_)
dev.off()

cairo_pdf(file = 'www/sup-figure01.pdf', width = 6.5, height = 5.8)
grid.newpage()
c1 = 0.65
c2 = 0.35
vpa_ <- viewport(width = c1, height = 1, x = c1/2, y = 0.5)
vpb_ <- viewport(width = c2, height = 1, x = c1+c2/2-0.05, y = 0.5)
print(p2, vp = vpa_)
print(p1, vp = vpb_)
dev.off()
