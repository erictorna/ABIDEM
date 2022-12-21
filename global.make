PROJECT=ABIDEM

NIMP=10
DATASETS = $(foreach imp,$(shell seq 0 $(NIMP)),$(shell printf 'imputation/$(PROJECT)-imp_%02d.RData' $(imp)))
TO_KEEP = $(foreach imp,$(shell seq 1 $(NIMP)),$(shell printf 'data/imputation-process-imp_%02d.RData' $(imp)))

RMD2HTML=R/run-knit2html.R
INDEX=www/index.html

GROUPS = ALL Men Wom

