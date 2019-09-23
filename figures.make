include global.make

FIGURES = cumulative-incidences hazard-ratios

RDATA = $(foreach group,$(GROUPS),$(foreach figure,$(FIGURES),$(shell printf 'figures/%s_%s.RData' $(figure) $(group))))
RMD = $(foreach group,$(GROUPS),$(foreach figure,$(FIGURES),$(shell printf 'www/figure-%s_%s.html' $(figure) $(group))))

all : $(RDATA) $(RMD) $(INDEX) www/flow-chart.svg www/figure01.svg www/sup-figure01.svg

#       www/flow-chart.svg www/figure02.svg www/figure01_sup.svg www/figure02_sup.svg \
#       figures/smoothened-hazard-ratios-A.RData figures/smoothened-hazard-ratios-B.RData \
#       www/smoothened-hazard-ratios.html www/figure02_alt1.svg  www/figure02_alt2.svg 

www/index.html : index.Rmd
	Rscript -e 'rmarkdown::render("$<", output_file = "$(@F)", output_dir =  "$(@D)")'

figures/%_CVD-no_DM2-no.RData : figures/%.R $(DATASETS) 
	Rscript -e 'GROUP = "CVD-no_DM2-no"; source("$<")'

www/figure-%_CVD-no_DM2-no.html : figures/%.Rmd figures/%_CVD-no_DM2-no.RData
	Rscript -e 'GROUP = "CVD-no_DM2-no"; rmarkdown::render("$<", output_file = "$(@F)", output_dir =  "$(@D)")'

figures/%_CVD-no_DM2-yes.RData : figures/%.R $(DATASETS) 
	Rscript -e 'GROUP = "CVD-no_DM2-yes"; source("$<")'

www/figure-%_CVD-no_DM2-yes.html : figures/%.Rmd figures/%_CVD-no_DM2-yes.RData
	Rscript -e 'GROUP = "CVD-no_DM2-yes"; rmarkdown::render("$<", output_file = "$(@F)", output_dir =  "$(@D)")'

figures/%_CVD-yes.RData : figures/%.R $(DATASETS) 
	Rscript -e 'GROUP = "CVD-yes"; source("$<")'

www/figure-%_CVD-yes.html : figures/%.Rmd figures/%_CVD-yes.RData
	Rscript -e 'GROUP = "CVD-yes"; rmarkdown::render("$<", output_file = "$(@F)", output_dir =  "$(@D)")'

figures/%_CVD-yes_DM2-no.RData : figures/%.R $(DATASETS) 
	Rscript -e 'GROUP = "CVD-yes_DM2-no"; source("$<")'

www/figure-%_CVD-yes_DM2-no.html : figures/%.Rmd figures/%_CVD-yes_DM2-no.RData
	Rscript -e 'GROUP = "CVD-yes_DM2-no"; rmarkdown::render("$<", output_file = "$(@F)", output_dir =  "$(@D)")'

figures/%_CVD-yes_DM2-yes.RData : figures/%.R $(DATASETS) 
	Rscript -e 'GROUP = "CVD-yes_DM2-yes"; source("$<")'

www/figure-%_CVD-yes_DM2-yes.html : figures/%.Rmd figures/%_CVD-yes_DM2-yes.RData
	Rscript -e 'GROUP = "CVD-yes_DM2-yes"; rmarkdown::render("$<", output_file = "$(@F)", output_dir =  "$(@D)")'

figures/%_ALL.RData : figures/%.R $(DATASETS) 
	Rscript -e 'GROUP = "ALL"; source("$<")'

www/figure-%_ALL.html : figures/%.Rmd figures/%_ALL.RData
	Rscript -e 'GROUP = "ALL"; rmarkdown::render("$<", output_file = "$(@F)", output_dir =  "$(@D)")'

www/flow-chart.svg : figures/flow-chart.R $(RMD)
	Rscript $<

www/%.svg : figures/%.R $(RMD)
	Rscript $<