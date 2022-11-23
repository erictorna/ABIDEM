include global.make

FIGURES = cumulative-incidences 

RDATA = $(foreach group,$(GROUPS),$(foreach figure,$(FIGURES),$(shell printf 'figures/%s_%s.RData' $(figure) $(group))))
RMD = $(foreach group,$(GROUPS),$(foreach figure,$(FIGURES),$(shell printf 'www/figure-%s_%s.html' $(figure) $(group))))

all : $(RDATA) $(RMD) $(INDEX) 

#www/flow-chart.svg www/figure01.svg www/figure01a.svg www/sup-figure01.svg www/sup-figure01a.svg

#       www/flow-chart.svg www/figure02.svg www/figure01_sup.svg www/figure02_sup.svg \
#       figures/smoothened-hazard-ratios-A.RData figures/smoothened-hazard-ratios-B.RData \
#       www/smoothened-hazard-ratios.html www/figure02_alt1.svg  www/figure02_alt2.svg 

www/index.html : index.Rmd
	Rscript -e 'rmarkdown::render("$<", output_file = "$(@F)", output_dir =  "$(@D)")'

figures/%_ALL.RData : figures/%.R $(DATASETS) 
	Rscript -e 'GROUP = "ALL"; source("$<")'

www/figure-%_ALL.html : figures/%.Rmd figures/%_ALL.RData
	Rscript -e 'GROUP = "ALL"; rmarkdown::render("$<", output_file = "$(@F)", output_dir =  "$(@D)")'

figures/%_Men.RData : figures/%.R $(DATASETS) 
	Rscript -e 'GROUP = "Men"; source("$<")'

www/figure-%_Men.html : figures/%.Rmd figures/%_Men.RData
	Rscript -e 'GROUP = "Men"; rmarkdown::render("$<", output_file = "$(@F)", output_dir =  "$(@D)")'

figures/%_Wom.RData : figures/%.R $(DATASETS) 
	Rscript -e 'GROUP = "Wom"; source("$<")'
	
www/figure-%_Wom.html : figures/%.Rmd figures/%_Wom.RData
	Rscript -e 'GROUP = "Wom"; rmarkdown::render("$<", output_file = "$(@F)", output_dir =  "$(@D)")'



