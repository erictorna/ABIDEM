include global.make

TABLES = baseline-characteristics incidences hazard-ratios hazard-ratios-01 hazard-ratios-03 hazard-ratios-03-3_categories

RDATA = $(foreach group,$(GROUPS),$(foreach table,$(TABLES),$(shell printf 'tables/%s_%s.RData' $(table) $(group))))
RMD = $(foreach group,$(GROUPS),$(foreach table,$(TABLES),$(shell printf 'www/table-%s_%s.html' $(table) $(group))))

all : $(RDATA) $(RMD) $(INDEX) www/table01.docx www/table02.docx www/sup-table01.docx www/sup-table02.docx www/sup-table03.docx

www/index.html : index.Rmd
	Rscript -e 'rmarkdown::render("$<", output_file = "$(@F)", output_dir =  "$(@D)")'

tables/%_CVD-no_DM2-no.RData : tables/%.R $(DATASETS) 
	Rscript -e 'GROUP = "CVD-no_DM2-no"; source("$<")'

www/table-%_CVD-no_DM2-no.html : tables/%.Rmd tables/%_CVD-no_DM2-no.RData
	Rscript -e 'GROUP = "CVD-no_DM2-no"; rmarkdown::render("$<", output_file = "$(@F)", output_dir =  "$(@D)")'

tables/%_CVD-no_DM2-yes.RData : tables/%.R $(DATASETS) 
	Rscript -e 'GROUP = "CVD-no_DM2-yes"; source("$<")'

www/table-%_CVD-no_DM2-yes.html : tables/%.Rmd tables/%_CVD-no_DM2-yes.RData
	Rscript -e 'GROUP = "CVD-no_DM2-yes"; rmarkdown::render("$<", output_file = "$(@F)", output_dir =  "$(@D)")'

tables/%_CVD-yes.RData : tables/%.R $(DATASETS) 
	Rscript -e 'GROUP = "CVD-yes"; source("$<")'

www/table-%_CVD-yes.html : tables/%.Rmd tables/%_CVD-yes.RData
	Rscript -e 'GROUP = "CVD-yes"; rmarkdown::render("$<", output_file = "$(@F)", output_dir =  "$(@D)")'

tables/%_CVD-yes_DM2-no.RData : tables/%.R $(DATASETS) 
	Rscript -e 'GROUP = "CVD-yes_DM2-no"; source("$<")'

www/table-%_CVD-yes_DM2-no.html : tables/%.Rmd tables/%_CVD-yes_DM2-no.RData
	Rscript -e 'GROUP = "CVD-yes_DM2-no"; rmarkdown::render("$<", output_file = "$(@F)", output_dir =  "$(@D)")'

tables/%_CVD-yes_DM2-yes.RData : tables/%.R $(DATASETS) 
	Rscript -e 'GROUP = "CVD-yes_DM2-yes"; source("$<")'

www/table-%_CVD-yes_DM2-yes.html : tables/%.Rmd tables/%_CVD-yes_DM2-yes.RData
	Rscript -e 'GROUP = "CVD-yes_DM2-yes"; rmarkdown::render("$<", output_file = "$(@F)", output_dir =  "$(@D)")'

tables/%_ALL.RData : tables/%.R $(DATASETS) 
	Rscript -e 'GROUP = "ALL"; source("$<")'

www/table-%_ALL.html : tables/%.Rmd tables/%_ALL.RData
	Rscript -e 'GROUP = "ALL"; rmarkdown::render("$<", output_file = "$(@F)", output_dir =  "$(@D)")'

www/%.docx : tables/%.R $(RMD)
	Rscript $<


