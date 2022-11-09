include global.make

TABLES = baseline-characteristics incidences hazard-ratios hazard-ratios-03

RDATA = $(foreach group,$(GROUPS),$(foreach table,$(TABLES),$(shell printf 'tables/%s_%s.RData' $(table) $(group))))
RMD = $(foreach group,$(GROUPS),$(foreach table,$(TABLES),$(shell printf 'www/table-%s_%s.html' $(table) $(group))))

all : $(RDATA) $(RMD) $(INDEX) 
    #    www/table01.docx www/table01a.docx www/table01b.docx \
    #    www/sup-table01.docx \
	#    www/sup-table02.docx www/sup-table02a.docx www/sup-table02b.docx \
	#    www/sup-table03.docx www/sup-table03a.docx \
	#    www/sup-table04.docx www/sup-table03a.docx 

www/index.html : index.Rmd
	Rscript -e 'rmarkdown::render("$<", output_file = "$(@F)", output_dir =  "$(@D)")'

tables/%_Men.RData : tables/%.R $(DATASETS) 
	Rscript -e 'GROUP = "Men"; source("$<")'

www/table-%_Men.html : tables/%.Rmd tables/%_Men.RData
	Rscript -e 'GROUP = "Men"; rmarkdown::render("$<", output_file = "$(@F)", output_dir =  "$(@D)")'

tables/%_Wom.RData : tables/%.R $(DATASETS) 
	Rscript -e 'GROUP = "Wom"; source("$<")'

www/table-%_Wom.html : tables/%.Rmd tables/%_Wom.RData
	Rscript -e 'GROUP = "Wom"; rmarkdown::render("$<", output_file = "$(@F)", output_dir =  "$(@D)")'

tables/%_ALL.RData : tables/%.R $(DATASETS) 
	Rscript -e 'GROUP = "ALL"; source("$<")'

www/table-%_ALL.html : tables/%.Rmd tables/%_ALL.RData
	Rscript -e 'GROUP = "ALL"; rmarkdown::render("$<", output_file = "$(@F)", output_dir =  "$(@D)")'

www/%.docx : tables/%.R $(RMD)
	Rscript $<


