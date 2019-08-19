include global.make

all : $(DATASETS) www/imputation-process.html $(TO_KEEP)

imputation/K-imp_%.RData : data/imputed-dataset-imp_%.RData
	cp $< $@

data/imputed-dataset-imp_00.RData : imputation/complete_case-dataset.R data/imputation-process-imp_01.RData build.data/K.RData
	Rscript $<

data/imputed-dataset-imp_%.RData : imputation/imputed-dataset.R data/imputation-process-imp_%.RData build.data/K.RData
	Rscript -e 'NIMP = $*; source("$<")'

data/imputation-process-imp_%.RData : imputation/imputation-process.R data/03b-mi-variable-selection.RData
	Rscript -e 'NIMP = $*; source("$<")'

data/03b-mi-variable-selection.RData : imputation/03b-mi-variable-selection.R data/03a-mi-dataset.RData
	Rscript $<

data/03a-mi-dataset.RData : imputation/03a-mi-dataset.R build.data/K.RData
	Rscript $<

www/imputation-process.html : imputation/imputation-process.Rmd $(DATASETS)
	Rscript -e 'rmarkdown::render("$<", output_file = "$(@F)", output_dir =  "$(@D)")'

## FILE BUILDING

imputation/imputation-process-imp_%.R : imputation/imputation-process.R
	sed '1iNIMP = $*' $< > $@

imputation/imputed-dataset-imp_%.R : imputation/imputed-dataset.R
	sed '1iNIMP = $*' $< > $@

# data/%.RData : imputation/%.R
# 	Rscript $<
