all : build.data/K.RData

build.data/K.RData : data/03-extra_variables.RData
	cp $< $@
	
data/%.RData : build.data/%.R
	Rscript $<

data/03-extra_variables.RData : data/02-variables_of_interest.RData

data/02-variables_of_interest.RData : data/01-diabi_population.RData

data/01-diabi_population.RData : data/00b-symptomatic_pad.RData

data/00b-symptomatic_pad.RData : data/00a-diabi_initial_population.RData
