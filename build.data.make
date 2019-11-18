all : build.data/$(PROJECT).RData

build.data/$(PROJECT).RData : data/03-extra_variables.RData
	cp $< $@
	
data/%.RData : build.data/%.R
	Rscript $<

data/03-extra_variables.RData : data/02-variables_of_interest.RData 

data/02-variables_of_interest.RData : data/01-abidem_population.RData

data/01-abidem_population.RData : data/00b-symptomatic_pad.RData data/dementia_ani.RData

data/00b-symptomatic_pad.RData : data/00a-diabi_initial_population.RData
