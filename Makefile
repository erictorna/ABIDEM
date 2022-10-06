all : 
	$(MAKE) -f build.data.make 
	$(MAKE) -f imputation.make  
	$(MAKE) -f tables.make  
	$(MAKE) -f figures.make 

