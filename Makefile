all : 
	$(MAKE) -f build.data.make -j 4 -k
	$(MAKE) -f imputation.make -j 10 -k
	$(MAKE) -f tables.make -j 5 -k
	$(MAKE) -f figures.make -k

