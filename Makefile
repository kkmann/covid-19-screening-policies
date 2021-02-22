all: build_site

dependencies:
	Rscript -e 'source("renv/activate.R"); renv::restore()'
	julia -e 'using Pkg; Pkg.activate(".julia"); Pkg.instantiate(); Pkg.precompile()'
	
build_site: dependencies
	Rscript -e 'source("renv/activate.R"); source("run.R")'
	
clean:
	rm -rf *_cache *_files _site figures site_libs *.rds