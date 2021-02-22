all: build_site

dependencies:
	Rscript -e 'source("renv/activate.R"); renv::restore()'
	julia -e 'using Pkg; Pkg.activate(".julia"); Pkg.instantiate(); Pkg.precompile()'
	
build_site: dependencies
	Rscript -e 'source("renv/activate.R"); rmarkdown::render_site(encoding = "UTF-8")'
	
clean:
	rm -rf *_cache *_files figures site_libs *.rds