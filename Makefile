all: build_site

build_site:
	Rscript -e 'source("renv/activate.R"); rmarkdown::render_site(encoding = "UTF-8")'