all: build_site

build_site:
	Rscript -e 'rmarkdown::render_site(encoding = "UTF-8")'