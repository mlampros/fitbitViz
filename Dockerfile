FROM rocker/rstudio:devel

LABEL maintainer='Lampros Mouselimis'

RUN export DEBIAN_FRONTEND=noninteractive; apt-get -y update && \
 apt-get install -y libssl-dev zlib1g-dev libv8-dev pandoc pandoc-citeproc libgeos-dev libgeos++-dev default-jre-headless libgl1-mesa-dev make libmagic-dev gdal-bin libxml2-dev libmagick++-dev libfftw3-dev libproj-dev libgdal-dev imagemagick libicu-dev libudunits2-dev libcurl4-openssl-dev libpng-dev libglu1-mesa-dev && \
 apt-get install -y sudo && \
 apt-get -y update

RUN apt-get update \
	&& apt-get install -y --no-install-recommends \
                ghostscript \
                lmodern \
                pandoc-citeproc \
                qpdf \
                r-cran-formatr \
                r-cran-ggplot2 \
                r-cran-knitr \
		r-cran-rmarkdown \
                r-cran-runit \
                r-cran-testthat \
                texinfo \
                texlive-fonts-extra \
                texlive-fonts-recommended \
                texlive-latex-extra \
                texlive-latex-recommended \
                texlive-luatex \
                texlive-plain-generic \
                texlive-science \
                texlive-xetex

RUN R -e "install.packages(c( 'glue', 'httr', 'jsonlite', 'ggplot2', 'lubridate', 'patchwork', 'data.table', 'stats', 'viridis', 'scales', 'ggthemes', 'varian', 'paletteer', 'XML', 'hms', 'leaflet', 'sf', 'rstudioapi', 'grDevices', 'leafgl', 'raster', 'exactextractr', 'sp', 'magrittr', 'rayshader', 'OpenImageR', 'geodist', 'utils', 'CopernicusDEM', 'testthat', 'knitr', 'rmarkdown', 'DT', 'rgl', 'magick', 'remotes' ), repos =  'https://cloud.r-project.org/' )"

RUN  R -e "remotes::install_github('mlampros/fitbitViz', upgrade = 'always', dependencies = TRUE, repos = 'https://cloud.r-project.org/')" && \
 apt-get autoremove -y && \
 apt-get clean

ENV USER rstudio
