FROM rocker/shiny:latest

#RUN apt-get update -qq && apt-get -y --no-install-recommends install \
#  libmysqlclient-dev\
#  libgdal-dev\
#  libproj-dev

#RUN apt-get update -qq && apt-get -y --no-install-recommends install \
#  libxml2-dev \
#  libcairo2-dev \
#  libsqlite-dev \
#  libmariadbd-dev \
#  libmariadbclient-dev \
#  libpq-dev \
#  libssh2-1-dev \
#  unixodbc-dev \
#  libsasl2-dev

RUN Rscript -e "install.packages(c('shinyjs', 'shinydashboard', 'DT', 'rlang'), repos = 'http://cran.us.r-project.org')"

RUN Rscript -e "install.packages(c('tidyverse', 'plotly', 'glue', 'gt'), repos = 'http://cran.us.r-project.org')"

ARG PROPHET=FALSE
RUN Rscript -e "install.packages(c('fpp3', 'fable.prophet'), repos = 'http://cran.us.r-project.org')"

RUN Rscript -e "install.packages(c('leaflet', 'sf', 'tmap'), repos = 'http://cran.us.r-project.org', dependencies = TRUE)"


## Copiar archivos al volumen
## COPY Mapas.Rproj ./Mapas.Rproj
## COPY app.R ./app.R
## COPY .RData ./.RData
## COPY .Rhistory ./.Rhistory

## COPY data ./data
## COPY docs ./docs
## COPY figures ./figures
## COPY models ./models
## COPY Modulos ./Modulos
## COPY src ./src
## COPY tests ./tests
## COPY www ./www

COPY . .

## Exponer el puerto
EXPOSE 3838

## Dar accesos
## RUN sudo chown -R shiny:shiny /

## Correr el contenedor
CMD ["R", "-e", "shiny::runApp('./app.R', host = '0.0.0.0', port = 3838)"]

