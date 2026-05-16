FROM rocker/r-ver:4.3.2

RUN apt-get update && apt-get install -y \
    libcurl4-openssl-dev \
    libssl-dev \
    libsasl2-dev \
    zlib1g-dev \
    libfontconfig1-dev \
    libfreetype6-dev \
    libpng-dev \
    libtiff5-dev \
    libjpeg-dev \
    libxml2-dev \
    libharfbuzz-dev \
    libfribidi-dev \
    libuv1-dev \
    && rm -rf /var/lib/apt/lists/*

RUN R -e "install.packages(c('shiny', 'bslib', 'mongolite', 'DT', 'dplyr', 'tidyr', 'httr2', 'future', 'promises'), repos='https://cloud.r-project.org/')"

RUN mkdir /app
COPY . /app/

RUN if [ -f /app/app.r ]; then mv /app/app.r /app/app.R; fi

EXPOSE 3838

CMD ["R", "-e", "shiny::runApp('/app', host = '0.0.0.0', port = 3838)"]
