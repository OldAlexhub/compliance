# Use the official R Shiny image as the base image
FROM rocker/shiny:latest

# Install system dependencies for R packages
RUN apt-get update && apt-get install -y \
    libcurl4-openssl-dev \
    libssl-dev \
    libxml2-dev \
    libfontconfig1-dev \
    libxt-dev \
    pandoc \
    pandoc-citeproc

# Install R packages required for the app
RUN R -e "install.packages(c('shiny', 'dplyr', 'ggplot2', 'shinythemes'), repos='https://cran.rstudio.com/')"

# Create app directory
RUN mkdir -p /srv/shiny-server/compliance

# Copy the app to the image
COPY app.R /srv/shiny-server/compliance/

# Make the Shiny app readable by all users
RUN chmod -R 755 /srv/shiny-server/

# Expose the port for the Shiny app
EXPOSE 3838

# Run the Shiny app
CMD ["/usr/bin/shiny-server"]
