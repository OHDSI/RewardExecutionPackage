FROM rocker/rstudio:4.1.3

# Create path for code
COPY . /RewardExecutionPackage
COPY renv.lock ./

RUN sudo apt-get update -y \
    && sudo apt-get install -y --no-install-recommends \
                            libsodium-dev \
                            openjdk-11-jdk-headless \
                            ant \
                            git \
                            libbz2-dev \
                            liblzma-dev \
                            libcairo2-dev \
                            libcurl4-openssl-dev \
                            libfontconfig1-dev \
                            libpcre3-dev \
                            libssl-dev \
                            libxml2 \
                            libxml2-dev \
                            openjdk-11-jdk-headless \
                            pandoc \
                            libharfbuzz-dev \
                            zlib1g-dev \
    && sudo apt-get clean;

# Fix certificate issues for JAVA
RUN sudo apt-get update && \
    sudo apt-get install ca-certificates-java && \
    sudo apt-get clean && \
    sudo update-ca-certificates -f;

# Setup JAVA_HOME
ENV JAVA_HOME=/usr/lib/jvm/java-11-openjdk-amd64
ENV LD_LIBRARY_PATH=/usr/lib/jvm/java-11-openjdk-amd64/lib/server

RUN R CMD javareconf

RUN R -e "install.packages(c('devtools', 'renv'))"
RUN R -e "renv::restore()"
RUN R -e "devtools::install('./RewardExecutionPackage')"

CMD R