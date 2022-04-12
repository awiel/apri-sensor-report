# apri-sensor-report

# install
## install R
* sudo apt install r-base-core
* sudo apt install libcurl4-openssl-dev libmagick++-dev
## install R packages
Start R: 
* R
* install.packages("tidyverse")
* install.packages("magick")

## install apri-sensor-report
* git clone https://github.com/awiel/apri-sensor-report.git
* cd apri-sensor-report
* cp -r config-sample config
* cp -r report-sample report
* mkdir tmp/cache
* mkdir plot

# create report
1. add your sensor to config/apri-sensor-sensorIds.json
2. create your report file in folder report, see the example in the example folder

# report file
File in report folder, name: <report-name>.json

# Start report
  Rscript apri-sensor-generic-plot.R <report-name>

# Licence
  
Dit werk valt onder een Creative Commons Naamsvermelding 3.0 Nederland-licentie
![CC-BY](https://i.creativecommons.org/l/by/3.0/nl/88x31.png)

[CC BY 3.0 NL](http://creativecommons.org/licenses/by/3.0/nl/)
