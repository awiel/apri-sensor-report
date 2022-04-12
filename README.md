# apri-sensor-report

# install
## install R
* sudo apt install r-base-core
* sudo apt install libcurl4-openssl-dev libmagick++-dev cmake libnlopt-dev
## install R packages
Start R:
* R
* install.packages(c("rcpp","labeling","farver","digest"))

install.packages(c("ggplot2","gtable","rlang","scales","R6","lifecycle","munsell","colorspace","glue","tibble","ellipsis","magrittr","crayon","pillar","fansi","utf8","vctrs","pkgconfig","withr"))

install.packages(c("magick","gargle","Rcpp","fs","purrr","cellranger"))

install.packages(c("fastmap","htmltools","sass","jquerylib","bslib","rmarkdown","reprex"))

install.packages(c("tidyverse","broom","dplyr","generics","tidyselect","tidyr","backports","dbplyr","DBI","assertthat","forcats","haven","hms","jsonlite","lubridate","modelr","readr","readxl","reprex","stringr","stringi","rstudioapi"))
*
install.packages("ggpubr")

## install apri-sensor-report
* git clone https://github.com/awiel/apri-sensor-report.git
* cd apri-sensor-report
* cp -r config-sample config
* cp -r report-sample report
* mkdir tmp/cache
* mkdir plot

# create report
1. add your sensor ID's to config/apri-sensor-sensorIds.json
2. add your sensor types to config/apri-sensor-sensorTypes.json
3. overrule defaults e.g outputfolder in config/apri-sensor-config.R
4. create your report file in folder report, see the example in the example folder

# report file
File in report folder, name: <report-name>.json

# Start report
  Rscript apri-sensor-generic-plot.R <report-name>

# Licence

Dit werk valt onder een Creative Commons Naamsvermelding 3.0 Nederland-licentie
![CC-BY](https://i.creativecommons.org/l/by/3.0/nl/88x31.png)

[CC BY 3.0 NL](http://creativecommons.org/licenses/by/3.0/nl/)
