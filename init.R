my_packages = c("tidyverse", "shiny", "shinydashboard", "plyr", "dplyr", "ggplot2", "zoo", "lubridate", "highcharter", "reshape2", "waffle", "RColorBrewer", "plotly")

install_if_missing = function(p) {
  if (p %in% rownames(installed.packages()) == FALSE) {
    install.packages(p)
  }
}

invisible(sapply(my_packages, install_if_missing))