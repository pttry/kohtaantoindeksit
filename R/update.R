update_data <- function() {

  source("data-raw/luo_statfin_data_robonomist.R"); rm(list = ls())
  source("data-raw/luo_data_alueittain.R"); rm(list = ls())
  source("data-raw/luo_data_ammateittain.R"); rm(list = ls())
  source("data-raw/luo_data_ammateittain_maakunnittain.R"); rm(list = ls())
  source("data-raw/luo_data_alueellinen_keskittyminen.R"); rm(list = ls())
  source("data-raw/luo_indeksit.R"); rm(list = ls())
  source("text/parameters/luo_parameters.R"); rm(list = ls())

}

update_figures <- function() {

  lapply(list.files("luo_kuviot/"), function(x) {source(paste0("luo_kuviot/", x)); rm(list = ls())})
  lapply(list.files("luo_kuviot_mv/"), function(x) {source(paste0("luo_kuviot_mv/", x)); rm(list = ls())})

}
