# latest month

kuukaudet <-  c("Tammikuu", "Helmikuu", "Maaliskuu", "Huhtikuu", "Toukokuu", "Kesäkuu",
                "Heinäkuu", "Elokuu", "Syyskuu", "Lokakuu", "Marraskuu", "Joulukuu")
quarters <- c("01" = "Q1", "04" = "Q2", "07" = "Q3", "10" = "Q4")

data("indeksi_alueittain")
min_time <- min(indeksi_alueittain$time)
max_time <- max(indeksi_alueittain$time)
writeLines(paste(kuukaudet[as.double(substring(max_time, 6,7))], substring(max_time, 1, 4)),
           "text/parameters/lastdatamonth.txt", sep = "")
writeLines(paste(kuukaudet[as.double(substring(min_time, 6,7))], substring(min_time, 1, 4)),
           "text/parameters/firstdatamonth.txt", sep = "")

data("indeksi_suuralueittain")
indeksi_suuralueittain <- dplyr::filter(indeksi_suuralueittain, source == "tyovoimatutkimus")
min_time <- min(indeksi_suuralueittain$time)
max_time <- max(indeksi_suuralueittain$time)
writeLines(paste(quarters[substring(max_time, 6,7)], substring(max_time, 1, 4)),
           "text/parameters/lastdataquarter.txt", sep = "")
writeLines(paste(quarters[substring(min_time, 6,7)], substring(min_time, 1, 4)),
           "text/parameters/firstdataquarter.txt", sep = "")
