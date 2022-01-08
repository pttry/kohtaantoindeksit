data <- pttdatahaku::ptt_read_data("tyonv_12ti", only_codes = TRUE) |>
  dplyr::filter(grepl("MK", alue)) |>
  dplyr::filter(ammattiryhma != "SSS",
         !grepl(pattern = "X", ammattiryhma)) |>
  tidyr::spread(tiedot, value)

data <- statfitools::randomize_na(data)

data_kokomaa <- data |> dplyr::group_by(time) |>
  dplyr::summarize(kokomaa_TYOTTOMATLOPUSSA = sum(TYOTTOMATLOPUSSA),
                   kokomaa_AVPAIKATLOPUSSA = sum(AVPAIKATLOPUSSA))

data0 <- data.frame()
for(i in 1:4) {
  data_temp <- data
  data_temp$ammattiryhma <- substring(data$ammattiryhma, 1, i)
  data_temp <- data_temp |>
    dplyr::group_by(ammattiryhma, time, alue) |>
    dplyr::summarize(TYOTTOMATLOPUSSA = sum(TYOTTOMATLOPUSSA),
                     AVPAIKATLOPUSSA = sum(AVPAIKATLOPUSSA)) |>
    dplyr::ungroup()
  data_temp$code_level <- i
  data0 <- rbind(data0, data_temp)
  print(i)
}

data_ammateittain_maakunnittain <- left_join(data0, data_kokomaa, by = "time")

usethis::use_data(data_ammateittain_maakunnittain, overwrite = TRUE)
