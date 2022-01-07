data("data_ammateittain")

as <- seq(0.25, 0.75, by = 0.05)
mismatch_ammatti <- data.frame()

for(a in as){

  mismatch_temp <- data_ammateittain |>
    dplyr::group_by(time, ammattiryhma_codetaso) |>
    dplyr::summarize(mismatch = 1 - sum((AVPAIKATLOPUSSA/kokomaa_AVPAIKATLOPUSSA)^a * (TYOTTOMATLOPUSSA/kokomaa_TYOTTOMATLOPUSSA)^(1-a)))  |>
    dplyr::ungroup() |>
    dplyr::group_by(ammattiryhma_codetaso) |>
    dplyr::mutate(mismatch_sa = statfitools::sa_series(mismatch, time),
           mismatch_trend = statfitools::trend_series(mismatch, time)) |>
    tidyr::gather(tiedot, value, -time, -ammattiryhma_codetaso) |>
    dplyr::ungroup()
  mismatch_temp$a <- a
  mismatch_ammatti <- rbind(mismatch_ammatti, mismatch_temp)
  print(a); Sys.sleep(0.01)
}

indeksi_ammateittain <- mismatch_ammatti
usethis::use_data(indeksi_ammateittain, overwrite = TRUE)
