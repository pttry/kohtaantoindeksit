data("data_alueittain")

as <- seq(0.25, 0.75, by = 0.05)
mismatch_alue <- data.frame()

for(a in as){

  mismatch_temp <- data_alueittain |>
    dplyr::group_by(time, region_level) |>
    dplyr::summarize(mismatch = 1 -sum((AVPAIKATLOPUSSA/kokomaa_AVPAIKATLOPUSSA)^a * (TYOTTOMATLOPUSSA/kokomaa_TYOTTOMATLOPUSSA)^(1-a))) |>
    dplyr::ungroup() |>
    dplyr::group_by(region_level) |>
    dplyr::mutate(mismatch_sa = statfitools::sa_series(mismatch, time),
           mismatch_trend = statfitools::trend_series(mismatch, time),
           mismatch_sa_trend = statfitools::trend_series(mismatch_sa, time)) |>
    tidyr::gather(tiedot, value, -time, -region_level) |>
    dplyr::ungroup()
  mismatch_temp$a <- a
  mismatch_alue <- rbind(mismatch_alue, mismatch_temp)
  print(a); Sys.sleep(0.01)
}

indeksi_alueittain <- mismatch_alue
usethis::use_data(indeksi_alueittain, overwrite = TRUE)
