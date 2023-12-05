
data(tyonv_12r5)
data <- tyonv_12r5 |>
  dplyr::filter(tiedot %in% c("TYOTTOMATLOPUSSA", "AVPAIKATLOPUSSA", "TYOVOIMATK")) |>
  tidyr::spread(tiedot, value)

data_kunnat <- dplyr::filter(data, grepl("KU", alue)) |> dplyr::rename(kunta_code = alue)
data_seutukunnat <- dplyr::filter(data, grepl("SK", alue)) |> dplyr::rename(seutukunta_code = alue)
data_maakunnat <- dplyr::filter(data, grepl("MK", alue)) |> dplyr::rename(maakunta_code = alue)


data_kunnat$kunta_code <- as.character(data_kunnat$kunta_code)
data_seutukunnat$seutukunta_code <- as.character(data_seutukunnat$seutukunta_code)
data_kunnat <- add_region(data_kunnat, "maakunta_code", "seutukunta_code", from = "kunta_code")
data_seutukunnat <- add_region(data_seutukunnat, "maakunta_code", from = "seutukunta_code")

# Impute NAs
data_seutukunnat <- data_seutukunnat |>
  statfitools::recover_na(data_maakunnat, "seutukunta_code", "maakunta_code", "AVPAIKATLOPUSSA") |>
  statfitools::recover_na(data_maakunnat, "seutukunta_code", "maakunta_code", "TYOTTOMATLOPUSSA")
data_kunnat <- data_kunnat |>
  statfitools::recover_na(data_seutukunnat, "kunta_code", "seutukunta_code", "AVPAIKATLOPUSSA") |>
  statfitools::recover_na(data_seutukunnat, "kunta_code", "seutukunta_code", "TYOTTOMATLOPUSSA")

df_largest_kunnat <- data_kunnat |>
  dplyr::filter(time == max(time)) |>
  dplyr::arrange(desc(TYOVOIMATK))

output <- data.frame()

for(largest_kunnat_maara in c(50, 150)) {

  largest_kunnat <- df_largest_kunnat$kunta_code[1:largest_kunnat_maara]

  times <- as.character(unique(data_kunnat$time))
  df <- data.frame()
  data_kunnat <- dplyr::mutate(data_kunnat, time = as.character(time))

  for(aika in times) {
    data_temp <- dplyr::filter(data_kunnat, time == aika)
    kokomaa_TYOTTOMATLOPUSSA <- sum(data_temp$TYOTTOMATLOPUSSA, na.rm = TRUE)
    kokomaa_AVPAIKATLOPUSSA <- sum(data_temp$AVPAIKATLOPUSSA, na.rm = TRUE)
    data_largest <- dplyr::filter(data_temp, kunta_code %in% largest_kunnat)
    largest_TYOTTOMATLOPUSSA <- sum(data_largest$TYOTTOMATLOPUSSA, na.rm = TRUE)
    largest_AVPAIKATLOPUSSA <- sum(data_largest$AVPAIKATLOPUSSA, na.rm = TRUE)
    df_temp <- data.frame(indeksi_TYOTTOMATLOPUSSA = largest_TYOTTOMATLOPUSSA/kokomaa_TYOTTOMATLOPUSSA,
                          indeksi_AVPAIKATLOPUSSA = largest_AVPAIKATLOPUSSA/kokomaa_AVPAIKATLOPUSSA,
                          kokomaa_TYOTTOMATLOPUSSA = kokomaa_TYOTTOMATLOPUSSA,
                          kokomaa_AVPAIKATLOPUSSA = kokomaa_AVPAIKATLOPUSSA,
                          largest_TYOTTOMATLOPUSSA = largest_TYOTTOMATLOPUSSA,
                          largest_AVPAIKATLOPUSSA = largest_AVPAIKATLOPUSSA,
                          smallest_AVPAIKATLOPUSSA = kokomaa_AVPAIKATLOPUSSA - largest_AVPAIKATLOPUSSA,
                          time = aika)
    df <- rbind(df, df_temp)
  }

  df <- df|> tidyr::gather("tiedot", "value", -time) |>
    dplyr::mutate(time = as.Date(time))

  df$largest_kunnat_maara <- largest_kunnat_maara

  output <- rbind(output, df)

}


data_alueellinen_keskittyminen <- output

usethis::use_data(data_alueellinen_keskittyminen, overwrite = TRUE)
