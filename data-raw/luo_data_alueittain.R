data <- ptt_read_data("tyonv_12r5") |>
  dplyr::filter(tiedot %in% c("TYOTTOMATLOPUSSA", "AVPAIKATLOPUSSA")) |>
  tidyr::spread(tiedot, value)


data_kunnat <- dplyr::filter(data, grepl("KU", alue)) |> dplyr::rename(kunta_code = alue)
data_seutukunnat <- dplyr::filter(data, grepl("SK", alue)) |> dplyr::rename(seutukunta_code = alue)
data_maakunnat <- dplyr::filter(data, grepl("MK", alue)) |> dplyr::rename(maakunta_code = alue)

# Add regions
key <- statficlassifications::get_regionkey(only_codes = TRUE)
data_kunnat <- dplyr::mutate(data_kunnat,
                             maakunta_code = statficlassifications::key_recode(kunta_code, key, to = "maakunta_code"),
                             seutukunta_code = statficlassifications::key_recode(kunta_code, key, to = "seutukunta_code"))
data_seutukunnat <- dplyr::mutate(data_seutukunnat,
                                  maakunta_code = statficlassifications::key_recode(seutukunta_code, key, to = "maakunta_code"))

# Impute NAs
data_seutukunnat <- data_seutukunnat |>
  statfitools::recover_na(data_maakunnat, "seutukunta_code", "maakunta_code", "AVPAIKATLOPUSSA") |>
  statfitools::recover_na(data_maakunnat, "seutukunta_code", "maakunta_code", "TYOTTOMATLOPUSSA")
data_kunnat <- data_kunnat |>
  statfitools::recover_na(data_seutukunnat, "kunta_code", "seutukunta_code", "AVPAIKATLOPUSSA") |>
  statfitools::recover_na(data_seutukunnat, "kunta_code", "seutukunta_code", "TYOTTOMATLOPUSSA")

data_kunnat$region_level <- "kunta"
data_seutukunnat$region_level <- "seutukunta"
data_maakunnat$region_level <- "maakunta"

data_kunnat <- dplyr::select(data_kunnat, -seutukunta_code, -maakunta_code) |> dplyr::rename(alue = kunta_code)
data_seutukunnat <- dplyr::select(data_seutukunnat, -maakunta_code) |> dplyr::rename(alue = seutukunta_code)
data_maakunnat <- data_maakunnat |> dplyr::rename(alue = maakunta_code)

data <- rbind(data_kunnat, data_seutukunnat, data_maakunnat)

data_kokomaa <- data |> dplyr::group_by(time) |>
  dplyr::filter(region_level == "maakunta") |>
  dplyr::summarize(kokomaa_TYOTTOMATLOPUSSA = sum(TYOTTOMATLOPUSSA),
            kokomaa_AVPAIKATLOPUSSA = sum(AVPAIKATLOPUSSA))

data_alueittain <- dplyr::left_join(data, data_kokomaa, by = "time")

usethis::use_data(data_alueittain, overwrite = TRUE)
