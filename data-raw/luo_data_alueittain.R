
data(tyonv_12r5)
data <- tyonv_12r5 |>
  dplyr::filter(tiedot %in% c("TYOTTOMATLOPUSSA", "AVPAIKATLOPUSSA")) |>
  tidyr::spread(tiedot, value)


data_kunnat <- dplyr::filter(data, grepl("KU", alue)) |> dplyr::rename(kunta_code = alue)
data_seutukunnat <- dplyr::filter(data, grepl("SK", alue)) |> dplyr::rename(seutukunta_code = alue)
data_maakunnat <- dplyr::filter(data, grepl("MK", alue)) |> dplyr::rename(maakunta_code = alue)

data_kunnat <- add_region(data_kunnat, "maakunta_code", "seutukunta_code", from = "kunta_code")
data_seutukunnat <- add_region(data_seutukunnat, "maakunta_code", from = "seutukunta_code")

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


# suuralueittain, tyovoimatutkimus

data("atp_11n1")
data_avoimet_tyopaikat <- atp_11n1 |>
  tidyr::spread(tiedot, value) |>
  dplyr::rename(AVPAIKATLOPUSSA = atp_lkm) |>
  dplyr::filter(suuralue != "SSS")

data("tyti_137i")
data_tyottomat <- tyti_137i |>
  dplyr::filter(tiedot == "Tyottomat") |>
  tidyr::spread(tiedot, value) |>
  dplyr::rename(suuralue = suuralue_2012) |>
  dplyr::filter(suuralue != "SSS") |>
  dplyr::mutate(TYOTTOMATLOPUSSA = 1000*Tyottomat)


data_kokomaa_avoimet_tyopaikat <- data_avoimet_tyopaikat |>
  dplyr::group_by(time) |>
  dplyr::summarize(kokomaa_AVPAIKATLOPUSSA = sum(AVPAIKATLOPUSSA))
data_kokomaa_tyottomat <- data_tyottomat |>
  dplyr::group_by(time) |>
  dplyr::summarize(kokomaa_TYOTTOMATLOPUSSA = sum(TYOTTOMATLOPUSSA))

data_tyottomat <- dplyr::left_join(data_tyottomat, data_kokomaa_tyottomat, by = "time")
data_avoimet_tyopaikat <- dplyr::left_join(data_avoimet_tyopaikat, data_kokomaa_avoimet_tyopaikat, by = "time")

data <- dplyr::left_join(data_tyottomat, data_avoimet_tyopaikat, by= c("suuralue", "time")) |>
  dplyr::filter(!is.na(AVPAIKATLOPUSSA))

data_suuralueittain_tyovoimatutkimus <- data

usethis::use_data(data_suuralueittain_tyovoimatutkimus, overwrite = TRUE)


# Suuralueittain, tyonvalitystilasto

data("tyonv_12r5")
data_tyonv_12r5 <- tyonv_12r5 |>
  dplyr::filter(grepl("MK", alue)) |>
  dplyr::rename(maakunta = alue) |>
  dplyr::filter(tiedot %in% c("TYOTTOMATLOPUSSA", "AVPAIKATLOPUSSA")) |>
  tidyr::spread(tiedot, value)

data_tyonv_12r5$maakunta_code <- as.character(data_tyonv_12r5$maakunta)
data_tyonv_12r5 <- add_region(data_tyonv_12r5, "suuralue_code", from = "maakunta_code")
data_tyonv_12r5 <- rename(data_tyonv_12r5, suuralue = suuralue_code) %>% select(-maakunta_code)

data_suuralueet <- data_tyonv_12r5 |>
                   dplyr::group_by(time, suuralue) |>
                   dplyr::summarize(TYOTTOMATLOPUSSA = sum(TYOTTOMATLOPUSSA),
                                    AVPAIKATLOPUSSA = sum(AVPAIKATLOPUSSA)) |>
                   dplyr::filter(suuralue != "SA5") |>
                   dplyr::ungroup()

data_kokomaa <- data_suuralueet |>
                dplyr::group_by(time) |>
                dplyr::summarize(kokomaa_TYOTTOMATLOPUSSA = sum(TYOTTOMATLOPUSSA),
                                 kokomaa_AVPAIKATLOPUSSA = sum(AVPAIKATLOPUSSA))

data <- dplyr::left_join(data_suuralueet, data_kokomaa, by = "time")

data_suuralueittain_tyonvalitystilasto <- data

usethis::use_data(data_suuralueittain_tyonvalitystilasto, overwrite = TRUE)

