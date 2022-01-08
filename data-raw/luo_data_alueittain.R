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


# suuralueittain, tyovoimatutkimus

data_avoimet_tyopaikat <- pttdatahaku::ptt_read_data("atp_11n1", only_codes = TRUE) |>
  tidyr::spread(tiedot, value) |>
  dplyr::rename(AVPAIKATLOPUSSA = atp_lkm) |>
  dplyr::filter(suuralue != "SSS")
data_tyottomat <- pttdatahaku::ptt_read_data("tyti_137i",only_codes = TRUE) |>
  dplyr::filter(tiedot == "Tyottomat") |>
  tidyr::spread(tiedot, value) |>
  dplyr::rename(suuralue = suuralue_2012) |>
  dplyr::filter(suuralue != "SSS") |>
  dplyr::mutate(TYOTTOMATLOPUSSA = 1000*Tyottomat) |>
  dplyr::filter(sukupuoli == "SSS")


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

data_tyonv_12r5 <- pttdatahaku::ptt_read_data("tyonv_12r5", only_codes = TRUE) |>
  dplyr::filter(grepl("MK", alue)) |>
  dplyr::rename(maakunta = alue) |>
  dplyr::filter(tiedot %in% c("TYOTTOMATLOPUSSA", "AVPAIKATLOPUSSA")) |>
  tidyr::spread(tiedot, value)

key <- statficlassifications::get_regionkey("maakunta", "suuralue", only_codes = TRUE)
data_tyonv_12r5 <- dplyr::mutate(data_tyonv_12r5,
                                 suuralue= statficlassifications::key_recode(maakunta, key, by = "values"))

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

