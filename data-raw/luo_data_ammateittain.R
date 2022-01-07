# Hae data
data <- pttdatahaku::ptt_read_data("tyonv_12ti", only_codes = TRUE) |>
        dplyr::filter(alue %in% c("SSS","MK01","MK02","MK04","MK05","MK06","MK07","MK08","MK09","MK10","MK11",
                                  "MK12","MK13","MK14","MK15","MK16","MK17","MK18","MK19","MK21"))

# Poista luokka tuntematon
data <- data |>
  dplyr::filter(ammattiryhma != "SSS") |>
  dplyr::filter(!grepl("X", ammattiryhma))

# Impute NAs
data <- statfitools::randomize_na(data)

data <- data |> tidyr::spread(tiedot, value)

# Get national numbers
data_kokomaa <- data |>
  dplyr::filter(alue == "SSS") |>
  dplyr::group_by(time) |>
  dplyr::summarize(kokomaa_TYOTTOMATLOPUSSA = sum(TYOTTOMATLOPUSSA),
            kokomaa_AVPAIKATLOPUSSA = sum(AVPAIKATLOPUSSA))

data <- data |> dplyr::filter(alue != "SSS")

# Aggragate classification levels

data <- dplyr::bind_rows(
  lapply(1:4,
         FUN = function(i) {

           data |> dplyr::mutate(codetaso = substring(ammattiryhma, 1, i)) |>
             dplyr::group_by(codetaso, time) |>
             dplyr::summarize(TYOTTOMATLOPUSSA = sum(TYOTTOMATLOPUSSA, na.rm = TRUE),
                       AVPAIKATLOPUSSA = sum(AVPAIKATLOPUSSA, na.rm = TRUE)) |>
             dplyr::rename(ammattiryhma_code = codetaso) |>
             dplyr::mutate(ammattiryhma_codetaso = i) |>
             dplyr::ungroup()

         })
)


data_ammateittain <- dplyr::left_join(data, data_kokomaa, by = "time")

usethis::use_data(data_ammateittain, overwrite = TRUE)

