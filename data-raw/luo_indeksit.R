data("data_alueittain")
as <- seq(0.25, 0.75, by = 0.05)
indeksi_alueittain <- rbind(compute_mismatch_index(data_alueittain, as, by = region_level))
usethis::use_data(indeksi_alueittain, overwrite = TRUE)

data("data_ammateittain")
as <- seq(0.25, 0.75, by = 0.05)
indeksi_ammateittain <- compute_mismatch_index(data_ammateittain, as, by = ammattiryhma_codetaso)
usethis::use_data(indeksi_ammateittain, overwrite = TRUE)

data("data_ammateittain_maakunnittain")
as <- seq(0.25, 0.75, by = 0.05)
indeksi_ammateittain_maakunnittain <- compute_mismatch_index(data_ammateittain_maakunnittain, as, by = code_level)
usethis::use_data(indeksi_ammateittain_maakunnittain, overwrite = TRUE)

data("data_suuralueittain_tyovoimatutkimus")
as <- seq(0.25, 0.75, by = 0.05)
mismatch_tyovoimatutkimus <- compute_mismatch_index(data_suuralueittain_tyovoimatutkimus, as)

data("data_suuralueittain_tyonvalitystilasto")
as <- seq(0.25, 0.75, by = 0.05)
mismatch_tyonvalitystilasto <- compute_mismatch_index(data_suuralueittain_tyonvalitystilasto, as)

mismatch_tyonvalitystilasto$source <- "tyonvalitystilasto"
mismatch_tyovoimatutkimus$source <- "tyovoimatutkimus"
indeksi_suuralueittain <- rbind(mismatch_tyonvalitystilasto, mismatch_tyovoimatutkimus)

usethis::use_data(indeksi_suuralueittain, overwrite = TRUE)
