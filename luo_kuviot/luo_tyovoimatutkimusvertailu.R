
# Update data

#pttdatahaku::ptt_db_update("aw_db", tables = "atp_11n1")
#pttdatahaku::ptt_db_update("aw_db", tables = "tyti_11c9")
#pttdatahaku::ptt_db_update("aw_db", tables = "tyonv_12r5")

# suuralueet

# Tyovoimatutkimuksen tiedot
data_avoimet_tyopaikat <- pttdatahaku::ptt_read_data("atp_11n1", only_codes = TRUE) |>
  tidyr::spread(tiedot, value) |>
  dplyr::rename(Avoimet_tyopaikat = atp_lkm) |>
  dplyr::filter(suuralue != "SSS")
data_tyottomat <- pttdatahaku::ptt_read_data("tyti_137i",only_codes = TRUE) |>
  dplyr::filter(tiedot == "Tyottomat") |>
  tidyr::spread(tiedot, value) |>
  dplyr::rename(suuralue = suuralue_2012) |>
  dplyr::filter(suuralue != "SSS") |>
  dplyr::mutate(Tyottomat = 1000*Tyottomat) |>
  dplyr::filter(sukupuoli == "SSS")


data_kokomaa_avoimet_tyopaikat <- data_avoimet_tyopaikat |>
  dplyr::group_by(time) |>
  dplyr::summarize(kokomaa_avoimet_tyopaikat = sum(Avoimet_tyopaikat))
data_kokomaa_tyottomat <- data_tyottomat |>
  dplyr::group_by(time) |>
  dplyr::summarize(kokomaa_tyottomat = sum(Tyottomat))

data_tyottomat <- dplyr::left_join(data_tyottomat, data_kokomaa_tyottomat, by = "time")
data_avoimet_tyopaikat <- dplyr::left_join(data_avoimet_tyopaikat, data_kokomaa_avoimet_tyopaikat, by = "time")

data <- dplyr::left_join(data_tyottomat, data_avoimet_tyopaikat, by= c("suuralue", "time")) |>
          dplyr::filter(!is.na(Avoimet_tyopaikat))

as <- seq(0.25, 0.75, by = 0.05)
mismatch_tyovoimatutkimus <- data.frame()

for(a in as){

  mismatch_temp <- data |>
    group_by(time) |>
    summarize(mismatch = 1 -sum((Avoimet_tyopaikat/kokomaa_avoimet_tyopaikat)^a * (Tyottomat/kokomaa_tyottomat)^(1-a)))  |>
    ungroup() |>
    gather(tiedot, value, -time) |>
    ungroup()
  mismatch_temp$a <- a
  mismatch_tyovoimatutkimus <- rbind(mismatch_tyovoimatutkimus, mismatch_temp)
  print(a); Sys.sleep(0.01)
}

p <- mismatch_tyovoimatutkimus |> filter(a == 0.5) |>
  ggplot(aes(x = time, y = value, group = 1)) +
  geom_line(size = 1) +
  theme(legend.position = "bottom",
        legend.justification = "left") +
  scale_y_continuous(labels = ggptt::percent_comma,
                     breaks = seq(0.00,0.03, by =0.02),
                     minor_breaks = seq(0.00,0.03, by =0.01)) +
  scale_x_date(breaks = as.Date(paste(seq(2014,2020,by=2), "-01-01", sep = "")),
               date_labels = "%Y") +
  labs(x = NULL, y = latex2exp::TeX("$M_t$"),
       title = "Työvoimatutkimuksen tiedot työttömistä") +
  coord_cartesian(ylim = c(0,0.03)) +
  geom_hline(yintercept = 0, col = "black", linetype = 2)+
  theme(panel.grid.minor = element_line(size=0.5)) +
  guides(alpha = "none")

mismatch_tyovoimatutkimus <- mutate(mismatch_tyovoimatutkimus, benchmark = ifelse(a == 0.5, "benchmark", "not_benchmark"))

p_alue_herkkyys <- mismatch_tyovoimatutkimus |> filter(tiedot == "mismatch") |>
  ggplot(aes(x = time, y = value, alpha = benchmark, group = a)) +
  geom_line(size = 0.3) +
  scale_alpha_discrete(range = c(1,0.3)) +
  theme(legend.position = "bottom",
        legend.justification = "left") +
  scale_y_continuous(labels = percent_comma,
                     breaks = seq(0.00,0.03, by =0.01)) +
  scale_color_discrete(labels = c("Kunta", "Seutukunta", "Maakunta")) +
  scale_x_date(breaks = as.Date(paste(seq(2014,2020,by=2), "-01-01", sep = "")),
               date_labels = "%Y") +
  labs(x = NULL, y = latex2exp::TeX("$M_t$"),
       color= NULL,
       alpha = NULL) +
  coord_cartesian(ylim = c(0,0.03)) +
  geom_hline(yintercept = 0, col = "black", linetype = 2)+
  theme(panel.grid.minor = element_line(size=0.5)) + guides(alpha = "none")

data_seutukunnat <- mutate(data_seutukunnat, maakunta_code = recode_region(seutukunta_code, to = "maakunta_code"))

# Tyonvalitystilaston tiedot:
data_tyonv_12r5 <- pttdatahaku::ptt_read_data("tyonv_12r5", only_codes = TRUE) |>
  dplyr::filter(grepl("MK", alue)) |>
  dplyr::rename(maakunta = alue) |>
  dplyr::filter(tiedot %in% c("TYOTTOMATLOPUSSA", "AVPAIKATLOPUSSA")) |>
  tidyr::spread(tiedot, value) |>
  dplyr::rename(Tyottomat = TYOTTOMATLOPUSSA, Avoimet_tyopaikat = AVPAIKATLOPUSSA)


key <- get_key("maakunta_1_20120101%23suuralue_1_20120101")
key <- key |>
  mutate(maakunta = set_region_codes(key$source_code, region_level = "maakunta"),
         suuralue = paste0("SA", target_code)) |> select(maakunta, suuralue)
data_tyonv_12r5 <- left_join(data_tyonv_12r5, key, by = "maakunta")


data_suuralueet <- data_tyonv_12r5 |> dplyr::group_by(time, suuralue) |>
  dplyr::summarize(Tyottomat = sum(Tyottomat),
            Avoimet_tyopaikat = sum(Avoimet_tyopaikat)) |>
  dplyr::filter(suuralue != "SA5") |> ungroup()

data_kokomaa <- data_suuralueet |>
  dplyr::group_by(time) |>
  dplyr::summarize(kokomaa_tyottomat = sum(Tyottomat),
            kokomaa_avoimet_tyopaikat = sum(Avoimet_tyopaikat))

data <- dplyr::left_join(data_suuralueet, data_kokomaa, by = "time")

as <- seq(0.25, 0.75, by = 0.05)
mismatch_tyonvalitystilasto <- data.frame()

for(a in as){

  mismatch_temp <- data |>
    group_by(time) |>
    summarize(mismatch = 1 -sum((Avoimet_tyopaikat/kokomaa_avoimet_tyopaikat)^a * (Tyottomat/kokomaa_tyottomat)^(1-a)))  |>
    ungroup() |>
    gather(tiedot, value, -time) |>
    ungroup()
  mismatch_temp$a <- a
  mismatch_tyonvalitystilasto <- rbind(mismatch_tyonvalitystilasto, mismatch_temp)
  print(a); Sys.sleep(0.01)
}

mismatch_tyonvalitystilasto <- mutate(mismatch_tyonvalitystilasto, benchmark = ifelse(a == 0.5, "benchmark", "not_benchmark"))


p_alue_herkkyys <- mismatch_tyonvalitystilasto |> filter(tiedot == "mismatch") |>
  ggplot(aes(x = time, y = value, alpha = benchmark, group = a)) +
  geom_line(size = 0.3) +
  scale_alpha_discrete(range = c(1,0.3)) +
  theme(legend.position = "bottom",
        legend.justification = "left") +
  scale_y_continuous(labels = ggptt::percent_comma,
                     breaks = seq(0.00,0.03, by =0.01)) +
  scale_color_discrete(labels = c("Kunta", "Seutukunta", "Maakunta")) +
  scale_x_date(breaks = as.Date(paste(seq(2014,2020,by=2), "-01-01", sep = "")),
               date_labels = "%Y") +
  labs(x = NULL, y = latex2exp::TeX("$M_t$"),
       color= NULL,
       alpha = NULL) +
  coord_cartesian(ylim = c(0,0.03)) +
  geom_hline(yintercept = 0, col = "black", linetype = 2)+
  theme(panel.grid.minor = element_line(size=0.5)) + guides(alpha = "none")

mismatch_tyonvalitystilasto$source <- "tyonvalitystilasto"
mismatch_tyovoimatutkimus$source <- "tyovoimatutkimus"
mismatch <- rbind(mismatch_tyonvalitystilasto, mismatch_tyovoimatutkimus)


p <- mismatch |> filter(a == 0.5) |>
  ggplot(aes(x = time, y = value, color = source)) +
  geom_line(size = 1, alpha = 0.3) +
  geom_smooth(se = FALSE) +
  theme(legend.position = "bottom",
        legend.justification = "left") +
  scale_y_continuous(labels = ggptt::percent_comma,
                     breaks = seq(0.00,0.03, by =0.01)) +
  scale_x_date(breaks = as.Date(paste(seq(2006,2020,by=2), "-01-01", sep = "")),
               date_labels = "%Y") +
  scale_color_manual(labels = c("Työnvälitystilasto", "Työvoimatutkimus"),
                     values = ggptt_palettes$ptt[1:2]) +
  labs(x = NULL, y = latex2exp::TeX("$M_t$"),
       color = NULL) +
  coord_cartesian(ylim = c(0,0.03)) +
  geom_hline(yintercept = 0, col = "black", linetype = 2)+
  theme(panel.grid.minor = element_line(size=0.5)) +
  guides(alpha = "none")

ggsave(filename = "kuviot/tyovoimatutkimus_mk.png", width = 8, height = 5)
