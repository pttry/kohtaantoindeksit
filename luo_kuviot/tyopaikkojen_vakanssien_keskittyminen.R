

# Hae data
data <- pttdatahaku::ptt_read_data("tyonv_12r5", only_codes = TRUE) %>%
  dplyr::filter(tiedot %in% c("TYOTTOMATLOPUSSA", "AVPAIKATLOPUSSA", "TYOVOIMATK")) %>%
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



df_largest_kunnat <- data_kunnat %>% filter(time == "2021-11-01") %>%
  arrange(desc(TYOVOIMATK))
largest_kunnat_maara <- 150

  largest_kunnat <- df_largest_kunnat$kunta_code[1:largest_kunnat_maara]

  times <- as.character(unique(data_kunnat$time))
  df <- data.frame()
  data_kunnat <- dplyr::mutate(data_kunnat, time = as.character(time))

  for(aika in times) {
    data_temp <- dplyr::filter(data_kunnat, time == aika)
    kokomaa_TYOTTOMATLOPUSSA <- sum(data_temp$TYOTTOMATLOPUSSA, na.rm = TRUE)
    kokomaa_AVPAIKATLOPUSSA <- sum(data_temp$AVPAIKATLOPUSSA, na.rm = TRUE)
    data_largest <- filter(data_temp, kunta_code %in% largest_kunnat)
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

  df <- df%>% tidyr::gather("tiedot", "value", -time) %>%
    dplyr::mutate(time = as.Date(time))

p2 <- df %>% dplyr::filter(tiedot %in% c("indeksi_TYOTTOMATLOPUSSA",
                                  "indeksi_AVPAIKATLOPUSSA")) %>%
  dplyr::mutate(tiedot = plyr::revalue(tiedot, c("indeksi_AVPAIKATLOPUSSA" = "Avoimet työpaikat",
                                          "indeksi_TYOTTOMATLOPUSSA" = "Työttömät"))) %>%
  ggplot(aes(x = time, y = value, color = tiedot)) +
  geom_line( alpha = 0.3) +
  geom_smooth(se = FALSE) +
  labs(x = NULL,
       y = paste("Osuus suurimassa ", as.character(largest_kunnat_maara), " kunnassa", sep = ""),
       color = NULL) +
  scale_y_continuous(labels = percent_comma) +
  scale_x_date(breaks = as.Date(paste(seq(2006,2022,by=2), "-01-01", sep = "")),
               date_labels = "%Y") +
  scale_color_manual(
                     values = ggptt_palettes$ptt[1:2])

p <- gridExtra::grid.arrange(p1, p2, nrow = 1)
ggsave("kuviot/alueellinen_keskittyminen.pdf",plot = p,  width = 11.2, height = 6)


df %>% filter(tiedot %in% c("kokomaa_AVPAIKATLOPUSSA", "largest_AVPAIKATLOPUSSA")) %>%
       ggplot(aes(x = time, y = value, col = tiedot)) +
       geom_line() +
       geom_smooth(se = FALSE) +
       labs(y = "Avoimet työpaikat",
            x = NULL,
            color = NULL) +
  scale_x_date(breaks = as.Date(paste(seq(2006,2022,by=2), "-01-01", sep = "")),
               date_labels = "%Y") +
       scale_color_manual(labels = c("Avoimet työpaikat koko maassa", "Avoimet työpaikat 50 suurimassa kunnassa"),
                          values = ggptt_palettes$ptt[1:2])


df %>% filter(tiedot %in% c("smallest_avoimet_tyopaikat", "largest_avoimet_tyopaikat")) %>%
  ggplot(aes(x = time, y = value, col = tiedot)) +
  geom_line() +
  geom_smooth(se = FALSE) +
  labs(y = "Avoimet työpaikat",
       x = NULL,
       color = NULL) +
  scale_x_date(breaks = as.Date(paste(seq(2006,2020,by=2), "-01-01", sep = "")),
               date_labels = "%Y") +
  scale_color_manual(labels = c("50 suurimassa kunnassa", "Muissa kunnissa"),
                     values = ggptt_palettes$ptt[1:2])
