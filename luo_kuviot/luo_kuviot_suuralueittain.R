# i.e. vertaillaan työvoimatutkimusta ja työnvälitystilastoa

data("indeksi_suuralueittain")


p <- indeksi_suuralueittain |>
  spread(tiedot, value) |>
  filter(a == 0.5) |>
  mutate(value_year = ifelse(grepl("01-01", time), mismatch_loess, NA)) |>
  ggplot(aes(x = time, col = source)) +
  geom_line(aes(y = mismatch), alpha = 0.3, size = 1) +
  geom_line(aes(y = mismatch_loess), alpha = 1, size = 1) +
  geom_point(aes(y = value_year, shape = source, col = source)) +
  scale_y_continuous(labels = ggptt::percent_comma,
                     breaks = seq(0.00,0.03, by =0.01)) +
  scale_color_manual(name = "Datalähde", values = ggptt_palettes$ptt[1:2], labels = c("Työnvälitystilasto", "Työvoimatutkimus")) +
  scale_shape_manual(name = "Datalähde", values = 15:16, labels = c("Työnvälitystilasto", "Työvoimatutkimus")) +
  scale_x_date(breaks = as.Date(paste(seq(2006,2022,by=2), "-01-01", sep = "")),
               date_labels = "%Y") +
  labs(x = NULL, y = latex2exp::TeX("$M_t$")) +
  coord_cartesian(ylim = c(0,0.03)) +
  geom_hline(yintercept = 0, col = "black", linetype = 2)+
  theme(panel.grid.minor = element_line(size = 0.5))

ggsave(filename = "kuviot/indeksi_suuralueittain.pdf", width = 8, height = 5)






# Herkkyys

mismatch_tyovoimatutkimus <- dplyr::filter(indeksi_suuralueittain, source = "tyovoimatutkimus")
mismatch_tyovoimatutkimus <- dplyr::mutate(mismatch_tyovoimatutkimus, benchmark = ifelse(a == 0.5, "benchmark", "not_benchmark"))

p_alue_herkkyys <- mismatch_tyovoimatutkimus |> dplyr::filter(tiedot == "mismatch") |>
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


mismatch_tyonvalitystilasto <- dplyr::filter(indeksi_suuralueittain, source = "ttyonvalitystilasto")
mismatch_tyonvalitystilasto <- dplyr::mutate(mismatch_tyonvalitystilasto, benchmark = ifelse(a == 0.5, "benchmark", "not_benchmark"))


p_alue_herkkyys <- mismatch_tyonvalitystilasto |> dplyr::filter(tiedot == "mismatch") |>
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
