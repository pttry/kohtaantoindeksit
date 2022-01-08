data <- pttdatahaku::ptt_read_data("tyonv_12r5", only_codes = TRUE) |>
        dplyr::filter(alue == "SSS") |>
  dplyr::filter(tiedot %in% c("TYOTTOMATLOPUSSA", "AVPAIKATLOPUSSA", "TYOVOIMATK")) |>
  tidyr::spread(tiedot, value)

ggptt::set_ptt()

plot_beveridge_curve(data) +
  scale_y_continuous(labels = percent_comma) +
  scale_x_continuous(labels = percent_comma)


ggsave("kuviot/beveridge_curve.pdf",plot = last_plot(),  width = 8, height = 5)
