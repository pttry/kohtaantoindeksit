plot_alueellinen_keskittyminen <- function(data, .largest_kunnat_maara) {
  data |> filter(largest_kunnat_maara == .largest_kunnat_maara) |>
    dplyr::filter(tiedot %in% c("indeksi_TYOTTOMATLOPUSSA", "indeksi_AVPAIKATLOPUSSA")) |>
    dplyr::group_by(tiedot) |>
    dplyr::mutate(value_loess = statfitools::loess_series(value, time)) |>
    dplyr::mutate(value_year = ifelse(grepl("01-01", time), value_loess, NA)) |>
    ggplot(aes(x = time, color = tiedot)) +
    geom_line(aes(y = value), alpha = 0.3, size = 1) +
    geom_line(aes(y = value_loess), alpha  =1, size = 1) +
    geom_point(aes(y = value_year, shape = tiedot)) +
    labs(x = NULL,
         y = paste("Osuus suurimassa ", as.character(.largest_kunnat_maara), " kunnassa", sep = ""),
         color = NULL) +
    scale_y_continuous(labels = percent_comma) +
    scale_x_date(breaks = as.Date(paste(seq(2006,2022,by=4), "-01-01", sep = "")),
                 date_labels = "%Y") +
    scale_color_manual(name = "", labels = c("Avoimet työpaikat", "Työttömät"),
      values = ggptt_palettes$ptt[1:2]) +
    scale_shape_manual(name = "", values = 15:16, labels = c("Avoimet työpaikat", "Työttömät")) +
    theme_bw() +
    theme(legend.text = element_text(size = 15),
          legend.position = "bottom",
          legend.justification = "left",
          axis.title = element_text(size = 15),
          axis.text = element_text(size = 15))
}

data(data_alueellinen_keskittyminen)

p1 <- output |> plot_alueellinen_keskittyminen(50)
p2 <- output |> plot_alueellinen_keskittyminen(150)

p <- ggpubr::ggarrange(p1, p2, nrow = 1, common.legend = TRUE, legend = "bottom")
ggsave("kuviot/alueellinen_keskittyminen.pdf",plot = p,  width = 11.2, height = 6)
save_plot_AW_raportti("alueellinen_keskittyminen", width = 10, height = 6, plot = p)
save_data_AW_raportti(output, "av_tyopaikat_tyottomat_keskittyminen")
