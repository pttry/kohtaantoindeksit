plot_alueellinen_keskittyminen <- function(data, .largest_kunnat_maara) {
  data |> filter(largest_kunnat_maara == .largest_kunnat_maara) |>
    dplyr::filter(tiedot %in% c("indeksi_TYOTTOMATLOPUSSA", "indeksi_AVPAIKATLOPUSSA")) |>
    dplyr::group_by(tiedot) |>
    dplyr::mutate(value_loess = statfitools::loess_series(value, time)) |>
    dplyr::mutate(value_year = ifelse(grepl("01-01", time), value_loess, NA)) |>
    ggplot(aes(x = time, color = tiedot)) +
    geom_line(aes(y = value), alpha = 0.3, size = 1) +
    geom_line(aes(y = value_loess), alpha  =1, size = 1) +
    geom_point(aes(y = value_year, shape = tiedot), size = point_size) +
    labs(x = NULL,
         y = paste("Osuus suurimassa ", as.character(.largest_kunnat_maara), " kunnassa", sep = ""),
         color = NULL) +
    scale_y_continuous(labels = percent_comma) +
    scale_x_date(breaks = as.Date(paste(seq(2006,2022,by=4), "-01-01", sep = "")),
                 date_labels = "%Y") +
    scale_color_grey(name = "", labels = c("Avoimet työpaikat", "Työttömät")) +
    scale_shape_manual(name = "", values = 15:16, labels = c("Avoimet työpaikat", "Työttömät")) +
    theme_bw() +
    theme(legend.text = element_text(size = text_size),
          legend.position = "bottom",
          axis.title = element_text(size = text_size),
          axis.text = element_text(size = text_size))
}

data(data_alueellinen_keskittyminen)

p1 <- data_alueellinen_keskittyminen |> plot_alueellinen_keskittyminen(50)
p2 <- data_alueellinen_keskittyminen |> plot_alueellinen_keskittyminen(150)

p <- ggpubr::ggarrange(p1, p2, nrow = 1, common.legend = TRUE, legend = "bottom")
ggsave("kuviot_mv/alueellinen_keskittyminen.png",plot = p,  width = 11.2, height = 6)
