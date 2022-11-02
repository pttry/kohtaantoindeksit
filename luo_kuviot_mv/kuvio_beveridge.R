data(tyonv_12r5)

tyonv_12r5 <- tyonv_12r5 |>
              dplyr::filter(alue == "SSS") |>
              dplyr::filter(tiedot %in% c("TYOTTOMATLOPUSSA", "AVPAIKATLOPUSSA", "TYOVOIMATK")) |>
              tidyr::spread(tiedot, value)

plot_beveridge_curve <- function(df,
                                 series_type = "trend",
                                 number_type = "relative",
                                 colors = c("#00627D", "#00713D")) {

  if(number_type == "relative") {
    df$x <- df$TYOTTOMATLOPUSSA / df$TYOVOIMATK
    df$y <- df$AVPAIKATLOPUSSA / (df$AVPAIKATLOPUSSA + df$TYOVOIMATK - df$TYOTTOMATLOPUSSA)
  } else {
    df$x <- df$TYOTTOMATLOPUSSA
    df$y <- df$AVPAIKATLOPUSSA
  }
  if(series_type %in% c("trend", "sa")) {
    df$y <- do.call(paste0(series_type, "_series"), list(x = df$y, time = df$time))
    df$x <- do.call(paste0(series_type, "_series"), list(x = df$x, time = df$time))
  }

  df$vuosi_label = sapply(df$time, function(t) {ifelse(grepl("01-01", t), format(t, "%Y"), "")})

  ggplot(df, aes(x = x, y = y, label = vuosi_label)) +
    geom_point(size = 2, aes(color = lubridate::year(time)), size = point_size) +
    geom_path(aes(color = lubridate::year(time)), size = 1) +
    scale_color_gradient(high = "black", low = "grey80", breaks = seq(2006,2022, by = 4)) +
    ggrepel::geom_text_repel(color = "black", max.overlaps = Inf,min.segment.length = 0, box.padding = 0.5) +
    labs(y = ifelse(number_type == "relative", "Vakanssiaste", "Avoimet työpaikat"),
         x = ifelse(number_type == "relative", "Työttömyysaste", "Työttömät"), color = NULL)
}

plot_beveridge_curve(tyonv_12r5) +
  scale_y_continuous(labels = ggptt::percent_comma) +
  scale_x_continuous(labels = ggptt::percent_comma) +
  theme_bw()

ggsave("kuviot_mv/beveridge_curve.png",plot = last_plot(),  width = 8, height = 5)
