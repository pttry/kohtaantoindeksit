plot_beveridge_curve <- function(df = data_kokomaa_1001,
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
    geom_point(size = 1, color = colors[1]) +
    geom_path(size = 1, color = colors[2]) +
    geom_text(color = "black") +
    labs(y = ifelse(number_type == "relative", "Vakanssiaste", "Avoimet työpaikat"),
         x = ifelse(number_type == "relative", "Työttömyysaste", "Työttömät"))
}
