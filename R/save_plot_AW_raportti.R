save_plot_AW_raportti <- function(x, ...) {
  path <- "C:/Users/juhoa/Pellervon Taloustutkimus PTT ry/198 AW Osaajapula ja kohtaanto - Raportti/kuviot/"
  ggsave(paste0(path, x, ".png"), ...)
}

save_data_AW_raportti <- function(x, x_name = deparse(substitute(x))) {
  xlsx::write.xlsx(x, paste0("C:/Users/juhoa/Pellervon Taloustutkimus PTT ry/198 AW Osaajapula ja kohtaanto - Raportti/kuviodata/", x_name, ".xlsx"))
  #saveRDS(x, paste0("C:/Users/juhoa/Pellervon Taloustutkimus PTT ry/198 AW Osaajapula ja kohtaanto - Raportti/kuviodata/", x_name, ".rds"))

}
