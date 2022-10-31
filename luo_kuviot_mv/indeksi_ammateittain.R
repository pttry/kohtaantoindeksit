data("indeksi_ammateittain")

p <- indeksi_ammateittain |>
  spread(tiedot, value) |>
  filter(a == 0.5) |>
  mutate(ammattiryhma_codetaso = as.factor(ammattiryhma_codetaso)) |>
  mutate(value_year = ifelse(grepl("01-01", time), mismatch_trend, NA)) |>
  ggplot(aes(x = time, col = ammattiryhma_codetaso)) +
  geom_line(aes(y = mismatch), alpha = 0.3, size = 1) +
  geom_line(aes(y = mismatch_trend), alpha = 1, size = 1) +
  geom_point(aes(y = value_year, shape = ammattiryhma_codetaso, col = ammattiryhma_codetaso), size = point_size) +
  scale_y_continuous(labels = percent_comma,
                     breaks = seq(0,0.25,by = 0.05),
                     minor_breaks = seq(0,0.25, by = 0.01)) +
  scale_color_grey(name = "Työmarkkinamääritelmä",labels = paste(1:4, "numerotaso", sep = "-")) +
  scale_shape_manual(name = "Työmarkkinamääritelmä", values = 15:18, labels = paste(1:4, "numerotaso", sep = "-")) +
  scale_x_date(breaks = as.Date(paste(seq(2006,2022,by=2), "-01-01", sep = "")),
               date_labels = "%Y") +
  labs(x = NULL, y = latex2exp::TeX("$M_t$")) +
  coord_cartesian(ylim = c(0, 0.25)) +
  geom_hline(yintercept = 0, col = "black", linetype = 2) +
  theme_bw() +
  theme(panel.grid.minor = element_line(size = 0.5),
        legend.position = "bottom",
        legend.justification = "left",
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 12),
        axis.title = element_text(size = 12)) +
  guides(colour = guide_legend(title.position = "top"),
         shape = guide_legend(title.position = "top"))

ggsave("kuviot_mv/indeksi_ammateittain.pdf", plot = p, width = 8, height = 4)
