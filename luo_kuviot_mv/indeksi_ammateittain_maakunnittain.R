data("indeksi_ammateittain_maakunnittain")

p <- indeksi_ammateittain_maakunnittain |>
  spread(tiedot, value) |>
  filter(a == 0.5) |>
  mutate(code_level = as.factor(code_level)) |>
  mutate(value_year = ifelse(grepl("01-01", time), mismatch_trend, NA)) |>
  ggplot(aes(x = time, col = code_level)) +
  geom_line(aes(y = mismatch), alpha = 0.3, size = 1) +
  geom_line(aes(y = mismatch_trend), alpha = 1, size = 1) +
  geom_point(aes(y = value_year, shape = code_level, col = code_level), size = point_size) +
  scale_y_continuous(labels = ggptt::percent_comma,
                     breaks = seq(0,0.40,by = 0.05),
                     minor_breaks = seq(0,0.40, by = 0.01)) +
  scale_color_grey(name = "Työmarkkinamääritelmä",labels = paste(1:4, "numerotaso-maakunta", sep = "-")) +
  scale_shape_manual(name = "Työmarkkinamääritelmä", values = 15:18, labels = paste(1:4, "numerotaso-maakunta", sep = "-")) +
  scale_x_date(breaks = as.Date(paste(seq(2006,2022,by=2), "-01-01", sep = "")),
               date_labels = "%Y") +
  labs(x = NULL, y = latex2exp::TeX("$M_t$")) +
  coord_cartesian(ylim = c(0, 0.38)) +
  geom_hline(yintercept = 0, col = "black", linetype = 2)+
  theme_bw() +
  theme(panel.grid.minor = element_line(size = 0.5),
        legend.position = "bottom",
        legend.justification = "left",
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 12),
        axis.title = element_text(size = 12)) +
  guides(colour = guide_legend(title.position = "top", nrow = 2),
         shape = guide_legend(title.position = "top", nrow = 2))


ggsave("kuviot_mv/indeksi_ammateittain_maakunnittain.png", plot = p, width = 8, height = 4)

