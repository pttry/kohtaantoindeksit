data("indeksi_alueittain")

p <-indeksi_alueittain |>
  spread(tiedot, value) |>
  filter(a == 0.5) |>
  mutate(region_level = factor(region_level, levels = c("kunta", "seutukunta", "maakunta"))) |>
  mutate(value_year = ifelse(grepl("01-01", time), mismatch_trend, NA)) |>
  ggplot(aes(x = time, col = region_level)) +
  geom_line(aes(y = mismatch), alpha = 0.3, size = 1) +
  geom_line(aes(y = mismatch_trend), alpha = 1, size = 1) +
  geom_point(aes(y = value_year, shape = region_level, col = region_level)) +
  scale_y_continuous(labels = ggptt::percent_comma,
                     breaks = seq(0.00,0.08, by =0.02),
                     minor_breaks = seq(0.00,0.07, by =0.01)) +
  scale_color_discrete(name = "Työmarkkinamääritelmä",  labels = c("Kunta", "Seutukunta", "Maakunta")) +
  scale_shape_manual(name = "Työmarkkinamääritelmä", values = 15:17, labels = c("Kunta", "Seutukunta", "Maakunta")) +
  scale_x_date(breaks = as.Date(paste(seq(2006,2022,by=2), "-01-01", sep = "")),
               date_labels = "%Y") +
  labs(x = NULL, y = latex2exp::TeX("$M_t$")) +
  coord_cartesian(ylim = c(0,0.08)) +
  geom_hline(yintercept = 0, col = "black", linetype = 2) +
  theme_bw() +
  theme(panel.grid.minor = element_line(size = 0.5),
        legend.position = "bottom",
        legend.justification = "left") +
  guides(colour = guide_legend(title.position = "top"),
         shape = guide_legend(title.position = "top"))

ggsave("kuviot/indeksi_alueittain.pdf", plot = p, width = 8, height = 5)
