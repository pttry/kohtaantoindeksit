data("indeksi_alueittain")

indeksi_alueittain <- mutate(indeksi_alueittain, benchmark = ifelse(a == 0.5, "benchmark", "not_benchmark"))

point_data <- indeksi_alueittain |>
  filter(a == 0.5, tiedot == "mismatch_trend") |>
  mutate(value_year = ifelse(grepl("01-01", time), value, NA))

p_alue_herkkyys <- indeksi_alueittain |> filter(tiedot == "mismatch_trend") |>
  mutate(region_level = factor(region_level, levels = c("kunta", "seutukunta", "maakunta"))) |>
  ggplot(aes(x = time, y = value, color = region_level, alpha = benchmark, group = interaction(a,region_level))) +
  geom_line(size = 0.3) +
  geom_point(aes(y = value_year, shape = region_level), data = point_data, size = 3) +
  scale_alpha_discrete(range = c(1,0.3)) +
  scale_y_continuous(labels = ggptt::percent_comma,
                     breaks = seq(0.00,0.08, by =0.02),
                     minor_breaks = seq(0.00,0.07, by =0.01)) +
  scale_shape_manual(name = "Työmarkkinamääritelmä", values = 15:17, labels = c("Kunta", "Seutukunta", "Maakunta")) +
  scale_color_grey(name = "Työmarkkinamääritelmä", labels = c("Kunta", "Seutukunta", "Maakunta")) +
  scale_x_date(breaks = as.Date(paste(seq(2006,2022,by=2), "-01-01", sep = "")),
               date_labels = "%Y") +
  labs(x = NULL, y = latex2exp::TeX("$M_t$"),
       color= "Työmarkkinamääritelmä",
       alpha = NULL) +
  coord_cartesian(ylim = c(0,0.08)) +
  geom_hline(yintercept = 0, col = "black", linetype = 2)+
  theme_bw() +
  guides(alpha = "none") +
  guides(colour = guide_legend(title.position = "top", nrow = 2),
         shape = guide_legend(title.position = "top", nrow = 2)) +
  theme(legend.text = element_text(size = 15),
        legend.title = element_text(size = 15),
        axis.title = element_text(size = 15),
        panel.grid.minor = element_line(size=0.5),
        legend.position = "bottom",
        legend.justification = "left")

data("indeksi_ammateittain")

indeksi_ammateittain <- mutate(indeksi_ammateittain, benchmark = ifelse(a == 0.5, "benchmark", "not_benchmark"))

point_data <- indeksi_ammateittain |>
  filter(a == 0.5, tiedot == "mismatch_trend") |>
  mutate(value_year = ifelse(grepl("01-01", time), value, NA)) |>
  mutate(ammattiryhma_codetaso = as.factor(ammattiryhma_codetaso))

p_ammatti_herkkyys <- indeksi_ammateittain |> filter(tiedot == "mismatch_trend") |>
  mutate(ammattiryhma_codetaso = as.factor(ammattiryhma_codetaso)) |>
  ggplot(aes(x = time, y = value, color = ammattiryhma_codetaso, alpha = benchmark, group = interaction(a,ammattiryhma_codetaso))) +
  geom_line(size = 0.3) +
  geom_point(aes(y = value_year, shape = ammattiryhma_codetaso), data = point_data, size = 3) +
  scale_alpha_discrete(range = c(1,0.3)) +
  scale_y_continuous(labels = percent_comma,
                     breaks = seq(0,0.25,by = 0.05),
                     minor_breaks = seq(0,0.25, by = 0.01)) +
  scale_shape_manual(name = "Työmarkkinamääritelmä", values = 15:18, labels = paste(1:4, "numerotaso", sep = "-")) +
  scale_color_grey(name = "Työmarkkinamääritelmä", labels = paste(1:4, "numerotaso", sep = "-")) +
  scale_x_date(breaks = as.Date(paste(seq(2006,2022,by=2), "-01-01", sep = "")),
               date_labels = "%Y") +
  labs(x = NULL, y = latex2exp::TeX("$M_t$"),
       color= "Työmarkkinamääritelmä",
       alpha = NULL) +
  coord_cartesian(ylim = c(0, 0.25)) +
  geom_hline(yintercept = 0, col = "black", linetype = 2)+
  theme(panel.grid.minor = element_line(size=0.5)) +
  guides(alpha = "none") +
  guides(colour = guide_legend(title.position = "top", nrow = 2),
         shape = guide_legend(title.position = "top", nrow = 2)) +
  theme_bw() +
  theme(legend.text = element_text(size = 15),
        legend.title = element_text(size = 15),
        axis.title = element_text(size = 15),
        legend.position = "bottom",
        legend.justification = "left")

p_herkkyys <- gridExtra::grid.arrange(p_alue_herkkyys, p_ammatti_herkkyys, nrow = 1)
ggsave("kuviot_mv/herkkyys_a.png",plot = p_herkkyys,  width = 11.2, height = 6)
