data("indeksi_ammateittain_maakunnittain")

p <- indeksi_ammateittain_maakunnittain |>
     dplyr::filter(tiedot %in% c("mismatch", "mismatch_trend"), a == 0.5) |>
  ggplot(aes(x = time, y = value, col = as.factor(code_level), alpha = tiedot)) +
  geom_line(size = 1) +
  theme(legend.position = "bottom",
        legend.justification = "left") +
  scale_alpha_discrete(range = c(0.3,1)) +
  scale_y_continuous(labels = ggptt::percent_comma,
                     breaks = seq(0,0.40,by = 0.05),
                     minor_breaks = seq(0,0.40, by = 0.01)) +
  scale_color_discrete(labels = paste(1:4, "numerotaso-maakunta", sep = "-")) +
  scale_x_date(breaks = as.Date(paste(seq(2006,2020,by=2), "-01-01", sep = "")),
               date_labels = "%Y") +
  labs(x = NULL, y = latex2exp::TeX("$M_t$"),
       color= NULL)+
  geom_hline(yintercept = 0, color = "black", linetype = 2) +
  coord_cartesian(ylim = c(0, 0.38)) +
  theme(panel.grid.minor = element_line(size=0.5)) +
  guides(alpha = "none")

ggsave("kuviot/indeksi_ammateittain_maakunnittain.pdf", plot = p, width = 8, height = 5)
