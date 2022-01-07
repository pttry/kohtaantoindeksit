data("indeksi_alueittain")
library(tidyverse)

p <-indeksi_alueittain %>% filter(tiedot %in% c("mismatch","mismatch_trend"), a == 0.5) %>%
  mutate(region_level = factor(region_level, levels = c("kunta", "seutukunta", "maakunta"))) %>%
  ggplot(aes(x = time, y = value, col = region_level, alpha = tiedot)) +
  geom_line(size = 1) +
  scale_alpha_discrete(range = c(0.3,1)) +
  scale_y_continuous(labels = ggptt::percent_comma,
                     breaks = seq(0.00,0.08, by =0.02),
                     minor_breaks = seq(0.00,0.07, by =0.01)) +
  scale_color_discrete(labels = c("Kunta", "Seutukunta", "Maakunta")) +
  scale_x_date(breaks = as.Date(paste(seq(2006,2020,by=2), "-01-01", sep = "")),
               date_labels = "%Y") +
  labs(x = NULL, y = latex2exp::TeX("$M_t$"),
       color= "Työmarkkinamääritelmä") +
  coord_cartesian(ylim = c(0,0.08)) +
  geom_hline(yintercept = 0, col = "black", linetype = 2)+
  theme(panel.grid.minor = element_line(size = 0.5)) +
  guides(alpha = "none")

ggsave("kuviot/indeksi_alueittain.png", plot = p, width = 8, height = 5)


p_herkkyys <- grid.arrange(p_alue_herkkyys, p_ammatti_herkkyys, nrow = 1)
ggsave("kuviot/herkkyys.png",plot = p_herkkyys,  width = 11.2, height = 6)
