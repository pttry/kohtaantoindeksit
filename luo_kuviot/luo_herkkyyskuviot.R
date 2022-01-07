data("indeksi_alueittain")
library(tidyverse)

indeksi_alueittain <- mutate(indeksi_alueittain, benchmark = ifelse(a == 0.5, "benchmark", "not_benchmark"))

p_alue_herkkyys <- indeksi_alueittain %>% filter(tiedot == "mismatch_trend") %>%
  ggplot(aes(x = time, y = value, color = region_level, alpha = benchmark, group = interaction(a,region_level))) +
  geom_line(size = 0.3) +
  scale_alpha_discrete(range = c(1,0.3)) +
  scale_y_continuous(labels = ggptt::percent_comma,
                     breaks = seq(0.00,0.08, by =0.02),
                     minor_breaks = seq(0.00,0.07, by =0.01)) +
  scale_color_discrete(labels = c("Kunta", "Seutukunta", "Maakunta")) +
  scale_x_date(breaks = as.Date(paste(seq(2006,2020,by=2), "-01-01", sep = "")),
               date_labels = "%Y") +
  labs(x = NULL, y = latex2exp::TeX("$M_t$"),
       color= NULL,
       alpha = NULL) +
  coord_cartesian(ylim = c(0,0.08)) +
  geom_hline(yintercept = 0, col = "black", linetype = 2)+
  theme(panel.grid.minor = element_line(size=0.5)) + guides(alpha = "none")

ggsave("kuviot/indeksi_alueittain_herkkyys.png", plot = p_alue_herkkyys, width = 8, height = 5)

data("indeksi_ammateittain")

indeksi_ammateittain <- mutate(indeksi_ammateittain, benchmark = ifelse(a == 0.5, "benchmark", "not_benchmark"))

p_ammatti_herkkyys <- indeksi_ammateittain %>% filter(tiedot == "mismatch_trend") %>%
  ggplot(aes(x = time, y = value, color = as.factor(ammattiryhma_codetaso), alpha = benchmark, group = interaction(a,as.factor(ammattiryhma_codetaso)))) +
  geom_line(size = 0.3) +
  scale_alpha_discrete(range = c(1,0.3)) +
  scale_y_continuous(labels = percent_comma,
                     breaks = seq(0,0.25,by = 0.05),
                     minor_breaks = seq(0,0.25, by = 0.01)) +
  scale_color_discrete(labels = paste(1:4, "numerotaso", sep = "-")) +
  scale_x_date(breaks = as.Date(paste(seq(2006,2020,by=2), "-01-01", sep = "")),
               date_labels = "%Y") +
  labs(x = NULL, y = latex2exp::TeX("$M_t$"),
       color= NULL,
       alpha = NULL) +
  coord_cartesian(ylim = c(0, 0.25)) +
  geom_hline(yintercept = 0, col = "black", linetype = 2)+
  theme(panel.grid.minor = element_line(size=0.5)) +
  guides(alpha = "none")#,
# color = guide_legend(nrow=2,byrow=TRUE))

ggsave("kuviot/indeksi_ammateittain_herkkyys.png", plot = p_ammatti_herkkyys, width = 8, height = 5)

p_herkkyys <- gridExtra::grid.arrange(p_alue_herkkyys, p_ammatti_herkkyys, nrow = 1)
ggsave("kuviot/herkkyys.png",plot = p_herkkyys,  width = 11.2, height = 6)
