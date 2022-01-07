data("indeksi_ammateittain")
library(tidyverse)

p <- mismatch_ammatti %>% filter(tiedot %in% c("mismatch", "mismatch_trend"), a == 0.5) %>%
  ggplot(aes(x = time, y = value, col = as.factor(ammattiryhma_codetaso), alpha = tiedot)) +
  geom_line(size = 1) +
  scale_alpha_discrete(range = c(0.3,1)) +
  scale_y_continuous(labels = percent_comma,
                     breaks = seq(0,0.25,by = 0.05),
                     minor_breaks = seq(0,0.25, by = 0.01)) +
  scale_color_discrete(labels = paste(1:4, "numerotaso", sep = "-")) +
  scale_x_date(breaks = as.Date(paste(seq(2006,2020,by=2), "-01-01", sep = "")),
               date_labels = "%Y") +
  labs(x = NULL, y = latex2exp::TeX("$M_t$"),
       color= "Työmarkkinamääritelmä") +
  geom_hline(yintercept = 0, color = "black", linetype = 2) +
  coord_cartesian(ylim = c(0, 0.25)) +
  theme(panel.grid.minor = element_line(size=0.5)) +
  guides(alpha = "none")


ggsave("kuviot/indeksi_ammateittain.png", plot = p, width = 8, height = 5)


