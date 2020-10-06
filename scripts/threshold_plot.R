rm(list = ls())
library(tidyverse)
load("data/rdata/threshol_ucayali.RData")
#lbls <- c(names(x))
x <- x %>%
  mutate(month = (10:19) %>% as.character())
names(x)[1:3] <- c("a", "b", "c")

x <- gather(x, key = type, value = value, -month)
#seq(as.Date("2020-03-01"), as.Date("2020-12-31"), by = "1 month")
# lbls <- c(
#   "cluster 6 and 8", "cluster 8", "cluster 6"
# )

lbls <- c(
  "Ucayali basin", "Ucayali basin\nAmazon region", "Ucayali basin\nAndes region"
)


plt <- ggplot(x, aes(month, value, group = type)) +
  labs(
    title = "SEASONAL EVOLUTION OF THRESHOLD",
    y = "threshold of LST"
  ) +
  geom_line(aes(linetype = type, color = type, size = type)) +
  scale_linetype_manual(
    values = c("solid", "solid", "solid"), labels = lbls
  ) +
  scale_color_manual(
    values = c("blue", "black", "gray"),
    labels = lbls
  ) +
  scale_size_manual(values = rep(.8, 5), labels = lbls) +
  # scale_x_date(
  #   limits = c(as.Date("2020-03-01"), as.Date("2020-12-31")),
  #   breaks = seq(as.Date("2020-03-01"), as.Date("2020-12-31"), by = "1 month"),
  #   date_labels = "%b", expand = expansion(mult = c(0, 0))
  # ) +
  scale_x_discrete(
    labels = month.abb[3:12], expand = expansion(mult = c(0, 0))
  ) +
  scale_y_continuous(
    breaks = seq(16, 35, 2),
    limits = c(16, 35),
    expand = expansion(mult = c(0, 0))
  ) +
  theme_bw() +
  theme(
    legend.background = element_rect(fill = "white", color = "black", size = .3),
    legend.margin = margin(.25, 4, 6, 4),
    legend.key.width = unit(.35, "cm"),
    legend.key.height = unit(.35, "cm"),
    legend.position = c(0.25, 0.85),
    legend.title = element_blank(),
    legend.text = element_text(size = 10, family = "Source Sans Pro"),
    plot.title = element_text(size = 12, hjust = 1, family = "Source Sans Pro"),
    axis.text.x = element_text(
      size = 10, colour = "black", family = "Source Sans Pro",
      face = "bold", angle = 45, hjust = 1.2
    ),
    axis.text.y = element_text(
      size = 12, face = "bold", family = "Source Sans Pro", color = "black"
    ),
    axis.title.x = element_blank(),
    axis.title.y = element_text(
      size = 12, face = "bold", family = "Source Sans Pro", color = "black"
    ),
    axis.ticks.x = element_line(color = "black"),
    axis.ticks.y = element_line(color = "black"),
    panel.grid = element_blank(),
    panel.border = element_rect(size = .5, color = "black"),
    plot.margin = margin(1.5, .1, 1, 1, "cm"),
    axis.line.y = element_line(
      size = .5, color = "black"
    )
  )

ggsave(
  plot = plt,
  "exports/threshold_clus_ucayali_hdf_from_2000_to_2020.png",
  width = 10, height = 12, units = "cm", dpi = 1000
)
