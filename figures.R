theme_set(theme_bw())
theme_update(panel.grid.minor = element_line(size = 0.2), panel.grid.major = element_line(size = 0.2))
#theme_update(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
theme_update(text = element_text(size = 14, family="CMU Serif"))
theme_update(axis.ticks = element_line(size = 0.2))
theme_update(axis.line = element_blank())

timeseries %>% ggplot(aes(x = datetime, y = demand)) + 
  geom_line(size = 0.3) + geom_line(aes(x = datetime, y = qdemand), color = "red") +
  labs(x = "Година", y = "MW")
ggsave("four_seventeen.png", width = 18, height = 9, units = "cm", dpi = 600)

timeseries %>% ggplot(aes(x = datetime, y = qddemand)) + geom_line(size = 0.3) +
  labs(x = "Година", y = "MW")
ggsave("four_eighteen.png", width = 18, height = 9, units = "cm", dpi = 600)

hhfits %>% ggplot(aes(x = dateyear, y = .innov)) + geom_line(size = 0.3) +
  labs(x = "Година", y = TeX("$\\epsilon_{t}$"))
ggsave("four_nineteen.png", width = 18, height = 9, units = "cm", dpi = 600)

timeseries %>% ggplot(aes(x = dateyear, y = temp1)) + geom_line(size = 0.3) +
  labs(x = "Година", y = "Температура")
ggsave("four_twenty.png", width = 18, height = 9, units = "cm", dpi = 600)

demand %>% pivot_longer(c(demand_fcast, demand), names_to = "var", values_to = "value") %>% 
  ggplot(aes(x = datetime, y = value)) +
  geom_line(size = 0.3) + facet_grid(vars(var), scales = "fixed") +
  labs(x = "Година", y = "MW")
ggsave("four_twentyone.png", width = 18, height = 12, units = "cm", dpi = 600)

df <- tibble(time = fc$datetime, qdemand = q.demand$y)
df %>% ggplot(aes(x = datetime, y = qdemand)) + geom_line(size = 0.3) +
  labs(x = "Година", y = "MW")
ggsave("four_twentytwo.png", width = 18, height = 9, units = "cm", dpi = 600)

