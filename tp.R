library(tidyverse)

playerValuations <- read_csv('player_valuations.csv')
players <- read_csv('players.csv')
gameEvents <- read_csv('game_events.csv')
clubs <- read_csv('clubs.csv')

delanteros <- players %>%
  filter(position == 'Attack' & last_season == '2024')

glimpse(delanteros)


ggplot(delanteros, aes(x = sub_position, y = market_value_in_eur)) +
  geom_boxplot(fill = "steelblue", color = "black", outlier.color = "red") +
  scale_y_continuous(labels = \(x) paste0("€", round(x / 1e6, 1), "M")) +
  coord_cartesian(ylim = c(0, 10e6)) +  # <- zoom hasta 10 millones
  labs(
    title = "Valor de mercado de delanteros vigentes",
    x = "Subposición",
    y = "Valor de mercado (€ millones)"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

goles_2024 <- gameEvents %>%
  filter(type == "Goals", year(date) == 2024)

goles_por_jugador_2024 <- goles_2024 %>%
  group_by(player_id) %>%
  summarise(goles_2024 = n(), .groups = "drop")

delanteros_con_goles <- delanteros %>%
  left_join(goles_por_jugador_2024, by = "player_id") %>%
  mutate(goles_2024 = replace_na(goles_2024, 0))  # los que no tienen goles → 0

ggplot(delanteros_con_goles, aes(x = goles_2024, y = market_value_in_eur, color = sub_position)) +
  geom_point(alpha = 0.7, size = 3) +
  geom_smooth(method = "lm", se = FALSE, color = "black") +  # línea de tendencia general
  scale_y_continuous(labels = \(x) paste0("€", round(x / 1e6, 1), "M")) +
  labs(
    title = "Relación entre goles y valor de mercado de delanteros (2024)",
    x = "Goles en 2024",
    y = "Valor de mercado (€ millones)",
    color = "Subposición"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(face = "bold", size = 14)
  )


