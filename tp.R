library(tidyverse)
library(modelr)
library(lubridate)
library(ggplot2)
library(dplyr)

# ---- Carga de los datasets ----

playerValuations <- read_csv('player_valuations.csv')
players <- read_csv('players.csv')
gameEvents <- read_csv('game_events.csv')
clubs <- read_csv('clubs.csv')
competitions <- read_csv("competitions.csv")
appearances <- read_csv("appearances.csv")

# ---- End Carga de los datasets ----

delanteros <- players |>
  filter(position == "Attack" & last_season == 2024) |>
  mutate(age = floor(interval(date_of_birth, today()) / years(1))) |>
  filter(!is.na(market_value_in_eur) & !is.na(height_in_cm))

appearances <- appearances |>
  semi_join(delanteros, join_by(player_id)) |>
  filter(date >= "2023-08-01")

glimpse(delanteros)

# ---- BOX_PLOT: Valuacion de delanteros por subposicion ----

ggplot(delanteros, aes(x = sub_position, y = market_value_in_eur)) +
  geom_boxplot(fill = "steelblue", color = "black", outlier.color = "red") +
  scale_x_discrete(labels = c(
    "Centre-Forward" = "Delantero Centro",
    "Left Winger"   = "Extremo Izquierdo",
    "Right Winger"  = "Extremo Derecho",
    "Second Striker" = "Segundo Delantero"
  )) +
  scale_y_continuous(labels = \(x) paste0("€", round(x / 1e6, 1), "M")) +
  coord_cartesian(ylim = c(0, 10e6)) +  # zoom hasta 10 millones
  labs(
    title = "Valor de mercado de delanteros vigentes",
    x = "Subposición",
    y = "Valor de mercado (€ millones)"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# ---- End BOX_PLOT: Valuacion de delanteros por subposicion ----


# ---- Scatter PLOT : Goles de delanteros vigentes ----

delanteros_con_goles <- appearances %>%
  group_by(player_id) %>%
  summarise(
    goals = sum(goals)
  ) %>% right_join(delanteros, join_by(player_id)) %>%
  filter(goals != 0) #ver si tiene sentido este filtro

ggplot(delanteros_con_goles, aes(x = goals, y = market_value_in_eur, color = sub_position)) +
  geom_point(alpha = 0.7, size = 3) +
  geom_smooth(method = "lm", se = FALSE, color = "black") +  # línea de tendencia general
  scale_color_discrete(labels = c(
    "Centre-Forward" = "Delantero Centro",
    "Left Winger"    = "Extremo Izquierdo",
    "Right Winger"   = "Extremo Derecho",
    "Second Striker" = "Segundo Delantero"
  )) +
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

# ---- End Scatter PLOT : Goles de delanteros vigentes ----

# ---- Valor de mercado vs edad ----

delanteros %>%
  ggplot(aes(x = factor(age), y = market_value_in_eur)) +
  geom_col(
    fill = "#4292c6",
    width = 0.7
  ) +
  geom_smooth(
    method = "loess",
    se = FALSE,
    color = "red",
    linewidth = 1.2
  ) +
  labs(
    title = "Valor de mercado total de delanteros según edad",
    x = "Edad del jugador (años)",
    y = "Valor de mercado total [EUR]",
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
    plot.subtitle = element_text(size = 11, hjust = 0.5),
    axis.title = element_text(size = 12, face = "bold"),
    axis.text = element_text(size = 10),
    panel.grid.minor = element_blank()
  )

# ---- End Valor de mercado vs edad ----

# ---- Valor de mercado vs mediana edad ----

delanteros_avg <- delanteros %>%
  group_by(age) %>%
  summarise(median_value = median(market_value_in_eur, na.rm = TRUE))

delanteros_avg %>%
  ggplot(aes(x = age, y = median_value)) +
  # Barras que muestran la mediana del valor de mercado por edad
  geom_col(
    fill = "steelblue",
    color = "black",
    width = 0.7
  ) +
  # Curva suavizada para mostrar la tendencia general
  geom_smooth(
    method = "loess",
    se = FALSE,
    color = "red",
    linewidth = 1.2
  ) +
  labs(
    title = "Mediana del valor de mercado de delanteros según edad",
    subtitle = "Valores expresados en euros (EUR) con curva suavizada",
    x = "Edad del jugador (años)",
    y = "Mediana del valor de mercado [EUR]"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
    plot.subtitle = element_text(size = 11, hjust = 0.5),
    axis.title = element_text(size = 12, face = "bold"),
    axis.text = element_text(size = 10),
    panel.grid.minor = element_blank()
  )

# ---- End Valor de mercado vs mediana edad ----

# ----Valor de mercado vs liga ----

delanteros_ligas <- appearances %>%
  left_join(competitions, join_by(competition_id)) %>%
  select(player_id, competition_id, type, name) %>%
  filter(type == "domestic_league") %>%
  rename(league = name) %>%
  left_join(delanteros, join_by(player_id)) %>%
  select(market_value_in_eur, league)

delanteros_ligas %>%
  ggplot(aes(y = league, x = market_value_in_eur)) +
  geom_boxplot(
    varwidth = TRUE,
    fill = "#6baed6",
    color = "black",
    outlier.color = "red",
    outlier.alpha = 0.6
  ) +
  coord_cartesian(xlim = c(0, 5e+7)) +
  labs(
    title = "Distribución del valor de mercado de delanteros por liga",
    x = "Valor de mercado [EUR]",
    y = "Liga",
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
    plot.subtitle = element_text(size = 11, hjust = 0.5),
    axis.title = element_text(size = 12, face = "bold"),
    axis.text = element_text(size = 10),
    plot.caption = element_text(size = 9, color = "gray40"),
    panel.grid.minor = element_blank()
  )

# ---- End Valor de mercado vs liga ----

# ---- Valor de mercado vs altura ----

delanteros_height_avg <- delanteros %>%
  group_by(height_in_cm) %>%
  summarise(median_market_value = median(market_value_in_eur, na.rm = T))

delanteros_height_avg %>%
  ggplot(aes(x = height_in_cm, y = median_market_value)) +
  geom_col(
    fill = "steelblue",
    color = "black",
    width = 0.7
  ) +
  geom_smooth(
    method = "loess",
    se = FALSE,
    color = "red",
    linewidth = 1.2
  ) +
  labs(
    title = "Mediana del valor de mercado de delanteros según altura",
    subtitle = "Valores expresados en euros (EUR) con curva suavizada",
    x = "Altura del jugador [cm]",
    y = "Mediana del valor de mercado [EUR]"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
    plot.subtitle = element_text(size = 11, hjust = 0.5),
    axis.title = element_text(size = 12, face = "bold"),
    axis.text = element_text(size = 10),
    panel.grid.minor = element_blank()
  )

# ---- End Valor de mercado vs altura ----

# ---- Valor de mercado vs altura dividido por subposicion ----

delanteros_height_position_avg <- delanteros %>%
  group_by(sub_position, height_in_cm) %>%
  summarise(median_market_value = median(market_value_in_eur, na.rm = T))

delanteros_height_position_avg %>%
  mutate(sub_position = fct_reorder(sub_position, median_market_value, .fun = median)) %>%
  ggplot(aes(x = height_in_cm, y = median_market_value)) +
  geom_col(
    aes(fill = sub_position),
    color = "black",
    width = 0.7,
    alpha = 0.85
  ) +
  geom_smooth(
    method = "loess",
    se = FALSE,
    color = "red",
    linewidth = 1.1
  ) +
  facet_wrap(~ sub_position, ncol = 2, scales = "free_y") +
  labs(
    title = "Relación entre altura y valor de mercado de atacantes",
    subtitle = "Valores medianos agrupados por subposición específica",
    x = "Altura (cm)",
    y = "Valor de mercado mediano (€)",
    fill = "Subposición"
  ) +
  #scale_y_continuous(labels = label_number_si(prefix = "€")) +
  scale_fill_brewer(palette = "Blues") +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(size = 12, color = "gray30"),
    axis.title = element_text(face = "bold"),
    axis.text = element_text(color = "gray20"),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    legend.position = "bottom",
    strip.text = element_text(face = "bold", size = 11)
  )

# ---- End Valor de mercado vs altura dividido por subposicion ----

# ---- Comienzo del modelado ----

stats_delanteros <- appearances |>
  group_by(player_id) |>
  summarise(
    n = n(),
    goals = sum(goals),
    assists = sum(assists),
    goals_90 = sum(goals)/n(),
    assists_90 = sum(assists)/n(),
    red_cards = sum(red_cards),
    yellow_cards = sum(yellow_cards),
  ) |> 
  inner_join(delanteros, join_by(player_id))

glimpse(stats_delanteros)

# modelo 1
mod1 <- lm(market_value_in_eur ~ poly(goals, 3), data =stats_delanteros)
summary(mod1)

# modelo 2
mod2 <- lm(market_value_in_eur ~ goals * goals_90, data =stats_delanteros)
summary(mod2)

# modelo 3
mod3 <- lm(market_value_in_eur ~ goals * goals_90 + assists, data =stats_delanteros)
summary(mod3)

# modelo 4
mod4 <- lm(market_value_in_eur ~ goals * goals_90 + assists * assists_90, data =stats_delanteros)
summary(mod4)

# modelo 5
mod5 <- lm(market_value_in_eur ~ goals * goals_90 * assists * assists_90, data =stats_delanteros)
summary(mod5)

# modelo 6
mod6 <- lm(market_value_in_eur ~ goals * goals_90 * assists * assists_90 * age, data =stats_delanteros)
summary(mod6)

# modelo 7
mod7 <- lm(market_value_in_eur ~ goals * goals_90 * assists * assists_90 * age * sub_position, data =stats_delanteros)
summary(mod7)

# modelo 8
mod8 <- lm(market_value_in_eur ~ goals * goals_90 * assists * assists_90 * age * sub_position * red_cards * yellow_cards, data =stats_delanteros)
summary(mod8)

# modelo 9
mod9 <- lm(market_value_in_eur ~ goals * current_club_domestic_competition_id - 1, data =stats_delanteros)
summary(mod9)

# modelo 10
mod10 <- lm(market_value_in_eur ~ goals * age * current_club_domestic_competition_id - 1, data =stats_delanteros)
summary(mod10)

# modelo 11
mod11 <- lm(market_value_in_eur ~ goals * assists * age * sub_position * current_club_domestic_competition_id - 1, data =stats_delanteros)
summary(mod11)

# modelo 12
mod12 <- lm(market_value_in_eur ~ goals * assists * age * sub_position + current_club_domestic_competition_id - 1, data =stats_delanteros)
summary(mod13)

# modelo 13
mod13 <- lm(market_value_in_eur ~ goals * assists * age * sub_position * current_club_domestic_competition_id - 1, data =stats_delanteros)
summary(mod12)


anova(mod1, mod13)

# Gráficos de residuos

attackers_stats <- attackers_stats |>
  add_residuals(mod1) |>
  add_predictions(mod1)

attackers_stats |>
  ggplot(aes(x = pred, y = resid)) +
  geom_point()+
  geom_hline(yintercept = 0, color="red", size= 1)

attackers_stats |>
  ggplot(aes(x = goals, y = resid)) +
  geom_point()+
  geom_hline(yintercept = 0, color="red", size= 1)

attackers_stats |>
  ggplot(aes(x = assists, y = resid)) +
  geom_point()+
  geom_hline(yintercept = 0, color="red", size= 1)













