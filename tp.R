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

# ---- Seleccion de tablas y variables ----

players <- players %>%
  select(
    player_id, 
    name, 
    last_season, 
    date_of_birth,
    position, 
    sub_position,
    height_in_cm, 
    market_value_in_eur
  )

competitions <- competitions %>%
  select(competition_id, type, sub_type, name)

appearances <- appearances %>%
  select(-player_club_id, -player_current_club_id)

# ---- End Seleccion de tablas y variables ----

delanteros <- players %>%
  filter(position == 'Attack' & last_season == '2024')

delanteros <- delanteros %>%
  mutate(age = floor(interval(date_of_birth, today()) / years(1)))

appearances <- appearances %>% semi_join(delanteros, join_by(player_id))

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

app <- appearances %>%
  group_by(player_id) %>%
  summarise(minutos_jugados=sum(minutes_played), asistencias_totales=sum(assists), asistencia_por_min=asistencias_totales/minutos_jugados, goles_por_min=goals/minutos_jugados) 

delanteros_con_goles <- delanteros_con_goles %>%
  left_join(app, by="player_id")

# ---- Modelo 1 ----
mod1 <- lm(market_value_in_eur ~ goals + age + asistencias_totales + height_in_cm, data = delanteros_con_goles)
summary(mod1)
# ---- End Modelo 1 ----

# ---- Modelo 2 ----
mod2 <- lm(market_value_in_eur ~ goals + age + asistencias_totales + height_in_cm + sub_position, data = delanteros_con_goles)
summary(mod2)
# ---- End Modelo 2 ----

# ---- Modelo 3 ----
mod3 <- lm(market_value_in_eur ~ goals + factor(age) + asistencias_totales + height_in_cm + sub_position, data = delanteros_con_goles)
summary(mod3)
# ---- End Modelo 3 ----

anova(mod1, mod2, mod3)













