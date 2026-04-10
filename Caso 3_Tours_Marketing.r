# ============================================================
# TALLER BUSINESS ANALYTICS – CASO TOUR MARKETING (OPRY)
# ============================================================

library(readr)
library(dplyr)
library(ggplot2)
library(psych)
library(gt)


# ============================================================
# 1. EXPLORACIÓN INICIAL
# ============================================================

opry <- read_csv("Opry.csv")

glimpse(opry)
summary(opry)
head(opry)

# Resumen estadístico de variables clave
opry %>%
  select(Gasto_Publicidad, Ventas, Ordenes, sp, mei,
         Google_Trends_Opry, Google_Trend_Nashville) %>%
  describe() %>%
  as.data.frame() %>%
  tibble::rownames_to_column(var = "Variable") %>%
  select(Variable, n, mean, sd, min, max) %>%
  mutate(across(where(is.numeric), ~ round(., 2))) %>%
  gt() %>%
  tab_header(title = "Resumen Estadístico") %>%
  cols_label(n = "N", mean = "Media", sd = "Desv. Est.",
             min = "Mínimo", max = "Máximo") %>%
  fmt_number(columns = where(is.numeric), decimals = 2, use_seps = TRUE)

# Revisar valores NA
colSums(is.na(opry))


# Gráfico de dispersión Ventas vs Gasto_Publicidad
ggplot(opry, aes(x = Gasto_Publicidad, y = Ventas)) +
  geom_point(alpha = 0.6, color = "steelblue") +
  geom_smooth(method = "lm", se = TRUE, color = "firebrick") +
  labs(
    title = "Dispersión: Ventas vs Gasto en Publicidad",
    x = "Gasto en Publicidad ($)",
    y = "Ventas ($)"
  ) +
  theme_minimal()

# Gráfico temporal con doble eje
escala <- max(opry$Ventas) / max(opry$Gasto_Publicidad)

opry |>
  mutate(Date = as.Date(Date)) |>
  ggplot(aes(x = Date)) +
  geom_line(aes(y = Ventas, color = "Ventas"), linewidth = 0.8) +
  geom_line(aes(y = Gasto_Publicidad * escala, color = "Gasto Publicidad"),
            linewidth = 0.8) +
  scale_y_continuous(
    name = "Ventas ($)",
    sec.axis = sec_axis(~ . / escala, name = "Gasto en Publicidad ($)")
  ) +
  scale_color_manual(
    values = c("Ventas" = "steelblue", "Gasto Publicidad" = "firebrick")
  ) +
  labs(title = "Evolución temporal: Ventas y Gasto en Publicidad",
       x = "Fecha", color = "") +
  theme_minimal() +
  theme(legend.position = "bottom")

# ============================================================
# 2. REGRESIÓN NAIVE (SIMPLE)
# ============================================================

options(scipen = 999)  # evitar notación científica

modelo <- lm(Ventas ~ Gasto_Publicidad, data = opry)
summary(modelo)

#Tabla Resumen
tidy(modelo) %>%
  mutate(
    across(where(is.numeric), ~ round(., 4)),
    Sig = case_when(
      p.value < 0.001 ~ "***",
      p.value < 0.01  ~ "**",
      p.value < 0.05  ~ "*",
      p.value < 0.10  ~ ".",
      TRUE            ~ ""
    )
  ) %>%
  rename(Variable = term, Coeficiente = estimate,
         Error_Est = std.error, t = statistic, p_valor = p.value) %>%
  bind_rows(
    tibble(Variable = "R²",      Coeficiente = round(summary(modelo)$r.squared, 4)),
    tibble(Variable = "R² Adj.", Coeficiente = round(summary(modelo)$adj.r.squared, 4))
  ) %>%
  gt() %>%
  tab_header(title = "Modelo 1: Ventas ~ Gasto_Publicidad") %>%
  tab_source_note("*** p < 0.001  |  ** p < 0.01  |  * p < 0.05  |  . p < 0.10")

# ============================================================
# 3. REGRESIÓN CON DUMMY DE ESTACIONALIDAD
# ============================================================

# La variable Holliday_seasson ya está en los datos:
# marca con 1 las semanas de bajas ventas de diciembre y enero
# (2023-12-30, 2024-01-06 a 2024-01-27, 2024-12-28, 2025-01-04 a 2025-01-25)

# Verificar:
opry %>%
  filter(Holliday_seasson == 1) %>%
  select(Date, Ventas, Holliday_seasson)

modelo_2 <- lm(Ventas ~ Gasto_Publicidad + Holliday_seasson, data = opry)
options(scipen = 999)
summary(modelo_2)

#Tabla resumen
tidy(modelo_2) %>%
  mutate(
    across(where(is.numeric), ~ round(., 4)),
    Sig = case_when(
      p.value < 0.001 ~ "***",
      p.value < 0.01  ~ "**",
      p.value < 0.05  ~ "*",
      p.value < 0.10  ~ ".",
      TRUE            ~ ""
    )
  ) %>%
  rename(Variable = term, Coeficiente = estimate,
         Error_Est = std.error, t = statistic, p_valor = p.value) %>%
  bind_rows(
    tibble(Variable = "R²",      Coeficiente = round(summary(modelo_2)$r.squared, 4)),
    tibble(Variable = "R² Adj.", Coeficiente = round(summary(modelo_2)$adj.r.squared, 4))
  ) %>%
  gt() %>%
  tab_header(title = "Modelo 2: Ventas ~ Gasto_Publicidad + Holliday_seasson") %>%
  tab_source_note("*** p < 0.001  |  ** p < 0.01  |  * p < 0.05  |  . p < 0.10")

# ============================================================
# 4. TRANSFORMACIÓN LOGARÍTMICA
# ============================================================

opry <- opry %>%
  mutate(Log_Ventas = log(Ventas))

modelo_3 <- lm(Log_Ventas ~ Gasto_Publicidad + Holliday_seasson, data = opry)
options(scipen = 999)
summary(modelo_3)

#Tabla Resumen
tidy(modelo_3) %>%
  mutate(
    across(where(is.numeric), ~ round(., 6)),
    Sig = case_when(
      p.value < 0.001 ~ "***",
      p.value < 0.01  ~ "**",
      p.value < 0.05  ~ "*",
      p.value < 0.10  ~ ".",
      TRUE            ~ ""
    )
  ) %>%
  rename(Variable = term, Coeficiente = estimate,
         Error_Est = std.error, t = statistic, p_valor = p.value) %>%
  bind_rows(
    tibble(Variable = "R²",      Coeficiente = round(summary(modelo_3)$r.squared, 4)),
    tibble(Variable = "R² Adj.", Coeficiente = round(summary(modelo_3)$adj.r.squared, 4))
  ) %>%
  gt() %>%
  tab_header(title = "Modelo 3: Log(Ventas) ~ Gasto_Publicidad + Holliday_seasson") %>%
  tab_source_note("*** p < 0.001  |  ** p < 0.01  |  * p < 0.05  |  . p < 0.10")





