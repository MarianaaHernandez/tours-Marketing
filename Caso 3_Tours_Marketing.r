# ============================================================
# TALLER BUSINESS ANALYTICS – CASO TOUR MARKETING (OPRY)
# ============================================================
install.packages("gt")
install.packages("broom")
install.packages("flextable")
install.packages("officer")
library(readr)
library(dplyr)
library(ggplot2)
library(psych)
library(gt)
library(broom)


# ============================================================
# 1. EXPLORACIÓN INICIAL
# ============================================================
setwd('/Users/maru/Desktop/Javeriana/4/Analitica de los negocios/tours-Marketing')
opry <- read_csv("Opry copy.csv")

glimpse(opry)
summary(opry)
head(opry)

# Resumen estadístico de variables clave
describe(opry %>% select(Ventas, Gasto_Publicidad, Ordenes, sp, mei, Google_Trends_Opry, Google_Trend_Nashville))

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
# 4. TRANSFORMACIÓN LOGARÍTMICA  (+  Google_Trends_Opry)
# ============================================================

opry <- opry |>
  mutate(Log_Ventas = log(Ventas))

# Se agrega Google_Trends_Opry como nueva variable continua al modelo
# logarítmico. Captura el interés de búsqueda en Google, un proxy de la
# demanda anticipada independiente del gasto y de la estacionalidad baja.
modelo_3 <- lm(
  Log_Ventas ~ Gasto_Publicidad + Holliday_seasson + Google_Trends_Opry,
  data = opry
)
options(scipen = 999)
summary(modelo_3)

# Tabla Resumen
tidy(modelo_3) |>
  mutate(
    across(where(is.numeric), ~ round(., 6)),
    Sig = case_when(
      p.value < 0.001 ~ "***",
      p.value < 0.01  ~ "**",
      p.value < 0.05  ~ "*",
      p.value < 0.10  ~ ".",
      TRUE            ~ ""
    )
  ) |>
  rename(Variable  = term, Coeficiente = estimate,
         Error_Est = std.error, t = statistic, p_valor = p.value) |>
  bind_rows(
    tibble(Variable = "R²",
           Coeficiente = round(summary(modelo_3)$r.squared, 4)),
    tibble(Variable = "R² Adj.",
           Coeficiente = round(summary(modelo_3)$adj.r.squared, 4))
  ) |>
  gt() |>
  tab_header(
    title    = paste(
      "Modelo 3: Log(Ventas) ~",
      "Gasto_Publicidad + Holliday_seasson + Google_Trends_Opry"
    ),
    subtitle = "Nueva variable continua: Google Trends Opry"
  ) |>
  tab_source_note(
    "*** p < 0.001  |  ** p < 0.01  |  * p < 0.05  |  . p < 0.10"
  )

# ============================================================
# 5. Modelo propio  (+  dummy temporada_alta)
# ============================================================

library(broom)

# Nueva dummy: temporada_alta
# Marca con 1 las semanas de junio, julio y agosto (alta temporada turística
# en Nashville). Se espera coeficiente positivo y significativo, lo que
# evidenciará un buen ajuste al capturar el efecto de verano.
opry <- opry |>
  mutate(
    temporada_alta = if_else(
      lubridate::month(as.Date(Date)) %in% c(6, 7, 8), 1L, 0L
    )
  )

# Verificar activación de las dummies
opry |> filter(thanksgiving == 1)               |> select(Date, Ventas)
opry |> filter(week_before_christmas == 1)      |> select(Date, Ventas)
opry |> filter(two_weeks_before_christmas == 1) |> select(Date, Ventas)
opry |> filter(temporada_alta == 1)             |> select(Date, Ventas)

# Modelo 4
modelo_4 <- lm(
  Log_Ventas ~ Gasto_Publicidad + Holliday_seasson +
    Flights_to_Nashville + Google_Trends_Opry +
    thanksgiving + week_before_christmas +
    two_weeks_before_christmas + temporada_alta,
  data = opry
)

options(scipen = 999)
summary(modelo_4)

# Tabla resumen modelo 4
tidy(modelo_4) |>
  mutate(
    across(where(is.numeric), ~ round(., 6)),
    Sig = case_when(
      p.value < 0.001 ~ "***",
      p.value < 0.01  ~ "**",
      p.value < 0.05  ~ "*",
      p.value < 0.10  ~ ".",
      TRUE            ~ ""
    )
  ) |>
  rename(Variable    = term,
         Coeficiente = estimate,
         Error_Est   = std.error,
         t           = statistic,
         p_valor     = p.value) |>
  bind_rows(
    tibble(Variable = "R²",
           Coeficiente = round(summary(modelo_4)$r.squared, 4)),
    tibble(Variable = "R² Adj.",
           Coeficiente = round(summary(modelo_4)$adj.r.squared, 4))
  ) |>
  gt() |>
  tab_header(
    title    = "Modelo 4 (Propio)",
    subtitle = paste(
      "Log(Ventas) ~ Gasto + Holliday + Flights + Google Trends",
      "+ Dummies Navidad/Thanksgiving + temporada_alta"
    )
  ) |>
  tab_source_note(
    "*** p < 0.001  |  ** p < 0.01  |  * p < 0.05  |  . p < 0.10"
  )

# ============================================================
# 6. Exportación de resultados a Word
# ============================================================

library(flextable)
library(officer)

tabla_coef <- tidy(modelo_4) %>%
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
  rename(
    Variable       = term,
    Coeficiente    = estimate,
    `Error Est.`   = std.error,
    `t`            = statistic,
    `p-valor`      = p.value,
    `Sig.`         = Sig
  ) %>%
  bind_rows(
    tibble(Variable = "R²",      Coeficiente = round(summary(modelo_4)$r.squared,     4)),
    tibble(Variable = "R² Adj.", Coeficiente = round(summary(modelo_4)$adj.r.squared, 4))
  )

# Construir tabla con flextable
ft <- flextable(tabla_coef) %>%
  set_caption(
    caption = as_paragraph(
      as_chunk("Modelo 4: Log(Ventas) — Resultados de Regresión",
               props = fp_text_default(bold = TRUE))
    )
  ) %>%
  add_footer_lines("*** p < 0.001  |  ** p < 0.01  |  * p < 0.05  |  . p < 0.10") %>%
  theme_vanilla() %>%
  bold(part = "header") %>%
  bg(bg = "#DEEAF1", part = "header") %>%
  align(align = "center", part = "all") %>%
  align(j = 1, align = "left",  part = "all") %>%
  autofit()

# Crear documento Word y exportar
doc <- read_docx() %>%
  body_add_par("Resultados del Modelo de Regresión – Opry Tours",
               style = "heading 1") %>%
  body_add_par("") %>%
  body_add_par(
    paste(
      "El Modelo 4 regresa el logaritmo natural de las ventas sobre el gasto en publicidad,",
      "dos dummies de estacionalidad (temporada baja y Navidad), vuelos hacia Nashville,",
      "Google Trends del Opry y dummies de Thanksgiving y la semana previa a Navidad."
    ),
    style = "Normal"
  ) %>%
  body_add_par("") %>%
  body_add_flextable(ft)

print(doc, target = "Resultados_Modelo4.docx")
message("Exportado: Resultados_Modelo4.docx")

# ============================================================
# 7. Reflexión final – diagnóstico del modelo
# ============================================================