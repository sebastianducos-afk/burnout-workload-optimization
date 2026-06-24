# =============================================================================
# FASE 3 — PREPROCESADO DE DATOS PARA MODELADO
# Predicción de Burnout y Optimización de Carga de Trabajo
# =============================================================================
#
# CORRECCIONES APLICADAS (según auditoría + revisión de scripts previos):
#
#   C1. Cargar organizational_events.csv y derivar 'event_type' dominante por
#       (team_id, week): variable categórica que la referencia incluye y que
#       el original ignoraba. La tabla weekly_workload NO tiene event_type
#       directamente; hay que derivarla del CSV de eventos.
#       Criterio: evento de mayor intensidad en esa semana/equipo.
#       Si no hay evento → "none".
#
#   C2. Añadir 'team_avg_workload' como predictor directo (además de
#       'team_workload_gap'). Existe en weekly_workload desde Datos_sintéticos_3.R
#       (media de task_volume por equipo/semana) pero el original no la incluía
#       en predictor_cols.
#
#   C3. NO se añade 'recent_absence_days': esa columna no existe en ninguna
#       tabla generada por Datos_sintéticos_3.R. 'sick_days_recent' es la
#       variable equivalente y ya estaba en predictor_cols.
#
#   C4. Añadir columna 'split' a los CSV exportados para trazabilidad.
#
#   C5. Renombrar columnas con prefijos trazables num__ / cat__, alineando
#       con la convención de los CSV de referencia del repositorio.
#
#   C6. Guardar prep_pipeline.rds para que Fases 4 y 5 puedan aplicar las
#       mismas transformaciones a nuevos datos sin reajustar parámetros.
#
#   C7. Exportar operational.csv con valores en escala original (sin
#       normalizar) y variables de segmento sin dummificar ('department',
#       'job_level'). Fase 6 lo usa para el LP; Fase 5 para los análisis
#       por subgrupo organizativo.
# =============================================================================

library(dplyr)
library(tidyr)
library(purrr)
library(recipes)

# ── Directorios de salida ─────────────────────────────────────────────────────
dir.create("data fase 3", recursive = TRUE, showWarnings = FALSE)


# =============================================================================
# 1. CARGA DE DATOS
# =============================================================================
teams     <- read.csv("datos sinteticos/teams.csv",                 stringsAsFactors = FALSE)
employees <- read.csv("datos sinteticos/employees.csv",             stringsAsFactors = FALSE)
weekly    <- read.csv("datos sinteticos/weekly_workload.csv",       stringsAsFactors = FALSE)
events    <- read.csv("datos sinteticos/organizational_events.csv", stringsAsFactors = FALSE)

cat(sprintf("weekly_workload: %d filas x %d columnas\n", nrow(weekly), ncol(weekly)))
cat(sprintf("organizational_events: %d filas\n", nrow(events)))


# =============================================================================
# 2. DERIVACION DE event_type DOMINANTE POR (team_id, week)  (C1)
#
#    weekly_workload contiene 'team_event_load' (suma numerica de intensidades)
#    pero NO el tipo de evento. organizational_events.csv recoge el tipo.
#    Se toma el evento de mayor intensidad de cada (team_id, week) como
#    representante categorico. Equipos sin evento reciben event_type = "none".
#
#    Valores posibles (definidos en Datos_sinteticos_3.R):
#      quarter_close, audit, system_migration,
#      product_launch, restructuring, peak_season, none
# =============================================================================
dominant_event <- events %>%
  group_by(team_id, week) %>%
  slice_max(event_intensity, n = 1, with_ties = FALSE) %>%
  select(team_id, week, event_type) %>%
  ungroup()

weekly <- weekly %>%
  left_join(dominant_event, by = c("team_id", "week")) %>%
  mutate(event_type = if_else(is.na(event_type), "none", event_type))

cat("\nDistribucion de event_type (incluye 'none'):\n")
print(table(weekly$event_type))


# =============================================================================
# 3. CONSTRUCCION DE LA TABLA BASE DE MODELADO
# =============================================================================
emp_features <- employees %>%
  select(
    employee_id,
    job_level,
    role_type,
    remote_ratio,
    salary_position_percentile,
    base_engagement_trait,
    base_resilience_score,
    tenure_months,
    age
    # NOTA (C3): 'recent_absence_days' no existe en employees.csv
    # ni en weekly_workload.csv generados por Datos_sinteticos_3.R.
    # 'sick_days_recent' (ya en predictor_cols) es la variable equivalente.
  )

team_features <- teams %>%
  select(
    team_id,
    department,
    manager_quality_score,
    baseline_workload_level,
    team_turnover_rate,
    business_criticality
  )

model_data <- weekly %>%
  left_join(emp_features,  by = "employee_id") %>%
  left_join(team_features, by = "team_id")

stopifnot(nrow(model_data) == nrow(weekly))
cat(sprintf("\nTabla base construida: %d filas x %d columnas\n",
            nrow(model_data), ncol(model_data)))


# =============================================================================
# 4. INGENIERIA DE VARIABLES TEMPORALES (sin fuga de informacion)
# =============================================================================
model_data <- model_data %>%
  arrange(employee_id, week) %>%
  group_by(employee_id) %>%
  mutate(
    # Lags (t-1)
    lag_weekly_hours      = lag(weekly_hours,      1),
    lag_overtime_hours    = lag(overtime_hours,    1),
    lag_deadline_pressure = lag(deadline_pressure, 1),
    lag_engagement_score  = lag(engagement_score,  1),
    
    # Medias moviles (ultimas 4 semanas anteriores)
    rolling_4_weekly_hours   = (lag(weekly_hours,   1) + lag(weekly_hours,   2) +
                                  lag(weekly_hours,   3) + lag(weekly_hours,   4)) / 4,
    rolling_4_overtime_hours = (lag(overtime_hours, 1) + lag(overtime_hours, 2) +
                                  lag(overtime_hours, 3) + lag(overtime_hours, 4)) / 4,
    
    # Diferencial individuo-equipo
    # team_avg_workload ya existe en weekly_workload (media de task_volume por equipo/semana)
    team_workload_gap = weekly_hours - team_avg_workload
  ) %>%
  ungroup()


# =============================================================================
# 5. SELECCION FINAL DE COLUMNAS
# =============================================================================
id_cols    <- c("employee_id", "week", "team_id")
target_col <- "high_burnout_risk"

predictor_cols <- c(
  # Carga y trabajo semanal
  "weekly_hours", "overtime_hours", "deadline_pressure", "meeting_hours",
  "workload_variability", "task_volume", "task_complexity",
  "active_projects", "team_event_load",
  # C1: tipo de evento dominante derivado de organizational_events.csv
  "event_type",
  # C2: predictor directo de carga media del equipo (ya en weekly_workload)
  "team_avg_workload",
  # Bienestar y comportamiento
  # 'sick_days_recent' cubre el concepto de ausencias (no existe 'recent_absence_days')
  "vacation_days_recent", "sick_days_recent", "training_hours_recent",
  "engagement_score", "performance_score",
  # Caracteristicas estables del empleado
  "job_level", "role_type", "remote_ratio",
  "salary_position_percentile", "base_engagement_trait", "base_resilience_score",
  "tenure_months", "age",
  # Caracteristicas del equipo
  "department", "manager_quality_score", "baseline_workload_level",
  "team_turnover_rate", "business_criticality",
  # Features temporales derivadas
  "lag_weekly_hours", "lag_overtime_hours",
  "lag_deadline_pressure", "lag_engagement_score",
  "rolling_4_weekly_hours", "rolling_4_overtime_hours",
  "team_workload_gap"
)

# Verificacion: todas las columnas deben existir en model_data
cols_faltantes <- setdiff(predictor_cols, names(model_data))
if (length(cols_faltantes) > 0) {
  stop(sprintf(
    "Columnas declaradas en predictor_cols que no existen en model_data:\n  %s",
    paste(cols_faltantes, collapse = ", ")
  ))
}

model_data <- model_data %>%
  select(all_of(c(id_cols, target_col, predictor_cols)))

cat(sprintf("Columnas en model_data: %d (%d predictores + 1 target + 3 ids)\n",
            ncol(model_data), length(predictor_cols)))


# =============================================================================
# 6. DIVISION TEMPORAL (train / validation / test)
# =============================================================================

# C4: columna 'split' para trazabilidad (ausente en el original)
model_data <- model_data %>%
  mutate(split = case_when(
    week >= 1  & week <= 36 ~ "train",
    week >= 37 & week <= 44 ~ "validation",
    week >= 45 & week <= 52 ~ "test",
    TRUE                    ~ NA_character_
  ))

train_data <- model_data %>% filter(split == "train")
valid_data <- model_data %>% filter(split == "validation")
test_data  <- model_data %>% filter(split == "test")

train_ids <- train_data %>% select(all_of(id_cols), split)
valid_ids <- valid_data %>% select(all_of(id_cols), split)
test_ids  <- test_data  %>% select(all_of(id_cols), split)

train_x <- train_data %>% select(all_of(predictor_cols))
train_y <- train_data %>% pull(high_burnout_risk)

valid_x <- valid_data %>% select(all_of(predictor_cols))
valid_y <- valid_data %>% pull(high_burnout_risk)

test_x  <- test_data  %>% select(all_of(predictor_cols))
test_y  <- test_data  %>% pull(high_burnout_risk)

cat(sprintf("Split -> train: %d | valid: %d | test: %d\n",
            nrow(train_x), nrow(valid_x), nrow(test_x)))
cat(sprintf("Tasa burnout -> train: %.2f %% | valid: %.2f %% | test: %.2f %%\n",
            100 * mean(train_y), 100 * mean(valid_y), 100 * mean(test_y)))


# =============================================================================
# 7. TABLA OPERATIVA EN ESCALA ORIGINAL  (C7)
#
#    Conserva todas las variables en sus unidades reales (horas, scores, etc.)
#    y los segmentos organizativos sin dummificar:
#      - 'department': necesario en Fase 5 (analisis por subgrupo) y
#        Fase 6 (restricciones de gobernanza del LP)
#      - 'job_level' : necesario en Fase 5 y Fase 6 (compatibilidad de nivel)
#      - 'event_type': conservado en forma categorica original
#
#    Fase 4 NO usa este archivo (trabaja con los CSV normalizados).
# =============================================================================
operational_data <- model_data %>%
  select(all_of(c(id_cols, "split", target_col, predictor_cols)))

write.csv(operational_data, "data fase 3/operational.csv", row.names = FALSE)
cat(sprintf("Tabla operativa exportada: %d filas x %d columnas\n",
            nrow(operational_data), ncol(operational_data)))


# =============================================================================
# 8. PREPROCESADO CON recipes (ajuste SOLO sobre train)
# =============================================================================

# Registrar variables nominales antes del bake para asignar prefijos despues
nominal_vars_original <- train_x %>%
  select(where(is.character)) %>%
  names()

cat(sprintf("\nVariables nominales a dummificar: %s\n",
            paste(nominal_vars_original, collapse = ", ")))

preprocessing_recipe <- recipe(~ ., data = train_x) %>%
  # Eliminar predictores con varianza cero
  step_zv(all_predictors()) %>%
  # Imputar NAs: mediana para numericas, moda para nominales
  # (NAs provienen de lags semana 1 y de missing values en engagement/performance)
  step_impute_median(all_numeric_predictors()) %>%
  step_impute_mode(all_nominal_predictors()) %>%
  # Dummificar variables nominales (one-hot encoding)
  step_dummy(all_nominal_predictors(), one_hot = TRUE) %>%
  # Normalizar numericas (media 0, sd 1) con parametros fijados en train
  step_normalize(all_numeric_predictors())

# Ajustar UNICAMENTE con train_x (sin ver validacion ni test)
prep_pipeline <- prep(preprocessing_recipe, training = train_x)

# Aplicar a los tres conjuntos sin reajustar parametros
train_preproc <- bake(prep_pipeline, new_data = train_x)
valid_preproc <- bake(prep_pipeline, new_data = valid_x)
test_preproc  <- bake(prep_pipeline, new_data = test_x)

cat(sprintf("Features tras preprocesado (sin prefijos): %d\n", ncol(train_preproc)))


# =============================================================================
# 9. RENOMBRADO CON PREFIJOS TRAZABLES  (C5)
#
#    num__  para variables numericas (originales + derivadas temporales)
#    cat__  para dummies generadas a partir de variables nominales originales
#           (event_type, job_level, role_type, department)
#
#    Logica: una columna es 'cat__' si su nombre empieza por el nombre de
#    alguna variable nominal original seguido de "_".
#    El resto son 'num__'.
# =============================================================================
add_prefixes <- function(df, nominal_original_vars) {
  cols     <- names(df)
  new_cols <- sapply(cols, function(col) {
    is_dummy <- any(vapply(
      nominal_original_vars,
      function(v) startsWith(col, paste0(v, "_")),
      logical(1)
    ))
    if (is_dummy) paste0("cat__", col) else paste0("num__", col)
  })
  stats::setNames(df, new_cols)
}

train_preproc <- add_prefixes(train_preproc, nominal_vars_original)
valid_preproc <- add_prefixes(valid_preproc, nominal_vars_original)
test_preproc  <- add_prefixes(test_preproc,  nominal_vars_original)

n_features_final <- ncol(train_preproc)
cat(sprintf("Features tras preprocesado (con prefijos): %d\n", n_features_final))
cat("  Muestra num__ : ",
    paste(head(grep("^num__", names(train_preproc), value = TRUE), 5), collapse = ", "),
    "\n")
cat("  Muestra cat__ : ",
    paste(head(grep("^cat__", names(train_preproc), value = TRUE), 5), collapse = ", "),
    "\n")


# =============================================================================
# 10. RECONSTITUCION DE DATASETS FINALES
#     Orden: ids + split + features preprocesadas + target
# =============================================================================
train_final <- bind_cols(train_ids, train_preproc, tibble(high_burnout_risk = train_y))
valid_final <- bind_cols(valid_ids, valid_preproc, tibble(high_burnout_risk = valid_y))
test_final  <- bind_cols(test_ids,  test_preproc,  tibble(high_burnout_risk = test_y))

# Verificaciones de integridad
stopifnot(
  nrow(train_final) == 21600,
  nrow(valid_final) == 4800,
  nrow(test_final)  == 4800
)
stopifnot(
  !any(is.na(train_final)),
  !any(is.na(valid_final)),
  !any(is.na(test_final))
)
stopifnot(
  !any(sapply(select(train_final, where(is.numeric)),
              function(x) any(is.infinite(x))))
)

cat(sprintf("\nColumnas totales por dataset: %d\n", ncol(train_final)))
cat(sprintf("Tasa burnout final -> train: %.2f %% | valid: %.2f %% | test: %.2f %%\n",
            100 * mean(train_final$high_burnout_risk),
            100 * mean(valid_final$high_burnout_risk),
            100 * mean(test_final$high_burnout_risk)))


# =============================================================================
# 11. EXPORTAR DATASETS PROCESADOS Y PIPELINE
# =============================================================================
write.csv(train_final, "data fase 3/train.csv",      row.names = FALSE)
write.csv(valid_final, "data fase 3/validation.csv", row.names = FALSE)
write.csv(test_final,  "data fase 3/test.csv",       row.names = FALSE)

# C6: guardar el pipeline ajustado para reproducibilidad
# Fases 4/5 cargan con readRDS() y aplican bake() sin reajustar parametros
saveRDS(prep_pipeline, "data fase 3/prep_pipeline.rds")

cat("\n== Resumen de exportacion ==========================================\n")
cat(sprintf("  train.csv       : %d x %d\n", nrow(train_final), ncol(train_final)))
cat(sprintf("  validation.csv  : %d x %d\n", nrow(valid_final), ncol(valid_final)))
cat(sprintf("  test.csv        : %d x %d\n", nrow(test_final),  ncol(test_final)))
cat(sprintf("  operational.csv : %d x %d  (escala original, segmentos reales)\n",
            nrow(operational_data), ncol(operational_data)))
cat("  prep_pipeline.rds guardado\n")
cat("== Fase 3 completada ===============================================\n")