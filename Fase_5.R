# =============================================================================
# FASE 5 — INTERPRETABILIDAD Y EXPLICACIÓN DEL MODELO
# =============================================================================
#
# CORRECCIONES APLICADAS (acumuladas sobre los dos errores anteriores):
#
#   C1. Cargar el modelo final real desde el artefacto de Fase 4, no reentrenar.
#
#   C2. Detectar el tipo de modelo desde el artefacto — no asumir Regresión
#       Logística sin verificarlo.
#
#   C3a. (Error 1 — sesión anterior)
#        operational.csv tiene columnas originales ('weekly_hours', etc.) pero
#        feature_cols tiene nombres con prefijos ('num__weekly_hours', etc.).
#        Fix: aplicar bake() + add_prefixes() antes de predecir.
#
#   C3b. (Error 2 — esta sesión)
#        names(pipeline$template) devuelve columnas POST-dummificación
#        ('event_type_audit', 'event_type_none', ...), no las originales.
#        Esto ocurre porque $template en un objeto prep() refleja el
#        output del último step, no la entrada.
#        Fix: obtener columnas originales desde pipeline$var_info
#        (que sí conserva los nombres de entrada) y los nombres de variables
#        nominales desde el step_dummy ya entrenado (step$columns).
#
#   C3c. (Causa raíz de 'Variables nominales:' vacío)
#        filter(type %in% c("nominal","character")) devuelve 0 filas porque
#        recipes almacena las variables carácter como type = "nominal" solo
#        si son factor; las variables carácter puras aparecen con
#        type = "character" según la versión, pero el campo puede variar.
#        Fix: extraer las variables nominales directamente del step_dummy
#        entrenado (step$columns), que es la fuente canónica.
#
#   C4. El resumen ejecutivo solo lista archivos realmente generados.
# =============================================================================

library(dplyr)
library(tidyr)
library(ggplot2)
library(recipes)

set.seed(42)

dir.create("fase 5",         showWarnings = FALSE)
dir.create("fase 5/reports", showWarnings = FALSE)
dir.create("fase 5/figures", showWarnings = FALSE)


# =============================================================================
# 1. CARGAR ARTEFACTO DEL MODELO FINAL (Fase 4)
# =============================================================================
artifact_path <- "fase 4/models/final_model.rds"
if (!file.exists(artifact_path))
  stop(sprintf(
    "[Fase 5] Artefacto no encontrado: %s\nEjecuta Fase 4 antes de continuar.",
    artifact_path
  ))

artifact     <- readRDS(artifact_path)
final_model  <- artifact$model_object
final_name   <- artifact$model_name
feature_cols <- artifact$feature_cols   # nombres CON prefijos: num__*, cat__*
threshold    <- artifact$threshold

cat(sprintf("Modelo cargado desde Fase 4 : %s\n", final_name))
cat(sprintf("  Entrenado con  : %s\n",  artifact$trained_on))
cat(sprintf("  AUC validacion : %.4f\n", artifact$metrics_valid$auc))
cat(sprintf("  AUC test       : %.4f\n", artifact$metrics_test$auc))
cat(sprintf("  Features       : %d\n",   length(feature_cols)))


# =============================================================================
# 2. CARGAR PIPELINE DE FASE 3 Y EXTRAER METADATOS
#
#    Dos objetos necesarios:
#
#    a) orig_pred_cols — columnas de ENTRADA al pipeline (nombres originales,
#       sin prefijos). Fuente: pipeline$var_info filtrando role == "predictor".
#       NO usar names(pipeline$template): refleja columnas POST-dummies.
#
#    b) nominal_vars_original — variables nominales que step_dummy expandió.
#       Método robusto: cruzar feature_cols (fuente de verdad del modelo,
#       con prefijos cat__/num__) con orig_pred_cols.
#       Una variable v en orig_pred_cols es nominal si existe al menos una
#       columna en feature_cols con prefijo "cat__v_".
#       Este enfoque NO depende de ningún campo interno de recipes
#       ($columns, $type, $object) cuya presencia varía entre versiones,
#       lo que provocaba nominal_vars_original vacío en las dos iteraciones
#       anteriores pese a los fallbacks.
# =============================================================================
pipeline_path <- "data fase 3/prep_pipeline.rds"
if (!file.exists(pipeline_path))
  stop(sprintf(
    "[Fase 5] Pipeline no encontrado: %s\nEjecuta Fase 3 corregida antes de continuar.",
    pipeline_path
  ))

prep_pipeline <- readRDS(pipeline_path)

# a) Columnas originales de entrada al pipeline
orig_pred_cols <- prep_pipeline$var_info %>%
  filter(role == "predictor") %>%
  pull(variable)

# b) Variables nominales: derivadas cruzando feature_cols y orig_pred_cols
#    Para cada columna cat__X en feature_cols, X empieza por "varname_nivel".
#    Buscamos qué nombre en orig_pred_cols es prefijo de X (i.e. X empieza
#    por paste0(varname, "_")).  El separador siempre es "_" en recipes por
#    defecto; si el pipeline usó otro, ajustar dummy_sep abajo.
dummy_sep <- "_"

nominal_vars_original <- unique(unlist(lapply(
  feature_cols[startsWith(feature_cols, "cat__")],
  function(col) {
    # Quitar el prefijo cat__ para obtener "varname_nivel"
    stripped <- sub("^cat__", "", col)
    # Buscar qué variable original es prefijo de "varname_nivel"
    orig_pred_cols[vapply(
      orig_pred_cols,
      function(v) startsWith(stripped, paste0(v, dummy_sep)),
      logical(1)
    )]
  }
)))

if (length(nominal_vars_original) == 0)
  warning(paste(
    "[Fase 5] nominal_vars_original sigue vacío.",
    "Revisa que feature_cols contiene columnas con prefijo 'cat__'",
    "y que orig_pred_cols coincide con los nombres originales del pipeline."
  ))

cat(sprintf("\nPipeline cargado\n"))
cat(sprintf("  Predictores originales : %d\n", length(orig_pred_cols)))
cat(sprintf("  Variables nominales    : %s\n",
            paste(nominal_vars_original, collapse = ", ")))
cat(sprintf("  Separador dummies      : '%s'\n", dummy_sep))


# =============================================================================
# 3. FUNCIÓN AUXILIAR: add_prefixes
#    Replica el renombrado num__/cat__ que hace Fase 3 después del bake().
#    Una columna es cat__ si su nombre empieza por el nombre de alguna
#    variable nominal original seguido del separador de dummies.
# =============================================================================
add_prefixes <- function(df, nom_vars, sep = "_") {
  cols     <- names(df)
  new_cols <- sapply(cols, function(col) {
    is_dummy <- length(nom_vars) > 0 &&
      any(vapply(nom_vars,
                 function(v) startsWith(col, paste0(v, sep)),
                 logical(1)))
    if (is_dummy) paste0("cat__", col) else paste0("num__", col)
  })
  stats::setNames(df, new_cols)
}


# =============================================================================
# 4. FUNCIÓN AUXILIAR: preprocess_orig  (C3b — fix de la causa raíz)
#    Transforma un dataframe con columnas originales (sin prefijos) al espacio
#    del modelo (columnas con prefijos num__/cat__).
#
#    Pasos:
#      1. Seleccionar orig_pred_cols — columnas de ENTRADA al pipeline.
#         Estas son las columnas que bake() espera recibir.
#         (No names(pipeline$template), que son las de SALIDA.)
#      2. bake() aplica todas las transformaciones del pipeline.
#      3. add_prefixes() añade num__/cat__ igual que en Fase 3.
#      4. select(feat_cols) garantiza el mismo orden que el modelo vio.
# =============================================================================
preprocess_orig <- function(df_orig, pipeline, orig_cols,
                            nom_vars, sep, feat_cols) {
  # Paso 1: columnas de entrada al pipeline (nombres originales)
  cols_disponibles <- intersect(orig_cols, names(df_orig))
  cols_faltantes   <- setdiff(orig_cols, names(df_orig))
  if (length(cols_faltantes) > 0) {
    warning(sprintf(
      "[preprocess_orig] %d columnas del pipeline no están en df_orig:\n  %s",
      length(cols_faltantes),
      paste(head(cols_faltantes, 8), collapse = ", ")
    ))
  }
  df_sel <- df_orig %>% select(all_of(cols_disponibles))
  
  # Paso 2: aplicar pipeline sin reajustar parámetros
  df_baked <- bake(pipeline, new_data = df_sel)
  
  # Paso 3: añadir prefijos num__/cat__
  df_prefixed <- add_prefixes(df_baked, nom_vars, sep)
  
  # Paso 4: seleccionar exactamente las columnas del modelo
  missing_feat <- setdiff(feat_cols, names(df_prefixed))
  if (length(missing_feat) > 0)
    stop(sprintf(
      "[preprocess_orig] Columnas del modelo ausentes tras el preprocesado:\n  %s",
      paste(head(missing_feat, 8), collapse = ", ")
    ))
  df_prefixed %>% select(all_of(feat_cols))
}


# =============================================================================
# 5. CARGA DE DATOS PREPROCESADOS DE FASE 3
#    Los CSV de fase 3 ya tienen prefijos → select directo con feature_cols.
# =============================================================================
train_raw <- read.csv("data fase 3/train.csv",      stringsAsFactors = FALSE)
valid_raw <- read.csv("data fase 3/validation.csv", stringsAsFactors = FALSE)
test_raw  <- read.csv("data fase 3/test.csv",       stringsAsFactors = FALSE)

train_x <- train_raw %>% select(all_of(feature_cols))
train_y <- train_raw %>% pull(high_burnout_risk)
test_x  <- test_raw  %>% select(all_of(feature_cols))
test_y  <- test_raw  %>% pull(high_burnout_risk)

all_raw <- bind_rows(train_raw, valid_raw, test_raw)
all_x   <- all_raw %>% select(all_of(feature_cols))

cat(sprintf("\nDatos cargados: %d obs. | %d features\n",
            nrow(all_raw), length(feature_cols)))


# =============================================================================
# 6. FUNCIÓN DE PREDICCIÓN UNIFICADA
# =============================================================================
if (final_name == "Gradient Boosting (XGBoost)") {
  if (!requireNamespace("xgboost", quietly = TRUE))
    stop("El modelo final es XGBoost pero 'xgboost' no está instalado.")
  suppressPackageStartupMessages(library(xgboost))
}

predict_prob <- function(model_obj, model_nm, new_x) {
  if (model_nm == "Random Forest") {
    predict(model_obj, data = new_x)$predictions[, "1"]
  } else if (model_nm == "Gradient Boosting (XGBoost)") {
    dm <- xgboost::xgb.DMatrix(data = as.matrix(new_x))
    predict(model_obj, newdata = dm)
  } else {
    predict(model_obj, newdata = new_x, type = "response")
  }
}

test_prob <- predict_prob(final_model, final_name, test_x)
all_prob  <- predict_prob(final_model, final_name, all_x)

cat(sprintf("Predicciones generadas. Riesgo medio en test: %.2f %%\n",
            100 * mean(test_prob)))


# =============================================================================
# 7. INTERPRETABILIDAD GLOBAL — Importancia de variables
# =============================================================================
get_global_importance <- function(model_obj, model_nm, feat_cols) {
  if (model_nm == "Random Forest") {
    imp <- model_obj$variable.importance
    if (is.null(imp)) stop("rf_model$variable.importance es NULL.")
    data.frame(variable   = names(imp),
               imp_value  = as.numeric(imp),
               imp_metric = "Impurity (Gini reduction)",
               direccion  = NA_character_,
               odds_ratio = NA_real_,
               stringsAsFactors = FALSE) %>%
      arrange(-imp_value)
    
  } else if (model_nm == "Gradient Boosting (XGBoost)") {
    imp <- xgboost::xgb.importance(model = model_obj)
    data.frame(variable   = imp$Feature,
               imp_value  = imp$Gain,
               imp_metric = "Gain (XGBoost)",
               direccion  = NA_character_,
               odds_ratio = NA_real_,
               stringsAsFactors = FALSE)
    
  } else {
    coefs <- coef(model_obj)
    coefs <- coefs[names(coefs) != "(Intercept)"]
    data.frame(variable   = names(coefs),
               imp_value  = abs(as.numeric(coefs)),
               imp_metric = "|coeficiente| (log-odds)",
               coef_val   = as.numeric(coefs),
               odds_ratio = exp(as.numeric(coefs)),
               direccion  = ifelse(coefs > 0, "Aumenta riesgo", "Reduce riesgo"),
               stringsAsFactors = FALSE) %>%
      arrange(-imp_value)
  }
}

importance_df <- get_global_importance(final_model, final_name, feature_cols)
top20         <- importance_df %>% head(20)

cat("\n── Top 10 variables por importancia ──────────────────────────────\n")
print(top20 %>% select(variable, imp_value, imp_metric) %>% head(10),
      row.names = FALSE)

write.csv(importance_df,
          "fase 5/reports/importancia_global.csv", row.names = FALSE)
cat("✓ Tabla de importancia global exportada\n")


# ── Figura 1: Importancia global (top 20) ─────────────────────────────────────
has_direction <- "direccion" %in% names(top20) && !all(is.na(top20$direccion))

if (has_direction) {
  p_global <- ggplot(top20,
                     aes(x = reorder(variable, imp_value),
                         y = coef_val, fill = direccion)) +
    geom_col(alpha = 0.85) +
    geom_hline(yintercept = 0, linewidth = 0.4, color = "gray40") +
    coord_flip() +
    scale_fill_manual(values = c("Aumenta riesgo" = "tomato",
                                 "Reduce riesgo"  = "steelblue")) +
    labs(title    = sprintf("Importancia global — %s (top 20)", final_name),
         subtitle = "Coeficiente en escala log-odds | ordenado por |coef|",
         x = NULL, y = "Coeficiente (log-odds)", fill = NULL) +
    theme_minimal(base_size = 11) +
    theme(legend.position = "bottom")
} else {
  p_global <- ggplot(top20,
                     aes(x = reorder(variable, imp_value), y = imp_value)) +
    geom_col(fill = "steelblue", alpha = 0.85) +
    coord_flip() +
    labs(title    = sprintf("Importancia global — %s (top 20)", final_name),
         subtitle = unique(top20$imp_metric),
         x = NULL, y = "Importancia") +
    theme_minimal(base_size = 11)
}

ggsave("fase 5/figures/fig1_importancia_global.png", plot = p_global,
       width = 10, height = 7, dpi = 150, bg = "white")
cat("✓ Figura 1 (importancia global) guardada\n")


# =============================================================================
# 8. INTERPRETABILIDAD LOCAL — Top casos de alto riesgo en TEST
# =============================================================================
N_LOCAL <- 20

test_full <- test_raw %>%
  mutate(prob_burnout = test_prob) %>%
  arrange(desc(prob_burnout))

top_cases <- test_full %>% head(N_LOCAL)

cat(sprintf("\n── Top %d casos de mayor riesgo predicho (TEST) ──────────────\n",
            N_LOCAL))
cat(sprintf("  Prob. maxima         : %.4f\n", max(top_cases$prob_burnout)))
cat(sprintf("  Prob. minima         : %.4f\n", min(top_cases$prob_burnout)))
cat(sprintf("  Verdaderos positivos : %d / %d\n",
            sum(top_cases$high_burnout_risk == 1), N_LOCAL))

numeric_features <- feature_cols[sapply(test_x, is.numeric)]

if (final_name == "Regresión logística") {
  shared_coefs <- intersect(numeric_features, names(coef(final_model)))
  contrib_mat  <- sweep(
    as.matrix(top_cases[, shared_coefs]),
    MARGIN = 2,
    STATS  = coef(final_model)[shared_coefs],
    FUN    = "*"
  )
  contrib_df <- as.data.frame(contrib_mat) %>%
    mutate(employee_id  = top_cases$employee_id,
           week         = top_cases$week,
           prob_burnout = top_cases$prob_burnout,
           real         = top_cases$high_burnout_risk)
} else {
  contrib_df <- as.data.frame(as.matrix(top_cases[, numeric_features])) %>%
    mutate(employee_id  = top_cases$employee_id,
           week         = top_cases$week,
           prob_burnout = top_cases$prob_burnout,
           real         = top_cases$high_burnout_risk)
  shared_coefs <- numeric_features
}

write.csv(contrib_df,
          "fase 5/reports/contribuciones_locales_top_riesgo.csv",
          row.names = FALSE)
cat("✓ Tabla de contribuciones locales exportada\n")


# ── Figura 2: Heatmap de contribuciones ───────────────────────────────────────
top12_features <- importance_df %>%
  filter(variable %in% shared_coefs) %>%
  head(12) %>%
  pull(variable)

contrib_long <- contrib_df %>%
  mutate(caso = paste0("E", employee_id, "_W", week)) %>%
  select(caso, prob_burnout, all_of(top12_features)) %>%
  pivot_longer(-c(caso, prob_burnout),
               names_to  = "feature",
               values_to = "contribucion") %>%
  mutate(caso = reorder(caso, prob_burnout))

fill_label <- if (final_name == "Regresión logística") "Contrib.\n(log-odds)" else
  "Valor\n(norm.)"

p_local <- ggplot(contrib_long,
                  aes(x = feature, y = caso, fill = contribucion)) +
  geom_tile(color = "white", linewidth = 0.3) +
  scale_fill_gradient2(low = "steelblue", mid = "white", high = "tomato",
                       midpoint = 0) +
  labs(
    title    = sprintf("Top %d casos de mayor riesgo (TEST)", N_LOCAL),
    subtitle = if (final_name == "Regresión logística")
      "Valor = coef_j x x_ij  |  Rojo: empuja hacia riesgo  |  Azul: protege"
    else
      "Valor normalizado  |  Rojo: alto  |  Azul: bajo",
    x = NULL, y = "Empleado — Semana", fill = fill_label
  ) +
  theme_minimal(base_size = 10) +
  theme(axis.text.x = element_text(angle = 40, hjust = 1, size = 9),
        axis.text.y = element_text(size = 8))

ggsave("fase 5/figures/fig2_explicaciones_locales.png", plot = p_local,
       width = 12, height = 8, dpi = 150, bg = "white")
cat("✓ Figura 2 (explicaciones locales) guardada\n")


# =============================================================================
# 9. ANÁLISIS POR SEGMENTOS ORGANIZATIVOS
#    operational.csv tiene columnas originales sin prefijos.
#    preprocess_orig() las transforma correctamente antes de predecir.
#    'department' y 'job_level' se usan solo como etiquetas de subgrupo,
#    no como input al modelo.
# =============================================================================
operational_path <- "data fase 3/operational.csv"
files_generated  <- character(0)

if (!file.exists(operational_path)) {
  message("[Fase 5] operational.csv no encontrado. Se omite el analisis por segmento.")
  operational_ok <- FALSE
} else {
  operational_ok <- TRUE
  oper_raw <- read.csv(operational_path, stringsAsFactors = FALSE)
  
  cat(sprintf("\noperational.csv cargado: %d filas x %d columnas\n",
              nrow(oper_raw), ncol(oper_raw)))
  cat(sprintf("  Columnas orig. en pipeline disponibles: %d / %d\n",
              sum(orig_pred_cols %in% names(oper_raw)),
              length(orig_pred_cols)))
  
  # Transformar al espacio del modelo con la función corregida (C3b)
  oper_feat <- preprocess_orig(
    df_orig   = oper_raw,
    pipeline  = prep_pipeline,
    orig_cols = orig_pred_cols,      # ← columnas ENTRADA del pipeline
    nom_vars  = nominal_vars_original,
    sep       = dummy_sep,
    feat_cols = feature_cols
  )
  
  oper_prob <- predict_prob(final_model, final_name, oper_feat)
  oper_all  <- oper_raw %>% mutate(prob_burnout = oper_prob)
  
  cat(sprintf("Predicciones operativas OK. Riesgo medio: %.2f %%\n",
              100 * mean(oper_prob)))
}

# ── 9.1. Por departamento ─────────────────────────────────────────────────────
if (operational_ok && "department" %in% names(oper_all)) {
  
  dept_df <- oper_all %>%
    group_by(department) %>%
    summarise(n_obs         = n(),
              riesgo_medio  = mean(prob_burnout),
              tasa_real     = mean(high_burnout_risk),
              n_alto_riesgo = sum(prob_burnout >= threshold),
              .groups = "drop") %>%
    arrange(desc(riesgo_medio))
  
  cat("\n── Riesgo por departamento ──────────────────────────────────────\n")
  print(dept_df, row.names = FALSE)
  
  write.csv(dept_df, "fase 5/reports/riesgo_por_departamento.csv",
            row.names = FALSE)
  files_generated <- c(files_generated,
                       "fase 5/reports/riesgo_por_departamento.csv")
  
  p_dept <- ggplot(
    dept_df %>%
      select(department, riesgo_medio, tasa_real) %>%
      pivot_longer(-department, names_to = "tipo", values_to = "valor") %>%
      mutate(tipo = recode(tipo,
                           riesgo_medio = "Riesgo predicho (medio)",
                           tasa_real    = "Tasa real de burnout")),
    aes(x = reorder(department, valor), y = valor, fill = tipo)
  ) +
    geom_col(position = "dodge", alpha = 0.85, width = 0.7) +
    coord_flip() +
    scale_fill_manual(values = c("Riesgo predicho (medio)" = "steelblue",
                                 "Tasa real de burnout"    = "tomato")) +
    scale_y_continuous(labels = scales::percent_format(accuracy = 0.1)) +
    labs(title    = "Riesgo predicho vs. tasa real por departamento",
         subtitle = sprintf("Modelo: %s", final_name),
         x = NULL, y = NULL, fill = NULL) +
    theme_minimal(base_size = 11) +
    theme(legend.position = "bottom")
  
  ggsave("fase 5/figures/fig3_riesgo_por_departamento.png", plot = p_dept,
         width = 10, height = 6, dpi = 150, bg = "white")
  files_generated <- c(files_generated,
                       "fase 5/figures/fig3_riesgo_por_departamento.png")
  cat("✓ Figura 3 (riesgo por departamento) guardada\n")
  
} else if (operational_ok) {
  message("'department' no encontrado en operational.csv — analisis omitido.")
}

# ── 9.2. Por nivel profesional ────────────────────────────────────────────────
if (operational_ok && "job_level" %in% names(oper_all)) {
  
  level_df <- oper_all %>%
    group_by(job_level) %>%
    summarise(n_obs         = n(),
              riesgo_medio  = mean(prob_burnout),
              tasa_real     = mean(high_burnout_risk),
              n_alto_riesgo = sum(prob_burnout >= threshold),
              .groups = "drop") %>%
    arrange(desc(riesgo_medio))
  
  cat("\n── Riesgo por nivel profesional ──────────────────────────────────\n")
  print(level_df, row.names = FALSE)
  
  write.csv(level_df, "fase 5/reports/riesgo_por_job_level.csv",
            row.names = FALSE)
  files_generated <- c(files_generated,
                       "fase 5/reports/riesgo_por_job_level.csv")
  
  p_level <- ggplot(
    level_df %>%
      select(job_level, riesgo_medio, tasa_real) %>%
      pivot_longer(-job_level, names_to = "tipo", values_to = "valor") %>%
      mutate(tipo = recode(tipo,
                           riesgo_medio = "Riesgo predicho (medio)",
                           tasa_real    = "Tasa real de burnout")),
    aes(x = reorder(job_level, valor), y = valor, fill = tipo)
  ) +
    geom_col(position = "dodge", alpha = 0.85, width = 0.6) +
    coord_flip() +
    scale_fill_manual(values = c("Riesgo predicho (medio)" = "steelblue",
                                 "Tasa real de burnout"    = "tomato")) +
    scale_y_continuous(labels = scales::percent_format(accuracy = 0.1)) +
    labs(title    = "Riesgo predicho vs. tasa real por nivel profesional",
         subtitle = sprintf("Modelo: %s", final_name),
         x = NULL, y = NULL, fill = NULL) +
    theme_minimal(base_size = 11) +
    theme(legend.position = "bottom")
  
  ggsave("fase 5/figures/fig4_riesgo_por_job_level.png", plot = p_level,
         width = 9, height = 5, dpi = 150, bg = "white")
  files_generated <- c(files_generated,
                       "fase 5/figures/fig4_riesgo_por_job_level.png")
  cat("✓ Figura 4 (riesgo por job_level) guardada\n")
  
} else if (operational_ok) {
  message("'job_level' no encontrado en operational.csv — analisis omitido.")
}

# ── 9.3. Cruce departamento × job_level ───────────────────────────────────────
if (operational_ok &&
    all(c("department", "job_level") %in% names(oper_all))) {
  
  cross_df <- oper_all %>%
    group_by(department, job_level) %>%
    summarise(riesgo_medio = mean(prob_burnout),
              n_obs        = n(),
              .groups = "drop")
  
  p_cross <- ggplot(cross_df,
                    aes(x = job_level, y = department, fill = riesgo_medio)) +
    geom_tile(color = "white", linewidth = 0.5) +
    geom_text(aes(label = sprintf("%.1f%%", riesgo_medio * 100)), size = 3.2) +
    scale_fill_gradient(low = "white", high = "tomato",
                        labels = scales::percent_format()) +
    labs(title    = "Riesgo predicho medio (%) — departamento x nivel profesional",
         subtitle = "Celdas con pocas obs. deben interpretarse con cautela",
         x = "Nivel profesional", y = "Departamento",
         fill = "Riesgo\nmedio") +
    theme_minimal(base_size = 11) +
    theme(axis.text.x = element_text(angle = 30, hjust = 1))
  
  ggsave("fase 5/figures/fig5_riesgo_cruce_dept_job_level.png", plot = p_cross,
         width = 10, height = 7, dpi = 150, bg = "white")
  files_generated <- c(files_generated,
                       "fase 5/figures/fig5_riesgo_cruce_dept_job_level.png")
  cat("✓ Figura 5 (cruce dept x job_level) guardada\n")
}


# =============================================================================
# 10. RESUMEN EJECUTIVO  (C4: solo lista lo generado realmente)
# =============================================================================
always_generated <- c(
  "fase 5/reports/importancia_global.csv",
  "fase 5/reports/contribuciones_locales_top_riesgo.csv",
  "fase 5/figures/fig1_importancia_global.png",
  "fase 5/figures/fig2_explicaciones_locales.png"
)
all_generated <- c(always_generated, files_generated)

top3_risk <- importance_df %>%
  filter(!is.na(direccion), direccion == "Aumenta riesgo") %>% head(3)
top3_prot <- importance_df %>%
  filter(!is.na(direccion), direccion == "Reduce riesgo")  %>% head(3)

criterio_dept  <- operational_ok && "department" %in% names(oper_all)
criterio_level <- operational_ok && "job_level"  %in% names(oper_all)
criterio_cruce <- criterio_dept && criterio_level

lineas <- c(
  "# Resumen de Interpretabilidad — Fase 5",
  "=========================================",
  "",
  sprintf("Modelo           : %s",   final_name),
  sprintf("AUC test         : %.4f", artifact$metrics_test$auc),
  sprintf("F1  test         : %.4f", artifact$metrics_test$f1),
  sprintf("Entrenado con    : %s",   artifact$trained_on),
  sprintf("Features totales : %d",   length(feature_cols)),
  ""
)

if (nrow(top3_risk) > 0)
  lineas <- c(lineas,
              "## Variables que mas AUMENTAN el riesgo",
              paste(sprintf("  %d. %-40s  imp=%.4f",
                            seq_along(top3_risk$variable),
                            top3_risk$variable, top3_risk$imp_value),
                    collapse = "\n"), "")

if (nrow(top3_prot) > 0)
  lineas <- c(lineas,
              "## Variables que mas REDUCEN el riesgo",
              paste(sprintf("  %d. %-40s  imp=%.4f",
                            seq_along(top3_prot$variable),
                            top3_prot$variable, top3_prot$imp_value),
                    collapse = "\n"), "")

lineas <- c(lineas,
            "## Archivos generados",
            paste0("  ", all_generated), "",
            "## Criterios de cierre",
            "  [x] Modelo cargado desde artefacto de Fase 4 (no reentrenado)",
            "  [x] Tipo de modelo detectado automaticamente",
            "  [x] Pipeline de Fase 3 cargado para transformar operational.csv",
            "      (fix: orig_pred_cols desde var_info, nom_vars desde step_dummy$columns)",
            "  [x] Importancia global segun tipo de modelo",
            "  [x] Explicaciones locales top 20 casos de mayor riesgo",
            if (criterio_dept)
              "  [x] Analisis por departamento" else
                "  [ ] Analisis por departamento omitido",
            if (criterio_level)
              "  [x] Analisis por job_level" else
                "  [ ] Analisis por job_level omitido",
            if (criterio_cruce)
              "  [x] Cruce departamento x job_level" else
                "  [ ] Cruce omitido",
            "  [x] Resumen solo declara archivos efectivamente generados"
)

writeLines(lineas, "fase 5/reports/resumen_interpretabilidad.txt")
cat("\n✓ Resumen guardado en fase 5/reports/resumen_interpretabilidad.txt\n")
cat(sprintf("✓ Fase 5 completada. Archivos generados: %d\n", length(all_generated)))