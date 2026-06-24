# =============================================================================
# FASE 4 — MODELADO PREDICTIVO DEL BURNOUT
# Predicción de Burnout y Optimización de Carga de Trabajo
# =============================================================================
#
# CORRECCIONES APLICADAS (según auditoría):
#   C1. Gestión automática de dependencias — el script instala lo que falta
#       y da un mensaje accionable si no puede. Elimina el fallo en library()
#   C2. Excluir 'split' de las features (columna añadida por Fase 3 corregida)
#   C3. Reentrenar el modelo final con train + validation antes de evaluar test
#   C4. Guardar el modelo final como artefacto RDS con metadatos completos
#   C5. Generar informe metodológico Markdown completo
#   C6. Importancia de variables para cualquier modelo, no solo Random Forest
# =============================================================================


# =============================================================================
# 0. GESTIÓN DE DEPENDENCIAS  (C1)
# =============================================================================
required_pkgs <- c("dplyr", "tidyr", "pROC", "ranger", "xgboost",
                   "ggplot2", "scales")

missing_pkgs <- required_pkgs[
  !sapply(required_pkgs, requireNamespace, quietly = TRUE)
]

if (length(missing_pkgs) > 0) {
  message(sprintf("[Fase 4] Instalando: %s",
                  paste(missing_pkgs, collapse = ", ")))
  install.packages(missing_pkgs,
                   repos = "https://cloud.r-project.org",
                   quiet = TRUE)
}

for (pkg in required_pkgs) {
  if (!requireNamespace(pkg, quietly = TRUE))
    stop(sprintf("[Fase 4] Paquete '%s' no disponible. Instálalo con:\n  install.packages('%s')",
                 pkg, pkg))
  suppressPackageStartupMessages(library(pkg, character.only = TRUE))
}

set.seed(42)

# ── Directorios de salida ─────────────────────────────────────────────────────
dir.create("fase 4",          showWarnings = FALSE)
dir.create("fase 4/reports",  showWarnings = FALSE)
dir.create("fase 4/figures",  showWarnings = FALSE)
dir.create("fase 4/models",   showWarnings = FALSE)


# =============================================================================
# 1. CARGA DE DATOS (salida de Fase 3)
# =============================================================================
train_raw <- read.csv("data fase 3/train.csv",      stringsAsFactors = FALSE)
valid_raw <- read.csv("data fase 3/validation.csv", stringsAsFactors = FALSE)
test_raw  <- read.csv("data fase 3/test.csv",       stringsAsFactors = FALSE)

id_cols    <- c("employee_id", "week", "team_id")
target_col <- "high_burnout_risk"

# C2: excluir 'split' de las features — es una columna de metadatos,
# incluirla filtraría información temporal directa al modelo
excl_cols <- c(id_cols, target_col, "split")

train_ids <- train_raw %>% select(all_of(id_cols))
valid_ids <- valid_raw %>% select(all_of(id_cols))
test_ids  <- test_raw  %>% select(all_of(id_cols))

train_y <- train_raw %>% pull(high_burnout_risk)
valid_y <- valid_raw %>% pull(high_burnout_risk)
test_y  <- test_raw  %>% pull(high_burnout_risk)

feature_cols <- setdiff(names(train_raw), excl_cols)

train_x <- train_raw %>% select(all_of(feature_cols))
valid_x <- valid_raw %>% select(all_of(feature_cols))
test_x  <- test_raw  %>% select(all_of(feature_cols))

cat(sprintf("Train : %d filas × %d features | tasa burnout: %.2f %%\n",
            nrow(train_x), ncol(train_x), 100 * mean(train_y)))
cat(sprintf("Valid : %d filas × %d features | tasa burnout: %.2f %%\n",
            nrow(valid_x), ncol(valid_x), 100 * mean(valid_y)))
cat(sprintf("Test  : %d filas × %d features | tasa burnout: %.2f %%\n",
            nrow(test_x),  ncol(test_x),  100 * mean(test_y)))


# =============================================================================
# 2. FUNCIÓN DE MÉTRICAS
# =============================================================================
calc_metrics <- function(y_true, y_prob, threshold = 0.5, model_name = "") {
  
  y_pred <- as.integer(y_prob >= threshold)
  
  tp <- sum(y_true == 1 & y_pred == 1)
  fp <- sum(y_true == 0 & y_pred == 1)
  fn <- sum(y_true == 1 & y_pred == 0)
  tn <- sum(y_true == 0 & y_pred == 0)
  
  precision <- ifelse(tp + fp == 0, 0, tp / (tp + fp))
  recall    <- ifelse(tp + fn == 0, 0, tp / (tp + fn))
  f1        <- ifelse(precision + recall == 0, 0,
                      2 * precision * recall / (precision + recall))
  accuracy  <- (tp + tn) / length(y_true)
  
  roc_obj <- suppressMessages(pROC::roc(y_true, y_prob, quiet = TRUE))
  auc_val <- as.numeric(pROC::auc(roc_obj))
  
  conf_mat <- matrix(c(tn, fp, fn, tp), nrow = 2,
                     dimnames = list(Predicted = c("0","1"),
                                     Actual    = c("0","1")))
  
  list(model = model_name,
       auc = round(auc_val, 4), f1 = round(f1, 4),
       precision = round(precision, 4), recall = round(recall, 4),
       accuracy  = round(accuracy, 4),
       tp = tp, fp = fp, fn = fn, tn = tn,
       conf_mat = conf_mat, roc_obj = roc_obj)
}

metrics_row <- function(m) {
  data.frame(modelo = m$model, auc = m$auc, f1 = m$f1,
             precision = m$precision, recall = m$recall,
             accuracy = m$accuracy,
             tp = m$tp, fp = m$fp, fn = m$fn, tn = m$tn,
             stringsAsFactors = FALSE)
}


# =============================================================================
# 3. BASELINE — Clasificador trivial (mayoría)
# =============================================================================
baseline_prob    <- rep(mean(train_y), nrow(valid_x))
metrics_baseline <- calc_metrics(valid_y, baseline_prob,
                                 threshold = 0.5,
                                 model_name = "Baseline (mayoría)")

cat(sprintf("\n[Baseline]  AUC: %.4f | F1: %.4f | Recall: %.4f\n",
            metrics_baseline$auc, metrics_baseline$f1, metrics_baseline$recall))


# =============================================================================
# 4. REGRESIÓN LOGÍSTICA
# =============================================================================
train_lr <- bind_cols(train_x, tibble(y = train_y))

lr_model <- glm(y ~ ., data = train_lr,
                family  = binomial(link = "logit"),
                control = list(maxit = 200))

lr_prob_valid <- predict(lr_model, newdata = valid_x, type = "response")
metrics_lr    <- calc_metrics(valid_y, lr_prob_valid,
                              threshold = 0.5,
                              model_name = "Regresión logística")

cat(sprintf("[Logística] AUC: %.4f | F1: %.4f | Recall: %.4f\n",
            metrics_lr$auc, metrics_lr$f1, metrics_lr$recall))


# =============================================================================
# 5. RANDOM FOREST
# =============================================================================
rf_model <- ranger::ranger(
  x             = train_x,
  y             = factor(train_y, levels = c(0, 1)),
  num.trees     = 500,
  mtry          = max(1, floor(sqrt(ncol(train_x)))),
  min.node.size = 10,
  class.weights = c("0" = 1, "1" = (1 - mean(train_y)) / mean(train_y)),
  probability   = TRUE,
  importance    = "impurity",
  seed          = 42,
  num.threads   = 1
)

rf_prob_valid <- predict(rf_model, data = valid_x)$predictions[, "1"]
metrics_rf    <- calc_metrics(valid_y, rf_prob_valid,
                              threshold = 0.5,
                              model_name = "Random Forest")

cat(sprintf("[RF]        AUC: %.4f | F1: %.4f | Recall: %.4f\n",
            metrics_rf$auc, metrics_rf$f1, metrics_rf$recall))


# =============================================================================
# 6. GRADIENT BOOSTING (XGBoost)
# =============================================================================
scale_pos <- sum(train_y == 0) / sum(train_y == 1)

dtrain <- xgboost::xgb.DMatrix(data = as.matrix(train_x), label = train_y)
dvalid <- xgboost::xgb.DMatrix(data = as.matrix(valid_x), label = valid_y)

xgb_params <- list(
  objective        = "binary:logistic",
  eval_metric      = "auc",
  eta              = 0.05,
  max_depth        = 5,
  subsample        = 0.8,
  colsample_bytree = 0.8,
  scale_pos_weight = scale_pos,
  seed             = 42
)

xgb_model <- xgboost::xgb.train(
  params                = xgb_params,
  data                  = dtrain,
  nrounds               = 400,
  watchlist             = list(train = dtrain, valid = dvalid),
  early_stopping_rounds = 30,
  verbose               = 0
)

xgb_prob_valid <- predict(xgb_model, newdata = dvalid)
metrics_xgb    <- calc_metrics(valid_y, xgb_prob_valid,
                               threshold = 0.5,
                               model_name = "Gradient Boosting (XGBoost)")

cat(sprintf("[XGBoost]   AUC: %.4f | F1: %.4f | Recall: %.4f\n",
            metrics_xgb$auc, metrics_xgb$f1, metrics_xgb$recall))


# =============================================================================
# 7. COMPARATIVA EN VALIDACIÓN
# =============================================================================
comparativa_valid <- bind_rows(
  metrics_row(metrics_baseline),
  metrics_row(metrics_lr),
  metrics_row(metrics_rf),
  metrics_row(metrics_xgb)
) %>% arrange(desc(auc))

cat("\n── Comparativa en validación ──────────────────────────────────────\n")
print(comparativa_valid %>% select(modelo, auc, f1, precision, recall, accuracy),
      row.names = FALSE)


# =============================================================================
# 8. SELECCIÓN DEL MODELO FINAL
# =============================================================================
best_row <- comparativa_valid %>%
  filter(modelo != "Baseline (mayoría)") %>%
  slice(1)

final_model_name <- best_row$modelo

cat(sprintf("\n── Modelo seleccionado: %s (AUC validación = %.4f) ──\n",
            final_model_name, best_row$auc))


# =============================================================================
# 9. REENTRENAMIENTO DEL MODELO FINAL CON TRAIN + VALIDATION  (C3)
#    Razón: el modelo de selección se entrenó solo con train para mantener
#    la validación como estimador imparcial. El modelo de producción
#    (y el que se evalúa en test) se reentrena con todos los datos etiquetados
#    disponibles antes del test, maximizando la información aprovechada.
# =============================================================================
trainval_x  <- bind_rows(train_x, valid_x)
trainval_y  <- c(train_y, valid_y)
sp_final    <- sum(trainval_y == 0) / sum(trainval_y == 1)

cat(sprintf("\nReentrenando '%s' con train+validation (%d filas)…\n",
            final_model_name, nrow(trainval_x)))

if (final_model_name == "Random Forest") {
  
  final_model <- ranger::ranger(
    x             = trainval_x,
    y             = factor(trainval_y, levels = c(0, 1)),
    num.trees     = 500,
    mtry          = max(1, floor(sqrt(ncol(trainval_x)))),
    min.node.size = 10,
    class.weights = c("0" = 1, "1" = sp_final),
    probability   = TRUE,
    importance    = "impurity",
    seed          = 42,
    num.threads   = 1
  )
  final_prob_test <- predict(final_model, data = test_x)$predictions[, "1"]
  
} else if (final_model_name == "Gradient Boosting (XGBoost)") {
  
  dtrainval <- xgboost::xgb.DMatrix(data  = as.matrix(trainval_x),
                                    label = trainval_y)
  # Se usa el nrounds óptimo identificado por early stopping en validación
  best_nrounds <- xgb_model$best_iteration
  final_model  <- xgboost::xgb.train(
    params  = modifyList(xgb_params,
                         list(scale_pos_weight = sp_final)),
    data    = dtrainval,
    nrounds = best_nrounds,
    verbose = 0
  )
  dtest           <- xgboost::xgb.DMatrix(data = as.matrix(test_x))
  final_prob_test <- predict(final_model, newdata = dtest)
  
} else {
  # Regresión logística
  tv_lr       <- bind_cols(trainval_x, tibble(y = trainval_y))
  final_model <- glm(y ~ ., data = tv_lr,
                     family  = binomial(link = "logit"),
                     control = list(maxit = 200))
  final_prob_test <- predict(final_model, newdata = test_x, type = "response")
}

cat("Reentrenamiento completado.\n")


# =============================================================================
# 10. EVALUACIÓN DEL MODELO FINAL EN TEST
# =============================================================================
metrics_final_test <- calc_metrics(test_y, final_prob_test,
                                   threshold  = 0.5,
                                   model_name = paste0(final_model_name, " (TEST)"))

cat("\n── Resultado en TEST ───────────────────────────────────────────────\n")
cat(sprintf("  AUC       : %.4f\n", metrics_final_test$auc))
cat(sprintf("  F1        : %.4f\n", metrics_final_test$f1))
cat(sprintf("  Precision : %.4f\n", metrics_final_test$precision))
cat(sprintf("  Recall    : %.4f\n", metrics_final_test$recall))
cat(sprintf("  Accuracy  : %.4f\n", metrics_final_test$accuracy))
cat(sprintf("  Conf. mat → TP:%d  FP:%d  FN:%d  TN:%d\n",
            metrics_final_test$tp, metrics_final_test$fp,
            metrics_final_test$fn, metrics_final_test$tn))


# =============================================================================
# 11. GUARDAR MODELO FINAL Y METADATOS  (C4)
#     Fase 5 y Fase 6 cargan este artefacto directamente.
#     El campo 'feature_cols' garantiza el mismo orden de columnas en inferencia.
# =============================================================================
model_artifact <- list(
  model_name   = final_model_name,
  model_object = final_model,
  feature_cols = feature_cols,
  threshold    = 0.5,
  trained_on   = "train + validation",
  metrics_valid = best_row,
  metrics_test  = metrics_row(metrics_final_test),
  r_version    = R.version$version.string,
  trained_at   = format(Sys.time(), "%Y-%m-%d %H:%M:%S")
)

saveRDS(model_artifact, "fase 4/models/final_model.rds")
cat("\n✓ Modelo final guardado en fase 4/models/final_model.rds\n")


# =============================================================================
# 12. EXPORTAR MÉTRICAS EN CSV
# =============================================================================
comparativa_completa <- bind_rows(
  comparativa_valid %>% mutate(conjunto = "validación"),
  metrics_row(metrics_final_test) %>% mutate(conjunto = "test")
) %>% relocate(conjunto, .after = modelo)

write.csv(comparativa_completa,
          "fase 4/reports/metricas_modelos.csv", row.names = FALSE)
cat("✓ Métricas exportadas en fase 4/reports/metricas_modelos.csv\n")


# =============================================================================
# 13. FIGURAS
# =============================================================================

# ── Figura 1: Curvas ROC ─────────────────────────────────────────────────────
png("fase 4/figures/fig7_roc_curves.png", width = 900, height = 700,
    res = 130, bg = "white")
plot(metrics_baseline$roc_obj, col = "gray60",      lwd = 1.5,
     main = "Curvas ROC — comparativa en validación", legacy.axes = TRUE)
plot(metrics_lr$roc_obj,       col = "steelblue",   lwd = 2, add = TRUE)
plot(metrics_rf$roc_obj,       col = "forestgreen", lwd = 2, add = TRUE)
plot(metrics_xgb$roc_obj,      col = "tomato",      lwd = 2, add = TRUE)
abline(a = 0, b = 1, lty = 2, col = "black")
legend("bottomright",
       legend = c(
         sprintf("Baseline       AUC=%.3f", metrics_baseline$auc),
         sprintf("Log. Regresión AUC=%.3f", metrics_lr$auc),
         sprintf("Random Forest  AUC=%.3f", metrics_rf$auc),
         sprintf("XGBoost        AUC=%.3f", metrics_xgb$auc)
       ),
       col = c("gray60", "steelblue", "forestgreen", "tomato"),
       lwd = 2, bty = "n", cex = 0.85)
dev.off()

# ── Figura 2: Comparativa de métricas en validación ──────────────────────────
comp_long <- comparativa_valid %>%
  filter(modelo != "Baseline (mayoría)") %>%
  select(modelo, auc, f1, precision, recall) %>%
  pivot_longer(-modelo, names_to = "metrica", values_to = "valor")

p_metricas <- ggplot(comp_long, aes(x = metrica, y = valor, fill = modelo)) +
  geom_col(position = "dodge", width = 0.65) +
  scale_fill_manual(values = c(
    "Regresión logística"         = "steelblue",
    "Random Forest"               = "forestgreen",
    "Gradient Boosting (XGBoost)" = "tomato"
  )) +
  scale_y_continuous(limits = c(0, 1), labels = scales::percent_format()) +
  labs(title = "Comparativa de métricas en validación",
       x = "Métrica", y = "Valor", fill = "Modelo") +
  theme_minimal(base_size = 12) +
  theme(legend.position = "bottom")

ggsave("fase 4/figures/fig8_metricas_comparativa.png", plot = p_metricas,
       width = 9, height = 5, dpi = 150, bg = "white")

# ── Figura 3: Importancia de variables del modelo final  (C6) ─────────────────
# Se genera para cualquier tipo de modelo, no solo RF
get_importance_df <- function(model_name, lr_obj, rf_obj, xgb_obj) {
  if (model_name == "Random Forest" && !is.null(rf_obj$variable.importance)) {
    data.frame(variable  = names(rf_obj$variable.importance),
               imp_value = rf_obj$variable.importance)
  } else if (model_name == "Gradient Boosting (XGBoost)") {
    imp <- xgboost::xgb.importance(model = xgb_obj)
    data.frame(variable = imp$Feature, imp_value = imp$Gain)
  } else {
    # Regresión logística: valor absoluto del coeficiente (excluye intercepto)
    coefs <- coef(lr_obj)
    coefs <- coefs[names(coefs) != "(Intercept)"]
    data.frame(variable = names(coefs), imp_value = abs(as.numeric(coefs)))
  }
}

imp_df <- get_importance_df(final_model_name,
                            lr_model, rf_model, xgb_model) %>%
  arrange(-imp_value) %>%
  head(20)

subtitle_imp <- switch(final_model_name,
                       "Random Forest"               = "Métrica: impurity (reducción media de Gini)",
                       "Gradient Boosting (XGBoost)" = "Métrica: Gain (XGBoost)",
                       "Métrica: |coeficiente| (log-odds)"
)

p_imp <- ggplot(imp_df,
                aes(x = reorder(variable, imp_value), y = imp_value)) +
  geom_col(fill = "steelblue", alpha = 0.85) +
  coord_flip() +
  labs(title    = sprintf("Importancia de variables — %s (top 20)",
                          final_model_name),
       subtitle = subtitle_imp,
       x = "", y = "Importancia") +
  theme_minimal(base_size = 11)

ggsave("fase 4/figures/fig9_feature_importance.png", plot = p_imp,
       width = 9, height = 6, dpi = 150, bg = "white")

# ── Figura 4: Matriz de confusión del modelo final en TEST ───────────────────
conf_df <- data.frame(
  Predicho = factor(c("0","0","1","1"), levels = c("0","1")),
  Real     = factor(c("0","1","0","1"), levels = c("0","1")),
  n        = c(metrics_final_test$tn, metrics_final_test$fn,
               metrics_final_test$fp, metrics_final_test$tp)
)

p_conf <- ggplot(conf_df, aes(x = Real, y = Predicho, fill = n)) +
  geom_tile(color = "white") +
  geom_text(aes(label = n), size = 6, fontface = "bold") +
  scale_fill_gradient(low = "white", high = "steelblue") +
  labs(title = paste0("Matriz de confusión — ", final_model_name, " (TEST)"),
       x = "Clase real", y = "Clase predicha") +
  theme_minimal(base_size = 12) +
  theme(legend.position = "none")

ggsave("fase 4/figures/fig10_confusion_matrix_test.png", plot = p_conf,
       width = 5, height = 4, dpi = 150, bg = "white")

cat("✓ Figuras exportadas en fase 4/figures/\n")


# =============================================================================
# 14. INFORME METODOLÓGICO MARKDOWN  (C5)
# =============================================================================
baseline_gain <- round((best_row$auc - metrics_baseline$auc) * 100, 1)

fmt_table <- function(df) {
  rows <- apply(
    df %>% select(modelo, auc, f1, precision, recall, accuracy), 1,
    function(r) sprintf("| %-32s | %5s | %5s | %9s | %6s | %8s |",
                        r["modelo"], r["auc"], r["f1"],
                        r["precision"], r["recall"], r["accuracy"])
  )
  hdr <- "| Modelo                           |   AUC |    F1 | Precision | Recall | Accuracy |"
  sep <- "|----------------------------------|------:|------:|----------:|-------:|---------:|"
  paste(c(hdr, sep, rows), collapse = "\n")
}

informe <- c(
  "# Informe metodológico — Fase 4: Modelado predictivo del burnout",
  "",
  sprintf("Generado : %s  ", format(Sys.time(), "%Y-%m-%d %H:%M:%S")),
  sprintf("R version: %s  ", R.version$version.string),
  "",
  "---",
  "",
  "## 1. Objetivo",
  "",
  "Entrenar y comparar modelos de clasificación binaria para predecir",
  "`high_burnout_risk` a partir de los datos preprocesados en Fase 3.",
  "La variable objetivo está desbalanceada; se aplica ponderación de clases",
  "en los modelos que lo soportan (RF, XGBoost).",
  "",
  "## 2. Modelos evaluados",
  "",
  "- **Baseline (mayoría):** predice siempre la tasa media del train.",
  "  Cota mínima que cualquier modelo real debe superar.",
  "- **Regresión logística:** modelo lineal interpretable en escala log-odds.",
  "- **Random Forest:** 500 árboles, `mtry = sqrt(p)`, `min.node.size = 10`.",
  "- **XGBoost:** boosting con `eta = 0.05`, parada temprana sobre AUC en validación.",
  "",
  "## 3. Criterio de selección",
  "",
  "Modelo con mayor **AUC en validación** (semanas 37–44), excluyendo el baseline.",
  "El AUC mide capacidad discriminante independientemente del umbral, relevante",
  "en detección temprana donde el umbral se ajusta al coste real de cada error.",
  "",
  "## 4. Resultados en validación",
  "",
  fmt_table(comparativa_valid),
  "",
  sprintf("**Modelo seleccionado: %s** (AUC = %.4f)",
          final_model_name, best_row$auc),
  sprintf("Ganancia sobre baseline: +%.1f pp de AUC.", baseline_gain),
  "",
  "## 5. Reentrenamiento y resultado en test",
  "",
  sprintf("El modelo final se reentrenó con **train + validation** (%d filas)",
          nrow(trainval_x)),
  "antes de evaluar en test (semanas 45–52). Esto maximiza la información",
  "disponible sin contaminar el test.",
  "",
  "| Métrica   | Valor  |",
  "|-----------|-------:|",
  sprintf("| AUC       | %.4f |", metrics_final_test$auc),
  sprintf("| F1        | %.4f |", metrics_final_test$f1),
  sprintf("| Precision | %.4f |", metrics_final_test$precision),
  sprintf("| Recall    | %.4f |", metrics_final_test$recall),
  sprintf("| Accuracy  | %.4f |", metrics_final_test$accuracy),
  sprintf("| TP / FP   | %d / %d |", metrics_final_test$tp, metrics_final_test$fp),
  sprintf("| FN / TN   | %d / %d |", metrics_final_test$fn, metrics_final_test$tn),
  "",
  "## 6. Interpretación de métricas",
  "",
  "- **AUC > 0.80** → buena capacidad discriminante para detección de burnout.",
  "- **Recall** es la métrica prioritaria: un falso negativo (empleado en riesgo",
  "  no detectado) tiene mayor coste humano que un falso positivo.",
  "- **F1** equilibra precision y recall; útil como métrica única en clases",
  "  desbalanceadas.",
  "- El umbral por defecto (0.5) es provisional. En producción debe calibrarse",
  "  según el coste relativo de cada tipo de error.",
  "",
  "## 7. Limitaciones",
  "",
  "1. Datos sintéticos: el rendimiento en producción puede diferir.",
  "2. Umbral 0.5 provisional, no calibrado.",
  "3. Variables lag imputan NAs de las primeras semanas con la mediana,",
  "   lo que puede introducir sesgo en empleados recién incorporados.",
  "4. El modelo captura patrones temporales mediante ventana deslizante,",
  "   no es un modelo secuencial nativo.",
  "",
  "## 8. Artefactos exportados",
  "",
  "| Artefacto | Ruta |",
  "|-----------|------|",
  "| Modelo final (RDS) | `fase 4/models/final_model.rds` |",
  "| Métricas (CSV) | `fase 4/reports/metricas_modelos.csv` |",
  "| Curvas ROC | `fase 4/figures/fig7_roc_curves.png` |",
  "| Comparativa métricas | `fase 4/figures/fig8_metricas_comparativa.png` |",
  "| Importancia variables | `fase 4/figures/fig9_feature_importance.png` |",
  "| Matriz confusión TEST | `fase 4/figures/fig10_confusion_matrix_test.png` |",
  "",
  "## 9. Paso a Fase 5",
  "",
  sprintf("El modelo `%s` está guardado en `fase 4/models/final_model.rds`.",
          final_model_name),
  "Fase 5 debe cargarlo con `readRDS()` y usar `model_object` para predicciones.",
  "`feature_cols` garantiza el mismo orden de columnas en inferencia.",
  ""
)

writeLines(informe, "fase 4/reports/informe_metodologico_fase4.md")
cat("✓ Informe metodológico exportado en fase 4/reports/informe_metodologico_fase4.md\n")

cat("\n══════════════════════════════════════════════════════════════════\n")
cat(sprintf(" Fase 4 completada. Modelo: %s\n", final_model_name))
cat(sprintf(" AUC validación: %.4f | AUC test: %.4f\n",
            best_row$auc, metrics_final_test$auc))
cat("══════════════════════════════════════════════════════════════════\n")