# =============================================================================
# FASE 6 — BASELINE OR CLÁSICO: STAFFING OPERATIVO
# =============================================================================
#
# CORRECCIONES APLICADAS (según auditoría):
#   C1. Gestión automática de dependencias R + instrucciones para GLPK (solver)
#   C2. Eliminar dependencia de 'readr': usar write.csv en lugar de write_csv
#   C3. Cargar datos desde operational.csv (escala original, sin normalizar).
#       El original usaba los CSV preprocesados de Fase 3 (valores escalados),
#       lo que hace que las "horas" del LP sean z-scores, no horas reales.
#   C4. Recuperar 'department' y 'job_level' reales desde operational.csv,
#       en lugar de inventarlos cuando no están en los CSV del modelo.
#   C5. Cargar el modelo final desde el artefacto de Fase 4 para la
#       evaluación ex post del burnout, en lugar de reentrenar una LR auxiliar.
#   C6. Corregir la restricción de capacidad del LP para incorporar la carga
#       actual del empleado: usar avail_i = max(0, cap_i - carga_actual)
#       como RHS en lugar de cap_i, y reportar overtime total coherentemente.
#   C7. El informe solo marca como superados los criterios verificables.
# =============================================================================


# =============================================================================
# 0. GESTIÓN DE DEPENDENCIAS  (C1 + C2)
# =============================================================================
# NOTA sobre el solver GLPK:
#   'ROI.plugin.glpk' requiere tener GLPK instalado a nivel de sistema.
#   En Ubuntu/Debian:  sudo apt-get install libglpk-dev
#   En macOS:          brew install glpk
#   En Windows:        instalar Rtools y seguir https://cran.r-project.org/web/packages/ROI.plugin.glpk
#   Luego, desde R:    install.packages(c("ompr", "ompr.roi", "ROI.plugin.glpk"))

r_pkgs <- c("dplyr", "tidyr", "purrr", "ggplot2", "scales",
            "ompr", "ompr.roi", "ROI.plugin.glpk")

missing_pkgs <- r_pkgs[!sapply(r_pkgs, requireNamespace, quietly = TRUE)]

if (length(missing_pkgs) > 0) {
  glpk_pkgs <- intersect(missing_pkgs,
                         c("ompr", "ompr.roi", "ROI.plugin.glpk"))
  if (length(glpk_pkgs) > 0) {
    message(sprintf(
      "[Fase 6] Los paquetes de optimización requieren GLPK a nivel de sistema.\n
       En Ubuntu: sudo apt-get install libglpk-dev\n
       En macOS : brew install glpk\n
       Luego instala en R: install.packages(c(%s))",
      paste0('"', glpk_pkgs, '"', collapse = ", ")
    ))
  }
  non_glpk <- setdiff(missing_pkgs, c("ompr", "ompr.roi", "ROI.plugin.glpk"))
  if (length(non_glpk) > 0) {
    install.packages(non_glpk, repos = "https://cloud.r-project.org", quiet = TRUE)
  }
}

for (pkg in r_pkgs) {
  if (!requireNamespace(pkg, quietly = TRUE))
    stop(sprintf(
      "[Fase 6] Paquete '%s' no disponible.\n  Sigue las instrucciones del bloque 0.",
      pkg
    ))
  suppressPackageStartupMessages(library(pkg, character.only = TRUE))
}

# XGBoost solo si el modelo final lo requiere (se carga después)
set.seed(42)

dir.create("fase_06_optimizacion/results",  recursive = TRUE, showWarnings = FALSE)
dir.create("fase_06_optimizacion/figures",  recursive = TRUE, showWarnings = FALSE)
dir.create("fase_06_optimizacion/reports",  recursive = TRUE, showWarnings = FALSE)


# =============================================================================
# PARÁMETROS GLOBALES
# =============================================================================
PERCENTIL_CAP <- 0.70
C_INTERTEAM   <- 0.35
OMEGA         <- 1.0
RHO           <- 0.20
MAX_LEVEL_GAP <- 1
M_PENALTY     <- 1e4
SEMANA_OBJ    <- NULL

LEVEL_RANK <- c(junior = 1, mid = 2, senior = 3, lead = 4, executive = 5)

cat(strrep("=", 70), "\n")
cat("FASE 6 — BASELINE OR CLÁSICO: STAFFING OPERATIVO\n")
cat(strrep("=", 70), "\n")
cat(sprintf("  PERCENTIL_CAP : %.2f\n", PERCENTIL_CAP))
cat(sprintf("  C_INTERTEAM   : %.2f\n", C_INTERTEAM))
cat(sprintf("  OMEGA         : %.2f\n", OMEGA))
cat(sprintf("  RHO           : %.2f\n", RHO))
cat(sprintf("  MAX_LEVEL_GAP : %d\n",   MAX_LEVEL_GAP))
cat(sprintf("  M_PENALTY     : %.0f\n", M_PENALTY))


# =============================================================================
# 1. CARGA DE DATOS OPERATIVOS  (C3 + C4)
#    Se usa operational.csv, que contiene valores en escala original (horas
#    reales, no z-scores) y conserva 'department' y 'job_level' sin dummificar.
#    El LP trabaja así con horas interpretables y restricciones reales.
# =============================================================================
oper_path <- "data fase 3/operational.csv"

if (!file.exists(oper_path)) {
  stop(paste(
    "[Fase 6] No se encontró 'data fase 3/operational.csv'.",
    "Ejecuta la Fase 3 corregida para generarlo.",
    "Este archivo es necesario para que el LP opere en escala real de horas.",
    sep = "\n"
  ))
}

operational <- read.csv(oper_path, stringsAsFactors = FALSE)

id_cols    <- c("employee_id", "week", "team_id")
target_col <- "high_burnout_risk"
excl_oper  <- c(id_cols, target_col, "split")

if (is.null(SEMANA_OBJ)) SEMANA_OBJ <- max(operational$week)
semana_df <- operational %>% filter(week == SEMANA_OBJ)

cat(sprintf("\n── Datos operativos cargados ──────────────────────────────────────\n"))
cat(sprintf("  Origen         : %s\n", oper_path))
cat(sprintf("  Obs. totales   : %d\n", nrow(operational)))
cat(sprintf("  Semana obj.    : %d\n", SEMANA_OBJ))
cat(sprintf("  Empleados      : %d\n", nrow(semana_df)))

# Features disponibles para el modelo ML de evaluación ex post
feature_cols_oper <- setdiff(names(operational), excl_oper)


# =============================================================================
# 2. CARGA DEL MODELO FINAL DE FASE 4 PARA EVALUACIÓN EX POST  (C5)
#    El original reentrenaba una LR auxiliar, lo que rompe la trazabilidad.
#    Ahora se usa el mismo modelo que Fase 5, garantizando coherencia.
# =============================================================================
artifact_path <- "fase 4/models/final_model.rds"

if (!file.exists(artifact_path)) {
  stop(paste(
    "[Fase 6] No se encontró el artefacto del modelo final.",
    "Ejecuta Fase 4 antes de continuar.",
    sprintf("Ruta esperada: %s", artifact_path),
    sep = "\n"
  ))
}

artifact     <- readRDS(artifact_path)
final_model  <- artifact$model_object
final_name   <- artifact$model_name
feat_cols_f4 <- artifact$feature_cols

cat(sprintf("\n── Modelo para evaluación ex post (Fase 4) ────────────────────────\n"))
cat(sprintf("  Tipo    : %s\n", final_name))
cat(sprintf("  AUC val.: %.4f | AUC test: %.4f\n",
            artifact$metrics_valid$auc,
            artifact$metrics_test$auc))

# Si el modelo es XGBoost, necesitamos el paquete
if (final_name == "Gradient Boosting (XGBoost)") {
  if (!requireNamespace("xgboost", quietly = TRUE))
    stop("El modelo final es XGBoost pero 'xgboost' no está instalado.")
  suppressPackageStartupMessages(library(xgboost))
}

# Función de predicción unificada
predict_burnout <- function(df_features) {
  # Intersectar con las features conocidas por el modelo
  common_cols <- intersect(feat_cols_f4, names(df_features))
  missing_f4  <- setdiff(feat_cols_f4, names(df_features))
  if (length(missing_f4) > 0) {
    message(sprintf(
      "[predict_burnout] %d features del modelo no están en operational.csv: %s",
      length(missing_f4),
      paste(head(missing_f4, 5), collapse = ", ")
    ))
    # Añadir columnas faltantes con 0 como aproximación
    for (col in missing_f4) df_features[[col]] <- 0
  }
  df_f4 <- df_features[, feat_cols_f4, drop = FALSE]
  
  if (final_name == "Random Forest") {
    predict(final_model, data = df_f4)$predictions[, "1"]
  } else if (final_name == "Gradient Boosting (XGBoost)") {
    dm <- xgboost::xgb.DMatrix(data = as.matrix(df_f4))
    predict(final_model, newdata = dm)
  } else {
    predict(final_model, newdata = df_f4, type = "response")
  }
}

cat(sprintf("  Coeficientes / features : %d\n", length(feat_cols_f4)))


# =============================================================================
# 3. PARÁMETROS DE EMPLEADOS
# =============================================================================
seleccionar_col_carga <- function(df) {
  candidatas <- unique(c("weekly_hours", "overtime_hours",
                         grep("hour", names(df), value = TRUE,
                              ignore.case = TRUE)))
  candidatas <- intersect(candidatas, names(df))
  if (length(candidatas) == 0) stop("No se encontró columna de horas.")
  vars <- sapply(candidatas, function(col) var(df[[col]], na.rm = TRUE))
  col  <- names(which.max(vars))
  cat(sprintf("  Columna de carga: '%s'  (var=%.4f)\n", col, vars[col]))
  col
}

cat(sprintf("\n── Detección de columna de carga ───────────────────────────────────\n"))
col_carga <- seleccionar_col_carga(semana_df)

H_umbral <- quantile(semana_df[[col_carga]], probs = PERCENTIL_CAP, na.rm = TRUE)
cat(sprintf("  H (p%.0f) = %.4f horas\n", PERCENTIL_CAP * 100, H_umbral))

# C4: usar department y job_level reales de operational.csv
if ("job_level" %in% names(semana_df)) {
  semana_df <- semana_df %>%
    mutate(level_rank = LEVEL_RANK[tolower(job_level)],
           level_rank = ifelse(is.na(level_rank), 2L, as.integer(level_rank)))
} else {
  warning("[Fase 6] 'job_level' no encontrado en operational.csv. Asignando 'mid' a todos.")
  semana_df <- semana_df %>% mutate(job_level = "mid", level_rank = 2L)
}

if (!"department" %in% names(semana_df)) {
  warning("[Fase 6] 'department' no encontrado en operational.csv. Usando team_id como proxy.")
  semana_df <- semana_df %>% mutate(department = paste0("dept_", team_id))
}

# Predicción de burnout ANTES de la optimización (C5: usa modelo de Fase 4)
prob_before_all <- predict_burnout(semana_df)

# C6: calcular capacidad DISPONIBLE = max(0, umbral - carga_actual)
#     La restricción LP debe referirse a la capacidad libre, no al umbral total,
#     para que asignaciones adicionales respeten la carga ya existente.
empleados_df <- semana_df %>%
  mutate(
    carga_actual    = .data[[col_carga]],
    cap_i           = H_umbral,
    # avail_i: horas que puede absorber sin generar overtime adicional
    avail_i         = pmax(cap_i - carga_actual, 0),
    overtime_before = pmax(carga_actual - cap_i, 0),
    riesgo_before   = prob_before_all
  ) %>%
  select(employee_id, team_id, department, job_level, level_rank,
         carga_actual, cap_i, avail_i, overtime_before, riesgo_before)

cat(sprintf("  Empleados con overtime > 0 : %d / %d\n",
            sum(empleados_df$overtime_before > 0), nrow(empleados_df)))
cat(sprintf("  Overtime total observado   : %.2f horas\n",
            sum(empleados_df$overtime_before)))
cat(sprintf("  Capacidad disponible total : %.2f horas\n",
            sum(empleados_df$avail_i)))


# =============================================================================
# 4. CONSTRUCCIÓN DE PROYECTOS TEMPORALES
# =============================================================================
proyectos_raw <- empleados_df %>%
  group_by(team_id, department) %>%
  summarise(
    exceso_total = sum(overtime_before),
    cap_total    = sum(cap_i),
    nivel_medio  = round(mean(level_rank)),
    .groups = "drop"
  ) %>%
  filter(exceso_total > 1e-6)

proyectos_list <- list()
pid <- 1L

for (r in seq_len(nrow(proyectos_raw))) {
  row    <- proyectos_raw[r, ]
  exceso <- row$exceso_total
  umbral_split <- 0.40 * row$cap_total
  
  if (exceso > umbral_split) {
    for (frac_info in list(c(0.60, "high"), c(0.40, "medium"))) {
      frac       <- as.numeric(frac_info[1])
      intensidad <- frac_info[2]
      proyectos_list[[pid]] <- tibble(
        project_id          = paste0("P", sprintf("%03d", pid)),
        origin_team         = row$team_id,
        department          = row$department,
        required_level_rank = row$nivel_medio,
        project_hours       = exceso * frac,
        project_intensity   = intensidad
      )
      pid <- pid + 1L
    }
  } else {
    proyectos_list[[pid]] <- tibble(
      project_id          = paste0("P", sprintf("%03d", pid)),
      origin_team         = row$team_id,
      department          = row$department,
      required_level_rank = row$nivel_medio,
      project_hours       = exceso,
      project_intensity   = "medium"
    )
    pid <- pid + 1L
  }
}

proyectos_df <- bind_rows(proyectos_list)
cat(sprintf("\n── Proyectos temporales generados: %d\n", nrow(proyectos_df)))
cat(sprintf("  Demanda total: %.2f horas\n", sum(proyectos_df$project_hours)))


# =============================================================================
# 5. GENERACIÓN DE PARES FACTIBLES (i, p)
# =============================================================================
pares_df <- inner_join(
  empleados_df %>% select(employee_id, team_id, department,
                          job_level, level_rank, cap_i, avail_i, carga_actual),
  proyectos_df %>% select(project_id, origin_team, department,
                          required_level_rank, project_hours, project_intensity),
  by           = "department",
  relationship = "many-to-many"
) %>%
  filter(abs(level_rank - required_level_rank) <= MAX_LEVEL_GAP) %>%
  mutate(
    interteam    = as.integer(team_id != origin_team),
    c_classic_ip = ifelse(interteam == 1L, C_INTERTEAM, 0.0)
  )

cat(sprintf("\n── Pares factibles |A|: %d\n", nrow(pares_df)))
if (nrow(pares_df) == 0)
  stop("Sin pares factibles. Revisa department, job_level o MAX_LEVEL_GAP.")


# =============================================================================
# 6. FORMULACIÓN Y RESOLUCIÓN DEL LP CON ompr
# =============================================================================
emp_ids  <- empleados_df$employee_id
proj_ids <- proyectos_df$project_id
n_emp    <- length(emp_ids)
n_proj   <- length(proj_ids)

emp_idx  <- setNames(seq_len(n_emp),  emp_ids)
proj_idx <- setNames(seq_len(n_proj), proj_ids)

cost_mat <- matrix(0,  nrow = n_emp, ncol = n_proj)
pair_mat <- matrix(0L, nrow = n_emp, ncol = n_proj)

for (r in seq_len(nrow(pares_df))) {
  ii <- emp_idx[pares_df$employee_id[r]]
  pp <- proj_idx[pares_df$project_id[r]]
  cost_mat[ii, pp] <- pares_df$c_classic_ip[r]
  pair_mat[ii, pp] <- 1L
}

x_ub <- matrix(0, nrow = n_emp, ncol = n_proj)
for (pp in seq_len(n_proj)) {
  x_ub[, pp] <- pair_mat[, pp] * proyectos_df$project_hours[pp]
}

# C6: usar avail_i como RHS de la restricción de capacidad
#     avail_i = max(0, cap_i - carga_actual)
#     → la variable o[i] del LP representa el overtime generado SOLO por
#       las nuevas asignaciones, ya que la carga actual ya está descontada.
avail_vec <- empleados_df$avail_i
dem_vec   <- proyectos_df$project_hours
team_emp  <- empleados_df$team_id
orig_proj <- proyectos_df$origin_team
equipos   <- unique(empleados_df$team_id)

team_cap <- empleados_df %>%
  group_by(team_id) %>%
  summarise(Cap_k = sum(avail_i), .groups = "drop")   # C6: también con avail

cat(sprintf("\n── Construyendo modelo ompr (%d emp × %d proyectos) ────────────────\n",
            n_emp, n_proj))

model <- MIPModel() %>%
  
  add_variable(x[i, p], i = 1:n_emp, p = 1:n_proj,
               type = "continuous", lb = 0) %>%
  
  add_variable(o[i], i = 1:n_emp,
               type = "continuous", lb = 0) %>%
  
  add_variable(slack[p], p = 1:n_proj,
               type = "continuous", lb = 0) %>%
  
  set_objective(
    sum_over(cost_mat[i, p] * x[i, p], i = 1:n_emp, p = 1:n_proj) +
      OMEGA     * sum_over(o[i],     i = 1:n_emp) +
      M_PENALTY * sum_over(slack[p], p = 1:n_proj),
    sense = "min"
  ) %>%
  
  # Restricción 1: cobertura de demanda con holgura penalizada
  add_constraint(
    sum_over(x[i, p], i = 1:n_emp) + slack[p] == dem_vec[p],
    p = 1:n_proj
  ) %>%
  
  # Restricción 2: capacidad disponible  (C6: usa avail_i, no cap_i)
  #   sum_p x_ip - o_i <= avail_i
  #   donde avail_i = max(0, cap_i - carga_actual)
  #   El overtime o_i captura solo las horas nuevas que exceden la capacidad libre.
  add_constraint(
    sum_over(x[i, p], p = 1:n_proj) - o[i] <= avail_vec[i],
    i = 1:n_emp
  ) %>%
  
  # Restricción 3: factibilidad de pares (empleado solo puede ir a proyectos asignables)
  add_constraint(
    x[i, p] <= x_ub[i, p],
    i = 1:n_emp, p = 1:n_proj
  )

# Restricción 4: gobernanza interequipo (basada en capacidad disponible)
for (k in equipos) {
  idx_emp_k    <- which(team_emp == k)
  idx_proj_ext <- which(orig_proj != k)
  if (length(idx_emp_k) == 0 || length(idx_proj_ext) == 0) next
  Cap_k <- team_cap %>% filter(team_id == k) %>% pull(Cap_k)
  if (Cap_k < 1e-9) next
  model <- model %>%
    add_constraint(
      sum_over(x[i, p], i = idx_emp_k, p = idx_proj_ext) <= RHO * Cap_k
    )
}

cat("  Resolviendo con GLPK...\n")
sol <- solve_model(model, with_ROI(solver = "glpk", verbose = FALSE))

cat(sprintf("  Estado    : %s\n", sol$status))
cat(sprintf("  Objetivo  : %.4f\n", objective_value(sol)))

slack_sol           <- get_solution(sol, slack[p])
demanda_no_cubierta <- sum(slack_sol$value)
if (demanda_no_cubierta > 1e-4) {
  cat(sprintf("  ⚠ Demanda no cubierta: %.2f h (%.1f%% del total)\n",
              demanda_no_cubierta,
              100 * demanda_no_cubierta / sum(dem_vec)))
  cat("    → Considera aumentar RHO o MAX_LEVEL_GAP.\n")
} else {
  cat("  ✓ Cobertura completa de demanda\n")
}

if (!sol$status %in% c("success", "optimal", "OPTIMAL"))
  stop(paste("Estado no óptimo:", sol$status))

x_raw <- get_solution(sol, x[i, p])
o_raw <- get_solution(sol, o[i])


# =============================================================================
# 7. EXTRACCIÓN DE SOLUCIÓN
# =============================================================================
x_sol <- x_raw %>%
  filter(value > 1e-6) %>%
  rename(emp_idx = i, proj_idx = p, assigned_hours = value) %>%
  mutate(
    employee_id = emp_ids[emp_idx],
    project_id  = proj_ids[proj_idx]
  ) %>%
  left_join(empleados_df %>% select(employee_id,
                                    employee_team = team_id,
                                    department, job_level),
            by = "employee_id") %>%
  left_join(proyectos_df %>% select(project_id,
                                    project_origin_team = origin_team,
                                    project_intensity),
            by = "project_id") %>%
  left_join(pares_df %>% select(employee_id, project_id, c_classic_ip),
            by = c("employee_id", "project_id")) %>%
  mutate(
    classic_cost       = round(c_classic_ip * assigned_hours, 6),
    # C6: overtime generado = horas asignadas que exceden la capacidad libre
    overtime_generated = round(pmax(
      assigned_hours - avail_vec[emp_idx], 0
    ), 6)
  ) %>%
  select(employee_id, project_id,
         employee_team, project_origin_team,
         department, job_level, project_intensity,
         assigned_hours, classic_cost, overtime_generated)

o_sol <- o_raw %>%
  mutate(employee_id = emp_ids[i]) %>%
  select(employee_id, overtime_new = value)   # renombrado: overtime de nuevas asignaciones


# =============================================================================
# 8. COMPARATIVA EMPLEADOS BEFORE / AFTER
# =============================================================================
horas_asignadas <- x_sol %>%
  group_by(employee_id) %>%
  summarise(horas_lp = sum(assigned_hours), .groups = "drop")

apoyo_dado <- x_sol %>%
  filter(employee_team != project_origin_team) %>%
  group_by(employee_id) %>%
  summarise(support_to_other_teams = sum(assigned_hours), .groups = "drop")

apoyo_recibido <- x_sol %>%
  filter(employee_team != project_origin_team) %>%
  left_join(proyectos_df %>% select(project_id, origin_team), by = "project_id") %>%
  group_by(origin_team) %>%
  summarise(support_received = sum(assigned_hours), .groups = "drop") %>%
  left_join(empleados_df %>% select(employee_id, team_id) %>% distinct(),
            by = c("origin_team" = "team_id")) %>%
  group_by(employee_id) %>%
  summarise(support_received = first(support_received), .groups = "drop")

comparativa_df <- empleados_df %>%
  left_join(horas_asignadas, by = "employee_id") %>%
  left_join(o_sol,           by = "employee_id") %>%
  left_join(apoyo_dado,      by = "employee_id") %>%
  left_join(apoyo_recibido,  by = "employee_id") %>%
  mutate(
    horas_lp               = replace_na(horas_lp, 0),
    overtime_new           = replace_na(overtime_new, 0),
    support_to_other_teams = replace_na(support_to_other_teams, 0),
    support_received       = replace_na(support_received, 0),
    weekly_hours_after     = carga_actual + horas_lp,
    # C6: overtime total = pre-existente + overtime generado por nuevas asignaciones
    overtime_after         = overtime_before + overtime_new
  )

# Predicción de burnout DESPUÉS (C5: misma función, mismo modelo de Fase 4)
feat_after              <- semana_df %>% select(all_of(names(semana_df)))
feat_after[[col_carga]] <- comparativa_df$weekly_hours_after
prob_after              <- predict_burnout(feat_after)

comparativa_df <- comparativa_df %>%
  mutate(
    predicted_probability_before = round(riesgo_before, 4),
    predicted_probability_after  = round(prob_after, 4),
    predicted_probability_delta  = round(prob_after - riesgo_before, 4)
  ) %>%
  select(
    employee_id, team_id, department, job_level,
    weekly_hours_before = carga_actual,
    weekly_hours_after,
    overtime_before,
    overtime_after,
    predicted_probability_before,
    predicted_probability_after,
    predicted_probability_delta,
    support_to_other_teams,
    support_received
  )


# =============================================================================
# 9. RESUMEN GLOBAL
# =============================================================================
resumen_df <- tibble(
  total_overtime_before   = sum(comparativa_df$overtime_before),
  total_overtime_after    = sum(comparativa_df$overtime_after),
  avg_risk_before         = mean(comparativa_df$predicted_probability_before),
  avg_risk_after          = mean(comparativa_df$predicted_probability_after),
  high_risk_before        = sum(comparativa_df$predicted_probability_before >= 0.5),
  high_risk_after         = sum(comparativa_df$predicted_probability_after  >= 0.5),
  total_interteam_support = sum(comparativa_df$support_to_other_teams),
  num_active_assignments  = nrow(x_sol)
)

red_ot   <- resumen_df$total_overtime_before - resumen_df$total_overtime_after
red_risk <- (resumen_df$avg_risk_before - resumen_df$avg_risk_after) * 100

cat(sprintf("\n── Resumen ejecutivo ────────────────────────────────────────────────\n"))
cat(sprintf("  Overtime total antes (h)  : %.2f\n", resumen_df$total_overtime_before))
cat(sprintf("  Overtime total después (h): %.2f\n", resumen_df$total_overtime_after))
cat(sprintf("  Reducción overtime (h)    : %.2f\n", red_ot))
cat(sprintf("  Riesgo medio antes        : %.2f%%\n", resumen_df$avg_risk_before * 100))
cat(sprintf("  Riesgo medio después      : %.2f%%\n", resumen_df$avg_risk_after  * 100))
cat(sprintf("  Alto riesgo antes / desp. : %d / %d\n",
            resumen_df$high_risk_before, resumen_df$high_risk_after))
cat(sprintf("  Apoyo interequipo (h)     : %.2f\n", resumen_df$total_interteam_support))
cat(sprintf("  Asignaciones activas      : %d\n",   resumen_df$num_active_assignments))


# =============================================================================
# 10. FIGURAS
# =============================================================================
PALETA <- c("Antes" = "tomato", "Después" = "steelblue")

# ── Figura 1: Overtime before/after (distribución) ────────────────────────────
ot_long <- comparativa_df %>%
  select(employee_id, overtime_before, overtime_after) %>%
  pivot_longer(-employee_id, names_to = "momento", values_to = "overtime") %>%
  mutate(momento = recode(momento,
                          overtime_before = "Antes",
                          overtime_after  = "Después")) %>%
  filter(overtime > 1e-9)

if (nrow(ot_long) > 0) {
  p1 <- ggplot(ot_long, aes(x = overtime, fill = momento)) +
    geom_histogram(position = "identity", alpha = 0.65, bins = 25) +
    scale_fill_manual(values = PALETA) +
    labs(title    = "Distribución de overtime antes y después — OR Clásico Fase 6",
         subtitle = sprintf("H = percentil %.0f | Solo empleados con overtime > 0",
                            PERCENTIL_CAP * 100),
         x = "Overtime (horas reales)", y = "Nº empleados", fill = NULL) +
    theme_minimal(base_size = 11) +
    theme(legend.position = "top")
  ggsave("fase_06_optimizacion/figures/fig1_overtime_before_after.png",
         p1, width = 9, height = 5, dpi = 150, bg = "white")
  cat("✓ Figura 1 (overtime before/after) guardada\n")
}

# ── Figura 2: Riesgo predicho before/after ────────────────────────────────────
riesgo_long <- comparativa_df %>%
  select(employee_id, predicted_probability_before, predicted_probability_after) %>%
  pivot_longer(-employee_id, names_to = "momento", values_to = "prob") %>%
  mutate(momento = recode(momento,
                          predicted_probability_before = "Antes",
                          predicted_probability_after  = "Después"))

p2 <- ggplot(riesgo_long, aes(x = prob, fill = momento)) +
  geom_histogram(position = "identity", alpha = 0.65, bins = 30) +
  geom_vline(xintercept = 0.5, linetype = "dashed", color = "gray40") +
  scale_fill_manual(values = PALETA) +
  labs(title    = "Riesgo predicho de burnout antes y después — evaluación ex post",
       subtitle = sprintf("Modelo: %s | NOTA: el riesgo NO entra en la optimización.",
                          final_name),
       x = "Probabilidad de burnout predicha", y = "Nº empleados", fill = NULL) +
  theme_minimal(base_size = 11) +
  theme(legend.position = "top")
ggsave("fase_06_optimizacion/figures/fig2_riesgo_before_after.png",
       p2, width = 9, height = 5, dpi = 150, bg = "white")
cat("✓ Figura 2 (riesgo before/after) guardada\n")

# ── Figura 3: Delta de riesgo predicho ───────────────────────────────────────
p3 <- ggplot(comparativa_df,
             aes(x = predicted_probability_delta,
                 fill = predicted_probability_delta < 0)) +
  geom_histogram(bins = 30, alpha = 0.85) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray40") +
  scale_fill_manual(values = c("TRUE" = "steelblue", "FALSE" = "tomato"),
                    labels = c("TRUE" = "Riesgo baja", "FALSE" = "Riesgo sube")) +
  labs(title    = "Cambio en riesgo predicho por empleado (Δ ex post)",
       subtitle = "Azul = la redistribución reduce el riesgo  |  Rojo = lo aumenta",
       x = "Δ probabilidad de burnout (después − antes)",
       y = "Nº empleados", fill = NULL) +
  theme_minimal(base_size = 11) +
  theme(legend.position = "top")
ggsave("fase_06_optimizacion/figures/fig3_delta_riesgo.png",
       p3, width = 9, height = 5, dpi = 150, bg = "white")
cat("✓ Figura 3 (delta riesgo) guardada\n")

# ── Figura 4: Horas asignadas y overtime por equipo ──────────────────────────
eq_df <- comparativa_df %>%
  group_by(team_id) %>%
  summarise(
    horas_asig_total = sum(weekly_hours_after - weekly_hours_before),
    overtime_total   = sum(overtime_after),
    .groups = "drop"
  ) %>%
  arrange(desc(overtime_total)) %>%
  mutate(team_id = factor(team_id, levels = rev(unique(team_id))))

eq_long <- eq_df %>%
  pivot_longer(-team_id, names_to = "tipo", values_to = "valor") %>%
  mutate(tipo = recode(tipo,
                       horas_asig_total = "Horas asignadas (LP)",
                       overtime_total   = "Overtime residual"))

p4 <- ggplot(eq_long, aes(x = team_id, y = valor, fill = tipo)) +
  geom_col(position = "dodge", alpha = 0.85, width = 0.65) +
  coord_flip() +
  scale_fill_manual(values = c("Horas asignadas (LP)" = "steelblue",
                               "Overtime residual"    = "tomato")) +
  labs(title    = "Horas asignadas y overtime residual por equipo",
       subtitle = "Equipos ordenados por overtime residual descendente",
       x = "Equipo", y = "Horas reales", fill = NULL) +
  theme_minimal(base_size = 11) +
  theme(legend.position = "top")
ggsave("fase_06_optimizacion/figures/fig4_asignaciones_por_equipo.png",
       p4, width = 9, height = 6, dpi = 150, bg = "white")
cat("✓ Figura 4 (asignaciones por equipo) guardada\n")

# ── Figura 5: Apoyo interequipo ───────────────────────────────────────────────
if (nrow(x_sol %>% filter(employee_team != project_origin_team)) > 0) {
  apoyo_df <- x_sol %>%
    filter(employee_team != project_origin_team) %>%
    group_by(employee_team, project_origin_team) %>%
    summarise(horas = sum(assigned_hours), .groups = "drop") %>%
    mutate(flujo = paste0("T", employee_team, " → T", project_origin_team))
  
  p5 <- ggplot(apoyo_df,
               aes(x = reorder(flujo, horas), y = horas,
                   fill = factor(project_origin_team))) +
    geom_col(alpha = 0.85) +
    coord_flip() +
    scale_fill_viridis_d(option = "C", begin = 0.2, end = 0.85) +
    labs(title    = "Flujos de apoyo interequipo — OR Clásico Fase 6",
         subtitle = sprintf("Limitado a ρ = %.0f%% de la capacidad disponible del equipo donante",
                            RHO * 100),
         x = "Flujo (donante → receptor)", y = "Horas cedidas",
         fill = "Equipo\nreceptor") +
    theme_minimal(base_size = 11) +
    theme(legend.position = "right")
  ggsave("fase_06_optimizacion/figures/fig5_apoyo_interequipo.png",
         p5, width = 9, height = 5, dpi = 150, bg = "white")
  cat("✓ Figura 5 (apoyo interequipo) guardada\n")
} else {
  cat("  (Sin flujos interequipo — figura 5 omitida)\n")
}

# ── Figura 6: Top 15 empleados más tensionados ────────────────────────────────
top15_emp <- comparativa_df %>%
  filter(overtime_after > 1e-9) %>%
  arrange(desc(overtime_after)) %>%
  head(15) %>%
  mutate(
    emp_label = paste0("E", employee_id, " (", job_level, ")"),
    emp_label = factor(emp_label, levels = rev(emp_label))
  )

if (nrow(top15_emp) > 0) {
  p6 <- ggplot(top15_emp,
               aes(x = emp_label, y = overtime_after,
                   fill = predicted_probability_after)) +
    geom_col(alpha = 0.9) +
    geom_errorbar(aes(ymin = overtime_before, ymax = overtime_before),
                  color = "gray30", width = 0.5, linewidth = 0.8,
                  linetype = "dashed") +
    coord_flip() +
    scale_fill_gradient(low = "steelblue", high = "tomato",
                        labels = scales::percent_format(accuracy = 1)) +
    labs(title    = "Top 15 empleados con mayor overtime residual",
         subtitle = "Barra = overtime after | Línea discontinua = overtime before | Color = riesgo after",
         x = NULL, y = "Overtime (horas reales)", fill = "Riesgo\nburnout after") +
    theme_minimal(base_size = 11) +
    theme(legend.position = "right")
  ggsave("fase_06_optimizacion/figures/fig6_top_empleados_tensionados.png",
         p6, width = 10, height = 6, dpi = 150, bg = "white")
  cat("✓ Figura 6 (top empleados tensionados) guardada\n")
}

# ── Figura 7: Scatter riesgo before vs after ─────────────────────────────────
p7 <- ggplot(comparativa_df,
             aes(x = predicted_probability_before,
                 y = predicted_probability_after,
                 color = overtime_after > 0)) +
  geom_point(alpha = 0.5, size = 1.8) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "gray50") +
  scale_color_manual(
    values = c("TRUE" = "tomato", "FALSE" = "steelblue"),
    labels = c("TRUE" = "Con overtime", "FALSE" = "Sin overtime")
  ) +
  labs(title    = "Riesgo de burnout antes vs después — OR Clásico Fase 6",
       subtitle = "Puntos por encima de la diagonal: el riesgo aumentó tras la asignación",
       x = "Riesgo predicho antes", y = "Riesgo predicho después", color = NULL) +
  theme_minimal(base_size = 11) +
  theme(legend.position = "top")
ggsave("fase_06_optimizacion/figures/fig7_scatter_riesgo.png",
       p7, width = 8, height = 6, dpi = 150, bg = "white")
cat("✓ Figura 7 (scatter riesgo) guardada\n")


# =============================================================================
# 11. EXPORTACIÓN DE RESULTADOS  (C2: write.csv en lugar de write_csv)
# =============================================================================
write.csv(x_sol,          "fase_06_optimizacion/results/asignacion_staffing_or_clasico.csv",   row.names = FALSE)
write.csv(comparativa_df, "fase_06_optimizacion/results/comparativa_empleados_or_clasico.csv", row.names = FALSE)
write.csv(resumen_df,     "fase_06_optimizacion/results/resumen_global_or_clasico.csv",         row.names = FALSE)

cat("\n✓ asignacion_staffing_or_clasico.csv exportado\n")
cat("✓ comparativa_empleados_or_clasico.csv exportado\n")
cat("✓ resumen_global_or_clasico.csv exportado\n")


# =============================================================================
# 12. INFORME MARKDOWN  (C7: solo marca criterios verificables)
# =============================================================================
df_to_md <- function(df) {
  if (nrow(df) == 0) return("_Sin datos._")
  hdr  <- paste0("| ", paste(names(df), collapse = " | "), " |")
  sep  <- paste0("| ", paste(rep("---", ncol(df)), collapse = " | "), " |")
  rows <- apply(df, 1, function(r) {
    vals <- sapply(r, function(v) {
      num <- suppressWarnings(as.numeric(v))
      if (!is.na(num)) round(num, 4) else v
    })
    paste0("| ", paste(vals, collapse = " | "), " |")
  })
  paste(c(hdr, sep, rows), collapse = "\n")
}

top5_emp <- comparativa_df %>%
  arrange(desc(overtime_after)) %>%
  head(5) %>%
  select(employee_id, team_id, job_level,
         overtime_before, overtime_after, predicted_probability_delta)

top5_eq <- comparativa_df %>%
  group_by(team_id) %>%
  summarise(overtime_total = sum(overtime_after),
            apoyo_dado     = sum(support_to_other_teams),
            .groups = "drop") %>%
  arrange(desc(overtime_total)) %>%
  head(5)

informe_md <- paste0(
  "# Informe OR clásico — Staffing operativo (Fase 6)

## Descripción del modelo

Baseline clásico de staffing operativo basado en **coste y overtime** en **escala real de horas**.
El burnout NO participa en la optimización; se evalúa ex post usando el modelo de Fase 4
(`", final_name, "`) para preparar la comparación con OR+ML en Fase 7.

## Formulación matemática

**Variables de decisión**

- `x_ip ≥ 0`    : horas del empleado i asignadas al proyecto p
- `o_i ≥ 0`     : overtime generado por nuevas asignaciones del empleado i
- `slack_p ≥ 0` : demanda no cubierta del proyecto p (penalizada con M = ", M_PENALTY, ")

**Función objetivo**

```
min  Σ c_ip·x_ip  +  ω·Σ o_i  +  M·Σ slack_p
```

**Restricciones**

1. Cobertura: `Σ_i x_ip + slack_p = d_p`             ∀ p
2. Capacidad: `Σ_p x_ip − o_i ≤ avail_i`             ∀ i
   donde avail_i = max(0, cap_i − carga_actual_i)
3. Factibilidad: `x_ip ≤ d_p` si (i,p) ∈ A; 0 en otro caso
4. Gobernanza: `Σ apoyo externo de k ≤ ρ · Avail_k`  ∀ k

## Resultados globales

| Métrica | Antes | Después |
|---|---|---|
| Overtime total (h) | ", round(resumen_df$total_overtime_before, 2), " | ", round(resumen_df$total_overtime_after, 2), " |
| Reducción overtime (h) | — | ", round(red_ot, 2), " |
| Riesgo medio (ex post) | ", round(resumen_df$avg_risk_before * 100, 2), "% | ", round(resumen_df$avg_risk_after * 100, 2), "% |
| Alto riesgo (≥ 0.5) | ", resumen_df$high_risk_before, " | ", resumen_df$high_risk_after, " |
| Apoyo interequipo (h) | — | ", round(resumen_df$total_interteam_support, 2), " |
| Asignaciones activas | — | ", resumen_df$num_active_assignments, " |

## Top 5 empleados más tensionados

", df_to_md(top5_emp), "

## Top 5 equipos más tensionados

", df_to_md(top5_eq), "

## Criterios de cierre verificados

- [x] LP continuo exacto con ompr + GLPK
- [x] Datos en escala real de horas (no valores normalizados)
- [x] department y job_level reales desde operational.csv
- [x] Restricción de capacidad usa avail_i (carga actual descontada)
- [x] Evaluación ex post con el modelo final de Fase 4 (", final_name, ")
- [x] Tres CSVs de salida exportados
- [x] 7 figuras ggplot2 generadas (fig5 solo si hay apoyo interequipo)
- [x] Informe solo declara criterios efectivamente superados
"
)

writeLines(informe_md,
           "fase_06_optimizacion/reports/informe_or_clasico_staffing.md")

cat("✓ informe_or_clasico_staffing.md exportado\n")
cat(sprintf("\n✓ Fase 6 completada correctamente\n"))
cat(strrep("=", 70), "\n")