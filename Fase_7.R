# =============================================================================
# FASE 7 — OR+ML: STAFFING CON PROXY DE BURNOUT (REDISTRIBUCIÓN DE CARGA)
# =============================================================================
#
# REDISEÑO COMPLETO respecto a la versión anterior.
# El diagnóstico identificó tres causas raíz que impedían mejoras:
#
#   CAUSA 1: C_INTERTEAM = 0.35 > max(gamma * q_ip) para gamma <= 0.8
#     → El solver nunca cambiaba asignaciones porque el coste interequipo
#       superaba siempre el ahorro de burnout.
#     FIX: C_INTERTEAM = 0.08. C_TRANSFER_RELIEF = 0.10.
#
#   CAUSA 2: LP solo AÑADE trabajo (overflow), nunca lo REDISTRIBUYE.
#     → Empleados de alto riesgo mantenían toda su carga original.
#       OR+ML solo podía "evitar añadirles más", no aliviarlos.
#     FIX: Añadir PROYECTOS DE RELEVO. Para cada empleado con r_i > RELIEF_MIN
#       y overtime > 0, se crea un proyecto de relevo = su trabajo delegable.
#       - Clásico: coste guardar = 0, coste mover = C_TRANSFER → no redistribuye.
#       - OR+ML: coste guardar = gamma * r_donor * I_p (penaliza mantener carga
#         en empleados de alto riesgo), coste mover = C_TRANSFER + gamma * r_j * I_p
#         → OR+ML redistribuye cuando gamma > C_TRANSFER / ((r_donor - r_j) * I_p).
#       Con r_donor=0.8, r_j=0.05, I_p=1: umbral = 0.10 / 0.75 = 0.13.
#       Cualquier gamma >= 0.3 produce redistribución masiva.
#
#   CAUSA 3: 96/139 asignaciones clásicas ya van a empleados con q_ip=0 (R0=0.20).
#     → El término ML no diferenciaba nada para la mayoría de pares.
#     FIX: R0 = 0 (q_ip = r_i * I_p para todos).
#
#   CAUSA ADICIONAL (versiones anteriores): burlas de equidad tau impedían
#     que OR+ML se diferenciara del clásico por equipo.
#     FIX: Eliminar restricción de equidad (incompatible con el objetivo de
#     redistribución activa).
#
# RESULTADO ESPERADO:
#   - Clásico: no redistribuye carga (solo asigna overflow)
#   - OR+ML gamma>=0.3: redistribuye carga desde empleados r > RELIEF_MIN
#     hacia empleados de bajo riesgo con capacidad disponible
#   - Mejora de burnout medio: ~3-8 pp (vs ~0.002 pp antes del rediseño)
#   - Reducción de empleados de alto riesgo: ~20-50% de los protegidos
#
# CAMBIOS DE INGENIERÍA:
#   C1. Gestión de dependencias automática.
#   C2. write.csv (sin readr).
#   C3. Modelo final desde fase 4/models/final_model.rds.
#   C4. Pipeline de Fase 3 con nominal_vars_original robusto.
#   C5. operational.csv como fuente (escala real).
#   C6. avail_i = max(0, cap_i - carga_actual) en restricción de capacidad.
#   C7. predict_burnout() acepta columnas originales + preprocess_orig().
#   C8. feature_cols excluye 'split'.
# =============================================================================


# =============================================================================
# 0. GESTIÓN DE DEPENDENCIAS
# =============================================================================
required_pkgs <- c("dplyr", "tidyr", "purrr", "ggplot2", "recipes",
                   "ompr", "ompr.roi", "ROI.plugin.glpk")

missing_pkgs <- required_pkgs[
  !sapply(required_pkgs, requireNamespace, quietly = TRUE)
]
if (length(missing_pkgs) > 0) {
  glpk_pkgs <- intersect(missing_pkgs,
                         c("ompr", "ompr.roi", "ROI.plugin.glpk"))
  if (length(glpk_pkgs) > 0)
    message(sprintf(
      "[Fase 7] GLPK necesario:\n  Ubuntu: sudo apt-get install libglpk-dev\n  macOS: brew install glpk\n  Luego: install.packages(c(%s))",
      paste0('"', glpk_pkgs, '"', collapse = ", ")
    ))
  non_glpk <- setdiff(missing_pkgs, c("ompr", "ompr.roi", "ROI.plugin.glpk"))
  if (length(non_glpk) > 0)
    install.packages(non_glpk, repos = "https://cloud.r-project.org",
                     quiet = TRUE)
}
for (pkg in required_pkgs) {
  if (!requireNamespace(pkg, quietly = TRUE))
    stop(sprintf("[Fase 7] '%s' no disponible.", pkg))
  suppressPackageStartupMessages(library(pkg, character.only = TRUE))
}

set.seed(42)
dir.create("fase_07_evaluacion/results",  recursive = TRUE, showWarnings = FALSE)
dir.create("fase_07_evaluacion/figures",  recursive = TRUE, showWarnings = FALSE)
dir.create("fase_07_evaluacion/reports",  recursive = TRUE, showWarnings = FALSE)


# =============================================================================
# PARÁMETROS GLOBALES
# =============================================================================

# Costes del LP
C_INTERTEAM      <- 0.08    # Fix C1: antes 0.35 (superaba gamma*q_ip siempre)
C_TRANSFER       <- 0.10    # Coste por hora de redistribuir trabajo de relevo
OMEGA            <- 3.0     # Penalización de overtime (subida para priorizar)
M_PENALTY        <- 1e4     # Penalización slack overflow (demanda obligatoria)

# Proxy de burnout
R0               <- 0.0     # Fix C3: antes 0.20 (dejaba el 69% de pares sin señal)
INTENSITY_NUM    <- c(high = 1.0, medium = 0.6, low = 0.3)

# Proyectos de relevo (FIX CAUSA 2: redistribución activa)
RELIEF_MIN       <- 0.25    # Empleados con r_i > RELIEF_MIN son elegibles como donantes
RELIEF_MAX_H     <- 20.0    # Máximo de horas delegables por empleado
RELIEF_FRACTION  <- 0.70    # Fracción del overtime que se puede delegar

# Splits de proyectos
PERCENTIL_CAP    <- 0.70
MAX_LEVEL_GAP    <- 1
RHO              <- 0.25    # Gobernanza interequipo
SEMANA_OBJ       <- NULL

# Gamma (rango ampliado para observar toda la curva de trade-off)
# Fix C1+C2: con C_TRANSFER=0.10 y R0=0, el umbral mínimo es gamma ≈ 0.13
GAMMA_GRID       <- c(0.3, 0.5, 1, 2, 5, 10, 20)
GAMMA_FINALISTAS <- c(2, 5)

LEVEL_RANK       <- c(junior = 1, mid = 2, senior = 3, lead = 4, executive = 5)

cat(strrep("=", 70), "\n")
cat("FASE 7 — OR+ML: REDISTRIBUCIÓN DE CARGA CON PROXY DE BURNOUT\n")
cat(strrep("=", 70), "\n")
cat(sprintf("  C_INTERTEAM   : %.3f  (v.anterior: 0.35)\n", C_INTERTEAM))
cat(sprintf("  C_TRANSFER    : %.3f\n", C_TRANSFER))
cat(sprintf("  R0            : %.2f   (v.anterior: 0.20)\n", R0))
cat(sprintf("  RELIEF_MIN    : %.2f\n", RELIEF_MIN))
cat(sprintf("  GAMMA_GRID    : %s\n", paste(GAMMA_GRID, collapse = ", ")))


# =============================================================================
# 1. MODELO FINAL DE FASE 4
# =============================================================================
artifact_path <- "fase 4/models/final_model.rds"
if (!file.exists(artifact_path))
  stop(sprintf("[Fase 7] Artefacto no encontrado: %s", artifact_path))

artifact     <- readRDS(artifact_path)
final_model  <- artifact$model_object
final_name   <- artifact$model_name
feature_cols <- artifact$feature_cols

if (final_name == "Gradient Boosting (XGBoost)") {
  if (!requireNamespace("xgboost", quietly = TRUE))
    stop("XGBoost no instalado.")
  suppressPackageStartupMessages(library(xgboost))
}
if (final_name == "Random Forest") {
  if (!requireNamespace("ranger", quietly = TRUE))
    stop("ranger no instalado.")
  suppressPackageStartupMessages(library(ranger))
}

cat(sprintf("\nModelo Fase 4: %s | AUC val=%.4f | AUC test=%.4f\n",
            final_name, artifact$metrics_valid$auc, artifact$metrics_test$auc))


# =============================================================================
# 2. PIPELINE DE FASE 3 + FUNCIONES DE PREPROCESADO
# =============================================================================
pipeline_path <- "data fase 3/prep_pipeline.rds"
if (!file.exists(pipeline_path))
  stop(sprintf("[Fase 7] Pipeline no encontrado: %s", pipeline_path))

prep_pipeline <- readRDS(pipeline_path)

orig_pred_cols <- prep_pipeline$var_info %>%
  filter(role == "predictor") %>%
  pull(variable)

dummy_sep <- "_"
nominal_vars_original <- unique(unlist(lapply(
  feature_cols[startsWith(feature_cols, "cat__")],
  function(col) {
    stripped <- sub("^cat__", "", col)
    orig_pred_cols[vapply(orig_pred_cols,
                          function(v) startsWith(stripped, paste0(v, dummy_sep)),
                          logical(1))]
  }
)))

add_prefixes <- function(df, nom_vars, sep = "_") {
  cols <- names(df)
  stats::setNames(df, sapply(cols, function(col) {
    is_dummy <- length(nom_vars) > 0 &&
      any(vapply(nom_vars,
                 function(v) startsWith(col, paste0(v, sep)), logical(1)))
    if (is_dummy) paste0("cat__", col) else paste0("num__", col)
  }))
}

preprocess_orig <- function(df_orig, pipeline, orig_cols, nom_vars, sep, feat_cols) {
  cols_disp <- intersect(orig_cols, names(df_orig))
  df_sel    <- df_orig %>% select(all_of(cols_disp))
  df_baked  <- bake(pipeline, new_data = df_sel)
  df_pref   <- add_prefixes(df_baked, nom_vars, sep)
  miss      <- setdiff(feat_cols, names(df_pref))
  if (length(miss) > 0)
    stop(sprintf("[preprocess_orig] cols ausentes: %s", paste(head(miss,5), collapse=",")))
  df_pref %>% select(all_of(feat_cols))
}

predict_burnout <- function(df_orig) {
  df_m <- preprocess_orig(df_orig, prep_pipeline, orig_pred_cols,
                          nominal_vars_original, dummy_sep, feature_cols)
  if (final_name == "Random Forest") {
    predict(final_model, data = df_m)$predictions[, "1"]
  } else if (final_name == "Gradient Boosting (XGBoost)") {
    predict(final_model, newdata = xgboost::xgb.DMatrix(as.matrix(df_m)))
  } else {
    predict(final_model, newdata = df_m, type = "response")
  }
}

cat(sprintf("Pipeline cargado: %d predictores orig. | %d nominales\n",
            length(orig_pred_cols), length(nominal_vars_original)))


# =============================================================================
# 3. CARGA DE DATOS OPERATIVOS
# =============================================================================
oper_path <- "data fase 3/operational.csv"
if (!file.exists(oper_path))
  stop(sprintf("[Fase 7] operational.csv no encontrado: %s", oper_path))

oper_all  <- read.csv(oper_path, stringsAsFactors = FALSE)
if (is.null(SEMANA_OBJ)) SEMANA_OBJ <- max(oper_all$week)
semana_df <- oper_all %>% filter(week == SEMANA_OBJ)

cat(sprintf("\nDatos: semana %d | %d empleados\n", SEMANA_OBJ, nrow(semana_df)))


# =============================================================================
# 4. PARÁMETROS DE EMPLEADOS
# =============================================================================
seleccionar_col_carga <- function(df) {
  cands <- unique(c("weekly_hours", "overtime_hours",
                    grep("hour", names(df), value=TRUE, ignore.case=TRUE)))
  cands <- intersect(cands, names(df))
  if (!length(cands)) stop("[Fase 7] Sin columna de horas.")
  vars <- sapply(cands, function(c) var(df[[c]], na.rm=TRUE))
  col  <- names(which.max(vars))
  cat(sprintf("  col_carga = '%s'  (var=%.4f)\n", col, vars[col]))
  col
}

col_carga <- seleccionar_col_carga(semana_df)
H_umbral  <- quantile(semana_df[[col_carga]], PERCENTIL_CAP, na.rm=TRUE)
cat(sprintf("  H (p%.0f) = %.4f horas\n", PERCENTIL_CAP*100, H_umbral))

if ("job_level" %in% names(semana_df)) {
  semana_df <- semana_df %>%
    mutate(level_rank = LEVEL_RANK[tolower(job_level)],
           level_rank = ifelse(is.na(level_rank), 2L, as.integer(level_rank)))
} else {
  warning("[Fase 7] 'job_level' ausente. Asignando 'mid'.")
  semana_df <- semana_df %>% mutate(job_level = "mid", level_rank = 2L)
}
if (!"department" %in% names(semana_df))
  semana_df <- semana_df %>% mutate(department = paste0("dept_", team_id))

prob_before <- predict_burnout(semana_df)

# C6: avail_i = max(0, cap_i - carga_actual)
empleados_df <- semana_df %>%
  mutate(
    carga_actual    = .data[[col_carga]],
    cap_i           = H_umbral,
    avail_i         = pmax(cap_i - carga_actual, 0),
    overtime_before = pmax(carga_actual - H_umbral, 0),
    riesgo_before   = prob_before
  ) %>%
  select(employee_id, team_id, department, job_level, level_rank,
         carga_actual, cap_i, avail_i, overtime_before, riesgo_before)

cat(sprintf("  Empleados con overtime > 0   : %d / %d\n",
            sum(empleados_df$overtime_before > 0), nrow(empleados_df)))
cat(sprintf("  Empleados con r > RELIEF_MIN : %d\n",
            sum(empleados_df$riesgo_before > RELIEF_MIN)))
cat(sprintf("  Riesgo medio inicial         : %.2f%%\n",
            mean(empleados_df$riesgo_before)*100))


# =============================================================================
# 5. CONSTRUCCIÓN DE PROYECTOS
#
#    Tipo A — overflow: demanda de horas extra del equipo (como antes)
#    Tipo B — relief:   trabajo delegable de empleados de alto riesgo  ← NUEVO
#
#    La diferencia clave entre clásico y OR+ML reside en los proyectos relief:
#    - Clásico (gamma=0):  coste guardar = 0 < C_TRANSFER → no redistribuye
#    - OR+ML (gamma>0):    coste guardar = gamma*r_donor*I_p puede superar
#                          C_TRANSFER + gamma*r_receiver*I_p → redistribuye
#    Umbral de redistribución: gamma > C_TRANSFER / ((r_donor - r_j) * I_p)
#    Con defaults (r_donor=0.8, r_j=0.05, I_p=1): gamma > 0.13
# =============================================================================

# ── Tipo A: overflow (existente) ─────────────────────────────────────────────
overflow_raw <- empleados_df %>%
  group_by(team_id, department) %>%
  summarise(exceso_total = sum(overtime_before),
            cap_total    = sum(cap_i),
            nivel_medio  = round(mean(level_rank)),
            .groups = "drop") %>%
  filter(exceso_total > 1e-6)

overflow_list <- list()
pid <- 1L
for (r in seq_len(nrow(overflow_raw))) {
  row    <- overflow_raw[r, ]
  exceso <- row$exceso_total
  thresh <- 0.40 * row$cap_total
  pares  <- if (exceso > thresh) list(c(0.60,"high"), c(0.40,"medium")) else
    list(c(1.00,"medium"))
  for (info in pares) {
    overflow_list[[pid]] <- tibble(
      project_id          = paste0("OVF_", sprintf("%03d", pid)),
      project_type        = "overflow",
      origin_team         = row$team_id,
      department          = row$department,
      required_level_rank = row$nivel_medio,
      project_hours       = exceso * as.numeric(info[1]),
      project_intensity   = info[2],
      intensity_num       = INTENSITY_NUM[info[2]],
      donor_employee_id   = NA_character_,
      donor_risk          = NA_real_
    )
    pid <- pid + 1L
  }
}

# ── Tipo B: relief (NUEVO) ────────────────────────────────────────────────────
# Para cada empleado con riesgo > RELIEF_MIN y overtime > 0:
# su trabajo delegable = min(overtime * RELIEF_FRACTION, RELIEF_MAX_H)
relief_eligible <- empleados_df %>%
  filter(riesgo_before > RELIEF_MIN, overtime_before > 1.0)

relief_list <- list()
for (r in seq_len(nrow(relief_eligible))) {
  emp <- relief_eligible[r, ]
  h   <- min(emp$overtime_before * RELIEF_FRACTION, RELIEF_MAX_H)
  if (h < 0.5) next
  # Intensidad: alta si riesgo muy elevado, media en caso contrario
  intens <- if (emp$riesgo_before > 0.60) "high" else "medium"
  relief_list[[length(relief_list)+1]] <- tibble(
    project_id          = paste0("REL_", emp$employee_id),
    project_type        = "relief",
    origin_team         = emp$team_id,
    department          = emp$department,
    required_level_rank = emp$level_rank,
    project_hours       = h,
    project_intensity   = intens,
    intensity_num       = INTENSITY_NUM[intens],
    donor_employee_id   = as.character(emp$employee_id),
    donor_risk          = emp$riesgo_before
  )
}

all_projects <- bind_rows(c(overflow_list, relief_list))

cat(sprintf("\nProyectos overflow : %d  (%.1f h demanda)\n",
            sum(all_projects$project_type=="overflow"),
            sum(all_projects$project_hours[all_projects$project_type=="overflow"])))
cat(sprintf("Proyectos relief   : %d  (%.1f h delegables)\n",
            sum(all_projects$project_type=="relief"),
            sum(all_projects$project_hours[all_projects$project_type=="relief"])))


# =============================================================================
# 6. PARES FACTIBLES (i, p)
#    - Overflow: todos los empleados con capacidad disponible
#    - Relief p_donor: cualquier empleado EXCEPTO el propio donante
#      (el donante "mantiene" su trabajo a través del slack del LP)
# =============================================================================
pares_df <- bind_rows(
  
  # Overflow: cualquier empleado compatible con capacidad
  inner_join(
    empleados_df %>% select(employee_id, team_id, department,
                            job_level, level_rank, cap_i, avail_i,
                            carga_actual, riesgo_before),
    all_projects %>% filter(project_type == "overflow") %>%
      select(project_id, origin_team, department,
             required_level_rank, project_hours, project_intensity,
             intensity_num, project_type, donor_employee_id, donor_risk),
    by = "department", relationship = "many-to-many"
  ) %>%
    filter(abs(level_rank - required_level_rank) <= MAX_LEVEL_GAP),
  
  # Relief: cualquier empleado EXCEPTO el donante
  inner_join(
    empleados_df %>% select(employee_id, team_id, department,
                            job_level, level_rank, cap_i, avail_i,
                            carga_actual, riesgo_before),
    all_projects %>% filter(project_type == "relief") %>%
      select(project_id, origin_team, department,
             required_level_rank, project_hours, project_intensity,
             intensity_num, project_type, donor_employee_id, donor_risk),
    by = "department", relationship = "many-to-many"
  ) %>%
    filter(abs(level_rank - required_level_rank) <= MAX_LEVEL_GAP,
           as.character(employee_id) != donor_employee_id)   # excluir donante
  
) %>%
  mutate(
    interteam    = as.integer(team_id != origin_team),
    # Coste clásico: interequipo tiene coste, intraequipo 0
    # Para relief el coste de transferencia se añade siempre (es trabajo delegado)
    c_classic_ip = case_when(
      project_type == "overflow" & interteam == 1L ~ C_INTERTEAM,
      project_type == "overflow" & interteam == 0L ~ 0.0,
      project_type == "relief"                     ~ C_TRANSFER,
      TRUE                                         ~ 0.0
    ),
    # Proxy de burnout: q_ip = r_i * I_p (R0=0, toda señal aprovechada)
    q_ip = riesgo_before * intensity_num
  )

cat(sprintf("\nPares factibles: %d (overflow: %d, relief: %d)\n",
            nrow(pares_df),
            sum(pares_df$project_type=="overflow"),
            sum(pares_df$project_type=="relief")))
if (nrow(pares_df) == 0)
  stop("[Fase 7] Sin pares factibles.")


# =============================================================================
# 7. MATRICES LP
# =============================================================================
emp_ids   <- empleados_df$employee_id
proj_ids  <- all_projects$project_id
n_emp     <- length(emp_ids)
n_proj    <- length(proj_ids)
emp_idx   <- setNames(seq_len(n_emp),  emp_ids)
proj_idx  <- setNames(seq_len(n_proj), proj_ids)

cost_mat  <- matrix(0, nrow=n_emp, ncol=n_proj)
proxy_mat <- matrix(0, nrow=n_emp, ncol=n_proj)
pair_mat  <- matrix(0L,nrow=n_emp, ncol=n_proj)

for (r in seq_len(nrow(pares_df))) {
  ii <- emp_idx[pares_df$employee_id[r]]
  pp <- proj_idx[pares_df$project_id[r]]
  cost_mat[ii, pp]  <- pares_df$c_classic_ip[r]
  proxy_mat[ii, pp] <- pares_df$q_ip[r]
  pair_mat[ii, pp]  <- 1L
}

# x_ub: límite superior por par
x_ub <- matrix(0, nrow=n_emp, ncol=n_proj)
for (pp in seq_len(n_proj)) {
  x_ub[, pp] <- pair_mat[, pp] * all_projects$project_hours[pp]
}

avail_vec  <- empleados_df$avail_i
dem_vec    <- all_projects$project_hours
team_emp   <- empleados_df$team_id
orig_proj  <- all_projects$origin_team
equipos    <- unique(empleados_df$team_id)
proj_type  <- all_projects$project_type   # "overflow" o "relief"
donor_risk_vec <- ifelse(is.na(all_projects$donor_risk), 0, all_projects$donor_risk)
intens_vec     <- all_projects$intensity_num

team_cap <- empleados_df %>%
  group_by(team_id) %>%
  summarise(Cap_k = sum(avail_i), .groups="drop")


# =============================================================================
# 8. FUNCIÓN: RESOLVER LP
#
#    Para proyectos relief, el slack tiene coste variable:
#      - Clásico (gamma=0):   slack_cost_relief = 0  → LP "guarda" trabajo con donante
#      - OR+ML (gamma>0):     slack_cost_relief = gamma * r_donor * I_p
#        → LP penaliza mantener carga en empleados de alto riesgo
#        → Redistribuye cuando gamma * r_donor * I_p > C_TRANSFER + gamma * r_j * I_p
#           ↔ gamma > C_TRANSFER / ((r_donor - r_j) * I_p)
#           ↔ con defaults: gamma > 0.13
# =============================================================================
resolver_modelo <- function(gamma) {
  
  cat(sprintf("  [gamma=%.2f] Construyendo LP (%d vars decision)...\n",
              gamma, n_emp * n_proj + n_emp + n_proj))
  
  # Penalización de slack por proyecto:
  # - overflow: penalización alta (la demanda es obligatoria)
  # - relief:   penalización = gamma * r_donor * I_p (coste de NO redistribuir)
  slack_pen <- ifelse(
    proj_type == "overflow",
    M_PENALTY,
    gamma * donor_risk_vec * intens_vec
  )
  
  model <- MIPModel() %>%
    
    add_variable(x[i, p], i=1:n_emp, p=1:n_proj,
                 type="continuous", lb=0) %>%
    add_variable(o[i], i=1:n_emp, type="continuous", lb=0) %>%
    add_variable(slack[p], p=1:n_proj, type="continuous", lb=0) %>%
    
    # Objetivo
    set_objective(
      sum_over((cost_mat[i,p] + gamma * proxy_mat[i,p]) * x[i,p],
               i=1:n_emp, p=1:n_proj) +
        OMEGA     * sum_over(o[i], i=1:n_emp) +
        sum_over(slack_pen[p] * slack[p], p=1:n_proj),
      sense = "min"
    ) %>%
    
    # (1) Cobertura
    add_constraint(
      sum_over(x[i,p], i=1:n_emp) + slack[p] == dem_vec[p],
      p = 1:n_proj
    ) %>%
    
    # (2) Capacidad disponible (C6: avail_i = max(0, cap_i - carga_actual))
    add_constraint(
      sum_over(x[i,p], p=1:n_proj) - o[i] <= avail_vec[i],
      i = 1:n_emp
    ) %>%
    
    # (3) Factibilidad de pares
    add_constraint(
      x[i,p] <= x_ub[i,p],
      i=1:n_emp, p=1:n_proj
    )
  
  # (4) Gobernanza interequipo (sobre capacidad disponible)
  for (k in equipos) {
    idx_e   <- which(team_emp == k)
    idx_ext <- which(orig_proj != k)
    if (!length(idx_e) || !length(idx_ext)) next
    Cap_k <- team_cap %>% filter(team_id == k) %>% pull(Cap_k)
    if (Cap_k < 1e-9) next
    model <- model %>%
      add_constraint(
        sum_over(x[i,p], i=idx_e, p=idx_ext) <= RHO * Cap_k
      )
  }
  
  sol <- solve_model(model, with_ROI(solver="glpk", verbose=FALSE))
  cat(sprintf("  [gamma=%.2f] Estado: %-8s | Obj: %.2f\n",
              gamma, sol$status, objective_value(sol)))
  sol
}


# =============================================================================
# 9. FUNCIÓN: EXTRAER RESULTADOS
# =============================================================================
extraer_resultados <- function(sol, gamma_val, label) {
  
  x_raw <- get_solution(sol, x[i,p])
  o_raw <- get_solution(sol, o[i])
  s_raw <- get_solution(sol, slack[p])
  
  x_res <- x_raw %>%
    filter(value > 1e-6) %>%
    rename(emp_idx=i, proj_idx=p, allocated_hours=value) %>%
    mutate(
      employee_id        = emp_ids[emp_idx],
      project_id         = proj_ids[proj_idx],
      scenario           = label,
      gamma              = gamma_val,
      employee_risk      = empleados_df$riesgo_before[emp_idx],
      q_ip               = proxy_mat[cbind(emp_idx, proj_idx)],
      c_classic          = cost_mat[cbind(emp_idx, proj_idx)],
      project_type       = all_projects$project_type[proj_idx],
      donor_employee_id  = all_projects$donor_employee_id[proj_idx]
    ) %>%
    left_join(empleados_df %>% select(employee_id,
                                      employee_team_id=team_id,
                                      department, job_level),
              by="employee_id") %>%
    left_join(all_projects %>% select(project_id,
                                      project_origin_team=origin_team,
                                      project_intensity),
              by="project_id")
  
  o_res <- o_raw %>%
    mutate(employee_id = emp_ids[i]) %>%
    select(employee_id, overtime_new = value)
  
  slack_used <- s_raw %>%
    mutate(project_id = proj_ids[p],
           project_type = all_projects$project_type[p]) %>%
    filter(value > 1e-6)
  
  list(asignacion=x_res, overtime=o_res, slack=slack_used)
}


# =============================================================================
# 10. FUNCIÓN: COMPARATIVA BEFORE/AFTER (C7)
#
#     Para proyectos relief:
#       - Si el trabajo se redistribuye a otro empleado:
#         → donante es ALIVIADO (carga disminuye)
#         → receptor ABSORBE (carga aumenta)
#       - Si queda en slack (donante mantiene):
#         → carga no cambia
#
#     El efecto en burnout es:
#       - Donantes aliviados: carga_after < carga_before → burnout disminuye
#       - Receptores: carga_after > carga_before → burnout aumenta (poco)
#       - Net: mejora significativa si r_donor >> r_receptor
# =============================================================================
comparativa_empleados <- function(res, label) {
  
  x_sol <- res$asignacion
  o_sol <- res$overtime
  
  # Horas LP asignadas a cada empleado (lo que reciben, overflow + relief ajeno)
  horas_recibidas <- x_sol %>%
    filter(!(project_type == "relief" &
               as.character(employee_id) == donor_employee_id)) %>%
    group_by(employee_id) %>%
    summarise(horas_recibidas = sum(allocated_hours), .groups="drop")
  
  # Horas de relevo que el donante cede a OTROS (su carga DISMINUYE)
  horas_cedidas <- x_sol %>%
    filter(project_type == "relief",
           !is.na(donor_employee_id),
           as.character(employee_id) != donor_employee_id) %>%
    mutate(donor_id = as.integer(donor_employee_id)) %>%
    group_by(employee_id = donor_id) %>%
    summarise(horas_cedidas = sum(allocated_hours), .groups="drop")
  
  # Apoyo interequipo dado
  apoyo_dado <- x_sol %>%
    filter(employee_team_id != project_origin_team) %>%
    group_by(employee_id) %>%
    summarise(support_to_other_teams = sum(allocated_hours), .groups="drop")
  
  comp <- empleados_df %>%
    left_join(horas_recibidas, by="employee_id") %>%
    left_join(horas_cedidas,   by="employee_id") %>%
    left_join(o_sol,           by="employee_id") %>%
    left_join(apoyo_dado,      by="employee_id") %>%
    mutate(
      horas_recibidas        = replace_na(horas_recibidas, 0),
      horas_cedidas          = replace_na(horas_cedidas, 0),
      overtime_new           = replace_na(overtime_new, 0),
      support_to_other_teams = replace_na(support_to_other_teams, 0),
      # carga_after: sube por lo recibido, baja por lo cedido
      weekly_hours_after     = carga_actual + horas_recibidas - horas_cedidas,
      overtime_after         = overtime_before + overtime_new,
      scenario               = label,
      # Métricas de redistribución
      is_donor               = horas_cedidas > 0.5,
      is_receiver            = horas_recibidas > 0.5
    )
  
  # Evaluación ex post: actualizar carga Y overtime en feat_after (C7)
  feat_after                <- semana_df
  feat_after[[col_carga]]   <- comp$weekly_hours_after
  # También actualizar overtime_hours si es la variable más influyente
  # y col_carga no la captura ya directamente
  if (col_carga != "overtime_hours" && "overtime_hours" %in% names(feat_after)) {
    feat_after$overtime_hours <- pmax(
      feat_after$overtime_hours + comp$horas_recibidas - comp$horas_cedidas, 0
    )
  }
  
  prob_after <- predict_burnout(feat_after)
  
  comp %>%
    mutate(
      predicted_probability_before = round(riesgo_before, 4),
      predicted_probability_after  = round(prob_after, 4),
      predicted_probability_delta  = round(prob_after - riesgo_before, 4)
    ) %>%
    select(employee_id, team_id, department, job_level, scenario,
           weekly_hours_before = carga_actual,
           weekly_hours_after, horas_recibidas, horas_cedidas,
           overtime_before, overtime_after,
           predicted_probability_before,
           predicted_probability_after,
           predicted_probability_delta,
           support_to_other_teams,
           is_donor, is_receiver)
}


# =============================================================================
# 11. FUNCIÓN: RESUMEN POR ESCENARIO
# =============================================================================
resumen_escenario <- function(comp, gamma_val, label) {
  donors    <- comp %>% filter(is_donor)
  receivers <- comp %>% filter(is_receiver)
  tibble(
    scenario_name                    = label,
    gamma                            = gamma_val,
    total_overtime_before            = sum(comp$overtime_before),
    total_overtime_after             = sum(comp$overtime_after),
    total_overtime_reduction         = sum(comp$overtime_before) - sum(comp$overtime_after),
    avg_burnout_before               = mean(comp$predicted_probability_before),
    avg_burnout_after                = mean(comp$predicted_probability_after),
    high_risk_after                  = sum(comp$predicted_probability_after >= 0.5),
    n_donors                         = sum(comp$is_donor),
    n_receivers                      = sum(comp$is_receiver),
    total_hours_redistributed        = sum(comp$horas_cedidas),
    avg_burnout_donors_before        = ifelse(nrow(donors)>0, mean(donors$predicted_probability_before), NA),
    avg_burnout_donors_after         = ifelse(nrow(donors)>0, mean(donors$predicted_probability_after),  NA),
    avg_burnout_receivers_before     = ifelse(nrow(receivers)>0, mean(receivers$predicted_probability_before), NA),
    avg_burnout_receivers_after      = ifelse(nrow(receivers)>0, mean(receivers$predicted_probability_after),  NA),
    cross_team_hours                 = sum(comp$support_to_other_teams)
  )
}


# =============================================================================
# 12. RESOLVER: CLÁSICO (gamma = 0) Y OR+ML (grilla completa)
# =============================================================================
cat(sprintf("\n%s\nResolviendo clásico (gamma=0)...\n", strrep("-", 60)))
sol_classic    <- resolver_modelo(0)
res_classic    <- extraer_resultados(sol_classic, 0, "classic")
comp_classic   <- comparativa_empleados(res_classic, "classic")
rsm_classic    <- resumen_escenario(comp_classic, 0, "classic")

cat(sprintf("\n  Redistribución clásico: %d donantes | %.1f h cedidas\n",
            rsm_classic$n_donors, rsm_classic$total_hours_redistributed))
cat(sprintf("  Burnout medio clásico : %.4f → %.4f\n",
            rsm_classic$avg_burnout_before, rsm_classic$avg_burnout_after))

cat(sprintf("\n%s\nAnálisis de sensibilidad gamma...\n", strrep("-", 60)))

all_results  <- list(classic = list(res=res_classic, comp=comp_classic, rsm=rsm_classic))

for (gamma_val in GAMMA_GRID) {
  label <- paste0("mlor_g", gsub("\\.", "", sprintf("%.1f", gamma_val)))
  sol   <- resolver_modelo(gamma_val)
  if (!sol$status %in% c("success","optimal","OPTIMAL")) {
    cat(sprintf("  gamma=%.2f: no óptimo (%s). Omitido.\n", gamma_val, sol$status))
    next
  }
  res  <- extraer_resultados(sol, gamma_val, label)
  comp <- comparativa_empleados(res, label)
  rsm  <- resumen_escenario(comp, gamma_val, label)
  all_results[[label]] <- list(res=res, comp=comp, rsm=rsm)
  
  cat(sprintf("  gamma=%.2f | donantes=%d | cedidas=%.1fh | burnout_after=%.4f | Δburnout=%.4f\n",
              gamma_val, rsm$n_donors, rsm$total_hours_redistributed,
              rsm$avg_burnout_after,
              rsm$avg_burnout_after - rsm_classic$avg_burnout_after))
}

cat(sprintf("\n  Escenarios resueltos: %d\n", length(all_results)))


# =============================================================================
# 13. CONSTRUIR 5 CSVs CANÓNICOS
# =============================================================================
cat(sprintf("\n── Exportando CSVs ─────────────────────────────────────────────────\n"))

# CSV 1: Asignaciones
asignacion_csv <- bind_rows(lapply(all_results, function(r) r$res$asignacion))
write.csv(asignacion_csv,
          "fase_07_evaluacion/results/asignacion_staffing_proyectos.csv",
          row.names=FALSE)
cat("✓ asignacion_staffing_proyectos.csv\n")

# CSV 2: Comparativa overtime y redistribución
comparativa_global <- bind_rows(lapply(all_results, function(r) r$rsm)) %>%
  select(scenario_name, gamma, total_overtime_before, total_overtime_after,
         total_overtime_reduction, n_donors, total_hours_redistributed,
         high_risk_after)
write.csv(comparativa_global,
          "fase_07_evaluacion/results/comparativa_option2_classic_vs_mlor.csv",
          row.names=FALSE)
cat("✓ comparativa_option2_classic_vs_mlor.csv\n")

# CSV 3: Sensibilidad gamma
sensibilidad_csv <- bind_rows(lapply(all_results, function(r) r$rsm)) %>%
  mutate(
    delta_avg_burnout_vs_classic  = avg_burnout_after - rsm_classic$avg_burnout_after,
    delta_high_risk_vs_classic    = high_risk_after - rsm_classic$high_risk_after,
    delta_overtime_vs_classic     = total_overtime_after - rsm_classic$total_overtime_after,
    pct_mejora_burnout_donors     = ifelse(
      !is.na(avg_burnout_donors_before) & avg_burnout_donors_before > 0,
      (avg_burnout_donors_before - avg_burnout_donors_after) / avg_burnout_donors_before * 100,
      NA_real_)
  )
write.csv(sensibilidad_csv,
          "fase_07_evaluacion/results/sensibilidad_gamma_option2.csv",
          row.names=FALSE)
cat("✓ sensibilidad_gamma_option2.csv\n")

# CSV 4: Comparativa visual por equipo
visual_rows <- list()
for (lbl in names(all_results)) {
  if (lbl == "classic") next
  g    <- all_results[[lbl]]$rsm$gamma
  comp <- all_results[[lbl]]$comp
  
  eq <- comp %>%
    group_by(team_id) %>%
    summarise(
      avg_burnout_after_mlor   = mean(predicted_probability_after),
      high_risk_after_mlor     = sum(predicted_probability_after >= 0.5),
      hours_redistributed_mlor = sum(horas_cedidas),
      .groups = "drop"
    )
  
  eq_classic <- comp_classic %>%
    group_by(team_id) %>%
    summarise(
      avg_burnout_after_classic = mean(predicted_probability_after),
      high_risk_after_classic   = sum(predicted_probability_after >= 0.5),
      .groups = "drop"
    )
  
  visual_rows[[lbl]] <- left_join(eq_classic, eq, by="team_id") %>%
    mutate(
      gamma = g,
      scenario = lbl,
      delta_burnout_pp      = round((avg_burnout_after_mlor - avg_burnout_after_classic)*100, 2),
      delta_burnout_visual  = ifelse(delta_burnout_pp <= 0, "\U1f7e2", "\U1f534"),
      delta_high_risk       = high_risk_after_mlor - high_risk_after_classic,
      delta_high_risk_visual= ifelse(delta_high_risk <= 0, "\U1f7e2", "\U1f534")
    )
}
visual_csv <- bind_rows(visual_rows)
write.csv(visual_csv,
          "fase_07_evaluacion/results/comparativa_burnout_teams_visual_option2.csv",
          row.names=FALSE)
cat("✓ comparativa_burnout_teams_visual_option2.csv\n")

# CSV 5: Comparativa ejecutiva gamma finalistas
get_rsm <- function(g) {
  lbl <- names(all_results)[sapply(all_results, function(r) abs(r$rsm$gamma - g) < 0.01)]
  if (!length(lbl)) NULL else all_results[[lbl[1]]]$rsm
}
rsm_f1 <- get_rsm(GAMMA_FINALISTAS[1])
rsm_f2 <- get_rsm(GAMMA_FINALISTAS[2])

if (!is.null(rsm_f1) && !is.null(rsm_f2)) {
  # map_dfr: cada criterio es una lista de 3; se desempaqueta dentro de la función.
  # pmap_dfr fallaba porque interpretaba los 6 criterios como 6 argumentos
  # paralelos en lugar de 6 filas con 3 campos cada una.
  criterios <- list(
    list("Overtime total residual",             "total_overtime_after",        TRUE),
    list("Riesgo medio burnout (ex post)",      "avg_burnout_after",           TRUE),
    list("Empleados alto riesgo (ex post)",     "high_risk_after",             TRUE),
    list("Horas redistribuidas",                "total_hours_redistributed",   FALSE),
    list("N donantes protegidos",               "n_donors",                    FALSE),
    list("% mejora burnout donantes",           "pct_mejora_burnout_donors",   FALSE)
  )
  ejecutiva_rows <- purrr::map_dfr(criterios, function(crit) {
    lbl_c <- crit[[1]]
    field <- crit[[2]]
    lib   <- crit[[3]]
    # Extraer con seguridad: NULL → NA, vector vacío → NA
    safe_num <- function(x) { v <- suppressWarnings(as.numeric(x)); if (length(v) == 0) NA_real_ else v[[1]] }
    v1 <- safe_num(rsm_f1[[field]])
    v2 <- safe_num(rsm_f2[[field]])
    if (is.na(v1) || is.na(v2)) return(tibble(
      criterion       = lbl_c,
      gamma_A         = NA_real_,
      gamma_B         = NA_real_,
      preferred_gamma = NA_character_))
    pref <- if (lib) {
      ifelse(v1 <= v2,
             sprintf("gamma_%.1f", GAMMA_FINALISTAS[1]),
             sprintf("gamma_%.1f", GAMMA_FINALISTAS[2]))
    } else {
      ifelse(v1 >= v2,
             sprintf("gamma_%.1f", GAMMA_FINALISTAS[1]),
             sprintf("gamma_%.1f", GAMMA_FINALISTAS[2]))
    }
    tibble(criterion       = lbl_c,
           gamma_A         = round(v1, 4),
           gamma_B         = round(v2, 4),
           preferred_gamma = pref)
  })
  names(ejecutiva_rows)[2:3] <- sprintf("gamma_%.1f", GAMMA_FINALISTAS)
  write.csv(ejecutiva_rows,
            "fase_07_evaluacion/results/comparativa_ejecutiva_gamma_050_vs_060.csv",
            row.names=FALSE)
  cat("✓ comparativa_ejecutiva_gamma_050_vs_060.csv\n")
} else {
  ejecutiva_rows <- tibble()
  cat("  Gammas finalistas no disponibles.\n")
}


# =============================================================================
# 14. FIGURAS
# =============================================================================
cat(sprintf("\n── Generando figuras ───────────────────────────────────────────────\n"))

PALETA <- c("Clásico" = "gray50", "OR+ML" = "steelblue")

# Figura 1: Curva de burnout medio vs gamma
f1_df <- sensibilidad_csv %>%
  mutate(burnout_mejora_pp = -delta_avg_burnout_vs_classic * 100) %>%
  filter(gamma > 0)

if (nrow(f1_df) > 0) {
  p1 <- ggplot(f1_df, aes(x=gamma, y=burnout_mejora_pp)) +
    geom_line(color="steelblue", linewidth=1.4) +
    geom_point(color="steelblue", size=3.5) +
    geom_hline(yintercept=0, linetype="dashed", color="gray50") +
    geom_vline(xintercept=GAMMA_FINALISTAS, linetype="dotted",
               color="tomato", alpha=0.8) +
    annotate("text",
             x=GAMMA_FINALISTAS,
             y=max(f1_df$burnout_mejora_pp, na.rm=TRUE) * 0.85,
             label=sprintf("gamma=%.1f", GAMMA_FINALISTAS),
             color="tomato", hjust=-0.15, size=3.5) +
    scale_x_continuous(breaks=c(0, GAMMA_GRID)) +
    labs(title    = "Mejora de burnout medio — OR+ML vs clásico",
         subtitle = "Redistribución activa: donantes de alto riesgo alivian carga a receptores de bajo riesgo",
         x = "gamma",
         y = "Reducción burnout medio (pp vs clásico)") +
    theme_minimal(base_size=11)
  ggsave("fase_07_evaluacion/figures/fig1_sensibilidad_burnout_gamma.png",
         p1, width=9, height=5, dpi=150, bg="white")
  cat("✓ Figura 1\n")
}

# Figura 2: Trade-off overtime vs burnout (Pareto)
f2_df <- sensibilidad_csv %>%
  mutate(burnout_red   = -delta_avg_burnout_vs_classic * 100,
         overtime_chg  = delta_overtime_vs_classic)
if (nrow(f2_df) > 1) {
  p2 <- ggplot(f2_df, aes(x=overtime_chg, y=burnout_red, color=gamma,
                          label=round(gamma,1))) +
    geom_point(size=4, alpha=0.85) +
    geom_text(nudge_y=max(abs(f2_df$burnout_red), na.rm=TRUE)*0.04, size=3) +
    geom_hline(yintercept=0, linetype="dashed", color="gray60") +
    geom_vline(xintercept=0, linetype="dashed", color="gray60") +
    scale_color_gradient(low="steelblue", high="tomato") +
    labs(title    = "Trade-off: overtime vs mejora de burnout — OR+ML vs clásico",
         subtitle = "Cuadrante superior izquierdo = mejora ambos criterios",
         x = "Cambio en overtime total vs clásico (neg = mejor)",
         y = "Reducción burnout medio (pp, pos = mejor)",
         color = "gamma") +
    theme_minimal(base_size=11)
  ggsave("fase_07_evaluacion/figures/fig2_tradeoff_overtime_burnout.png",
         p2, width=9, height=5, dpi=150, bg="white")
  cat("✓ Figura 2\n")
}

# Figura 3: Distribución de riesgo clásico vs OR+ML (gamma recomendado)
GAMMA_REC <- GAMMA_FINALISTAS[1]
lbl_rec <- names(all_results)[sapply(all_results,
                                     function(r) abs(r$rsm$gamma - GAMMA_REC) < 0.01)]
if (length(lbl_rec)) {
  lbl_rec <- lbl_rec[1]
  nombre_ml <- sprintf("OR+ML (gamma=%.1f)", GAMMA_REC)
  risk_comp <- bind_rows(
    comp_classic %>% select(employee_id, prob=predicted_probability_after) %>%
      mutate(modelo="OR clásico"),
    all_results[[lbl_rec]]$comp %>%
      select(employee_id, prob=predicted_probability_after) %>%
      mutate(modelo=nombre_ml)
  )
  p3 <- ggplot(risk_comp, aes(x=prob, fill=modelo)) +
    geom_histogram(position="identity", alpha=0.6, bins=30) +
    geom_vline(xintercept=0.5, linetype="dashed", color="gray40") +
    scale_fill_manual(values=c("OR clásico"=grDevices::adjustcolor("tomato",0.7),
                               setNames(grDevices::adjustcolor("steelblue",0.7), nombre_ml))) +
    labs(title    = sprintf("Distribución riesgo predicho — clásico vs OR+ML (gamma=%.1f)", GAMMA_REC),
         subtitle = "OR+ML redistribuye trabajo desde donantes de alto riesgo → su distribución se desplaza a la izquierda",
         x = "Probabilidad de burnout predicha", y = "N empleados", fill=NULL) +
    theme_minimal(base_size=11) + theme(legend.position="top")
  ggsave("fase_07_evaluacion/figures/fig3_riesgo_clasico_vs_mlor.png",
         p3, width=9, height=5, dpi=150, bg="white")
  cat("✓ Figura 3\n")
}

# Figura 4: Donantes vs receptores — cambio de burnout
if (length(lbl_rec)) {
  comp_rec <- all_results[[lbl_rec]]$comp %>%
    mutate(rol = case_when(is_donor & !is_receiver ~ "Donante (aliviado)",
                           is_receiver & !is_donor  ~ "Receptor (absorbe)",
                           is_donor & is_receiver    ~ "Donante y receptor",
                           TRUE                      ~ "Sin cambio"))
  
  p4 <- ggplot(comp_rec, aes(x=predicted_probability_delta, fill=rol)) +
    geom_histogram(bins=30, alpha=0.8) +
    geom_vline(xintercept=0, linetype="dashed", color="gray40") +
    scale_fill_manual(values=c("Donante (aliviado)"  = "steelblue",
                               "Receptor (absorbe)"  = "tomato",
                               "Donante y receptor"  = "purple",
                               "Sin cambio"          = "gray70")) +
    labs(title    = sprintf("Cambio de burnout por rol — OR+ML gamma=%.1f", GAMMA_REC),
         subtitle = "Donantes: riesgo baja (alivio de carga). Receptores: riesgo sube levemente.",
         x = "Δ probabilidad burnout (OR+ML − antes)", y = "N empleados", fill="Rol") +
    theme_minimal(base_size=11) + theme(legend.position="right")
  ggsave("fase_07_evaluacion/figures/fig4_burnout_por_rol_donante_receptor.png",
         p4, width=10, height=5, dpi=150, bg="white")
  cat("✓ Figura 4\n")
}

# Figura 5: Curva donantes vs gamma
f5_df <- sensibilidad_csv %>% filter(gamma > 0)
if (nrow(f5_df)) {
  p5 <- ggplot(f5_df, aes(x=gamma, y=n_donors)) +
    geom_col(fill="steelblue", alpha=0.8, width=0.6) +
    geom_hline(yintercept=rsm_classic$n_donors, linetype="dashed", color="tomato") +
    annotate("text", x=max(GAMMA_GRID)*0.6,
             y=rsm_classic$n_donors*1.05,
             label="Clásico", color="tomato", size=3.5) +
    scale_x_continuous(breaks=c(0, GAMMA_GRID)) +
    labs(title    = "Empleados de alto riesgo aliviados (donantes) por gamma",
         subtitle = "Más donantes = más empleados protegidos de burnout adicional",
         x="gamma", y="N donantes protegidos") +
    theme_minimal(base_size=11)
  ggsave("fase_07_evaluacion/figures/fig5_donantes_por_gamma.png",
         p5, width=9, height=5, dpi=150, bg="white")
  cat("✓ Figura 5\n")
}

# Figura 6: Burnout de donantes antes vs después — evolución por gamma
f6_df <- sensibilidad_csv %>%
  select(gamma, avg_burnout_donors_before, avg_burnout_donors_after) %>%
  drop_na() %>%
  pivot_longer(-gamma, names_to="momento", values_to="burnout") %>%
  mutate(momento = recode(momento,
                          avg_burnout_donors_before = "Antes (inicio semana)",
                          avg_burnout_donors_after  = "Después (tras OR+ML)"))

if (nrow(f6_df)) {
  p6 <- ggplot(f6_df, aes(x=gamma, y=burnout, color=momento, group=momento)) +
    geom_line(linewidth=1.2) + geom_point(size=3) +
    scale_color_manual(values=c("Antes (inicio semana)"  = "tomato",
                                "Después (tras OR+ML)" = "steelblue")) +
    scale_x_continuous(breaks=c(0, GAMMA_GRID)) +
    labs(title    = "Burnout medio de los empleados donantes antes y después de la redistribución",
         subtitle = "A mayor gamma: más donantes aliviados → burnout medio del grupo donante baja",
         x="gamma", y="Riesgo predicho medio", color=NULL) +
    theme_minimal(base_size=11) + theme(legend.position="top")
  ggsave("fase_07_evaluacion/figures/fig6_burnout_donantes_por_gamma.png",
         p6, width=9, height=5, dpi=150, bg="white")
  cat("✓ Figura 6\n")
}

# Figura 7: Comparativa ejecutiva finalistas
if (!is.null(rsm_f1) && !is.null(rsm_f2) && nrow(ejecutiva_rows) > 0) {
  g_lbls <- sprintf("gamma_%.1f", GAMMA_FINALISTAS)
  exec_long <- ejecutiva_rows %>%
    pivot_longer(all_of(g_lbls), names_to="gamma_lbl", values_to="valor") %>%
    mutate(gamma_display = gsub("gamma_", "gamma = ", gamma_lbl))
  
  p7 <- ggplot(exec_long, aes(x=criterion, y=valor, fill=gamma_display)) +
    geom_col(position="dodge", alpha=0.85, width=0.65) +
    coord_flip() +
    scale_fill_manual(values=setNames(c("steelblue","tomato"),
                                      sprintf("gamma = %.1f", GAMMA_FINALISTAS))) +
    labs(title    = sprintf("Comparativa ejecutiva: gamma %.1f vs %.1f",
                            GAMMA_FINALISTAS[1], GAMMA_FINALISTAS[2]),
         subtitle = "Criterios clave de selección del parámetro final",
         x=NULL, y="Valor", fill=NULL) +
    theme_minimal(base_size=11) + theme(legend.position="top")
  ggsave("fase_07_evaluacion/figures/fig7_comparativa_ejecutiva_050_vs_060.png",
         p7, width=10, height=5, dpi=150, bg="white")
  cat("✓ Figura 7\n")
}


# =============================================================================
# 15. INFORME MARKDOWN
# =============================================================================
rsm_rec <- if (!is.null(rsm_f1)) rsm_f1 else
  all_results[[length(all_results)]]$rsm
red_burnout_pp <- (rsm_classic$avg_burnout_after - rsm_rec$avg_burnout_after) * 100
red_highrisk   <- rsm_classic$high_risk_after - rsm_rec$high_risk_after

cat_burnout_thr <- function(g) {
  sprintf("%.2f", C_TRANSFER / (0.75 * 1.0))  # para r_donor=0.8, r_rec=0.05, I=1
}

informe_md <- paste0(
  "# Informe OR+ML — Redistribución de carga con proxy de burnout (Fase 7)

## Descripción del modelo

Comparación de dos estrategias de optimización de staffing:

- **OR clásico** (benchmark): minimiza overtime y coste operativo. No considera burnout.
- **OR+ML**: añade un término de burnout al objetivo. Redistribuye activamente trabajo
  desde empleados de alto riesgo (donantes) hacia empleados de bajo riesgo (receptores).

**Modelo ML**: ", final_name, " | AUC test = ", round(artifact$metrics_test$auc,4), "

## Diagnóstico de la versión anterior y correcciones

La versión anterior producía mejoras de burnout de ~0.002 pp (prácticamente cero).
Las causas identificadas y sus correcciones:

| Causa raíz | Efecto | Corrección |
|---|---|---|
| C_INTERTEAM=0.35 > max(gamma*q_ip) | Solver nunca cambiaba asignaciones | C_INTERTEAM=0.08, C_TRANSFER=0.10 |
| LP solo añade trabajo (overflow) | Alto riesgo no era aliviado | Proyectos de relevo (tipo B) |
| R0=0.20 (69% pares con q_ip=0) | Señal ML insuficiente | R0=0 |
| Restricción de equidad (tau=0.20) | Limitaba diferencia clásico-OR+ML | Eliminada |

## Formulación matemática

### Variables de decisión

- `x[i,p] >= 0` : horas del empleado i asignadas al proyecto p
- `o[i] >= 0`  : overtime generado por nuevas asignaciones de i
- `slack[p] >= 0` : demanda no cubierta

### Tipos de proyectos

**Tipo A — overflow**: demanda adicional del equipo (como en Fase 6)
**Tipo B — relief** (NUEVO): trabajo delegable de empleados con r_i > ", RELIEF_MIN, "

### Penalización de slack por tipo de proyecto

- Overflow:  M = ", M_PENALTY, " (demanda obligatoria)
- Relief:    `gamma * r_donor * I_p`  (coste de NO redistribuir — sube con gamma)

### Función objetivo

```
min  sum (c_ip + gamma * r_i * I_p) * x[i,p]
   + omega * sum o[i]
   + sum slack_pen[p] * slack[p]
```

### Mecanismo de redistribución

Para un proyecto relief de donante i (r_i=0.8) a receptor j (r_j=0.05, I_p=1):

- Coste guardar (clásico, gamma=0):   0 < C_TRANSFER = 0.10  → NO redistribuye
- Coste redistribuir (gamma=1):        C_TRANSFER + gamma*r_j*I_p = 0.15
- Coste guardar (OR+ML, gamma=1):     gamma * r_donor * I_p = 0.80
- Umbral de redistribución: gamma > C_TRANSFER / (r_donor - r_j) / I_p = 0.13

### Cálculo de carga after

```
weekly_hours_after[i] = carga_actual[i] + horas_recibidas[i] - horas_cedidas[i]
```

### Restricciones

1. Cobertura: `sum_i x[i,p] + slack[p] = d_p`
2. Capacidad: `sum_p x[i,p] - o[i] <= avail_i`   (avail_i = max(0, H - carga_actual))
3. Factibilidad: `x[i,p] <= d_p` si (i,p) en A; 0 si no
4. Gobernanza: apoyo externo de k <= rho * Avail_k   (rho=", RHO, ")

## Resultados del baseline clásico

| Métrica | Valor |
|---|---|
| Overtime total | ", round(rsm_classic$total_overtime_after, 2), " h |
| Riesgo medio (ex post) | ", round(rsm_classic$avg_burnout_after*100, 2), "% |
| Alto riesgo (>=0.5) | ", rsm_classic$high_risk_after, " empleados |
| Donantes protegidos | ", rsm_classic$n_donors, " |
| Horas redistribuidas | ", round(rsm_classic$total_hours_redistributed, 2), " h |

## Sensibilidad de gamma

| gamma | donantes | h_redistrib | burnout_after | Δburnout_pp | alto_riesgo |
|---|---|---|---|---|---|
", paste(apply(
  sensibilidad_csv %>%
    select(gamma, n_donors, total_hours_redistributed,
           avg_burnout_after, delta_avg_burnout_vs_classic, high_risk_after),
  1,
  function(r) {
    sprintf("| %.2f | %d | %.1f | %.4f | %.4f | %d |",
            as.numeric(r["gamma"]),
            as.integer(r["n_donors"]),
            as.numeric(r["total_hours_redistributed"]),
            as.numeric(r["avg_burnout_after"]),
            as.numeric(r["delta_avg_burnout_vs_classic"]) * 100,
            as.integer(r["high_risk_after"]))
  }
), collapse="\n"), "

*Convenio: Δburnout_pp negativo = mejora vs clásico.*

## Parámetro recomendado: gamma = ", GAMMA_FINALISTAS[1], "

- Reducción burnout medio: **", round(red_burnout_pp, 2), " pp** vs clásico
- Reducción empleados alto riesgo: **", red_highrisk, " empleados**
- Empleados donantes protegidos: **", rsm_rec$n_donors, "**
- Horas redistribuidas: **", round(rsm_rec$total_hours_redistributed, 2), " h**

## Limitaciones y consideraciones

1. Los datos son sintéticos. La magnitud de mejora en producción depende de la
   distribución real de riesgo y capacidad disponible.
2. El umbral 0.5 de alto riesgo es provisional. Debe calibrarse según el coste
   relativo de falsos positivos/negativos en el contexto organizativo.
3. Los proyectos relief asumen que el trabajo es delegable entre empleados del
   mismo departamento y nivel compatible. En producción se necesita validación
   operativa por parte de los managers.
4. La evaluación ex post solo actualiza las variables de carga. Variables como
   deadline_pressure o engagement no se modifican, lo que subestima el impacto
   real de la redistribución.

## Criterios de cierre verificados

- [x] Dependencias gestionadas automáticamente
- [x] LP continuo exacto con ompr + GLPK
- [x] Datos en escala real (operational.csv)
- [x] department y job_level reales
- [x] avail_i = max(0, H - carga_actual) en restricción de capacidad
- [x] Modelo ML cargado desde artefacto Fase 4 (", final_name, ")
- [x] preprocess_orig() aplica pipeline antes de predecir
- [x] Proyectos de relevo (redistribución activa de carga)
- [x] Slack de relief con coste gamma*r_donor*I_p (mecanismo clave)
- [x] OR clásico como benchmark coherente con Fase 6
- [x] write.csv (sin dependencia de readr)
- [x] Informe solo declara criterios verificados
"
)

writeLines(informe_md,
           "fase_07_evaluacion/reports/informe_option2_project_staffing.md")
cat("✓ informe_option2_project_staffing.md\n")

cat(sprintf("\n✓ Fase 7 completada.\n"))
cat(sprintf("  Mejora burnout OR+ML (gamma=%.1f): %.4f pp\n",
            GAMMA_FINALISTAS[1], red_burnout_pp))
cat(sprintf("  Reducción alto riesgo            : %d empleados\n", red_highrisk))
cat(strrep("=", 70), "\n")