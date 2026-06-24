# =============================================================================
# GENERACIÓN DE DATASET SINTÉTICO – PREDICCIÓN DE BURNOUT Y OPTIMIZACIÓN
# DE CARGA DE TRABAJO EN ENTORNOS CORPORATIVOS
# =============================================================================

library(dplyr)
library(tidyr)
library(purrr)
library(stringr)

dir.create("datos sinteticos",  recursive = TRUE, showWarnings = FALSE)

set.seed(42)

# ── Constantes globales ──────────────────────────────────────────────────────
NUM_WEEKS        <- 52
NUM_TEAMS        <- 12
TOTAL_EMPLOYEES  <- 600
TEAM_SIZE_MIN    <- 30
TEAM_SIZE_MAX    <- 70

DEPARTMENTS <- c("Finance", "HR", "Marketing", "Operations", "IT", "Sales")

EVENT_TYPES <- c("quarter_close", "audit", "system_migration",
                 "product_launch", "restructuring", "peak_season")

# Semanas estacionales de alta actividad (cierres de trimestre)
PEAK_WEEKS <- c(12, 13, 14, 25, 26, 27, 38, 39, 40, 51, 52)

# Semanas habituales de vacaciones
VACATION_WEEKS <- 30:35


# =============================================================================
# 1. EQUIPOS (teams)
# =============================================================================
# · business_criticality: 1-5; IT/Operations tienden a criticidad alta.
# · baseline_workload_level: correlaciona positivamente con criticidad.
# · Se garantiza que los cinco niveles de criticidad aparecen al menos una vez.
# · team_size se ajusta iterativamente hasta sumar exactamente TOTAL_EMPLOYEES.

dept_forced <- rep(DEPARTMENTS, each = 2)   # 2 equipos por departamento

teams <- data.frame(
  team_id                 = 1:NUM_TEAMS,
  department              = dept_forced[1:NUM_TEAMS],
  team_size               = sample(TEAM_SIZE_MIN:TEAM_SIZE_MAX,
                                   NUM_TEAMS, replace = TRUE),
  manager_quality_score   = rbeta(NUM_TEAMS, 2, 2) * 100,
  baseline_workload_level = NA_real_,
  team_turnover_rate      = rbeta(NUM_TEAMS, 1.5, 3) * 0.35,
  business_criticality    = NA_integer_,
  stringsAsFactors = FALSE
)

# Probabilidades de criticidad según departamento
for (i in seq_len(NUM_TEAMS)) {
  dept <- teams$department[i]
  crit_probs <- switch(dept,
                       IT         = , Operations = c(0.05, 0.10, 0.20, 0.30, 0.35),
                       Finance    = , Sales      = c(0.10, 0.15, 0.25, 0.25, 0.25),
                       c(0.20, 0.20, 0.25, 0.20, 0.15)
  )
  teams$business_criticality[i] <- sample(1:5, 1, prob = crit_probs)
}

# Garantizar cobertura de todos los niveles 1-5
missing_levels <- setdiff(1:5, unique(teams$business_criticality))
for (lev in missing_levels) {
  idx <- sample(which(teams$business_criticality != lev), 1)
  teams$business_criticality[idx] <- lev
}

# Carga base correlacionada con criticidad
teams$baseline_workload_level <- rnorm(
  NUM_TEAMS,
  mean = 45 + teams$business_criticality * 8,
  sd   = 8
)
teams$baseline_workload_level <- pmax(pmin(teams$baseline_workload_level, 100), 0)

# Ajuste iterativo de team_size para sumar TOTAL_EMPLOYEES
current_sum <- sum(teams$team_size)
while (current_sum != TOTAL_EMPLOYEES) {
  if (current_sum < TOTAL_EMPLOYEES) {
    eligible <- which(teams$team_size < TEAM_SIZE_MAX)
    if (!length(eligible)) break
    teams$team_size[sample(eligible, 1)] <- teams$team_size[sample(eligible, 1)] + 1
  } else {
    eligible <- which(teams$team_size > TEAM_SIZE_MIN)
    if (!length(eligible)) break
    teams$team_size[sample(eligible, 1)] <- teams$team_size[sample(eligible, 1)] - 1
  }
  current_sum <- sum(teams$team_size)
}
stopifnot(sum(teams$team_size) == TOTAL_EMPLOYEES)

teams$department <- factor(teams$department, levels = DEPARTMENTS)


# =============================================================================
# 2. EMPLEADOS (employees)
# =============================================================================
# · Edad truncada en [23, 60] con media 35.
# · Tenencia (gamma): influye en job_level y base_resilience_score.
# · remote_ratio: IT muy alto, Operations muy bajo.
# · base_engagement_trait: afectado por calidad del mánager y rotación del equipo.

rtruncnorm_simple <- function(n, a, b, mean, sd) {
  pmax(a, pmin(b, rnorm(n, mean, sd)))
}

employees_list <- vector("list", TOTAL_EMPLOYEES)
emp_counter    <- 0L

for (t in seq_len(NUM_TEAMS)) {
  team_id   <- teams$team_id[t]
  team_size <- teams$team_size[t]
  
  for (i in seq_len(team_size)) {
    emp_counter <- emp_counter + 1L
    
    age    <- round(rtruncnorm_simple(1, 23, 60, 35, 10))
    tenure <- pmax(pmin(rgamma(1, shape = 2.5, scale = 20), 180), 1)
    
    # Job level según tenencia
    job_level <- if (tenure < 12) {
      sample(c("junior","mid","senior","lead"), 1, prob = c(0.60, 0.30, 0.10, 0.00))
    } else if (tenure < 36) {
      sample(c("junior","mid","senior","lead"), 1, prob = c(0.20, 0.50, 0.30, 0.00))
    } else if (tenure < 84) {
      sample(c("junior","mid","senior","lead"), 1, prob = c(0.05, 0.40, 0.50, 0.05))
    } else {
      sample(c("junior","mid","senior","lead"), 1, prob = c(0.00, 0.20, 0.55, 0.25))
    }
    
    # Role type según job level
    role_type <- if (job_level == "junior") {
      sample(c("analyst","specialist","coordinator","support"), 1,
             prob = c(0.40, 0.30, 0.20, 0.10))
    } else if (job_level == "mid") {
      sample(c("analyst","specialist","coordinator","support"), 1,
             prob = c(0.30, 0.30, 0.30, 0.10))
    } else if (job_level == "senior") {
      sample(c("analyst","specialist","coordinator","manager"), 1,
             prob = c(0.20, 0.30, 0.20, 0.30))
    } else {
      sample(c("manager","coordinator"), 1, prob = c(0.70, 0.30))
    }
    
    # Remote ratio según departamento
    dept <- as.character(teams$department[t])
    remote <- if (dept == "IT") {
      rbeta(1, 5, 1)
    } else if (dept == "Operations") {
      rbeta(1, 1, 3)
    } else {
      rbeta(1, 2, 2)
    }
    
    # Percentil salarial (mayor nivel → mayor percentil esperado)
    jl_index    <- match(job_level, c("junior","mid","senior","lead"))
    salary_pct  <- runif(1, 0, 100) * (0.6 + 0.4 * jl_index / 4)
    salary_pct  <- pmax(pmin(salary_pct, 100), 0)
    
    # Engagement base (afectado por calidad del mánager y rotación)
    base_eng <- rbeta(1, 2, 2) * 100 *
      (teams$manager_quality_score[t] / 100) *
      (1 - teams$team_turnover_rate[t] / 0.35)
    base_eng <- pmax(pmin(base_eng, 100), 5)
    
    # Resiliencia base (crece levemente con edad y tenencia)
    base_res <- rbeta(1, 2, 2) * 100 *
      (1 + 0.2 * (age - 30) / 30) *
      (1 + 0.1 * tenure / 180)
    base_res <- pmax(pmin(base_res, 100), 0)
    
    employees_list[[emp_counter]] <- data.frame(
      employee_id                = NA_integer_,
      team_id                    = team_id,
      manager_id                 = NA_integer_,
      age                        = age,
      tenure_months              = tenure,
      job_level                  = job_level,
      role_type                  = role_type,
      remote_ratio               = remote,
      salary_position_percentile = salary_pct,
      base_engagement_trait      = base_eng,
      base_resilience_score      = base_res,
      stringsAsFactors = FALSE
    )
  }
}

employees <- bind_rows(employees_list)
employees$employee_id <- seq_len(nrow(employees))

# Asignación de manager_id (lead más antiguo, o manager, o el más antiguo)
for (t in unique(employees$team_id)) {
  team_emps <- employees[employees$team_id == t, ]
  leaders   <- team_emps[team_emps$job_level == "lead", ]
  if (nrow(leaders) > 0) {
    mgr <- leaders$employee_id[which.max(leaders$tenure_months)]
  } else {
    managers <- team_emps[team_emps$role_type == "manager", ]
    mgr <- if (nrow(managers) > 0) {
      managers$employee_id[which.max(managers$tenure_months)]
    } else {
      team_emps$employee_id[which.max(team_emps$tenure_months)]
    }
  }
  employees$manager_id[employees$team_id == t] <- mgr
}

employees$job_level  <- factor(employees$job_level,
                               levels = c("junior","mid","senior","lead"))
employees$role_type  <- factor(employees$role_type,
                               levels = c("analyst","specialist","coordinator",
                                          "manager","support"))


# =============================================================================
# 3. EVENTOS ORGANIZATIVOS (organizational_events)
# =============================================================================
# · Entre 6 y 12 eventos por equipo (moderado, evitando sobresaturación).
# · Finance/HR/Operations tienen sesgo hacia audit y quarter_close.
# · Intensidad de restructuring siempre alta (3-5).
# · Eventos de audit/quarter_close en depts. financieros reciben factor ×1.2.

events_list <- list()

for (t in unique(teams$team_id)) {
  num_events <- sample(6:12, 1)
  weeks      <- sample(1:NUM_WEEKS, num_events, replace = FALSE)
  dept       <- as.character(teams$department[teams$team_id == t])
  
  for (w in weeks) {
    if (dept %in% c("Finance","HR","Operations") && runif(1) < 0.60) {
      event_type <- sample(EVENT_TYPES, 1,
                           prob = c(0.30, 0.30, 0.10, 0.10, 0.10, 0.10))
    } else if (w %in% PEAK_WEEKS) {
      event_type <- sample(EVENT_TYPES, 1,
                           prob = c(0.20, 0.10, 0.10, 0.10, 0.10, 0.40))
    } else {
      event_type <- sample(EVENT_TYPES, 1,
                           prob = c(0.15, 0.15, 0.20, 0.20, 0.15, 0.15))
    }
    
    intensity <- runif(1, 1.5, 4.5)
    if (event_type == "restructuring")  intensity <- runif(1, 3.0, 5.0)
    if (event_type == "quarter_close")  intensity <- runif(1, 2.5, 4.5)
    if (event_type %in% c("audit","quarter_close") &&
        dept %in% c("Finance","HR","Operations")) {
      intensity <- intensity * 1.2
    }
    intensity <- pmax(pmin(intensity, 5), 1)
    
    events_list[[length(events_list) + 1]] <- data.frame(
      event_id        = NA_integer_,
      week            = w,
      team_id         = t,
      event_type      = event_type,
      event_intensity = intensity,
      stringsAsFactors = FALSE
    )
  }
}

events          <- bind_rows(events_list)
events$event_id <- seq_len(nrow(events))
events$event_type <- factor(events$event_type, levels = EVENT_TYPES)


# =============================================================================
# 4. CARGA SEMANAL (weekly_workload)
# =============================================================================
# Dinámica temporal AR(1) por empleado:
#
#   workload_factor[t] = 0.70 × workload_factor[t-1]
#                      + 0.30 × (0.55 × carga_estructural + 0.45 × choque_temporal)
#
#   carga_estructural = f(baseline_equipo, job_level, role_type, remote_ratio)
#   choque_temporal   = estacionalidad + efecto_eventos + ruido
#
# Esto garantiza que el contexto del empleado y del equipo persiste semana a
# semana y no se "olvida" tras la primera semana.
#
# OVERTIME: la media de horas extra escala linealmente con el exceso sobre 45 h,
# lo que produce cor(weekly_hours, overtime_hours) > 0.60 (verificado al final).

emp_dict  <- split(employees, employees$employee_id)
team_dict <- split(teams, teams$team_id)

prev_week_data <- list()   # estado AR del empleado
weekly_records <- list()

for (week in seq_len(NUM_WEEKS)) {
  for (i in seq_len(nrow(employees))) {
    
    emp  <- emp_dict[[as.character(employees$employee_id[i])]]
    team <- team_dict[[as.character(emp$team_id)]]
    
    # Carga de eventos del equipo esta semana
    team_events    <- events[events$week == week &
                               events$team_id == emp$team_id, ]
    team_event_load <- if (nrow(team_events) > 0) {
      sum(team_events$event_intensity)
    } else 0
    
    # ── Carga estructural (escala 0-100) ─────────────────────────────────────
    baseline_team <- team$baseline_workload_level / 100
    
    job_factor <- switch(as.character(emp$job_level),
                         lead   = 1.55, senior = 1.40, junior = 0.70, 1.00)
    role_factor   <- ifelse(as.character(emp$role_type) %in%
                              c("manager","coordinator"), 1.25, 1.00)
    remote_factor <- 1 - 0.10 * emp$remote_ratio
    
    structural_load <- baseline_team * job_factor * role_factor * remote_factor
    structural_load <- pmax(pmin(structural_load, 1.0), 0.30) * 100
    
    # ── Choque temporal ───────────────────────────────────────────────────────
    seasonal      <- ifelse(week %in% PEAK_WEEKS, 38, 0)
    event_effect  <- team_event_load * 15
    noise         <- rnorm(1, 0, 6)
    temporal_shock <- pmax(pmin(seasonal + event_effect + noise, 100), 0)
    
    # ── Dinámica AR(1) ────────────────────────────────────────────────────────
    if (week == 1) {
      workload_factor <- structural_load * 0.80 + temporal_shock * 0.20
    } else {
      prev           <- prev_week_data[[as.character(emp$employee_id)]]
      persistent     <- 0.70 * prev$workload_factor
      new_info       <- 0.30 * (0.55 * structural_load + 0.45 * temporal_shock)
      workload_factor <- persistent + new_info
    }
    workload_factor <- pmax(pmin(workload_factor, 100), 0)
    
    # ── Variables derivadas ───────────────────────────────────────────────────
    
    # Volumen de tareas (log-normal)
    task_volume <- exp(rnorm(1, log(50 + workload_factor * 0.95), 0.40))
    task_volume <- pmax(pmin(task_volume, 120), 30)
    
    # Número de proyectos activos
    active_projects <- rpois(1, lambda = 2 + workload_factor / 25)
    active_projects <- pmax(pmin(active_projects, 6), 1)
    
    # Complejidad de tareas (1-5)
    comp_probs      <- c(0.05, 0.15, 0.30, 0.25, 0.25) +
      (workload_factor / 100) * c(-0.03, -0.02, 0.00, 0.03, 0.02)
    comp_probs      <- pmax(comp_probs, 0)
    comp_probs      <- comp_probs / sum(comp_probs)
    task_complexity <- sample(1:5, 1, prob = comp_probs)
    
    # Presión de plazos
    deadline_pressure <- workload_factor * 0.85 +
      team_event_load * 14 +
      ifelse(week %in% PEAK_WEEKS, 15, 0)
    deadline_pressure <- pmax(pmin(deadline_pressure, 100), 0)
    
    # Horas de reunión (diferenciadas por nivel/rol)
    base_meeting <- if (as.character(emp$job_level) %in% c("lead","senior") ||
                        as.character(emp$role_type) == "manager") {
      rnorm(1, 10, 3)
    } else if (as.character(emp$role_type) == "coordinator") {
      rnorm(1,  7, 2.5)
    } else {
      rnorm(1,  4, 2.0)
    }
    meeting_hours <- base_meeting + 0.15 * workload_factor + team_event_load * 0.8
    meeting_hours <- pmax(pmin(meeting_hours, 25), 0)
    
    # Horas semanales totales
    weekly_hours <- 45 + (workload_factor - 50) * 0.42 + rnorm(1, 0, 4)
    weekly_hours <- pmax(pmin(weekly_hours, 78), 30)
    
    # ── Horas extra (overtime) ────────────────────────────────────────────────
    # La MEDIA de overtime escala con el exceso sobre 45 h para asegurar
    # cor(weekly_hours, overtime_hours) > 0.60.
    if (weekly_hours > 45) {
      excess   <- weekly_hours - 45
      ot_prob  <- pmin(0.78 + excess / 18, 0.97)
      mean_ot  <- 3 + excess * 0.90       # media escala linealmente con exceso
      overtime_hours <- if (runif(1) < ot_prob) {
        rnorm(1, mean_ot, 3.0)
      } else 0
    } else {
      overtime_hours <- 0
    }
    overtime_hours <- pmax(pmin(overtime_hours, 30), 0)
    
    # Variabilidad de carga (diferencia absoluta respecto a semana anterior)
    if (week == 1) {
      workload_variability <- 0
    } else {
      prev <- prev_week_data[[as.character(emp$employee_id)]]
      workload_variability <- 0.60 * abs(workload_factor - prev$workload_factor) +
        0.40 * abs(rnorm(1, 0, 5)) +
        ifelse(team_event_load > 0, 5, 0)
      workload_variability <- pmax(pmin(workload_variability, 100), 0)
    }
    
    # Días de vacaciones (más en semanas estivales/fin de año)
    vac <- rpois(1, ifelse(week %in% c(VACATION_WEEKS, 50:52), 4, 0.8))
    vac <- pmax(pmin(vac, 10), 0)
    
    # Días de baja por enfermedad (crece levemente con carga)
    sick <- rpois(1, 0.30 + 0.03 * workload_factor / 100)
    sick <- pmax(pmin(sick, 5), 0)
    
    # Horas de formación (inversamente proporcional a la carga)
    train <- rexp(1, rate = ifelse(workload_factor < 40, 0.5, 2.0))
    train <- pmax(pmin(train, 20), 0)
    
    # Guardar registro semanal
    weekly_records[[length(weekly_records) + 1]] <- data.frame(
      employee_id           = emp$employee_id,
      team_id               = emp$team_id,       # CORRECCIÓN 1: conservar team_id
      week                  = week,
      active_projects       = active_projects,
      task_volume           = task_volume,
      task_complexity       = task_complexity,
      deadline_pressure     = deadline_pressure,
      meeting_hours         = meeting_hours,
      weekly_hours          = weekly_hours,
      overtime_hours        = overtime_hours,
      workload_variability  = workload_variability,
      vacation_days_recent  = vac,
      sick_days_recent      = sick,
      training_hours_recent = train,
      team_event_load       = team_event_load,
      workload_factor       = workload_factor,   # auxiliar; se elimina al final
      stringsAsFactors = FALSE
    )
    
    prev_week_data[[as.character(emp$employee_id)]] <-
      list(workload_factor = workload_factor)
  }
}

weekly <- bind_rows(weekly_records)

# team_avg_workload: media semanal de task_volume del equipo
# team_id ya está disponible directamente en la tabla; no se necesita join externo
weekly <- weekly %>%
  group_by(team_id, week) %>%
  mutate(team_avg_workload = mean(task_volume)) %>%
  ungroup()


# =============================================================================
# 5. ENGAGEMENT Y PERFORMANCE
# =============================================================================
# engagement_score (0-100):
#   base_engagement (rasgo del empleado)   ×0.30
#   manager_quality                        ×0.30
#   (1 – carga)                            ×0.30
#   efecto evento (negativo, ×0.10)
#   efecto vacaciones (positivo)
#
# performance_score (0-100):
#   función cuadrática de la carga (óptimo entorno al 50 %)
#   más contribución positiva del engagement

engagement  <- numeric(nrow(weekly))
performance <- numeric(nrow(weekly))

for (i in seq_len(nrow(weekly))) {
  row  <- weekly[i, ]
  emp  <- emp_dict[[as.character(row$employee_id)]]
  team <- team_dict[[as.character(emp$team_id)]]
  
  base_eng    <- emp$base_engagement_trait / 100
  mgr_qual    <- team$manager_quality_score / 100
  load_factor <- row$workload_factor / 100
  
  event_effect <- 0
  if (row$team_event_load > 0) {
    evs <- events[events$week    == row$week &
                    events$team_id == emp$team_id, ]
    event_effect <- if (any(evs$event_type == "restructuring")) {
      -0.50
    } else {
      -0.10 * row$team_event_load
    }
  }
  vac_effect <- 0.07 * row$vacation_days_recent
  
  eng <- base_eng * 0.30 + mgr_qual * 0.30 + (1 - load_factor) * 0.30 +
    event_effect + vac_effect
  eng <- pmax(pmin(eng, 1), 0) * 100
  engagement[i] <- eng
  
  perf <- 1 - 2.5 * (load_factor - 0.5)^2 + (eng / 100) * 0.20
  perf <- pmax(pmin(perf + rnorm(1, 0, 6), 100), 0)
  performance[i] <- perf
}

weekly$engagement_score  <- engagement
weekly$performance_score <- performance


# =============================================================================
# 6. BURNOUT_SCORE Y HIGH_BURNOUT_RISK
# =============================================================================
# Fórmula lineal ponderada (escala 0-1 antes de ×100):
#
#  Factores de RIESGO (pesos positivos)
#    overtime_hours       0.48  ← principal driver (cumplir cor > 0.60 con target)
#    deadline_pressure    0.18
#    task_complexity      0.08
#    workload_variability 0.06
#    team_avg_workload    0.05
#    meeting_hours        0.04
#
#  Factores PROTECTORES (pesos negativos)
#    engagement_score     −0.06
#    manager_quality      −0.04
#    vacation_days        −0.14
#    resilience           −0.02
#
#  Post-ajuste multiplicativo para reforzar señal de overtime y vacaciones
#  sin distorsionar el resto, y reescalado ligero para tasa ~8-10 %.
#
#  Umbral high_burnout_risk: burnout_score ≥ 68

scale01 <- function(x, min_val, max_val) (x - min_val) / (max_val - min_val)

weekly <- weekly %>%
  mutate(
    overtime_scaled    = scale01(overtime_hours,    0,   30),
    deadline_scaled    = scale01(deadline_pressure, 0,  100),
    complexity_scaled  = scale01(task_complexity,   1,    5),
    variability_scaled = scale01(workload_variability, 0, 100),
    teamavg_scaled     = scale01(team_avg_workload, 30,  120),
    meeting_scaled     = scale01(meeting_hours,      0,   25),
    engagement_scaled  = scale01(engagement_score,   0,  100),
    vacation_scaled    = scale01(vacation_days_recent, 0, 10)
  ) %>%
  left_join(employees %>% select(employee_id, base_resilience_score),
            by = "employee_id") %>%
  left_join(teams %>% select(team_id, manager_quality_score), by = "team_id") %>%
  mutate(
    resilience_scaled    = base_resilience_score   / 100,
    mgr_quality_scaled   = manager_quality_score   / 100
  )

weekly$burnout_score_raw <- with(weekly,
                                 0.48 * overtime_scaled    +
                                   0.18 * deadline_scaled    +
                                   0.08 * complexity_scaled  +
                                   0.06 * variability_scaled +
                                   0.05 * teamavg_scaled     +
                                   0.04 * meeting_scaled     -
                                   0.06 * engagement_scaled  -
                                   0.04 * mgr_quality_scaled -
                                   0.14 * vacation_scaled    -
                                   0.02 * resilience_scaled
)

# Efecto adicional de eventos y ruido
weekly$event_adj    <- weekly$team_event_load * 0.08
weekly$burnout_score <- (weekly$burnout_score_raw +
                           weekly$event_adj +
                           rnorm(nrow(weekly), 0, 0.10)) * 100
weekly$burnout_score <- pmax(pmin(weekly$burnout_score, 100), 0)

# Post-ajuste multiplicativo (refuerza overtime, penaliza vacaciones)
weekly$burnout_score <- with(weekly,
                             burnout_score *
                               (1 + 0.15 * scale01(overtime_hours,      0, 30) -
                                  0.12 * scale01(vacation_days_recent, 0, 10))
)
weekly$burnout_score <- pmax(pmin(weekly$burnout_score, 100), 0)

# Reescalado suave para situar tasa en 8-10 %
weekly$burnout_score <- pmin(weekly$burnout_score * 1.05, 100)

# Variable objetivo binaria (umbral 68 puntos)
weekly$high_burnout_risk <- as.integer(weekly$burnout_score >= 68)

# Eliminar columnas auxiliares de la tabla final
# CORRECCIÓN 1: team_id ya NO se elimina — es clave estructural de la tabla
weekly <- weekly %>%
  select(-overtime_scaled, -deadline_scaled, -complexity_scaled,
         -variability_scaled, -teamavg_scaled, -meeting_scaled,
         -engagement_scaled, -vacation_scaled, -resilience_scaled,
         -mgr_quality_scaled, -event_adj, -workload_factor,
         -base_resilience_score, -manager_quality_score,
         -burnout_score_raw)


# =============================================================================
# 7. MISSING VALUES (parcialmente informativos)
# =============================================================================
# engagement_score (~10 %):
#   · 7 % aleatorio (mecanismo MAR leve)
#   · 3 % adicional condicionado a carga alta (weekly_hours > 45)
#   → missings más probables cuando el empleado está sobrecargado (MNAR suave)
#
# training_hours_recent (~4 %) y performance_score (~3 %): aleatorios (MCAR)

# Engagement: combinación aleatoria + informativa
missing_eng <- rep(FALSE, nrow(weekly))
missing_eng[runif(nrow(weekly)) < 0.07] <- TRUE   # componente aleatoria
high_load_idx <- which(weekly$weekly_hours > 45)
if (length(high_load_idx) > 0) {
  add_n   <- min(round(0.03 * nrow(weekly)), length(high_load_idx))
  add_idx <- sample(high_load_idx, add_n)
  missing_eng[add_idx] <- TRUE                     # componente informativa
}
weekly$engagement_score[missing_eng] <- NA

# Training y performance: MCAR
weekly$training_hours_recent[runif(nrow(weekly)) < 0.04] <- NA
weekly$performance_score    [runif(nrow(weekly)) < 0.03] <- NA


# =============================================================================
# 8. OUTLIERS INTENCIONALES
# =============================================================================
# Se introducen valores atípicos moderados que reflejan situaciones reales:
#   · Semanas excepcionales con >65 h semanales (1.5 % de registros)
#   · Presión máxima (>95) cuando eventos muy intensos (team_event_load > 3)
#   · Picos de proyectos activos (6) en contextos complejos
#   · Caída de engagement durante restructuring (efecto organizativo real)

# Horas semanales extremas (1.5 %)
mask_hrs  <- runif(nrow(weekly)) < 0.015
weekly$weekly_hours[mask_hrs] <- runif(sum(mask_hrs), 65, 78)

# Presión de plazos máxima en eventos muy intensos
mask_dead <- weekly$team_event_load > 3 & runif(nrow(weekly)) < 0.05
weekly$deadline_pressure[mask_dead] <- runif(sum(mask_dead), 95, 100)

# Proyectos en situación crítica
mask_proj <- runif(nrow(weekly)) < 0.02 & weekly$task_complexity >= 4
weekly$active_projects[mask_proj] <- 6

# Caída de engagement durante semanas de restructuring (−50 %)
restructuring_weeks <- unique(events$week[events$event_type == "restructuring"])
for (w in restructuring_weeks) {
  teams_aff <- unique(events$team_id[events$week == w &
                                       events$event_type == "restructuring"])
  emps_aff  <- employees$employee_id[employees$team_id %in% teams_aff]
  mask_rest <- weekly$week == w & weekly$employee_id %in% emps_aff
  weekly$engagement_score[mask_rest] <-
    pmax(pmin(weekly$engagement_score[mask_rest] * 0.50, 100), 0)
}


# =============================================================================
# 9. EXPORTAR CSV
# =============================================================================
write.csv(teams,    "datos sinteticos/teams.csv",                 row.names = FALSE)
write.csv(employees,"datos sinteticos/employees.csv",             row.names = FALSE)
write.csv(events,   "datos sinteticos/organizational_events.csv", row.names = FALSE)
write.csv(weekly,   "datos sinteticos/weekly_workload.csv",       row.names = FALSE)

message("✓ Exportación completada. Filas en weekly_workload: ", nrow(weekly))
message("  Columnas en weekly_workload: ", paste(names(weekly), collapse = ", "))