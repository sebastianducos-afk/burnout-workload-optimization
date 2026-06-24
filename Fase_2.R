# =============================================================================
# FASE 2 — ANÁLISIS EXPLORATORIO Y VALIDACIÓN DEL DATASET
# Predicción de Burnout y Optimización de Carga de Trabajo
# =============================================================================

library(dplyr)
library(tidyr)
library(ggplot2)
library(corrplot)
library(patchwork)

# ── Directorios de salida ────────────────────────────────────────────────────
dir.create("figures fase 2", showWarnings = FALSE)

# Función auxiliar: guardar figura
save_fig <- function(plot, filename, w = 8, h = 5) {
  ggsave(file.path("figures fase 2", filename), plot = plot,
         width = w, height = h, dpi = 150, bg = "white")
  invisible(plot)
}

# ── Carga de datos ────────────────────────────────────────────────────────────
teams     <- read.csv("datos sinteticos/teams.csv")
employees <- read.csv("datos sinteticos/employees.csv")
events    <- read.csv("datos sinteticos/organizational_events.csv")
weekly    <- read.csv("datos sinteticos/weekly_workload.csv")


# =============================================================================
# A. CALIDAD ESTRUCTURAL
# =============================================================================
dims <- list(
  teams     = dim(teams),
  employees = dim(employees),
  events    = dim(events),
  weekly    = dim(weekly)
)

cat("── Dimensiones ────────────────────────────────\n")
for (nm in names(dims))
  cat(sprintf("  %-12s: %d filas × %d columnas\n", nm, dims[[nm]][1], dims[[nm]][2]))

str(teams); str(employees); str(events); str(weekly)

# Clave primaria compuesta en weekly
dup_weekly <- sum(duplicated(weekly[, c("employee_id", "week")]))
cat("\nDuplicados en weekly (employee_id, week):", dup_weekly, "\n")

# Integridad referencial
emp_team_ok    <- all(employees$team_id  %in% teams$team_id)
weekly_emp_ok  <- all(weekly$employee_id %in% employees$employee_id)
weekly_team_ok <- all(weekly$team_id     %in% teams$team_id)
cat("Integridad employees.team_id  -> teams.team_id :", emp_team_ok,    "\n")
cat("Integridad weekly.employee_id -> employees     :", weekly_emp_ok,  "\n")
cat("Integridad weekly.team_id     -> teams.team_id :", weekly_team_ok, "\n")


# =============================================================================
# B. MISSING VALUES
# =============================================================================
missing_summary <- weekly %>%
  summarise(across(everything(), ~ sum(is.na(.)))) %>%
  pivot_longer(everything(), names_to = "variable", values_to = "na_count") %>%
  mutate(na_pct = round(100 * na_count / nrow(weekly), 2)) %>%
  filter(na_count > 0) %>%
  arrange(desc(na_pct))

print(missing_summary)


# =============================================================================
# C. ESTADÍSTICOS DESCRIPTIVOS
# =============================================================================
num_vars <- c("weekly_hours", "overtime_hours", "deadline_pressure",
              "team_event_load", "engagement_score", "performance_score",
              "team_avg_workload", "burnout_score")

descriptivos <- weekly %>%
  select(all_of(num_vars)) %>%
  summarise(across(everything(), list(
    mean   = ~ mean(., na.rm = TRUE),
    sd     = ~ sd(., na.rm = TRUE),
    min    = ~ min(., na.rm = TRUE),
    p25    = ~ quantile(., 0.25, na.rm = TRUE),
    median = ~ median(., na.rm = TRUE),
    p75    = ~ quantile(., 0.75, na.rm = TRUE),
    max    = ~ max(., na.rm = TRUE)
  ))) %>%
  pivot_longer(everything(),
               names_to  = c("variable", "stat"),
               names_pattern = "(.*)_(mean|sd|min|p25|median|p75|max)") %>%
  pivot_wider(names_from = stat, values_from = value)

print(descriptivos)


# =============================================================================
# D. VARIABLE OBJETIVO
# =============================================================================
target_rate <- mean(weekly$high_burnout_risk, na.rm = TRUE)
cat(sprintf("\nTasa de high_burnout_risk: %.2f %%\n", 100 * target_rate))
print(table(weekly$high_burnout_risk))

# ── Figura 1: Distribución de burnout_score ───────────────────────────────────
p_burnout_hist <- ggplot(weekly, aes(x = burnout_score)) +
  geom_histogram(bins = 40, fill = "steelblue", color = "white") +
  geom_vline(xintercept = 68, linetype = "dashed", color = "red", linewidth = 0.8) +
  annotate("text", x = 70, y = Inf, label = "Umbral 68", vjust = 2,
           hjust = 0, color = "red", size = 3.5) +
  labs(title = "Distribución de burnout_score",
       subtitle = "Línea roja = umbral de clasificación (≥ 68 → alto riesgo)",
       x = "Burnout Score", y = "Frecuencia") +
  theme_minimal(base_size = 12)

save_fig(p_burnout_hist, "fig1_burnout_score_hist.png")

# ── Figura 2: Balance del target ──────────────────────────────────────────────
target_df <- weekly %>%
  count(high_burnout_risk) %>%
  mutate(label = ifelse(high_burnout_risk == 1, "1 · Alto riesgo", "0 · Bajo riesgo"),
         pct   = round(100 * n / sum(n), 1))

p_target_balance <- ggplot(target_df, aes(x = label, y = n, fill = label)) +
  geom_col(width = 0.5) +
  geom_text(aes(label = paste0(pct, " %")), vjust = -0.4, size = 4) +
  scale_fill_manual(values = c("0 · Bajo riesgo" = "steelblue",
                               "1 · Alto riesgo" = "tomato")) +
  labs(title = "Balance de high_burnout_risk", x = "", y = "Observaciones") +
  theme_minimal(base_size = 12) +
  theme(legend.position = "none")

save_fig(p_target_balance, "fig2_target_balance.png", w = 5, h = 4)


# =============================================================================
# E. CORRELACIONES
# =============================================================================
cor_matrix <- weekly[, num_vars] %>%
  cor(use = "pairwise.complete.obs")

# ── Figura 3: Heatmap de correlaciones ───────────────────────────────────────
png(file.path("figures fase 2", "fig3_correlation_heatmap.png"),
    width = 900, height = 750, res = 120, bg = "white")
corrplot(cor_matrix, method = "color", type = "upper", order = "hclust",
         tl.col = "black", tl.srt = 45, addCoef.col = "black",
         number.cex = 0.70, cl.cex = 0.80,
         title = "Heatmap de correlaciones (variables clave)",
         mar = c(0, 0, 2, 0))
dev.off()

# Correlaciones con burnout_score
cor_burnout <- sort(cor_matrix["burnout_score", ], decreasing = TRUE)
cat("\nCorrelaciones con burnout_score:\n")
print(round(cor_burnout, 3))

cor_hours_ot <- cor(weekly$weekly_hours, weekly$overtime_hours,
                    use = "complete.obs")

cor_vac_burn <- cor(weekly$vacation_days_recent, weekly$burnout_score,
                    use = "complete.obs")

cor_man_eng  <- weekly %>%
  left_join(teams %>% select(team_id, manager_quality_score), by = "team_id") %>%
  summarise(r = cor(manager_quality_score, engagement_score,
                    use = "complete.obs")) %>%
  pull(r)

cat(sprintf("\ncor(weekly_hours, overtime_hours)    : %.3f  [esperado > 0.60]\n",  cor_hours_ot))
cat(sprintf("cor(vacation_days, burnout_score)    : %.3f  [esperado -0.15 – -0.25]\n", cor_vac_burn))
cat(sprintf("cor(manager_quality, engagement)     : %.3f  [esperado > 0.30]\n",    cor_man_eng))

# ── Figura 4: Dispersión overtime vs burnout / engagement vs burnout ──────────
p_ot_burn <- ggplot(weekly, aes(x = overtime_hours, y = burnout_score)) +
  geom_point(alpha = 0.08, color = "steelblue") +
  geom_smooth(method = "lm", color = "red", se = TRUE) +
  labs(title = "Horas extra vs Burnout",
       x = "Overtime hours", y = "Burnout score") +
  theme_minimal(base_size = 11)

p_eng_burn <- ggplot(weekly, aes(x = engagement_score, y = burnout_score)) +
  geom_point(alpha = 0.08, color = "steelblue") +
  geom_smooth(method = "lm", color = "red", se = TRUE) +
  labs(title = "Engagement vs Burnout",
       x = "Engagement score", y = "Burnout score") +
  theme_minimal(base_size = 11)

p_scatter_combined <- p_ot_burn + p_eng_burn +
  plot_annotation(title = "Relaciones clave con burnout_score")

save_fig(p_scatter_combined, "fig4_scatter_burnout_relations.png", w = 10, h = 4.5)


# =============================================================================
# F. ANÁLISIS POR SUBGRUPOS
# =============================================================================

weekly_enh <- weekly %>%
  left_join(employees %>% select(employee_id, job_level), by = "employee_id") %>%
  left_join(teams     %>% select(team_id, department),    by = "team_id")

# Riesgo por departamento
dept_risk <- weekly_enh %>%
  group_by(department) %>%
  summarise(
    n_obs          = n(),
    high_risk_rate = mean(high_burnout_risk, na.rm = TRUE),
    avg_burnout    = mean(burnout_score,     na.rm = TRUE)
  ) %>%
  arrange(desc(high_risk_rate))

cat("\nTasa de alto riesgo por departamento:\n")
print(dept_risk)

# ── Figura 5: Boxplot burnout por job_level ───────────────────────────────────
p_boxplot_joblevel <- ggplot(weekly_enh,
                             aes(x = factor(job_level,
                                            levels = c("junior","mid","senior","lead")),
                                 y = burnout_score, fill = job_level)) +
  geom_boxplot(outlier.size = 0.6, outlier.alpha = 0.4) +
  scale_fill_brewer(palette = "Blues") +
  labs(title = "Burnout score por nivel profesional",
       x = "Job level", y = "Burnout score") +
  theme_minimal(base_size = 12) +
  theme(legend.position = "none")

save_fig(p_boxplot_joblevel, "fig5_burnout_by_joblevel.png", w = 7, h = 4.5)


# =============================================================================
# G. EVOLUCIÓN TEMPORAL
# =============================================================================
PEAK_WEEKS  <- c(12, 13, 14, 25, 26, 27, 38, 39, 40, 51, 52)
event_weeks <- unique(events$week)

weekly_ts <- weekly %>%
  group_by(week) %>%
  summarise(avg_burnout   = mean(burnout_score,     na.rm = TRUE),
            high_risk_pct = mean(high_burnout_risk, na.rm = TRUE))

# ── Figura 6: Serie temporal del burnout medio ────────────────────────────────
peak_bands <- data.frame(
  start = c(12, 25, 38, 51) - 0.5,
  end   = c(14, 27, 40, 52) + 0.5
)

p_ts_burnout <- ggplot(weekly_ts, aes(x = week, y = avg_burnout)) +
  geom_rect(data = peak_bands,
            aes(xmin = start, xmax = end, ymin = -Inf, ymax = Inf),
            inherit.aes = FALSE, fill = "gold", alpha = 0.3) +
  geom_line(color = "steelblue", linewidth = 0.9) +
  geom_point(data = weekly_ts[weekly_ts$week %in% event_weeks, ],
             aes(x = week, y = avg_burnout), color = "red", size = 1.8) +
  labs(title    = "Evolución semanal del burnout promedio",
       subtitle = "Bandas amarillas = semanas pico · Puntos rojos = semanas con eventos",
       x = "Semana", y = "Burnout score medio") +
  theme_minimal(base_size = 12)

save_fig(p_ts_burnout, "fig6_burnout_timeseries.png", w = 10, h = 4.5)

cat("\nFiguras guardadas en /figures fase 2:\n")
cat(paste(" ·", list.files("figures fase 2")), sep = "\n")

