# =============================================================================
# run_all.R  —  Orquestador del pipeline completo (Fases 1 → 7)
#
# Ejecuta las siete fases EN ORDEN, respetando las dependencias entre ellas:
#
#   Fase 1  Generación del dataset sintético      -> "datos sinteticos/"
#   Fase 2  Análisis exploratorio (EDA)           -> "figures fase 2/"
#   Fase 3  Preprocesado (recipes)                -> "data fase 3/"
#   Fase 4  Modelado predictivo del burnout       -> "fase 4/"
#   Fase 5  Interpretabilidad del modelo          -> "fase 5/"
#   Fase 6  Baseline OR clásico (optimización)    -> "fase_06_optimizacion/"
#   Fase 7  OR + ML (redistribución de carga)     -> "fase_07_evaluacion/"
#
# USO
#   Desde la terminal (recomendado):
#       Rscript run_all.R
#   Desde R/RStudio:
#       source("run_all.R")
#
# El script fija el directorio de trabajo a la RAÍZ del repositorio (la carpeta
# que contiene este archivo), de modo que todas las rutas relativas de las fases
# se resuelven correctamente, sin importar desde dónde se lance.
# =============================================================================

# ── Localizar la raíz del repositorio (carpeta de este archivo) ───────────────
get_this_file <- function() {
  # 1) Ejecutado con Rscript:  Rscript run_all.R
  args <- commandArgs(trailingOnly = FALSE)
  m <- grep("^--file=", args)
  if (length(m) > 0) return(normalizePath(sub("^--file=", "", args[m[1]])))
  # 2) source("run_all.R") en una sesión interactiva
  if (!is.null(sys.frames())) {
    for (i in rev(seq_along(sys.frames()))) {
      of <- sys.frame(i)$ofile
      if (!is.null(of)) return(normalizePath(of))
    }
  }
  # 3) Último recurso: directorio de trabajo actual
  return(normalizePath(file.path(getwd(), "run_all.R"), mustWork = FALSE))
}

repo_root <- dirname(get_this_file())
setwd(repo_root)
cat("Directorio de trabajo (raíz del repo):\n  ", repo_root, "\n\n", sep = "")

scripts_dir <- file.path(repo_root, "scripts")

phases <- c(
  "Fase_1.R", "Fase_2.R", "Fase_3.R",
  "Fase_4.R", "Fase_5.R", "Fase_6.R", "Fase_7.R"
)

# ── Ejecutar cada fase en un entorno limpio ───────────────────────────────────
overall_start <- Sys.time()

for (ph in phases) {
  path <- file.path(scripts_dir, ph)
  if (!file.exists(path)) stop("No se encuentra el script: ", path)

  cat("=================================================================\n")
  cat(sprintf(">> Ejecutando %s\n", ph))
  cat("=================================================================\n")

  t0 <- Sys.time()
  # local = new.env() aísla las variables de cada fase; chdir = FALSE mantiene
  # el wd en la raíz para que las rutas relativas ("datos sinteticos/", etc.)
  # funcionen tal y como están escritas en los scripts.
  ok <- tryCatch({
    source(path, local = new.env(), chdir = FALSE, echo = FALSE)
    TRUE
  }, error = function(e) {
    cat(sprintf("\n✗ ERROR en %s:\n  %s\n", ph, conditionMessage(e)))
    FALSE
  })

  dt <- round(as.numeric(difftime(Sys.time(), t0, units = "secs")), 1)
  if (!ok) {
    cat(sprintf("\nPipeline detenido en %s tras %s s.\n", ph, dt))
    quit(status = 1, save = "no")
  }
  cat(sprintf("\n✓ %s completada en %s s.\n\n", ph, dt))
}

total <- round(as.numeric(difftime(Sys.time(), overall_start, units = "mins")), 1)
cat("=================================================================\n")
cat(sprintf("✓ PIPELINE COMPLETO. Tiempo total: %s min\n", total))
cat("=================================================================\n")
cat("\nResultados generados en:\n")
cat("  datos sinteticos/        (CSV del dataset sintético)\n")
cat("  figures fase 2/          (gráficos del EDA)\n")
cat("  data fase 3/             (train/validation/test + pipeline)\n")
cat("  fase 4/                  (modelo final + informe)\n")
cat("  fase 5/                  (interpretabilidad)\n")
cat("  fase_06_optimizacion/    (OR clásico)\n")
cat("  fase_07_evaluacion/      (OR + ML)\n")
