# =============================================================================
# install_dependencies.R
# Instala TODOS los paquetes de R necesarios para ejecutar el pipeline (Fases 1-7).
#
# Uso:
#   Rscript install_dependencies.R
#   # o, dentro de R/RStudio:
#   source("install_dependencies.R")
#
# NOTA sobre GLPK (solver de optimización, Fases 6 y 7):
#   El paquete 'ROI.plugin.glpk' necesita la librería GLPK instalada en el SISTEMA.
#     - Ubuntu/Debian : sudo apt-get install -y libglpk-dev
#     - macOS (Homebrew): brew install glpk
#     - Windows        : se instala con Rtools (ver README.md)
#   Instala GLPK ANTES de ejecutar este script.
# =============================================================================

repos <- "https://cloud.r-project.org"

# Paquetes requeridos por cada fase (unión completa)
required_pkgs <- c(
  # Manipulación de datos (Fases 1-7)
  "dplyr", "tidyr", "purrr", "stringr",
  # Visualización (Fases 2, 4, 5, 6)
  "ggplot2", "corrplot", "patchwork", "scales",
  # Preprocesado (Fases 3, 5, 7)
  "recipes",
  # Modelado y evaluación (Fases 4, 5, 6, 7)
  "pROC", "ranger", "xgboost",
  # Optimización / OR (Fases 6, 7) -> requieren GLPK del sistema
  "ompr", "ompr.roi", "ROI.plugin.glpk"
)

installed <- rownames(installed.packages())
missing   <- required_pkgs[!(required_pkgs %in% installed)]

if (length(missing) == 0) {
  message("✓ Todos los paquetes ya están instalados.")
} else {
  message("Instalando ", length(missing), " paquete(s): ",
          paste(missing, collapse = ", "))
  install.packages(missing, repos = repos)
}

# Verificación final: ¿se pueden cargar todos?
ok  <- vapply(required_pkgs, requireNamespace, logical(1), quietly = TRUE)
bad <- required_pkgs[!ok]

cat("\n────────────────────────────────────────────\n")
if (length(bad) == 0) {
  cat("✓ Verificación correcta: los", length(required_pkgs),
      "paquetes están disponibles.\n")
  cat("  Ya puedes ejecutar:  Rscript run_all.R\n")
} else {
  cat("✗ No se han podido cargar:", paste(bad, collapse = ", "), "\n")
  if (any(c("ompr", "ompr.roi", "ROI.plugin.glpk") %in% bad)) {
    cat("\n  Los paquetes de optimización requieren GLPK en el sistema.\n")
    cat("    Ubuntu/Debian : sudo apt-get install -y libglpk-dev\n")
    cat("    macOS         : brew install glpk\n")
    cat("  Instala GLPK y vuelve a ejecutar este script.\n")
  }
  quit(status = 1)
}
