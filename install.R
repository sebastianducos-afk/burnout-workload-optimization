# install.R — lo ejecuta Binder al construir el entorno.
# Instala los paquetes de R con el repositorio binario que configura Binder
# (rápido). GLPK ya está disponible gracias a apt.txt (libglpk-dev).

pkgs <- c(
  "dplyr", "tidyr", "purrr", "stringr",
  "ggplot2", "corrplot", "patchwork", "scales",
  "recipes",
  "pROC", "ranger", "xgboost",
  "ompr", "ompr.roi", "ROI.plugin.glpk"
)

install.packages(pkgs)

# Verificación: si algo no carga, la construcción falla con un mensaje claro.
ok  <- vapply(pkgs, requireNamespace, logical(1), quietly = TRUE)
if (any(!ok)) stop("No se pudieron instalar: ", paste(pkgs[!ok], collapse = ", "))
