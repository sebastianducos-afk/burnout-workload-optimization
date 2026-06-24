#!/usr/bin/env bash
# =============================================================================
# setup.sh — Instala las dependencias de SISTEMA (GLPK) y de R.
# Detecta el sistema operativo e instala GLPK; luego instala los paquetes de R.
#
#   Uso:  bash setup.sh
# =============================================================================
set -euo pipefail

echo "==> 1/2  Instalando GLPK (solver para Fases 6 y 7)..."

if [[ "$(uname)" == "Darwin" ]]; then
  # macOS
  if command -v brew >/dev/null 2>&1; then
    brew install glpk || true
  else
    echo "    Homebrew no encontrado. Instálalo desde https://brew.sh y reintenta."
    exit 1
  fi
elif command -v apt-get >/dev/null 2>&1; then
  # Debian / Ubuntu
  sudo apt-get update
  sudo apt-get install -y libglpk-dev
elif command -v dnf >/dev/null 2>&1; then
  # Fedora / RHEL
  sudo dnf install -y glpk-devel
elif command -v pacman >/dev/null 2>&1; then
  # Arch
  sudo pacman -S --noconfirm glpk
else
  echo "    No se ha detectado un gestor de paquetes compatible."
  echo "    Instala GLPK manualmente (paquete de desarrollo) y reintenta."
fi

echo "==> 2/2  Instalando paquetes de R..."
if ! command -v Rscript >/dev/null 2>&1; then
  echo "    Rscript no está en el PATH. Instala R (>= 4.1) desde https://cran.r-project.org"
  exit 1
fi
Rscript install_dependencies.R

echo ""
echo "✓ Setup completado. Ahora ejecuta:  Rscript run_all.R"
