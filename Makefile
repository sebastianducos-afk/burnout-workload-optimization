# =============================================================================
# Makefile — atajos para preparar y ejecutar el pipeline.
#   make setup    -> instala GLPK (sistema) + paquetes de R
#   make deps     -> instala solo los paquetes de R
#   make run      -> ejecuta el pipeline completo (Fases 1-7)
#   make clean    -> borra todas las salidas generadas
# =============================================================================

.PHONY: setup deps run clean

setup:
	bash setup.sh

deps:
	Rscript install_dependencies.R

run:
	Rscript run_all.R

clean:
	rm -rf "datos sinteticos" "figures fase 2" "data fase 3" \
	       "fase 4" "fase 5" "fase_06_optimizacion" "fase_07_evaluacion" \
	       Rplots.pdf
	@echo "Salidas eliminadas."
