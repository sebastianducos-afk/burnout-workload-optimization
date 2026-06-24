# Predicción de Burnout y Optimización de Carga de Trabajo

Pipeline completo en **R** que combina **Machine Learning** y **Optimización (Investigación Operativa)** para predecir el riesgo de *burnout* de empleados y redistribuir su carga de trabajo de forma óptima.

El proyecto está organizado en **7 fases encadenadas**: cada fase consume las salidas de las anteriores. Cualquier persona puede clonar el repositorio, instalar las dependencias y reproducir **todos** los resultados con un único comando.

> Trabajo desarrollado como Trabajo de Fin de Grado (TFG) en Inteligencia y Analítica de Negocios.

---

## Tabla de contenidos

- [Arquitectura del pipeline](#arquitectura-del-pipeline)
- [Requisitos](#requisitos)
- [Instalación rápida](#instalación-rápida)
- [Cómo ejecutar](#cómo-ejecutar)
- [Estructura del repositorio](#estructura-del-repositorio)
- [Salidas generadas](#salidas-generadas)
- [Resultados de referencia](#resultados-de-referencia)
- [Solución de problemas](#solución-de-problemas)
- [Licencia](#licencia)

---

## Arquitectura del pipeline

```
 Fase 1  Generación del dataset sintético        ─┐
         (600 empleados × 52 semanas, AR(1))      │  datos sinteticos/*.csv
                                                   ▼
 Fase 2  Análisis exploratorio (EDA)             ── figures fase 2/
                                                   │
 Fase 3  Preprocesado con {recipes}              ─┤  data fase 3/{train,validation,test}.csv
         (train/val/test + pipeline + operational)│  data fase 3/prep_pipeline.rds
                                                   ▼  data fase 3/operational.csv
 Fase 4  Modelado predictivo del burnout         ── fase 4/models/final_model.rds
         (Regresión Logística, Random Forest,     │  fase 4/reports, fase 4/figures
          XGBoost — selección del mejor)          │
                                                   ▼
 Fase 5  Interpretabilidad del modelo            ── fase 5/reports, fase 5/figures
         (importancia, contribuciones, subgrupos) │
                                                   ▼
 Fase 6  Baseline OR clásico (staffing)          ── fase_06_optimizacion/
         (programación lineal con GLPK)           │
                                                   ▼
 Fase 7  OR + ML: redistribución de carga        ── fase_07_evaluacion/
         (proxy de burnout, análisis de gamma)    │
```

Cada fase escribe en su propia carpeta y la siguiente la lee. El orden es **estricto**: la Fase *n* necesita la salida de la *n-1*.

---

## Requisitos

| Componente | Versión recomendada | Notas |
|------------|---------------------|-------|
| **R**      | ≥ 4.1               | https://cran.r-project.org |
| **GLPK**   | cualquiera reciente | Librería de sistema para el solver (Fases 6 y 7) |
| RStudio    | opcional            | Cómodo, pero no necesario |

### Paquetes de R

`dplyr`, `tidyr`, `purrr`, `stringr`, `ggplot2`, `corrplot`, `patchwork`, `scales`, `recipes`, `pROC`, `ranger`, `xgboost`, `ompr`, `ompr.roi`, `ROI.plugin.glpk`.

Se instalan automáticamente con `install_dependencies.R` (ver más abajo).

> **Importante:** `ROI.plugin.glpk` necesita la librería **GLPK** instalada en el sistema operativo *antes* de instalarlo. Las instrucciones por SO están justo debajo.

---

## Instalación rápida

### Opción A — Script automático (Linux / macOS)

```bash
git clone https://github.com/<tu-usuario>/burnout-workload-optimization.git
cd burnout-workload-optimization
bash setup.sh        # instala GLPK + paquetes de R
```

### Opción B — Manual (los tres sistemas)

1. **Instala GLPK** (librería de sistema):

   ```bash
   # Ubuntu / Debian
   sudo apt-get update && sudo apt-get install -y libglpk-dev

   # macOS (con Homebrew)
   brew install glpk

   # Fedora / RHEL
   sudo dnf install -y glpk-devel
   ```

   **Windows:** instala [Rtools](https://cran.r-project.org/bin/windows/Rtools/) (incluye el toolchain necesario). `install.packages("ROI.plugin.glpk")` compilará GLPK automáticamente.

2. **Instala los paquetes de R:**

   ```bash
   Rscript install_dependencies.R
   ```

   (o, dentro de R/RStudio: `source("install_dependencies.R")`)

---

## Cómo ejecutar

### Todo el pipeline de una vez

```bash
Rscript run_all.R
```

`run_all.R` ejecuta las Fases 1 → 7 en orden, fija automáticamente el directorio de trabajo a la raíz del repositorio y se detiene con un mensaje claro si alguna fase falla.

Con `make`:

```bash
make setup   # GLPK + paquetes de R   (equivale a bash setup.sh)
make run     # ejecuta run_all.R
make clean   # borra todas las salidas generadas
```

### Una fase concreta

Las fases usan rutas relativas a la **raíz del repositorio**, así que ejecútalas desde ahí:

```bash
# desde la carpeta raíz del repo
Rscript scripts/Fase_1.R
Rscript scripts/Fase_2.R
# ...
```

En RStudio: abre el proyecto en la carpeta raíz, comprueba con `getwd()` que estás en la raíz y haz `source("scripts/Fase_4.R")`.

> Si ejecutas una fase suelta, recuerda que necesita las salidas de las fases anteriores. La forma más segura de reproducir todo desde cero es `Rscript run_all.R`.

---

## Estructura del repositorio

```
burnout-workload-optimization/
├── README.md
├── LICENSE
├── .gitignore
├── .gitattributes
├── Makefile                     # atajos: setup / deps / run / clean
├── setup.sh                     # instala GLPK + paquetes de R (Linux/macOS)
├── install_dependencies.R       # instala/verifica los paquetes de R
├── run_all.R                    # orquesta las Fases 1-7 en orden
├── scripts/
│   ├── Fase_1.R                 # generación del dataset sintético
│   ├── Fase_2.R                 # EDA
│   ├── Fase_3.R                 # preprocesado (recipes)
│   ├── Fase_4.R                 # modelado predictivo
│   ├── Fase_5.R                 # interpretabilidad
│   ├── Fase_6.R                 # OR clásico (optimización)
│   └── Fase_7.R                 # OR + ML (redistribución de carga)
└── .github/workflows/
    └── run-pipeline.yml         # CI: ejecuta el pipeline completo en Ubuntu
```

Las carpetas de **salida** (`datos sinteticos/`, `data fase 3/`, `fase 4/`, etc.) **no** se versionan: se regeneran al ejecutar el pipeline (ver `.gitignore`).

---

## Salidas generadas

| Carpeta | Contenido |
|---------|-----------|
| `datos sinteticos/` | `teams.csv`, `employees.csv`, `organizational_events.csv`, `weekly_workload.csv` |
| `figures fase 2/` | Gráficos del análisis exploratorio (PNG) |
| `data fase 3/` | `train.csv`, `validation.csv`, `test.csv`, `operational.csv`, `prep_pipeline.rds` |
| `fase 4/` | `models/final_model.rds`, informe metodológico, curvas ROC, comparativa de modelos |
| `fase 5/` | Importancia de variables, contribuciones, riesgo por departamento y job level |
| `fase_06_optimizacion/` | Asignación y comparativa del OR clásico + informe |
| `fase_07_evaluacion/` | Redistribución OR+ML, análisis de sensibilidad a *gamma*, informe ejecutivo |

---

## Resultados de referencia

Como el dataset es sintético y todas las fases fijan `set.seed(42)`, los resultados son **reproducibles**. Valores indicativos del modelo y la optimización (pueden variar ligeramente según versiones de paquetes):

- **Modelo final:** Regresión Logística — AUC ≈ 0.99, F1 ≈ 0.80, *recall* ≈ 79 %, *precision* ≈ 83 %.
- **Optimización OR+ML:** reducción de empleados de alto riesgo (≈ −16 %) mediante redistribución de carga hacia empleados con capacidad disponible; *gamma* = 2 como punto de equilibrio recomendado.

Consulta los informes generados en `fase 4/reports/`, `fase 5/reports/` y `fase_07_evaluacion/reports/` para las cifras exactas de tu ejecución.

---

## Solución de problemas

**`ROI.plugin.glpk` no se instala / no carga.**
Falta GLPK en el sistema. Instálalo (`libglpk-dev` en Ubuntu, `glpk` en macOS, Rtools en Windows) y vuelve a ejecutar `Rscript install_dependencies.R`.

**`No such file or directory: datos sinteticos/teams.csv` (o similar).**
Estás ejecutando una fase sin haber generado las salidas previas, o el directorio de trabajo no es la raíz del repo. Solución: ejecuta `Rscript run_all.R` desde la raíz, o lanza las fases en orden desde la raíz.

**`xgboost` falla al compilar en macOS.**
Suele resolverse con `brew install libomp`. Como alternativa, instala el binario: `install.packages("xgboost", type = "binary")`.

**La Fase 1 tarda.**
Genera 600 × 52 = 31 200 registros con dinámica AR(1) mediante bucles en R; es la fase más lenta. Es normal que tarde algo más que el resto.

---

## Licencia

[MIT](LICENSE).
