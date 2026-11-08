# EYP3407 · Trabajo final · Caso 06: gravedad de colisiones viales registradas (STATS19)

**Integrantes:** Guillermo Cruz, Lucas Pastrián, Sebastián Pavéz, Constanza Sepúlveda
**Curso:** Tópicos Aplicados en Estadística (EYP3407)
**Pregunta:** ¿qué características distinguen colisiones graves o fatales dentro de las colisiones registradas? (clasificación retrospectiva; Gran Bretaña, 2021–2023, fuerzas policiales 1, 6 y 13).

## Contenido

```
README.md          # este archivo
analisis.R         # punto de entrada: ejecuta todo (preparación, ajuste, evaluación e informe)
preparacion.R      # auditoría de integridad y construcción de la base analítica (la llama analisis.R)
informe.qmd        # fuente del informe (Quarto -> PDF), lee las tablas de salidas/
estilo.typ         # estilo del informe (formato tipográfico); lo usa informe.qmd
logo_puc.png       # logo de la portada del informe
datos/             # copia del paquete del curso, sin modificar
salidas/           # se genera al ejecutar (tablas .csv, figuras .png, predicciones, registro)
informe.pdf        # se genera al ejecutar
```

## Datos

- **Paquete usado:** `caso06_stats19.zip`, entregado por el curso EYP3407 en Canvas (copia de datos preparada el 12 de octubre de 2026). Contiene `colisiones.csv`, `vehiculos.csv`, `victimas.csv`, `stats19_diccionario.xlsx` y `LEEME.txt`. Subconjunto: años 2021–2023 y fuerzas policiales con códigos 1, 6 y 13.
- **Fuente original y versión:** Department for Transport (Reino Unido), Road Safety Open Data (STATS19): archivos anuales 2021–2023 de colisiones, vehículos y víctimas, y guía de datos 2025 (diccionario). Las URL exactas están en `datos/LEEME.txt`.
- **Cómo obtenerlos:** los cinco archivos del paquete están incluidos, sin modificar, en la carpeta `datos/` de este repositorio. Si se parte del `.zip` original, basta descomprimirlo dentro de `datos/`. No es necesario descargar de nuevo desde el portal de origen; los portales pueden revisar sus datos después de la fecha de la copia.
- **Condiciones de reutilización:** datos del DfT bajo Open Government Licence v3.0. *Contains public sector information licensed under the Open Government Licence v3.0.* https://www.nationalarchives.gov.uk/doc/open-government-licence/version/3/ (verificar la licencia vigente en el sitio del productor antes de publicar). El paquete del curso es un subconjunto sin modificar de esos datos; la licencia permite su redistribución con la atribución indicada.

## Entorno

Probado con (Ubuntu 26.04 sobre WSL2):

| Componente | Versión |
|---|---|
| R | 4.5.2 |
| data.table | 1.17.0 |
| readxl | 1.4.5 |
| glmnet | 4.1.10 |
| Matrix | 1.7.4 |
| rpart | 4.1.24 |
| ranger | 0.18.0 |
| ggplot2 | 4.0.2 |
| knitr / rmarkdown | 1.51 / 2.30 |
| Quarto | 1.11.9 (incluye Typst 0.15.1 para generar el PDF; no requiere LaTeX) |
| Fuente del informe | Liberation Serif (si falta, Typst usa otra fuente y avisa) |

Con otras versiones el análisis debería ejecutarse igual, pero los resultados pueden diferir en decimales (sobre todo los del bosque aleatorio, `ranger`).

Instalación de paquetes de R (cualquier sistema):

```r
install.packages(c("data.table", "readxl", "glmnet", "Matrix", "rpart", "ranger",
                   "ggplot2", "knitr", "rmarkdown"))
```

Alternativa en Ubuntu/Debian (paquetes ya compilados, más rápida; incluye R y la fuente del informe):

```bash
sudo apt install -y --no-install-recommends r-base r-cran-data.table r-cran-readxl \
  r-cran-glmnet r-cran-ranger r-cran-ggplot2 r-cran-knitr r-cran-rmarkdown fonts-liberation
```

Quarto se instala desde <https://quarto.org>. Si Quarto no está instalado, `analisis.R` ejecuta todo el análisis y deja las tablas y figuras en `salidas/`, pero no genera el informe (aparece un aviso).

## Ejecución

Desde la raíz del repositorio, con los datos en `datos/`:

```bash
Rscript --vanilla analisis.R
```

Un único comando ejecuta: auditoría y base analítica → validación temporal en desarrollo (2021–2022) → selección → reajuste → evaluación única en prueba (2023) → incertidumbre, diagnóstico y sensibilidades → figuras y tablas → `informe.pdf`.

Variables de entorno opcionales: `STATS19_DIR` (carpeta de entradas, por defecto `datos`), `STATS19_OUT` (carpeta de salidas, por defecto `salidas`) y `STATS19_THREADS` (hilos de `ranger`, por defecto 1).

**Control de ejecución.** Las semillas están fijadas (2026). Los hilos de `ranger` se fijan en 1 por defecto para que los resultados sean idénticos en cualquier computador; subir `STATS19_THREADS` acelera el ajuste pero puede cambiar los resultados en decimales finales. Las reglas de partición están en `analisis.R` (sección 4) y las predicciones de prueba se guardan en `salidas/predicciones_prueba.csv.gz`, desde las cuales se calculan las métricas. Tiempo aproximado: 9 minutos (ver registro más abajo).

## Registro de reproducción desde copia limpia

- **Fecha:** 8 de noviembre de 2026, de 17:58 a 18:07 (hora de Chile, UTC−3).
- **Realizada por:** Lucas Pastrián, en su computador (distinto del usado durante el desarrollo).
- **Sistema y recursos:** Ubuntu 26.04 sobre WSL2, 4 núcleos (`ranger` con 1 hilo), 7 GB de memoria; versiones de la tabla de la sección Entorno.
- **Copia limpia:** carpeta del repositorio con los cinco archivos del paquete en `datos/`, sin `salidas/`, sin `informe.pdf` ni otros archivos generados previamente, y sin objetos de sesiones previas.
- **Comando:** `Rscript --vanilla analisis.R`
- **Resultado:** ejecución completa sin errores ni pasos manuales en 8,8 minutos (8,7 de análisis y el resto del informe); generó 26 archivos en `salidas/` e `informe.pdf` (14 páginas, con anexo).

## Decisiones y trazabilidad

- Las tres tablas se leen como texto (`collision_index` mezcla letras y dígitos); los vehículos se agregan a nivel de colisión **antes** de unir, y la tabla de víctimas se usa solo para auditoría.
- Se usan las variables `*_historic` (formato 2011), completas en 2021–2023.
- La reserva de prueba (2023) se evaluó una primera vez durante el desarrollo del código, con una búsqueda de hiperparámetros más reducida. Después, con resultados **de desarrollo**, se amplió la grilla (la mejor configuración estaba en el borde) y se añadió la referencia por fuerza. Los resultados de regresión logística y bosque no cambiaron; los del árbol cambiaron levemente. Está explicado en la sección 5.6 del informe.

## Aportes del equipo

- **Guillermo Cruz:** diseño de la evaluación (particiones, métricas, referencia y regla de selección) y redacción del informe.
- **Lucas Pastrián:** preparación y auditoría de los datos; repositorio y reproducibilidad (README, versiones y prueba desde copia limpia).
- **Sebastián Pavéz:** preparación y auditoría de los datos; revisión general del trabajo frente a la pauta.
- **Constanza Sepúlveda:** diseño de la evaluación y presentación.
- **Todos:** ajuste y comparación de modelos; incertidumbre, diagnóstico y análisis de sensibilidad.

## Declaración de uso de IA

- **Herramienta:** Claude (Anthropic).
- **Fechas:** 3, 4 y 8 de noviembre de 2026.
- **Propósito:** programación y depuración de `preparacion.R` y `analisis.R`, redacción de `informe.qmd` y revisión de la estructura frente a la pauta.
- **Partes afectadas:** código de preparación y análisis (`preparacion.R`, `analisis.R`), informe (`informe.qmd`) y documentación del repositorio (`README.md`).
- **Comprobaciones realizadas:** ejecución completa del código sobre los datos entregados, incluida una ejecución desde copia limpia con el comando anterior; verificación de integridad de claves y conteos; las cifras del informe se leen de las tablas generadas.
- **Responsabilidad:** el equipo revisó el código, los resultados, las afirmaciones y las referencias, y puede explicarlos en la defensa.
