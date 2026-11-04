# EYP3407 · Trabajo final · Caso 06: gravedad de colisiones viales registradas (STATS19)

**Integrantes:** Guillermo Cruz, Lucas Pastrián, Sebastián Pavés, Constanza Sepúlveda
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
presentacion.pdf   # diapositivas del equipo
```

## Datos

- **Paquete usado:** `caso06_stats19.zip`, entregado por el curso EYP3407 en Canvas (copia de datos preparada el 12 de octubre de 2026). Contiene `colisiones.csv`, `vehiculos.csv`, `victimas.csv`, `stats19_diccionario.xlsx` y `LEEME.txt`. Subconjunto: años 2021–2023 y fuerzas policiales con códigos 1, 6 y 13.
- **Fuente original y versión:** Department for Transport (Reino Unido), Road Safety Open Data (STATS19): archivos anuales 2021–2023 de colisiones, vehículos y víctimas, y guía de datos 2025 (diccionario). Las URL exactas están en `datos/LEEME.txt`.
- **Cómo obtenerlos:** los cinco archivos del paquete están incluidos, sin modificar, en la carpeta `datos/` de este repositorio. Si se parte del `.zip` original, basta descomprimirlo dentro de `datos/`. No es necesario descargar de nuevo desde el portal de origen; los portales pueden revisar sus datos después de la fecha de la copia.
- **Condiciones de reutilización:** datos del DfT bajo Open Government Licence v3.0. *Contains public sector information licensed under the Open Government Licence v3.0.* https://www.nationalarchives.gov.uk/doc/open-government-licence/version/3/ (verificar la licencia vigente en el sitio del productor antes de publicar). El paquete fue armado por el curso; [COMPLETAR: confirmar con el profesor o el ayudante que puede publicarse].

## Entorno

Probado con:

| Componente | Versión |
|---|---|
| R | 4.3.3 |
| data.table | 1.14.10 |
| readxl | 1.4.3 |
| glmnet | 4.1.8 |
| Matrix | 1.6.5 |
| rpart | 4.1.23 |
| ranger | 0.16.0 |
| ggplot2 | 3.4.4 |
| knitr / rmarkdown | 1.45 / (instalado con R) |
| Quarto | 1.11.9 (incluye Typst para generar el PDF; no requiere LaTeX) |

Instalación de paquetes de R:

```r
install.packages(c("data.table", "readxl", "glmnet", "Matrix", "rpart", "ranger",
                   "ggplot2", "knitr", "rmarkdown"))
```

Quarto se instala desde <https://quarto.org>. Si Quarto no está instalado, `analisis.R` ejecuta todo el análisis y deja las tablas y figuras en `salidas/`, pero no genera el informe (aparece un aviso).

## Ejecución

Desde la raíz del repositorio, con los datos en `datos/`:

```bash
Rscript --vanilla analisis.R
```

Un único comando ejecuta: auditoría y base analítica → validación temporal en desarrollo (2021–2022) → selección → reajuste → evaluación única en prueba (2023) → incertidumbre, diagnóstico y sensibilidades → figuras y tablas → `informe.pdf`.

Variables de entorno opcionales: `STATS19_DIR` (carpeta de entradas, por defecto `datos`), `STATS19_OUT` (carpeta de salidas, por defecto `salidas`) y `STATS19_THREADS` (hilos de `ranger`, por defecto 1).

**Control de ejecución.** Las semillas están fijadas (2026). Los hilos de `ranger` se fijan en 1 por defecto para que los resultados sean idénticos en cualquier computador; subir `STATS19_THREADS` acelera el ajuste pero puede cambiar los resultados en decimales finales. Las reglas de partición están en `analisis.R` (sección 4) y las predicciones de prueba se guardan en `salidas/predicciones_prueba.csv.gz`, desde las cuales se calculan las métricas. Tiempo aproximado: ver registro más abajo (≈ 9 minutos con un núcleo y 4 GB de memoria).

## Registro de reproducción desde copia limpia CAMBIAR ESTO A UN COSO REAL

- **Fecha:** 4 de noviembre de 2026 (02:31 a 02:39 UTC).
- **Entorno:** Linux (Ubuntu 24), 1 núcleo, 4 GB de memoria, R 4.3.3, Quarto 1.11.9.
- **Comando:** `Rscript --vanilla analisis.R`, desde una copia limpia armada con el contenido de `repositorio_caso06.zip` y los cinco archivos del paquete en `datos/`, sin objetos de sesiones previas.
- **Resultado:** ejecución completa sin errores en 6,7 minutos; generó 26 archivos en `salidas/` y `informe.pdf` (14 páginas, con anexo). Las tablas de selección, de prueba, de incertidumbre y de confusión resultaron idénticas, byte a byte, a las de una ejecución anterior independiente (8,3 minutos).
- **Realizada por:** Claude en un entorno de pruebas, no por el equipo.

**Pendiente para el equipo:** repetir esta prueba en un entorno nuevo o en el computador de otro integrante y anotar aquí fecha, comando y resultado, como exige la pauta.

## Decisiones y trazabilidad

- Las tres tablas se leen como texto (`collision_index` mezcla letras y dígitos); los vehículos se agregan a nivel de colisión **antes** de unir, y la tabla de víctimas se usa solo para auditoría.
- Se usan las variables `*_historic` (formato 2011), completas en 2021–2023.
- La reserva de prueba (2023) se evaluó una primera vez durante el desarrollo del código, con una búsqueda de hiperparámetros más reducida. Después, con resultados **de desarrollo**, se amplió la grilla (la mejor configuración estaba en el borde) y se añadió la referencia por fuerza. Los resultados de regresión logística y bosque no cambiaron; los del árbol cambiaron levemente. Está explicado en la sección 5.6 del informe.

## Aportes del equipo

[COMPLETAR]

## Declaración de uso de IA

- **Herramienta:** Claude (Anthropic).
- **Fechas:** [COMPLETAR]
- **Propósito:** programación y depuración de `preparacion.R` y `analisis.R`, redacción de `informe.qmd` y revisión de la estructura frente a la pauta.
- **Partes afectadas:** COMPLETAR
- **Comprobaciones realizadas:** ejecución completa del código sobre los datos entregados, incluida una ejecución desde copia limpia con el comando anterior; verificación de integridad de claves y conteos; las cifras del informe se leen de las tablas generadas.
- **Responsabilidad:** el equipo debe revisar el código, los resultados, las afirmaciones y las referencias, y poder explicarlos en la defensa [COMPLETAR: confirmación del equipo].
