# =============================================================================
# EYP3407 - Caso 06: STATS19 (Gran Bretania, 2021-2023, fuerzas 1, 6 y 13)
# preparacion.R : auditoria de integridad, construccion de la base analitica
#                 (una fila por colision) y tablas de apoyo para el informe.
#
# Uso (desde la raiz del repositorio, con los CSV en datos/):
#   Rscript --vanilla preparacion.R
# Opcional: STATS19_DIR=<carpeta con los CSV>  STATS19_OUT=<carpeta de salidas>
#
# Paquetes: data.table, readxl
# Salidas (en salidas/):
#   tabla_integridad.csv         chequeos de claves y conteos entre tablas
#   tabla_construccion.csv       filas y unidades en cada paso (tabla del informe)
#   tabla_predictores.csv        auditoria de TODAS las columnas de colisiones y
#                                vehiculos (nombre, interpretacion, tipo,
#                                disponibilidad, decision)
#   tabla_prevalencia.csv        respuesta por anio y fuerza
#   tabla_medicion.csv           collision_injury_based por fuerza y anio
#   tabla_codigos_especiales.csv codigos -1 / 9 / 99 por predictor
#   base_analitica.rds           lista(base, grupos, particion) para los modelos
# =============================================================================

suppressPackageStartupMessages({
  library(data.table)
  library(readxl)
})

DIR_DATOS  <- Sys.getenv("STATS19_DIR", "datos")
DIR_SALIDA <- Sys.getenv("STATS19_OUT", "salidas")
dir.create(DIR_SALIDA, showWarnings = FALSE, recursive = TRUE)
ruta <- function(f) file.path(DIR_DATOS, f)
salida <- function(f) file.path(DIR_SALIDA, f)

FUERZAS <- c("1", "6", "13")   # Metropolitan, Greater Manchester, West Yorkshire
ANIOS   <- c("2021", "2022", "2023")

# -----------------------------------------------------------------------------
# 1. Lectura. TODO como texto: collision_index mezcla letras y digitos (p. ej.
#    "202106K079841") y una inferencia automatica de tipos genera cruces falsos.
# -----------------------------------------------------------------------------
leer <- function(f) fread(ruta(f), colClasses = "character", na.strings = "",
                          encoding = "UTF-8")
col <- leer("colisiones.csv")
veh <- leer("vehiculos.csv")
vic <- leer("victimas.csv")

# -----------------------------------------------------------------------------
# 2. Auditoria de integridad
# -----------------------------------------------------------------------------
n_veh_tab <- veh[, .N, by = collision_index]
n_vic_tab <- vic[, .N, by = collision_index]
cmp_veh <- merge(col[, .(collision_index, nv = as.integer(number_of_vehicles))],
                 n_veh_tab, by = "collision_index", all.x = TRUE)
cmp_vic <- merge(col[, .(collision_index, nc = as.integer(number_of_casualties))],
                 n_vic_tab, by = "collision_index", all.x = TRUE)

integridad <- data.table(
  chequeo = c(
    "colisiones: filas",
    "colisiones: collision_index unicos",
    "colisiones: anios fuera de 2021-2023",
    "colisiones: fuerzas fuera de {1,6,13}",
    "colisiones: anio de 'date' distinto de collision_year",
    "vehiculos: filas",
    "vehiculos: duplicados (collision_index, vehicle_reference)",
    "vehiculos: filas cuya colision no esta en colisiones.csv",
    "colisiones sin ningun vehiculo",
    "colisiones con number_of_vehicles distinto del conteo en vehiculos",
    "victimas: filas",
    "victimas: duplicados (collision_index, casualty_reference)",
    "victimas: filas cuya colision no esta en colisiones.csv",
    "colisiones sin ninguna victima",
    "colisiones con number_of_casualties distinto del conteo en victimas",
    "filas que produciria la union directa vehiculos x victimas por colision"
  ),
  valor = c(
    nrow(col),
    uniqueN(col$collision_index),
    sum(!col$collision_year %in% ANIOS),
    sum(!col$police_force %in% FUERZAS),
    sum(substr(col$date, 7, 10) != col$collision_year),
    nrow(veh),
    sum(duplicated(veh[, .(collision_index, vehicle_reference)])),
    sum(!veh$collision_index %in% col$collision_index),
    sum(is.na(cmp_veh$N)),
    sum(cmp_veh$nv != cmp_veh$N, na.rm = TRUE) + sum(is.na(cmp_veh$N)),
    nrow(vic),
    sum(duplicated(vic[, .(collision_index, casualty_reference)])),
    sum(!vic$collision_index %in% col$collision_index),
    sum(is.na(cmp_vic$N)),
    sum(cmp_vic$nc != cmp_vic$N, na.rm = TRUE) + sum(is.na(cmp_vic$N)),
    sum(as.numeric(cmp_veh$N) * as.numeric(cmp_vic$N[match(cmp_veh$collision_index,
                                                           cmp_vic$collision_index)]),
        na.rm = TRUE)
  )
)
fwrite(integridad, salida("tabla_integridad.csv"))

# Condiciones que invalidan el resto del analisis
stopifnot(
  uniqueN(col$collision_index) == nrow(col),
  !any(duplicated(veh[, .(collision_index, vehicle_reference)])),
  all(veh$collision_index %in% col$collision_index),
  all(col$collision_year %in% ANIOS),
  all(col$police_force %in% FUERZAS),
  all(col$collision_severity %in% c("1", "2", "3"))
)

# -----------------------------------------------------------------------------
# 3. Respuesta: 1 = fatal o grave (codigos 1 y 2); 0 = leve (codigo 3).
#    No hay valores desconocidos en collision_severity (se verifica arriba).
#    No se usan las columnas ajustadas ni enhanced_* (ver tabla_predictores).
# -----------------------------------------------------------------------------
col[, y := as.integer(collision_severity %in% c("1", "2"))]

# -----------------------------------------------------------------------------
# 4. Agregados de vehiculos a nivel de colision (ANTES de unir; relacion 1:N)
#    Los -1 se tratan como NA y 9/99 como categoria propia ("desconocido").
# -----------------------------------------------------------------------------
num <- function(x) { z <- suppressWarnings(as.integer(x)); z[z == -1L] <- NA; z }
max_na <- function(x) if (all(is.na(x))) NA_integer_ else max(x, na.rm = TRUE)
min_na <- function(x) if (all(is.na(x))) NA_integer_ else min(x, na.rm = TRUE)

veh[, `:=`(
  vt = num(vehicle_type),
  edad_c = num(age_of_driver),
  edad_v = num(age_of_vehicle),
  derrape = skidding_and_overturning %in% as.character(1:5),
  sale_calzada = vehicle_leaving_carriageway %in% as.character(1:8)
)]

agr <- veh[, .(
  # descriptivos del parque involucrado
  n_veh_tabla   = .N,
  n_bici        = sum(vt == 1, na.rm = TRUE),
  n_moto        = sum(vt %in% c(2, 3, 4, 5, 23, 97), na.rm = TRUE),
  n_bus         = sum(vt %in% c(10, 11), na.rm = TRUE),
  n_pesado      = sum(vt %in% c(20, 21, 98), na.rm = TRUE),
  n_tipo_desc   = sum(is.na(vt) | vt == 99),
  edad_cond_min = min_na(edad_c),
  edad_cond_max = max_na(edad_c),
  n_edad_desc   = sum(is.na(edad_c)),
  edad_veh_max  = max_na(edad_v),
  # reconstruccion de la dinamica del choque (post-evento): se separan para la
  # sensibilidad "excluir variables reconstruidas tras el evento"
  n_derrape_vuelco = sum(derrape),
  n_sale_calzada   = sum(sale_calzada)
), by = collision_index]

# -----------------------------------------------------------------------------
# 5. Predictores de la tabla de colisiones
#    Se usan las columnas *_historic (formato 2011): estan completas en 2021-2023
#    y distinguen categorias que el formato 2024 agrupa (p. ej. en junction_detail
#    los codigos 1, 2 y 5 pasan a 0). No se mezclan ambos formatos.
# -----------------------------------------------------------------------------
cat_desc <- function(x) { x[is.na(x) | x == "-1"] <- "desc"; x }

col[, `:=`(
  fecha  = as.IDate(date, format = "%d/%m/%Y"),
  hora   = as.integer(substr(time, 1, 2)),
  speed_limit_num = num(speed_limit),
  number_of_vehicles = as.integer(number_of_vehicles)
)]
col[, `:=`(mes = month(fecha), dia_semana = day_of_week)]

cats <- c("road_type", "first_road_class", "second_road_class",
          "junction_detail_historic", "junction_control",
          "pedestrian_crossing_human_control_historic",
          "pedestrian_crossing_physical_facilities_historic",
          "light_conditions", "weather_conditions", "road_surface_conditions",
          "special_conditions_at_site", "carriageway_hazards_historic",
          "urban_or_rural_area", "trunk_road_flag", "dia_semana")
col[, (cats) := lapply(.SD, function(x) cat_desc(as.character(x))), .SDcols = cats]

# -----------------------------------------------------------------------------
# 6. Union 1:1 (colisiones x agregados) y particion temporal
#    Bloques de validacion: 2021 | 2022H1 | 2022H2 (desarrollo) y 2023 (prueba).
#    Cada colision queda completa en un unico bloque.
# -----------------------------------------------------------------------------
n_antes <- nrow(col)
base <- merge(col, agr, by = "collision_index", all.x = TRUE)
stopifnot(nrow(base) == n_antes, !anyNA(base$n_veh_tabla))
setorder(base, fecha, collision_index)

base[, periodo := fifelse(collision_year == "2023", "prueba", "desarrollo")]
base[, bloque := fcase(
  collision_year == "2021", "2021",
  collision_year == "2022" & mes <= 6, "2022H1",
  collision_year == "2022", "2022H2",
  default = "2023")]
base[, fuerza := factor(police_force, levels = FUERZAS,
                        labels = c("1_Metropolitan", "6_GreaterManchester",
                                   "13_WestYorkshire"))]

# Grupos de variables para modelos y sensibilidades
grupos <- list(
  colision_cat = cats,
  colision_num = c("speed_limit_num", "number_of_vehicles", "hora", "mes"),
  vehiculos_descriptivos = c("n_bici", "n_moto", "n_bus", "n_pesado", "n_tipo_desc",
                             "edad_cond_min", "edad_cond_max", "n_edad_desc",
                             "edad_veh_max"),
  vehiculos_dinamica_reconstruida = c("n_derrape_vuelco", "n_sale_calzada"),
  agrupacion_geografica = c("local_authority_ons_district", "local_authority_highway"),
  diseno = c("fuerza", "collision_injury_based", "periodo", "bloque", "fecha")
)
# number_of_vehicles se verifica contra la tabla de vehiculos (0 discrepancias)
# y es conocido al registrar la colision.

# -----------------------------------------------------------------------------
# 7. Tablas para el informe
# -----------------------------------------------------------------------------
constr <- data.table(
  paso = c(
    "1. colisiones.csv (2021-2023, fuerzas 1, 6, 13)",
    "2. vehiculos.csv",
    "3. victimas.csv (solo auditoria)",
    "4. union directa vehiculos x victimas (NO se usa)",
    "5. vehiculos agregados a 1 fila por colision",
    "6. colisiones x agregados (union 1:1)",
    "7. con respuesta valida (collision_severity en 1, 2, 3)",
    "8. base de desarrollo (2021-2022)",
    "9. base de prueba (2023)"
  ),
  filas = c(nrow(col), nrow(veh), nrow(vic),
            integridad[chequeo == chequeo[16], valor],
            nrow(agr), nrow(base), sum(base$collision_severity %in% c("1", "2", "3")),
            sum(base$periodo == "desarrollo"), sum(base$periodo == "prueba")),
  colisiones_unicas = c(uniqueN(col$collision_index), uniqueN(veh$collision_index),
                        uniqueN(vic$collision_index), NA, uniqueN(agr$collision_index),
                        uniqueN(base$collision_index), uniqueN(base$collision_index),
                        uniqueN(base[periodo == "desarrollo"]$collision_index),
                        uniqueN(base[periodo == "prueba"]$collision_index)),
  casos_y1 = c(sum(col$y), NA, NA, NA, NA, sum(base$y), sum(base$y),
               sum(base[periodo == "desarrollo"]$y), sum(base[periodo == "prueba"]$y))
)
constr[, prevalencia := round(casos_y1 / colisiones_unicas, 4)]
fwrite(constr, salida("tabla_construccion.csv"))

prev <- base[, .(n = .N, graves_fatales = sum(y), fatales = sum(collision_severity == "1"),
                 prevalencia = round(mean(y), 4)),
             by = .(collision_year, fuerza)][order(collision_year, fuerza)]
fwrite(prev, salida("tabla_prevalencia.csv"))

med <- base[, .N, by = .(fuerza, collision_year, collision_injury_based)]
med[, prop := round(N / sum(N), 4), by = .(fuerza, collision_year)]
fwrite(med[order(fuerza, collision_year, collision_injury_based)],
       salida("tabla_medicion.csv"))

esp <- rbindlist(lapply(c(cats[cats != "dia_semana"]), function(v)
  data.table(variable = v,
             prop_desc = round(mean(base[[v]] == "desc"), 4),
             prop_9_99 = round(mean(base[[v]] %in% c("9", "99")), 4))))
esp <- rbind(esp,
  data.table(variable = "edad_cond_min (NA si todas desconocidas)",
             prop_desc = round(mean(is.na(base$edad_cond_min)), 4), prop_9_99 = NA),
  data.table(variable = "edad_veh_max (NA si todas desconocidas)",
             prop_desc = round(mean(is.na(base$edad_veh_max)), 4), prop_9_99 = NA),
  data.table(variable = "speed_limit_num",
             prop_desc = round(mean(is.na(base$speed_limit_num)), 4), prop_9_99 = NA))
fwrite(esp, salida("tabla_codigos_especiales.csv"))

# -----------------------------------------------------------------------------
# 8. Auditoria de predictores: TODAS las columnas de colisiones y vehiculos
#    Editar 'decision' aqui si el equipo cambia un criterio; el script falla si
#    queda una columna sin clasificar.
# -----------------------------------------------------------------------------
A <- function(tabla, nombre, interp, tipo, disp, decision)
  data.table(tabla, nombre, interp, tipo, disp, decision)

IDt <- "identificador"; RES <- "respuesta"; FUGA <- "derivada/consecuencia de la respuesta"
aud <- rbindlist(list(
  A("colisiones","collision_index",IDt,"texto","registro","Excluida: clave de union"),
  A("colisiones","collision_ref_no",IDt,"texto","registro","Excluida: clave"),
  A("colisiones","collision_year","anio de la colision","entero","registro","Solo particion temporal; no predictor"),
  A("colisiones","date","fecha","fecha","registro","Se usa para mes y particion"),
  A("colisiones","time","hora","texto","registro","Se usa como hora (entero); incluida"),
  A("colisiones","day_of_week","dia (1 = domingo)","categorica","registro","Incluida (dia_semana)"),
  A("colisiones","collision_severity",RES,"categorica","resultado","Respuesta (1,2 -> 1; 3 -> 0)"),
  A("colisiones","enhanced_severity_collision",FUGA,"categorica","posterior","Excluida: fuga (78% = -1)"),
  A("colisiones","collision_injury_based","sistema de reporte de gravedad","binaria","registro","No predictor: variable de diseno/estratificacion y sensibilidad"),
  A("colisiones","collision_adjusted_severity_serious",FUGA,"numerica","posterior","Excluida: fuga (ajuste de gravedad)"),
  A("colisiones","collision_adjusted_severity_slight",FUGA,"numerica","posterior","Excluida: fuga (ajuste de gravedad)"),
  A("colisiones","number_of_casualties","numero de lesionados","entero","posterior","Excluida: consecuencia de la colision"),
  A("colisiones","did_police_officer_attend_scene_of_accident","asistencia policial","categorica","posterior","Excluida del modelo principal: depende de la gravedad"),
  A("colisiones","number_of_vehicles","vehiculos involucrados","entero","registro","Incluida (coincide 100% con tabla vehiculos)"),
  A("colisiones","police_force","fuerza policial","categorica","registro","No predictor en modelo principal: estratificacion y diagnostico (confunde sistema de medicion)"),
  A("colisiones","location_easting_osgr","coordenada","numerica","registro","Excluida: ubicacion fina; 2 NA"),
  A("colisiones","location_northing_osgr","coordenada","numerica","registro","Excluida: ubicacion fina; 2 NA"),
  A("colisiones","longitude","coordenada","numerica","registro","Excluida del modelo principal: ubicacion fina"),
  A("colisiones","latitude","coordenada","numerica","registro","Excluida del modelo principal: ubicacion fina"),
  A("colisiones","local_authority_district","distrito (codigo antiguo)","categorica","registro","Excluida: 100% = -1"),
  A("colisiones","local_authority_ons_district","distrito ONS","categorica","registro","No predictor: unidad de agrupacion para remuestreo"),
  A("colisiones","local_authority_highway","autoridad vial","categorica","registro","No predictor: unidad de agrupacion para remuestreo"),
  A("colisiones","local_authority_highway_current","autoridad vial vigente","categorica","registro","Excluida: redundante"),
  A("colisiones","lsoa_of_accident_location","area censal","categorica","registro","Excluida: alta cardinalidad"),
  A("colisiones","first_road_class","clase de via 1","categorica","registro","Incluida"),
  A("colisiones","first_road_number","numero de via 1","categorica","registro","Excluida: alta cardinalidad"),
  A("colisiones","second_road_class","clase de via 2","categorica","registro","Incluida"),
  A("colisiones","second_road_number","numero de via 2","categorica","registro","Excluida: alta cardinalidad"),
  A("colisiones","road_type","tipo de via","categorica","registro","Incluida"),
  A("colisiones","speed_limit","limite de velocidad (mph)","numerica","registro","Incluida (speed_limit_num)"),
  A("colisiones","junction_detail_historic","interseccion (formato 2011)","categorica","registro","Incluida: completa 2021-2023"),
  A("colisiones","junction_detail","interseccion (formato 2024)","categorica","registro","Excluida: conversion del formato 2011 con perdida de categorias"),
  A("colisiones","junction_control","control de interseccion","categorica","registro","Incluida (~27% -1 -> 'desc')"),
  A("colisiones","pedestrian_crossing_human_control_historic","cruce, control humano (2011)","categorica","registro","Incluida"),
  A("colisiones","pedestrian_crossing_physical_facilities_historic","cruce, instalaciones (2011)","categorica","registro","Incluida"),
  A("colisiones","pedestrian_crossing","cruce (formato 2024)","categorica","registro","Excluida: duplica las dos anteriores"),
  A("colisiones","light_conditions","luz","categorica","registro","Incluida"),
  A("colisiones","weather_conditions","clima","categorica","registro","Incluida"),
  A("colisiones","road_surface_conditions","superficie","categorica","registro","Incluida"),
  A("colisiones","special_conditions_at_site","condiciones especiales","categorica","registro","Incluida"),
  A("colisiones","carriageway_hazards_historic","peligros en calzada (2011)","categorica","registro","Incluida"),
  A("colisiones","carriageway_hazards","peligros en calzada (2024)","categorica","registro","Excluida: duplica la version 2011"),
  A("colisiones","urban_or_rural_area","urbano/rural","categorica","registro","Incluida"),
  A("colisiones","trunk_road_flag","via troncal","categorica","registro","Incluida"),
  A("vehiculos","collision_index",IDt,"texto","registro","Clave de agregacion"),
  A("vehiculos","collision_year","anio","texto","registro","No usada"),
  A("vehiculos","collision_ref_no",IDt,"texto","registro","No usada"),
  A("vehiculos","vehicle_reference","id del vehiculo en la colision","texto","registro","Clave; no predictor"),
  A("vehiculos","vehicle_type","tipo de vehiculo","categorica","registro","Agregado: n_bici, n_moto, n_bus, n_pesado, n_tipo_desc"),
  A("vehiculos","age_of_driver","edad conductor","numerica","registro","Agregado: edad_cond_min/max, n_edad_desc"),
  A("vehiculos","age_of_vehicle","antiguedad vehiculo","numerica","registro","Agregado: edad_veh_max"),
  A("vehiculos","skidding_and_overturning","derrape/vuelco","categorica","reconstruida post-evento","Agregado: n_derrape_vuelco (grupo reconstruido)"),
  A("vehiculos","vehicle_leaving_carriageway","salida de calzada","categorica","reconstruida post-evento","Agregado: n_sale_calzada (grupo reconstruido)")
))
aud[, c("tabla", "nombre") := .(tabla, nombre)]
# columnas no clasificadas -> error (obliga a documentar cada variable)
faltan <- function(tab, f) {
  x <- setdiff(names(fread(ruta(f), nrows = 0)), aud[tabla == tab, nombre])
  data.table(tabla = rep(tab, length(x)), nombre = x)
}
sin_clasif <- rbind(faltan("colisiones", "colisiones.csv"),
                    faltan("vehiculos", "vehiculos.csv"))
# Columnas de vehiculos no usadas deliberadamente (sin agregar): se registran
no_usadas_veh <- sin_clasif[tabla == "vehiculos"]
if (nrow(no_usadas_veh)) {
  aud <- rbind(aud, data.table(tabla = "vehiculos", nombre = no_usadas_veh$nombre,
    interp = "otras variables de vehiculo/conductor", tipo = "varias", disp = "registro",
    decision = "No usada en el modelo principal (se puede agregar si el equipo lo justifica)"))
}
stopifnot(!nrow(sin_clasif[tabla == "colisiones"]))
setnames(aud, c("tabla", "variable", "interpretacion", "tipo", "disponibilidad", "decision"))
fwrite(aud, salida("tabla_predictores.csv"))

# -----------------------------------------------------------------------------
# 9. Guardado
# -----------------------------------------------------------------------------
base_final <- base[, c("collision_index", "y", "collision_severity",
                       unlist(grupos), "fecha"), with = FALSE]
base_final <- base_final[, unique(names(base_final)), with = FALSE]
saveRDS(list(base = base_final, grupos = grupos), salida("base_analitica.rds"))

cat("\n== Integridad ==\n");        print(integridad)
cat("\n== Construccion de la muestra ==\n"); print(constr)
cat("\n== Prevalencia por anio y fuerza ==\n"); print(prev)
cat("\n== Sistema de reporte (collision_injury_based) ==\n")
print(med[order(fuerza, collision_year, collision_injury_based)])
cat("\nBase analitica:", nrow(base_final), "filas x", ncol(base_final), "columnas\n")
