# =============================================================================
# EYP3407 - Caso 06: STATS19 - punto de entrada del analisis
#   Rscript --vanilla analisis.R
# Ejecuta: preparacion.R -> validacion temporal en desarrollo (2021-2022) ->
#          seleccion -> reajuste -> evaluacion UNICA en prueba (2023) ->
#          incertidumbre, diagnostico, sensibilidades, figuras y tablas.
# Paquetes: data.table, readxl, glmnet, Matrix, rpart, ranger, ggplot2
# Entradas: datos/*.csv (o STATS19_DIR). Salidas: salidas/ (o STATS19_OUT).
#
# -----------------------------------------------------------------------------
# EXPECTATIVAS FIJADAS ANTES DEL AJUSTE (ver informe, seccion de metodos)
#  Referencia: prevalencia de entrenamiento. Sin discriminacion (AUC = 0,5);
#     sirve como piso de perdida logaritmica.
#  M1 logistica con ridge/lasso: captura efectos aproximadamente aditivos
#     (velocidad, tipo de via, motos/ciclistas). Falla ante interacciones
#     (p. ej. velocidad x tipo de usuario) y no recoge no linealidades.
#  M2 arbol podado: captura interacciones simples y umbrales (limite de
#     velocidad, motos). Falla por inestabilidad y probabilidades poco finas.
#  M3 bosque aleatorio: captura interacciones y no linealidades; esperamos la
#     mejor perdida logaritmica. Puede fallar por sobreajuste a la fuerza
#     (confusion con el sistema de medicion) y su calibracion puede requerir
#     cuidado.
#  Regla de seleccion (fijada antes): menor perdida logaritmica media en las
#     validaciones; M2/M3 reemplazan a un modelo mas simple solo si mejoran
#     la perdida media en mas de 0,002.
#  Umbral: maximiza sensibilidad + especificidad - 1 (indice J de Youden) sobre
#     las predicciones de validacion agrupadas, fijado en desarrollo.
# =============================================================================

t0 <- Sys.time()
suppressPackageStartupMessages({
  library(data.table); library(Matrix); library(glmnet)
  library(rpart); library(ranger); library(ggplot2)
})
options(datatable.print.nrows = 50)
set.seed(2026)
# Hilos de ranger: fijo en 1 por defecto para que los resultados sean identicos en
# cualquier computador (STATS19_THREADS=<k> acelera, con posibles diferencias minimas).
NT <- as.integer(Sys.getenv("STATS19_THREADS", "1"))

# --- Presupuesto de busqueda (maximo 20 configuraciones por familia) ---------
LAMBDAS   <- sort(exp(seq(log(1e-5), log(1e-1), length.out = 10)), decreasing = TRUE)
GRID_M1   <- CJ(alpha = c(0, 1), lambda = LAMBDAS)                 # 20 (ridge, lasso)
GRID_M2   <- CJ(cp = exp(seq(log(1e-4), log(3e-2), length.out = 6)),
                minbucket = c(20, 50, 200))                       # 18 (arbol podado)
GRID_M3   <- CJ(mtry = c(4, 8, 14), min.node.size = c(25, 100, 400, 1000, 2000)) # 15
TREES_TUNE <- 200; TREES_FINAL <- 500
B_BOOT <- 2000; B_BOOT_AUC <- 500
DELTA_SIMPLE <- 0.002
CLIP <- 1e-4

invisible(capture.output(source("preparacion.R", local = new.env())))  # genera salidas/base_analitica.rds
DIR_SALIDA <- Sys.getenv("STATS19_OUT", "salidas")
sal <- function(f) file.path(DIR_SALIDA, f)
obj <- readRDS(sal("base_analitica.rds"))
base <- obj$base; G <- obj$grupos
base[, sem := paste0(year(fecha), fifelse(mes <= 6, "H1", "H2"))]
base[, cluster_geo := local_authority_highway]

VARS_CAT  <- G$colision_cat
VARS_NUM  <- c(G$colision_num, G$vehiculos_descriptivos, G$vehiculos_dinamica_reconstruida)
VARS_FULL <- list(cat = VARS_CAT, num = VARS_NUM)
VARS_SIN_DIN <- list(cat = VARS_CAT, num = setdiff(VARS_NUM, G$vehiculos_dinamica_reconstruida))

# =============================================================================
# 1. Metricas
# =============================================================================
logloss <- function(y, p) { p <- pmin(pmax(p, CLIP), 1 - CLIP); -mean(y * log(p) + (1 - y) * log(1 - p)) }
brier   <- function(y, p) mean((y - p)^2)
auc_roc <- function(y, p) {
  n1 <- sum(y == 1); n0 <- sum(y == 0)
  if (n1 == 0 || n0 == 0 || length(unique(p)) == 1) return(0.5)
  (sum(rank(p)[y == 1]) - n1 * (n1 + 1) / 2) / (n1 * n0)
}
# Area bajo la curva precision-sensibilidad = precision promedio escalonada:
# sum_k (R_k - R_{k-1}) * P_k, evaluada en los umbrales distintos de p.
auc_pr <- function(y, p) {
  o <- order(-p); y <- y[o]; p <- p[o]
  fin <- c(which(diff(p) != 0), length(p))
  tp <- cumsum(y)[fin]; k <- fin
  rec <- tp / sum(y); prec <- tp / k
  sum(diff(c(0, rec)) * prec)
}
matriz <- function(y, p, u) {
  pred <- as.integer(p >= u)
  tp <- sum(pred == 1 & y == 1); fp <- sum(pred == 1 & y == 0)
  fn <- sum(pred == 0 & y == 1); tn <- sum(pred == 0 & y == 0)
  data.table(umbral = u, TP = tp, FP = fp, FN = fn, TN = tn,
             sensibilidad = tp / (tp + fn), especificidad = tn / (tn + fp),
             VPP = ifelse(tp + fp > 0, tp / (tp + fp), NA_real_))
}
umbral_youden <- function(y, p) {
  cand <- sort(unique(quantile(p, probs = seq(0.01, 0.99, by = 0.005))))
  j <- vapply(cand, function(u) { m <- matriz(y, p, u); m$sensibilidad + m$especificidad - 1 }, 0)
  cand[which.max(j)]
}
resumen <- function(y, p)
  data.table(n = length(y), eventos = sum(y), prevalencia = mean(y),
             logloss = logloss(y, p), brier = brier(y, p),
             auc_roc = auc_roc(y, p), auc_pr = auc_pr(y, p))

# =============================================================================
# 2. Preprocesamiento: SIEMPRE ajustado con el entrenamiento de cada particion
#    (niveles de factores, medianas de imputacion). Categorias no vistas -> "otro".
# =============================================================================
ajustar_prep <- function(tr, vars) {
  list(vars = vars,
       niveles = lapply(tr[, vars$cat, with = FALSE], function(x) c(sort(unique(x)), "otro")),
       med = vapply(tr[, vars$num, with = FALSE], function(x) median(x, na.rm = TRUE), 0))
}
aplicar_prep <- function(pp, df) {
  out <- data.frame(row.names = seq_len(nrow(df)))
  for (v in pp$vars$cat) {
    x <- df[[v]]; x[!x %in% pp$niveles[[v]]] <- "otro"
    out[[v]] <- factor(x, levels = pp$niveles[[v]])
  }
  for (v in pp$vars$num) {
    x <- df[[v]]
    if (anyNA(pp$med[[v]]) || v %in% c("edad_cond_min", "edad_cond_max", "edad_veh_max"))
      out[[paste0(v, "_na")]] <- as.integer(is.na(x))
    x[is.na(x)] <- pp$med[[v]]
    out[[v]] <- as.numeric(x)
  }
  out$hora_sin <- sin(2 * pi * df$hora / 24); out$hora_cos <- cos(2 * pi * df$hora / 24)
  out$mes_sin  <- sin(2 * pi * df$mes / 12);  out$mes_cos  <- cos(2 * pi * df$mes / 12)
  out
}
mm <- function(D, ref) {
  ctr <- lapply(ref[vapply(ref, is.factor, TRUE)], contrasts, contrasts = FALSE)
  sparse.model.matrix(~ . - 1, data = D, contrasts.arg = ctr)
}

# =============================================================================
# 3. Ajuste y prediccion por procedimiento
# =============================================================================
fit_predict <- function(modelo, par, tr, te, vars, trees = TREES_TUNE) {
  if (modelo == "REF") return(rep(mean(tr$y), nrow(te)))
  if (modelo == "REF_F") {   # referencia complementaria: prevalencia por fuerza (solo entrenamiento)
    tasa <- tr[, .(p = mean(y)), by = fuerza]
    p <- tasa$p[match(te$fuerza, tasa$fuerza)]; p[is.na(p)] <- mean(tr$y); return(p)
  }
  pp <- ajustar_prep(tr, vars)
  Dtr <- aplicar_prep(pp, tr); Dte <- aplicar_prep(pp, te)
  if (modelo == "M1") {
    fit <- glmnet(mm(Dtr, Dtr), tr$y, family = "binomial", alpha = par$alpha, lambda = LAMBDAS)
    return(as.numeric(predict(fit, mm(Dte, Dtr), s = par$lambda, type = "response")))
  }
  if (modelo == "M2") {
    d <- cbind(Dtr, y = factor(tr$y, levels = c(0, 1)))
    fit <- rpart(y ~ ., data = d, method = "class",
                 control = rpart.control(cp = min(GRID_M2$cp), minbucket = par$minbucket,
                                         maxdepth = 12, xval = 0))
    fit <- prune(fit, cp = par$cp)
    return(as.numeric(predict(fit, Dte, type = "prob")[, "1"]))
  }
  if (modelo == "M3") {
    d <- cbind(Dtr, y = factor(tr$y, levels = c(0, 1)))
    fit <- ranger(y ~ ., data = d, probability = TRUE, num.trees = trees,
                  mtry = min(par$mtry, ncol(Dtr)), min.node.size = par$min.node.size,
                  respect.unordered.factors = "order", seed = 2026, num.threads = NT)
    return(as.numeric(predict(fit, Dte, num.threads = NT)$predictions[, "1"]))
  }
}
GRIDS <- list(M1 = GRID_M1, M2 = GRID_M2, M3 = GRID_M3)

# =============================================================================
# 4. DESARROLLO (2021-2022): validacion temporal de origen creciente
#    Estructura: entrenar con semestres previos y validar en el siguiente.
#    La prueba (2023) NO se carga hasta la seccion 7.
# =============================================================================
dev <- base[periodo == "desarrollo"]
FOLDS <- list(
  list(nombre = "F1: 2021H1 -> 2021H2", train = "2021H1",                     val = "2021H2"),
  list(nombre = "F2: 2021 -> 2022H1",   train = c("2021H1", "2021H2"),        val = "2022H1"),
  list(nombre = "F3: 2021-2022H1 -> 2022H2", train = c("2021H1", "2021H2", "2022H1"), val = "2022H2"))

cv_modelo <- function(modelo, par, vars, d = dev) {
  rbindlist(lapply(seq_along(FOLDS), function(k) {
    f <- FOLDS[[k]]; tr <- d[sem %in% f$train]; va <- d[sem %in% f$val]
    data.table(fold = k, id = va$collision_index, y = va$y,
               fuerza = va$fuerza, p = fit_predict(modelo, par, tr, va, vars))
  }))
}
resumen_cv <- function(pr) pr[, .(logloss = logloss(y, p), auc_roc = auc_roc(y, p),
                                  n = .N), by = fold]

cat("\n== Busqueda en desarrollo ==\n")
busqueda <- list(); pred_cv <- list()
cv_ref <- cv_modelo("REF", NULL, VARS_FULL)
cv_ref_f <- cv_modelo("REF_F", NULL, VARS_FULL)
for (m in names(GRIDS)) {
  g <- GRIDS[[m]]; t1 <- Sys.time()
  for (i in seq_len(nrow(g))) {
    pr <- cv_modelo(m, g[i], VARS_FULL)
    r <- resumen_cv(pr)
    busqueda[[paste(m, i)]] <- cbind(modelo = m, config = i, g[i],
      ll_medio = mean(r$logloss), ll_sd = sd(r$logloss),
      auc_medio = mean(r$auc_roc),
      t(setNames(r$logloss, paste0("ll_fold", r$fold))))
    pred_cv[[paste(m, i)]] <- pr
  }
  cat(sprintf("%s: %d configuraciones en %.1f min\n", m, nrow(g),
              as.numeric(difftime(Sys.time(), t1, units = "mins"))))
}
busqueda <- rbindlist(busqueda, fill = TRUE)
fwrite(busqueda, sal("tabla_busqueda_desarrollo.csv"))

mejor <- busqueda[, .SD[which.min(ll_medio)], by = modelo]
ref_cv_ll <- resumen_cv(cv_ref)
sel_tab <- rbind(
  data.table(modelo = "REF", config = NA_integer_, ll_medio = mean(ref_cv_ll$logloss),
             ll_sd = sd(ref_cv_ll$logloss), auc_medio = 0.5),
  mejor[, .(modelo, config, ll_medio, ll_sd, auc_medio)], fill = TRUE)

# regla de seleccion fijada antes del ajuste
elegido <- "M1"
for (m in c("M2", "M3"))
  if (mejor[modelo == m, ll_medio] < mejor[modelo == elegido, ll_medio] - DELTA_SIMPLE) elegido <- m
sel_tab[, elegido := modelo == elegido]
fwrite(sel_tab, sal("tabla_seleccion.csv"))
cat("\nMejor configuracion por familia y modelo elegido:", elegido, "\n"); print(sel_tab)

# umbrales (indice J) con predicciones de validacion agrupadas de la mejor config
UMBRAL <- sapply(mejor$modelo, function(m) {
  pr <- pred_cv[[paste(m, mejor[modelo == m, config])]]; umbral_youden(pr$y, pr$p) })
names(UMBRAL) <- mejor$modelo
cat("\nUmbrales fijados en desarrollo:\n"); print(UMBRAL)

# tabla de desarrollo (pares: mismas filas de validacion)
dev_tab <- rbindlist(c(list(cbind(modelo = "REF", resumen(cv_ref$y, cv_ref$p)),
                           cbind(modelo = "REF_F", resumen(cv_ref_f$y, cv_ref_f$p))),
  lapply(mejor$modelo, function(m) {
    pr <- pred_cv[[paste(m, mejor[modelo == m, config])]]
    cbind(modelo = m, resumen(pr$y, pr$p)) })))
dev_tab[, dif_logloss_vs_ref := logloss - logloss[modelo == "REF"]]
fwrite(dev_tab, sal("tabla_modelos_desarrollo.csv"))
fwrite(data.table(modelo = names(UMBRAL), umbral = as.numeric(UMBRAL)), sal("tabla_umbrales_desarrollo.csv"))

# =============================================================================
# 5. SENSIBILIDADES (definidas con datos de desarrollo)
#   S1: excluir variables reconstruidas tras el evento (dinamica del choque).
#   S2: medicion homogenea: solo fuerzas 6 y 13 con collision_injury_based = 1
#       (entrena y evalua en ese subconjunto; se compara con el modelo completo
#       evaluado sobre las MISMAS filas).
# =============================================================================
cat("\n== Sensibilidades (desarrollo) ==\n")
sens <- list()
for (m in mejor$modelo) {
  par <- GRIDS[[m]][mejor[modelo == m, config]]
  full <- pred_cv[[paste(m, mejor[modelo == m, config])]]
  # S1
  s1 <- cv_modelo(m, par, VARS_SIN_DIN)
  sens[[paste(m, "S1")]] <- data.table(modelo = m, sensibilidad = "S1 sin variables reconstruidas",
    ll_completo = logloss(full$y, full$p), ll_variante = logloss(s1$y, s1$p),
    auc_completo = auc_roc(full$y, full$p), auc_variante = auc_roc(s1$y, s1$p), n = nrow(s1))
  # S2
  sub <- dev[fuerza != "1_Metropolitan" & collision_injury_based == "1"]
  s2 <- cv_modelo(m, par, VARS_FULL, d = sub)
  fsub <- full[id %in% s2$id]; s2 <- s2[match(fsub$id, id)]
  sens[[paste(m, "S2")]] <- data.table(modelo = m, sensibilidad = "S2 medicion homogenea (fuerzas 6 y 13, injury_based=1)",
    ll_completo = logloss(fsub$y, fsub$p), ll_variante = logloss(s2$y, s2$p),
    auc_completo = auc_roc(fsub$y, fsub$p), auc_variante = auc_roc(s2$y, s2$p), n = nrow(s2))
}
sens_dev <- rbindlist(sens)
sens_dev[, dif_logloss := ll_variante - ll_completo]
fwrite(sens_dev, sal("tabla_sensibilidad_desarrollo.csv"))
print(sens_dev)

# =============================================================================
# 6. Reajuste con TODO desarrollo (2021-2022) con las configuraciones elegidas
# =============================================================================
cat("\n== Reajuste final con desarrollo completo ==\n")
# =============================================================================
# 7. EVALUACION UNICA EN PRUEBA (2023). Desde aqui se leen las respuestas de 2023.
# =============================================================================
test <- base[periodo == "prueba"]
pred <- data.table(collision_index = test$collision_index, y = test$y, fuerza = test$fuerza,
                   collision_severity = test$collision_severity,
                   urban_or_rural_area = test$urban_or_rural_area,
                   cluster_geo = test$cluster_geo, fecha = test$fecha,
                   REF = fit_predict("REF", NULL, dev, test, VARS_FULL),
                   REF_F = fit_predict("REF_F", NULL, dev, test, VARS_FULL))
for (m in mejor$modelo) {
  par <- GRIDS[[m]][mejor[modelo == m, config]]
  set(pred, j = m, value = fit_predict(m, par, dev, test, VARS_FULL, trees = TREES_FINAL))
}
fwrite(pred, sal("predicciones_prueba.csv.gz"))

MODELOS <- c("REF", "REF_F", mejor$modelo)
test_tab <- rbindlist(lapply(MODELOS, function(m) cbind(modelo = m, resumen(pred$y, pred[[m]]))))
test_tab[, `:=`(dif_logloss_vs_ref = logloss - logloss[modelo == "REF"],
                dif_logloss_vs_ref_f = logloss - logloss[modelo == "REF_F"],
                dif_auc_pr_vs_ref = auc_pr - auc_pr[modelo == "REF"],
                elegido = modelo == elegido)]
fwrite(test_tab, sal("tabla_modelos_prueba.csv"))

conf <- rbindlist(lapply(mejor$modelo, function(m) cbind(modelo = m, matriz(pred$y, pred[[m]], UMBRAL[[m]]))))
fwrite(conf, sal("tabla_confusion_prueba.csv"))
cat("\n== PRUEBA 2023 ==\n"); print(test_tab); print(conf)

# --- Diagnostico por fuerza y por zona (modelo elegido y referencia) ---------
diag_grupo <- function(g) rbindlist(lapply(c("REF", "REF_F", elegido), function(m)
  pred[, cbind(modelo = m, resumen(y, get(m))), by = g]), fill = TRUE)
diag_f <- diag_grupo("fuerza"); diag_u <- diag_grupo("urban_or_rural_area")
setnames(diag_u, "urban_or_rural_area", "grupo"); setnames(diag_f, "fuerza", "grupo")
diag_all <- rbind(cbind(tipo = "fuerza", diag_f), cbind(tipo = "urbano_rural", diag_u))
fwrite(diag_all, sal("tabla_diagnostico_subgrupos.csv"))

# Calibracion (deciles de probabilidad predicha, modelo elegido)
pred[, p_el := get(elegido)]
pred[, decil := cut(p_el, breaks = unique(quantile(p_el, 0:10 / 10)), include.lowest = TRUE, labels = FALSE)]
cal <- pred[, .(n = .N, p_media = mean(p_el), frec_obs = mean(y)), by = decil][order(decil)]
fwrite(cal, sal("tabla_calibracion_prueba.csv"))

# --- Incertidumbre: remuestreo de pruebas con predicciones FIJAS --------------
# (condicional a los ajustes; no incorpora incertidumbre de seleccion/entrenamiento)
cat("\n== Remuestreo (predicciones fijas) ==\n")
perdida <- function(y, p) { p <- pmin(pmax(p, CLIP), 1 - CLIP); -(y * log(p) + (1 - y) * log(1 - p)) }
pred[, loss_ref := perdida(y, REF)]
pred[, loss_ref_f := perdida(y, REF_F)]
pred[, loss_el := perdida(y, p_el)]
pred[, semana := format(fecha, "%G-%V")]
boot_unidad <- function(unidad, ref = "loss_ref") {
  s <- pred[, .(d = sum(get(ref) - loss_el), n = .N), by = unidad]
  est <- sum(s$d) / sum(s$n)
  b <- replicate(B_BOOT, { i <- sample.int(nrow(s), replace = TRUE); sum(s$d[i]) / sum(s$n[i]) })
  data.table(referencia = ref, unidad = unidad, n_unidades = nrow(s), reduccion_logloss = est,
             ic_inf = quantile(b, 0.025), ic_sup = quantile(b, 0.975))
}
boot_auc <- function(unidad) {
  idx <- split(seq_len(nrow(pred)), pred[[unidad]]); nm <- length(idx)
  b <- replicate(B_BOOT_AUC, { i <- unlist(idx[sample.int(nm, replace = TRUE)])
    auc_roc(pred$y[i], pred$p_el[i]) })
  data.table(unidad = unidad, auc_elegido = auc_roc(pred$y, pred$p_el),
             ic_inf = quantile(b, 0.025), ic_sup = quantile(b, 0.975))
}
boot <- rbind(boot_unidad("cluster_geo"), boot_unidad("semana"),
              boot_unidad("cluster_geo", "loss_ref_f"), boot_unidad("semana", "loss_ref_f"))
boot[, referencia := fifelse(referencia == "loss_ref", "REF (prevalencia global)", "REF_F (prevalencia por fuerza)")]
boot[, B := B_BOOT]
boot_a <- rbind(boot_auc("cluster_geo"), boot_auc("semana"))[, B := B_BOOT_AUC]
fwrite(boot, sal("tabla_incertidumbre_logloss.csv")); fwrite(boot_a, sal("tabla_incertidumbre_auc.csv"))
print(boot); print(boot_a)

# --- Eventos raros: fatales (codigo 1) frente a graves y leves -------------------
raros <- pred[, .(n = .N, p_media_elegido = mean(p_el), p_media_ref = mean(REF)),
              by = collision_severity][order(collision_severity)]
fwrite(raros, sal("tabla_eventos_raros.csv"))
y_fat <- as.integer(pred$collision_severity == "1")
top20 <- pred$p_el >= quantile(pred$p_el, 0.8)
raros_res <- data.table(n_fatales = sum(y_fat), auc_fatal_vs_resto = auc_roc(y_fat, pred$p_el),
                        pct_fatales_en_20pct_superior = mean(top20[y_fat == 1]),
                        pct_graves_en_20pct_superior = mean(top20[pred$collision_severity == "2"]))
fwrite(raros_res, sal("tabla_eventos_raros_resumen.csv")); print(raros); print(raros_res)

# --- Sensibilidades en prueba (informativas; no se usan para elegir nada) -----
sens_test <- list()
for (m in mejor$modelo) {
  par <- GRIDS[[m]][mejor[modelo == m, config]]
  p1 <- fit_predict(m, par, dev, test, VARS_SIN_DIN, trees = TREES_FINAL)
  sens_test[[paste(m, 1)]] <- data.table(modelo = m, sensibilidad = "S1 sin variables reconstruidas",
    ll_completo = logloss(pred$y, pred[[m]]), ll_variante = logloss(pred$y, p1), n = nrow(pred))
  sub_tr <- dev[fuerza != "1_Metropolitan" & collision_injury_based == "1"]
  sub_te <- test[fuerza != "1_Metropolitan" & collision_injury_based == "1"]
  p2 <- fit_predict(m, par, sub_tr, sub_te, VARS_FULL, trees = TREES_FINAL)
  pc <- pred[match(sub_te$collision_index, collision_index)][[m]]
  sens_test[[paste(m, 2)]] <- data.table(modelo = m, sensibilidad = "S2 medicion homogenea",
    ll_completo = logloss(sub_te$y, pc), ll_variante = logloss(sub_te$y, p2), n = nrow(sub_te))
}
sens_test <- rbindlist(sens_test); sens_test[, dif_logloss := ll_variante - ll_completo]
fwrite(sens_test, sal("tabla_sensibilidad_prueba.csv")); print(sens_test)

# =============================================================================
# 8. Figuras (titulo, unidades y leyenda)
# =============================================================================
pr_curva <- function(y, p, nombre) {
  o <- order(-p); y <- y[o]; p <- p[o]
  fin <- c(which(diff(p) != 0), length(p))
  data.table(modelo = nombre, sensibilidad = cumsum(y)[fin] / sum(y), precision = cumsum(y)[fin] / fin)
}
curvas <- rbindlist(lapply(MODELOS, function(m) pr_curva(pred$y, pred[[m]], m)))
g1 <- ggplot(curvas, aes(sensibilidad, precision, colour = modelo)) + geom_line() +
  geom_hline(yintercept = mean(pred$y), linetype = 2, colour = "grey40") +
  labs(title = "Curvas precision-sensibilidad en prueba (2023)",
       subtitle = "Linea punteada: prevalencia (piso de la referencia)",
       x = "Sensibilidad (recall)", y = "Precision (VPP)", colour = "Procedimiento") +
  theme_minimal()
ggsave(sal("fig_precision_sensibilidad.png"), g1, width = 7, height = 4.5, dpi = 150)

g2 <- ggplot(cal, aes(p_media, frec_obs, size = n)) + geom_point() +
  geom_abline(slope = 1, intercept = 0, linetype = 2) +
  labs(title = paste0("Calibracion en prueba (2023): modelo ", elegido),
       subtitle = "Deciles de probabilidad predicha; punteada = calibracion perfecta",
       x = "Probabilidad media predicha", y = "Frecuencia observada de fatal o grave",
       size = "Colisiones") + theme_minimal()
ggsave(sal("fig_calibracion.png"), g2, width = 6, height = 5, dpi = 150)

dd <- diag_f[, .(grupo, modelo, logloss)]
g3 <- ggplot(dd, aes(grupo, logloss, fill = modelo)) + geom_col(position = "dodge") +
  labs(title = "Perdida logaritmica en prueba por fuerza policial",
       x = "Fuerza policial", y = "Perdida logaritmica (nats por colision)", fill = "Procedimiento") +
  theme_minimal()
ggsave(sal("fig_logloss_por_fuerza.png"), g3, width = 7, height = 4.5, dpi = 150)

# =============================================================================
# 9. Registro de ejecucion
# =============================================================================
writeLines(c(paste("Fecha:", format(Sys.time())),
             paste("Duracion (min):", round(as.numeric(difftime(Sys.time(), t0, units = "mins")), 1)),
             paste("Modelo elegido:", elegido),
             capture.output(sessionInfo())), sal("registro_ejecucion.txt"))
cat("\nAnalisis terminado. Duracion:", round(as.numeric(difftime(Sys.time(), t0, units = "mins")), 1), "min\n")

# =============================================================================
# 10. Informe (Quarto -> PDF). Lee las tablas de salidas/; no recalcula nada.
# =============================================================================
if (file.exists("informe.qmd")) {
  qb <- Sys.which("quarto")
  if (nzchar(qb)) {
    cat("Generando informe.pdf ...\n")
    st <- system2(qb, c("render", "informe.qmd"))
    if (st != 0) warning("quarto render fallo (codigo ", st, ")")
  } else message("quarto no esta instalado: el informe no se genero (ver README).")
}
