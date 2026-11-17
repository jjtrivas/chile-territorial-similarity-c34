# ============================================================================
# 06 - RECONSTRUCCION INDEPENDIENTE DE LAS TRES REDES DESDE LA MATRIZ FINAL
# Proyecto: chile-territorial-similarity-c34
#
# Alcance: reconstruye distancias y enlaces DESDE LA MATRIZ PROCESADA, y
# compara los resultados con los grafos finales archivados.
# NO equivale a reconstruir toda la matriz desde las fuentes primarias.
# No modifica datos de origen, matriz, grafo ni figuras publicados.
# ============================================================================

if (!requireNamespace("igraph", quietly = TRUE)) {
  stop("Falta igraph. Instala con install.packages('igraph').")
}

carpeta <- file.path("data", "processed")
archivos <- file.path(carpeta, c(
  "03A_matriz_V2_candidata.rds",
  "01H_tasas_C34_ajustadas.rds",
  "03B_redes_demografia_historica.rds"
))
if (!all(file.exists(archivos))) {
  stop("No encuentro los RDS: abre el proyecto .Rproj de GitHub antes de ejecutar.")
}

base <- readRDS(archivos[1])
tasas <- readRDS(archivos[2])
archivo_redes <- readRDS(archivos[3])

nombres_redes <- c("A", "B", "A_ajustada")
stopifnot(
  nrow(base) == 18L,
  nrow(tasas) == 18L,
  all(nombres_redes %in% names(archivo_redes$redes)),
  "comuna" %in% names(base),
  "comuna" %in% names(tasas),
  !anyDuplicated(base$comuna),
  !anyDuplicated(tasas$comuna)
)

# Tomar exclusivamente el ORDEN de las comunas del grafo archivado.
# Las distancias y los enlaces se calcularan desde cero; no se reutilizan.
comunas <- rownames(archivo_redes$redes$B$distancias)
stopifnot(length(comunas) == 18L, !anyDuplicated(comunas))
stopifnot(setequal(base$comuna, comunas), setequal(tasas$comuna, comunas))
base <- base[match(comunas, base$comuna), , drop = FALSE]
tasas <- tasas[match(comunas, tasas$comuna), , drop = FALSE]

bloques <- list(
  ambiental = c("n_declaraciones_zona", "tiene_latente_num"),
  social = c("pobreza_multidimensional_2017", "porcentaje_viviendas_hacinadas_2024"),
  demografico_sanitario = c(
    "porcentaje_65mas_historico",  # No usar la columna del anio 2020
    "aps_publicas_10k_log",
    "hospital_alta_complejidad_num"
  )
)

columnas_tasas <- c("tasa_c34_100k", "tasa_ajustada_100k")
stopifnot(
  all(unlist(bloques, use.names = FALSE) %in% names(base)),
  all(columnas_tasas %in% names(tasas)),
  identical(as.character(base$comuna), comunas),
  identical(as.character(tasas$comuna), comunas)
)

# Convertir a matriz numerica; normalizar con z-score y obtener distancias
# euclidianas; cada bloque se normaliza dividiendo por su distancia maxima.
distancia_bloque <- function(datos) {
  x <- as.matrix(datos)
  storage.mode(x) <- "double"
  if (anyNA(x) || !all(is.finite(x))) {
    stop("Hay valores no numericos, ausentes o infinitos en un bloque.")
  }
  desv <- apply(x, 2, stats::sd)
  if (!all(is.finite(desv)) || any(desv <= 0)) {
    stop("Una de las variables del bloque carece de variacion.")
  }
  d <- as.matrix(stats::dist(scale(x), method = "euclidean"))
  if (!is.finite(max(d)) || max(d) <= 0) {
    stop("La distancia maxima del bloque no es valida.")
  }
  d <- d / max(d)
  dimnames(d) <- list(comunas, comunas)
  d
}

cat("\n[1/3] Recalculando bloques y distancias desde la matriz...\n")
d_ambiental <- distancia_bloque(base[, bloques$ambiental, drop = FALSE])
d_social <- distancia_bloque(base[, bloques$social, drop = FALSE])
d_demografico <- distancia_bloque(base[, bloques$demografico_sanitario, drop = FALSE])

cruda <- as.numeric(tasas$tasa_c34_100k)
ajustada <- as.numeric(tasas$tasa_ajustada_100k)
if (anyNA(cruda) || anyNA(ajustada) || any(cruda < 0) || any(ajustada < 0)) {
  stop("Las tasas C34 contienen valores invalidos.")
}
d_c34_crudo <- distancia_bloque(matrix(log1p(cruda), ncol = 1))
d_c34_ajustado <- distancia_bloque(matrix(log1p(ajustada), ncol = 1))

# Cada bloque tiene el mismo peso, como en el procedimiento original.
distancias_nuevas <- list(
  A = (d_ambiental + d_social + d_demografico + d_c34_crudo) / 4,
  B = (d_ambiental + d_social + d_demografico) / 3,
  A_ajustada = (d_ambiental + d_social + d_demografico + d_c34_ajustado) / 4
)

# Identificar enlaces por nombres; la orientacion de cada arista no importa.
clave_enlace <- function(desde, hasta) {
  paste(pmin(desde, hasta), pmax(desde, hasta), sep = " | ")
}

crear_enlaces <- function(d) {
  s <- 1 - d
  umbral <- as.numeric(stats::quantile(
    s[upper.tri(s)], probs = 0.60, names = FALSE
  ))
  pos <- which(upper.tri(s) & s >= umbral, arr.ind = TRUE)
  enlaces <- data.frame(
    from = rownames(s)[pos[, 1]],
    to = colnames(s)[pos[, 2]],
    similitud = s[pos],
    stringsAsFactors = FALSE
  )
  list(enlaces = enlaces, umbral = umbral)
}

cat("[2/3] Comparando enlaces y matrices con las tres redes archivadas...\n")
resumen <- lapply(nombres_redes, function(nombre) {
  nuevo <- crear_enlaces(distancias_nuevas[[nombre]])
  antiguo <- archivo_redes$redes[[nombre]]
  guardada <- antiguo$distancias
  stopifnot(
    is.matrix(guardada),
    identical(rownames(guardada), comunas),
    identical(colnames(guardada), comunas)
  )
  error_max <- max(abs(distancias_nuevas[[nombre]] - guardada))
  g <- antiguo$grafo
  guardados <- igraph::as_data_frame(g, what = "edges")
  enlaces_nuevos <- sort(clave_enlace(nuevo$enlaces$from, nuevo$enlaces$to))
  enlaces_archivo <- sort(clave_enlace(guardados$from, guardados$to))
  estructura <- c(
    nodos = igraph::vcount(g),
    enlaces = igraph::ecount(g),
    componentes = igraph::components(g)$no,
    aislados = sum(igraph::degree(g) == 0)
  )
  iguales <- identical(enlaces_nuevos, enlaces_archivo)
  data.frame(
    red = nombre,
    nodos = as.integer(estructura["nodos"]),
    enlaces_reconstruidos = nrow(nuevo$enlaces),
    enlaces_archivados = as.integer(estructura["enlaces"]),
    enlaces_identicos = iguales,
    componentes = as.integer(estructura["componentes"]),
    aislados = as.integer(estructura["aislados"]),
    error_maximo_distancia = error_max,
    matrices_coinciden = is.finite(error_max) && error_max < 1e-10,
    stringsAsFactors = FALSE
  )
})
resumen <- do.call(rbind, resumen)
rownames(resumen) <- NULL
print(resumen, row.names = FALSE, digits = 6)

cat("\n[3/3] Comprobando los resultados y guardando auditoria local...\n")
correcto <- all(
  resumen$nodos == 18L,
  resumen$enlaces_reconstruidos == 61L,
  resumen$enlaces_archivados == 61L,
  resumen$componentes == 4L,
  resumen$aislados == 2L,
  resumen$enlaces_identicos,
  resumen$matrices_coinciden
)

carpeta_salida <- file.path("exports", "verification")
dir.create(carpeta_salida, recursive = TRUE, showWarnings = FALSE)
salida <- file.path(carpeta_salida, "06_control_redes_desde_matriz.csv")
utils::write.csv(resumen, salida, row.names = FALSE, fileEncoding = "UTF-8")
cat("Tabla de auditoria:", salida, "\n")

if (correcto) {
  cat("\nREDES_RECONSTRUIDAS_DESDE_MATRIZ - 3/3 REDES\n")
  cat("Las 61 aristas de cada red coinciden con las archivadas.\n")
} else {
  cat("\nDIFERENCIAS_EN_RECONSTRUCCION - REVISAR ANTES DE PUBLICAR\n")
}
cat("NOTA: falta la reconstruccion integral de la matriz desde todas las fuentes originales.\n")
cat("FIN_DEL_CONTROL_DE_REDES\n")
