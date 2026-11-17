# ======================================================================
# 05_ine_y_control_redes_local.R
# Etapa A: reconstruir poblacion comunal por edad y periodo desde INE T5
#          y cotejar con los datos procesados archivados.
# Etapa A2: recalcular tasas C34 crudas/ajustadas desde RPC + INE.
# Etapa B: ejecutar la auditoria de redes DESDE LOS RDS ARCHIVADOS.
# IMPORTANTE: B no reconstruye las redes desde todas las fuentes brutas.
# No altera fuentes originales ni sobreescribe los RDS del repositorio.
# Requiere readxl, igraph, .Rproj abierto y FUENTES_RAIZ definido en R.
# ======================================================================

local({
  if (!file.exists("README.md") || !dir.exists("data/processed") ||
      !file.exists("scripts/03_prepublicacion.R")) {
    stop("Abre primero chile-territorial-similarity-c34.Rproj y vuelve a ejecutar.")
  }
  if (!requireNamespace("readxl", quietly = TRUE)) {
    stop("Instala readxl una vez: install.packages('readxl')")
  }
  if (!requireNamespace("haven", quietly = TRUE)) {
    stop("Instala haven una vez: install.packages('haven')")
  }
  if (!requireNamespace("igraph", quietly = TRUE)) {
    stop("Instala igraph una vez: install.packages('igraph')")
  }
  ruta_rpc <- Sys.getenv("RPC_SAV_PATH", unset = "")
  if (!nzchar(ruta_rpc) || !file.exists(ruta_rpc)) {
    stop("Define RPC_SAV_PATH como ruta local del archivo .sav RPC original.")
  }
  raiz <- Sys.getenv("FUENTES_RAIZ", unset = "")
  if (!nzchar(raiz) || !dir.exists(raiz)) {
    stop("Define FUENTES_RAIZ como la carpeta local donde guardaste las fuentes originales.")
  }
  referencia <- readRDS("data/processed/01_tabla_C34.rds")
  pob_ref <- readRDS("data/processed/01E_poblacion_edad_validada.rds")
  cols <- c("comuna", "inicio_rpc", "fin_rpc", "personas_anio")
  stopifnot(is.data.frame(referencia), nrow(referencia) == 18L,
            all(cols %in% names(referencia)),
            all(c("comuna", "anio", "grupo_edad", "poblacion") %in% names(pob_ref)))
  grupos <- c(paste0(seq(0, 75, 5), "-", seq(4, 79, 5)), "80+")
  region_comunas <- list(
    `02` = c("Calama", "María Elena", "Tocopilla"),
    `07` = c("Curicó", "Maule", "Talca"),
    `08` = c("Chiguayante", "Concepción", "Coronel", "Hualpén",
             "Hualqui", "Los Ángeles", "Lota", "Penco",
             "San Pedro de la Paz", "Talcahuano", "Tomé"),
    `14` = c("Valdivia")
  )
  normalizar <- function(x) {
    z <- iconv(as.character(x), from = "", to = "ASCII//TRANSLIT")
    toupper(gsub("[[:space:]]", "", z))
  }
  cat("\n[A/2] Reconstruyendo denominadores INE, hoja T5...\n")
  encontrados <- character()
  registros <- list()
  n <- 0L
  for (reg in names(region_comunas)) {
    nombre <- paste0("SalComUsuarios-", reg, "Tok.xls")
    rutas <- list.files(raiz, recursive = TRUE, full.names = TRUE,
                         ignore.case = TRUE,
                         pattern = paste0("^SalComUsuarios-", reg, "Tok\\.xls$"))
    rutas <- rutas[!grepl("GITHUB_C34_|[/\\\\]renv[/\\\\]", rutas,
                           ignore.case = TRUE)]
    if (length(rutas) == 0L) stop("No aparece la fuente INE: ", nombre)
    if (length(rutas) > 1L) {
      hs <- unname(tools::md5sum(rutas))
      if (length(unique(hs)) != 1L) {
        stop("Hay copias diferentes de ", nombre,
             ". Identifica la fuente correcta antes de continuar.")
      }
    }
    archivo <- rutas[1]
    encontrados <- c(encontrados, archivo)
    if (!("T5" %in% readxl::excel_sheets(archivo))) {
      stop("La fuente no tiene hoja T5: ", nombre)
    }
    x <- as.matrix(as.data.frame(readxl::read_excel(
      archivo, sheet = "T5", col_names = FALSE,
      col_types = "text", .name_repair = "minimal")))
    normal <- matrix(normalizar(x), nrow = nrow(x), ncol = ncol(x))
    for (com in region_comunas[[reg]]) {
      pos <- which(normal == normalizar(com), arr.ind = TRUE)
      if (nrow(pos) != 6L) {
        stop("Se esperaban 6 bloques para ", com, "; encontrados: ", nrow(pos))
      }
      r <- referencia[referencia$comuna == com, , drop = FALSE]
      if (nrow(r) != 1L) stop("Falta referencia unica para: ", com)
      for (j in seq_len(nrow(pos))) {
        f <- pos[j, 1]
        c0 <- pos[j, 2]
        fr_edades <- seq.int(f + 7L, f + 23L)
        columnas <- seq.int(c0 + 1L, c0 + 6L)
        if (max(fr_edades) > nrow(x) || max(columnas) > ncol(x)) {
          stop("Bloque INE incompleto para ", com)
        }
        etiquetas <- normalizar(x[fr_edades, c0])
        if (!identical(as.character(etiquetas), grupos) ||
            normalizar(x[f + 5L, c0]) != "TOTAL") {
          stop("Estructura INE inesperada para ", com, " bloque ", j)
        }
        anos <- suppressWarnings(as.integer(x[f + 3L, columnas]))
        for (k in seq_along(columnas)) {
          ano <- anos[k]
          if (is.na(ano) || ano < r$inicio_rpc || ano > r$fin_rpc) next
          col <- columnas[k]
          total <- suppressWarnings(as.numeric(x[f + 5L, col]))
          valores <- suppressWarnings(as.numeric(x[fr_edades, col]))
          if (is.na(total) || anyNA(valores) || any(valores < 0) ||
              sum(valores) != total) {
            stop("Poblacion INE inconsistente en ", com, ", ", ano)
          }
          n <- n + 1L
          registros[[n]] <- data.frame(
            comuna = com, anio = ano, grupo_edad = grupos,
            poblacion = valores, stringsAsFactors = FALSE)
        }
      }
    }
    cat("  Region ", reg, ": ", length(region_comunas[[reg]]),
        " comunas, T5 leida\n", sep = "")
  }
  stopifnot(length(registros) > 0L)
  todo <- do.call(rbind, registros)
  clave <- paste(todo$comuna, todo$anio, todo$grupo_edad, sep = "|")
  grupos_valores <- split(todo$poblacion, clave)
  conflictos <- vapply(grupos_valores, function(z) length(unique(z)) > 1L,
                      logical(1))
  if (any(conflictos)) stop("Anios repetidos contienen poblaciones distintas.")
  reconstruida <- todo[!duplicated(clave), , drop = FALSE]
  rownames(reconstruida) <- NULL
  controles <- lapply(seq_len(nrow(referencia)), function(i) {
    r <- referencia[i, ]
    z <- reconstruida[reconstruida$comuna == r$comuna, , drop = FALSE]
    anos <- seq.int(r$inicio_rpc, r$fin_rpc)
    cobertura <- identical(sort(unique(z$anio)), as.integer(anos))
    categorias <- nrow(z) == length(anos) * 17L
    total <- sum(z$poblacion)
    data.frame(comuna = r$comuna, cobertura_completa = cobertura,
               grupos_edad_completos = categorias,
               personas_anio_INE = total,
               personas_anio_referencia = r$personas_anio,
               diferencia = total - r$personas_anio)
  })
  control <- do.call(rbind, controles)
  # Auditoria de las 2380 filas procesadas (no solo de los totales)
  nombres <- c("comuna", "anio", "grupo_edad", "poblacion")
  ref <- pob_ref[, nombres, drop = FALSE]
  if (anyDuplicated(paste(ref$comuna, ref$anio, ref$grupo_edad)) ||
      anyDuplicated(paste(reconstruida$comuna, reconstruida$anio,
                          reconstruida$grupo_edad))) {
    stop("Claves territoriales/temporales duplicadas inesperadas.")
  }
  comparacion <- merge(reconstruida, ref,
                       by = c("comuna", "anio", "grupo_edad"),
                       all = TRUE, suffixes = c("_INE", "_archivado"))
  filas_iguales <- nrow(comparacion) == nrow(reconstruida) &&
    nrow(comparacion) == nrow(ref) &&
    !anyNA(comparacion[, c("poblacion_INE", "poblacion_archivado")]) &&
    all(comparacion$poblacion_INE == comparacion$poblacion_archivado)
  ok <- nrow(control) == 18L && all(control$cobertura_completa) &&
    all(control$grupos_edad_completos) && all(control$diferencia == 0) &&
    filas_iguales
  cat("\n=== CONTROL DE DENOMINADORES INE ===\n")
  print(control, row.names = FALSE)
  cat("\nPersonas-anio INE:", sum(control$personas_anio_INE),
      "\nPersonas-anio archivadas:", sum(control$personas_anio_referencia),
      "\nFilas comuna-ano-edad:", nrow(reconstruida),
      "\nCoincidencia exacta por edad:", filas_iguales, "\n")
  if (!ok) {
    stop("INE_NO_COINCIDE. Se detiene aqui; no ejecutar ni publicar la etapa B.")
  }
  dir.create("results", showWarnings = FALSE)
  write.csv(control, "results/Tabla_verificacion_poblacion_INE.csv",
            row.names = FALSE, fileEncoding = "UTF-8")
  cat("\nINE_COINCIDE_CON_REFERENCIA - 18/18 COMUNAS\n")

  # A2. Reconstruccion de tasas con estandar interno, metodo original 1G/1H.
  cat("\n[A2] Recalculando tasas C34 desde RPC + INE...\n")
  mat <- readRDS("data/processed/03A_matriz_V2_candidata.rds")
  ajustadas_ref <- readRDS("data/processed/01H_tasas_C34_ajustadas.rds")
  stopifnot(all(c("nombre_comuna_rpc", "codigo_comuna_2008") %in% names(mat)),
            all(c("comuna", "tasa_ajustada_100k") %in% names(ajustadas_ref)))
  mapa <- unique(data.frame(
    comuna = trimws(as.character(mat$nombre_comuna_rpc)),
    codigo = suppressWarnings(as.integer(as.character(mat$codigo_comuna_2008))),
    stringsAsFactors = FALSE))
  mapa <- merge(mapa, referencia[, c("comuna", "inicio_rpc", "fin_rpc", "casos_c34",
                                         "personas_anio", "tasa_c34_100k")],
                by = "comuna", all = TRUE, sort = FALSE)
  if (nrow(mapa) != 18L || anyNA(mapa) || anyDuplicated(mapa$codigo)) {
    stop("Mapa RPC-comunas no coincide con las 18 comunas.")
  }
  rpc <- haven::read_sav(ruta_rpc)
  if (!all(c("CIE10", "regCom", "Año_diag", "Edad") %in% names(rpc))) {
    stop("Al RPC original le faltan columnas necesarias para tasas ajustadas.")
  }
  dx <- toupper(trimws(as.character(rpc[["CIE10"]])))
  dx[is.na(dx)] <- ""
  cd <- suppressWarnings(as.integer(as.character(rpc[["regCom"]])))
  ano <- suppressWarnings(as.integer(as.character(rpc[["Año_diag"]])))
  ed <- suppressWarnings(as.numeric(as.character(rpc[["Edad"]])))
  pos <- match(cd, mapa$codigo)
  seleccion <- grepl("^C34", dx) & !is.na(pos) & !is.na(ano)
  seleccion[is.na(seleccion)] <- FALSE
  seleccion <- seleccion & ano >= mapa$inicio_rpc[pos] & ano <= mapa$fin_rpc[pos]
  seleccion[is.na(seleccion)] <- FALSE
  if (sum(seleccion) != 1864L || anyNA(ed[seleccion]) ||
      any(ed[seleccion] < 0 | ed[seleccion] > 120)) {
    stop("Conteo o edades RPC inesperados; se detiene la reconstruccion.")
  }
  casos_comuna <- tabulate(pos[seleccion], nbins = nrow(mapa))
  if (!all(casos_comuna == mapa$casos_c34)) {
    stop("La reconstruccion del RPC por comuna ha cambiado.")
  }
  cruda_reconstruida <- casos_comuna / mapa$personas_anio * 100000
  if (any(abs(cruda_reconstruida - mapa$tasa_c34_100k) > 1e-6)) {
    stop("Las tasas crudas C34 no coinciden con las archivadas.")
  }
  cat("TASAS_CRUDAS_C34_COINCIDEN - 18/18 COMUNAS\n")
  edad_grupo <- cut(ed[seleccion], breaks = c(seq(0, 80, 5), Inf),
                    right = FALSE, include.lowest = TRUE, labels = grupos)
  if (anyNA(edad_grupo)) stop("No se pudieron agrupar las edades RPC.")
  casos <- as.data.frame(table(
    comuna = factor(mapa$comuna[pos[seleccion]], levels = referencia$comuna),
    grupo_edad = factor(as.character(edad_grupo), levels = grupos)))
  names(casos)[3] <- "casos"
  den <- aggregate(poblacion ~ comuna + grupo_edad, data = reconstruida, FUN = sum)
  estratos <- merge(casos, den, by = c("comuna", "grupo_edad"), all.x = TRUE)
  if (nrow(estratos) != 306L || anyNA(estratos) ||
      any(estratos$poblacion <= 0) || sum(estratos$casos) != 1864L) {
    stop("Los estratos RPC-INE no superan la auditoria.")
  }
  estandar <- aggregate(poblacion ~ grupo_edad, data = estratos, FUN = sum)
  estandar$peso <- estandar$poblacion / sum(estandar$poblacion)
  estratos$peso <- estandar$peso[match(estratos$grupo_edad, estandar$grupo_edad)]
  tasas <- do.call(rbind, lapply(split(estratos, estratos$comuna), function(z) {
    tasa <- 100000 * sum(z$peso * z$casos / z$poblacion)
    data.frame(comuna = unique(z$comuna), tasa_ajustada_reconstruida = tasa,
               stringsAsFactors = FALSE)
  }))
  tasas <- merge(tasas, ajustadas_ref[, c("comuna", "tasa_ajustada_100k")],
                 by = "comuna", all = TRUE, sort = TRUE)
  if (nrow(tasas) != 18L || anyNA(tasas)) {
    stop("Faltan comunas al comparar tasas ajustadas.")
  }
  tasas <- merge(tasas, data.frame(comuna = mapa$comuna,
                                   tasa_cruda_reconstruida = cruda_reconstruida,
                                   tasa_cruda_referencia = mapa$tasa_c34_100k),
                 by = "comuna", all = TRUE, sort = TRUE)
  tasas$diferencia <- tasas$tasa_ajustada_reconstruida - tasas$tasa_ajustada_100k
  cat("\n=== CONTROL TASAS C34 AJUSTADAS ===\n")
  print(tasas, row.names = FALSE, digits = 8)
  if (anyNA(tasas) || any(abs(tasas$diferencia) > 1e-6)) {
    stop("Las tasas ajustadas reconstruidas no coinciden; no publicar.")
  }
  write.csv(tasas, "results/Tabla_verificacion_tasas_ajustadas_C34.csv",
            row.names = FALSE, fileEncoding = "UTF-8")
  cat("TASAS_AJUSTADAS_C34_COINCIDEN - 18/18 COMUNAS\n")
  cat("\n[B/2] Comprobando las redes desde los RDS procesados...\n")
  source("scripts/03_prepublicacion.R", local = TRUE)
  cat("\n============================================\n")
  cat("AUDITORIA_COMBINADA_TERMINADA\n")
  cat("INE original: verificado frente a denominadores archivados.\n")
  cat("Tasas C34 ajustadas: calculadas desde RPC+INE y cotejadas.\n")
  cat("Redes: comprobadas desde objetos RDS procesados.\n")
  cat("ATENCION: reconstruccion de redes desde todas las fuentes brutas PENDIENTE.\n")
  cat("============================================\n")
})
