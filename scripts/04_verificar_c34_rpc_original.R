# ================================================================
# 04_verificar_c34_rpc_original.R
# Reproduccion de conteos C34 desde el RPC original (MINSAL).
# Unidad de conteo: registros C34 por comuna y ventana RPC.
# NO publica ni guarda microdatos, ni identificadores de personas.
# Requiere RStudio abierto en la raiz del repositorio y paquete haven.
#
# Antes de ejecutarlo, establecer una ruta local NO PUBLICA:
# Sys.setenv(RPC_SAV_PATH = "C:/ruta/privada/BBDD_RPC_1998-2019_ID.sav")
# source("scripts/04_verificar_c34_rpc_original.R")
# ================================================================

if (!requireNamespace("haven", quietly = TRUE)) {
  stop("Instala haven: install.packages('haven')")
}

archivo_ref <- file.path("data", "processed", "01_tabla_C34.rds")
archivo_mat <- file.path("data", "processed", "03A_matriz_V2_candidata.rds")
if (!all(file.exists(c(archivo_ref, archivo_mat)))) {
  stop("Abre primero el archivo .Rproj del repositorio; faltan los RDS en data/processed/.")
}

archivo_sav <- Sys.getenv("RPC_SAV_PATH", unset = "")
if (!nzchar(archivo_sav)) {
  stop("Define RPC_SAV_PATH en tu sesion de R con la ruta LOCAL del SAV original. No subas esa ruta ni la base a GitHub.")
}
if (!file.exists(archivo_sav) || !grepl("\\.sav$", archivo_sav, ignore.case = TRUE)) {
  stop("RPC_SAV_PATH no apunta a un archivo .sav disponible.")
}

# Leer la fuente original solamente en memoria.
rpc <- haven::read_sav(archivo_sav)
campos_rpc <- c("CIE10", "regCom", "Año_diag")
if (!all(campos_rpc %in% names(rpc))) {
  stop("El RPC no incluye las columnas esperadas: CIE10, regCom y Año_diag.")
}

referencia <- readRDS(archivo_ref)
matriz <- readRDS(archivo_mat)
cols_ref <- c("comuna", "inicio_rpc", "fin_rpc", "casos_c34")
cols_mat <- c("nombre_comuna_rpc", "codigo_comuna_2008")
if (!all(cols_ref %in% names(referencia)) ||
    !all(cols_mat %in% names(matriz))) {
  stop("La referencia RDS o matriz territorial tiene columnas distintas a las esperadas.")
}

mapa <- unique(data.frame(
  comuna = trimws(as.character(matriz$nombre_comuna_rpc)),
  codigo_rpc = suppressWarnings(as.integer(as.character(matriz$codigo_comuna_2008))),
  stringsAsFactors = FALSE
))

ref <- referencia[, cols_ref, drop = FALSE]
ref$comuna <- trimws(as.character(ref$comuna))
control <- merge(mapa, ref, by = "comuna", all = TRUE, sort = FALSE)

if (nrow(control) != 18L ||
    anyNA(control[, c("comuna", "codigo_rpc", "inicio_rpc", "fin_rpc", "casos_c34")]) ||
    anyDuplicated(control$comuna) || anyDuplicated(control$codigo_rpc)) {
  stop("No se pudieron asociar univocamente las 18 comunas con la referencia.")
}

codigo <- toupper(trimws(as.character(rpc[["CIE10"]])))
codigo[is.na(codigo)] <- ""
comuna <- suppressWarnings(as.integer(as.character(rpc[["regCom"]])))
anio <- suppressWarnings(as.integer(as.character(rpc[["Año_diag"]])))
pos <- match(comuna, control$codigo_rpc)

# Los limites temporales provienen de la referencia archivada.
seleccion <- grepl("^C34", codigo) & !is.na(pos) & !is.na(anio)
seleccion[is.na(seleccion)] <- FALSE
seleccion <- seleccion &
  anio >= control$inicio_rpc[pos] &
  anio <= control$fin_rpc[pos]
seleccion[is.na(seleccion)] <- FALSE

control$casos_rpc_original <- tabulate(pos[seleccion], nbins = nrow(control))
control$diferencia <- control$casos_rpc_original - control$casos_c34
control$coincide <- control$diferencia == 0L
control <- control[order(control$comuna), c(
  "comuna", "codigo_rpc", "inicio_rpc", "fin_rpc", "casos_c34",
  "casos_rpc_original", "diferencia", "coincide"
)]
rownames(control) <- NULL

cat("\n=== VERIFICACION DE REGISTROS C34 DESDE RPC MINSAL ===\n")
print(control, row.names = FALSE)
cat("\nComunas:", nrow(control),
    "\nRegistros C34 en RPC:", sum(control$casos_rpc_original),
    "\nRegistros de referencia:", sum(control$casos_c34),
    "\nCoincidencias:", sum(control$coincide), "/18\n")

valido <- nrow(control) == 18L &&
  sum(control$casos_rpc_original) == 1864L &&
  sum(control$casos_c34) == 1864L &&
  all(control$coincide)

if (!valido) {
  stop("La reconstruccion presenta diferencias. No publicar como validada.")
}

# Publicable solo tras revisar la tabla agregada y permisos de uso.
# El archivo nuevo se mantiene en la carpeta local de resultados.
salida <- file.path("results", "Tabla_verificacion_C34_RPC_original.csv")
write.csv(control, salida, row.names = FALSE, fileEncoding = "UTF-8")
cat("Tabla de verificacion agregada guardada en:", salida, "\n")
cat("\nRPC_C34_COINCIDE_CON_REFERENCIA - 18/18 COMUNAS\n")

# Nota: esto verifica conteos de registros C34, no personas unicas,
# denominadores poblacionales, tasas ajustadas, ni la red completa.
