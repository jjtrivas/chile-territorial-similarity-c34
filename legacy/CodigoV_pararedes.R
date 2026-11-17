
# ============================================================
# BLOQUE 0 - CIERRE INVESTIGACION RED C34
# Version: 10-11-2026
# ============================================================

# 1. RUTA PRINCIPAL
ruta <- paste0(
  "C:/Users/jjtri/OneDrive - Universidad Mayor/",
  "Documentos/redes"
)

if (!dir.exists(ruta)) {
  stop("ERROR: No se encontro la carpeta redes.")
}

# 2. CREAR CARPETA DEL PROYECTO
proyecto <- file.path(ruta, "CIERRE_C34_2026")

carpetas <- c(
  "01_fuentes_y_diccionario",
  "02_scripts",
  "03_datos_derivados",
  "04_resultados",
  "05_figuras",
  "06_manuscrito"
)

dir.create(proyecto, showWarnings = FALSE)

for (x in carpetas) {
  dir.create(
    file.path(proyecto, x),
    showWarnings = FALSE
  )
}

# 3. SEMILLA GLOBAL
semilla_global <- 1234L
set.seed(semilla_global)

# 4. PLAN DE ANALISIS
archivo_plan <- file.path(proyecto, "PLAN_ANALISIS.md")

if (file.exists(archivo_plan)) {
  stop("Ya existe PLAN_ANALISIS.md. No se sobrescribira.")
}

plan <- c(
  "# PLAN DE ANALISIS - RED DE SIMILITUD C34",
  "",
  paste(
    "Fecha:",
    format(
      Sys.time(),
      "%Y-%m-%d %H:%M:%S %Z",
      tz = "America/Santiago"
    )
  ),
  "",
  "## OBJETIVO",
  "Caracterizar similitudes multidimensionales entre",
  "18 comunas chilenas, incluyendo cancer pulmonar C34,",
  "y explorar correspondencias oncologicas sin circularidad.",
  "",
  "## RED A - PRINCIPAL",
  "Incluye C34, declaraciones regulatorias, indicadores",
  "sociales y demografico-sanitarios.",
  "",
  "## RED B - COMPLEMENTARIA",
  "Excluye C34 de la construccion de similitud.",
  "Permite explorar correspondencia con C34.",
  "",
  "## METODOLOGIA",
  "Distancias euclidianas estandarizadas.",
  "Normalizacion por bloque y pesos equivalentes.",
  "Umbral principal: percentil 0.60.",
  "Sensibilidad: percentiles 0.55 y 0.65.",
  "Louvain y comparacion de agrupamientos.",
  "Analisis de sensibilidad por exclusion de bloques.",
  "",
  "## INDICADOR ONCOLOGICO",
  "C34 con poblaciones historicas segun ventanas RPC.",
  "Pendiente: documentar incertidumbre de las tasas.",
  "",
  "## LIMITACIONES",
  "Estudio ecologico y transversal multitemporal.",
  "RPC: periodos comunales entre 1998 y 2019.",
  "Pobreza: 2017.",
  "Hacinamiento: 2024.",
  "Demografia: 2020.",
  "Infraestructura sanitaria: 2026.",
  "Acceso oncologico efectivo: no medido.",
  "Declaraciones regulatorias no miden exposicion.",
  "Sin inferencias causales ni individuales.",
  "",
  "## REPRODUCIBILIDAD",
  "Semilla global: 1234.",
  "Conservar fuentes originales sin modificaciones.",
  "No publicar identificadores personales.",
  "Cada resultado final debe tener una fuente verificable."
)

writeLines(plan, archivo_plan)

# 5. REGISTRAR SEMILLA
writeLines(
  paste0("SEMILLA_GLOBAL=", semilla_global),
  file.path(
    proyecto,
    "02_scripts",
    "SEMILLA_GLOBAL.txt"
  )
)

# 6. CREAR ARCHIVO DE LIMITACIONES
archivo_limitaciones <- file.path(
  proyecto,
  "06_manuscrito",
  "LIMITACIONES.md"
)

if (!file.exists(archivo_limitaciones)) {
  writeLines(
    c(
      "# LIMITACIONES DEL ESTUDIO",
      "",
      "- Diseno ecologico transversal multitemporal.",
      "- Ventanas RPC diferentes entre comunas.",
      "- Tasas C34 crudas sin ajuste etario confirmado.",
      "- Incertidumbre de tasas pendiente de documentar.",
      "- Variables sanitarias contemporaneas de 2026.",
      "- Acceso oncologico efectivo no medido.",
      "- Declaraciones regulatorias no equivalen a exposicion.",
      "- Riesgo de falacia ecologica.",
      "- Validacion externa no realizada."
    ),
    archivo_limitaciones
  )
}

# 7. REGISTRAR ENTORNO R
writeLines(
  capture.output(sessionInfo()),
  file.path(
    proyecto,
    "sessionInfo_bloque0.txt"
  )
)

# 8. COMPROBAR ARCHIVOS CREADOS
archivos_control <- c(
  archivo_plan,
  archivo_limitaciones,
  file.path(
    proyecto,
    "02_scripts",
    "SEMILLA_GLOBAL.txt"
  ),
  file.path(
    proyecto,
    "sessionInfo_bloque0.txt"
  )
)

stopifnot(
  all(dir.exists(file.path(proyecto, carpetas))),
  all(file.exists(archivos_control))
)

# 9. RESULTADO
cat("\n====================================\n")
cat("BLOQUE 0 - PREPARACION COMPLETADA\n")
cat("====================================\n")
cat("Ruta:", proyecto, "\n")
cat("Carpetas creadas/verificadas:",
    length(carpetas), "\n")
cat("Archivos de control:",
    length(archivos_control), "\n")
cat("Semilla global:", semilla_global, "\n")
cat("Environment original: CONSERVADO\n")
cat("Fuentes originales: SIN MODIFICAR\n")
cat("Inventario MD5: PENDIENTE\n")
cat("renv: PENDIENTE\n")
cat("====================================\n")


# ============================================================
# BLOQUE 0B - INVENTARIO DE FUENTES
# Solo inspecciona; no modifica archivos originales
# ============================================================

archivos <- list.files(
  ruta,
  recursive = TRUE,
  full.names = TRUE
)

archivos <- archivos[file.exists(archivos)]
archivos <- archivos[!dir.exists(archivos)]

# Excluir carpeta de cierre
carpeta_cierre <- paste0(
  normalizePath(proyecto, winslash = "/"),
  "/"
)

archivos <- archivos[
  !startsWith(
    normalizePath(archivos, winslash = "/"),
    carpeta_cierre
  )
]

patron <- paste(
  c(
    "RPC",
    "SalComUsuarios",
    "proyecciones",
    "matriz_18",
    "Red_base",
    "Validacion_robustez",
    "S1_",
    "S2_",
    "RECONSTRUIR",
    "robustez",
    "Red.docx"
  ),
  collapse = "|"
)

seleccion <- archivos[
  grepl(patron, basename(archivos), ignore.case = TRUE)
]

inventario <- data.frame(
  archivo = basename(seleccion),
  ruta = seleccion,
  tamano_MB = round(
    file.info(seleccion)$size / 1024^2, 2
  )
)

cat("\n=== INVENTARIO PRELIMINAR ===\n")
print(inventario, row.names = FALSE)
cat("\nTOTAL:", nrow(inventario), "archivos\n")


# ============================================================
# BLOQUE 0C - MANIFIESTO Y RENV
# No modifica archivos originales
# ============================================================

stopifnot(
  exists("inventario"),
  exists("proyecto"),
  nrow(inventario) > 0,
  dir.exists(proyecto)
)

# 1. Seleccionar archivos relevantes
patron_fuentes <- paste(
  c(
    "^BBDD_RPC_1998-2019_ID\\.sav$",
    "^BD_RPC_1998-2019_ID\\.zip$",
    "^base-2002-proyecciones.*\\.rar$",
    "^SalComUsuarios-[0-9]+Tok\\.xls$",
    "^matriz_18_comunas.*\\.xlsx$",
    "^S1_Preparacion.*\\.R$",
    "^S2_Analisis.*\\.R$",
    "^RECONSTRUIR RED BASE.*\\.R$",
    "^red de similitud.*robustez.*\\.R$",
    "^Red\\.docx$"
  ),
  collapse = "|"
)

fuentes <- inventario[
  grepl(
    patron_fuentes,
    inventario$archivo,
    ignore.case = TRUE
  ),
]

stopifnot(nrow(fuentes) > 0)

# 2. Calcular MD5
fuentes$md5 <- unname(
  tools::md5sum(fuentes$ruta)
)

fuentes$bytes <- as.numeric(
  file.info(fuentes$ruta)$size
)

archivo_md5 <- file.path(
  proyecto,
  "01_fuentes_y_diccionario",
  "MANIFIESTO_MD5.csv"
)

if (file.exists(archivo_md5)) {
  stop("MANIFIESTO_MD5.csv ya existe. Revisarlo.")
}

write.csv(
  fuentes[, c("archivo", "ruta", "bytes", "md5")],
  archivo_md5,
  row.names = FALSE,
  fileEncoding = "UTF-8"
)

# 3. Comprobar libros INE repetidos
ine <- fuentes[
  grepl("^SalComUsuarios-", fuentes$archivo),
]

duplicados <- split(
  ine$md5,
  ine$archivo
)

comparacion_ine <- data.frame(
  archivo = names(duplicados),
  copias = lengths(duplicados),
  md5_iguales = vapply(
    duplicados,
    function(x) {
      !anyNA(x) && length(unique(x)) == 1
    },
    logical(1)
  )
)

write.csv(
  comparacion_ine,
  file.path(
    proyecto,
    "01_fuentes_y_diccionario",
    "COMPARACION_COPIAS_INE.csv"
  ),
  row.names = FALSE
)

# 4. Preparar renv
renv_disponible <- requireNamespace(
  "renv", quietly = TRUE
)

if (renv_disponible) {
  
  if (!file.exists(file.path(proyecto, "renv.lock"))) {
    
    renv::init(
      project = proyecto,
      bare = TRUE,
      restart = FALSE
    )
    
  }
  
  # Las dependencias definitivas se congelaran
  # cuando exista el script maestro.
}

# 5. Resultado
cat("\n=== CIERRE BLOQUE 0 ===\n")
cat("Fuentes con MD5:", nrow(fuentes), "\n")
cat("MD5 fallidos:", sum(is.na(fuentes$md5)), "\n")
cat("Libros INE comprobados:",
    nrow(comparacion_ine), "\n")
cat("Libros INE con copias diferentes:",
    sum(!comparacion_ine$md5_iguales), "\n")
cat("renv disponible:", renv_disponible, "\n")
cat("renv.lock existe:",
    file.exists(file.path(proyecto, "renv.lock")),
    "\n")
cat("=== FIN BLOQUE 0 ===\n")


# =====================================================
# BLOQUE 0D - RESOLVER PENDIENTES
# =====================================================

# 1. Identificar archivo INE diferente
cat("\n=== LIBROS INE DIFERENTES ===\n")

diferentes <- comparacion_ine$archivo[
  !comparacion_ine$md5_iguales
]

print(diferentes)

cat("\n=== RUTAS Y MD5 ===\n")

print(
  ine[
    ine$archivo %in% diferentes,
    c("archivo", "ruta", "md5")
  ],
  row.names = FALSE
)

# 2. Instalar renv si falta
if (!requireNamespace("renv", quietly = TRUE)) {
  install.packages("renv")
}

stopifnot(requireNamespace("renv", quietly = TRUE))

# 3. Inicializar el proyecto
if (!file.exists(file.path(proyecto, "renv.lock"))) {
  renv::init(
    project = proyecto,
    bare = TRUE,
    restart = FALSE
  )
}

# 4. Verificar
cat("\n=== VERIFICACION FINAL ===\n")

cat(
  "renv instalado:",
  requireNamespace("renv", quietly = TRUE),
  "\n"
)

cat(
  "renv.lock presente:",
  file.exists(file.path(proyecto, "renv.lock")),
  "\n"
)

cat("=== FIN BLOQUE 0D ===\n")


# BLOQUE 0E - VERIFICACION Y CIERRE DE RENV

cat("\n=== RUTAS DEL PROYECTO ===\n")
cat("Proyecto esperado:\n")
print(normalizePath(proyecto, winslash = "/"))

cat("Directorio de trabajo actual:\n")
print(normalizePath(getwd(), winslash = "/"))

# Crear el registro inicial de dependencias
renv::snapshot(
  project = proyecto,
  prompt = FALSE
)

# Verificar el archivo
cat("\n=== RESULTADO RENV ===\n")

archivo_lock <- file.path(proyecto, "renv.lock")

cat("renv instalado:",
    requireNamespace("renv", quietly = TRUE), "\n")

cat("renv.lock creado:",
    file.exists(archivo_lock), "\n")

if (file.exists(archivo_lock)) {
  cat("Ubicacion:\n")
  print(normalizePath(archivo_lock, winslash = "/"))
}

cat("\n=== FIN VERIFICACION ===\n")



# BLOQUE 0E - VERIFICACION Y CIERRE DE RENV

cat("\n=== RUTAS DEL PROYECTO ===\n")
cat("Proyecto esperado:\n")
print(normalizePath(proyecto, winslash = "/"))

cat("Directorio de trabajo actual:\n")
print(normalizePath(getwd(), winslash = "/"))

# Crear el registro inicial de dependencias
renv::snapshot(
  project = proyecto,
  prompt = FALSE
)

# Verificar el archivo
cat("\n=== RESULTADO RENV ===\n")

archivo_lock <- file.path(proyecto, "renv.lock")

cat("renv instalado:",
    requireNamespace("renv", quietly = TRUE), "\n")

cat("renv.lock creado:",
    file.exists(archivo_lock), "\n")

if (file.exists(archivo_lock)) {
  cat("Ubicacion:\n")
  print(normalizePath(archivo_lock, winslash = "/"))
}

cat("\n=== FIN VERIFICACION ===\n")


# BLOQUE 0F - COMPARAR POBLACIONES INE BIOBIO

library(readxl)

archivo_1 <- file.path(
  ruta, "base poblacional", "DatCom",
  "SalComUsuarios-08Tok.xls"
)

archivo_2 <- file.path(
  ruta, "SalComUsuarios-08Tok.xls"
)

stopifnot(file.exists(archivo_1), file.exists(archivo_2))

# Leer la misma hoja de ambas copias
a <- read_excel(
  archivo_1,
  sheet = "T1",
  col_names = FALSE,
  .name_repair = "minimal"
)

b <- read_excel(
  archivo_2,
  sheet = "T1",
  col_names = FALSE,
  .name_repair = "minimal"
)

cat("\n=== COMPARACION INE BIOBIO ===\n")

cat("Dimensiones copia 1:",
    nrow(a), "x", ncol(a), "\n")

cat("Dimensiones copia 2:",
    nrow(b), "x", ncol(b), "\n")

cat("Contenido T1 identico:",
    identical(a, b), "\n")

if (identical(dim(a), dim(b))) {
  
  diferencias <- which(
    as.matrix(a) != as.matrix(b) &
      !(is.na(as.matrix(a)) & is.na(as.matrix(b))),
    arr.ind = TRUE
  )
  
  cat("Celdas diferentes:",
      nrow(diferencias), "\n")
  
  if (nrow(diferencias) > 0) {
    print(head(diferencias, 10))
  }
}

cat("\n=== FIN COMPARACION ===\n")

# BLOQUE 0F - COMPARAR POBLACIONES INE BIOBIO

library(readxl)

archivo_1 <- file.path(
  ruta, "base poblacional", "DatCom",
  "SalComUsuarios-08Tok.xls"
)

archivo_2 <- file.path(
  ruta, "SalComUsuarios-08Tok.xls"
)

stopifnot(file.exists(archivo_1), file.exists(archivo_2))

# Leer la misma hoja de ambas copias
a <- read_excel(
  archivo_1,
  sheet = "T1",
  col_names = FALSE,
  .name_repair = "minimal"
)

b <- read_excel(
  archivo_2,
  sheet = "T1",
  col_names = FALSE,
  .name_repair = "minimal"
)

cat("\n=== COMPARACION INE BIOBIO ===\n")

cat("Dimensiones copia 1:",
    nrow(a), "x", ncol(a), "\n")

cat("Dimensiones copia 2:",
    nrow(b), "x", ncol(b), "\n")

cat("Contenido T1 identico:",
    identical(a, b), "\n")

if (identical(dim(a), dim(b))) {
  
  diferencias <- which(
    as.matrix(a) != as.matrix(b) &
      !(is.na(as.matrix(a)) & is.na(as.matrix(b))),
    arr.ind = TRUE
  )
  
  cat("Celdas diferentes:",
      nrow(diferencias), "\n")
  
  if (nrow(diferencias) > 0) {
    print(head(diferencias, 10))
  }
}

cat("\n=== FIN COMPARACION ===\n")


# BLOQUE 0F - COMPARAR POBLACIONES INE BIOBIO

library(readxl)

archivo_1 <- file.path(
  ruta, "base poblacional", "DatCom",
  "SalComUsuarios-08Tok.xls"
)

archivo_2 <- file.path(
  ruta, "SalComUsuarios-08Tok.xls"
)

stopifnot(file.exists(archivo_1), file.exists(archivo_2))

# Leer la misma hoja de ambas copias
a <- read_excel(
  archivo_1,
  sheet = "T1",
  col_names = FALSE,
  .name_repair = "minimal"
)

b <- read_excel(
  archivo_2,
  sheet = "T1",
  col_names = FALSE,
  .name_repair = "minimal"
)

cat("\n=== COMPARACION INE BIOBIO ===\n")

cat("Dimensiones copia 1:",
    nrow(a), "x", ncol(a), "\n")

cat("Dimensiones copia 2:",
    nrow(b), "x", ncol(b), "\n")

cat("Contenido T1 identico:",
    identical(a, b), "\n")

if (identical(dim(a), dim(b))) {
  
  diferencias <- which(
    as.matrix(a) != as.matrix(b) &
      !(is.na(as.matrix(a)) & is.na(as.matrix(b))),
    arr.ind = TRUE
  )
  
  cat("Celdas diferentes:",
      nrow(diferencias), "\n")
  
  if (nrow(diferencias) > 0) {
    print(head(diferencias, 10))
  }
}

cat("\n=== FIN COMPARACION ===\n")

