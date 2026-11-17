
# ============================================================
# BLOQUE 1A - TASAS C34 E INTERVALOS DE CONFIANZA
# Proyecto: Red de similitud territorial - 18 comunas
# Fuente: objeto validacion previamente auditado
# Metodo: intervalos exactos de Poisson al 95%
# ============================================================

# 1. Verificar objetos necesarios

if (!exists("validacion")) {
  stop("No existe el objeto validacion en el Environment.")
}

if (!exists("proyecto")) {
  stop("No existe el objeto proyecto en el Environment.")
}

if (!dir.exists(proyecto)) {
  stop("La carpeta del proyecto no existe.")
}

# 2. Verificar columnas reales

columnas_necesarias <- c(
  "comuna",
  "inicio",
  "fin",
  "anios_cobertura",
  "c34_base_original",
  "personas_anio",
  "tasa_c34_100k"
)

faltantes <- setdiff(
  columnas_necesarias,
  names(validacion)
)

if (length(faltantes) > 0) {
  stop(
    "Faltan columnas: ",
    paste(faltantes, collapse = ", ")
  )
}

# 3. Preparar datos

datos <- as.data.frame(
  validacion[, columnas_necesarias]
)

datos$comuna <- as.character(datos$comuna)

numericas <- setdiff(columnas_necesarias, "comuna")

for (v in numericas) {
  datos[[v]] <- as.numeric(datos[[v]])
}

stopifnot(
  nrow(datos) == 18,
  !anyDuplicated(datos$comuna),
  !anyNA(datos),
  all(is.finite(as.matrix(datos[numericas]))),
  all(datos$c34_base_original >= 0),
  all(datos$c34_base_original ==
        floor(datos$c34_base_original)),
  all(datos$personas_anio > 0),
  all(datos$anios_cobertura ==
        datos$fin - datos$inicio + 1)
)

# 4. Recalcular tasas C34

datos$tasa_recalculada <- (
  datos$c34_base_original /
    datos$personas_anio
) * 100000

diferencia_maxima <- max(abs(
  datos$tasa_recalculada -
    datos$tasa_c34_100k
))

if (diferencia_maxima > 1e-6) {
  stop(
    "Las tasas recalculadas no coinciden ",
    "con la auditoria anterior."
  )
}

# 5. Intervalos exactos de Poisson al 95%

alpha <- 0.05

casos <- datos$c34_base_original
exposicion <- datos$personas_anio

limite_inferior <- ifelse(
  casos == 0,
  0,
  qchisq(alpha / 2, 2 * casos) /
    (2 * exposicion) * 100000
)

limite_superior <- (
  qchisq(
    1 - alpha / 2,
    2 * (casos + 1)
  ) /
    (2 * exposicion)
) * 100000

# 6. Crear tabla definitiva

tabla_c34 <- data.frame(
  comuna = datos$comuna,
  inicio_rpc = datos$inicio,
  fin_rpc = datos$fin,
  anios_cobertura = datos$anios_cobertura,
  casos_c34 = casos,
  personas_anio = exposicion,
  tasa_c34_100k = datos$tasa_recalculada,
  ic95_inferior = limite_inferior,
  ic95_superior = limite_superior,
  stringsAsFactors = FALSE
)

tabla_c34 <- tabla_c34[
  order(tabla_c34$comuna),
]

rownames(tabla_c34) <- NULL

# 7. Verificaciones finales

stopifnot(
  nrow(tabla_c34) == 18,
  all(tabla_c34$ic95_inferior <=
        tabla_c34$tasa_c34_100k),
  all(tabla_c34$ic95_superior >=
        tabla_c34$tasa_c34_100k),
  all(tabla_c34$casos_c34 >= 5)
)

# 8. Guardar archivos

carpeta_datos <- file.path(
  proyecto,
  "03_datos_derivados"
)

carpeta_resultados <- file.path(
  proyecto,
  "04_resultados"
)

dir.create(
  carpeta_datos,
  recursive = TRUE,
  showWarnings = FALSE
)

dir.create(
  carpeta_resultados,
  recursive = TRUE,
  showWarnings = FALSE
)

write.csv(
  tabla_c34,
  file.path(
    carpeta_resultados,
    "01_tasas_C34_IC95.csv"
  ),
  row.names = FALSE,
  fileEncoding = "UTF-8"
)

saveRDS(
  tabla_c34,
  file.path(
    carpeta_datos,
    "01_tabla_C34.rds"
  )
)

# 9. Verificar exportaciones

archivo_csv <- file.path(
  carpeta_resultados,
  "01_tasas_C34_IC95.csv"
)

archivo_rds <- file.path(
  carpeta_datos,
  "01_tabla_C34.rds"
)

tabla_verificacion <- read.csv(
  archivo_csv,
  check.names = FALSE
)

stopifnot(
  file.exists(archivo_csv),
  file.exists(archivo_rds),
  nrow(tabla_verificacion) == 18
)

# 10. Mostrar resultados

cat("\n======================================\n")
cat("BLOQUE 1A - RESULTADOS C34\n")
cat("======================================\n")

cat("Comunas:", nrow(tabla_c34), "\n")
cat("Casos C34:", sum(tabla_c34$casos_c34), "\n")
cat(
  "Diferencia maxima de tasas:",
  diferencia_maxima, "\n"
)

cat(
  "Comunas con menos de 5 casos:",
  sum(tabla_c34$casos_c34 < 5), "\n"
)

cat("\n=== TABLA DE TASAS E IC95 ===\n")

print(
  tabla_c34[, c(
    "comuna",
    "casos_c34",
    "tasa_c34_100k",
    "ic95_inferior",
    "ic95_superior"
  )],
  row.names = FALSE,
  digits = 4
)

cat("\n=== ARCHIVOS GENERADOS ===\n")
cat(archivo_csv, "\n")
cat(archivo_rds, "\n")

cat("\nBLOQUE 1A FINALIZADO\n")


# ==========================================================
# BLOQUE 1B - VIABILIDAD DEL AJUSTE POR EDAD
# Solo inspecciona la estructura de los datos
# No modifica las tasas ni los archivos originales
# ==========================================================

library(readxl)

# 1. Verificar objetos
stopifnot(
  exists("rpc_c34"),
  exists("matriz_c34"),
  exists("proyecto")
)

# 2. Verificar campos RPC
cat("\n=== VARIABLES RPC C34 ===\n")

campos <- c(
  "Edad", "Sexo", "Año_diag", "RPC", "regCom"
)

print(
  data.frame(
    variable = campos,
    existe = campos %in% names(rpc_c34)
  ),
  row.names = FALSE
)

campos_presentes <- intersect(
  campos,
  names(rpc_c34)
)

cat("\n=== TIPOS DE VARIABLES RPC ===\n")

print(
  sapply(
    rpc_c34[campos_presentes],
    function(x) paste(class(x), collapse = "/")
  )
)

cat("\n=== EDAD RPC ===\n")

if ("Edad" %in% names(rpc_c34)) {
  edad <- suppressWarnings(
    as.numeric(rpc_c34$Edad)
  )
  
  cat("Registros C34:", length(edad), "\n")
  cat("Edad faltante:", sum(is.na(edad)), "\n")
  cat("Edad minima:", min(edad, na.rm = TRUE), "\n")
  cat("Edad maxima:", max(edad, na.rm = TRUE), "\n")
}

# 3. Inspeccionar hojas INE de Biobio
archivo_ine <- file.path(
  ruta,
  "base poblacional",
  "DatCom",
  "SalComUsuarios-08Tok.xls"
)

stopifnot(file.exists(archivo_ine))

cat("\n=== HOJAS INE BIOBIO ===\n")

hojas <- excel_sheets(archivo_ine)
print(hojas)

# 4. Examinar posibles hojas de edad
hojas_edad <- intersect(
  c("T2", "T5", "ESAcal"),
  hojas
)

for (hoja in hojas_edad) {
  
  cat("\n=== HOJA:", hoja, "===\n")
  
  muestra <- read_excel(
    archivo_ine,
    sheet = hoja,
    col_names = FALSE,
    n_max = 12,
    .name_repair = "minimal"
  )
  
  cat(
    "Columnas detectadas:",
    ncol(muestra), "\n"
  )
  
  print(
    as.data.frame(
      muestra[, seq_len(min(8, ncol(muestra)))]
    ),
    row.names = FALSE
  )
}

cat("\nBLOQUE 1B - INSPECCION FINALIZADA\n")


# ============================================================
# BLOQUE 1C - ESTRUCTURA DE POBLACION POR EDAD INE
# Inspeccion de las cuatro regiones utilizadas
# No modifica archivos originales
# ============================================================

library(readxl)

regiones <- c("02", "07", "08", "14")

carpeta_ine <- file.path(
  ruta,
  "base poblacional",
  "DatCom"
)

for (region in regiones) {
  
  archivo <- file.path(
    carpeta_ine,
    paste0("SalComUsuarios-", region, "Tok.xls")
  )
  
  cat("\n==================================\n")
  cat("REGION:", region, "\n")
  cat("==================================\n")
  
  stopifnot(file.exists(archivo))
  
  hojas <- excel_sheets(archivo)
  
  cat("Hojas disponibles:\n")
  print(hojas)
  
  for (hoja in intersect(c("T5", "ESAcal"), hojas)) {
    
    datos_hoja <- read_excel(
      archivo,
      sheet = hoja,
      col_names = FALSE,
      n_max = 8,
      .name_repair = "minimal"
    )
    
    cat("\nHOJA:", hoja, "\n")
    cat("Columnas:", ncol(datos_hoja), "\n")
    
    cat("Primeras 8 filas y 10 columnas:\n")
    
    print(
      as.data.frame(
        datos_hoja[
          ,
          seq_len(min(10, ncol(datos_hoja)))
        ]
      ),
      row.names = FALSE
    )
  }
}

cat("\nBLOQUE 1C FINALIZADO\n")


# ============================================================
# BLOQUE 1D - VERIFICACION DE LAS 18 COMUNAS
# Poblacion por edad - INE
# SOLO INSPECCIONA - NO MODIFICA FUENTES
# ============================================================

library(readxl)

stopifnot(
  exists("validacion"),
  exists("ruta"),
  exists("proyecto")
)

# 1. Comunas y ventanas RPC ya auditadas
comunas_rpc <- as.data.frame(
  validacion[, c("comuna", "inicio", "fin")]
)

stopifnot(
  nrow(comunas_rpc) == 18,
  !anyDuplicated(comunas_rpc$comuna)
)

# 2. Asociacion territorial
regiones_comunas <- list(
  "02" = c("Calama", "María Elena", "Tocopilla"),
  "07" = c("Curicó", "Maule", "Talca"),
  "08" = c(
    "Chiguayante", "Concepción", "Coronel",
    "Hualpén", "Hualqui", "Los Ángeles",
    "Lota", "Penco", "San Pedro de la Paz",
    "Talcahuano", "Tomé"
  ),
  "14" = c("Valdivia")
)

normalizar <- function(x) {
  x <- iconv(
    as.character(x),
    from = "",
    to = "ASCII//TRANSLIT"
  )
  toupper(trimws(x))
}

# 3. Inspeccionar estructura regional
resultados <- list()
detalle <- list()
contador <- 0L

for (region in names(regiones_comunas)) {
  
  archivo <- file.path(
    ruta,
    "base poblacional",
    "DatCom",
    paste0("SalComUsuarios-", region, "Tok.xls")
  )
  
  stopifnot(file.exists(archivo))
  
  cat("\nREGION:", region, "\n")
  
  for (hoja in c("T5", "ESAcal")) {
    
    x <- read_excel(
      archivo,
      sheet = hoja,
      col_names = FALSE,
      .name_repair = "minimal",
      col_types = "text"
    )
    
    x <- as.data.frame(x)
    
    # Buscar nombres en todas las columnas.
    # Algunas tablas repiten bloques horizontales.
    matriz_texto <- as.matrix(x)
    matriz_normal <- matrix(
      normalizar(matriz_texto),
      nrow = nrow(matriz_texto),
      ncol = ncol(matriz_texto)
    )
    
    for (comuna in regiones_comunas[[region]]) {
      
      posiciones <- which(
        matriz_normal == normalizar(comuna),
        arr.ind = TRUE
      )
      
      encontrada <- nrow(posiciones) > 0
      
      contador <- contador + 1L
      
      resultados[[contador]] <- data.frame(
        region = region,
        comuna = comuna,
        hoja = hoja,
        encontrada = encontrada,
        posiciones = nrow(posiciones)
      )
      
      if (encontrada) {
        
        # Revisar encabezados próximos al nombre
        for (j in seq_len(nrow(posiciones))) {
          
          fila <- posiciones[j, 1]
          columna <- posiciones[j, 2]
          
          filas_revision <- seq.int(
            fila,
            min(fila + 5L, nrow(x))
          )
          
          ventana <- matriz_texto[
            filas_revision, ,
            drop = FALSE
          ]
          
          valores <- trimws(as.vector(ventana))
          anios <- suppressWarnings(
            as.integer(valores)
          )
          
          anios <- sort(unique(
            anios[
              !is.na(anios) &
                anios >= 1990 &
                anios <= 2020
            ]
          ))
          
          detalle[[length(detalle) + 1L]] <-
            data.frame(
              region = region,
              comuna = comuna,
              hoja = hoja,
              fila = fila,
              columna = columna,
              anios_cercanos = paste(
                anios,
                collapse = ","
              )
            )
        }
      }
    }
  }
}

tabla_cobertura <- do.call(
  rbind,
  resultados
)

tabla_detalle <- if (length(detalle) > 0) {
  do.call(rbind, detalle)
} else {
  data.frame(
    region = character(),
    comuna = character(),
    hoja = character(),
    fila = integer(),
    columna = integer(),
    anios_cercanos = character()
  )
}

# 4. Resumen por comuna
resumen <- reshape(
  tabla_cobertura[
    , c("region", "comuna", "hoja", "encontrada")
  ],
  idvar = c("region", "comuna"),
  timevar = "hoja",
  direction = "wide"
)

names(resumen) <- sub(
  "^encontrada\\.",
  "",
  names(resumen)
)

resumen <- merge(
  comunas_rpc,
  resumen,
  by = "comuna",
  all.x = TRUE,
  sort = FALSE
)

resumen <- resumen[
  order(resumen$region, resumen$comuna),
]

# 5. Guardar diagnostico
salida <- file.path(
  proyecto,
  "04_resultados"
)

dir.create(
  salida,
  recursive = TRUE,
  showWarnings = FALSE
)

write.csv(
  resumen,
  file.path(
    salida,
    "01D_presencia_comunas_INE.csv"
  ),
  row.names = FALSE,
  fileEncoding = "UTF-8"
)

write.csv(
  tabla_detalle,
  file.path(
    salida,
    "01D_encabezados_INE.csv"
  ),
  row.names = FALSE,
  fileEncoding = "UTF-8"
)

# 6. Mostrar resultados
cat("\n====================================\n")
cat("BLOQUE 1D - RESULTADOS\n")
cat("====================================\n")

print(resumen, row.names = FALSE)

cat("\nComunas evaluadas:",
    nrow(resumen), "\n")

cat("Comunas encontradas en T5:",
    sum(resumen$T5, na.rm = TRUE), "\n")

cat("Comunas encontradas en ESAcal:",
    sum(resumen$ESAcal, na.rm = TRUE), "\n")

cat("\n=== ENCABEZADOS DETECTADOS ===\n")

print(
  tabla_detalle,
  row.names = FALSE,
  right = FALSE
)

cat("\nBLOQUE 1D FINALIZADO\n")


# BLOQUE 1E - DIAGNOSTICO DEL ERROR
# Inspeccion de Concepcion, hoja T5

library(readxl)

archivo <- file.path(
  ruta,
  "base poblacional",
  "DatCom",
  "SalComUsuarios-08Tok.xls"
)

x <- as.matrix(as.data.frame(
  read_excel(
    archivo,
    sheet = "T5",
    col_names = FALSE,
    col_types = "text",
    .name_repair = "minimal"
  )
))

# Concepcion comienza en fila 3, columna 1
f <- 3L
c0 <- 1L

filas <- seq.int(f, f + 66L)

etiquetas <- normalizar_edad(x[filas, c0])

cat("\n=== ESTRUCTURA CONCEPCION ===\n")

print(
  data.frame(
    fila = filas,
    etiqueta = etiquetas,
    valor_1990 = x[filas, 2]
  ),
  row.names = FALSE
)

cat("\n=== DETECCION AUTOMATICA ===\n")

cat(
  "Filas TOTAL:",
  sum(etiquetas == "TOTAL", na.rm = TRUE),
  "\n"
)

cat(
  "Filas de edad:",
  sum(es_grupo_edad(x[filas, c0]), na.rm = TRUE),
  "\n"
)

cat("\n=== FIN DIAGNOSTICO ===\n")


# ============================================================
# BLOQUE 1E - EXTRACCION CORREGIDA DE POBLACION POR EDAD
# Proyecto: Red de similitud territorial - 18 comunas
# Fuente: INE, hoja T5
#
# Solo utiliza los años de cobertura RPC.
# Valida las 17 categorias de edad y sus denominadores.
# No modifica los archivos originales.
# ============================================================

library(readxl)

# ------------------------------------------------------------
# 1. COMPROBAR OBJETOS
# ------------------------------------------------------------

stopifnot(
  exists("tabla_detalle"),
  exists("tabla_c34"),
  exists("resumen"),
  exists("ruta"),
  exists("proyecto"),
  nrow(tabla_c34) == 18
)

# ------------------------------------------------------------
# 2. NORMALIZACION DE ETIQUETAS
# ------------------------------------------------------------

normalizar_etiqueta <- function(z) {
  z <- iconv(
    as.character(z),
    to = "ASCII//TRANSLIT"
  )
  
  toupper(gsub("[[:space:]]", "", z))
}

grupos_esperados <- c(
  paste0(
    seq(0, 75, 5),
    "-",
    seq(4, 79, 5)
  ),
  "80+"
)

# ------------------------------------------------------------
# 3. EXTRAER DATOS DE LAS 18 COMUNAS
# ------------------------------------------------------------

registros <- list()
n <- 0L

for (region in unique(resumen$region)) {
  
  archivo <- file.path(
    ruta,
    "base poblacional",
    "DatCom",
    paste0("SalComUsuarios-", region, "Tok.xls")
  )
  
  stopifnot(file.exists(archivo))
  
  x <- as.matrix(as.data.frame(
    read_excel(
      archivo,
      sheet = "T5",
      col_names = FALSE,
      col_types = "text",
      .name_repair = "minimal"
    )
  ))
  
  comunas <- resumen$comuna[
    resumen$region == region
  ]
  
  for (comuna in comunas) {
    
    # Periodo valido para cada comuna
    periodo <- tabla_c34[
      tabla_c34$comuna == comuna,
    ]
    
    stopifnot(nrow(periodo) == 1L)
    
    posiciones <- unique(
      tabla_detalle[
        tabla_detalle$region == region &
          tabla_detalle$comuna == comuna &
          tabla_detalle$hoja == "T5",
        c("fila", "columna")
      ]
    )
    
    if (nrow(posiciones) != 6L) {
      stop(
        "Numero inesperado de bloques en: ",
        comuna
      )
    }
    
    cat(
      "Procesando:",
      comuna,
      "| Periodo:",
      periodo$inicio_rpc,
      "-",
      periodo$fin_rpc,
      "\n"
    )
    
    for (j in seq_len(nrow(posiciones))) {
      
      f <- posiciones$fila[j]
      c0 <- posiciones$columna[j]
      
      fila_anios <- f + 3L
      fila_total <- f + 5L
      
      filas_edades <- seq.int(
        f + 7L,
        f + 23L
      )
      
      columnas <- seq.int(
        c0 + 1L,
        c0 + 6L
      )
      
      # Validar etiquetas de la estructura
      etiquetas <- normalizar_etiqueta(
        x[filas_edades, c0]
      )
      
      if (!identical(
        etiquetas,
        grupos_esperados
      )) {
        stop(
          "Etiquetas etarias inesperadas en ",
          comuna,
          ", bloque ",
          j
        )
      }
      
      if (
        normalizar_etiqueta(
          x[fila_total, c0]
        ) != "TOTAL"
      ) {
        stop(
          "Fila TOTAL no identificada en ",
          comuna
        )
      }
      
      # Leer encabezados anuales
      anios <- suppressWarnings(
        as.integer(
          x[fila_anios, columnas]
        )
      )
      
      for (k in seq_along(columnas)) {
        
        anio <- anios[k]
        
        # Ignorar columnas sin año reconocido
        if (is.na(anio)) {
          next
        }
        
        # Utilizar exclusivamente el periodo RPC
        if (
          anio < periodo$inicio_rpc ||
          anio > periodo$fin_rpc
        ) {
          next
        }
        
        columna <- columnas[k]
        
        total <- suppressWarnings(
          as.numeric(
            x[fila_total, columna]
          )
        )
        
        poblacion <- suppressWarnings(
          as.numeric(
            x[filas_edades, columna]
          )
        )
        
        # Validacion numerica estricta
        if (
          is.na(total) ||
          anyNA(poblacion) ||
          any(poblacion < 0) ||
          sum(poblacion) != total
        ) {
          stop(
            "Error poblacional en ",
            comuna,
            ", año ",
            anio,
            ". Total INE: ",
            total,
            "; suma de edades: ",
            sum(poblacion, na.rm = TRUE)
          )
        }
        
        n <- n + 1L
        
        registros[[n]] <- data.frame(
          comuna = comuna,
          region = region,
          anio = anio,
          grupo_edad = grupos_esperados,
          poblacion = poblacion,
          total_ine = total,
          stringsAsFactors = FALSE
        )
      }
    }
  }
}

# ------------------------------------------------------------
# 4. COMPROBAR EXTRACCION
# ------------------------------------------------------------

if (length(registros) == 0L) {
  stop("No se extrajo ninguna poblacion.")
}

poblacion_completa <- do.call(
  rbind,
  registros
)

clave <- paste(
  poblacion_completa$comuna,
  poblacion_completa$anio,
  poblacion_completa$grupo_edad,
  sep = "|"
)

# Revisar que años repetidos tengan valores iguales
duplicados <- split(
  poblacion_completa$poblacion,
  clave
)

inconsistencias <- sum(
  vapply(
    duplicados,
    function(z) {
      length(unique(z)) > 1L
    },
    logical(1)
  )
)

if (inconsistencias > 0L) {
  stop(
    "Existen valores diferentes para ",
    "una misma comuna, año y edad."
  )
}

# Eliminar repeticiones de años entre bloques
poblacion_edad <- poblacion_completa[
  !duplicated(clave),
]

rownames(poblacion_edad) <- NULL

# ------------------------------------------------------------
# 5. VALIDAR COBERTURA DE CADA COMUNA
# ------------------------------------------------------------

control <- do.call(
  rbind,
  lapply(
    seq_len(nrow(tabla_c34)),
    function(i) {
      
      ref <- tabla_c34[i, ]
      
      z <- poblacion_edad[
        poblacion_edad$comuna == ref$comuna &
          poblacion_edad$anio >= ref$inicio_rpc &
          poblacion_edad$anio <= ref$fin_rpc,
      ]
      
      anios_esperados <- seq.int(
        ref$inicio_rpc,
        ref$fin_rpc
      )
      
      anios_encontrados <- sort(
        unique(z$anio)
      )
      
      cobertura_correcta <- identical(
        as.integer(anios_encontrados),
        as.integer(anios_esperados)
      )
      
      grupos_correctos <- (
        nrow(z) ==
          length(anios_esperados) * 17L
      )
      
      # Un total poblacional por año
      totales <- unique(
        z[, c(
          "anio",
          "total_ine"
        )]
      )
      
      total_reconstruido <- sum(
        totales$total_ine
      )
      
      data.frame(
        comuna = ref$comuna,
        anios_esperados = length(anios_esperados),
        anios_encontrados = length(anios_encontrados),
        cobertura_correcta = cobertura_correcta,
        grupos_correctos = grupos_correctos,
        personas_anio_T5 = total_reconstruido,
        personas_anio_original = ref$personas_anio,
        stringsAsFactors = FALSE
      )
    }
  )
)

control$diferencia <- (
  control$personas_anio_T5 -
    control$personas_anio_original
)

# ------------------------------------------------------------
# 6. MOSTRAR RESULTADOS ANTES DE GUARDAR
# ------------------------------------------------------------

cat("\n====================================\n")
cat("BLOQUE 1E - RESULTADOS\n")
cat("====================================\n")

cat(
  "Comunas extraidas:",
  length(unique(poblacion_edad$comuna)),
  "\n"
)

cat(
  "Grupos de edad:",
  length(unique(poblacion_edad$grupo_edad)),
  "\n"
)

cat(
  "Inconsistencias en años repetidos:",
  inconsistencias,
  "\n"
)

cat("\n=== CONTROL DE LAS 18 COMUNAS ===\n")

print(
  control,
  row.names = FALSE
)

cat("\n=== RESUMEN DE VALIDACION ===\n")

cat(
  "Comunas con cobertura completa:",
  sum(control$cobertura_correcta),
  "\n"
)

cat(
  "Comunas con 17 grupos por año:",
  sum(control$grupos_correctos),
  "\n"
)

cat(
  "Comunas con denominador coincidente:",
  sum(control$diferencia == 0),
  "\n"
)

# ------------------------------------------------------------
# 7. GUARDAR SOLO SI TODO ES CORRECTO
# ------------------------------------------------------------

validacion_ok <- (
  nrow(control) == 18L &&
    all(control$cobertura_correcta) &&
    all(control$grupos_correctos) &&
    all(control$diferencia == 0)
)

if (!validacion_ok) {
  
  cat(
    "\nATENCION: Hay diferencias por revisar.\n",
    "No se guardaran archivos validados.\n"
  )
  
} else {
  
  carpeta_datos <- file.path(
    proyecto,
    "03_datos_derivados"
  )
  
  carpeta_resultados <- file.path(
    proyecto,
    "04_resultados"
  )
  
  saveRDS(
    poblacion_edad,
    file.path(
      carpeta_datos,
      "01E_poblacion_edad_validada.rds"
    )
  )
  
  write.csv(
    control,
    file.path(
      carpeta_resultados,
      "01E_control_poblacion_edad.csv"
    ),
    row.names = FALSE,
    fileEncoding = "UTF-8"
  )
  
  cat("\nARCHIVOS VALIDADOS Y GUARDADOS\n")
}

cat("\nBLOQUE 1E FINALIZADO\n")


# ============================================================
# BLOQUE 1F - VINCULACION RPC Y POBLACION POR EDAD
# Diagnostico previo a estandarizacion directa
# No modifica fuentes ni datos originales
# ============================================================

stopifnot(
  exists("rpc_c34"),
  exists("matriz_c34"),
  exists("tabla_c34"),
  exists("proyecto")
)

archivo_pob <- file.path(
  proyecto,
  "03_datos_derivados",
  "01E_poblacion_edad_validada.rds"
)

stopifnot(file.exists(archivo_pob))

pob <- readRDS(archivo_pob)

cat("\n=== BLOQUE 1F - ESTRUCTURAS ===\n")

cat("\nColumnas del RPC:\n")
print(names(rpc_c34))

cat("\nColumnas de matriz_c34:\n")
print(names(matriz_c34))

cat("\nColumnas de tabla_c34:\n")
print(names(tabla_c34))

cat("\n=== CAMPOS DE IDENTIFICACION RPC ===\n")

campos <- grep(
  "comun|regcom|rpc|cod|edad|sexo|ano|año",
  names(rpc_c34),
  value = TRUE,
  ignore.case = TRUE
)

print(campos)

cat("\n=== CONTROL DE EDADES C34 ===\n")

stopifnot("Edad" %in% names(rpc_c34))

edades <- suppressWarnings(
  as.numeric(rpc_c34$Edad)
)

cat("Registros C34:", length(edades), "\n")
cat("Edades faltantes:", sum(is.na(edades)), "\n")
cat(
  "Edades fuera del rango 0-120:",
  sum(!is.na(edades) &
        (edades < 0 | edades > 120)),
  "\n"
)

grupo_edad_rpc <- cut(
  edades,
  breaks = c(seq(0, 80, 5), Inf),
  right = FALSE,
  include.lowest = TRUE,
  labels = c(
    paste0(
      seq(0, 75, 5), "-",
      seq(4, 79, 5)
    ),
    "80+"
  )
)

conteos_edad <- table(
  grupo_edad_rpc,
  useNA = "ifany"
)

cat("\n=== GRUPOS ETARIOS RPC ===\n")
print(conteos_edad)

cat("\n=== CONTROL POBLACIONAL ===\n")
cat(
  "Comunas INE:",
  length(unique(pob$comuna)),
  "\n"
)
cat(
  "Grupos INE:",
  length(unique(pob$grupo_edad)),
  "\n"
)

cat(
  "Poblacion total en personas-anio:",
  sum(pob$poblacion),
  "\n"
)

cat(
  "Denominador C34 acumulado:",
  sum(tabla_c34$personas_anio),
  "\n"
)

cat(
  "Diferencia:",
  sum(pob$poblacion) -
    sum(tabla_c34$personas_anio),
  "\n"
)

cat("\nBLOQUE 1F FINALIZADO\n")


# ============================================================
# BLOQUE 1G - VINCULACION C34 POR COMUNA Y EDAD
# 18 comunas - Registros Poblacionales de Cancer
# No modifica fuentes originales
# ============================================================

stopifnot(
  exists("rpc_c34"),
  exists("matriz_c34"),
  exists("tabla_c34"),
  exists("proyecto")
)

# 1. Preparar equivalencia entre comuna y codigo RPC

mapa <- as.data.frame(
  matriz_c34[, c(
    "nombre_comuna_rpc",
    "codigo_comuna_2008"
  )]
)

names(mapa) <- c("comuna", "codigo_rpc")

mapa$comuna <- trimws(as.character(mapa$comuna))
mapa$codigo_rpc <- suppressWarnings(
  as.integer(as.character(mapa$codigo_rpc))
)

mapa <- unique(mapa)

stopifnot(
  nrow(mapa) == 18,
  !anyNA(mapa$codigo_rpc),
  !anyDuplicated(mapa$comuna),
  !anyDuplicated(mapa$codigo_rpc)
)

# 2. Comprobar correspondencia con las 18 comunas

referencia <- as.data.frame(tabla_c34)

referencia$comuna <- trimws(
  as.character(referencia$comuna)
)

mapa <- merge(
  mapa,
  referencia[, c(
    "comuna",
    "inicio_rpc",
    "fin_rpc",
    "casos_c34"
  )],
  by = "comuna",
  all = TRUE,
  sort = FALSE
)

stopifnot(
  nrow(mapa) == 18,
  !anyNA(mapa),
  !anyDuplicated(mapa$codigo_rpc)
)

# 3. Extraer registros correspondientes a los codigos

rpc <- as.data.frame(
  rpc_c34[, c(
    "regCom",
    "Año_diag",
    "Edad"
  )]
)

rpc$codigo_rpc <- suppressWarnings(
  as.integer(as.character(rpc$regCom))
)

rpc$anio <- suppressWarnings(
  as.integer(as.character(rpc[["Año_diag"]]))
)

rpc$edad <- suppressWarnings(
  as.numeric(as.character(rpc$Edad))
)

rpc$comuna <- mapa$comuna[
  match(rpc$codigo_rpc, mapa$codigo_rpc)
]

rpc$inicio_rpc <- mapa$inicio_rpc[
  match(rpc$codigo_rpc, mapa$codigo_rpc)
]

rpc$fin_rpc <- mapa$fin_rpc[
  match(rpc$codigo_rpc, mapa$codigo_rpc)
]

# Aplicar seleccion territorial y temporal
seleccion <- (
  !is.na(rpc$comuna) &
    !is.na(rpc$anio) &
    rpc$anio >= rpc$inicio_rpc &
    rpc$anio <= rpc$fin_rpc
)

seleccion[is.na(seleccion)] <- FALSE

rpc_18 <- rpc[seleccion, ]

# 4. Comprobar edades y generar grupos quinquenales

stopifnot(
  !anyNA(rpc_18$edad),
  all(is.finite(rpc_18$edad)),
  all(rpc_18$edad >= 0 & rpc_18$edad <= 120)
)

grupos_edad <- c(
  paste0(seq(0, 75, 5), "-",
         seq(4, 79, 5)),
  "80+"
)

rpc_18$grupo_edad <- as.character(
  cut(
    rpc_18$edad,
    breaks = c(seq(0, 80, 5), Inf),
    right = FALSE,
    include.lowest = TRUE,
    labels = grupos_edad
  )
)

stopifnot(!anyNA(rpc_18$grupo_edad))

# 5. Agrupar sin exponer registros individuales

casos_por_edad <- as.data.frame(
  table(
    comuna = factor(
      rpc_18$comuna,
      levels = referencia$comuna
    ),
    grupo_edad = factor(
      rpc_18$grupo_edad,
      levels = grupos_edad
    )
  )
)

names(casos_por_edad)[3] <- "casos_c34"

# 6. Comparar conteos con auditoria anterior

totales <- aggregate(
  casos_c34 ~ comuna,
  data = casos_por_edad,
  FUN = sum
)

control_rpc <- merge(
  referencia[, c("comuna", "casos_c34")],
  totales,
  by = "comuna",
  suffixes = c("_original", "_reconstruido"),
  sort = TRUE
)

control_rpc$diferencia <- (
  control_rpc$casos_c34_reconstruido -
    control_rpc$casos_c34_original
)

# 7. Informar resultados

cat("\n=== BLOQUE 1G - RESULTADOS ===\n")

cat(
  "Codigos comunales:",
  nrow(mapa), "\n"
)

cat(
  "Casos seleccionados:",
  nrow(rpc_18), "\n"
)

cat(
  "Edades faltantes:",
  sum(is.na(rpc_18$edad)), "\n"
)

cat(
  "Casos esperados:",
  sum(referencia$casos_c34), "\n"
)

cat("\n=== CONTROL POR COMUNA ===\n")

print(
  control_rpc,
  row.names = FALSE
)

cat("\n=== VERIFICACION ===\n")

validacion_rpc <- (
  nrow(rpc_18) == 1864L &&
    nrow(control_rpc) == 18L &&
    all(control_rpc$diferencia == 0)
)

cat(
  "Vinculacion validada:",
  validacion_rpc, "\n"
)

# 8. Guardar solo los totales por comuna
# La tabla por edad se mantiene en memoria porque
# contiene celdas potencialmente menores de 5.

if (validacion_rpc) {
  
  write.csv(
    control_rpc,
    file.path(
      proyecto,
      "04_resultados",
      "01G_control_vinculacion_C34.csv"
    ),
    row.names = FALSE,
    fileEncoding = "UTF-8"
  )
  
  cat("Control agregado guardado.\n")
  
} else {
  
  cat(
    "ATENCION: Vinculacion no validada.\n",
    "No utilizar los casos por edad todavia.\n"
  )
}

cat("\nBLOQUE 1G FINALIZADO\n")



# ============================================================
# BLOQUE 1H - TASAS C34 AJUSTADAS POR EDAD
# 18 comunas - Estandarizacion directa
# Poblacion estandar interna: personas-anio combinadas
# IC95 aproximados bajo supuesto de Poisson
# ============================================================

# 1. Verificar entradas

stopifnot(
  exists("casos_por_edad"),
  exists("tabla_c34"),
  exists("proyecto"),
  exists("validacion_rpc"),
  isTRUE(validacion_rpc)
)

archivo_pob <- file.path(
  proyecto, "03_datos_derivados",
  "01E_poblacion_edad_validada.rds"
)

stopifnot(file.exists(archivo_pob))

pob <- readRDS(archivo_pob)

grupos <- c(
  paste0(seq(0, 75, 5), "-",
         seq(4, 79, 5)),
  "80+"
)

comunas <- as.character(tabla_c34$comuna)

stopifnot(
  length(comunas) == 18L,
  !anyDuplicated(comunas),
  nrow(casos_por_edad) == 18L * 17L,
  !anyNA(pob$poblacion),
  all(pob$poblacion >= 0)
)

# 2. Acumular personas-anio por comuna y edad

denominadores <- aggregate(
  poblacion ~ comuna + grupo_edad,
  data = pob,
  FUN = sum
)

casos <- as.data.frame(casos_por_edad)

casos$comuna <- as.character(casos$comuna)
casos$grupo_edad <- as.character(casos$grupo_edad)
casos$casos_c34 <- as.numeric(casos$casos_c34)

# 3. Vincular casos y denominadores

estratos <- merge(
  casos,
  denominadores,
  by = c("comuna", "grupo_edad"),
  all.x = TRUE,
  sort = FALSE
)

estratos <- estratos[
  order(
    match(estratos$comuna, comunas),
    match(estratos$grupo_edad, grupos)
  ),
]

stopifnot(
  nrow(estratos) == 306L,
  !anyDuplicated(paste(
    estratos$comuna, estratos$grupo_edad
  )),
  !anyNA(estratos),
  all(estratos$poblacion > 0),
  all(estratos$casos_c34 >= 0),
  sum(estratos$casos_c34) == 1864L,
  sum(estratos$poblacion) ==
    sum(tabla_c34$personas_anio)
)

# 4. Construir poblacion estandar interna

estandar <- aggregate(
  poblacion ~ grupo_edad,
  data = estratos,
  FUN = sum
)

estandar <- estandar[
  match(grupos, estandar$grupo_edad),
]

stopifnot(
  nrow(estandar) == 17L,
  !anyNA(estandar$poblacion),
  all(estandar$poblacion > 0)
)

estandar$peso <- (
  estandar$poblacion /
    sum(estandar$poblacion)
)

stopifnot(abs(sum(estandar$peso) - 1) < 1e-10)

estratos$peso <- estandar$peso[
  match(estratos$grupo_edad, estandar$grupo_edad)
]

# 5. Calcular tasas estandarizadas y varianza
# Varianza aproximada de la tasa directamente
# estandarizada bajo independencia Poisson.

calcular_comuna <- function(z) {
  
  tasa_especifica <- (
    z$casos_c34 / z$poblacion
  )
  
  tasa_ajustada <- (
    100000 * sum(z$peso * tasa_especifica)
  )
  
  varianza <- (
    100000^2 *
      sum(
        z$peso^2 *
          z$casos_c34 /
          (z$poblacion^2)
      )
  )
  
  error_estandar <- sqrt(varianza)
  
  # IC aproximado lognormal, cuando la tasa > 0
  if (tasa_ajustada > 0) {
    factor <- 1.96 * error_estandar / tasa_ajustada
    
    li <- tasa_ajustada * exp(-factor)
    ls <- tasa_ajustada * exp(factor)
  } else {
    li <- NA_real_
    ls <- NA_real_
  }
  
  data.frame(
    comuna = unique(z$comuna),
    casos = sum(z$casos_c34),
    personas_anio = sum(z$poblacion),
    tasa_ajustada_100k = tasa_ajustada,
    error_estandar = error_estandar,
    ic95_ajustado_inferior = li,
    ic95_ajustado_superior = ls
  )
}

resultados <- do.call(
  rbind,
  lapply(
    split(estratos, estratos$comuna),
    calcular_comuna
  )
)

rownames(resultados) <- NULL

# 6. Comparar con las tasas crudas verificadas

comparacion <- merge(
  tabla_c34[, c(
    "comuna",
    "casos_c34",
    "personas_anio",
    "tasa_c34_100k"
  )],
  resultados,
  by = "comuna",
  suffixes = c("_original", "_ajustado"),
  sort = TRUE
)

comparacion$diferencia_tasas <- (
  comparacion$tasa_ajustada_100k -
    comparacion$tasa_c34_100k
)

comparacion$razon_ajustada_cruda <- (
  comparacion$tasa_ajustada_100k /
    comparacion$tasa_c34_100k
)

# 7. Verificaciones estrictas

stopifnot(
  nrow(comparacion) == 18L,
  all(comparacion$casos_c34 ==
        comparacion$casos),
  all(comparacion$personas_anio_original ==
        comparacion$personas_anio_ajustado),
  all(is.finite(
    comparacion$tasa_ajustada_100k
  )),
  all(comparacion$tasa_ajustada_100k > 0),
  all(comparacion$ic95_ajustado_inferior <=
        comparacion$tasa_ajustada_100k),
  all(comparacion$ic95_ajustado_superior >=
        comparacion$tasa_ajustada_100k)
)

# 8. Exportar SOLO resultados agregados comunales
# No se exportan casos desagregados por edad.

salida_csv <- file.path(
  proyecto, "04_resultados",
  "01H_comparacion_C34_cruda_ajustada.csv"
)

salida_rds <- file.path(
  proyecto, "03_datos_derivados",
  "01H_tasas_C34_ajustadas.rds"
)

write.csv(
  comparacion,
  salida_csv,
  row.names = FALSE,
  fileEncoding = "UTF-8"
)

saveRDS(comparacion, salida_rds)

# Guardar pesos de la poblacion de referencia;
# no contiene casos individuales ni celdas C34.

write.csv(
  estandar,
  file.path(
    proyecto, "04_resultados",
    "01H_poblacion_estandar_interna.csv"
  ),
  row.names = FALSE,
  fileEncoding = "UTF-8"
)

# 9. Presentar los resultados

cat("\n====================================\n")
cat("BLOQUE 1H - RESULTADOS\n")
cat("====================================\n")

cat("Comunas:", nrow(comparacion), "\n")
cat(
  "Casos C34:",
  sum(comparacion$casos), "\n"
)

cat(
  "Poblacion estandar (personas-anio):",
  sum(estandar$poblacion), "\n"
)

cat(
  "Peso estandar acumulado:",
  sum(estandar$peso), "\n"
)

cat("\n=== TASAS CRUDAS Y AJUSTADAS ===\n")

print(
  comparacion[, c(
    "comuna",
    "casos",
    "tasa_c34_100k",
    "tasa_ajustada_100k",
    "ic95_ajustado_inferior",
    "ic95_ajustado_superior"
  )],
  row.names = FALSE,
  digits = 4
)

cat("\n=== COMPARACION DE ORDENAMIENTOS ===\n")

rho <- cor(
  comparacion$tasa_c34_100k,
  comparacion$tasa_ajustada_100k,
  method = "spearman"
)

cat(
  "Correlacion Spearman cruda-ajustada:",
  round(rho, 4), "\n"
)

cat("\n=== ARCHIVOS ===\n")
cat(salida_csv, "\n")
cat(salida_rds, "\n")

cat("\nBLOQUE 1H FINALIZADO\n")


# ============================================================
# BLOQUE 2A - REDES TERRITORIALES A, B Y A AJUSTADA
# 18 comunas - Distancia euclidiana por bloques
# Percentil 60 de similitud
# ============================================================

stopifnot(
  exists("matriz_c34"),
  exists("tabla_c34"),
  exists("proyecto"),
  exists("comparacion")
)

if (!requireNamespace("igraph", quietly = TRUE)) {
  stop("Falta igraph. Instalar con install.packages('igraph').")
}

set.seed(1234)

# ------------------------------------------------------------
# 1. BASE ANALITICA
# ------------------------------------------------------------

comunas <- as.character(tabla_c34$comuna)

pos <- match(comunas, matriz_c34$nombre_comuna_rpc)

stopifnot(
  length(comunas) == 18L,
  !anyNA(pos),
  !anyDuplicated(pos)
)

base <- as.data.frame(matriz_c34[pos, ])

base$comuna <- comunas

# Agregar C34 crudo y ajustado por nombre de comuna
idx <- match(comunas, comparacion$comuna)

stopifnot(!anyNA(idx))

base$c34_crudo <- comparacion$tasa_c34_100k[idx]
base$c34_ajustado <- comparacion$tasa_ajustada_100k[idx]

# ------------------------------------------------------------
# 2. DEFINIR BLOQUES
# ------------------------------------------------------------

bloque_ambiental <- c(
  "n_declaraciones_zona",
  "tiene_latente_num"
)

bloque_social <- c(
  "pobreza_multidimensional_2017",
  "porcentaje_viviendas_hacinadas_2024"
)

bloque_demografico_sanitario <- c(
  "porcentaje_65mas_2020",
  "aps_publicas_10k_log",
  "hospital_alta_complejidad_num"
)

variables <- c(
  bloque_ambiental,
  bloque_social,
  bloque_demografico_sanitario,
  "c34_crudo",
  "c34_ajustado"
)

faltantes <- setdiff(variables, names(base))

if (length(faltantes) > 0L) {
  stop(
    "Variables ausentes: ",
    paste(faltantes, collapse = ", ")
  )
}

for (v in variables) {
  base[[v]] <- as.numeric(base[[v]])
}

stopifnot(
  !anyNA(base[, variables]),
  all(vapply(
    base[, variables],
    function(z) all(is.finite(z)),
    logical(1)
  ))
)

# ------------------------------------------------------------
# 3. FUNCION DE DISTANCIA POR BLOQUE
# ------------------------------------------------------------

distancia_bloque <- function(datos, variables) {
  
  x <- as.matrix(datos[, variables, drop = FALSE])
  
  desv <- apply(x, 2, sd)
  
  if (any(!is.finite(desv)) || any(desv == 0)) {
    stop(
      "Variable sin variacion en bloque: ",
      paste(variables, collapse = ", ")
    )
  }
  
  x <- scale(x)
  
  d <- as.matrix(dist(x, method = "euclidean"))
  
  maximo <- max(d)
  
  if (maximo <= 0) {
    stop("Distancia maxima del bloque igual a cero.")
  }
  
  d / maximo
}

d_ambiental <- distancia_bloque(
  base, bloque_ambiental
)

d_social <- distancia_bloque(
  base, bloque_social
)

d_demografico <- distancia_bloque(
  base, bloque_demografico_sanitario
)

d_c34_crudo <- distancia_bloque(
  transform(base, c34_crudo = log1p(c34_crudo)),
  "c34_crudo"
)

d_c34_ajustado <- distancia_bloque(
  transform(base, c34_ajustado = log1p(c34_ajustado)),
  "c34_ajustado"
)

# Red A: cuatro bloques
d_A <- (
  d_ambiental +
    d_social +
    d_demografico +
    d_c34_crudo
) / 4

# Red B: tres bloques, sin informacion C34
d_B <- (
  d_ambiental +
    d_social +
    d_demografico
) / 3

# Red A ajustada: cuatro bloques
d_A_ajustada <- (
  d_ambiental +
    d_social +
    d_demografico +
    d_c34_ajustado
) / 4

dimnames(d_A) <- list(comunas, comunas)
dimnames(d_B) <- list(comunas, comunas)
dimnames(d_A_ajustada) <- list(comunas, comunas)

# ------------------------------------------------------------
# 4. CONSTRUIR REDES AL PERCENTIL 60
# ------------------------------------------------------------

crear_red <- function(d, etiqueta) {
  
  similitud <- 1 - d
  
  valores <- similitud[upper.tri(similitud)]
  
  umbral <- as.numeric(
    quantile(valores, probs = 0.60, names = FALSE)
  )
  
  indices <- which(
    upper.tri(similitud) & similitud >= umbral,
    arr.ind = TRUE
  )
  
  aristas <- data.frame(
    from = rownames(similitud)[indices[, 1]],
    to = colnames(similitud)[indices[, 2]],
    similitud = similitud[indices],
    stringsAsFactors = FALSE
  )
  
  vertices <- data.frame(
    name = rownames(similitud)
  )
  
  g <- igraph::graph_from_data_frame(
    aristas,
    directed = FALSE,
    vertices = vertices
  )
  
  set.seed(1234)
  
  comunidades <- igraph::cluster_louvain(
    g,
    weights = igraph::E(g)$similitud
  )
  
  componentes <- igraph::components(g)
  
  lista_grados <- igraph::degree(g)
  
  resumen <- data.frame(
    red = etiqueta,
    nodos = igraph::vcount(g),
    aristas = igraph::ecount(g),
    umbral = umbral,
    componentes = componentes$no,
    aislados = sum(lista_grados == 0),
    comunidades_louvain = length(comunidades),
    modularidad = igraph::modularity(
      comunidades
    ),
    stringsAsFactors = FALSE
  )
  
  list(
    grafo = g,
    aristas = aristas,
    similitud = similitud,
    distancias = d,
    comunidades = comunidades,
    resumen = resumen
  )
}

red_A <- crear_red(d_A, "A_C34_crudo")
red_B <- crear_red(d_B, "B_sin_C34")
red_A_ajustada <- crear_red(
  d_A_ajustada, "A_C34_ajustado"
)

# ------------------------------------------------------------
# 5. COMPARAR ENLACES ENTRE REDES
# ------------------------------------------------------------

claves_aristas <- function(red) {
  sort(
    apply(
      red$aristas[, c("from", "to"), drop = FALSE],
      1,
      function(z) paste(sort(z), collapse = "|")
    )
  )
}

comparar_redes <- function(red1, red2, nombre) {
  
  a <- claves_aristas(red1)
  b <- claves_aristas(red2)
  
  comunes <- length(intersect(a, b))
  union <- length(union(a, b))
  
  data.frame(
    comparacion = nombre,
    enlaces_comunes = comunes,
    solo_primera = length(setdiff(a, b)),
    solo_segunda = length(setdiff(b, a)),
    jaccard = if (union > 0) comunes / union else NA_real_
  )
}

comparaciones <- rbind(
  comparar_redes(
    red_A, red_B,
    "A_crudo_vs_B_sin_C34"
  ),
  comparar_redes(
    red_A, red_A_ajustada,
    "A_crudo_vs_A_ajustado"
  )
)

# ------------------------------------------------------------
# 6. GUARDAR RESULTADOS
# ------------------------------------------------------------

carpeta_resultados <- file.path(
  proyecto, "04_resultados"
)

carpeta_datos <- file.path(
  proyecto, "03_datos_derivados"
)

carpeta_figuras <- file.path(
  proyecto, "05_figuras"
)

dir.create(carpeta_figuras, showWarnings = FALSE)

resumen_redes <- do.call(
  rbind,
  lapply(
    list(red_A, red_B, red_A_ajustada),
    function(z) z$resumen
  )
)

write.csv(
  resumen_redes,
  file.path(
    carpeta_resultados,
    "02A_resumen_redes.csv"
  ),
  row.names = FALSE
)

write.csv(
  comparaciones,
  file.path(
    carpeta_resultados,
    "02A_comparacion_enlaces.csv"
  ),
  row.names = FALSE
)

for (nombre in c("A", "B", "A_ajustada")) {
  
  objeto <- switch(
    nombre,
    A = red_A,
    B = red_B,
    A_ajustada = red_A_ajustada
  )
  
  write.csv(
    objeto$aristas,
    file.path(
      carpeta_resultados,
      paste0("02A_aristas_", nombre, ".csv")
    ),
    row.names = FALSE
  )
}

saveRDS(
  list(
    base = base,
    A = red_A,
    B = red_B,
    A_ajustada = red_A_ajustada,
    comparaciones = comparaciones
  ),
  file.path(
    carpeta_datos,
    "02A_redes_comparables.rds"
  )
)

# ------------------------------------------------------------
# 7. GENERAR FIGURAS PRELIMINARES
# ------------------------------------------------------------

dibujar_red <- function(objeto, nombre) {
  
  g <- objeto$grafo
  
  set.seed(1234)
  
  posiciones <- igraph::layout_with_fr(
    g,
    weights = igraph::E(g)$similitud
  )
  
  grupo <- igraph::membership(objeto$comunidades)
  
  colores <- grDevices::hcl.colors(
    max(grupo),
    palette = "Set 2"
  )
  
  archivo <- file.path(
    carpeta_figuras,
    paste0("02A_red_", nombre, ".png")
  )
  
  png(
    filename = archivo,
    width = 1800,
    height = 1400,
    res = 180
  )
  
  plot(
    g,
    layout = posiciones,
    vertex.color = colores[grupo],
    vertex.size = 19,
    vertex.label.cex = 0.70,
    vertex.label.color = "black",
    vertex.frame.color = "white",
    edge.width = 1.5,
    edge.color = "grey65",
    main = paste("Red territorial:", nombre)
  )
  
  dev.off()
}

dibujar_red(red_A, "A_C34_crudo")
dibujar_red(red_B, "B_sin_C34")
dibujar_red(red_A_ajustada, "A_C34_ajustado")

# ------------------------------------------------------------
# 8. IMPRIMIR RESULTADOS
# ------------------------------------------------------------

cat("\n====================================\n")
cat("BLOQUE 2A - RESULTADOS DE REDES\n")
cat("====================================\n")

cat("\n=== RESUMEN ===\n")
print(
  resumen_redes,
  row.names = FALSE,
  digits = 5
)

cat("\n=== COMPARACION ESTRUCTURAL ===\n")
print(
  comparaciones,
  row.names = FALSE,
  digits = 5
)

cat("\n=== COMUNAS AISLADAS ===\n")

for (nombre in c("A", "B", "A_ajustada")) {
  
  objeto <- switch(
    nombre,
    A = red_A,
    B = red_B,
    A_ajustada = red_A_ajustada
  )
  
  grados <- igraph::degree(objeto$grafo)
  
  cat(
    nombre, ":",
    paste(
      names(grados)[grados == 0],
      collapse = ", "
    ),
    "\n"
  )
}

cat("\n=== ARCHIVOS EXPORTADOS ===\n")
cat("Tablas de redes: 04_resultados\n")
cat("Objetos reproducibles: 03_datos_derivados\n")
cat("Figuras PNG: 05_figuras\n")

cat("\nBLOQUE 2A FINALIZADO\n")


# ============================================================
# BLOQUE 3A - DEMOGRAFIA HISTORICA
# Red de similitud territorial - 18 comunas
# Reconstruccion del porcentaje de poblacion 65+
# durante la ventana RPC de cada comuna
# ============================================================

# 1. Verificar archivos y objetos

stopifnot(
  exists("proyecto"),
  dir.exists(proyecto)
)

archivo_pob <- file.path(
  proyecto,
  "03_datos_derivados",
  "01E_poblacion_edad_validada.rds"
)

archivo_c34 <- file.path(
  proyecto,
  "03_datos_derivados",
  "01_tabla_C34.rds"
)

archivo_redes <- file.path(
  proyecto,
  "03_datos_derivados",
  "02A_redes_comparables.rds"
)

stopifnot(
  file.exists(archivo_pob),
  file.exists(archivo_c34),
  file.exists(archivo_redes)
)

pob <- readRDS(archivo_pob)
c34 <- readRDS(archivo_c34)
redes_v1 <- readRDS(archivo_redes)

base_v1 <- redes_v1$base

# 2. Comprobar estructura

stopifnot(
  all(c("comuna", "anio", "grupo_edad",
        "poblacion") %in% names(pob)),
  all(c("comuna", "inicio_rpc", "fin_rpc",
        "personas_anio") %in% names(c34)),
  "porcentaje_65mas_2020" %in% names(base_v1),
  nrow(c34) == 18,
  nrow(base_v1) == 18,
  !anyDuplicated(c34$comuna),
  !anyDuplicated(base_v1$comuna),
  setequal(c34$comuna, base_v1$comuna),
  !anyNA(pob$poblacion),
  all(pob$poblacion >= 0)
)

# 3. Definir grupos etarios mayores de 65

grupos_65mas <- c(
  "65-69",
  "70-74",
  "75-79",
  "80+"
)

# 4. Calcular indicador por comuna

resultados <- lapply(
  seq_len(nrow(c34)),
  function(i) {
    
    ref <- c34[i, ]
    
    z <- pob[
      pob$comuna == ref$comuna &
        pob$anio >= ref$inicio_rpc &
        pob$anio <= ref$fin_rpc,
    ]
    
    anios_esperados <- seq.int(
      ref$inicio_rpc,
      ref$fin_rpc
    )
    
    stopifnot(
      setequal(unique(z$anio), anios_esperados),
      nrow(z) == 17L * length(anios_esperados),
      !anyDuplicated(paste(z$anio, z$grupo_edad)),
      all(grupos_65mas %in% z$grupo_edad)
    )
    
    total <- sum(z$poblacion)
    
    mayores <- sum(
      z$poblacion[
        z$grupo_edad %in% grupos_65mas
      ]
    )
    
    stopifnot(
      total == ref$personas_anio,
      mayores >= 0,
      mayores <= total
    )
    
    data.frame(
      comuna = as.character(ref$comuna),
      inicio_rpc = ref$inicio_rpc,
      fin_rpc = ref$fin_rpc,
      personas_anio = total,
      personas_anio_65mas = mayores,
      porcentaje_65mas_historico =
        100 * mayores / total
    )
  }
)

demografia_historica <- do.call(
  rbind,
  resultados
)

rownames(demografia_historica) <- NULL

# 5. Comparar con indicador original de 2020

pos <- match(
  demografia_historica$comuna,
  base_v1$comuna
)

stopifnot(!anyNA(pos))

demografia_historica$porcentaje_65mas_2020 <-
  as.numeric(
    base_v1$porcentaje_65mas_2020[pos]
  )

demografia_historica$diferencia_puntos <-
  demografia_historica$porcentaje_65mas_historico -
  demografia_historica$porcentaje_65mas_2020

stopifnot(
  nrow(demografia_historica) == 18,
  !anyNA(demografia_historica),
  all(is.finite(
    demografia_historica$porcentaje_65mas_historico
  )),
  all(
    demografia_historica$porcentaje_65mas_historico
    >= 0 &
      demografia_historica$porcentaje_65mas_historico
    <= 100
  )
)

# 6. Crear MATRIZ CANDIDATA V2
# No sustituir todavia la matriz V1

base_v2_candidata <- base_v1

idx <- match(
  base_v2_candidata$comuna,
  demografia_historica$comuna
)

stopifnot(!anyNA(idx))

base_v2_candidata$porcentaje_65mas_historico <-
  demografia_historica$porcentaje_65mas_historico[idx]

# Conservar ambas variables para comparar.
# No reemplazar porcentaje_65mas_2020 todavia.

# 7. Guardar resultados

carpeta_datos <- file.path(
  proyecto, "03_datos_derivados"
)

carpeta_resultados <- file.path(
  proyecto, "04_resultados"
)

write.csv(
  demografia_historica,
  file.path(
    carpeta_resultados,
    "03A_demografia_historica_18_comunas.csv"
  ),
  row.names = FALSE,
  fileEncoding = "UTF-8"
)

saveRDS(
  base_v2_candidata,
  file.path(
    carpeta_datos,
    "03A_matriz_V2_candidata.rds"
  )
)

# 8. Mostrar resultados

cat("\n====================================\n")
cat("BLOQUE 3A - RESULTADOS\n")
cat("====================================\n")

cat(
  "Comunas verificadas:",
  nrow(demografia_historica), "\n"
)

cat(
  "Personas-anio totales:",
  sum(demografia_historica$personas_anio),
  "\n"
)

cat("\n=== DEMOGRAFIA HISTORICA VS 2020 ===\n")

print(
  demografia_historica[, c(
    "comuna",
    "inicio_rpc",
    "fin_rpc",
    "porcentaje_65mas_2020",
    "porcentaje_65mas_historico",
    "diferencia_puntos"
  )],
  row.names = FALSE,
  digits = 4
)

cat("\n=== COMPARACION ===\n")

rho <- cor(
  demografia_historica$porcentaje_65mas_2020,
  demografia_historica$porcentaje_65mas_historico,
  method = "spearman"
)

cat(
  "Spearman historico vs 2020:",
  round(rho, 4), "\n"
)

cat(
  "Diferencia absoluta maxima (puntos):",
  round(max(abs(
    demografia_historica$diferencia_puntos
  )), 4),
  "\n"
)

cat("\nArchivos guardados correctamente.\n")
cat("BLOQUE 3A FINALIZADO\n")


# ============================================================
# BLOQUE 3B - SENSIBILIDAD A DEMOGRAFIA HISTORICA
# Cambio exclusivo: porcentaje 65+ 2020 -> historico
# No modifica ni sobrescribe redes originales
# ============================================================

stopifnot(exists("proyecto"), dir.exists(proyecto))

if (!requireNamespace("igraph", quietly = TRUE)) {
  stop("Se necesita el paquete igraph.")
}

archivo_redes <- file.path(
  proyecto, "03_datos_derivados",
  "02A_redes_comparables.rds"
)

archivo_v2 <- file.path(
  proyecto, "03_datos_derivados",
  "03A_matriz_V2_candidata.rds"
)

stopifnot(
  file.exists(archivo_redes),
  file.exists(archivo_v2)
)

original <- readRDS(archivo_redes)
base <- readRDS(archivo_v2)

stopifnot(
  nrow(base) == 18L,
  !anyDuplicated(base$comuna),
  setequal(base$comuna, rownames(original$A$distancias))
)

comunas <- rownames(original$A$distancias)
base <- base[match(comunas, base$comuna), ]

# 1. Calcular únicamente el bloque que cambia

variables_comunes <- c(
  "aps_publicas_10k_log",
  "hospital_alta_complejidad_num"
)

distancia_bloque <- function(datos, variables) {
  
  x <- as.matrix(datos[, variables, drop = FALSE])
  storage.mode(x) <- "double"
  
  stopifnot(
    !anyNA(x),
    all(is.finite(x)),
    all(apply(x, 2, sd) > 0)
  )
  
  d <- as.matrix(dist(scale(x)))
  
  stopifnot(max(d) > 0)
  
  d / max(d)
}

d_anterior <- distancia_bloque(
  base,
  c("porcentaje_65mas_2020", variables_comunes)
)

d_historica <- distancia_bloque(
  base,
  c("porcentaje_65mas_historico", variables_comunes)
)

dimnames(d_anterior) <- list(comunas, comunas)
dimnames(d_historica) <- list(comunas, comunas)

cambio <- d_historica - d_anterior

# 2. Actualizar las distancias conservando
# los otros bloques exactamente iguales

distancias_nuevas <- list(
  A = original$A$distancias + cambio / 4,
  B = original$B$distancias + cambio / 3,
  A_ajustada = original$A_ajustada$distancias +
    cambio / 4
)

# 3. Construir redes con el mismo percentil 60

crear_red <- function(d, nombre) {
  
  stopifnot(
    is.matrix(d),
    nrow(d) == 18L,
    max(abs(d - t(d))) < 1e-10
  )
  
  s <- 1 - d
  
  umbral <- as.numeric(
    quantile(
      s[upper.tri(s)],
      probs = 0.60,
      names = FALSE
    )
  )
  
  pos <- which(
    upper.tri(s) & s >= umbral,
    arr.ind = TRUE
  )
  
  aristas <- data.frame(
    from = rownames(s)[pos[, 1]],
    to = colnames(s)[pos[, 2]],
    similitud = s[pos],
    stringsAsFactors = FALSE
  )
  
  g <- igraph::graph_from_data_frame(
    aristas,
    directed = FALSE,
    vertices = data.frame(name = rownames(s))
  )
  
  set.seed(1234)
  
  comunidad <- igraph::cluster_louvain(
    g,
    weights = igraph::E(g)$similitud
  )
  
  grados <- igraph::degree(g)
  
  resumen <- data.frame(
    red = nombre,
    nodos = igraph::vcount(g),
    aristas = igraph::ecount(g),
    umbral = umbral,
    componentes = igraph::components(g)$no,
    aislados = sum(grados == 0),
    comunidades = length(comunidad),
    modularidad = igraph::modularity(comunidad)
  )
  
  list(
    grafo = g,
    aristas = aristas,
    distancias = d,
    comunidades = comunidad,
    resumen = resumen
  )
}

nuevas <- list(
  A = crear_red(
    distancias_nuevas$A,
    "A_demografia_historica"
  ),
  B = crear_red(
    distancias_nuevas$B,
    "B_demografia_historica"
  ),
  A_ajustada = crear_red(
    distancias_nuevas$A_ajustada,
    "A_ajustada_demografia_historica"
  )
)

# 4. Comparar los enlaces con V1

claves <- function(objeto) {
  
  aristas <- objeto$aristas
  
  sort(apply(
    aristas[, c("from", "to"), drop = FALSE],
    1,
    function(z) paste(sort(z), collapse = " | ")
  ))
}

comparar <- function(antes, despues, nombre) {
  
  x <- claves(antes)
  y <- claves(despues)
  
  comunes <- length(intersect(x, y))
  unicos <- length(union(x, y))
  
  data.frame(
    red = nombre,
    enlaces_originales = length(x),
    enlaces_historicos = length(y),
    conservados = comunes,
    eliminados = length(setdiff(x, y)),
    agregados = length(setdiff(y, x)),
    jaccard = comunes / unicos
  )
}

comparacion_temporal <- do.call(
  rbind,
  lapply(
    names(nuevas),
    function(nombre) {
      comparar(
        original[[nombre]],
        nuevas[[nombre]],
        nombre
      )
    }
  )
)

resumen_original <- do.call(
  rbind,
  lapply(
    names(nuevas),
    function(nombre) {
      z <- original[[nombre]]$resumen
      z$red <- nombre
      z
    }
  )
)

resumen_historico <- do.call(
  rbind,
  lapply(nuevas, function(z) z$resumen)
)

# 5. Guardar los nuevos resultados sin
# reemplazar los archivos anteriores

write.csv(
  comparacion_temporal,
  file.path(
    proyecto, "04_resultados",
    "03B_comparacion_temporal_redes.csv"
  ),
  row.names = FALSE,
  fileEncoding = "UTF-8"
)

saveRDS(
  list(
    redes = nuevas,
    comparacion = comparacion_temporal,
    cambio_bloque = cambio
  ),
  file.path(
    proyecto, "03_datos_derivados",
    "03B_redes_demografia_historica.rds"
  )
)

# 6. Resultados

cat("\n=== BLOQUE 3B - RESULTADOS ===\n")

cat("\n=== REDES ORIGINALES ===\n")
print(resumen_original, row.names = FALSE, digits = 5)

cat("\n=== REDES CON DEMOGRAFIA HISTORICA ===\n")
print(resumen_historico, row.names = FALSE, digits = 5)

cat("\n=== CAMBIOS DE CONEXIONES ===\n")
print(comparacion_temporal, row.names = FALSE, digits = 5)

cat("\n=== COMUNAS AISLADAS ===\n")

for (nombre in names(nuevas)) {
  
  g <- nuevas[[nombre]]$grafo
  grados <- igraph::degree(g)
  
  cat(
    nombre, ":",
    paste(names(grados)[grados == 0], collapse = ", "),
    "\n"
  )
}

cat("\n=== ENLACES MODIFICADOS ===\n")

for (nombre in names(nuevas)) {
  
  anteriores <- claves(original[[nombre]])
  actuales <- claves(nuevas[[nombre]])
  
  cat("\nRED:", nombre, "\n")
  
  cat("Eliminados:\n")
  print(setdiff(anteriores, actuales))
  
  cat("Agregados:\n")
  print(setdiff(actuales, anteriores))
}

cat("\nBLOQUE 3B FINALIZADO\n")


# ============================================================
# BLOQUE 3C - VIABILIDAD TEMPORAL DE VARIABLES SOCIALES
# 18 comunas
# No reemplaza variables ni modifica matrices originales
# ============================================================

stopifnot(exists("proyecto"), dir.exists(proyecto))

archivo_redes <- file.path(
  proyecto, "03_datos_derivados",
  "02A_redes_comparables.rds"
)

archivo_c34 <- file.path(
  proyecto, "03_datos_derivados",
  "01_tabla_C34.rds"
)

stopifnot(file.exists(archivo_redes),
          file.exists(archivo_c34))

base <- readRDS(archivo_redes)$base
c34 <- readRDS(archivo_c34)

stopifnot(
  nrow(base) == 18L,
  nrow(c34) == 18L,
  setequal(base$comuna, c34$comuna)
)

# 1. Identificar indicadores sociales existentes
patron <- paste(
  "pobreza",
  "hacinamiento",
  "hacinadas",
  sep = "|"
)

variables <- grep(
  patron, names(base),
  value = TRUE,
  ignore.case = TRUE
)

cat("\n=== VARIABLES SOCIALES EXISTENTES ===\n")
print(variables)

# 2. Variables con año explícito
ingresos <- grep(
  "^pobreza_ingresos_[0-9]{4}$",
  names(base),
  value = TRUE
)

multidimensional <- grep(
  "^pobreza_multidimensional_[0-9]{4}$",
  names(base),
  value = TRUE
)

# 3. Verificar disponibilidad numérica
disponibilidad <- do.call(
  rbind,
  lapply(
    c(ingresos, multidimensional),
    function(v) {
      valores <- suppressWarnings(
        as.numeric(base[[v]])
      )
      
      data.frame(
        variable = v,
        comunas_con_dato = sum(is.finite(valores)),
        comunas_sin_dato = sum(!is.finite(valores))
      )
    }
  )
)

cat("\n=== DISPONIBILIDAD POR VARIABLE ===\n")
print(disponibilidad, row.names = FALSE)

# 4. Localizar años disponibles dentro de ventana RPC
seleccionar <- function(comuna, inicio, fin, vars) {
  
  anos <- as.integer(sub(
    ".*_([0-9]{4})$", "\\1", vars
  ))
  
  elegibles <- which(
    anos >= inicio & anos <= fin
  )
  
  if (!length(elegibles)) {
    return(NA_character_)
  }
  
  # Preferir el año más reciente dentro de la ventana
  elegibles <- elegibles[order(
    anos[elegibles], decreasing = TRUE
  )]
  
  fila <- match(comuna, base$comuna)
  
  for (j in elegibles) {
    valor <- suppressWarnings(
      as.numeric(base[[vars[j]]][fila])
    )
    
    if (length(valor) == 1L && is.finite(valor)) {
      return(vars[j])
    }
  }
  
  NA_character_
}

control <- do.call(
  rbind,
  lapply(seq_len(nrow(c34)), function(i) {
    
    ref <- c34[i, ]
    
    data.frame(
      comuna = as.character(ref$comuna),
      inicio_rpc = ref$inicio_rpc,
      fin_rpc = ref$fin_rpc,
      ingresos_en_ventana = seleccionar(
        ref$comuna,
        ref$inicio_rpc,
        ref$fin_rpc,
        ingresos
      ),
      multidimensional_en_ventana = seleccionar(
        ref$comuna,
        ref$inicio_rpc,
        ref$fin_rpc,
        multidimensional
      ),
      stringsAsFactors = FALSE
    )
  })
)

# 5. Evaluar disponibilidad del hacinamiento
variable_hacinamiento <-
  "porcentaje_viviendas_hacinadas_2024"

control$hacinamiento_2024_disponible <-
  if (variable_hacinamiento %in% names(base)) {
    !is.na(suppressWarnings(as.numeric(
      base[[variable_hacinamiento]][
        match(control$comuna, base$comuna)
      ]
    )))
  } else {
    FALSE
  }

# 6. Guardar diagnóstico
destino <- file.path(
  proyecto, "04_resultados",
  "03C_disponibilidad_social_historica.csv"
)

write.csv(
  control,
  destino,
  row.names = FALSE,
  na = "",
  fileEncoding = "UTF-8"
)

# 7. Mostrar resultados
cat("\n=== BLOQUE 3C - RESULTADOS ===\n")

print(
  control[, c(
    "comuna",
    "inicio_rpc",
    "fin_rpc",
    "ingresos_en_ventana",
    "multidimensional_en_ventana"
  )],
  row.names = FALSE,
  na.print = "SIN DATO"
)

cat("\n=== RESUMEN ===\n")

cat(
  "Comunas con pobreza por ingresos en ventana:",
  sum(!is.na(control$ingresos_en_ventana)),
  "de 18\n"
)

cat(
  "Comunas con pobreza multidimensional en ventana:",
  sum(!is.na(control$multidimensional_en_ventana)),
  "de 18\n"
)

cat(
  "Comunas con hacinamiento 2024 disponible:",
  sum(control$hacinamiento_2024_disponible),
  "de 18\n"
)

cat("\nArchivo generado:\n")
cat(destino, "\n")

cat("\nBLOQUE 3C FINALIZADO\n")


# ============================================================
# BLOQUE 3D - POBREZA HISTORICA POR INGRESOS
# 18 comunas - Preparacion de indicador candidato
# NO modifica las redes ni la matriz original
# ============================================================

stopifnot(
  exists("proyecto"),
  dir.exists(proyecto)
)

archivo_base <- file.path(
  proyecto, "03_datos_derivados",
  "02A_redes_comparables.rds"
)

archivo_control <- file.path(
  proyecto, "04_resultados",
  "03C_disponibilidad_social_historica.csv"
)

stopifnot(
  file.exists(archivo_base),
  file.exists(archivo_control)
)

base <- readRDS(archivo_base)$base

control_social <- read.csv(
  archivo_control,
  stringsAsFactors = FALSE,
  fileEncoding = "UTF-8",
  na.strings = c("", "NA")
)

stopifnot(
  nrow(base) == 18L,
  nrow(control_social) == 18L,
  !anyDuplicated(base$comuna),
  !anyDuplicated(control_social$comuna),
  setequal(base$comuna, control_social$comuna),
  "pobreza_ingresos_2017" %in% names(base),
  "ingresos_en_ventana" %in% names(control_social),
  !anyNA(control_social$ingresos_en_ventana)
)

# 1. Construir tabla histórica sin modificar V1

datos <- control_social[
  match(base$comuna, control_social$comuna),
]

datos$anio_pobreza <- as.integer(
  sub(
    ".*_([0-9]{4})$",
    "\\1",
    datos$ingresos_en_ventana
  )
)

datos$pobreza_ingresos_historica <- vapply(
  seq_len(nrow(datos)),
  function(i) {
    
    variable <- datos$ingresos_en_ventana[i]
    
    if (!variable %in% names(base)) {
      stop("Variable no encontrada: ", variable)
    }
    
    as.numeric(base[[variable]][i])
  },
  numeric(1)
)

datos$pobreza_ingresos_2017 <- as.numeric(
  base$pobreza_ingresos_2017
)

datos$diferencia_historica_2017 <-
  datos$pobreza_ingresos_historica -
  datos$pobreza_ingresos_2017

# 2. Comprobaciones

stopifnot(
  nrow(datos) == 18L,
  all(is.finite(datos$pobreza_ingresos_historica)),
  all(is.finite(datos$pobreza_ingresos_2017)),
  all(datos$pobreza_ingresos_historica >= 0),
  all(datos$pobreza_ingresos_2017 >= 0),
  all(datos$anio_pobreza >= datos$inicio_rpc),
  all(datos$anio_pobreza <= datos$fin_rpc)
)

# 3. Crear matriz candidata
# Mantener V1 intacta

base_social_candidata <- base

base_social_candidata$pobreza_ingresos_historica <-
  datos$pobreza_ingresos_historica

base_social_candidata$anio_pobreza_historica <-
  datos$anio_pobreza

# 4. Guardar resultados

archivo_csv <- file.path(
  proyecto, "04_resultados",
  "03D_pobreza_ingresos_historica.csv"
)

archivo_rds <- file.path(
  proyecto, "03_datos_derivados",
  "03D_matriz_social_candidata.rds"
)

write.csv(
  datos[, c(
    "comuna",
    "inicio_rpc",
    "fin_rpc",
    "anio_pobreza",
    "ingresos_en_ventana",
    "pobreza_ingresos_historica",
    "pobreza_ingresos_2017",
    "diferencia_historica_2017"
  )],
  archivo_csv,
  row.names = FALSE,
  fileEncoding = "UTF-8"
)

saveRDS(
  base_social_candidata,
  archivo_rds
)

# 5. Mostrar resumen

cat("\n=== BLOQUE 3D - RESULTADOS ===\n")

print(
  datos[, c(
    "comuna",
    "anio_pobreza",
    "pobreza_ingresos_historica",
    "pobreza_ingresos_2017",
    "diferencia_historica_2017"
  )],
  row.names = FALSE,
  digits = 4
)

cat("\n=== CONTROL ===\n")

cat(
  "Comunas con indicador histórico:",
  nrow(datos), "\n"
)

cat(
  "Años seleccionados:",
  paste(sort(unique(datos$anio_pobreza)),
        collapse = ", "),
  "\n"
)

cat(
  "Rango histórico:",
  paste(
    range(datos$pobreza_ingresos_historica),
    collapse = " - "
  ),
  "\n"
)

cat(
  "Diferencia absoluta máxima:",
  max(abs(datos$diferencia_historica_2017)),
  "\n"
)

cat("\nArchivos guardados:\n")
cat(archivo_csv, "\n")
cat(archivo_rds, "\n")

cat("\nBLOQUE 3D FINALIZADO\n")


# ============================================================
# BLOQUE 3E - SENSIBILIDAD SOCIOECONOMICA
# Tres escenarios: S0, S1, S2
# Se conserva demografia historica
# No sobrescribe redes anteriores
# ============================================================

stopifnot(exists("proyecto"), dir.exists(proyecto))
stopifnot(requireNamespace("igraph", quietly = TRUE))

carpeta_datos <- file.path(proyecto, "03_datos_derivados")
carpeta_resultados <- file.path(proyecto, "04_resultados")

v1 <- readRDS(file.path(
  carpeta_datos, "02A_redes_comparables.rds"
))

temporal <- readRDS(file.path(
  carpeta_datos, "03B_redes_demografia_historica.rds"
))

social <- readRDS(file.path(
  carpeta_datos, "03D_matriz_social_candidata.rds"
))

comunas <- rownames(temporal$redes$B$distancias)
base <- social[match(comunas, social$comuna), ]

stopifnot(
  nrow(base) == 18,
  !anyNA(base$comuna),
  identical(as.character(base$comuna), comunas)
)

# 1. Distancia social: dos variables por escenario

distancia_social <- function(variable_pobreza) {
  
  x <- as.matrix(base[, c(
    variable_pobreza,
    "porcentaje_viviendas_hacinadas_2024"
  )])
  
  storage.mode(x) <- "double"
  
  stopifnot(
    !anyNA(x),
    all(is.finite(x)),
    all(apply(x, 2, sd) > 0)
  )
  
  d <- as.matrix(dist(scale(x)))
  d <- d / max(d)
  dimnames(d) <- list(comunas, comunas)
  d
}

d_S0 <- distancia_social("pobreza_multidimensional_2017")
d_S1 <- distancia_social("pobreza_ingresos_2017")
d_S2 <- distancia_social("pobreza_ingresos_historica")

# 2. Sustituir solo el bloque social

redes_base <- temporal$redes

crear_escenario <- function(d_social_nueva, etiqueta) {
  
  salida <- list()
  
  for (nombre in c("A", "B", "A_ajustada")) {
    
    anterior <- redes_base[[nombre]]$distancias
    
    peso <- if (nombre == "B") 3 else 4
    
    d <- anterior + (d_social_nueva - d_S0) / peso
    d <- (d + t(d)) / 2
    diag(d) <- 0
    
    stopifnot(
      all(is.finite(d)),
      min(d) >= -1e-9,
      max(d) <= 1 + 1e-9
    )
    
    sim <- 1 - d
    umbral <- as.numeric(quantile(
      sim[upper.tri(sim)],
      0.60,
      names = FALSE
    ))
    
    posiciones <- which(
      upper.tri(sim) & sim >= umbral,
      arr.ind = TRUE
    )
    
    aristas <- data.frame(
      from = rownames(sim)[posiciones[, 1]],
      to = colnames(sim)[posiciones[, 2]],
      similitud = sim[posiciones]
    )
    
    grafo <- igraph::graph_from_data_frame(
      aristas,
      directed = FALSE,
      vertices = data.frame(name = comunas)
    )
    
    set.seed(1234)
    
    comunidad <- igraph::cluster_louvain(
      grafo,
      weights = igraph::E(grafo)$similitud
    )
    
    salida[[nombre]] <- list(
      distancias = d,
      aristas = aristas,
      grafo = grafo,
      comunidad = comunidad,
      resumen = data.frame(
        escenario = etiqueta,
        red = nombre,
        enlaces = igraph::ecount(grafo),
        componentes = igraph::components(grafo)$no,
        aislados = sum(igraph::degree(grafo) == 0),
        modularidad = igraph::modularity(comunidad)
      )
    )
  }
  
  salida
}

escenarios <- list(
  S0 = crear_escenario(d_S0, "S0_multidimensional2017"),
  S1 = crear_escenario(d_S1, "S1_ingresos2017"),
  S2 = crear_escenario(d_S2, "S2_ingresos_historicos")
)

# 3. Comparar enlaces

claves <- function(objeto) {
  a <- objeto$aristas
  sort(apply(
    a[, c("from", "to"), drop = FALSE],
    1,
    function(z) paste(sort(z), collapse = " | ")
  ))
}

comparar <- function(x, y, etiqueta, red) {
  
  a <- claves(x)
  b <- claves(y)
  
  inter <- length(intersect(a, b))
  un <- length(union(a, b))
  
  data.frame(
    contraste = etiqueta,
    red = red,
    enlaces_comunes = inter,
    eliminados = length(setdiff(a, b)),
    agregados = length(setdiff(b, a)),
    jaccard = inter / un
  )
}

comparaciones <- do.call(rbind, lapply(
  c("A", "B", "A_ajustada"),
  function(nombre) {
    rbind(
      comparar(
        escenarios$S0[[nombre]],
        escenarios$S1[[nombre]],
        "S0_vs_S1_definicion",
        nombre
      ),
      comparar(
        escenarios$S1[[nombre]],
        escenarios$S2[[nombre]],
        "S1_vs_S2_temporalidad",
        nombre
      )
    )
  }
))

resumen <- do.call(rbind, lapply(
  escenarios,
  function(e) do.call(rbind, lapply(
    e, function(x) x$resumen
  ))
))

# 4. Validar S0 frente al Bloque 3B

for (nombre in c("A", "B", "A_ajustada")) {
  stopifnot(
    max(abs(
      escenarios$S0[[nombre]]$distancias -
        redes_base[[nombre]]$distancias
    )) < 1e-10
  )
}

# 5. Exportar resultados

write.csv(
  comparaciones,
  file.path(
    carpeta_resultados,
    "03E_comparacion_pobreza_redes.csv"
  ),
  row.names = FALSE,
  fileEncoding = "UTF-8"
)

write.csv(
  resumen,
  file.path(
    carpeta_resultados,
    "03E_resumen_escenarios_sociales.csv"
  ),
  row.names = FALSE,
  fileEncoding = "UTF-8"
)

saveRDS(
  escenarios,
  file.path(
    carpeta_datos,
    "03E_redes_sensibilidad_social.rds"
  )
)

# 6. Mostrar resultados

cat("\n=== BLOQUE 3E - RESULTADOS ===\n")

cat("\n=== RESUMEN DE ESCENARIOS ===\n")
print(resumen, row.names = FALSE, digits = 5)

cat("\n=== COMPARACION DE ENLACES ===\n")
print(comparaciones, row.names = FALSE, digits = 5)

cat("\n=== CONTROL DE REPRODUCCION ===\n")
cat("S0 reproduce las distancias del Bloque 3B: TRUE\n")

cat("\nBLOQUE 3E FINALIZADO\n")


# ============================================================
# BLOQUE 4A - CORRESPONDENCIA TERRITORIAL Y C34
# Redes B S0/S1/S2, sin cancer en la similitud
# Tasas C34 crudas y ajustadas por edad
# Spearman y 1999 permutaciones de etiquetas
# ============================================================

stopifnot(exists("proyecto"), dir.exists(proyecto))

carpeta_datos <- file.path(proyecto, "03_datos_derivados")
carpeta_resultados <- file.path(proyecto, "04_resultados")

escenarios <- readRDS(file.path(
  carpeta_datos,
  "03E_redes_sensibilidad_social.rds"
))

tasas <- readRDS(file.path(
  carpeta_datos,
  "01H_tasas_C34_ajustadas.rds"
))

stopifnot(
  all(c("S0", "S1", "S2") %in% names(escenarios)),
  all(c("comuna", "tasa_c34_100k",
        "tasa_ajustada_100k") %in% names(tasas)),
  nrow(tasas) == 18L,
  !anyDuplicated(tasas$comuna)
)

# 1. Función para construir distancias entre tasas

diferencias_c34 <- function(valores) {
  valores <- log1p(as.numeric(valores))
  abs(outer(valores, valores, "-"))
}

# 2. Función de prueba por permutación
# La unidad permutada es la comuna, no cada pareja.

evaluar <- function(similitud, valores, nperm = 1999L) {
  
  stopifnot(
    is.matrix(similitud),
    nrow(similitud) == 18L,
    ncol(similitud) == 18L,
    length(valores) == 18L,
    all(is.finite(similitud)),
    all(is.finite(valores)),
    all(valores >= 0),
    max(abs(similitud - t(similitud))) < 1e-10
  )
  
  pares <- upper.tri(similitud)
  x <- similitud[pares]
  
  diferencias <- diferencias_c34(valores)
  y <- diferencias[pares]
  
  observado <- cor(
    x, y,
    method = "spearman"
  )
  
  n <- length(valores)
  
  permutados <- numeric(nperm)
  
  set.seed(1234)
  
  for (b in seq_len(nperm)) {
    
    orden <- sample.int(n)
    
    diferencias_perm <- diferencias[
      orden, orden, drop = FALSE
    ]
    
    permutados[b] <- cor(
      x,
      diferencias_perm[pares],
      method = "spearman"
    )
  }
  
  # Contraste bilateral
  p_bilateral <- (
    1 + sum(abs(permutados) >= abs(observado))
  ) / (nperm + 1)
  
  # Contraste direccional: similitud mayor,
  # diferencias de C34 menores (rho negativo).
  p_direccional <- (
    1 + sum(permutados <= observado)
  ) / (nperm + 1)
  
  data.frame(
    rho_spearman = observado,
    p_perm_bilateral = p_bilateral,
    p_perm_direccional = p_direccional,
    n_comunas = n,
    n_pares = sum(pares),
    n_permutaciones = nperm
  )
}

# 3. Ejecutar para escenarios S0, S1 y S2
# y para ambas definiciones de tasas C34.

resultados <- list()
indice <- 1L

for (esc in c("S0", "S1", "S2")) {
  
  d <- escenarios[[esc]]$B$distancias
  
  stopifnot(
    is.matrix(d),
    !is.null(rownames(d)),
    identical(rownames(d), colnames(d)),
    setequal(rownames(d), tasas$comuna)
  )
  
  comunas <- rownames(d)
  
  pos <- match(comunas, tasas$comuna)
  
  for (tipo in c("cruda", "ajustada")) {
    
    valores <- if (tipo == "cruda") {
      tasas$tasa_c34_100k[pos]
    } else {
      tasas$tasa_ajustada_100k[pos]
    }
    
    res <- evaluar(
      similitud = 1 - d,
      valores = valores,
      nperm = 1999L
    )
    
    res$escenario <- esc
    res$tipo_c34 <- tipo
    
    resultados[[indice]] <- res
    indice <- indice + 1L
  }
}

tabla_resultados <- do.call(rbind, resultados)

rownames(tabla_resultados) <- NULL

tabla_resultados <- tabla_resultados[, c(
  "escenario",
  "tipo_c34",
  "rho_spearman",
  "p_perm_bilateral",
  "p_perm_direccional",
  "n_comunas",
  "n_pares",
  "n_permutaciones"
)]

# Ajuste exploratorio por seis contrastes
tabla_resultados$p_BH_bilateral <- p.adjust(
  tabla_resultados$p_perm_bilateral,
  method = "BH"
)

# 4. Validaciones

stopifnot(
  nrow(tabla_resultados) == 6L,
  all(is.finite(tabla_resultados$rho_spearman)),
  all(abs(tabla_resultados$rho_spearman) <= 1),
  all(tabla_resultados$n_comunas == 18L),
  all(tabla_resultados$n_pares == 153L),
  all(tabla_resultados$p_perm_bilateral > 0),
  all(tabla_resultados$p_perm_bilateral <= 1)
)

# 5. Exportar tabla agregada

archivo_salida <- file.path(
  carpeta_resultados,
  "04A_correspondencia_B_C34.csv"
)

write.csv(
  tabla_resultados,
  archivo_salida,
  row.names = FALSE,
  fileEncoding = "UTF-8"
)

saveRDS(
  tabla_resultados,
  file.path(
    carpeta_datos,
    "04A_correspondencia_B_C34.rds"
  )
)

# 6. Mostrar resultados

cat("\n=== BLOQUE 4A - RESULTADOS ===\n")

print(
  tabla_resultados,
  row.names = FALSE,
  digits = 5
)

cat("\n=== INTERPRETACION DEL SIGNO ===\n")

cat(
  "Rho negativo: comunas territorialmente",
  "mas similares tienden a mostrar",
  "menores diferencias de C34.\n"
)

cat(
  "Rho positivo: comunas territorialmente",
  "mas similares tienden a mostrar",
  "mayores diferencias de C34.\n"
)

cat(
  "\nLos p-valores corresponden a",
  "permutaciones de etiquetas comunales.\n"
)

cat("\nArchivo:\n")
cat(archivo_salida, "\n")

cat("\nBLOQUE 4A FINALIZADO\n")


# ============================================================
# BLOQUE 4B - SENSIBILIDAD POR EXCLUSION DE COMUNAS
# 18 comunas; escenarios S0, S1 y S2
# C34 cruda y ajustada
# No modifica las redes existentes
# ============================================================

stopifnot(exists("proyecto"), dir.exists(proyecto))

datos_dir <- file.path(proyecto, "03_datos_derivados")
res_dir <- file.path(proyecto, "04_resultados")

escenarios <- readRDS(file.path(
  datos_dir, "03E_redes_sensibilidad_social.rds"
))

tasas <- readRDS(file.path(
  datos_dir, "01H_tasas_C34_ajustadas.rds"
))

previo <- readRDS(file.path(
  datos_dir, "04A_correspondencia_B_C34.rds"
))

stopifnot(
  all(c("S0", "S1", "S2") %in% names(escenarios)),
  nrow(tasas) == 18L,
  !anyDuplicated(tasas$comuna),
  nrow(previo) == 6L
)

# Correlacion entre similitud territorial y
# diferencia absoluta de log1p(tasa C34).
# Se usan todas las parejas del subconjunto.

calcular_rho <- function(d, tasas_alineadas, mantener) {
  
  subd <- d[mantener, mantener, drop = FALSE]
  
  valores <- log1p(tasas_alineadas[mantener])
  
  diferencias <- abs(outer(valores, valores, "-"))
  
  pares <- upper.tri(subd)
  
  suppressWarnings(cor(
    (1 - subd)[pares],
    diferencias[pares],
    method = "spearman"
  ))
}

detalle <- list()
resumen <- list()
k <- 1L

for (esc in c("S0", "S1", "S2")) {
  
  d <- escenarios[[esc]]$B$distancias
  
  stopifnot(
    nrow(d) == 18L,
    identical(rownames(d), colnames(d)),
    setequal(rownames(d), tasas$comuna),
    all(is.finite(d))
  )
  
  comunas <- rownames(d)
  pos <- match(comunas, tasas$comuna)
  
  for (tipo in c("cruda", "ajustada")) {
    
    variable <- if (tipo == "cruda") {
      "tasa_c34_100k"
    } else {
      "tasa_ajustada_100k"
    }
    
    valores <- as.numeric(tasas[[variable]][pos])
    
    stopifnot(
      length(valores) == 18L,
      all(is.finite(valores)),
      all(valores >= 0)
    )
    
    rho_total <- calcular_rho(
      d, valores, rep(TRUE, 18)
    )
    
    # Comprobar reproduccion del Bloque 4A.
    fila_previa <- previo[
      previo$escenario == esc &
        previo$tipo_c34 == tipo,
    ]
    
    stopifnot(
      nrow(fila_previa) == 1L,
      abs(rho_total - fila_previa$rho_spearman) <
        1e-10
    )
    
    loo <- do.call(rbind, lapply(
      seq_along(comunas),
      function(i) {
        
        mantener <- seq_along(comunas) != i
        
        data.frame(
          escenario = esc,
          tipo_c34 = tipo,
          comuna_excluida = comunas[i],
          rho_completo = rho_total,
          rho_sin_comuna = calcular_rho(
            d, valores, mantener
          )
        )
      }
    ))
    
    loo$cambio_absoluto <- abs(
      loo$rho_sin_comuna - loo$rho_completo
    )
    
    # Exclusion conjunta, predefinida por
    # su interes en la auditoria oncológica.
    mantener_dos <- !comunas %in% c(
      "María Elena", "Tocopilla"
    )
    
    stopifnot(sum(mantener_dos) == 16L)
    
    rho_sin_dos <- calcular_rho(
      d, valores, mantener_dos
    )
    
    detalle[[k]] <- loo
    
    resumen[[k]] <- data.frame(
      escenario = esc,
      tipo_c34 = tipo,
      rho_completo = rho_total,
      rho_min_sin_una = min(loo$rho_sin_comuna),
      rho_max_sin_una = max(loo$rho_sin_comuna),
      comuna_mayor_influencia =
        loo$comuna_excluida[
          which.max(loo$cambio_absoluto)
        ],
      cambio_maximo = max(loo$cambio_absoluto),
      rho_sin_maria_elena_tocopilla = rho_sin_dos,
      conserva_signo_en_todas =
        all(loo$rho_sin_comuna < 0)
    )
    
    k <- k + 1L
  }
}

detalle_4B <- do.call(rbind, detalle)
resumen_4B <- do.call(rbind, resumen)

rownames(detalle_4B) <- NULL
rownames(resumen_4B) <- NULL

stopifnot(
  nrow(detalle_4B) == 108L,
  nrow(resumen_4B) == 6L,
  all(is.finite(detalle_4B$rho_sin_comuna)),
  all(is.finite(
    resumen_4B$rho_sin_maria_elena_tocopilla
  ))
)

# Exportar
write.csv(
  detalle_4B,
  file.path(
    res_dir,
    "04B_detalle_exclusion_comunas.csv"
  ),
  row.names = FALSE,
  fileEncoding = "UTF-8"
)

write.csv(
  resumen_4B,
  file.path(
    res_dir,
    "04B_resumen_sensibilidad_comunas.csv"
  ),
  row.names = FALSE,
  fileEncoding = "UTF-8"
)

saveRDS(
  list(detalle = detalle_4B, resumen = resumen_4B),
  file.path(
    datos_dir,
    "04B_sensibilidad_comunas.rds"
  )
)

# Resultados
cat("\n=== BLOQUE 4B - RESULTADOS ===\n")

print(
  resumen_4B,
  row.names = FALSE,
  digits = 4
)

cat("\n=== CONTROL ===\n")

cat(
  "Analisis completos:",
  nrow(resumen_4B), "\n"
)

cat(
  "Exclusiones individuales:",
  nrow(detalle_4B), "\n"
)

cat(
  "Analisis que mantienen signo negativo",
  "en todas las exclusiones:",
  sum(resumen_4B$conserva_signo_en_todas),
  "de 6\n"
)

cat("\nBLOQUE 4B FINALIZADO\n")


# ============================================================
# BLOQUE 4C - ROBUSTEZ DE LA RED B
# Escenario S0 con demografia historica
# Umbrales p55, p60, p65
# Sensibilidad por exclusion de bloques
# No modifica resultados anteriores
# ============================================================

stopifnot(exists("proyecto"), dir.exists(proyecto))
stopifnot(requireNamespace("igraph", quietly = TRUE))

datos_dir <- file.path(proyecto, "03_datos_derivados")
res_dir <- file.path(proyecto, "04_resultados")

base <- readRDS(file.path(
  datos_dir, "03A_matriz_V2_candidata.rds"
))

temporal <- readRDS(file.path(
  datos_dir, "03B_redes_demografia_historica.rds"
))

tasas <- readRDS(file.path(
  datos_dir, "01H_tasas_C34_ajustadas.rds"
))

referencia <- temporal$redes$B$distancias
comunas <- rownames(referencia)

base <- base[match(comunas, base$comuna), ]
tasas <- tasas[match(comunas, tasas$comuna), ]

stopifnot(
  nrow(base) == 18L,
  nrow(tasas) == 18L,
  identical(as.character(base$comuna), comunas),
  identical(as.character(tasas$comuna), comunas)
)

# 1. Variables de la Red B

bloques <- list(
  ambiental = c(
    "n_declaraciones_zona",
    "tiene_latente_num"
  ),
  social = c(
    "pobreza_multidimensional_2017",
    "porcentaje_viviendas_hacinadas_2024"
  ),
  demografico_sanitario = c(
    "porcentaje_65mas_historico",
    "aps_publicas_10k_log",
    "hospital_alta_complejidad_num"
  )
)

variables <- unlist(bloques, use.names = FALSE)

stopifnot(
  all(variables %in% names(base))
)

# 2. Distancias por bloque

calcular_distancia <- function(vars) {
  
  x <- as.matrix(base[, vars, drop = FALSE])
  storage.mode(x) <- "double"
  
  stopifnot(
    all(is.finite(x)),
    all(apply(x, 2, sd) > 0)
  )
  
  d <- as.matrix(dist(scale(x)))
  d <- d / max(d)
  
  dimnames(d) <- list(comunas, comunas)
  d
}

dist_bloques <- lapply(bloques, calcular_distancia)

# 3. Matrices completas y leave-one-block-out

distancias <- list(
  completa = Reduce("+", dist_bloques) / 3,
  sin_ambiental = (
    dist_bloques$social +
      dist_bloques$demografico_sanitario
  ) / 2,
  sin_social = (
    dist_bloques$ambiental +
      dist_bloques$demografico_sanitario
  ) / 2,
  sin_demografico_sanitario = (
    dist_bloques$ambiental +
      dist_bloques$social
  ) / 2
)

# Validar reproduccion exacta de la red B S0
error_reconstruccion <- max(abs(
  distancias$completa - referencia
))

stopifnot(error_reconstruccion < 1e-10)

# 4. Funcion de red para umbral seleccionado

crear_red <- function(d, percentil) {
  
  s <- 1 - d
  limite <- as.numeric(quantile(
    s[upper.tri(s)],
    probs = percentil,
    names = FALSE
  ))
  
  indices <- which(
    upper.tri(s) & s >= limite,
    arr.ind = TRUE
  )
  
  aristas <- data.frame(
    from = rownames(s)[indices[, 1]],
    to = colnames(s)[indices[, 2]],
    similitud = s[indices]
  )
  
  g <- igraph::graph_from_data_frame(
    aristas,
    directed = FALSE,
    vertices = data.frame(name = comunas)
  )
  
  set.seed(1234)
  
  comunidades <- igraph::cluster_louvain(
    g,
    weights = igraph::E(g)$similitud
  )
  
  list(
    grafo = g,
    aristas = aristas,
    resumen = data.frame(
      percentil = percentil,
      umbral = limite,
      nodos = igraph::vcount(g),
      enlaces = igraph::ecount(g),
      componentes = igraph::components(g)$no,
      aislados = sum(igraph::degree(g) == 0),
      comunidades = length(comunidades),
      modularidad = igraph::modularity(comunidades)
    )
  )
}

# 5. Evaluar cuatro configuraciones y tres umbrales

resultados <- list()
objetos <- list()
k <- 1L

for (nombre in names(distancias)) {
  
  for (q in c(0.55, 0.60, 0.65)) {
    
    objeto <- crear_red(distancias[[nombre]], q)
    
    fila <- objeto$resumen
    fila$configuracion <- nombre
    
    resultados[[k]] <- fila
    objetos[[paste(nombre, q, sep = "_")]] <- objeto
    
    k <- k + 1L
  }
}

tabla_redes <- do.call(rbind, resultados)
rownames(tabla_redes) <- NULL

# 6. Correspondencia descriptiva con C34
# Solo para configuraciones de bloques.
# Los umbrales NO cambian la matriz completa
# de similitudes utilizada en este calculo.

correspondencia <- do.call(rbind, lapply(
  names(distancias),
  function(nombre) {
    
    d <- distancias[[nombre]]
    s <- 1 - d
    pares <- upper.tri(s)
    
    calcular <- function(y) {
      y <- log1p(as.numeric(y))
      dy <- abs(outer(y, y, "-"))
      
      suppressWarnings(cor(
        s[pares],
        dy[pares],
        method = "spearman"
      ))
    }
    
    data.frame(
      configuracion = nombre,
      rho_C34_cruda = calcular(
        tasas$tasa_c34_100k
      ),
      rho_C34_ajustada = calcular(
        tasas$tasa_ajustada_100k
      )
    )
  }
))

# 7. Comparar conexiones con la red completa
# usando siempre el mismo percentil 60.

obtener_claves <- function(objeto) {
  
  a <- objeto$aristas
  
  sort(apply(
    a[, c("from", "to"), drop = FALSE],
    1,
    function(z) paste(sort(z), collapse = "|")
  ))
}

base_q60 <- obtener_claves(objetos[["completa_0.6"]])

comparar_enlaces <- do.call(rbind, lapply(
  names(distancias),
  function(nombre) {
    
    nuevo <- obtener_claves(
      objetos[[paste0(nombre, "_0.6")]]
    )
    
    comunes <- length(intersect(base_q60, nuevo))
    todos <- length(union(base_q60, nuevo))
    
    data.frame(
      configuracion = nombre,
      enlaces_comunes = comunes,
      eliminados = length(setdiff(base_q60, nuevo)),
      agregados = length(setdiff(nuevo, base_q60)),
      jaccard = comunes / todos
    )
  }
))

# 8. Validaciones y exportacion

stopifnot(
  nrow(tabla_redes) == 12L,
  nrow(correspondencia) == 4L,
  nrow(comparar_enlaces) == 4L,
  all(is.finite(tabla_redes$modularidad)),
  all(is.finite(correspondencia$rho_C34_cruda)),
  all(is.finite(correspondencia$rho_C34_ajustada))
)

write.csv(
  tabla_redes,
  file.path(res_dir, "04C_umbrales_bloques.csv"),
  row.names = FALSE,
  fileEncoding = "UTF-8"
)

write.csv(
  correspondencia,
  file.path(res_dir, "04C_correspondencia_bloques.csv"),
  row.names = FALSE,
  fileEncoding = "UTF-8"
)

write.csv(
  comparar_enlaces,
  file.path(res_dir, "04C_enlaces_bloques.csv"),
  row.names = FALSE,
  fileEncoding = "UTF-8"
)

saveRDS(
  list(
    distancias = distancias,
    resultados = tabla_redes,
    correspondencia = correspondencia,
    enlaces = comparar_enlaces
  ),
  file.path(datos_dir, "04C_robustez_red_B.rds")
)

# 9. Resultados de consola

cat("\n=== BLOQUE 4C - RESULTADOS ===\n")

cat("\n=== REPRODUCCION RED B S0 ===\n")
cat("Error maximo:", error_reconstruccion, "\n")

cat("\n=== UMBRALES Y BLOQUES ===\n")
print(
  tabla_redes[, c(
    "configuracion",
    "percentil",
    "enlaces",
    "componentes",
    "aislados",
    "modularidad"
  )],
  row.names = FALSE,
  digits = 4
)

cat("\n=== CAMBIOS DE ENLACES EN P60 ===\n")
print(
  comparar_enlaces,
  row.names = FALSE,
  digits = 4
)

cat("\n=== CORRESPONDENCIA DESCRIPTIVA ===\n")
print(
  correspondencia,
  row.names = FALSE,
  digits = 4
)

cat("\nBLOQUE 4C FINALIZADO\n")


# ============================================================
# BLOQUE 5A - CARACTERIZACION TERRITORIAL
# 18 comunas - Escenario S0 con demografia historica
# No modifica matrices ni redes
# ============================================================

stopifnot(exists("proyecto"), dir.exists(proyecto))

datos_dir <- file.path(proyecto, "03_datos_derivados")
res_dir <- file.path(proyecto, "04_resultados")

base <- readRDS(file.path(
  datos_dir, "03A_matriz_V2_candidata.rds"
))

redes <- readRDS(file.path(
  datos_dir, "03B_redes_demografia_historica.rds"
))

tasas <- readRDS(file.path(
  datos_dir, "01H_tasas_C34_ajustadas.rds"
))

robustez <- readRDS(file.path(
  datos_dir, "04C_robustez_red_B.rds"
))

stopifnot(requireNamespace("igraph", quietly = TRUE))

comunas <- rownames(
  redes$redes$B$distancias
)

base <- base[match(comunas, base$comuna), ]
tasas <- tasas[match(comunas, tasas$comuna), ]

stopifnot(
  nrow(base) == 18L,
  nrow(tasas) == 18L,
  identical(as.character(base$comuna), comunas),
  identical(as.character(tasas$comuna), comunas)
)

# 1. Construir tabla descriptiva

campos <- c(
  "n_declaraciones_zona",
  "tiene_latente_num",
  "pobreza_multidimensional_2017",
  "porcentaje_viviendas_hacinadas_2024",
  "porcentaje_65mas_2020",
  "porcentaje_65mas_historico",
  "aps_publicas_10k",
  "hospital_alta_complejidad_num"
)

stopifnot(all(campos %in% names(base)))

perfiles <- data.frame(
  comuna = comunas,
  base[, campos, drop = FALSE],
  tasa_c34_cruda = tasas$tasa_c34_100k,
  tasa_c34_ajustada = tasas$tasa_ajustada_100k,
  stringsAsFactors = FALSE
)

# 2. Incorporar informacion de conectividad y Louvain

g <- redes$redes$B$grafo

grados <- igraph::degree(g)

comunidades <- igraph::membership(
  redes$redes$B$comunidades
)

perfiles$grado_red_B <- as.numeric(
  grados[perfiles$comuna]
)

perfiles$comunidad_louvain <- as.integer(
  comunidades[perfiles$comuna]
)

stopifnot(
  !anyNA(perfiles),
  all(is.finite(perfiles$grado_red_B)),
  length(unique(perfiles$comuna)) == 18L
)

# 3. Distribucion de indicadores ambientales

cat("\n=== BLOQUE 5A - RESULTADOS ===\n")

cat("\n=== DISTRIBUCION AMBIENTAL ===\n")

print(table(
  declaraciones = perfiles$n_declaraciones_zona,
  latente = perfiles$tiene_latente_num
))

# 4. Mostrar perfiles individuales

cat("\n=== PERFILES DE LAS 18 COMUNAS ===\n")

print(
  perfiles[, c(
    "comuna",
    "n_declaraciones_zona",
    "tiene_latente_num",
    "pobreza_multidimensional_2017",
    "porcentaje_65mas_historico",
    "hospital_alta_complejidad_num",
    "tasa_c34_ajustada",
    "grado_red_B",
    "comunidad_louvain"
  )],
  row.names = FALSE,
  digits = 4
)

# 5. Caracterizacion por comunidad

indicadores <- c(
  "n_declaraciones_zona",
  "tiene_latente_num",
  "pobreza_multidimensional_2017",
  "porcentaje_viviendas_hacinadas_2024",
  "porcentaje_65mas_historico",
  "aps_publicas_10k",
  "hospital_alta_complejidad_num",
  "tasa_c34_cruda",
  "tasa_c34_ajustada"
)

resumen_comunidades <- aggregate(
  perfiles[, indicadores, drop = FALSE],
  by = list(
    comunidad_louvain =
      perfiles$comunidad_louvain
  ),
  FUN = mean
)

tamano <- as.data.frame(table(
  perfiles$comunidad_louvain
))

names(tamano) <- c("comunidad_louvain", "n_comunas")

tamano$comunidad_louvain <- as.integer(
  as.character(tamano$comunidad_louvain)
)

resumen_comunidades <- merge(
  tamano,
  resumen_comunidades,
  by = "comunidad_louvain"
)

cat("\n=== PROMEDIOS POR COMUNIDAD ===\n")

print(
  resumen_comunidades,
  row.names = FALSE,
  digits = 4
)

# 6. Examinar si el bloque ambiental
# separa pares de comunas con categorias distintas

d_ambiental <- as.matrix(
  robustez$distancias$completa
)

stopifnot(
  identical(rownames(d_ambiental), comunas)
)

# Matriz de distancia ambiental exclusivamente
vars_amb <- c(
  "n_declaraciones_zona",
  "tiene_latente_num"
)

x <- scale(as.matrix(
  base[, vars_amb, drop = FALSE]
))

dist_amb <- as.matrix(dist(x))
dist_amb <- dist_amb / max(dist_amb)

rownames(dist_amb) <- comunas
colnames(dist_amb) <- comunas

pares <- which(upper.tri(dist_amb), arr.ind = TRUE)

tabla_pares <- data.frame(
  comuna_1 = comunas[pares[, 1]],
  comuna_2 = comunas[pares[, 2]],
  distancia_ambiental = dist_amb[pares],
  mismas_declaraciones =
    perfiles$n_declaraciones_zona[pares[, 1]] ==
    perfiles$n_declaraciones_zona[pares[, 2]],
  misma_latencia =
    perfiles$tiene_latente_num[pares[, 1]] ==
    perfiles$tiene_latente_num[pares[, 2]]
)

tabla_pares$mismo_perfil_regulatorio <-
  tabla_pares$mismas_declaraciones &
  tabla_pares$misma_latencia

cat("\n=== DISTANCIAS AMBIENTALES ===\n")

print(aggregate(
  distancia_ambiental ~ mismo_perfil_regulatorio,
  data = tabla_pares,
  FUN = function(z) c(
    n = length(z),
    media = mean(z),
    mediana = median(z)
  )
))

# 7. Exportar resultados agregados

write.csv(
  perfiles,
  file.path(
    res_dir,
    "05A_perfiles_territoriales.csv"
  ),
  row.names = FALSE,
  fileEncoding = "UTF-8"
)

write.csv(
  resumen_comunidades,
  file.path(
    res_dir,
    "05A_resumen_comunidades.csv"
  ),
  row.names = FALSE,
  fileEncoding = "UTF-8"
)

saveRDS(
  list(
    perfiles = perfiles,
    resumen_comunidades = resumen_comunidades,
    pares_ambientales = tabla_pares
  ),
  file.path(
    datos_dir,
    "05A_perfiles_territoriales.rds"
  )
)

cat("\nBLOQUE 5A FINALIZADO\n")


# ============================================================
# BLOQUE 5B - AUDITORIA DOCUMENTAL AMBIENTAL
# 18 comunas
# No modifica matrices ni redes
# ============================================================

stopifnot(exists("proyecto"), dir.exists(proyecto))

ruta_base <- file.path(
  proyecto, "03_datos_derivados",
  "03A_matriz_V2_candidata.rds"
)

ruta_c34 <- file.path(
  proyecto, "03_datos_derivados",
  "01_tabla_C34.rds"
)

stopifnot(
  file.exists(ruta_base),
  file.exists(ruta_c34)
)

base <- readRDS(ruta_base)
c34 <- readRDS(ruta_c34)

campos <- c(
  "comuna",
  "n_declaraciones_zona",
  "tiene_latente_num",
  "estado_zona_aire",
  "zonas_declaradas",
  "decretos",
  "contaminantes_saturados",
  "contaminantes_latentes",
  "links_fuente",
  "criterio_asignacion",
  "requiere_revision",
  "observacion"
)

stopifnot(
  all(campos %in% names(base)),
  nrow(base) == 18L,
  setequal(base$comuna, c34$comuna)
)

auditoria <- base[, campos, drop = FALSE]

pos <- match(auditoria$comuna, c34$comuna)

auditoria$inicio_rpc <- c34$inicio_rpc[pos]
auditoria$fin_rpc <- c34$fin_rpc[pos]

# Ordenar columnas para facilitar lectura
auditoria <- auditoria[, c(
  "comuna",
  "inicio_rpc",
  "fin_rpc",
  setdiff(campos, "comuna")
)]

# Identificar las combinaciones regulatorias
auditoria$grupo_regulatorio <- paste0(
  "D", auditoria$n_declaraciones_zona,
  "_L", auditoria$tiene_latente_num
)

# Identificar registros que requieren revision
# sin interpretar automaticamente su contenido
revision_texto <- tolower(trimws(
  as.character(auditoria$requiere_revision)
))

auditoria$revision_marcada <- (
  revision_texto %in%
    c("true", "1", "si", "sí", "yes")
)

auditoria$revision_marcada[
  is.na(auditoria$revision_marcada)
] <- FALSE

# Exportar auditoria completa
destino <- file.path(
  proyecto,
  "04_resultados",
  "05B_auditoria_documental_ambiental.csv"
)

write.csv(
  auditoria,
  destino,
  row.names = FALSE,
  na = "",
  fileEncoding = "UTF-8"
)

# Mostrar diagnostico sin imprimir enlaces extensos
cat("\n=== BLOQUE 5B - RESULTADOS ===\n")

cat("\n=== GRUPOS REGULATORIOS ===\n")
print(table(auditoria$grupo_regulatorio))

cat("\n=== REGISTROS MARCADOS PARA REVISION ===\n")
cat(sum(auditoria$revision_marcada), "de 18\n")

cat("\n=== INVENTARIO DOCUMENTAL ===\n")

print(
  auditoria[, c(
    "comuna",
    "inicio_rpc",
    "fin_rpc",
    "grupo_regulatorio",
    "estado_zona_aire",
    "decretos",
    "requiere_revision"
  )],
  row.names = FALSE,
  right = FALSE,
  max = 18
)

cat("\n=== ARCHIVO EXPORTADO ===\n")
cat(destino, "\n")

cat("\nBLOQUE 5B FINALIZADO\n")


# ============================================================
# BLOQUE 5C - AUDITORIA DE INFRAESTRUCTURA SANITARIA
# 18 comunas - Revision de indicadores y procedencia
# No modifica redes ni matrices anteriores
# ============================================================

stopifnot(exists("proyecto"), dir.exists(proyecto))

datos_dir <- file.path(proyecto, "03_datos_derivados")
res_dir <- file.path(proyecto, "04_resultados")

base <- readRDS(file.path(
  datos_dir, "03A_matriz_V2_candidata.rds"
))

c34 <- readRDS(file.path(
  datos_dir, "01_tabla_C34.rds"
))

# 1. Identificar todos los campos sanitarios

patron <- paste(
  "establecim", "hospital", "aps",
  "urgencia", "complejidad", "cobertura_salud",
  sep = "|"
)

campos_sanitarios <- grep(
  patron, names(base),
  value = TRUE,
  ignore.case = TRUE
)

cat("\n=== CAMPOS SANITARIOS ===\n")
print(campos_sanitarios)

# 2. Extraer variables realmente usadas en redes

campos_red <- c(
  "aps_publicas_10k_log",
  "hospital_alta_complejidad_num"
)

campos_auditoria <- c(
  "comuna",
  "poblacion_2020",
  "n_establecimientos_publicos",
  "n_aps_publicos",
  "aps_publicas_10k",
  "aps_publicas_10k_log",
  "n_hospitales_alta_complejidad_publicos",
  "hospital_alta_complejidad_num",
  "estado_cobertura_salud"
)

stopifnot(
  nrow(base) == 18L,
  nrow(c34) == 18L,
  !anyDuplicated(base$comuna),
  !anyDuplicated(c34$comuna),
  setequal(base$comuna, c34$comuna),
  all(campos_red %in% names(base))
)

disponibles <- intersect(campos_auditoria, names(base))
faltantes <- setdiff(campos_auditoria, names(base))

auditoria <- base[, disponibles, drop = FALSE]

pos <- match(auditoria$comuna, c34$comuna)

auditoria$inicio_rpc <- c34$inicio_rpc[pos]
auditoria$fin_rpc <- c34$fin_rpc[pos]

auditoria <- auditoria[, c(
  "comuna",
  "inicio_rpc",
  "fin_rpc",
  setdiff(names(auditoria), c(
    "comuna", "inicio_rpc", "fin_rpc"
  ))
)]

# 3. Controles de coherencia

numericas <- intersect(c(
  "poblacion_2020",
  "n_establecimientos_publicos",
  "n_aps_publicos",
  "aps_publicas_10k",
  "aps_publicas_10k_log",
  "n_hospitales_alta_complejidad_publicos",
  "hospital_alta_complejidad_num"
), names(auditoria))

for (v in numericas) {
  auditoria[[v]] <- suppressWarnings(
    as.numeric(auditoria[[v]])
  )
}

control_variables <- data.frame(
  variable = numericas,
  comunas_con_dato = vapply(
    auditoria[numericas],
    function(z) sum(is.finite(z)),
    integer(1)
  ),
  comunas_sin_dato = vapply(
    auditoria[numericas],
    function(z) sum(!is.finite(z)),
    integer(1)
  ),
  row.names = NULL
)

# Verificar transformacion log1p del indicador APS

if (all(c("aps_publicas_10k",
          "aps_publicas_10k_log") %in%
        names(auditoria))) {
  
  diferencia_log <- abs(
    auditoria$aps_publicas_10k_log -
      log1p(auditoria$aps_publicas_10k)
  )
  
  log_consistente <- all(
    is.finite(diferencia_log) &
      diferencia_log < 1e-6
  )
  
} else {
  log_consistente <- NA
}

# Comprobar si la tasa APS corresponde
# aproximadamente a la poblacion 2020

if (all(c("n_aps_publicos",
          "poblacion_2020",
          "aps_publicas_10k") %in%
        names(auditoria))) {
  
  tasa_reconstruida <- (
    10000 * auditoria$n_aps_publicos /
      auditoria$poblacion_2020
  )
  
  auditoria$diferencia_aps_10k <- (
    auditoria$aps_publicas_10k -
      tasa_reconstruida
  )
  
} else {
  auditoria$diferencia_aps_10k <- NA_real_
}

# 4. Registrar limitaciones temporales
# NO suponer anio de fuente sin documentacion

auditoria$anio_fuente_salud_verificado <- NA_integer_

auditoria$estado_temporal <- paste(
  "Pendiente verificar fecha de corte",
  "y disponibilidad durante ventana RPC"
)

auditoria$interpretacion_permitida <-
  "Infraestructura sanitaria de referencia"

# 5. Inventario orientativo de archivos fuente
# No equivale a verificar su contenido

fuentes_dir <- file.path(
  proyecto, "01_fuentes_y_diccionario"
)

archivos <- if (dir.exists(fuentes_dir)) {
  list.files(
    fuentes_dir,
    recursive = TRUE,
    full.names = FALSE
  )
} else {
  character(0)
}

patron_fuentes <- paste(
  "deis", "hospital", "aps",
  "establecimiento", "salud", "rem",
  sep = "|"
)

archivos_salud <- archivos[
  grepl(
    patron_fuentes,
    archivos,
    ignore.case = TRUE
  )
]

inventario_fuentes <- data.frame(
  archivo_relativo = archivos_salud
)

# 6. Exportar tablas documentales

archivo_csv <- file.path(
  res_dir, "05C_auditoria_sanitaria.csv"
)

archivo_control <- file.path(
  res_dir, "05C_control_variables_sanitarias.csv"
)

archivo_fuentes <- file.path(
  res_dir, "05C_inventario_fuentes_sanitarias.csv"
)

write.csv(
  auditoria,
  archivo_csv,
  row.names = FALSE,
  na = "",
  fileEncoding = "UTF-8"
)

write.csv(
  control_variables,
  archivo_control,
  row.names = FALSE,
  fileEncoding = "UTF-8"
)

write.csv(
  inventario_fuentes,
  archivo_fuentes,
  row.names = FALSE,
  fileEncoding = "UTF-8"
)

# 7. Informe final

cat("\n=== BLOQUE 5C - RESULTADOS ===\n")

cat("\n=== COBERTURA DE VARIABLES ===\n")
print(control_variables, row.names = FALSE)

cat("\n=== CAMPOS NO ENCONTRADOS ===\n")
print(faltantes)

cat("\n=== INDICADORES DE LA RED ===\n")
cat(
  "Transformacion APS log1p consistente:",
  log_consistente, "\n"
)

if ("hospital_alta_complejidad_num" %in%
    names(auditoria)) {
  
  cat("\nPresencia de alta complejidad:\n")
  
  print(table(
    auditoria$hospital_alta_complejidad_num,
    useNA = "ifany"
  ))
}

cat("\n=== PERFIL SANITARIO POR COMUNA ===\n")

columnas_mostrar <- intersect(c(
  "comuna",
  "inicio_rpc",
  "fin_rpc",
  "n_aps_publicos",
  "aps_publicas_10k",
  "hospital_alta_complejidad_num",
  "diferencia_aps_10k"
), names(auditoria))

print(
  auditoria[, columnas_mostrar, drop = FALSE],
  row.names = FALSE,
  digits = 5
)

cat("\n=== FUENTES IDENTIFICADAS POR NOMBRE ===\n")
cat("Archivos candidatos:", length(archivos_salud), "\n")
print(utils::head(archivos_salud, 20))

cat("\n=== ESTADO DOCUMENTAL ===\n")
cat(
  "Fecha historica de infraestructura:",
  "NO VERIFICADA\n"
)

cat("\nArchivos exportados:\n")
cat(archivo_csv, "\n")
cat(archivo_control, "\n")
cat(archivo_fuentes, "\n")

cat("\nBLOQUE 5C FINALIZADO\n")


# ============================================================
# BLOQUE 5D - CONTROL FINAL DE PROCEDENCIA SANITARIA
# No modifica ningun dato ni red
# ============================================================

stopifnot(exists("proyecto"), dir.exists(proyecto))

base <- readRDS(file.path(
  proyecto,
  "03_datos_derivados",
  "03A_matriz_V2_candidata.rds"
))

cat("\n=== BLOQUE 5D - RESULTADOS ===\n")

# 1. Revisar los indicadores de Tocopilla

columnas <- grep(
  "aps|hospital|establecim|urgencia|complejidad|salud",
  names(base),
  value = TRUE,
  ignore.case = TRUE
)

tocopilla <- base[
  base$comuna == "Tocopilla",
  c("comuna", columnas),
  drop = FALSE
]

stopifnot(nrow(tocopilla) == 1L)

cat("\n=== TOCOPILLA ===\n")
for (v in names(tocopilla)) {
  cat(
    v, ":",
    as.character(tocopilla[[v]][1]),
    "\n"
  )
}

# 2. Buscar fuentes en carpeta original
# No leer ni modificar archivos.

raiz <- dirname(proyecto)

archivos <- list.files(
  raiz,
  recursive = TRUE,
  full.names = TRUE,
  all.files = FALSE
)

archivos <- archivos[file.exists(archivos)]

patron <- paste(
  "deis", "hospital", "establec",
  "aps", "salud", "rem",
  "minsal", "complejidad",
  "dotacion", "atencion",
  sep = "|"
)

candidatos <- archivos[
  grepl(patron, basename(archivos),
        ignore.case = TRUE)
]

cat("\n=== ARCHIVOS CANDIDATOS ===\n")
cat("Total:", length(candidatos), "\n")

if (length(candidatos) > 0L) {
  print(
    utils::head(candidatos, 40),
    quote = FALSE
  )
} else {
  cat("Sin coincidencias por nombre.\n")
}

# 3. Guardar inventario sin alterar datos

write.csv(
  data.frame(
    archivo = candidatos
  ),
  file.path(
    proyecto,
    "04_resultados",
    "05D_fuentes_sanitarias_candidatas.csv"
  ),
  row.names = FALSE,
  fileEncoding = "UTF-8"
)

cat("\nBLOQUE 5D FINALIZADO\n")

