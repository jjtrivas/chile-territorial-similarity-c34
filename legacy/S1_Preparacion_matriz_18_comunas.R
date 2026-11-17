# ============================================================
# S1_PREPARACION_MATRIZ_18_COMUNAS.R
# Preparación de la matriz analítica para la red de similitud territorial
#
# Este script integra: RPC + condiciones ambientales-regulatorias,
# pobreza comunal, población y población de 65 años o más,
# establecimientos de salud, alta complejidad hospitalaria y hacinamiento.
#
# Producto final:
# matriz_18_comunas_RPC_zonas_pobreza_poblacion_salud_hacinamiento.xlsx
#
# Nota: la exploración de PM2.5/SINCA fue excluida de este archivo porque
# no formó parte de la red final reportada en el manuscrito.
# ============================================================

# ============================================================
# EXTRAER DATOS DE POBREZA COMUNAL
# 2009, 2011, 2013, 2015, 2017, 2020
# ============================================================

library(tidyverse)
library(readxl)
library(openxlsx)

# Carpeta donde tienes los archivos
ruta <- "C:/Users/jjtri/OneDrive - Universidad Mayor/Documentos/redes"

# ------------------------------------------------------------
# Funciones auxiliares
# ------------------------------------------------------------

normalizar_codigo <- function(x) {
  x <- as.character(x)
  x <- stringr::str_replace_all(x, "\\.0$", "")
  x <- stringr::str_trim(x)
  stringr::str_pad(x, width = 5, side = "left", pad = "0")
}

convertir_numero <- function(x) {
  x <- as.character(x)
  x <- stringr::str_replace_all(x, ",", ".")
  x <- stringr::str_trim(x)
  x[x %in% c("-", "", "NA", "N/A")] <- NA
  as.numeric(x)
}

leer_pobreza_simple <- function(archivo, hoja, nombre_variable, col_porcentaje, skip_filas = 2) {
  
  df <- read_excel(
    archivo,
    sheet = hoja,
    skip = skip_filas,
    .name_repair = "minimal"
  )
  
  salida <- tibble(
    codigo_comuna = normalizar_codigo(df[[1]]),
    nombre_comuna_pobreza = as.character(df[[3]])
  )
  
  salida[[nombre_variable]] <- convertir_numero(df[[col_porcentaje]])
  
  salida %>%
    filter(!is.na(codigo_comuna))
}

leer_pobreza_2009_2011 <- function(archivo) {
  
  df <- read_excel(
    archivo,
    sheet = "Hoja1",
    skip = 4,
    col_names = FALSE,
    .name_repair = "minimal"
  )
  
  tibble(
    codigo_comuna = normalizar_codigo(df[[1]]),
    nombre_comuna_pobreza = as.character(df[[2]]),
    pobreza_ingresos_2009 = convertir_numero(df[[4]]),
    pobreza_ingresos_2011 = convertir_numero(df[[7]]),
    cambio_pobreza_2011_2009 = convertir_numero(df[[9]])
  ) %>%
    filter(!is.na(codigo_comuna))
}

# ------------------------------------------------------------
# Rutas de archivos
# ------------------------------------------------------------

archivo_2009_2011 <- file.path(
  ruta,
  "PobrezaporComunas_SAE_20092011.xlsx"
)

archivo_2013 <- file.path(
  ruta,
  "PLANILLA_Estimaciones_comunales_tasa_pobreza_por_ingresos_2013.xlsx"
)

archivo_2015 <- file.path(
  ruta,
  "PLANILLA_Estimaciones_comunales_tasa_pobreza_por_ingresos_multidimensional_2015.xlsx"
)

archivo_2017 <- file.path(
  ruta,
  "PLANILLA_Estimaciones_comunales_tasa_pobreza_por_ingresos_multidimensional_2017.xlsx"
)

archivo_2020 <- file.path(
  ruta,
  "Estimaciones_de_Tasa_de_Pobreza_por_Ingresos_por_Comunas_2020_revisada2022_09.xlsx"
)

# ------------------------------------------------------------
# Leer cada fuente
# ------------------------------------------------------------

pob_2009_2011 <- leer_pobreza_2009_2011(
  archivo_2009_2011
)

pob_2013 <- leer_pobreza_simple(
  archivo = archivo_2013,
  hoja = "Pobreza Ingresos 2013",
  nombre_variable = "pobreza_ingresos_2013",
  col_porcentaje = 5
)

pob_2015_ingresos <- leer_pobreza_simple(
  archivo = archivo_2015,
  hoja = "Ingresos2015",
  nombre_variable = "pobreza_ingresos_2015",
  col_porcentaje = 5
)

pob_2015_multi <- leer_pobreza_simple(
  archivo = archivo_2015,
  hoja = "Multidimensional2015",
  nombre_variable = "pobreza_multidimensional_2015",
  col_porcentaje = 5
)

pob_2017_ingresos <- leer_pobreza_simple(
  archivo = archivo_2017,
  hoja = "Ingresos 2017",
  nombre_variable = "pobreza_ingresos_2017",
  col_porcentaje = 5
)

pob_2017_multi <- leer_pobreza_simple(
  archivo = archivo_2017,
  hoja = "Multidimensional 2017",
  nombre_variable = "pobreza_multidimensional_2017",
  col_porcentaje = 5
)

pob_2020 <- leer_pobreza_simple(
  archivo = archivo_2020,
  hoja = "Cifras 2020 revisadas en 2022",
  nombre_variable = "pobreza_ingresos_2020",
  col_porcentaje = 6
)

# ------------------------------------------------------------
# Unir todas las bases de pobreza
# ------------------------------------------------------------

bases_pobreza <- list(
  pob_2009_2011,
  pob_2013,
  pob_2015_ingresos,
  pob_2015_multi,
  pob_2017_ingresos,
  pob_2017_multi,
  pob_2020
)

pobreza_integrada <- bases_pobreza %>%
  purrr::map(~ select(.x, -nombre_comuna_pobreza)) %>%
  purrr::reduce(full_join, by = "codigo_comuna")

# Agregar nombre de comuna desde la primera base disponible
nombres_comunas <- bases_pobreza %>%
  bind_rows() %>%
  select(codigo_comuna, nombre_comuna_pobreza) %>%
  filter(!is.na(nombre_comuna_pobreza)) %>%
  distinct(codigo_comuna, .keep_all = TRUE)

pobreza_integrada <- pobreza_integrada %>%
  left_join(nombres_comunas, by = "codigo_comuna") %>%
  relocate(nombre_comuna_pobreza, .after = codigo_comuna)

# ------------------------------------------------------------
# Crear indicadores sociales resumidos
# ------------------------------------------------------------

cols_pobreza_ingresos <- c(
  "pobreza_ingresos_2009",
  "pobreza_ingresos_2011",
  "pobreza_ingresos_2013",
  "pobreza_ingresos_2015",
  "pobreza_ingresos_2017",
  "pobreza_ingresos_2020"
)

mat_pobreza <- pobreza_integrada %>%
  select(all_of(cols_pobreza_ingresos))

pobreza_integrada$pobreza_promedio_periodo <- rowMeans(
  mat_pobreza,
  na.rm = TRUE
)

pobreza_integrada$pobreza_promedio_periodo[
  is.nan(pobreza_integrada$pobreza_promedio_periodo)
] <- NA

pobreza_integrada$n_anios_pobreza_disponibles <- rowSums(
  !is.na(mat_pobreza)
)

pobreza_integrada <- pobreza_integrada %>%
  mutate(
    cambio_pobreza_2011_2020 = pobreza_ingresos_2020 - pobreza_ingresos_2011,
    cambio_pobreza_2013_2020 = pobreza_ingresos_2020 - pobreza_ingresos_2013
  )

View(pobreza_integrada)

# ------------------------------------------------------------
# Guardar matriz social
# ------------------------------------------------------------

archivo_salida <- file.path(
  ruta,
  "matriz_pobreza_comunal_integrada.xlsx"
)

write.xlsx(
  pobreza_integrada,
  archivo_salida,
  overwrite = TRUE
)

cat("Matriz de pobreza guardada en:\n")
cat(normalizePath(archivo_salida), "\n")

# ============================================================
# UNIR RPC + ZONAS CON MATRIZ DE POBREZA
# Versión usando CSV para evitar error de Excel/OneDrive
# ============================================================

archivo_rpc_csv <- file.path(
  ruta,
  "RPC_zonas_validado.csv"
)

if (!file.exists(archivo_rpc_csv)) {
  stop("No se encuentra RPC_zonas_validado.csv. Abre el Excel y guárdalo como CSV UTF-8 en la carpeta redes.")
}

# Intento 1: leer como CSV separado por coma
rpc_zonas <- readr::read_csv(
  archivo_rpc_csv,
  show_col_types = FALSE,
  locale = locale(decimal_mark = ",")
)

# Si quedó una sola columna, probablemente está separado por punto y coma
if (ncol(rpc_zonas) == 1) {
  rpc_zonas <- readr::read_delim(
    archivo_rpc_csv,
    delim = ";",
    show_col_types = FALSE,
    locale = locale(decimal_mark = ",")
  )
}

# Normalizar código comuna
rpc_zonas <- rpc_zonas %>%
  mutate(
    codigo_comuna = normalizar_codigo(codigo_comuna)
  )

cat("RPC + zonas cargado correctamente.\n")
cat("Filas:", nrow(rpc_zonas), "-- Columnas:", ncol(rpc_zonas), "\n")

View(rpc_zonas)


# ============================================================
# UNIR CON LA MATRIZ DE POBREZA YA CREADA
# ============================================================

datos_18_integrados <- rpc_zonas %>%
  left_join(
    pobreza_integrada,
    by = "codigo_comuna"
  )

View(datos_18_integrados)


# Revisar cuántas comunas quedaron con pobreza
cat("Resumen de cruce con pobreza:\n")

datos_18_integrados %>%
  transmute(
    nombre_comuna_rpc,
    codigo_comuna,
    pobreza_ingresos_2020,
    pobreza_multidimensional_2017,
    pobreza_promedio_periodo
  ) %>%
  print(n = Inf)


# ============================================================
# GUARDAR MATRIZ INTEGRADA
# ============================================================

archivo_integrado <- file.path(
  ruta,
  "matriz_18_comunas_RPC_zonas_pobreza.xlsx"
)

write.xlsx(
  datos_18_integrados,
  archivo_integrado,
  overwrite = TRUE
)

cat("Matriz RPC + zonas + pobreza guardada en:\n")
cat(normalizePath(archivo_integrado), "\n")

# ============================================================
# LIMPIAR COLUMNAS VACÍAS Y CREAR ESTADO DE COBERTURA SOCIAL
# ============================================================

datos_18_integrados <- datos_18_integrados %>%
  select(-matches("^\\.\\.\\.")) %>%
  mutate(
    estado_cobertura_social = case_when(
      !is.na(pobreza_ingresos_2020) &
        !is.na(pobreza_multidimensional_2017) &
        !is.na(pobreza_promedio_periodo) ~ "Con datos sociales",
      TRUE ~ "Datos sociales incompletos"
    )
  )

# Revisar resultado
datos_18_integrados %>%
  select(
    nombre_comuna_rpc,
    codigo_comuna,
    pobreza_ingresos_2020,
    pobreza_multidimensional_2017,
    pobreza_promedio_periodo,
    cambio_pobreza_2011_2020,
    estado_cobertura_social
  ) %>%
  print(n = Inf)

# Guardar nuevamente matriz limpia
archivo_integrado_limpio <- file.path(
  ruta,
  "matriz_18_comunas_RPC_zonas_pobreza_limpia.xlsx"
)

write.xlsx(
  datos_18_integrados,
  archivo_integrado_limpio,
  overwrite = TRUE
)

cat("Matriz integrada limpia guardada en:\n")
cat(normalizePath(archivo_integrado_limpio), "\n")

# ============================================================
# INCORPORAR POBLACIÓN 2020 + 65 AÑOS O MÁS
# E INCORPORAR ESTABLECIMIENTOS DE SALUD POR COMUNA
# ============================================================

library(tidyverse)
library(readxl)
library(openxlsx)

# ------------------------------------------------------------
# 1. FUNCIONES AUXILIARES
# ------------------------------------------------------------

normalizar_texto <- function(x) {
  x <- as.character(x)
  x <- stringi::stri_trans_general(x, "Latin-ASCII")
  x <- stringr::str_to_upper(x)
  x <- stringr::str_squish(x)
  x
}

numero_seguro <- function(x) {
  suppressWarnings(as.numeric(as.character(x)))
}

# ------------------------------------------------------------
# 2. EXTRAER POBLACIÓN 2020 DESDE LAS 15 PLANILLAS INE
# ------------------------------------------------------------

extraer_t1_poblacion <- function(tabla) {
  
  inicios <- which(normalizar_texto(tabla[[1]]) == "INICIO")
  
  purrr::map_dfr(seq_along(inicios), function(i) {
    
    inicio <- inicios[i]
    fin <- if (i < length(inicios)) inicios[i + 1] - 1 else nrow(tabla)
    
    bloque <- tabla[inicio:fin, , drop = FALSE]
    
    posibles_nombres <- bloque[[1]][1:min(5, nrow(bloque))]
    
    comuna <- posibles_nombres[
      which(
        !is.na(posibles_nombres) &
          normalizar_texto(posibles_nombres) != "INICIO"
      )[1]
    ]
    
    # En T1, 2020 está en la columna 5 y la población total en la 6
    posicion_2020 <- which(numero_seguro(bloque[[5]]) == 2020)
    
    poblacion_2020 <- if (length(posicion_2020) > 0) {
      numero_seguro(bloque[[6]][posicion_2020[1]])
    } else {
      NA_real_
    }
    
    tibble(
      comuna_poblacion = as.character(comuna),
      comuna_norm = normalizar_texto(comuna),
      poblacion_2020 = poblacion_2020
    )
  })
}


extraer_t2_65mas <- function(tabla) {
  
  inicios <- which(normalizar_texto(tabla[[1]]) == "INICIO")
  
  purrr::map_dfr(seq_along(inicios), function(i) {
    
    inicio <- inicios[i]
    fin <- if (i < length(inicios)) inicios[i + 1] - 1 else nrow(tabla)
    
    bloque <- tabla[inicio:fin, , drop = FALSE]
    
    posibles_nombres <- bloque[[1]][1:min(5, nrow(bloque))]
    
    comuna <- posibles_nombres[
      which(
        !is.na(posibles_nombres) &
          normalizar_texto(posibles_nombres) != "INICIO"
      )[1]
    ]
    
    etiquetas <- normalizar_texto(bloque[[1]])
    
    # Mantener solo la sección de población total,
    # antes de que comiencen los hombres.
    posicion_hombres <- which(etiquetas == "HOMBRES")[1]
    
    bloque_total <- if (!is.na(posicion_hombres)) {
      bloque[1:(posicion_hombres - 1), , drop = FALSE]
    } else {
      bloque
    }
    
    etiquetas_total <- normalizar_texto(bloque_total[[1]])
    
    posicion_total <- which(etiquetas_total == "TOTAL")[1]
    
    poblacion_total_2020_t2 <- if (!is.na(posicion_total)) {
      numero_seguro(bloque_total[[8]][posicion_total])
    } else {
      NA_real_
    }
    
    posiciones_65mas <- which(
      etiquetas_total %in% c(
        "65-69",
        "70-74",
        "75-79",
        "80+",
        "80 Y MAS"
      )
    )
    
    poblacion_65mas_2020 <- if (length(posiciones_65mas) > 0) {
      sum(
        numero_seguro(bloque_total[[8]][posiciones_65mas]),
        na.rm = TRUE
      )
    } else {
      NA_real_
    }
    
    tibble(
      comuna_poblacion = as.character(comuna),
      comuna_norm = normalizar_texto(comuna),
      poblacion_total_2020_t2 = poblacion_total_2020_t2,
      poblacion_65mas_2020 = poblacion_65mas_2020
    )
  })
}


extraer_poblacion_archivo <- function(archivo) {
  
  tabla_t1 <- read_excel(
    archivo,
    sheet = "T1",
    col_names = FALSE
  )
  
  tabla_t2 <- read_excel(
    archivo,
    sheet = "T2",
    col_names = FALSE
  )
  
  poblacion_t1 <- extraer_t1_poblacion(tabla_t1)
  poblacion_t2 <- extraer_t2_65mas(tabla_t2)
  
  full_join(
    poblacion_t1,
    poblacion_t2,
    by = "comuna_norm",
    suffix = c("_t1", "_t2")
  ) %>%
    transmute(
      comuna_poblacion = coalesce(
        comuna_poblacion_t1,
        comuna_poblacion_t2
      ),
      comuna_norm,
      poblacion_2020 = coalesce(
        poblacion_2020,
        poblacion_total_2020_t2
      ),
      poblacion_65mas_2020
    )
}


# ------------------------------------------------------------
# 3. LEER TODAS LAS PLANILLAS DE POBLACIÓN
# ------------------------------------------------------------

archivos_poblacion <- list.files(
  ruta,
  pattern = "^SalComUsuarios-[0-9]{2}Tok\\.xls$",
  full.names = TRUE
)

if (length(archivos_poblacion) < 15) {
  stop(
    "No se encontraron los 15 archivos SalComUsuarios-XXTok.xls. ",
    "Revisa que estén guardados en la carpeta redes."
  )
}

poblacion_comunal <- purrr::map_dfr(
  archivos_poblacion,
  extraer_poblacion_archivo
) %>%
  distinct(comuna_norm, .keep_all = TRUE) %>%
  mutate(
    porcentaje_65mas_2020 = if_else(
      poblacion_2020 > 0,
      poblacion_65mas_2020 / poblacion_2020 * 100,
      NA_real_
    )
  )

View(poblacion_comunal)

archivo_poblacion <- file.path(
  ruta,
  "poblacion_comunal_2020_65mas.xlsx"
)

write.xlsx(
  poblacion_comunal,
  archivo_poblacion,
  overwrite = TRUE
)

cat("Población comunal extraída correctamente.\n")
cat(normalizePath(archivo_poblacion), "\n")


# ------------------------------------------------------------
# 4. LEER Y RESUMIR ESTABLECIMIENTOS DE SALUD
# ------------------------------------------------------------

archivo_salud <- file.path(
  ruta,
  "establecimientos_20260616.csv"
)

if (!file.exists(archivo_salud)) {
  stop(
    "No se encuentra establecimientos_20260616.csv ",
    "en la carpeta redes."
  )
}

establecimientos <- readr::read_delim(
  archivo_salud,
  delim = ";",
  col_types = readr::cols(.default = readr::col_character()),
  show_col_types = FALSE
)

tipos_aps <- c(
  "CENTRO DE SALUD FAMILIAR (CESFAM)",
  "CENTRO COMUNITARIO DE SALUD FAMILIAR (CECOSF)",
  "POSTA DE SALUD RURAL (PSR)",
  "CONSULTORIO GENERAL RURAL (CGR)",
  "CONSULTORIO GENERAL URBANO (CGU)",
  "SERVICIO DE ATENCION PRIMARIA DE URGENCIA (SAPU)",
  "SERVICIO DE URGENCIA RURAL (SUR)",
  "SERVICIO DE ATENCION PRIMARIA DE URGENCIA DE ALTA RESOLUTIVIDAD (SAR)"
)

salud_comunal <- establecimientos %>%
  mutate(
    codigo_comuna = normalizar_codigo(ComunaCodigo),
    
    vigente = str_detect(
      normalizar_texto(EstadoFuncionamiento),
      "^VIGENTE"
    ),
    
    es_publico = normalizar_texto(TipoSistemaSaludGlosa) == "PUBLICO",
    
    es_hospital = normalizar_texto(TipoEstablecimientoGlosa) == "HOSPITAL",
    
    es_aps = normalizar_texto(TipoEstablecimientoGlosa) %in% tipos_aps,
    
    tiene_urgencia = normalizar_texto(TieneServicioUrgencia) == "SI",
    
    es_urgencia_hospitalaria =
      normalizar_texto(TipoUrgencia) == "URGENCIA HOSPITALARIA (UEH)",
    
    es_alta_complejidad =
      normalizar_texto(NivelComplejidadEstabGlosa) == "ALTA COMPLEJIDAD"
  ) %>%
  filter(
    vigente,
    normalizar_texto(AmbitoFuncionamiento) == "ESTABLECIMIENTO DE SALUD"
  ) %>%
  group_by(codigo_comuna) %>%
  summarise(
    n_establecimientos_total = n(),
    
    n_establecimientos_publicos = sum(es_publico, na.rm = TRUE),
    
    n_hospitales_publicos = sum(
      es_hospital & es_publico,
      na.rm = TRUE
    ),
    
    n_aps_publicos = sum(
      es_aps & es_publico,
      na.rm = TRUE
    ),
    
    n_urgencias_publicas = sum(
      tiene_urgencia & es_publico,
      na.rm = TRUE
    ),
    
    n_urgencias_hospitalarias = sum(
      es_urgencia_hospitalaria & es_publico,
      na.rm = TRUE
    ),
    
    n_alta_complejidad_publica = sum(
      es_alta_complejidad & es_publico,
      na.rm = TRUE
    ),
    
    .groups = "drop"
  ) %>%
  mutate(
    hospital_publico_presente = as.integer(
      n_hospitales_publicos > 0
    ),
    
    urgencia_hospitalaria_presente = as.integer(
      n_urgencias_hospitalarias > 0
    ),
    
    alta_complejidad_publica_presente = as.integer(
      n_alta_complejidad_publica > 0
    )
  )

View(salud_comunal)


# ------------------------------------------------------------
# 5. UNIR POBLACIÓN + SALUD A TU MATRIZ ACTUAL
# ------------------------------------------------------------

conteos_salud <- c(
  "n_establecimientos_total",
  "n_establecimientos_publicos",
  "n_hospitales_publicos",
  "n_aps_publicos",
  "n_urgencias_publicas",
  "n_urgencias_hospitalarias",
  "n_alta_complejidad_publica",
  "hospital_publico_presente",
  "urgencia_hospitalaria_presente",
  "alta_complejidad_publica_presente"
)

matriz_18_v2 <- datos_18_integrados %>%
  mutate(
    codigo_comuna = normalizar_codigo(codigo_comuna),
    comuna_norm = normalizar_texto(nombre_comuna_rpc)
  ) %>%
  left_join(
    poblacion_comunal %>%
      select(
        comuna_norm,
        poblacion_2020,
        poblacion_65mas_2020,
        porcentaje_65mas_2020
      ),
    by = "comuna_norm"
  ) %>%
  left_join(
    salud_comunal,
    by = "codigo_comuna"
  ) %>%
  mutate(
    across(
      all_of(conteos_salud),
      ~ replace_na(as.numeric(.), 0)
    ),
    
    # Indicadores comparables por población
    tasa_rpc_anual_aprox_100k = if_else(
      poblacion_2020 > 0,
      promedio_casos_anuales / poblacion_2020 * 100000,
      NA_real_
    ),
    
    proporcion_c34 = if_else(
      total_casos_C32_C33_C34 > 0,
      casos_C34_pulmon / total_casos_C32_C33_C34,
      NA_real_
    ),
    
    establecimientos_publicos_10k = if_else(
      poblacion_2020 > 0,
      n_establecimientos_publicos / poblacion_2020 * 10000,
      NA_real_
    ),
    
    aps_publicas_10k = if_else(
      poblacion_2020 > 0,
      n_aps_publicos / poblacion_2020 * 10000,
      NA_real_
    ),
    
    urgencias_publicas_100k = if_else(
      poblacion_2020 > 0,
      n_urgencias_publicas / poblacion_2020 * 100000,
      NA_real_
    ),
    
    estado_cobertura_demografica = if_else(
      !is.na(poblacion_2020),
      "Con población 2020",
      "Sin población 2020"
    ),
    
    estado_cobertura_salud = if_else(
      n_establecimientos_publicos > 0,
      "Con establecimientos públicos",
      "Sin establecimientos públicos registrados"
    )
  )

# ------------------------------------------------------------
# 6. REVISAR LAS 18 COMUNAS
# ------------------------------------------------------------

matriz_18_v2 %>%
  select(
    nombre_comuna_rpc,
    codigo_comuna,
    poblacion_2020,
    porcentaje_65mas_2020,
    promedio_casos_anuales,
    tasa_rpc_anual_aprox_100k,
    n_establecimientos_publicos,
    establecimientos_publicos_10k,
    n_hospitales_publicos,
    n_aps_publicos,
    aps_publicas_10k,
    n_urgencias_publicas,
    alta_complejidad_publica_presente
  ) %>%
  print(n = Inf)

View(matriz_18_v2)


# ------------------------------------------------------------
# 7. GUARDAR MATRIZ MEJORADA
# ------------------------------------------------------------

archivo_matriz_v2 <- file.path(
  ruta,
  "matriz_18_comunas_RPC_zonas_pobreza_poblacion_salud.xlsx"
)

write.xlsx(
  matriz_18_v2,
  archivo_matriz_v2,
  overwrite = TRUE
)

cat("\nMATRIZ MEJORADA GUARDADA EN:\n")
cat(normalizePath(archivo_matriz_v2), "\n")
# ============================================================
# AUDITORÍA DE POBLACIÓN Y ESTABLECIMIENTOS DE SALUD
# ============================================================

# Resumen general de cobertura
matriz_18_v2 %>%
  summarise(
    n_comunas = n(),
    sin_poblacion = sum(is.na(poblacion_2020)),
    sin_porcentaje_65mas = sum(is.na(porcentaje_65mas_2020)),
    poblacion_65mas_invalida = sum(
      poblacion_65mas_2020 > poblacion_2020,
      na.rm = TRUE
    ),
    comunas_sin_establecimientos_publicos = sum(
      n_establecimientos_publicos == 0,
      na.rm = TRUE
    ),
    comunas_sin_hospital_publico = sum(
      n_hospitales_publicos == 0,
      na.rm = TRUE
    ),
    comunas_sin_aps_publica = sum(
      n_aps_publicos == 0,
      na.rm = TRUE
    )
  ) %>%
  print()

# Ver los resultados de salud para las 18 comunas
matriz_18_v2 %>%
  select(
    nombre_comuna_rpc,
    poblacion_2020,
    porcentaje_65mas_2020,
    n_establecimientos_publicos,
    n_hospitales_publicos,
    n_aps_publicos,
    n_urgencias_publicas,
    n_urgencias_hospitalarias,
    n_alta_complejidad_publica,
    establecimientos_publicos_10k,
    aps_publicas_10k,
    urgencias_publicas_100k
  ) %>%
  arrange(nombre_comuna_rpc) %>%
  print(n = Inf)
# ============================================================
# REFINAR INDICADORES DE ALTA COMPLEJIDAD EN SALUD
# ============================================================

salud_alta_refinada <- establecimientos %>%
  mutate(
    codigo_comuna = normalizar_codigo(ComunaCodigo),
    
    vigente = str_detect(
      normalizar_texto(EstadoFuncionamiento),
      "^VIGENTE"
    ),
    
    es_publico = normalizar_texto(TipoSistemaSaludGlosa) == "PUBLICO",
    
    es_hospital = normalizar_texto(TipoEstablecimientoGlosa) == "HOSPITAL",
    
    es_alta_complejidad =
      normalizar_texto(NivelComplejidadEstabGlosa) == "ALTA COMPLEJIDAD"
  ) %>%
  filter(
    vigente,
    es_publico,
    es_alta_complejidad
  ) %>%
  group_by(codigo_comuna) %>%
  summarise(
    n_hospitales_alta_complejidad_publicos = sum(
      es_hospital,
      na.rm = TRUE
    ),
    
    n_centros_ambulatorios_alta_complejidad_publicos = sum(
      !es_hospital,
      na.rm = TRUE
    ),
    
    hospital_alta_complejidad_presente = as.integer(
      n_hospitales_alta_complejidad_publicos > 0
    ),
    
    .groups = "drop"
  )

# Quitar las versiones antiguas si ya existen
matriz_18_v3 <- matriz_18_v2 %>%
  select(
    -any_of(c(
      "n_hospitales_alta_complejidad_publicos",
      "n_centros_ambulatorios_alta_complejidad_publicos",
      "hospital_alta_complejidad_presente"
    ))
  ) %>%
  left_join(
    salud_alta_refinada,
    by = "codigo_comuna"
  ) %>%
  mutate(
    across(
      c(
        n_hospitales_alta_complejidad_publicos,
        n_centros_ambulatorios_alta_complejidad_publicos,
        hospital_alta_complejidad_presente
      ),
      ~ replace_na(as.numeric(.), 0)
    )
  )

# Revisar las 18 comunas
matriz_18_v3 %>%
  select(
    nombre_comuna_rpc,
    n_hospitales_publicos,
    n_hospitales_alta_complejidad_publicos,
    n_centros_ambulatorios_alta_complejidad_publicos,
    hospital_alta_complejidad_presente,
    alta_complejidad_publica_presente
  ) %>%
  arrange(nombre_comuna_rpc) %>%
  print(n = Inf)

# Guardar matriz mejorada
archivo_matriz_v3 <- file.path(
  ruta,
  "matriz_18_comunas_RPC_zonas_pobreza_poblacion_salud_refinada.xlsx"
)

write.xlsx(
  matriz_18_v3,
  archivo_matriz_v3,
  overwrite = TRUE
)

cat("Matriz refinada guardada en:\n")
cat(normalizePath(archivo_matriz_v3), "\n")

# ============================================================
# CARGAR BASE DE HACINAMIENTO COMUNAL
# ============================================================

archivo_hacinamiento <- file.path(
  ruta,
  "V3_N-de-dormitorios-hacinamiento-y-viv-con-mas-de-un-hogar.xlsx"
)

if (!file.exists(archivo_hacinamiento)) {
  stop(
    "No se encuentra el archivo de hacinamiento en la carpeta redes. ",
    "Revisa que el nombre sea exactamente ",
    "V3_N-de-dormitorios-hacinamiento-y-viv-con-mas-de-un-hogar.xlsx"
  )
}

# Hoja 4: tabla comunal de dormitorios, hacinamiento y viviendas.
# skip = 3 conserva la estructura usada en el análisis original.
hacinamiento_bruto <- readxl::read_excel(
  archivo_hacinamiento,
  sheet = "4",
  skip = 3
)

# ============================================================
# CREAR TABLA COMUNAL DE HACINAMIENTO
# ============================================================

hacinamiento_comunal <- hacinamiento_bruto %>%
  transmute(
    codigo_comuna = normalizar_codigo(hacinamiento_bruto[[5]]),
    nombre_comuna_hacinamiento = as.character(hacinamiento_bruto[[6]]),
    
    viviendas_ocupadas_2024 = as.numeric(hacinamiento_bruto[[7]]),
    
    viviendas_hacinamiento_medio_2024 = as.numeric(hacinamiento_bruto[[9]]),
    
    viviendas_hacinamiento_critico_2024 = as.numeric(hacinamiento_bruto[[10]])
  ) %>%
  filter(
    !is.na(codigo_comuna),
    codigo_comuna != "00000",
    !is.na(viviendas_ocupadas_2024),
    viviendas_ocupadas_2024 > 0
  ) %>%
  mutate(
    viviendas_hacinadas_2024 =
      viviendas_hacinamiento_medio_2024 +
      viviendas_hacinamiento_critico_2024,
    
    porcentaje_viviendas_hacinadas_2024 =
      viviendas_hacinadas_2024 /
      viviendas_ocupadas_2024 * 100
  )

View(hacinamiento_comunal)


# ============================================================
# UNIR HACINAMIENTO A LA MATRIZ ACTUAL
# ============================================================

matriz_18_v4 <- matriz_18_v3 %>%
  select(
    -any_of(c(
      "nombre_comuna_hacinamiento",
      "viviendas_ocupadas_2024",
      "viviendas_hacinamiento_medio_2024",
      "viviendas_hacinamiento_critico_2024",
      "viviendas_hacinadas_2024",
      "porcentaje_viviendas_hacinadas_2024",
      "estado_cobertura_hacinamiento"
    ))
  ) %>%
  left_join(
    hacinamiento_comunal,
    by = "codigo_comuna"
  ) %>%
  mutate(
    estado_cobertura_hacinamiento = if_else(
      !is.na(porcentaje_viviendas_hacinadas_2024),
      "Con dato Censo 2024",
      "Sin dato de hacinamiento"
    )
  )

# Revisar las 18 comunas
matriz_18_v4 %>%
  select(
    nombre_comuna_rpc,
    codigo_comuna,
    viviendas_ocupadas_2024,
    viviendas_hacinamiento_medio_2024,
    viviendas_hacinamiento_critico_2024,
    viviendas_hacinadas_2024,
    porcentaje_viviendas_hacinadas_2024,
    estado_cobertura_hacinamiento
  ) %>%
  arrange(nombre_comuna_rpc) %>%
  print(n = Inf)

# Guardar matriz actualizada
archivo_matriz_v4 <- file.path(
  ruta,
  "matriz_18_comunas_RPC_zonas_pobreza_poblacion_salud_hacinamiento.xlsx"
)

write.xlsx(
  matriz_18_v4,
  archivo_matriz_v4,
  overwrite = TRUE
)

cat("Matriz con hacinamiento guardada en:\n")
cat(normalizePath(archivo_matriz_v4), "\n")

