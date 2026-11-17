# AUDITORIA 01 - MODULARIDAD
# Red de similitud territorial: 18 comunas
# Reproduccion independiente del procedimiento S2

library(readxl)
library(igraph)

# 1. Seleccionar el Excel original
archivo <- file.choose()
datos <- as.data.frame(
  read_excel(archivo, sheet = "Matriz_red_base")
)

# 2. Variables utilizadas por S2
bloques <- list(
  oncologico = c(
    "tasa_rpc_anual_log",
    "proporcion_c34"
  ),
  ambiental_regulatorio = c(
    "n_declaraciones_zona",
    "tiene_latente_num"
  ),
  social_habitacional = c(
    "pobreza_multidimensional_2017",
    "porcentaje_viviendas_hacinadas_2024"
  ),
  demografico_acceso = c(
    "porcentaje_65mas_2020",
    "aps_publicas_10k_log",
    "hospital_alta_complejidad_num"
  )
)

variables <- unlist(bloques, use.names = FALSE)

stopifnot(
  nrow(datos) == 18,
  all(c("nombre_comuna_rpc", variables) %in% names(datos)),
  !anyNA(datos[, variables])
)

# 3. Distancias y red
distancia_bloque <- function(df, vars) {
  x <- df[, vars, drop = FALSE]
  x <- as.data.frame(lapply(x, as.numeric))
  x <- x[, vapply(x, function(v) sd(v) > 0, logical(1)),
         drop = FALSE]
  d <- as.matrix(dist(scale(x), method = "euclidean"))
  d / max(d[upper.tri(d)])
}

construir_red <- function(df) {
  distancias <- lapply(bloques, function(vars) {
    distancia_bloque(df, vars)
  })
  
  d <- Reduce("+", distancias) / length(distancias)
  s <- 1 - d
  nombres <- as.character(df$nombre_comuna_rpc)
  dimnames(s) <- list(nombres, nombres)
  
  umbral <- as.numeric(
    quantile(s[upper.tri(s)], probs = 0.60)
  )
  
  idx <- which(upper.tri(s) & s >= umbral, arr.ind = TRUE)
  
  enlaces <- data.frame(
    from = rownames(s)[idx[, 1]],
    to = colnames(s)[idx[, 2]],
    peso = s[idx]
  )
  
  red <- graph_from_data_frame(
    enlaces,
    directed = FALSE,
    vertices = data.frame(name = nombres)
  )
  
  list(red = red, umbral = umbral)
}

modularidad_louvain <- function(red) {
  comunidad <- cluster_louvain(
    red,
    weights = E(red)$peso
  )
  modularity(comunidad)
}

# 4. Ejecutar prueba nula con semilla controlada
set.seed(1234)

base <- construir_red(datos)
q_observada <- modularidad_louvain(base$red)

B <- 999
q_nula <- numeric(B)

for (i in seq_len(B)) {
  permutados <- datos
  
  for (v in variables) {
    permutados[[v]] <- sample(
      datos[[v]],
      replace = FALSE
    )
  }
  
  red_aleatoria <- construir_red(permutados)$red
  q_nula[i] <- modularidad_louvain(red_aleatoria)
}

p_valor <- (1 + sum(q_nula >= q_observada)) / (B + 1)

# 5. Resultados
resultado <- data.frame(
  semilla = 1234,
  permutaciones = B,
  comunas = vcount(base$red),
  enlaces = ecount(base$red),
  umbral = base$umbral,
  modularidad_observada = q_observada,
  modularidad_nula_media = mean(q_nula),
  modularidad_nula_sd = sd(q_nula),
  p_valor = p_valor
)

print(resultado, digits = 6)

# 6. Guardar auditoria sin sobrescribir originales
carpeta <- dirname(archivo)

write.csv(
  resultado,
  file.path(carpeta, "AUDITORIA_01_resultado.csv"),
  row.names = FALSE
)

write.csv(
  data.frame(iteracion = seq_len(B), modularidad = q_nula),
  file.path(carpeta, "AUDITORIA_01_permutaciones.csv"),
  row.names = FALSE
)

writeLines(
  capture.output(sessionInfo()),
  file.path(carpeta, "AUDITORIA_01_sesion_R.txt")
)

cat("\nAUDITORIA 01 FINALIZADA\n")


# AUDITORIA 02 - COMPARAR CLUSTERING k=2 Y k=3

library(readxl)

archivo <- file.choose()

# Selecciona nuevamente:
# Red_base_18_comunas_umbral_060.xlsx

sil <- read_excel(archivo, sheet = "Silhouette")
print(sil, n = Inf)

cat("\n=== GRUPOS k=3 ===\n")
grupos <- read_excel(archivo, sheet = "Clusters_k3")
print(grupos, n = Inf)

cat("\n=== CONVERGENCIA ===\n")
ari <- read_excel(archivo, sheet = "Convergencia_k3")
print(ari)


# AUDITORIA 02 - CORREGIR ETIQUETAS DE CLUSTERS

# Leer las comunas en el orden de la matriz original
matriz <- readxl::read_excel(
  archivo,
  sheet = "Matriz_red_base"
)

nombres <- as.character(matriz$nombre_comuna_rpc)
indices <- suppressWarnings(as.integer(grupos$comuna))

# Comprobar que los identificadores son válidos
stopifnot(
  length(nombres) == 18,
  length(indices) == 18,
  !anyNA(indices),
  setequal(indices, seq_len(18)),
  !anyNA(nombres),
  !anyDuplicated(nombres)
)

# Asociar los nombres a sus grupos
grupos_corregidos <- data.frame(
  comuna = nombres[indices],
  grupo_pam_k3 = grupos$grupo_pam_k3,
  grupo_jerarquico_k3 = grupos$grupo_jerarquico_k3
)

# Verificar concordancia
stopifnot(
  all(grupos_corregidos$grupo_pam_k3 ==
        grupos_corregidos$grupo_jerarquico_k3)
)

# Mostrar resultados
print(
  grupos_corregidos[
    order(grupos_corregidos$grupo_pam_k3,
          grupos_corregidos$comuna),
  ],
  row.names = FALSE
)

# Guardar tabla auditada sin alterar originales
write.csv(
  grupos_corregidos,
  file.path(
    dirname(archivo),
    "AUDITORIA_02_clusters_k3_corregidos.csv"
  ),
  row.names = FALSE,
  fileEncoding = "UTF-8"
)

#Maria Elena realmente esta

# AUDITORIA 02 - COMPARACION k=2 Y k=3

library(cluster)

bloques <- list(
  oncologico = c("tasa_rpc_anual_log", "proporcion_c34"),
  ambiental = c("n_declaraciones_zona", "tiene_latente_num"),
  social = c("pobreza_multidimensional_2017",
             "porcentaje_viviendas_hacinadas_2024"),
  demografico = c("porcentaje_65mas_2020",
                  "aps_publicas_10k_log",
                  "hospital_alta_complejidad_num")
)

distancias <- lapply(bloques, function(vars) {
  x <- as.data.frame(lapply(matriz[, vars], as.numeric))
  x <- x[, vapply(x, function(v) sd(v) > 0, logical(1)),
         drop = FALSE]
  d <- as.matrix(dist(scale(x)))
  d / max(d[upper.tri(d)])
})

distancia_global <- Reduce("+", distancias) / 4
dimnames(distancia_global) <- list(nombres, nombres)
d <- as.dist(distancia_global)

set.seed(1234)
pam2 <- pam(d, k = 2, diss = TRUE)
pam3 <- pam(d, k = 3, diss = TRUE)

comparacion <- data.frame(
  comuna = nombres,
  grupo_k2 = as.integer(pam2$clustering),
  grupo_k3 = as.integer(pam3$clustering)
)

print(comparacion[order(comparacion$grupo_k2,
                        comparacion$grupo_k3,
                        comparacion$comuna), ],
      row.names = FALSE)

cat("\nSilhouette PAM k=2:",
    mean(silhouette(pam2$clustering, d)[, "sil_width"]))

cat("\nSilhouette PAM k=3:",
    mean(silhouette(pam3$clustering, d)[, "sil_width"]), "\n")


# ============================================================
# AUDITORIA 03 - CORONEL Y CURICO
# Verificacion de centralidades y puntos de articulacion
# No modifica los datos originales
# ============================================================

library(igraph)

# 1. Reconstruir la red original desde la distancia global
similitud <- 1 - distancia_global

umbral <- as.numeric(
  quantile(similitud[upper.tri(similitud)],
           probs = 0.60)
)

idx <- which(
  upper.tri(similitud) & similitud >= umbral,
  arr.ind = TRUE
)

enlaces <- data.frame(
  from = rownames(similitud)[idx[, 1]],
  to = colnames(similitud)[idx[, 2]],
  peso = similitud[idx]
)

red <- graph_from_data_frame(
  enlaces,
  directed = FALSE,
  vertices = data.frame(name = nombres)
)

E(red)$distancia <- 1 / E(red)$peso

# 2. Verificar estructura de referencia
cat("\n=== RED ORIGINAL ===\n")
cat("Nodos:", vcount(red), "\n")
cat("Enlaces:", ecount(red), "\n")
cat("Componentes:", components(red)$no, "\n")
cat("Umbral:", round(umbral, 6), "\n")

stopifnot(
  vcount(red) == 18,
  ecount(red) == 61,
  components(red)$no == 3
)

# 3. Trabajar sobre el componente principal
comp <- components(red)
id_principal <- which.max(comp$csize)

nodos_principales <- V(red)$name[
  comp$membership == id_principal
]

principal <- induced_subgraph(
  red,
  vids = nodos_principales
)

stopifnot(vcount(principal) == 16)

# 4. Centralidades principales
centralidades <- data.frame(
  comuna = V(principal)$name,
  grado = degree(principal),
  fuerza = strength(
    principal,
    weights = E(principal)$peso
  ),
  intermediacion = betweenness(
    principal,
    directed = FALSE,
    weights = E(principal)$distancia,
    normalized = TRUE
  )
)

cat("\n=== CENTRALIDADES ===\n")
print(
  centralidades[
    centralidades$comuna %in% c("Coronel", "Curicó"),
  ],
  row.names = FALSE,
  digits = 6
)

# 5. Puntos de articulacion
articuladores <- as_ids(
  articulation_points(principal)
)

cat("\n=== PUNTOS DE ARTICULACION ===\n")
if (length(articuladores) == 0) {
  cat("Ninguno\n")
} else {
  print(articuladores)
}

# 6. Eliminar cada nodo por separado
evaluar_eliminacion <- function(nombre) {
  
  red_reducida <- delete_vertices(principal, nombre)
  
  data.frame(
    comuna_eliminada = nombre,
    nodos_restantes = vcount(red_reducida),
    enlaces_restantes = ecount(red_reducida),
    componentes_restantes = components(red_reducida)$no,
    componente_mayor = max(components(red_reducida)$csize)
  )
}

resultado_eliminacion <- do.call(
  rbind,
  lapply(
    c("Coronel", "Curicó"),
    evaluar_eliminacion
  )
)

cat("\n=== IMPACTO DE ELIMINACION ===\n")
print(resultado_eliminacion, row.names = FALSE)

# 7. Guardar auditoria
write.csv(
  centralidades,
  file.path(
    dirname(archivo),
    "AUDITORIA_03_centralidades.csv"
  ),
  row.names = FALSE,
  fileEncoding = "UTF-8"
)

write.csv(
  resultado_eliminacion,
  file.path(
    dirname(archivo),
    "AUDITORIA_03_eliminacion.csv"
  ),
  row.names = FALSE
)

cat("\nAUDITORIA 03 FINALIZADA\n")


# ============================================================
# AUDITORIA 04A - SENSIBILIDAD DEL UMBRAL
# Red territorial de 18 comunas
# No modifica los resultados originales
# ============================================================

library(igraph)

# Verificar que conservamos los objetos originales
stopifnot(
  exists("similitud"),
  exists("nombres"),
  exists("red"),
  vcount(red) == 18,
  ecount(red) == 61
)

# Construir red para cualquier cuantil
red_por_umbral <- function(p) {
  
  umbral_actual <- as.numeric(
    quantile(
      similitud[upper.tri(similitud)],
      probs = p
    )
  )
  
  idx <- which(
    upper.tri(similitud) &
      similitud >= umbral_actual,
    arr.ind = TRUE
  )
  
  enlaces_actuales <- data.frame(
    from = rownames(similitud)[idx[, 1]],
    to = colnames(similitud)[idx[, 2]],
    peso = similitud[idx]
  )
  
  red_actual <- graph_from_data_frame(
    enlaces_actuales,
    directed = FALSE,
    vertices = data.frame(name = nombres)
  )
  
  list(
    red = red_actual,
    enlaces = enlaces_actuales,
    umbral = umbral_actual
  )
}

# Evaluar cada escenario
evaluar_escenario <- function(p) {
  
  objeto <- red_por_umbral(p)
  g <- objeto$red
  
  componentes <- components(g)
  mayor <- which.max(componentes$csize)
  
  nodos_mayor <- V(g)$name[
    componentes$membership == mayor
  ]
  
  subred <- induced_subgraph(
    g,
    vids = nodos_mayor
  )
  
  articuladores <- as_ids(
    articulation_points(subred)
  )
  
  aisladas <- V(g)$name[degree(g) == 0]
  
  data.frame(
    cuantil = p,
    umbral_similitud = objeto$umbral,
    enlaces = ecount(g),
    componentes = componentes$no,
    componente_mayor = max(componentes$csize),
    aisladas = if (length(aisladas) == 0)
      "Ninguna" else paste(aisladas, collapse = " / "),
    coronel_articulador = "Coronel" %in% articuladores,
    curico_articulador = "Curicó" %in% articuladores
  )
}

resultados_umbral <- do.call(
  rbind,
  lapply(c(0.55, 0.60, 0.65), evaluar_escenario)
)

cat("\n=== SENSIBILIDAD DEL UMBRAL ===\n")
print(resultados_umbral, row.names = FALSE)

# Comparar enlaces con la red original
clave <- function(g) {
  el <- as_data_frame(g, what = "edges")
  paste(
    pmin(el$from, el$to),
    pmax(el$from, el$to),
    sep = "___"
  )
}

jaccard <- function(a, b) {
  length(intersect(a, b)) /
    length(union(a, b))
}

comparacion_enlaces <- data.frame(
  cuantil = c(0.55, 0.60, 0.65),
  jaccard_vs_base = sapply(
    c(0.55, 0.60, 0.65),
    function(p) {
      jaccard(
        clave(red),
        clave(red_por_umbral(p)$red)
      )
    }
  )
)

cat("\n=== COMPARACION DE ENLACES ===\n")
print(comparacion_enlaces, row.names = FALSE)

# Guardar salidas sin alterar originales
write.csv(
  resultados_umbral,
  file.path(dirname(archivo),
            "AUDITORIA_04A_umbrales.csv"),
  row.names = FALSE,
  fileEncoding = "UTF-8"
)

write.csv(
  comparacion_enlaces,
  file.path(dirname(archivo),
            "AUDITORIA_04A_jaccard.csv"),
  row.names = FALSE
)

cat("\nAUDITORIA 04A FINALIZADA\n")


# ============================================================
# AUDITORIA 04B - EXCLUSION DE BLOQUES
# Red de similitud territorial: 18 comunas
# No modifica los archivos originales
# ============================================================

library(igraph)

stopifnot(
  exists("matriz"),
  exists("bloques"),
  exists("nombres"),
  exists("red"),
  length(bloques) == 4
)

# 1. Construir distancia usando bloques seleccionados
distancia_seleccionada <- function(bloques_activos) {
  
  lista <- lapply(bloques_activos, function(vars) {
    
    x <- as.data.frame(
      lapply(matriz[, vars, drop = FALSE], as.numeric)
    )
    
    utiles <- vapply(
      x,
      function(v) is.finite(sd(v)) && sd(v) > 0,
      logical(1)
    )
    
    x <- x[, utiles, drop = FALSE]
    
    d <- as.matrix(dist(scale(x)))
    d / max(d[upper.tri(d)])
  })
  
  d_global <- Reduce("+", lista) / length(lista)
  dimnames(d_global) <- list(nombres, nombres)
  
  d_global
}

# 2. Construir red y conservar similitudes
crear_red_bloques <- function(bloques_activos) {
  
  d <- distancia_seleccionada(bloques_activos)
  s <- 1 - d
  
  umbral <- as.numeric(
    quantile(s[upper.tri(s)], probs = 0.60)
  )
  
  idx <- which(
    upper.tri(s) & s >= umbral,
    arr.ind = TRUE
  )
  
  enlaces <- data.frame(
    from = rownames(s)[idx[, 1]],
    to = colnames(s)[idx[, 2]],
    peso = s[idx]
  )
  
  g <- graph_from_data_frame(
    enlaces,
    directed = FALSE,
    vertices = data.frame(name = nombres)
  )
  
  list(red = g, similitud = s, umbral = umbral)
}

# 3. Reconstruir escenario base
base_bloques <- crear_red_bloques(bloques)

stopifnot(
  vcount(base_bloques$red) == 18,
  ecount(base_bloques$red) == 61,
  abs(base_bloques$umbral - 0.694155) < 0.00001
)

# 4. Evaluar exclusión de cada bloque
resultados_bloques <- do.call(
  rbind,
  lapply(names(bloques), function(excluir) {
    
    activos <- bloques[
      setdiff(names(bloques), excluir)
    ]
    
    escenario <- crear_red_bloques(activos)
    g <- escenario$red
    
    componentes <- components(g)
    id_mayor <- which.max(componentes$csize)
    
    subred <- induced_subgraph(
      g,
      vids = V(g)$name[
        componentes$membership == id_mayor
      ]
    )
    
    articuladores <- as_ids(
      articulation_points(subred)
    )
    
    aisladas <- V(g)$name[degree(g) == 0]
    
    sim_base <- base_bloques$similitud[
      upper.tri(base_bloques$similitud)
    ]
    
    sim_actual <- escenario$similitud[
      upper.tri(escenario$similitud)
    ]
    
    data.frame(
      bloque_excluido = excluir,
      enlaces = ecount(g),
      componentes = componentes$no,
      componente_mayor = max(componentes$csize),
      correlacion_spearman = cor(
        sim_base,
        sim_actual,
        method = "spearman"
      ),
      jaccard = jaccard(
        clave(base_bloques$red),
        clave(g)
      ),
      coronel_articulador =
        "Coronel" %in% articuladores,
      curico_articulador =
        "Curicó" %in% articuladores,
      aisladas = if (length(aisladas) == 0)
        "Ninguna" else paste(aisladas, collapse = " / ")
    )
  })
)

cat("\n=== SENSIBILIDAD POR BLOQUES ===\n")
print(resultados_bloques, row.names = FALSE,
      digits = 5)

# 5. Guardar auditoría
write.csv(
  resultados_bloques,
  file.path(
    dirname(archivo),
    "AUDITORIA_04B_exclusion_bloques.csv"
  ),
  row.names = FALSE,
  fileEncoding = "UTF-8"
)

cat("\nAUDITORIA 04B FINALIZADA\n")


# AUDITORIA 05 - PERIODOS RPC Y CATEGORIAS AMBIENTALES

library(dplyr)

# 1. Tabla temporal y regulatoria por comuna
auditoria_05 <- matriz %>%
  transmute(
    comuna = nombre_comuna_rpc,
    inicio_rpc = as.integer(primer_anio),
    fin_rpc = as.integer(ultimo_anio),
    anios_rpc = as.integer(n_anios_con_datos),
    poblacion_referencia = 2020,
    estado_ambiental = as.character(estado_zona_aire),
    n_declaraciones = as.numeric(n_declaraciones_zona),
    latente = as.numeric(tiene_latente_num)
  ) %>%
  arrange(inicio_rpc, fin_rpc, comuna)

cat("\n=== PERIODOS Y CLASIFICACION ===\n")
print(auditoria_05, n = Inf)

# 2. Comprobar redundancia entre indicadores
cat("\n=== VARIABLES AMBIENTALES ===\n")
print(table(
  declaraciones = auditoria_05$n_declaraciones,
  latente = auditoria_05$latente
))

cat("\n¿Son equivalentes? ",
    all(auditoria_05$n_declaraciones ==
          auditoria_05$latente + 1), "\n")

# 3. Periodos frente a categorías regulatorias
cat("\n=== PERIODOS RPC VS ESTADO AMBIENTAL ===\n")
print(
  with(auditoria_05,
       table(
         periodo = paste(inicio_rpc, fin_rpc, sep = "-"),
         estado = estado_ambiental
       ))
)

# 4. Verificar normalizacion del bloque ambiental
dist_ambiental <- function(vars) {
  x <- as.data.frame(
    lapply(matriz[, vars, drop = FALSE], as.numeric)
  )
  d <- as.matrix(dist(scale(x)))
  d / max(d[upper.tri(d)])
}

d_dos <- dist_ambiental(
  c("n_declaraciones_zona", "tiene_latente_num")
)

d_una <- dist_ambiental("tiene_latente_num")

cat("\n=== DIFERENCIA ENTRE DISTANCIAS AMBIENTALES ===\n")
print(max(abs(d_dos - d_una)))

# 5. Guardar auditoria
write.csv(
  auditoria_05,
  file.path(dirname(archivo),
            "AUDITORIA_05_temporal_regulatoria.csv"),
  row.names = FALSE,
  fileEncoding = "UTF-8"
)


# AUDITORIA 05B - DESFASE TEMPORAL RPC / POBLACION

auditoria_05b <- auditoria_05 %>%
  mutate(
    punto_medio_rpc = (inicio_rpc + fin_rpc) / 2,
    desfase_poblacional =
      poblacion_referencia - punto_medio_rpc,
    amplitud_periodo = fin_rpc - inicio_rpc + 1
  ) %>%
  select(
    comuna,
    inicio_rpc,
    fin_rpc,
    amplitud_periodo,
    punto_medio_rpc,
    poblacion_referencia,
    desfase_poblacional
  ) %>%
  arrange(desc(abs(desfase_poblacional)))

cat("\n=== DESFASE TEMPORAL ===\n")
print(auditoria_05b, n = Inf, width = Inf)

cat("\n=== RESUMEN ===\n")
auditoria_05b %>%
  summarise(
    desfase_absoluto_mediano =
      median(abs(desfase_poblacional)),
    desfase_absoluto_maximo =
      max(abs(desfase_poblacional)),
    comunas_desfase_mayor_5 =
      sum(abs(desfase_poblacional) > 5)
  ) %>%
  print()


# AUDITORIA 06A
# Inventario de variables históricas disponibles

columnas_historicas <- names(matriz)[
  grepl(
    "pobreza|poblacion|casos|anio|año|hacinamiento|zona|latente",
    names(matriz),
    ignore.case = TRUE
  )
]

cat("\n=== VARIABLES HISTORICAS DISPONIBLES ===\n")
print(columnas_historicas)

cat("\n=== COBERTURA DE LAS VARIABLES ===\n")

cobertura <- data.frame(
  variable = columnas_historicas,
  disponibles = sapply(
    matriz[columnas_historicas],
    function(x) sum(!is.na(x))
  ),
  faltantes = sapply(
    matriz[columnas_historicas],
    function(x) sum(is.na(x))
  )
)

print(cobertura, row.names = FALSE)


#Nueva codificacion

# ============================================================
# AUDITORIA 07A - BASE ORIGINAL RPC MINSAL
# Proyecto: Red de similitud territorial de 18 comunas
# Objetivo: leer la base y conocer sus variables
# No modifica ningun archivo original
# ============================================================

# 1. Instalar y cargar haven
if (!requireNamespace("haven", quietly = TRUE)) {
  install.packages("haven")
}

library(haven)

# 2. Seleccionar archivo ZIP
cat("\nSelecciona BD_RPC_1998-2019_ID.zip\n")

archivo_zip <- file.choose()

# 3. Crear carpeta de auditoria
carpeta_salida <- file.path(
  dirname(archivo_zip),
  "AUDITORIA_RPC"
)

dir.create(
  carpeta_salida,
  showWarnings = FALSE,
  recursive = TRUE
)

# 4. Extraer solamente el archivo SPSS
# Evita el PDF con caracteres especiales

nombre_sav <- "BBDD_RPC_1998-2019_ID.sav"

archivo_sav <- file.path(
  carpeta_salida,
  nombre_sav
)

cat("\nExtrayendo base SPSS...\n")

# Leer la entrada desde el ZIP
conexion_entrada <- unz(
  archivo_zip,
  nombre_sav,
  open = "rb"
)

# Abrir archivo de destino
conexion_salida <- file(
  archivo_sav,
  open = "wb"
)

# Copiar por bloques
repeat {
  bloque <- readBin(
    conexion_entrada,
    what = "raw",
    n = 1048576
  )
  
  if (length(bloque) == 0) break
  
  writeBin(
    bloque,
    conexion_salida
  )
}

close(conexion_entrada)
close(conexion_salida)

stopifnot(file.exists(archivo_sav))

cat("Archivo SPSS extraido correctamente.\n")

# 5. Leer base original
rpc_original <- haven::read_sav(archivo_sav)

# 6. Dimensiones
cat("\n====================================\n")
cat("DIMENSIONES DE LA BASE\n")
cat("====================================\n")

cat("Registros:", nrow(rpc_original), "\n")
cat("Variables:", ncol(rpc_original), "\n")

# 7. Nombres de variables
cat("\n====================================\n")
cat("NOMBRES DE VARIABLES\n")
cat("====================================\n")

print(names(rpc_original))

# 8. Diccionario de variables
diccionario <- data.frame(
  variable = names(rpc_original),
  tipo = vapply(
    rpc_original,
    function(x) paste(class(x), collapse = "/"),
    character(1)
  ),
  etiqueta = vapply(
    rpc_original,
    function(x) {
      etiqueta <- attr(x, "label")
      
      if (is.null(etiqueta)) {
        ""
      } else {
        as.character(etiqueta)
      }
    },
    character(1)
  )
)

cat("\n====================================\n")
cat("DICCIONARIO DE VARIABLES\n")
cat("====================================\n")

print(
  diccionario,
  row.names = FALSE,
  right = FALSE
)

# 9. Guardar diccionario
write.csv(
  diccionario,
  file.path(
    carpeta_salida,
    "AUDITORIA_07A_diccionario_RPC.csv"
  ),
  row.names = FALSE,
  fileEncoding = "UTF-8"
)

# 10. Guardar informacion de R
writeLines(
  capture.output(sessionInfo()),
  file.path(
    carpeta_salida,
    "AUDITORIA_07A_sesion_R.txt"
  )
)

cat("\n====================================\n")
cat("AUDITORIA 07A FINALIZADA\n")
cat("====================================\n")


# ============================================================
# AUDITORIA 07B - REGISTROS C34
# Base original RPC MINSAL 1998-2019
# Solo diagnostico: no modifica los datos
# ============================================================

# 1. Verificar base original
stopifnot(
  exists("rpc_original"),
  all(c(
    "CIE10", "Año_diag", "regCom",
    "RPC", "Comportamiento", "id_persona"
  ) %in% names(rpc_original))
)

# 2. Normalizar codigo diagnostico
codigo <- toupper(
  trimws(as.character(rpc_original$CIE10))
)

# Buscar todos los codigos que comienzan con C34
seleccion_c34 <- !is.na(codigo) &
  grepl("^C34", codigo)

rpc_c34 <- rpc_original[seleccion_c34, ]

cat("\n=== REGISTROS C34 ===\n")
cat("Total base:", nrow(rpc_original), "\n")
cat("Registros C34:", nrow(rpc_c34), "\n")

# 3. Codigos diagnosticos encontrados
cat("\n=== CODIGOS C34 ===\n")
print(sort(table(
  codigo[seleccion_c34]
), decreasing = TRUE))

# 4. Comportamiento tumoral
cat("\n=== COMPORTAMIENTO EN C34 ===\n")
print(table(
  rpc_c34$Comportamiento,
  useNA = "ifany"
))

# 5. Registros y personas distintas
ids <- rpc_c34$id_persona
ids_validos <- ids[!is.na(ids)]

cat("\n=== UNIDAD DE CONTEO ===\n")
cat("Filas C34:", nrow(rpc_c34), "\n")
cat("ID persona distintos:",
    length(unique(ids_validos)), "\n")
cat("Filas sin ID persona:",
    sum(is.na(ids)), "\n")

# 6. Resumir C34 por registro poblacional
resumen_rpc <- do.call(
  rbind,
  lapply(
    split(
      rpc_c34,
      as.character(rpc_c34$RPC)
    ),
    function(x) {
      
      anios <- suppressWarnings(
        as.integer(x[["Año_diag"]])
      )
      
      anios <- anios[!is.na(anios)]
      
      data.frame(
        RPC = as.character(x$RPC[1]),
        casos_c34 = nrow(x),
        primer_anio = if (length(anios) > 0)
          min(anios) else NA_integer_,
        ultimo_anio = if (length(anios) > 0)
          max(anios) else NA_integer_,
        anios_distintos = length(unique(anios))
      )
    }
  )
)

cat("\n=== C34 POR REGISTRO POBLACIONAL ===\n")
print(resumen_rpc, row.names = FALSE)

# 7. Verificar codigos y años faltantes
cat("\n=== CALIDAD BASICA ===\n")
cat("CIE10 faltante en base completa:",
    sum(is.na(codigo) | codigo == ""), "\n")
cat("Año diagnóstico faltante en C34:",
    sum(is.na(rpc_c34[["Año_diag"]])), "\n")

# 8. Guardar solo resumen agregado
write.csv(
  resumen_rpc,
  file.path(
    carpeta_salida,
    "AUDITORIA_07B_resumen_C34_RPC.csv"
  ),
  row.names = FALSE,
  fileEncoding = "UTF-8"
)

cat("\nAUDITORIA 07B FINALIZADA\n")


# ============================================================
# AUDITORIA 07C - COMPARACION C34 POR COMUNA
# Base original MINSAL vs matriz de 18 comunas
# No modifica los archivos originales
# ============================================================

library(dplyr)

stopifnot(
  exists("rpc_c34"),
  exists("matriz"),
  all(c("regCom", "Año_diag") %in% names(rpc_c34)),
  all(c(
    "codigo_comuna",
    "nombre_comuna_rpc",
    "casos_C34_pulmon"
  ) %in% names(matriz))
)

# 1. Contar los registros C34 por comuna
conteos_c34 <- rpc_c34 %>%
  mutate(
    codigo = as.integer(regCom),
    anio = as.integer(.data[["Año_diag"]])
  ) %>%
  group_by(codigo) %>%
  summarise(
    c34_base_original = n(),
    primer_anio_con_c34 = min(anio),
    ultimo_anio_con_c34 = max(anio),
    .groups = "drop"
  )

# 2. Recuperar los conteos usados en la red
conteos_red <- matriz %>%
  transmute(
    codigo = as.integer(as.character(codigo_comuna)),
    comuna = as.character(nombre_comuna_rpc),
    c34_matriz_original = as.integer(casos_C34_pulmon),
    primer_anio_matriz = as.integer(primer_anio),
    ultimo_anio_matriz = as.integer(ultimo_anio)
  )

# 3. Comparar ambas fuentes
comparacion_c34 <- conteos_red %>%
  left_join(conteos_c34, by = "codigo") %>%
  mutate(
    c34_base_original = coalesce(c34_base_original, 0L),
    diferencia = c34_base_original - c34_matriz_original,
    coincide = diferencia == 0
  ) %>%
  arrange(comuna)

cat("\n=== COMPARACION C34 POR COMUNA ===\n")

print(
  comparacion_c34,
  n = Inf,
  width = Inf
)

# 4. Resumen de la comprobacion
cat("\n=== RESUMEN DE AUDITORIA ===\n")

cat(
  "Comunas verificadas:",
  nrow(comparacion_c34), "\n"
)

cat(
  "Conteos coincidentes:",
  sum(comparacion_c34$coincide), "\n"
)

cat(
  "Conteos diferentes:",
  sum(!comparacion_c34$coincide), "\n"
)

cat(
  "Total C34 en las 18 comunas, matriz:",
  sum(comparacion_c34$c34_matriz_original), "\n"
)

cat(
  "Total C34 en las 18 comunas, base MINSAL:",
  sum(comparacion_c34$c34_base_original), "\n"
)

# 5. Guardar auditoria
write.csv(
  comparacion_c34,
  file.path(
    carpeta_salida,
    "AUDITORIA_07C_comparacion_C34.csv"
  ),
  row.names = FALSE,
  fileEncoding = "UTF-8"
)

cat("\nAUDITORIA 07C FINALIZADA\n")


# AUDITORIA 07D - AÑOS SIN CASOS C34
# No modifica los registros originales

library(dplyr)

# Ventanas oficiales de cobertura RPC
ventanas <- data.frame(
  RPC = 1:6,
  inicio = c(2003, 2006, 2003, 1998, 2008, 2017),
  fin = c(2015, 2011, 2014, 2017, 2018, 2019)
)

# Identificar RPC de cada comuna desde la base
asociacion <- rpc_c34 %>%
  filter(regCom %in% comparacion_c34$codigo) %>%
  distinct(
    codigo = regCom,
    RPC
  )

# Una comuna debe asociarse a un solo registro
stopifnot(
  !anyDuplicated(asociacion$codigo),
  nrow(asociacion) == 18
)

# Preparar periodos completos
periodos <- comparacion_c34 %>%
  select(codigo, comuna, c34_base_original) %>%
  left_join(asociacion, by = "codigo") %>%
  left_join(ventanas, by = "RPC") %>%
  mutate(
    anios_cobertura = fin - inicio + 1,
    promedio_anual_corregido =
      c34_base_original / anios_cobertura
  )

# Contar años con casos observados
anios_observados <- rpc_c34 %>%
  filter(regCom %in% comparacion_c34$codigo) %>%
  group_by(regCom) %>%
  summarise(
    anios_con_c34 = n_distinct(`Año_diag`),
    .groups = "drop"
  )

periodos <- periodos %>%
  left_join(
    anios_observados,
    by = c("codigo" = "regCom")
  ) %>%
  mutate(
    anios_sin_c34 =
      anios_cobertura - anios_con_c34
  ) %>%
  arrange(desc(anios_sin_c34), comuna)

cat("\n=== COBERTURA Y AÑOS SIN C34 ===\n")
print(periodos, n = Inf, width = Inf)

cat("\n=== RESUMEN ===\n")
cat("Comunas con al menos un año sin C34:",
    sum(periodos$anios_sin_c34 > 0), "\n")

# Guardar solo datos agregados
write.csv(
  periodos,
  file.path(
    carpeta_salida,
    "AUDITORIA_07D_cobertura_C34.csv"
  ),
  row.names = FALSE,
  fileEncoding = "UTF-8"
)


# AUDITORIA 08A - INVENTARIO DE ARCHIVOS INE
# No modifica archivos

ruta_redes <- dirname(archivo)

archivos_ine <- list.files(
  ruta_redes,
  pattern = "SalComUsuarios|poblacion|INE",
  full.names = TRUE,
  ignore.case = TRUE,
  recursive = TRUE
)

cat("\n=== ARCHIVOS POBLACIONALES ENCONTRADOS ===\n")
print(archivos_ine)

cat("\nTOTAL:", length(archivos_ine), "\n")


# ====================================================
# AUDITORIA 08B - ESTRUCTURA DE ARCHIVO INE
# Objetivo: identificar hojas y años disponibles
# No modifica archivos
# ====================================================

if (!requireNamespace("readxl", quietly = TRUE)) {
  install.packages("readxl")
}

library(readxl)

# Seleccionar archivo regional 08
candidatos <- archivos_ine[
  grepl("SalComUsuarios-08Tok\\.xls$",
        archivos_ine, ignore.case = TRUE)
]

# Preferir copia dentro de base poblacional
preferido <- candidatos[
  grepl("base poblacional", candidatos,
        ignore.case = TRUE)
]

archivo_ine <- if (length(preferido) > 0) {
  preferido[1]
} else {
  candidatos[1]
}

stopifnot(
  length(archivo_ine) == 1,
  !is.na(archivo_ine),
  file.exists(archivo_ine)
)

cat("\n=== ARCHIVO SELECCIONADO ===\n")
print(archivo_ine)

# Identificar hojas
hojas <- excel_sheets(archivo_ine)

cat("\n=== HOJAS DISPONIBLES ===\n")
print(hojas)

# Examinar primeras filas de cada hoja
for (hoja in hojas) {
  
  cat("\n================================\n")
  cat("HOJA:", hoja, "\n")
  cat("================================\n")
  
  muestra <- read_excel(
    archivo_ine,
    sheet = hoja,
    col_names = FALSE,
    n_max = 12
  )
  
  cat("Primeras 12 filas y 12 columnas:\n")
  
  print(
    muestra[, seq_len(min(12, ncol(muestra)))],
    width = Inf
  )
}

cat("\nAUDITORIA 08B FINALIZADA\n")


# ====================================================
# AUDITORIA 08C - IDENTIFICAR ARCHIVOS POBLACIONALES
# Revisar los 15 archivos SalComUsuarios
# No modifica archivos
# ====================================================

library(readxl)

# Usar solo los archivos de la carpeta DatCom
archivos_datcom <- archivos_ine[
  grepl("SalComUsuarios-[0-9]{2}Tok\\.xls$",
        archivos_ine, ignore.case = TRUE) &
    grepl("base poblacional", archivos_ine,
          ignore.case = TRUE)
]

cat("\n=== ARCHIVOS DATCOM ===\n")
cat("Cantidad:", length(archivos_datcom), "\n")

# Obtener encabezados principales de cada archivo
identificacion <- lapply(archivos_datcom, function(ruta) {
  
  datos <- read_excel(
    ruta,
    sheet = "T1",
    col_names = FALSE,
    n_max = 6,
    .name_repair = "minimal"
  )
  
  data.frame(
    archivo = basename(ruta),
    nombre_territorio = as.character(datos[[1]][3]),
    descripcion = as.character(datos[[2]][3]),
    stringsAsFactors = FALSE
  )
})

identificacion <- do.call(rbind, identificacion)

cat("\n=== IDENTIFICACION TERRITORIAL ===\n")

print(
  identificacion,
  row.names = FALSE
)

cat("\nAUDITORIA 08C FINALIZADA\n")


# ===================================================
# AUDITORIA 08D - INVENTARIO DE BASE POBLACIONAL
# Solo identifica archivos. No modifica nada.
# ===================================================

carpeta_base <- file.path(
  ruta_redes,
  "base poblacional"
)

stopifnot(dir.exists(carpeta_base))

archivos_todos <- list.files(
  carpeta_base,
  recursive = TRUE,
  full.names = TRUE,
  all.files = FALSE
)

info <- file.info(archivos_todos)

inventario <- data.frame(
  carpeta = dirname(archivos_todos),
  archivo = basename(archivos_todos),
  extension = tolower(
    tools::file_ext(archivos_todos)
  ),
  es_directorio = info$isdir,
  stringsAsFactors = FALSE
)

inventario <- inventario[
  !is.na(inventario$es_directorio) &
    !inventario$es_directorio,
]

cat("\n=== TOTAL DE ARCHIVOS ===\n")
cat(nrow(inventario), "\n")

cat("\n=== ARCHIVOS POR EXTENSION ===\n")
print(sort(
  table(inventario$extension),
  decreasing = TRUE
))

cat("\n=== ARCHIVOS DISPONIBLES ===\n")
print(
  inventario[, c("archivo", "extension")],
  row.names = FALSE,
  max = 100
)

cat("\nAUDITORIA 08D FINALIZADA\n")


# ============================================================
# AUDITORIA 08E - EXTRACCION DE POBLACION HISTORICA
# Prueba inicial: Concepcion y Coronel
# ============================================================

library(readxl)

# Leer toda la hoja T1 sin encabezados
datos <- read_excel(
  archivo_ine,
  sheet = "T1",
  col_names = FALSE,
  col_types = "text",
  .name_repair = "minimal"
)

# Convertir a matriz
datos <- as.matrix(datos)

# Buscar las filas que identifican cada comuna
comunas_prueba <- c("CONCEPCIÓN", "CORONEL")

extraer_poblacion <- function(nombre_comuna) {
  
  filas <- which(
    toupper(trimws(datos[, 1])) == nombre_comuna
  )
  
  if (length(filas) != 1) {
    stop(
      paste("No se identifico una fila unica para",
            nombre_comuna)
    )
  }
  
  fila_inicio <- filas[1]
  
  # Identificar encabezado AÑO
  fila_anios <- fila_inicio + 2
  
  # Buscar años y población dentro del bloque
  bloque <- datos[
    fila_anios:min(fila_anios + 40, nrow(datos)),
    ,
    drop = FALSE
  ]
  
  resultados <- data.frame(
    anio = integer(),
    poblacion = numeric()
  )
  
  for (i in seq_len(nrow(bloque))) {
    
    anio_izq <- suppressWarnings(
      as.integer(bloque[i, 1])
    )
    
    poblacion_izq <- suppressWarnings(
      as.numeric(bloque[i, 2])
    )
    
    anio_der <- suppressWarnings(
      as.integer(bloque[i, 5])
    )
    
    poblacion_der <- suppressWarnings(
      as.numeric(bloque[i, 6])
    )
    
    if (!is.na(anio_izq) &&
        anio_izq >= 1990 &&
        anio_izq <= 2020) {
      
      resultados <- rbind(
        resultados,
        data.frame(
          anio = anio_izq,
          poblacion = poblacion_izq
        )
      )
    }
    
    if (!is.na(anio_der) &&
        anio_der >= 1990 &&
        anio_der <= 2020) {
      
      resultados <- rbind(
        resultados,
        data.frame(
          anio = anio_der,
          poblacion = poblacion_der
        )
      )
    }
  }
  
  resultados <- resultados[
    !duplicated(resultados$anio),
  ]
  
  resultados$comuna <- nombre_comuna
  
  resultados[order(resultados$anio), ]
}

# Ejecutar para ambas comunas
poblacion_prueba <- do.call(
  rbind,
  lapply(
    comunas_prueba,
    extraer_poblacion
  )
)

cat("\n=== RESULTADO EXTRACCION ===\n")

print(
  poblacion_prueba,
  row.names = FALSE
)

cat("\n=== COBERTURA ===\n")

print(
  aggregate(
    anio ~ comuna,
    poblacion_prueba,
    function(x) c(
      minimo = min(x),
      maximo = max(x),
      cantidad = length(x)
    )
  )
)

cat("\nAUDITORIA 08E FINALIZADA\n")



# ============================================================
# AUDITORIA 08F - VERSION CORREGIDA
# Poblacion historica de las 18 comunas
# Se exige cobertura completa solo durante el periodo RPC
# ============================================================

library(readxl)
library(dplyr)

normalizar <- function(x) {
  x <- iconv(as.character(x), to = "ASCII//TRANSLIT")
  toupper(trimws(x))
}

extraer_comuna <- function(datos, comuna) {
  
  nombres <- normalizar(datos[, 1])
  inicio <- which(nombres == normalizar(comuna))
  
  if (length(inicio) != 1) {
    stop(paste("Comuna no identificada:", comuna))
  }
  
  # Buscar siguiente bloque
  siguientes <- which(
    nombres == "INICIO" &
      seq_along(nombres) > inicio
  )
  
  fin <- if (length(siguientes) > 0) {
    min(siguientes) - 1
  } else {
    nrow(datos)
  }
  
  bloque <- datos[inicio:fin, , drop = FALSE]
  
  extraer_par <- function(col_anio, col_pob) {
    
    anio <- suppressWarnings(
      as.integer(bloque[, col_anio])
    )
    
    pob <- suppressWarnings(
      as.numeric(bloque[, col_pob])
    )
    
    data.frame(
      anio = anio,
      poblacion = pob
    ) |>
      filter(
        !is.na(anio),
        anio >= 1990,
        anio <= 2020,
        !is.na(poblacion),
        poblacion > 0
      )
  }
  
  resultado <- bind_rows(
    extraer_par(1, 2),
    extraer_par(5, 6)
  )
  
  conflictos <- resultado |>
    group_by(anio) |>
    summarise(
      valores = n_distinct(poblacion),
      .groups = "drop"
    ) |>
    filter(valores > 1)
  
  if (nrow(conflictos) > 0) {
    stop(paste(
      "Poblaciones contradictorias:", comuna
    ))
  }
  
  resultado <- resultado |>
    distinct(anio, .keep_all = TRUE) |>
    arrange(anio)
  
  resultado$comuna <- comuna
  resultado
}

# Extraer todas las comunas
resultados <- list()

for (region in names(regiones)) {
  
  ruta <- archivos_datcom[
    grepl(
      paste0(
        "SalComUsuarios-", region, "Tok\\.xls$"
      ),
      archivos_datcom,
      ignore.case = TRUE
    )
  ]
  
  stopifnot(length(ruta) == 1)
  
  datos_region <- as.matrix(
    read_excel(
      ruta,
      sheet = "T1",
      col_names = FALSE,
      col_types = "text",
      .name_repair = "minimal"
    )
  )
  
  for (comuna in regiones[[region]]) {
    resultados[[comuna]] <- extraer_comuna(
      datos_region,
      comuna
    )
  }
}

poblacion_historica <- bind_rows(resultados)

# Comprobar cobertura durante los RPC
validacion <- periodos |>
  select(
    comuna,
    inicio,
    fin,
    anios_cobertura,
    c34_base_original
  ) |>
  left_join(
    poblacion_historica,
    by = "comuna"
  ) |>
  filter(anio >= inicio, anio <= fin) |>
  group_by(
    comuna,
    inicio,
    fin,
    anios_cobertura,
    c34_base_original
  ) |>
  summarise(
    anios_encontrados = n_distinct(anio),
    personas_anio = sum(poblacion),
    .groups = "drop"
  ) |>
  mutate(
    cobertura_correcta =
      anios_encontrados == anios_cobertura,
    tasa_c34_100k =
      c34_base_original / personas_anio * 100000
  ) |>
  arrange(comuna)

cat("\n=== VALIDACION POBLACION RPC ===\n")

print(
  validacion,
  n = Inf,
  width = Inf
)

cat("\n=== RESUMEN ===\n")
cat(
  "Comunas procesadas:",
  nrow(validacion), "\n"
)

cat(
  "Comunas con cobertura completa:",
  sum(validacion$cobertura_correcta), "\n"
)

# Guardar solo si las 18 estan completas
if (
  nrow(validacion) == 18 &&
  all(validacion$cobertura_correcta)
) {
  
  write.csv(
    poblacion_historica,
    file.path(
      carpeta_salida,
      "AUDITORIA_08F_poblacion_historica.csv"
    ),
    row.names = FALSE
  )
  
  write.csv(
    validacion,
    file.path(
      carpeta_salida,
      "AUDITORIA_08F_tasas_C34.csv"
    ),
    row.names = FALSE
  )
  
  cat("\nAUDITORIA 08F COMPLETADA\n")
  
} else {
  
  cat(
    "\nATENCION: Hay comunas con cobertura incompleta.\n"
  )
}


# ============================================================
# AUDITORIA 08G - CONSISTENCIA POBLACIONAL
# Comparar poblacion 2020 original e historica
# No modifica archivos
# ============================================================

library(dplyr)

# Poblacion 2020 de las planillas historicas
poblacion_ine_2020 <- poblacion_historica |>
  filter(anio == 2020) |>
  transmute(
    comuna,
    poblacion_2020_historica = poblacion
  )

# Poblacion 2020 de la matriz original
poblacion_matriz_2020 <- matriz |>
  transmute(
    comuna = as.character(nombre_comuna_rpc),
    poblacion_2020_original = as.numeric(poblacion_2020)
  )

# Comparacion
comparacion_poblacion <- poblacion_matriz_2020 |>
  left_join(
    poblacion_ine_2020,
    by = "comuna"
  ) |>
  mutate(
    diferencia =
      poblacion_2020_historica -
      poblacion_2020_original,
    diferencia_pct =
      100 * diferencia / poblacion_2020_original
  ) |>
  arrange(desc(abs(diferencia_pct)))

cat("\n=== COMPARACION POBLACION 2020 ===\n")

print(
  comparacion_poblacion,
  n = Inf,
  width = Inf
)

cat("\n=== RESUMEN ===\n")

cat(
  "Comunas comparadas:",
  nrow(comparacion_poblacion), "\n"
)

cat(
  "Poblaciones sin correspondencia:",
  sum(is.na(comparacion_poblacion$poblacion_2020_historica)),
  "\n"
)

cat(
  "Mayor diferencia porcentual absoluta:",
  max(abs(comparacion_poblacion$diferencia_pct),
      na.rm = TRUE),
  "\n"
)

cat("\nAUDITORIA 08G FINALIZADA\n")


# ============================================================
# AUDITORIA 09A - NUEVA MATRIZ ONCOLOGICA C34
# Integra las tasas historicas a la matriz original
# No modifica los archivos anteriores
# ============================================================

library(dplyr)

# Seleccionar tasas historicas
tasas_c34 <- validacion |>
  select(
    comuna,
    c34_base_original,
    personas_anio,
    tasa_c34_100k
  )

# Recuperar matriz original
matriz_c34 <- matriz |>
  mutate(
    comuna = as.character(nombre_comuna_rpc)
  ) |>
  left_join(
    tasas_c34,
    by = "comuna"
  )

# Verificar correspondencia
stopifnot(
  nrow(matriz_c34) == 18,
  !any(is.na(matriz_c34$tasa_c34_100k)),
  !anyDuplicated(matriz_c34$comuna)
)

# Transformacion logaritmica
matriz_c34 <- matriz_c34 |>
  mutate(
    tasa_c34_log = log1p(tasa_c34_100k)
  )

# Comparar el indicador nuevo con el anterior
comparacion_indicadores <- matriz_c34 |>
  transmute(
    comuna,
    tasa_original_100k =
      tasa_rpc_anual_aprox_100k,
    tasa_c34_historica_100k =
      tasa_c34_100k,
    diferencia =
      tasa_c34_historica_100k -
      tasa_rpc_anual_aprox_100k
  ) |>
  arrange(comuna)

cat("\n=== COMPARACION DE INDICADORES ===\n")

print(
  comparacion_indicadores,
  n = Inf,
  width = Inf
)

cat("\n=== RESUMEN ===\n")
cat("Comunas integradas:", nrow(matriz_c34), "\n")
cat("Valores C34 faltantes:",
    sum(is.na(matriz_c34$tasa_c34_100k)), "\n")

cat("\nAUDITORIA 09A FINALIZADA\n")


# =====================================================
# AUDITORIA 09B - EFECTO DEL CAMBIO DE INDICADOR
# No modifica datos ni archivos anteriores
# =====================================================

library(dplyr)

comparacion_09B <- matriz_c34 |>
  mutate(
    # C34 usando poblacion fija 2020
    # y la ventana oficial RPC
    tasa_c34_pob2020 =
      c34_base_original /
      (as.numeric(ultimo_anio) -
         as.numeric(primer_anio) + 1) /
      as.numeric(poblacion_2020) * 100000,
    
    # C34 con poblacion historica
    tasa_c34_historica = tasa_c34_100k
  ) |>
  select(
    comuna,
    tasa_rpc_anual_aprox_100k,
    tasa_c34_pob2020,
    tasa_c34_historica
  ) |>
  arrange(comuna)

cat("\n=== COMPARACION DE TRES INDICADORES ===\n")
print(comparacion_09B, n = Inf, width = Inf)

cat("\n=== CORRELACIONES ENTRE INDICADORES ===\n")
print(
  cor(
    comparacion_09B[, -1],
    method = "spearman",
    use = "complete.obs"
  )
)

cat("\nAUDITORIA 09B FINALIZADA\n")


# ============================================================
# AUDITORIA 10A - RED C34 CON POBLACION HISTORICA
# Reconstruccion de distancias y red de similitud
# ============================================================

library(dplyr)
library(igraph)

# 1. Seleccionar indicadores

bloques <- list(
  oncologico = c(
    "tasa_c34_log"
  ),
  
  ambiental = c(
    "n_declaraciones_zona",
    "tiene_latente_num"
  ),
  
  social = c(
    "pobreza_multidimensional_2017",
    "porcentaje_viviendas_hacinadas_2024"
  ),
  
  demografico_sanitario = c(
    "porcentaje_65mas_2020",
    "aps_publicas_10k_log",
    "hospital_alta_complejidad_num"
  )
)

# 2. Verificar variables
variables <- unlist(bloques)

stopifnot(
  all(variables %in% names(matriz_c34)),
  !anyNA(matriz_c34[, variables])
)

# 3. Distancias normalizadas por bloque

distancia_bloque <- function(columnas) {
  
  datos <- matriz_c34 |>
    select(all_of(columnas)) |>
    mutate(across(everything(), as.numeric))
  
  # Estandarizacion
  datos_z <- scale(datos)
  
  # Evitar variables sin variacion
  stopifnot(
    all(is.finite(datos_z))
  )
  
  d <- as.matrix(dist(datos_z))
  
  maximo <- max(d)
  
  if (maximo == 0) {
    stop("Bloque sin variacion")
  }
  
  d / maximo
}

distancias <- lapply(
  bloques,
  distancia_bloque
)

# 4. Distancia global: bloques con igual peso
distancia_global <- Reduce("+", distancias) /
  length(distancias)

similitud <- 1 - distancia_global

diag(similitud) <- 1

# 5. Umbral del percentil 60
valores_similitud <- similitud[
  upper.tri(similitud)
]

umbral <- as.numeric(
  quantile(valores_similitud, probs = 0.60)
)

# 6. Matriz de adyacencia
adyacencia <- similitud >= umbral
diag(adyacencia) <- FALSE

# 7. Construir grafo
red_c34 <- graph_from_adjacency_matrix(
  adyacencia,
  mode = "undirected",
  diag = FALSE
)

V(red_c34)$name <- matriz_c34$comuna

# Agregar pesos de similitud
aristas <- as_edgelist(red_c34, names = FALSE)

E(red_c34)$weight <- apply(
  aristas,
  1,
  function(e) {
    similitud[e[1], e[2]]
  }
)

# 8. Resultados basicos
cat("\n=== RED C34 HISTORICA ===\n")

cat("Nodos:", vcount(red_c34), "\n")
cat("Aristas:", ecount(red_c34), "\n")
cat("Umbral:", umbral, "\n")

componentes <- components(red_c34)

cat(
  "Componentes:",
  componentes$no, "\n"
)

cat(
  "Componente mayor:",
  max(componentes$csize), "\n"
)

cat("\n=== COMUNAS AISLADAS ===\n")

print(
  V(red_c34)$name[
    degree(red_c34) == 0
  ]
)

# 9. Modularidad descriptiva
set.seed(1234)

comunidades <- cluster_louvain(
  red_c34,
  weights = E(red_c34)$weight
)

cat("\n=== MODULARIDAD DESCRIPTIVA ===\n")

cat(
  "Modularidad:",
  modularity(comunidades), "\n"
)

cat(
  "Comunidades detectadas:",
  length(comunidades), "\n"
)

cat("\nAUDITORIA 10A FINALIZADA\n")


# ==========================================================
# AUDITORIA 10B - CONTRIBUCION DE LA VARIABLE HOSPITALARIA
# Comparacion de red C34 completa vs sin hospital
# ==========================================================

library(igraph)

# 1. Conservar todos los bloques, excepto la variable
# hospital_alta_complejidad_num
bloques_sin_hospital <- bloques

bloques_sin_hospital$demografico_sanitario <-
  setdiff(
    bloques$demografico_sanitario,
    "hospital_alta_complejidad_num"
  )

stopifnot(
  length(bloques_sin_hospital$demografico_sanitario) == 2
)

# 2. Reconstruir distancias conservando los 4 bloques
distancias_sin_hospital <- lapply(
  bloques_sin_hospital,
  distancia_bloque
)

distancia_sin_hospital <- Reduce(
  "+",
  distancias_sin_hospital
) / length(distancias_sin_hospital)

similitud_sin_hospital <- 1 - distancia_sin_hospital

diag(similitud_sin_hospital) <- 1

# 3. Comparar con el MISMO umbral de la red C34
# Esto permite ver cuantas conexiones realmente cambian

adyacencia_sin_hospital <-
  similitud_sin_hospital >= umbral

diag(adyacencia_sin_hospital) <- FALSE

red_sin_hospital <- graph_from_adjacency_matrix(
  adyacencia_sin_hospital,
  mode = "undirected",
  diag = FALSE
)

V(red_sin_hospital)$name <- matriz_c34$comuna

# 4. Comparacion de enlaces
enlaces_originales <- as.matrix(
  as_adjacency_matrix(red_c34, sparse = FALSE)
) > 0

enlaces_nuevos <- as.matrix(
  as_adjacency_matrix(red_sin_hospital, sparse = FALSE)
) > 0

superior <- upper.tri(enlaces_originales)

comunes <- sum(
  enlaces_originales[superior] &
    enlaces_nuevos[superior]
)

agregados <- sum(
  !enlaces_originales[superior] &
    enlaces_nuevos[superior]
)

perdidos <- sum(
  enlaces_originales[superior] &
    !enlaces_nuevos[superior]
)

union <- comunes + agregados + perdidos

jaccard <- if (union > 0) comunes / union else NA_real_

# 5. Resultados principales
cat("\n=== RED CON Y SIN HOSPITAL ===\n")

cat("Nodos:", vcount(red_c34), "\n")
cat("Aristas red completa:", ecount(red_c34), "\n")
cat("Aristas sin hospital:", ecount(red_sin_hospital), "\n")
cat("Enlaces conservados:", comunes, "\n")
cat("Enlaces nuevos:", agregados, "\n")
cat("Enlaces perdidos:", perdidos, "\n")
cat("Jaccard de enlaces:", round(jaccard, 4), "\n")

cat("\n=== COMPONENTES ===\n")
cat("Red completa:", components(red_c34)$no, "\n")
cat("Sin hospital:", components(red_sin_hospital)$no, "\n")

cat("\n=== COMUNAS AISLADAS SIN HOSPITAL ===\n")
print(
  V(red_sin_hospital)$name[
    degree(red_sin_hospital) == 0
  ]
)

cat("\nAUDITORIA 10B FINALIZADA\n")


# =====================================================
# AUDITORIA 10C - CONTINUACION
# Comparar red completa vs red sin hospital
# Con el mismo percentil de similitud
# =====================================================

library(igraph)

# 1. Crear red sin hospital con umbral ajustado
red_sin_hospital_ajustada <- graph_from_adjacency_matrix(
  adyacencia_ajustada,
  mode = "undirected",
  diag = FALSE
)

V(red_sin_hospital_ajustada)$name <- matriz_c34$comuna

# 2. Obtener matrices de conexiones
A <- as.matrix(
  as_adjacency_matrix(red_c34, sparse = FALSE)
) > 0

B <- as.matrix(
  as_adjacency_matrix(
    red_sin_hospital_ajustada,
    sparse = FALSE
  )
) > 0

superior <- upper.tri(A)

# 3. Comparar conexiones
conservadas <- sum(A[superior] & B[superior])
nuevas <- sum(!A[superior] & B[superior])
perdidas <- sum(A[superior] & !B[superior])

total_union <- conservadas + nuevas + perdidas

jaccard_ajustado <- conservadas / total_union

# 4. Resultados
cat("\n=== AUDITORIA 10C ===\n")

cat("Umbral red completa:", umbral, "\n")
cat("Umbral sin hospital:", umbral_ajustado, "\n")

cat("Aristas red completa:",
    ecount(red_c34), "\n")

cat("Aristas sin hospital:",
    ecount(red_sin_hospital_ajustada), "\n")

cat("Conexiones conservadas:", conservadas, "\n")
cat("Conexiones nuevas:", nuevas, "\n")
cat("Conexiones perdidas:", perdidas, "\n")
cat("Jaccard ajustado:",
    round(jaccard_ajustado, 4), "\n")

cat("\n=== COMPONENTES ===\n")

cat("Red completa:",
    components(red_c34)$no, "\n")

cat("Sin hospital:",
    components(red_sin_hospital_ajustada)$no, "\n")

cat("\n=== COMUNAS AISLADAS ===\n")

print(
  V(red_sin_hospital_ajustada)$name[
    degree(red_sin_hospital_ajustada) == 0
  ]
)

cat("\nAUDITORIA 10C FINALIZADA\n")


# AUDITORIA 10D - CONEXIONES QUE CAMBIARON

nombres <- V(red_c34)$name

# Usar matrices A y B de la auditoria 10C
posiciones <- which(upper.tri(A), arr.ind = TRUE)

cambios <- data.frame(
  comuna_1 = nombres[posiciones[, 1]],
  comuna_2 = nombres[posiciones[, 2]],
  red_completa = A[upper.tri(A)],
  red_sin_hospital = B[upper.tri(B)]
)

cambios <- cambios[
  cambios$red_completa != cambios$red_sin_hospital,
]

cambios$tipo <- ifelse(
  cambios$red_sin_hospital,
  "Conexion nueva",
  "Conexion perdida"
)

cat("\n=== CONEXIONES MODIFICADAS ===\n")
print(
  cambios[, c("comuna_1", "comuna_2", "tipo")],
  row.names = FALSE
)

cat("\n=== COMUNAS AISLADAS EN CADA RED ===\n")

cat("Red completa:\n")
print(nombres[degree(red_c34) == 0])

cat("Red sin hospital:\n")
print(nombres[degree(red_sin_hospital_ajustada) == 0])

cat("\nAUDITORIA 10D FINALIZADA\n")
