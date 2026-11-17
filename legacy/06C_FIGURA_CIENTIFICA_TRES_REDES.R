# ================================================================
# 06C | FIGURA CIENTIFICA COMPARATIVA DE LAS TRES REDES
# Usa EXCLUSIVAMENTE los grafos calculados y guardados.
# R base + igraph (no requiere ggplot2 ni otros paquetes).
# No recalcula distancias, enlaces, comunidades ni pruebas.
# ================================================================

if (!requireNamespace("igraph", quietly = TRUE)) {
  stop("Se necesita el paquete 'igraph'.")
}

# 1. Trabajar desde la sesion recuperada; no volver a calcular redes.
if (!exists("resultados_finales")) {
  stop("Primero debes cargar tu objeto 'resultados_finales' en RStudio.")
}
if (!exists("proyecto") || !dir.exists(proyecto)) {
  stop("No se encontro la carpeta de trabajo 'proyecto'.")
}

redes_06C <- resultados_finales$redes_historicas$redes
matriz_06C <- resultados_finales$matriz_v2

extraer_grafo_06C <- function(obj) {
  if (inherits(obj, "igraph")) return(obj)
  if (is.list(obj) && inherits(obj$grafo, "igraph")) return(obj$grafo)
  stop("No se encontro el grafo 'grafo' de una de las redes.")
}

g_06C <- lapply(redes_06C[c("A", "B", "A_ajustada")], extraer_grafo_06C)
if (any(vapply(g_06C, is.null, logical(1)))) stop("Falta alguna de las redes A, B o A_ajustada.")

# 2. Posiciones ESQUEMATICAS comunes para comparar tres paneles.
#    La geometria no representa coordenadas de Chile ni distancia entre comunas.
#    Las aristas reales se extraen de igraph y NO se dibujan manualmente.
xy_06C <- data.frame(
  comuna = c(
    "Calama", "Chiguayante", "Concepción", "Coronel", "Hualpén",
    "Hualqui", "Lota", "Penco", "San Pedro de la Paz",
    "Talcahuano", "Tomé", "Curicó", "Los Ángeles", "Talca",
    "Tocopilla", "Valdivia", "Maule", "María Elena"
  ),
  x = c(
    .80, .78, .56, .67, .25,
    .43, .29, .65, .51,
    .53, .32, .42, .59, .27,
    .72, .37, .84, .11
  ),
  y = c(
    .84, .68, .77, .80, .70,
    .62, .85, .63, .92,
    .68, .59, .32, .16, .23,
    .30, .15, .46, .43
  ),
  stringsAsFactors = FALSE
)
rownames(xy_06C) <- xy_06C$comuna

# Comprobacion critica: nada de comunas inventadas ni perdidas.
for (nombre in names(g_06C)) {
  presentes <- igraph::V(g_06C[[nombre]])$name
  if (length(presentes) != 18L || !setequal(presentes, xy_06C$comuna)) {
    stop(paste("Los nombres de nodos de la red", nombre,
               "no coinciden con las 18 comunas validadas."))
  }
}
if (!all(c("comuna", "tiene_latente_num") %in% names(matriz_06C))) {
  stop("No se encontraron las columnas regulatorias esperadas.")
}
if (anyDuplicated(matriz_06C$comuna)) stop("Hay comunas duplicadas en la matriz.")

categoria_06C <- setNames(
  ifelse(matriz_06C$tiene_latente_num == 1, "D2_L1", "D1_L0"),
  as.character(matriz_06C$comuna)
)
if (anyNA(categoria_06C[xy_06C$comuna])) {
  stop("No hay categoria regulatoria para una o mas comunas.")
}

# 3. Colores sobrios y legibles.
colores_06C <- c(D1_L0 = "#D99A3C", D2_L1 = "#347F9C")
col_gris_06C <- grDevices::adjustcolor("#87939C", alpha.f = .46)
col_rojo_06C <- "#C34D43"

# Correcciones manuales MINIMAS para que los textos no tapen nodos.
# dx,dy estan expresados en coordenadas del panel [0,1].
etiquetas_06C <- data.frame(
  comuna = xy_06C$comuna,
  dx = c(.030,.027,-.065,.013,-.020,-.025,-.005,.020,0,-.058,-.010,
         -.005,.026,-.018,.020,-.010,.018,.008),
  dy = c(.016,.015,-.035,.028,.028,-.033,.030,-.030,.031,-.030,-.034,
         .037,-.034,-.033,.031,-.036,.024,.025),
  stringsAsFactors = FALSE
)
rownames(etiquetas_06C) <- etiquetas_06C$comuna

# Clave de enlace no dirigido. Sirve para destacar enlaces nuevos vs B.
clave_06C <- function(a, b) paste(sort(c(a, b)), collapse = "||")
pares_06C <- function(g) {
  a <- igraph::as_data_frame(g, what = "edges")
  if (nrow(a) == 0) return(data.frame(from=character(),to=character(),clave=character()))
  a$clave <- mapply(clave_06C, a$from, a$to, USE.NAMES = FALSE)
  a
}
referencia_B_06C <- pares_06C(g_06C$B)$clave

# 4. Funcion de dibujo: TODOS los enlaces son del grafo guardado.
dibujar_panel_06C <- function(nombre, titulo) {
  gr <- g_06C[[nombre]]
  ar <- pares_06C(gr)

  graphics::plot.new()
  graphics::plot.window(xlim = c(.02, .98), ylim = c(.035, .99), asp = NA)
  graphics::rect(.025, .045, .975, .975,
                 col = "white", border = "#CBD2D8", lwd = 1)
  graphics::rect(.025, .898, .975, .975,
                 col = "#F1F4F6", border = NA)
  graphics::text(.50, .937, titulo, cex = .91, font = 2, col = "#24303C")

  # Ajustar y de la red para reservar una franja superior al titulo.
  sx <- function(x) .07 + x * .86
  sy <- function(y) .095 + y * .76

  if (nrow(ar) > 0) {
    nueva <- if (nombre == "B") rep(FALSE, nrow(ar)) else
      !(ar$clave %in% referencia_B_06C)

    # Primero dibujar aristas compartidas en gris y luego las nuevas en rojo.
    for (k in c(which(!nueva), which(nueva))) {
      p <- xy_06C[as.character(ar$from[k]), ]
      q <- xy_06C[as.character(ar$to[k]), ]
      graphics::segments(sx(p$x), sy(p$y), sx(q$x), sy(q$y),
                         col = if (nueva[k]) col_rojo_06C else col_gris_06C,
                         lwd = if (nueva[k]) 1.8 else 1.05)
    }
  }

  # Nodos: color segun la matriz original, no segun su posicion.
  nombres <- igraph::V(gr)$name
  pt <- xy_06C[nombres, ]
  graphics::points(sx(pt$x), sy(pt$y), pch = 21, cex = 1.20,
                   bg = unname(colores_06C[categoria_06C[nombres]]),
                   col = "white", lwd = 1.10)

  # Etiquetas comunales sobrepuesta deliberadamente a las aristas.
  for (nm in nombres) {
    pos <- xy_06C[nm, ]
    d <- etiquetas_06C[nm, ]
    graphics::text(sx(pos$x + d$dx), sy(pos$y + d$dy), labels = nm,
                   cex = .65, col = "#17212B", font = 2,
                   adj = c(.5, .5))
  }
  graphics::box(col = NA)
}

# 5. Leyenda COMUN fuera de los tres paneles.
dibujar_leyenda_06C <- function() {
  graphics::plot.new()
  graphics::plot.window(xlim = c(0, 1), ylim = c(0, 1))
  graphics::rect(.018, .11, .982, .90,
                 col = "#FAFBFC", border = "#CBD2D8", lwd = 1)
  graphics::text(.035, .70, "Regulatory profile:", adj = 0,
                 cex = .96, font = 2)

  graphics::points(.22, .70, pch=21, cex=1.36,
                   bg=colores_06C["D1_L0"], col="white")
  graphics::text(.235, .70, "D1_L0: one declaration, no latency",
                 adj=0, cex=.83)
  graphics::points(.53, .70, pch=21, cex=1.36,
                   bg=colores_06C["D2_L1"], col="white")
  graphics::text(.545, .70, "D2_L1: two declarations, with latency",
                 adj=0, cex=.83)

  graphics::segments(.22,.36,.25,.36,col=col_gris_06C,lwd=2)
  graphics::text(.258,.36,"Gray: shared with B (or B baseline)",adj=0,cex=.79)
  graphics::segments(.58,.36,.61,.36,col=col_rojo_06C,lwd=2.1)
  graphics::text(.618,.36,"Red: edge present in A/A adjusted, absent in B",adj=0,cex=.79)
}

# 6. Guardar versiones raster 300 dpi y PDF vectorial.
carpeta_06C <- file.path(proyecto, "04_resultados", "06A_ENTREGA_MANUSCRITO", "figuras")
dir.create(carpeta_06C, showWarnings = FALSE, recursive = TRUE)
archivo_png_06C <- file.path(carpeta_06C, "FIGURA_1_R_CIENTIFICA.png")
archivo_pdf_06C <- file.path(carpeta_06C, "FIGURA_1_R_CIENTIFICA.pdf")

crear_06C <- function() {
  op <- graphics::par(no.readonly = TRUE)
  on.exit(graphics::par(op), add=TRUE)
  graphics::layout(rbind(1:3, rep(4, 3)), heights=c(8.2,1.15))
  graphics::par(mar=c(.1,.1,.1,.1), xaxs="i", yaxs="i", family="sans")
  dibujar_panel_06C("A", "A  |  Crude C34")
  dibujar_panel_06C("B", "B  |  Without C34")
  dibujar_panel_06C("A_ajustada", "C  |  Age-adjusted C34")
  graphics::par(mar=c(.2,.1,.1,.1))
  dibujar_leyenda_06C()
}

grDevices::png(archivo_png_06C, width=5400, height=2250, res=300, bg="white")
crear_06C()
grDevices::dev.off()

grDevices::pdf(archivo_pdf_06C, width=18, height=7.5,
               bg="white", useDingbats=FALSE)
crear_06C()
grDevices::dev.off()

# 7. Auditoria simple: las tres redes siguen intactas.
cat("\nFIGURA GENERADA DIRECTAMENTE DESDE R\n")
for (nm in names(g_06C)) {
  cat(nm, ": ", igraph::vcount(g_06C[[nm]]), " nodos, ",
      igraph::ecount(g_06C[[nm]]), " enlaces; ",
      if (nm == "B") 0 else sum(!pares_06C(g_06C[[nm]])$clave %in% referencia_B_06C),
      " enlaces destacados frente a B\n", sep="")
}
cat("PNG: ", archivo_png_06C, "\nPDF: ", archivo_pdf_06C, "\n", sep="")

# ============================================================
# FIGURA 1 FINAL - TRES REDES TERRITORIALES
# Version compacta, mas limpia y estilo manuscrito
# Usa las redes ya calculadas en resultados_finales
# ============================================================

if (!requireNamespace("igraph", quietly = TRUE)) {
  stop("Se necesita el paquete 'igraph'.")
}

if (!exists("resultados_finales")) {
  stop("Debes tener cargado 'resultados_finales'.")
}

if (!exists("proyecto") || !dir.exists(proyecto)) {
  stop("No se encontro la carpeta 'proyecto'.")
}

library(igraph)

# ------------------------------------------------------------
# 1. Cargar redes y matriz
# ------------------------------------------------------------

redes <- resultados_finales$redes_historicas$redes
matriz <- resultados_finales$matriz_v2

extraer_grafo <- function(obj) {
  if (inherits(obj, "igraph")) return(obj)
  if (is.list(obj) && inherits(obj$grafo, "igraph")) return(obj$grafo)
  stop("No se encontro un grafo igraph.")
}

grafos <- lapply(redes[c("A", "B", "A_ajustada")], extraer_grafo)

stopifnot(
  all(c("A", "B", "A_ajustada") %in% names(grafos)),
  nrow(matriz) == 18L,
  "comuna" %in% names(matriz),
  "tiene_latente_num" %in% names(matriz)
)

# ------------------------------------------------------------
# 2. Colores y categorias regulatorias
# ------------------------------------------------------------

categoria <- ifelse(matriz$tiene_latente_num == 1, "D2_L1", "D1_L0")
names(categoria) <- as.character(matriz$comuna)

colores_nodos <- c(
  D1_L0 = "#C98A35",  # mostaza
  D2_L1 = "#2F7F99"   # azul petroleo
)

col_arista_base <- "#AEB7BF90"
col_arista_nueva <- "#D04A3A"

# ------------------------------------------------------------
# 3. Coordenadas manuales compactas
#    Estas coordenadas son ESQUEMATICAS
#    y solo buscan comparabilidad visual.
# ------------------------------------------------------------

coords <- data.frame(
  comuna = c(
    "San Pedro de la Paz", "Lota", "Calama", "Coronel", "Concepción",
    "Chiguayante", "Hualpén", "Penco", "Talcahuano", "Hualqui", "Tomé",
    "María Elena", "Maule", "Curicó", "Talca", "Tocopilla",
    "Valdivia", "Los Ángeles"
  ),
  x = c(
    0.53, 0.33, 0.70, 0.62, 0.49,
    0.76, 0.39, 0.64, 0.51, 0.44, 0.35,
    0.18, 0.83, 0.50, 0.35, 0.66,
    0.43, 0.58
  ),
  y = c(
    0.82, 0.73, 0.72, 0.67, 0.60,
    0.55, 0.56, 0.48, 0.53, 0.45, 0.42,
    0.28, 0.30, 0.20, 0.10, 0.16,
    0.06, 0.07
  ),
  stringsAsFactors = FALSE
)

rownames(coords) <- coords$comuna

# verificar coincidencia exacta de comunas
for (nm in names(grafos)) {
  nodos <- V(grafos[[nm]])$name
  stopifnot(length(nodos) == 18L, setequal(nodos, coords$comuna))
}

# ------------------------------------------------------------
# 4. Ajustes finos para etiquetas
# ------------------------------------------------------------

offsets <- data.frame(
  comuna = coords$comuna,
  dx = c(
    0.00, -0.01, 0.02, 0.02, -0.02,
    0.03, -0.04, 0.03, -0.03, 0.00, -0.03,
    0.00, 0.02, 0.00, -0.02, 0.03,
    -0.01, 0.03
  ),
  dy = c(
    0.03, 0.03, 0.02, 0.03, 0.02,
    0.00, 0.02, -0.03, -0.03, -0.03, -0.03,
    0.03, 0.03, 0.03, -0.03, 0.03,
    -0.03, -0.03
  ),
  stringsAsFactors = FALSE
)

rownames(offsets) <- offsets$comuna

# ------------------------------------------------------------
# 5. Funcion para obtener claves de enlaces
# ------------------------------------------------------------

clave_enlace <- function(a, b) {
  paste(sort(c(a, b)), collapse = "||")
}

tabla_aristas <- function(g) {
  ar <- igraph::as_data_frame(g, what = "edges")
  if (nrow(ar) == 0L) {
    return(data.frame(from = character(), to = character(), clave = character()))
  }
  ar$clave <- mapply(clave_enlace, ar$from, ar$to, USE.NAMES = FALSE)
  ar
}

claves_B <- tabla_aristas(grafos$B)$clave

# ------------------------------------------------------------
# 6. Funcion principal de dibujo
# ------------------------------------------------------------

dibujar_panel <- function(nombre, titulo) {
  
  g <- grafos[[nombre]]
  nodos <- V(g)$name
  ar <- tabla_aristas(g)
  
  plot.new()
  plot.window(xlim = c(0, 1), ylim = c(0, 1), asp = NA)
  
  # borde externo
  rect(0.01, 0.01, 0.99, 0.99, col = "white", border = "#C9D0D6", lwd = 1)
  
  # franja superior
  rect(0.01, 0.91, 0.99, 0.99, col = "#EEF2F5", border = NA)
  
  text(0.5, 0.95, titulo, cex = 1.05, font = 2, col = "#26333F")
  
  # aristas
  if (nrow(ar) > 0) {
    nuevas <- if (nombre == "B") {
      rep(FALSE, nrow(ar))
    } else {
      !(ar$clave %in% claves_B)
    }
    
    orden <- c(which(!nuevas), which(nuevas))
    
    for (k in orden) {
      p1 <- coords[ar$from[k], ]
      p2 <- coords[ar$to[k], ]
      
      segments(
        p1$x, p1$y, p2$x, p2$y,
        col = if (nuevas[k]) col_arista_nueva else col_arista_base,
        lwd = if (nuevas[k]) 2.4 else 1.2
      )
    }
  }
  
  # nodos
  puntos <- coords[nodos, ]
  
  points(
    puntos$x, puntos$y,
    pch = 21,
    cex = 1.55,
    bg = unname(colores_nodos[categoria[nodos]]),
    col = "white",
    lwd = 1.2
  )
  
  # etiquetas
  for (nm in nodos) {
    p <- coords[nm, ]
    o <- offsets[nm, ]
    text(
      p$x + o$dx, p$y + o$dy,
      labels = nm,
      cex = 0.82,
      font = 2,
      col = "#1B242C"
    )
  }
}

# ------------------------------------------------------------
# 7. Leyenda
# ------------------------------------------------------------

dibujar_leyenda <- function() {
  plot.new()
  plot.window(xlim = c(0, 1), ylim = c(0, 1))
  rect(0.01, 0.08, 0.99, 0.94, col = "#FAFBFC", border = "#C9D0D6", lwd = 1)
  
  text(0.03, 0.72, "Regulatory profile", adj = 0, cex = 1.0, font = 2, col = "#26333F")
  
  points(0.22, 0.72, pch = 21, cex = 1.55,
         bg = colores_nodos["D1_L0"], col = "white", lwd = 1.1)
  text(0.24, 0.72, "D1_L0: one declaration, no latency",
       adj = 0, cex = 0.86, col = "#26333F")
  
  points(0.55, 0.72, pch = 21, cex = 1.55,
         bg = colores_nodos["D2_L1"], col = "white", lwd = 1.1)
  text(0.57, 0.72, "D2_L1: two declarations, with latency",
       adj = 0, cex = 0.86, col = "#26333F")
  
  segments(0.22, 0.34, 0.27, 0.34, col = col_arista_base, lwd = 2.5)
  text(0.29, 0.34, "Gray: shared with B (baseline network)",
       adj = 0, cex = 0.82, col = "#26333F")
  
  segments(0.58, 0.34, 0.63, 0.34, col = col_arista_nueva, lwd = 2.7)
  text(0.65, 0.34, "Red: present in A or C, absent in B",
       adj = 0, cex = 0.82, col = "#26333F")
}

# ------------------------------------------------------------
# 8. Crear figura completa
# ------------------------------------------------------------

crear_figura <- function() {
  op <- par(no.readonly = TRUE)
  on.exit(par(op), add = TRUE)
  
  layout(matrix(c(1,2,3,4,4,4), nrow = 2, byrow = TRUE),
         heights = c(8.8, 1.5))
  
  par(mar = c(0.3, 0.3, 0.3, 0.3), xaxs = "i", yaxs = "i", family = "sans")
  
  dibujar_panel("A", "A | Crude C34")
  dibujar_panel("B", "B | Without C34")
  dibujar_panel("A_ajustada", "C | Age-adjusted C34")
  
  par(mar = c(0.1, 0.3, 0.1, 0.3))
  dibujar_leyenda()
}

# ------------------------------------------------------------
# 9. Exportar
# ------------------------------------------------------------

salida_dir <- file.path(
  proyecto, "04_resultados", "06A_ENTREGA_MANUSCRITO", "figuras"
)
dir.create(salida_dir, recursive = TRUE, showWarnings = FALSE)

archivo_png <- file.path(salida_dir, "FIGURA_1_R_CIENTIFICA_v2.png")
archivo_pdf <- file.path(salida_dir, "FIGURA_1_R_CIENTIFICA_v2.pdf")

png(archivo_png, width = 5400, height = 2300, res = 300, bg = "white")
crear_figura()
dev.off()

pdf(archivo_pdf, width = 18, height = 7.6, bg = "white", useDingbats = FALSE)
crear_figura()
dev.off()

# ------------------------------------------------------------
# 10. Confirmacion
# ------------------------------------------------------------

cat("\n=== FIGURA GENERADA ===\n")
cat("PNG:", archivo_png, "\n")
cat("PDF:", archivo_pdf, "\n\n")

for (nm in c("A", "B", "A_ajustada")) {
  g <- grafos[[nm]]
  ar <- tabla_aristas(g)
  n_rojas <- if (nm == "B") 0 else sum(!(ar$clave %in% claves_B))
  cat(
    nm, ": ",
    igraph::vcount(g), " nodos, ",
    igraph::ecount(g), " enlaces, ",
    n_rojas, " enlaces destacados frente a B\n",
    sep = ""
  )
}


# ============================================================
# FIGURA 1 - VERSION PAPER v3
# Redes A, B y A ajustada | 18 comunas chilenas
#
# - Utiliza grafos originales guardados
# - NO recalcula las redes
# - NO modifica indicadores ni resultados
# - Exporta PNG 300 dpi y PDF vectorial
# ============================================================

# ------------------------------------------------------------
# 1. COMPROBACIONES
# ------------------------------------------------------------

if (!requireNamespace("igraph", quietly = TRUE)) {
  stop("Debes instalar el paquete igraph.")
}

if (!exists("resultados_finales")) {
  stop("No existe el objeto resultados_finales.")
}

if (!exists("proyecto") || !dir.exists(proyecto)) {
  stop("No se encontro la carpeta proyecto.")
}

library(igraph)

redes <- resultados_finales$redes_historicas$redes
matriz <- resultados_finales$matriz_v2

extraer_grafo <- function(x) {
  if (inherits(x, "igraph")) return(x)
  
  if (is.list(x) && inherits(x$grafo, "igraph")) {
    return(x$grafo)
  }
  
  stop("No se encontro un objeto igraph.")
}

nombres_redes <- c("A", "B", "A_ajustada")

stopifnot(all(nombres_redes %in% names(redes)))

grafos <- lapply(
  redes[nombres_redes],
  extraer_grafo
)

stopifnot(
  all(vapply(grafos, igraph::vcount, numeric(1)) == 18),
  all(vapply(grafos, igraph::ecount, numeric(1)) == 61),
  all(c("comuna", "tiene_latente_num") %in% names(matriz))
)

# ------------------------------------------------------------
# 2. CATEGORIAS Y PALETA
# ------------------------------------------------------------

categoria <- ifelse(
  matriz$tiene_latente_num == 1,
  "D2_L1",
  "D1_L0"
)

names(categoria) <- as.character(matriz$comuna)

colores_nodos <- c(
  D1_L0 = "#C98A35",
  D2_L1 = "#2F7F99"
)

color_arista_base <- "#A4AFB9B8"
color_arista_nueva <- "#C8463A"

color_texto <- "#17212B"
color_borde <- "#D6DDE3"
color_cabecera <- "#F0F3F6"

# ------------------------------------------------------------
# 3. COORDENADAS COMUNES A LAS TRES REDES
# ------------------------------------------------------------
# Distribucion esquematica.
# NO representa ubicaciones ni distancias geograficas.

coords <- data.frame(
  comuna = c(
    "San Pedro de la Paz",
    "Lota",
    "Calama",
    "Coronel",
    "Concepción",
    "Chiguayante",
    "Hualpén",
    "Penco",
    "Talcahuano",
    "Hualqui",
    "Tomé",
    "María Elena",
    "Maule",
    "Curicó",
    "Talca",
    "Tocopilla",
    "Valdivia",
    "Los Ángeles"
  ),
  
  x = c(
    0.53, 0.33, 0.70, 0.62, 0.49,
    0.76, 0.39, 0.64, 0.51, 0.44,
    0.35, 0.18, 0.83, 0.50, 0.35,
    0.66, 0.43, 0.58
  ),
  
  y = c(
    0.82, 0.73, 0.72, 0.67, 0.60,
    0.55, 0.56, 0.48, 0.53, 0.45,
    0.42, 0.28, 0.30, 0.20, 0.10,
    0.16, 0.06, 0.07
  ),
  
  stringsAsFactors = FALSE
)

rownames(coords) <- coords$comuna

# Verificar nombres exactos y categorias
for (nm in nombres_redes) {
  nodos <- igraph::V(grafos[[nm]])$name
  
  stopifnot(
    length(nodos) == 18L,
    setequal(nodos, coords$comuna)
  )
}

stopifnot(
  !anyDuplicated(coords$comuna),
  !anyNA(categoria[coords$comuna])
)

# ------------------------------------------------------------
# 4. POSICIONES DE ETIQUETAS
# ------------------------------------------------------------

offsets <- data.frame(
  comuna = coords$comuna,
  
  dx = c(
    0.00, -0.01, 0.02, 0.02, -0.02,
    0.03, -0.04, 0.03, -0.03, 0.00,
    -0.03, 0.00, 0.02, 0.00, -0.02,
    0.03, -0.01, 0.03
  ),
  
  dy = c(
    0.035, 0.035, 0.025, 0.035, 0.025,
    0.000, 0.025, -0.035, -0.035,
    -0.035, -0.035, 0.035, 0.035,
    0.035, -0.035, 0.035, -0.035,
    -0.035
  ),
  
  stringsAsFactors = FALSE
)

rownames(offsets) <- offsets$comuna

# ------------------------------------------------------------
# 5. IDENTIFICACION DE ENLACES REALES
# ------------------------------------------------------------

clave_enlace <- function(a, b) {
  paste(sort(c(a, b)), collapse = "||")
}

tabla_aristas <- function(g) {
  
  ar <- igraph::as_data_frame(
    g,
    what = "edges"
  )
  
  if (nrow(ar) == 0) {
    return(data.frame(
      from = character(),
      to = character(),
      clave = character()
    ))
  }
  
  ar$clave <- mapply(
    clave_enlace,
    ar$from,
    ar$to,
    USE.NAMES = FALSE
  )
  
  ar
}

claves_B <- tabla_aristas(grafos$B)$clave

# ------------------------------------------------------------
# 6. FUNCION PARA DIBUJAR UN PANEL
# ------------------------------------------------------------

dibujar_panel <- function(nombre, titulo) {
  
  g <- grafos[[nombre]]
  nodos <- igraph::V(g)$name
  ar <- tabla_aristas(g)
  
  graphics::plot.new()
  
  graphics::plot.window(
    xlim = c(0, 1),
    ylim = c(0, 1),
    asp = NA
  )
  
  # Fondo y marco
  graphics::rect(
    0.01, 0.01, 0.99, 0.99,
    col = "white",
    border = color_borde,
    lwd = 1.2
  )
  
  # Encabezado
  graphics::rect(
    0.01, 0.91, 0.99, 0.99,
    col = color_cabecera,
    border = NA
  )
  
  graphics::text(
    0.50, 0.95,
    labels = titulo,
    cex = 1.30,
    font = 2,
    col = color_texto
  )
  
  # Clasificacion de enlaces
  nuevas <- if (nombre == "B") {
    rep(FALSE, nrow(ar))
  } else {
    !(ar$clave %in% claves_B)
  }
  
  # Dibujar primero conexiones grises
  # y despues enlaces diferenciales rojos.
  orden <- c(
    which(!nuevas),
    which(nuevas)
  )
  
  for (k in orden) {
    
    p1 <- coords[as.character(ar$from[k]), ]
    p2 <- coords[as.character(ar$to[k]), ]
    
    graphics::segments(
      x0 = p1$x,
      y0 = p1$y,
      x1 = p2$x,
      y1 = p2$y,
      
      col = if (nuevas[k]) {
        color_arista_nueva
      } else {
        color_arista_base
      },
      
      lwd = if (nuevas[k]) 3.4 else 1.8
    )
  }
  
  # Nodos mas grandes y con contorno
  puntos <- coords[nodos, ]
  
  graphics::points(
    puntos$x,
    puntos$y,
    
    pch = 21,
    cex = 2.50,
    
    bg = unname(
      colores_nodos[categoria[nodos]]
    ),
    
    col = "#263746",
    lwd = 1.15
  )
  
  # Etiquetas
  for (nm in nodos) {
    
    p <- coords[nm, ]
    o <- offsets[nm, ]
    
    graphics::text(
      x = p$x + o$dx,
      y = p$y + o$dy,
      labels = nm,
      
      cex = 0.98,
      font = 2,
      col = color_texto
    )
  }
}

# ------------------------------------------------------------
# 7. LEYENDA COMPARTIDA
# ------------------------------------------------------------

dibujar_leyenda <- function() {
  
  graphics::plot.new()
  
  graphics::plot.window(
    xlim = c(0, 1),
    ylim = c(0, 1)
  )
  
  graphics::rect(
    0.01, 0.08, 0.99, 0.94,
    col = "#FAFBFC",
    border = color_borde,
    lwd = 1.1
  )
  
  graphics::text(
    0.035, 0.72,
    "Regulatory profile:",
    adj = 0,
    cex = 1.08,
    font = 2,
    col = color_texto
  )
  
  # Categoria D1
  graphics::points(
    0.22, 0.72,
    pch = 21,
    cex = 2.0,
    bg = colores_nodos["D1_L0"],
    col = "#263746"
  )
  
  graphics::text(
    0.24, 0.72,
    "D1_L0: one declaration, no latency",
    adj = 0,
    cex = 0.94,
    col = color_texto
  )
  
  # Categoria D2
  graphics::points(
    0.55, 0.72,
    pch = 21,
    cex = 2.0,
    bg = colores_nodos["D2_L1"],
    col = "#263746"
  )
  
  graphics::text(
    0.57, 0.72,
    "D2_L1: two declarations, with latency",
    adj = 0,
    cex = 0.94,
    col = color_texto
  )
  
  # Conexion compartida
  graphics::segments(
    0.22, 0.34,
    0.27, 0.34,
    col = color_arista_base,
    lwd = 3.0
  )
  
  graphics::text(
    0.29, 0.34,
    "Gray: shared with B (or B baseline)",
    adj = 0,
    cex = 0.90,
    col = color_texto
  )
  
  # Conexion diferencial
  graphics::segments(
    0.58, 0.34,
    0.63, 0.34,
    col = color_arista_nueva,
    lwd = 3.5
  )
  
  graphics::text(
    0.65, 0.34,
    "Red: present in A or C, absent in B",
    adj = 0,
    cex = 0.90,
    col = color_texto
  )
}

# ------------------------------------------------------------
# 8. COMPOSICION DE FIGURA
# ------------------------------------------------------------

crear_figura <- function() {
  
  graphics::layout(
    matrix(
      c(1, 2, 3, 4, 4, 4),
      nrow = 2,
      byrow = TRUE
    ),
    heights = c(8.8, 1.5)
  )
  
  graphics::par(
    mar = c(0.3, 0.3, 0.3, 0.3),
    xaxs = "i",
    yaxs = "i",
    family = "sans"
  )
  
  dibujar_panel(
    "A",
    "A | Crude C34"
  )
  
  dibujar_panel(
    "B",
    "B | Without C34"
  )
  
  dibujar_panel(
    "A_ajustada",
    "C | Age-adjusted C34"
  )
  
  graphics::par(
    mar = c(0.1, 0.3, 0.1, 0.3)
  )
  
  dibujar_leyenda()
}

# ------------------------------------------------------------
# 9. EXPORTACION
# ------------------------------------------------------------

salida_dir <- file.path(
  proyecto,
  "04_resultados",
  "06A_ENTREGA_MANUSCRITO",
  "figuras"
)

dir.create(
  salida_dir,
  recursive = TRUE,
  showWarnings = FALSE
)

archivo_png <- file.path(
  salida_dir,
  "FIGURA_1_PAPER_v3.png"
)

archivo_pdf <- file.path(
  salida_dir,
  "FIGURA_1_PAPER_v3.pdf"
)

# PNG: 300 dpi
grDevices::png(
  filename = archivo_png,
  width = 5400,
  height = 2300,
  res = 300,
  bg = "white"
)

crear_figura()
grDevices::dev.off()

# PDF vectorial
grDevices::pdf(
  file = archivo_pdf,
  width = 18,
  height = 7.6,
  bg = "white",
  useDingbats = FALSE
)

crear_figura()
grDevices::dev.off()

# ------------------------------------------------------------
# 10. AUDITORIA
# ------------------------------------------------------------

cat("\n=== FIGURA PAPER v3 GENERADA ===\n")

for (nm in nombres_redes) {
  
  g <- grafos[[nm]]
  ar <- tabla_aristas(g)
  
  nuevas <- if (nm == "B") {
    0
  } else {
    sum(!(ar$clave %in% claves_B))
  }
  
  cat(
    nm, ": ",
    igraph::vcount(g), " nodos, ",
    igraph::ecount(g), " enlaces, ",
    nuevas, " enlaces destacados\n",
    sep = ""
  )
}

cat("\nPNG: ", archivo_png, "\n", sep = "")
cat("PDF: ", archivo_pdf, "\n", sep = "")

# Abrir la nueva figura en Windows
if (.Platform$OS.type == "windows") {
  shell.exec(archivo_png)
}

cat("\nPROCESO TERMINADO CORRECTAMENTE\n")


# PASO 3 - LOCALIZAR EL METODO DE AJUSTE POR EDAD

carpeta_scripts <- file.path(proyecto, "02_scripts")

if (!dir.exists(carpeta_scripts)) {
  carpeta_scripts <- proyecto
}

archivos <- list.files(
  carpeta_scripts,
  pattern = "\\.(R|r|Rmd|rmd)$",
  recursive = TRUE,
  full.names = TRUE
)

cat("\n=== SCRIPTS ENCONTRADOS ===\n")
cat("Cantidad:", length(archivos), "\n")

patron <- paste(
  "01H_tasas_C34_ajustadas",
  "tasa_ajustada_100k",
  "estandariz",
  "ajuste.*edad",
  "poblacion.*estandar",
  "población.*estándar",
  sep = "|"
)

for (archivo in archivos) {
  
  lineas <- tryCatch(
    readLines(archivo, warn = FALSE, encoding = "UTF-8"),
    error = function(e) character()
  )
  
  coincidencias <- grep(
    patron,
    lineas,
    ignore.case = TRUE
  )
  
  if (length(coincidencias) > 0) {
    
    cat("\n=== ARCHIVO:", basename(archivo), "===\n")
    
    posiciones <- unique(unlist(lapply(
      head(coincidencias, 12),
      function(i) seq(
        max(1, i - 3),
        min(length(lineas), i + 4)
      )
    )))
    
    for (i in posiciones) {
      cat(sprintf("%4d: %s\n", i, lineas[i]))
    }
  }
}

cat("\n=== BUSQUEDA TERMINADA ===\n")


# Buscar scripts en toda la carpeta REDES,
# incluyendo subcarpetas del proyecto.

carpeta_raiz <- dirname(proyecto)

cat("\n=== CARPETA QUE REVISAREMOS ===\n")
print(carpeta_raiz)

archivos_R <- list.files(
  path = carpeta_raiz,
  pattern = "\\.(R|Rmd|qmd|Rhistory)$",
  recursive = TRUE,
  full.names = TRUE,
  ignore.case = TRUE
)

cat("\n=== TOTAL DE SCRIPTS ENCONTRADOS ===\n")
print(length(archivos_R))

cat("\n=== ARCHIVOS RELACIONADOS CON C34 O TASAS ===\n")

candidatos <- archivos_R[
  grepl(
    "C34|tasa|ajust|edad|poblacion|red",
    basename(archivos_R),
    ignore.case = TRUE
  )
]

print(candidatos)

cat("\n=== BUSQUEDA DE CALCULO DE TASA AJUSTADA ===\n")

patron <- paste(
  "tasa_ajustada_100k",
  "personas_anio_ajustado",
  "estandariz",
  "standard",
  "pesos_edad",
  "poblacion_estandar",
  sep = "|"
)

for (archivo in archivos_R) {
  
  lineas <- tryCatch(
    readLines(archivo, warn = FALSE),
    error = function(e) character()
  )
  
  posiciones <- grep(
    patron,
    lineas,
    ignore.case = TRUE
  )
  
  if (length(posiciones) > 0) {
    
    cat("\nARCHIVO:", archivo, "\n")
    
    for (i in head(posiciones, 8)) {
      inicio <- max(1, i - 3)
      fin <- min(length(lineas), i + 5)
      
      cat(paste(
        sprintf("%d: %s",
                inicio:fin,
                lineas[inicio:fin]),
        collapse = "\n"
      ), "\n")
    }
  }
}

cat("\n=== REVISION TERMINADA ===\n")

