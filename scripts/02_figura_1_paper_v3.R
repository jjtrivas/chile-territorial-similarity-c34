# Adapted, standalone entry point for the author-approved Figure 1 v3 block.
# Original drawing code preserved below. Network graphs are read from saved RDS.
# Run from the repository root via the .Rproj file.
if (!file.exists("data/processed/03B_redes_demografia_historica.rds")) {
  stop("Open the repository as an RStudio project before sourcing this script.")
}
proyecto <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
resultados_finales <- list(
  redes_historicas = readRDS("data/processed/03B_redes_demografia_historica.rds"),
  matriz_v2 = readRDS("data/processed/03A_matriz_V2_candidata.rds")
)

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
  "exports",
  "figures"
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
