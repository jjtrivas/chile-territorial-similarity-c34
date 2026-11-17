# ============================================================
# BLOQUE 6A - EXPORTACION FINAL PARA MANUSCRITO
# Recupera objetos RDS ya procesados.
# Genera figuras, matrices y tablas.
# NO modifica las fuentes ni recalcula los modelos.
# ============================================================

stopifnot(
  exists("proyecto"),
  dir.exists(proyecto),
  exists("resultados_finales"),
  requireNamespace("igraph", quietly = TRUE)
)

library(igraph)
set.seed(1234)

# ------------------------------------------------------------
# 1. CARPETAS DE SALIDA
# ------------------------------------------------------------

salida <- file.path(
  proyecto, "04_resultados", "06A_ENTREGA_MANUSCRITO"
)
figuras_dir <- file.path(salida, "figuras")

dir.create(figuras_dir, recursive = TRUE,
           showWarnings = FALSE)

cat("\n=== 6A: INICIO ===\n")

# ------------------------------------------------------------
# 2. CARGAR OBJETOS RECUPERADOS
# ------------------------------------------------------------

redes <- resultados_finales$redes_historicas$redes
matriz <- resultados_finales$matriz_v2
tasas <- resultados_finales$tasas
asociaciones <- resultados_finales$correspondencia

stopifnot(
  all(c("A", "B", "A_ajustada") %in% names(redes)),
  nrow(matriz) == 18L,
  nrow(tasas) == 18L
)

# ------------------------------------------------------------
# 3. EXTRAER GRAFOS Y MATRICES EXISTENTES
# ------------------------------------------------------------

extraer_grafo <- function(x) {
  if (inherits(x, "igraph")) return(x)
  if (is.list(x) && inherits(x$grafo, "igraph"))
    return(x$grafo)
  stop("No se encontro un grafo igraph.")
}

extraer_distancias <- function(x) {
  if (is.list(x) && is.matrix(x$distancias))
    return(x$distancias)
  stop("No se encontro la matriz de distancias.")
}

grafos <- lapply(redes, extraer_grafo)
distancias <- lapply(redes, extraer_distancias)

comunas <- as.character(matriz$comuna)

stopifnot(
  length(comunas) == 18L,
  !anyDuplicated(comunas),
  all(vapply(grafos, igraph::vcount, numeric(1)) == 18),
  all(vapply(grafos, function(g)
    setequal(V(g)$name, comunas), logical(1)))
)

# ------------------------------------------------------------
# 4. COLORES CON SIGNIFICADO UNIFORME
# ------------------------------------------------------------

stopifnot("tiene_latente_num" %in% names(matriz))

grupo <- ifelse(
  matriz$tiene_latente_num == 1,
  "D2_L1", "D1_L0"
)
names(grupo) <- comunas

paleta <- c(
  D1_L0 = "#CC8D40",
  D2_L1 = "#397E99"
)

# ------------------------------------------------------------
# 5. POSICIONES IDENTICAS EN LAS TRES REDES
# ------------------------------------------------------------

gB <- grafos$B

pesosB <- E(gB)$similitud
if (is.null(pesosB)) pesosB <- E(gB)$weight
if (is.null(pesosB)) pesosB <- rep(1, ecount(gB))

stopifnot(
  length(pesosB) == ecount(gB),
  all(is.finite(pesosB)),
  all(pesosB > 0)
)

set.seed(1234)

layoutB <- layout_with_fr(
  gB, weights = pesosB, niter = 2000
)
rownames(layoutB) <- V(gB)$name

normalizar <- function(z) {
  r <- range(z)
  if (diff(r) == 0) return(rep(0, length(z)))
  2 * (z - r[1]) / diff(r) - 1
}

layoutB[, 1] <- normalizar(layoutB[, 1])
layoutB[, 2] <- normalizar(layoutB[, 2])

layoutB <- layoutB[comunas, , drop = FALSE]

# ------------------------------------------------------------
# 6. DIBUJAR REDES
# ------------------------------------------------------------

dibujar_red <- function(nombre) {
  
  g <- grafos[[nombre]]
  nodos <- V(g)$name
  
  coords <- layoutB[nodos, , drop = FALSE]
  
  # Para las redes A, destacar las conexiones
  # que no estan presentes en B.
  
  clave_enlaces <- function(gr) {
    ar <- as_data_frame(gr, what = "edges")
    if (nrow(ar) == 0L) return(character(0))
    vapply(seq_len(nrow(ar)), function(i) {
      paste(sort(c(ar$from[i], ar$to[i])),
            collapse = "|")
    }, character(1))
  }
  
  presentesB <- clave_enlaces(grafos$B)
  presentes <- clave_enlaces(g)
  
  color_enlaces <- rep("#AFB6BF90", ecount(g))
  
  if (nombre != "B") {
    color_enlaces[
      !(presentes %in% presentesB)
    ] <- "#C65C4B"
  }
  
  titulos <- c(
    A = "A: integracion con C34 crudo",
    B = "B: similitud territorial sin C34",
    A_ajustada = "A ajustada: C34 por edad"
  )
  
  plot(
    g,
    layout = coords,
    rescale = FALSE,
    xlim = c(-1.48, 1.48),
    ylim = c(-1.40, 1.40),
    vertex.size = 8,
    vertex.color = unname(paleta[grupo[nodos]]),
    vertex.frame.color = "white",
    vertex.label = nodos,
    vertex.label.cex = 0.67,
    vertex.label.color = "#222222",
    vertex.label.dist = 1.1,
    edge.color = color_enlaces,
    edge.width = 1.3,
    edge.curved = 0,
    main = titulos[[nombre]],
    margin = 0.05
  )
  
  legend(
    "bottomleft",
    legend = c(
      "D1_L0: una declaracion, sin latencia",
      "D2_L1: dos declaraciones, con latencia"
    ),
    col = unname(paleta),
    pch = 19,
    cex = 0.58,
    bty = "n"
  )
}

# ------------------------------------------------------------
# 7. FIGURAS INDIVIDUALES
# ------------------------------------------------------------

for (nombre in c("A", "B", "A_ajustada")) {
  
  archivo_png <- file.path(
    figuras_dir, paste0("Red_", nombre, ".png")
  )
  
  png(
    archivo_png,
    width = 3000,
    height = 2400,
    res = 270
  )
  
  dibujar_red(nombre)
  dev.off()
  
  archivo_pdf <- file.path(
    figuras_dir, paste0("Red_", nombre, ".pdf")
  )
  
  pdf(archivo_pdf, width = 11, height = 9)
  dibujar_red(nombre)
  dev.off()
}

# ------------------------------------------------------------
# 8. FIGURA COMPARATIVA
# ------------------------------------------------------------

png(
  file.path(figuras_dir, "Comparacion_A_B_Ajustada.png"),
  width = 4800, height = 1800, res = 200
)

par(mfrow = c(1, 3), mar = c(3, 1, 4, 1))

for (nombre in c("A", "B", "A_ajustada")) {
  dibujar_red(nombre)
}

dev.off()

pdf(
  file.path(figuras_dir, "Comparacion_A_B_Ajustada.pdf"),
  width = 18, height = 7
)

par(mfrow = c(1, 3), mar = c(3, 1, 4, 1))

for (nombre in c("A", "B", "A_ajustada")) {
  dibujar_red(nombre)
}

dev.off()

# ------------------------------------------------------------
# 9. EXPORTAR MATRICES DE DISTANCIAS Y SIMILITUD
# ------------------------------------------------------------

for (nombre in c("A", "B", "A_ajustada")) {
  
  D <- distancias[[nombre]]
  
  stopifnot(
    nrow(D) == 18L,
    ncol(D) == 18L,
    all(is.finite(D))
  )
  
  if (!is.null(rownames(D))) {
    D <- D[comunas, comunas, drop = FALSE]
  } else {
    stop("Faltan nombres de comunas en distancias.")
  }
  
  S <- 1 - D
  
  write.csv(
    D,
    file.path(salida, paste0("Distancia_", nombre, ".csv")),
    fileEncoding = "UTF-8"
  )
  
  write.csv(
    S,
    file.path(salida, paste0("Similitud_", nombre, ".csv")),
    fileEncoding = "UTF-8"
  )
}

# ------------------------------------------------------------
# 10. TABLA DE ESTRUCTURA DE REDES
# ------------------------------------------------------------

resumen_redes <- do.call(rbind, lapply(
  c("A", "B", "A_ajustada"),
  function(nombre) {
    
    g <- grafos[[nombre]]
    comp <- components(g)
    
    pesos <- E(g)$similitud
    if (is.null(pesos)) pesos <- E(g)$weight
    if (is.null(pesos)) pesos <- rep(1, ecount(g))
    
    modularidad_val <- NA_real_
    
    if (ecount(g) > 0) {
      comunidad <- cluster_louvain(
        g, weights = pesos
      )
      modularidad_val <- modularity(comunidad)
    }
    
    data.frame(
      red = nombre,
      nodos = vcount(g),
      enlaces = ecount(g),
      componentes = comp$no,
      aislados = sum(degree(g) == 0),
      mayor_componente = max(comp$csize),
      modularidad_descriptiva = modularidad_val
    )
  }
))

write.csv(
  resumen_redes,
  file.path(salida, "Tabla_estructura_redes.csv"),
  row.names = FALSE,
  fileEncoding = "UTF-8"
)

# ------------------------------------------------------------
# 11. EXPORTAR TABLAS PREVIAS SIN ALTERAR
# ------------------------------------------------------------

write.csv(
  asociaciones,
  file.path(salida, "Tabla_correspondencia_C34.csv"),
  row.names = FALSE,
  fileEncoding = "UTF-8"
)

write.csv(
  tasas,
  file.path(salida, "Tabla_tasas_C34.csv"),
  row.names = FALSE,
  fileEncoding = "UTF-8"
)

# ------------------------------------------------------------
# 12. MANIFIESTO DE INSUMOS Y SESION
# ------------------------------------------------------------

fuentes <- file.path(
  proyecto,
  "03_datos_derivados",
  c(
    "01H_tasas_C34_ajustadas.rds",
    "03A_matriz_V2_candidata.rds",
    "03B_redes_demografia_historica.rds",
    "03E_redes_sensibilidad_social.rds",
    "04A_correspondencia_B_C34.rds",
    "04B_sensibilidad_comunas.rds",
    "04C_robustez_red_B.rds",
    "05A_perfiles_territoriales.rds"
  )
)

stopifnot(all(file.exists(fuentes)))

manifiesto <- data.frame(
  archivo = basename(fuentes),
  md5 = unname(tools::md5sum(fuentes))
)

write.csv(
  manifiesto,
  file.path(salida, "Manifiesto_MD5.csv"),
  row.names = FALSE
)

capture.output(
  sessionInfo(),
  file = file.path(salida, "sessionInfo.txt")
)

# ------------------------------------------------------------
# 13. CONTROL FINAL
# ------------------------------------------------------------

cat("\n=== 6A: TABLA ESTRUCTURAL ===\n")
print(resumen_redes, row.names = FALSE)

cat("\n=== 6A: ARCHIVOS GENERADOS ===\n")
print(list.files(salida, recursive = TRUE))

cat("\n=== 6A: COMPROBACION B ===\n")

stopifnot(
  resumen_redes$nodos[resumen_redes$red == "B"] == 18,
  resumen_redes$enlaces[resumen_redes$red == "B"] == 61
)

cat("\nBLOQUE 6A TERMINADO CORRECTAMENTE\n")


# ======================================================
# 6B - FIGURA COMPARATIVA LEGIBLE PARA MANUSCRITO
# Solo visualizacion: no modifica resultados
# ======================================================

stopifnot(
  exists("grafos"), exists("layoutB"),
  exists("grupo"), exists("paleta"),
  dir.exists(figuras_dir)
)

clave_enlaces <- function(g) {
  a <- igraph::as_data_frame(g, what = "edges")
  if (nrow(a) == 0) return(character(0))
  vapply(seq_len(nrow(a)), function(i) {
    paste(sort(c(a$from[i], a$to[i])), collapse = "|")
  }, character(1))
}

enlaces_B <- clave_enlaces(grafos$B)

dibujar_final <- function(nombre) {
  
  g <- grafos[[nombre]]
  nodos <- igraph::V(g)$name
  xy <- layoutB[nodos, , drop = FALSE]
  
  claves <- clave_enlaces(g)
  
  colores_aristas <- rep(
    grDevices::adjustcolor("#81909B", alpha.f = 0.40),
    igraph::ecount(g)
  )
  
  if (nombre != "B") {
    colores_aristas[!(claves %in% enlaces_B)] <-
      "#C65447"
  }
  
  titulos <- c(
    A = "A. Integracion con C34 crudo",
    B = "B. Similitud sin C34",
    A_ajustada = "C. Integracion con C34 ajustado"
  )
  
  plot(
    g,
    layout = xy,
    rescale = FALSE,
    xlim = range(layoutB[, 1]) + c(-0.25, 0.25),
    ylim = range(layoutB[, 2]) + c(-0.17, 0.17),
    vertex.size = 12,
    vertex.color = unname(paleta[grupo[nodos]]),
    vertex.frame.color = "white",
    vertex.label = nodos,
    vertex.label.cex = 0.95,
    vertex.label.dist = 1.4,
    vertex.label.color = "#202020",
    edge.color = colores_aristas,
    edge.width = 1.4,
    edge.curved = 0,
    main = titulos[[nombre]],
    margin = 0.08
  )
}

# Comparacion vertical: un panel por fila
archivo <- file.path(
  figuras_dir,
  "Figura_1_comparacion_vertical_FINAL.png"
)

png(archivo, width = 2400, height = 4200, res = 240)

par(mfrow = c(3, 1), mar = c(3, 4, 4, 4))

for (n in c("A", "B", "A_ajustada")) {
  dibujar_final(n)
}

dev.off()

# Version PDF vectorial
pdf(
  file.path(
    figuras_dir,
    "Figura_1_comparacion_vertical_FINAL.pdf"
  ),
  width = 10, height = 17.5
)

par(mfrow = c(3, 1), mar = c(3, 4, 4, 4))

for (n in c("A", "B", "A_ajustada")) {
  dibujar_final(n)
}

dev.off()

cat("\nFIGURAS VERTICALES GENERADAS\n")


# =================================================
# TABLA 1 - DATOS ONCOLOGICOS PARA EL MANUSCRITO
# Solo inspeccion de resultados ya calculados
# =================================================

cat("\n=== COLUMNAS C34 ===\n")
print(names(resultados_finales$c34))

cat("\n=== COLUMNAS TASAS ===\n")
print(names(resultados_finales$tasas))

cat("\n=== TABLA C34 COMPLETA ===\n")
print(
  resultados_finales$c34,
  row.names = FALSE,
  digits = 5
)

cat("\n=== TASAS AJUSTADAS COMPLETAS ===\n")
print(
  resultados_finales$tasas,
  row.names = FALSE,
  digits = 5
)

cat("\nDATOS TABLA 1 LISTOS\n")

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