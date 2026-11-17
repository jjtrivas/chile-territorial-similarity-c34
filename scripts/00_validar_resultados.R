# Validation of archived processed results - no original files modified.
# Run from repository root (open chile-territorial-similarity-c34.Rproj).
if (!requireNamespace("igraph", quietly = TRUE)) {
  stop("Install igraph first: install.packages('igraph')")
}
stopifnot(file.exists("results/Tabla_tasas_C34.csv"))
stopifnot(file.exists("data/processed/03B_redes_demografia_historica.rds"))

tasas <- utils::read.csv("results/Tabla_tasas_C34.csv", check.names = FALSE)
correspondencia <- utils::read.csv("results/Tabla_correspondencia_C34.csv", check.names = FALSE)
estructura <- utils::read.csv("results/Tabla_estructura_redes.csv", check.names = FALSE)
obj <- readRDS("data/processed/03B_redes_demografia_historica.rds")

stopifnot(
  nrow(tasas) == 18L,
  sum(tasas$casos_c34) == 1864,
  abs(sum(tasas$personas_anio_original) - 14916395) < 0.5,
  nrow(correspondencia) == 6L,
  setequal(estructura$red, c("A", "B", "A_ajustada")),
  is.list(obj$redes)
)

nombres <- c("A", "B", "A_ajustada")
extraer <- function(nombre) {
  x <- obj$redes[[nombre]]
  if (inherits(x, "igraph")) return(x)
  if (is.list(x) && inherits(x$grafo, "igraph")) return(x$grafo)
  stop(paste("Missing igraph graph for", nombre))
}

resumen <- do.call(rbind, lapply(nombres, function(nombre) {
  g <- extraer(nombre)
  comp <- igraph::components(g)
  data.frame(
    red = nombre,
    nodos = igraph::vcount(g),
    enlaces = igraph::ecount(g),
    componentes = comp$no,
    aislados = sum(igraph::degree(g) == 0),
    mayor_componente = max(comp$csize)
  )
}))

esperado <- estructura[match(nombres, estructura$red),
                       c("red", "nodos", "enlaces", "componentes", "aislados", "mayor_componente")]
rownames(esperado) <- NULL
rownames(resumen) <- NULL
stopifnot(identical(resumen$red, esperado$red))
for (variable in setdiff(names(esperado), "red")) {
  stopifnot(all(resumen[[variable]] == esperado[[variable]]))
}
stopifnot(all(resumen$nodos == 18), all(resumen$enlaces == 61))
stopifnot(all(resumen$componentes == 4), all(resumen$aislados == 2))
stopifnot(nrow(readRDS("data/processed/03A_matriz_V2_candidata.rds")) == 18L)

cat("\nAUDITORIA DE OBJETOS DERIVADOS:  CONTROLES SUPERADOS\n")
print(resumen, row.names = FALSE)
cat("Se validaron tablas y grafos guardados; extracción desde fuentes originales todavía pendiente.\n")
