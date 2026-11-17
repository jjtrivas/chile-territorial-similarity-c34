# Export documented networks, distance/similarity matrices and commune matrix.
# No reconstruction from raw source datasets is claimed here.
if (!requireNamespace("igraph", quietly = TRUE)) stop("Install igraph")
stopifnot(file.exists("data/processed/03B_redes_demografia_historica.rds"))
redes_obj <- readRDS("data/processed/03B_redes_demografia_historica.rds")
matriz <- readRDS("data/processed/03A_matriz_V2_candidata.rds")
stopifnot(is.list(redes_obj$redes), nrow(matriz) == 18L)
dir.create("exports", showWarnings = FALSE)

utils::write.csv(matriz, "exports/matriz_territorial_18_comunas.csv",
                 row.names = FALSE, fileEncoding = "UTF-8")

for (nombre in c("A", "B", "A_ajustada")) {
  objeto <- redes_obj$redes[[nombre]]
  g <- if (inherits(objeto, "igraph")) objeto else objeto$grafo
  stopifnot(inherits(g, "igraph"), igraph::vcount(g) == 18L)

  # Real edges from saved graph; not inferred from image or manuscript.
  aristas <- igraph::as_data_frame(g, what = "edges")
  utils::write.csv(aristas,
                   file.path("exports", paste0("aristas_red_", nombre, ".csv")),
                   row.names = FALSE, fileEncoding = "UTF-8")

  d <- objeto$distancias
  if (!is.matrix(d)) stop(paste("Missing distance matrix for", nombre))
  stopifnot(nrow(d) == 18L, ncol(d) == 18L, all(is.finite(d)))
  utils::write.csv(d, file.path("exports", paste0("distancia_", nombre, ".csv")),
                   fileEncoding = "UTF-8")
  utils::write.csv(1 - d,
                   file.path("exports", paste0("similitud_", nombre, ".csv")),
                   fileEncoding = "UTF-8")
}
cat("\nEXPORTACION COMPLETADA en exports/\n")
