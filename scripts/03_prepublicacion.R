# GITHUB C34 - PRUEBA UNICA ANTES DE PUBLICAR
# Requiere R (version de referencia: 4.4.3) e igraph.
# Ejecutar desde la raiz del repositorio, sin objetos R preexistentes:
# source("scripts/03_prepublicacion.R")

local({
  if (!file.exists("README.md") || !dir.exists("data/processed")) {
    stop("Abre primero el archivo .Rproj del proyecto descomprimido.")
  }
  if (!requireNamespace("igraph", quietly = TRUE)) {
    stop("Falta igraph. Ejecuta install.packages('igraph') y vuelve a probar.")
  }
  cat("\n[1/5] Validando tablas y grafos guardados...\n")
  source("scripts/00_validar_resultados.R", local = TRUE)

  cat("\n[2/5] Verificando 8 archivos con MD5 original...\n")
  original <- utils::read.csv("docs/Manifiesto_MD5_original.csv",
                              stringsAsFactors = FALSE, check.names = FALSE)
  stopifnot(all(c("archivo", "md5") %in% names(original)))
  stopifnot(nrow(original) == 8L)
  rutas <- file.path("data/processed", original$archivo)
  stopifnot(all(file.exists(rutas)))
  md5 <- unname(tools::md5sum(rutas))
  stopifnot(all(tolower(md5) == tolower(original$md5)))

  cat("\n[3/5] Verificando los 6 contrastes C34...\n")
  c34 <- utils::read.csv("results/Tabla_correspondencia_C34.csv")
  stopifnot(nrow(c34) == 6L, all(c34$n_permutaciones == 1999L),
            all(!is.na(c34$p_BH_bilateral)),
            all(c34$p_BH_bilateral >= 0.05))

  cat("\n[4/5] Exportando redes y matrices a exports/...\n")
  source("scripts/01_exportar_redes.R", local = TRUE)
  esperados <- c("matriz_territorial_18_comunas.csv",
                paste0("aristas_red_", c("A","B","A_ajustada"), ".csv"),
                paste0("distancia_", c("A","B","A_ajustada"), ".csv"),
                paste0("similitud_", c("A","B","A_ajustada"), ".csv"))
  stopifnot(all(file.exists(file.path("exports", esperados))))

  cat("\n[5/5] Reproduciendo la figura en exports/figures/...\n")
  source("scripts/02_figura_1_paper_v3.R", local = TRUE)
  figuras <- file.path("exports", "figures",
                       c("FIGURA_1_PAPER_v3.png", "FIGURA_1_PAPER_v3.pdf"))
  stopifnot(all(file.exists(figuras)), all(file.info(figuras)$size > 3000))
  originales <- file.path("figures",
                          c("FIGURA_1_PAPER_v3.png", "FIGURA_1_PAPER_v3.pdf"))
  stopifnot(all(file.exists(originales)))
  cat("\n==========================================\n")
  cat("LISTO_PARA_SUBIR - PRUEBA LOCAL SUPERADA\n")
  cat("Se comprobaron RDS, tablas, grafos, matrices y figura.\n")
  cat("Reproduccion desde fuentes brutas: TODAVIA PENDIENTE.\n")
  cat("==========================================\n")
})
