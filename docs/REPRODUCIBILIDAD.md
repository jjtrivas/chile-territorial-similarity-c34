# Estado de reproducibilidad

## Disponible en esta versión

1. Archivos `.rds` derivados recuperados del cierre analítico (11 archivos), con verificación de estructura y controles reportados por el autor mediante R.
2. Comprobaciones automáticas de 18 comunas, 1.864 casos C34, 14.916.395 personas-año, 6 contrastes y estructura de las tres redes.
3. Exportación de matrices de distancia y similitud y listas de enlaces desde los grafos guardados, sin volver a construirlos.
4. Regeneración de la figura científica v3 a partir de las tres redes guardadas y la matriz de indicadores.

## Pendiente para una reproducción **desde las fuentes originales**

- Registrar fuentes oficiales con nombre de archivo, URL exacta, fecha de descarga, versión y permiso de redistribución.
- Depurar y hacer portátiles las rutinas de lectura del RPC y proyecciones INE, sin supuestos de objetos guardados previamente en memoria.
- Reconstruir indicadores y tasas ajustadas desde los datos de origen.
- Reconstruir las tres redes, pruebas de sensibilidad, correlaciones por permutación y sus 6 ajustes BH, y contrastar tolerancias numéricas.
- Registrar todas las dependencias realmente usadas en un nuevo `renv.lock`. El lockfile original archivado está incompleto.
- Añadir un test integral que compare salidas reconstruidas con los artefactos de referencia.

## Cómo ejecutar la etapa reproducible actual

1. Instalar R y el paquete `igraph` (`install.packages("igraph")`).
2. Abrir `chile-territorial-similarity-c34.Rproj` desde RStudio. Esto establece el directorio del proyecto.
3. Ejecutar en una sesion limpia `source("scripts/03_prepublicacion.R")`, que llama a los scripts 00, 01 y 02 en orden.
4. Consultar `exports/` y `exports/figures/`; los originales de `figures/` se mantienen intactos.

Ninguno de los tres scripts elimina o sobrescribe los archivos `.rds` originales. El script 02 genera copias en `exports/figures/`, sin sobrescribir las figuras originales guardadas en `figures/`.

## Criterios de control

El uso de `stopifnot()` detiene la ejecución si un valor estructural clave no coincide. Superar estos controles **no** equivale a demostrar que el proceso completo de extracción y procesamiento de las fuentes públicas ya es reproducible.

## Estado de pruebas del paquete

Comprobaciones de archivo (ZIP, SHA-256, MD5 y CSV) realizadas. **Prueba en RStudio con R e igraph aún requerida**; no atribuir estado de ejecución validada al paquete hasta obtener `LISTO_PARA_SUBIR`.
