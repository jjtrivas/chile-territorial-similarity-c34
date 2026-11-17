# Scripts originales conservados en `legacy/`

Estos seis documentos se recibieron como material original del proyecto. Se mantienen como **historial**, no como scripts listos para ejecutarse de principio a fin:

- `01_tasas_C34_IC95.R`: bloques de extracción, auditoría RPC, tasas y estandarización por edad y continuación de análisis.
- `S1_Preparacion_matriz_18_comunas.R`: integración de fuentes de pobreza, población, salud y hacinamiento; depende de rutas y archivos locales.
- `AUDITORIA_01_modularidad.R`: auditorías y especificaciones exploratorias; sus resultados de modularidad inicial no deben sustituir los valores descriptivos del cierre A/B/A ajustada.
- `CodigoV_pararedes.R`: inicialización del proyecto, inventarios y gestión original del entorno.
- `Codigo_completo_creo.R`: bloque de exportación final desde objetos RDS ya calculados y código de figuras.
- `06C_FIGURA_CIENTIFICA_TRES_REDES.R`: variantes del dibujo; la v3 está extraída como script portátil en `scripts/02_figura_1_paper_v3.R`.

**No ejecutar `source()` sobre todos los archivos de `legacy/`:** incluyen `file.choose()`, rutas locales, sobrescrituras de objetos, pasos repetidos, o dependencias de una sesión RStudio preexistente. El pipeline de origen será consolidado sin cambiar el análisis científico.
