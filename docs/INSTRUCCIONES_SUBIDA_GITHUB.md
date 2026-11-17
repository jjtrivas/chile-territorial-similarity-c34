# Cómo cargar este material en GitHub

Repositorio destino: `https://github.com/jjtrivas/chile-territorial-similarity-c34`

1. Descomprimir `GITHUB_C34_CANDIDATO_VERIFICACION.zip` en Windows.
2. **ANTES DE SUBIR** abrir el archivo `.Rproj` en RStudio y ejecutar `source("scripts/03_prepublicacion.R")`. Solo continuar si aparece `LISTO_PARA_SUBIR`. Si falla, enviar la consola para corregirlo.
3. **Actualizar `README.md`**: en GitHub, entrar al `README.md` actual → lápiz (**Edit**) → pegar el contenido del `README.md` del paquete → **Commit changes**. Así se evita un conflicto con el README inicial que ya existe.
4. Regresar a la raíz del repositorio y pulsar **Add file → Upload files**.
5. Arrastrar desde el Explorador de archivos las **seis carpetas** `data`, `docs`, `figures`, `legacy`, `results`, `scripts`, más el archivo `chile-territorial-similarity-c34.Rproj` (y `.gitignore` cuando sea posible). **NO** subir `exports/` generado durante la prueba; es una salida local.
6. Esperar a que el navegador muestre la lista de archivos. Comprobar rutas `data/processed/...` y `scripts/00_validar_resultados.R`.
7. Escribir el comentario: `Add audited research outputs and processed-data reproducibility scripts` y pulsar **Commit changes**.
8. Comprobar que el README abre correctamente y que se ven las seis carpetas en la raíz.

**IMPORTANTE**: GitHub almacena directamente archivos y carpetas. Subir únicamente el ZIP crea un archivo ZIP en el repositorio **sin extraerlo**. Hay que descomprimirlo y cargar su contenido. GitHub acepta el arrastre de carpetas en la interfaz web.

No cargar el ZIP de revisión completo (`GITHUB_C34_REVISION_NO_PUBLICAR.rar`) ni las bases brutas con posibles identificadores.
