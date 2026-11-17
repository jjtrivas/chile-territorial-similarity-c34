# Fuentes de datos y licencias

La investigación combina datos oficiales de Chile. La siguiente lista **no sustituye** una tabla de trazabilidad por archivo y fecha de descarga. La URL precisa y los términos de redistribución deben recuperarse del archivo/fuente original antes de ofrecer las bases crudas como descargas.

| Variable o conjunto | Institución o procedencia identificada | Archivo de trabajo conocido | URL/licencia | Estado |
|---|---|---|---|---|
| Registro poblacional de cáncer, C34 | MINSAL, registro poblacional de cáncer | `BD_RPC_1998-2019_ID.zip` | Por verificar | Solo derivados agregados |
| Población por comuna, edad y año | INE | `SalComUsuarios-XXTok.xls` y proyecciones históricas | Por verificar | Reutilización por verificar |
| Condición regulatoria zona saturada/latente | MMA y decretos | Matrices integradas y enlaces regulatorios | Por verificar | Revisión de decretos pendiente |
| Pobreza de ingresos y multidimensional | CASEN / institución pública responsable | `PLANILLA_Estimaciones...xlsx` | Por verificar | Origen por archivo pendiente |
| Hacinamiento de viviendas 2024 | Fuente censal oficial | `V3_N-de-dormitorios-hacinamiento-y-viv-con-mas-de-un-hogar.xlsx` | Por verificar | Versión por verificar |
| Establecimientos de salud (2026) | Registro público de establecimientos | `establecimientos_20260616.csv` | Por verificar | Fecha y versión por verificar |

**Advertencia:** el fichero RPC original citado en los scripts contiene campos como `id_persona`. Aunque el dataset se pueda obtener en una web pública, **no debe subirse automáticamente** al repositorio; revisar primero sus términos de uso y el contenido. Esta versión publica únicamente artefactos analíticos derivados, sin el archivo RPC bruto.

Los indicadores cubren periodos distintos. Documentar las ventanas RPC y los años específicos de los indicadores evita presentar la red como una serie temporal sincronizada.
