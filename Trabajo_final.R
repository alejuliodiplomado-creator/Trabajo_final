# ============================================================
# Trabajo final diplomado.
#Alejandra Julio R.

#El siguiente scrip contiene todo lo necesario para resolver los siguientes desafios y/o
#necesidades que tiene esta empresa fintech en el corto plazo. Estos son puntos que se han
#desarrollado a lo largo del diplomado y que ahora seran complidos y mejorados con la 
#retroalimentacion de los usuarios finales.

# Desafios:

# 1. Donstruir una base analitica de cobranzas, que permita identificar el estado de 
#las facturas, calcular dias de mora y gneerar un resumen de riesgo por cliente, a partir 
#de datos operaciones de facturacion y clientes

# 2. Desarrollar un mondelo de redes neuronales (ANN) el cual sera capaz de clasificar 
#empresas segun su nivel de riesgo de credito (evalucion de clientes).

#Ambos puntos estan orientados a las areas comercial y financiera para la toma de decisiones.

# ============================================================

# ============================================================
# Ejecuta esos dos archivos y carga en memoria las funciones que contienen.

source("01_base_analitica_cobranzas.R")
source("02_modelo_ann_riesgo_credito.R")

# ============================================================
# Parametros de entrada

ruta_clientes <- "data/clientes.xlsx"
ruta_facturas <- "data/facturas.xlsx"
ruta_finanzas <- "data/datos_completados.xlsx"
carpeta_salida <- "output"

# ============================================================
# Ejecucion componente cobranzas

resultado_cobranza <- construir_base_cobranzas(
  path_clientes = ruta_clientes,
  path_facturas = ruta_facturas,
  output_dir = carpeta_salida
)

# ============================================================
# Ejecución componente ANN

resultado_ann <- ejecutar_modelo_ann(
  path_datos = ruta_finanzas,
  output_dir = carpeta_salida
)

message("Proyecto ejecutado correctamente.")
source("01_base_analitica_cobranzas.R")
source("02_modelo_ann_riesgo_credito.R")

# ============================================================
# Parámetros de entrada

ruta_clientes <- "data/clientes.xlsx"
ruta_facturas <- "data/facturas.xlsx"
ruta_finanzas <- "data/datos_completados.xlsx"
carpeta_salida <- "output"

# ============================================================
# Ejecución componente cobranzas

resultado_cobranza <- construir_base_cobranzas(
  path_clientes = ruta_clientes,
  path_facturas = ruta_facturas,
  output_dir = carpeta_salida
)

# ============================================================
# Ejecución componente ANN

resultado_ann <- ejecutar_modelo_ann(
  path_datos = ruta_finanzas,
  output_dir = carpeta_salida
)

message("Proyecto ejecutado correctamente.")