# ============================================================
# 01_base_analitica_cobranzas.R
# Trabajo final del diplomado
# Construcción de base analítica de cobranzas mejorado
# ============================================================


# ESTRUCTURA DEL CÓDIGO
# 1. Importacion de librerias
# 2. Leer los archivos necesarios para el codigo
# 3. Limpiza de datos
# 4. 
# 5. 
# 6. 

# ============================================================
# 1. Importacion de librerias

suppressPackageStartupMessages({
  library(readxl)
  library(dplyr)
  library(lubridate)
  library(ggplot2)
  library(scales)
  library(readr)
  library(tibble)
})

# ============================================================
# 2. Leer los archivos
leer_archivo_tabular <- function(path) {
  ext <- tolower(tools::file_ext(path))
  if (ext %in% c("xlsx", "xls")) {
    readxl::read_excel(path)
  } else if (ext == "csv") {
    readr::read_csv(path, show_col_types = FALSE)
  } else {
    stop("Formato no soportado para: ", path)
  }
}

# ============================================================
#3. Limpiza de datos

reescala_segura <- function(x) {
  x <- ifelse(is.na(x), 0, x)
  if (length(unique(x)) <= 1) return(rep(0, length(x)))
  scales::rescale(x, to = c(0, 1))
}

# ============================================================


construir_base_cobranzas <- function(path_clientes,
                                     path_facturas,
                                     output_dir = "output",
                                     analysis_date = Sys.Date()) {

  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

  clientes <- leer_archivo_tabular(path_clientes)
  facturas <- leer_archivo_tabular(path_facturas)

  names(clientes) <- trimws(names(clientes))
  names(facturas) <- trimws(names(facturas))

  facturas <- facturas %>%
    mutate(
      Emisión     = suppressWarnings(lubridate::dmy(Emisión)),
      Vencimiento = suppressWarnings(lubridate::dmy(Vencimiento)),
      FechaPago   = suppressWarnings(lubridate::dmy(Pago))
    )

  facturas <- facturas %>%
    mutate(
      Estado = case_when(
        !is.na(FechaPago) ~ "Pagado",
        Vencimiento < analysis_date ~ "Vencido",
        TRUE ~ "Por vencer"
      ),
      Dias_Mora = if_else(
        Estado == "Vencido",
        as.numeric(analysis_date - Vencimiento),
        0
      )
    )

  if ("RUT Plataforma Sheriff" %in% names(clientes)) {
    clientes <- clientes %>%
      rename(Rut = `RUT Plataforma Sheriff`)
  }

  df_cobranza <- facturas %>%
    left_join(clientes, by = "Rut")

  resumen <- df_cobranza %>%
    group_by(Rut, Cliente = Cliente.x) %>%
    summarise(
      NumFacturas = n(),
      NumVencidas = sum(Estado == "Vencido"),
      MontoVencido = sum(if_else(Estado == "Vencido", Total, 0), na.rm = TRUE),
      DiasMoraProm = mean(Dias_Mora[Estado == "Vencido"], na.rm = TRUE),
      DiasMoraMax  = max(Dias_Mora[Estado == "Vencido"], na.rm = TRUE, default = 0),
      .groups = "drop"
    ) %>%
    mutate(
      DiasMoraProm = if_else(is.nan(DiasMoraProm), 0, DiasMoraProm)
    )

  resumen_scoring <- resumen %>%
    mutate(
      kpi_monto = reescala_segura(MontoVencido),
      kpi_mora  = reescala_segura(DiasMoraProm),
      kpi_venc  = reescala_segura(NumVencidas),
      score_riesgo = 0.50 * kpi_monto + 0.30 * kpi_mora + 0.20 * kpi_venc
    )

  q1 <- quantile(resumen_scoring$score_riesgo, 0.33, na.rm = TRUE)
  q2 <- quantile(resumen_scoring$score_riesgo, 0.66, na.rm = TRUE)

  resumen_scoring <- resumen_scoring %>%
    mutate(
      Riesgo = case_when(
        score_riesgo <= q1 ~ "Bajo",
        score_riesgo <= q2 ~ "Medio",
        TRUE ~ "Alto"
      )
    )

  top10_monto <- resumen_scoring %>%
    arrange(desc(MontoVencido)) %>%
    slice_head(n = 10)

  top10_vencidas <- resumen %>%
    arrange(desc(NumVencidas), desc(MontoVencido)) %>%
    slice_head(n = 10) %>%
    select(Rut, Cliente, NumFacturas, NumVencidas, MontoVencido, DiasMoraProm, DiasMoraMax)

  top10_mayor_riesgo <- resumen_scoring %>%
    arrange(desc(Riesgo == "Alto"), desc(MontoVencido), desc(NumVencidas)) %>%
    slice_head(n = 10) %>%
    select(Rut, Cliente, Riesgo, MontoVencido, NumVencidas, DiasMoraProm)

  top10_mas_facturas <- resumen %>%
    arrange(desc(NumFacturas), desc(MontoVencido)) %>%
    slice_head(n = 10) %>%
    select(Rut, Cliente, NumFacturas, NumVencidas, MontoVencido, DiasMoraProm)

  top10_unificado <- resumen_scoring %>%
    arrange(desc(Riesgo == "Alto"), desc(MontoVencido), desc(NumVencidas), desc(NumFacturas)) %>%
    slice_head(n = 10) %>%
    select(Rut, Cliente, Riesgo, MontoVencido, NumVencidas, NumFacturas, DiasMoraProm, DiasMoraMax)

  clientes_criticos <- resumen_scoring %>%
    filter(NumVencidas >= 3) %>%
    arrange(desc(Riesgo == "Alto"), desc(MontoVencido), desc(DiasMoraMax), desc(NumVencidas)) %>%
    select(Rut, Cliente, Riesgo, MontoVencido, NumVencidas, NumFacturas, DiasMoraProm, DiasMoraMax)

  rut_prioritarios <- bind_rows(
    clientes_criticos %>% select(Rut),
    top10_mayor_riesgo %>% select(Rut)
  ) %>%
    distinct()

  clientes_prioritarios <- resumen_scoring %>%
    semi_join(rut_prioritarios, by = "Rut") %>%
    select(
      Rut, Cliente, Riesgo, MontoVencido, NumVencidas,
      NumFacturas, DiasMoraProm, DiasMoraMax
    ) %>%
    arrange(
      desc(Riesgo == "Alto"),
      desc(Riesgo),
      desc(MontoVencido),
      desc(NumVencidas),
      desc(DiasMoraMax)
    ) %>%
    mutate(Prioridad = row_number())

  indicadores <- tibble(
    FechaEjecucion = as.character(analysis_date),
    Clientes = n_distinct(resumen_scoring$Rut),
    Facturas = nrow(df_cobranza),
    FacturasVencidas = sum(df_cobranza$Estado == "Vencido", na.rm = TRUE),
    MontoVencido = sum(if_else(df_cobranza$Estado == "Vencido", df_cobranza$Total, 0), na.rm = TRUE),
    DiasMoraPromedioCartera = mean(df_cobranza$Dias_Mora[df_cobranza$Estado == "Vencido"], na.rm = TRUE)
  )

  p <- top10_monto %>%
    ggplot(aes(
      x = reorder(Cliente, MontoVencido),
      y = MontoVencido,
      fill = Riesgo
    )) +
    geom_col() +
    coord_flip() +
    scale_y_continuous(
      labels = scales::label_number(
        big.mark = ".",
        decimal.mark = ",",
        accuracy = 1
      )
    ) +
    labs(
      title = "Top 10 clientes por monto vencido",
      x = "Cliente",
      y = "Monto vencido (CLP)"
    ) +
    theme_minimal()

  write.csv(resumen_scoring, file.path(output_dir, "resumen_scoring.csv"), row.names = FALSE)
  write.csv(resumen_scoring, file.path(output_dir, "resumen_cobranza.csv"), row.names = FALSE)
  write.csv(df_cobranza, file.path(output_dir, "base_cobranza.csv"), row.names = FALSE)
  write.csv(top10_vencidas, file.path(output_dir, "top10_facturas_vencidas.csv"), row.names = FALSE)
  write.csv(top10_mayor_riesgo, file.path(output_dir, "top10_mayor_riesgo.csv"), row.names = FALSE)
  write.csv(top10_mas_facturas, file.path(output_dir, "top10_mas_facturas.csv"), row.names = FALSE)
  write.csv(top10_unificado, file.path(output_dir, "top10_unificado.csv"), row.names = FALSE)
  write.csv(clientes_criticos, file.path(output_dir, "clientes_criticos_3mas_vencidas.csv"), row.names = FALSE)
  write.csv(clientes_prioritarios, file.path(output_dir, "clientes_prioritarios.csv"), row.names = FALSE)
  write.csv(indicadores, file.path(output_dir, "indicadores_cartera.csv"), row.names = FALSE)

  ggsave(
    filename = file.path(output_dir, "top10_monto_vencido.png"),
    plot = p,
    width = 9,
    height = 4,
    dpi = 150
  )

  invisible(list(
    base_cobranza = df_cobranza,
    resumen = resumen,
    resumen_scoring = resumen_scoring,
    top10_vencidas = top10_vencidas,
    top10_mayor_riesgo = top10_mayor_riesgo,
    top10_mas_facturas = top10_mas_facturas,
    top10_unificado = top10_unificado,
    clientes_criticos = clientes_criticos,
    clientes_prioritarios = clientes_prioritarios,
    indicadores = indicadores
  ))
}

# Ejemplo de uso:
# resultado_cobranzas <- construir_base_cobranzas(
#   path_clientes = "data/clientes.xlsx",
#   path_facturas = "data/facturas.xlsx",
#   output_dir = "output"
# )
