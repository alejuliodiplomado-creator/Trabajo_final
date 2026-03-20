# ============================================================
# 02_modelo_ann_riesgo_credito.R
# Trabajo final del diplomado
# Traducción a R del modelo ANN originalmente construido en Python
# ============================================================

# ESTRUCTURA DEL CÓDIGO
# 1. Importacion de librerias
# 2. Leer los archivos necesarios para el codigo
# 3. Metricas de clasificacion
# 4.Preparacion de datos para la ANN
# 5. Test / Train 
# 6. Construir red nueronal
# 7. Prediccion sobre toda la base

# ============================================================
# 1. Importacion de librerias

suppressPackageStartupMessages({
  library(readxl) #para leer archivos
  library(dplyr) #manipulacion de datos
  library(readr) #leer y escribir CSV, parsear numeros
  library(janitor) #limpiar nombres de columnas
  library(ggplot2) #graficos
  library(keras3) #construir y entrenar la red neuronal
  library(tibble) #crear tablas modernas
  library(writexl) #exportar a Excel
})

# ============================================================
# 2. Leer los archivos necesarios para el codigo
leer_archivo_modelo <- function(path) {
  ext <- tolower(tools::file_ext(path))
  if (ext %in% c("xlsx", "xls")) {
    readxl::read_excel(path)
  } else if (ext == "csv") {
    readr::read_csv(path, show_col_types = FALSE)
  } else {
    stop("Formato no soportado: ", path)
  }
}

# ============================================================
# 3. Metricas de clasificacion

#divisiones:

div_segura <- function(num, den) { #division
  out <- num / den
  out[is.infinite(out)] <- NA_real_ #Si la división produce infinito, lo reemplaza por NA
  out
}
# Metricas de clasificacion
#y_true: valores reales
#y_pred: valores predichos

metricas_clasificacion <- function(y_true, y_pred) {
  tp <- sum(y_true == 1 & y_pred == 1, na.rm = TRUE) #verdaderos positivos 1
  tn <- sum(y_true == 0 & y_pred == 0, na.rm = TRUE) #verdaderps negativos 0
  fp <- sum(y_true == 0 & y_pred == 1, na.rm = TRUE) #falsos positivos
  fn <- sum(y_true == 1 & y_pred == 0, na.rm = TRUE) #falsos negativos

  accuracy  <- (tp + tn) / (tp + tn + fp + fn) #para saber que % clasifico bien
  precision <- ifelse((tp + fp) == 0, 0, tp / (tp + fp)) # para conocer la presicion de la clasificacion
  recall    <- ifelse((tp + fn) == 0, 0, tp / (tp + fn)) #sensibilidad (de los 1 reales cuentos detecto)
  specificity <- ifelse((tn + fp) == 0, 0, tn / (tn + fp)) #Especificidad (de los 0 reales cuantos detecto)
  f1 <- ifelse((precision + recall) == 0, 0, 2 * precision * recall / (precision + recall)) #equilibrio entre precisión y recall.

#devuelve los resultados en tabla
  tibble(
    accuracy = accuracy,
    precision = precision,
    recall = recall,
    specificity = specificity,
    f1 = f1,
    TP = tp,
    TN = tn,
    FP = fp,
    FN = fn
  )
}

#division entrenamiento/prueba
particion_estratificada <- function(y, prop_train = 0.80, seed = 42) { #semilla para que los resultados pueda reproducirse
  set.seed(seed)
  idx_0 <- which(y == 0) #indices de la clase 0 y de la clase 1.
  idx_1 <- which(y == 1)

  if (length(idx_0) == 0 || length(idx_1) == 0) {
    stop("La variable objetivo no contiene ambas clases.")
  }

  train_0 <- sample(idx_0, size = max(1, floor(length(idx_0) * prop_train)))
  train_1 <- sample(idx_1, size = max(1, floor(length(idx_1) * prop_train)))

  train_idx <- sort(c(train_0, train_1))
  test_idx  <- setdiff(seq_along(y), train_idx)

  list(train = train_idx, test = test_idx)
}

# ============================================================
# 4. Preparacion de datos para la ANN

preparar_datos_ann <- function(path_datos) {
  df <- leer_archivo_modelo(path_datos) |> #Leer y limpiar nombres de columnas
    janitor::clean_names()

  columnas_requeridas <- c(
    "razon_social",
    "rut",
    "activos_corrientes",
    "pasivos_corrientes",
    "deuda_total",
    "activos_totales",
    "utilidad_neta",
    "ebit",
    "gastos_intereses",
    "ingresos_ventas",
    "ventas_pasadas",
    "dias_mora_promedio"
  )

#si faltan datos se para la ejecucion 
  faltantes <- setdiff(columnas_requeridas, names(df))
  if (length(faltantes) > 0) {
    stop("Faltan columnas requeridas: ", paste(faltantes, collapse = ", "))
  }

  columnas_numericas <- c(
    "activos_corrientes",
    "pasivos_corrientes",
    "deuda_total",
    "activos_totales",
    "utilidad_neta",
    "ebit",
    "gastos_intereses",
    "ingresos_ventas",
    "ventas_pasadas",
    "dias_mora_promedio"
  )

#Conversión de texto a número
  for (col in columnas_numericas) {
    df[[col]] <- readr::parse_number(
      as.character(df[[col]]),
      locale = readr::locale(decimal_mark = ",", grouping_mark = ".")
    )
  }

#elimina filas con NA
  df <- df |>
    filter(if_all(all_of(columnas_numericas), ~ !is.na(.x))) |>
    mutate( # ratios
      ratio_corriente = div_segura(activos_corrientes, pasivos_corrientes),
      deuda_activos = div_segura(deuda_total, activos_totales),
      roa = div_segura(utilidad_neta, activos_totales),
      ebit_intereses = div_segura(ebit, gastos_intereses),
      ventas = ingresos_ventas,
      crecimiento_ventas = div_segura(ingresos_ventas - ventas_pasadas, ventas_pasadas)
    ) |>
    mutate( #si alguna variable sigue teniendo infinitos, la reemplaza por NA
      across(
        c(ratio_corriente, deuda_activos, roa, ebit_intereses, ventas, crecimiento_ventas),
        ~ ifelse(is.infinite(.x), NA_real_, .x)
      )
    ) |>
    filter( #filtra filas validas y elimina filas con NA
      !is.na(ratio_corriente),
      !is.na(deuda_activos),
      !is.na(roa),
      !is.na(ebit_intereses),
      !is.na(ventas),
      !is.na(crecimiento_ventas)
    ) |>
# variable objetivo
#1 si dias_mora_promedio > 30
#0 en caso contrario
    mutate(
      default = if_else(dias_mora_promedio > 30, 1, 0)
    )

  df
}

ejecutar_modelo_ann <- function(path_datos,
                                output_dir = "output",
                                umbral = 0.5,
                                seed = 42) {

  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  keras3::set_random_seed(seed) #fija la semilla para que sea reproducible

  df <- preparar_datos_ann(path_datos)

  variables_modelo <- c(
    "ratio_corriente",
    "deuda_activos",
    "roa",
    "ebit_intereses",
    "ventas",
    "crecimiento_ventas"
  )

# Matriz X y vector Y
  X <- as.matrix(df[, variables_modelo]) #variables predictoras
  y <- df$default #variable objetivo

# ============================================================
# 5. Test / Train

  idx <- particion_estratificada(y, prop_train = 0.80, seed = seed)
#Separa los datos en entrenamiento y prueba.
  
  X_train <- X[idx$train, , drop = FALSE] 
  X_test  <- X[idx$test, , drop = FALSE]
  y_train <- y[idx$train]
  y_test  <- y[idx$test]

#Calcula media y desviacion estandar del set de entrenamiento
  centro <- apply(X_train, 2, mean, na.rm = TRUE)
  escala <- apply(X_train, 2, sd, na.rm = TRUE)
  escala[escala == 0] <- 1 #Si alguna desviacion es 0, la reemplaza por 1 para evitar division por cero.

  X_train_scaled <- scale(X_train, center = centro, scale = escala)
  X_test_scaled  <- scale(X_test, center = centro, scale = escala)
  X_total_scaled <- scale(X, center = centro, scale = escala)

 # ============================================================
# 6. Construir red nueronal
  
  modelo <- keras_model_sequential() |>
    layer_input(shape = ncol(X_train_scaled)) |>
    layer_dense(units = 16, activation = "relu") |> #Primera capa oculta: 16 neuronas, activacion ReLU.
    layer_dense(units = 8, activation = "relu") |> #Segunda capa oculta: 8 neuronas, activacion ReLU.
    layer_dense(units = 1, activation = "sigmoid") 
#Compilacion del modelo
   modelo |>
    compile(
      optimizer = "adam", #algoritmo de optimización
      loss = "binary_crossentropy", #función de pérdida para binaria
      metrics = c("accuracy") #métrica de seguimiento
    )

  early_stop <- callback_early_stopping( #si el modelo no mejora se detiene
    monitor = "val_loss",
    patience = 15L,
    restore_best_weights = TRUE
  )

# Entrenamiento
  historial <- modelo |>
    fit(
      x = X_train_scaled,
      y = y_train,
      validation_split = 0.20,
      epochs = 200,
      batch_size = 16,
      callbacks = list(early_stop),
      verbose = 1
    )

#guarda el historial de entrenamiento
  historial_df <- tibble(
    epoch = seq_along(historial$metrics$loss),
    loss = unlist(historial$metrics$loss),
    val_loss = unlist(historial$metrics$val_loss)
  )

#Grafico de peridida 
  grafico_loss <- ggplot(historial_df, aes(x = epoch)) +
    geom_line(aes(y = loss), linewidth = 0.8) + #entrenamiento
    geom_line(aes(y = val_loss), linewidth = 0.8, linetype = "dashed") + #validacion
    labs(
      title = "Evolución de la pérdida del modelo ANN",
      x = "Época",
      y = "Loss"
    ) +
    theme_minimal()
  
#guarda el grafico
  ggplot2::ggsave(
    filename = file.path(output_dir, "historial_entrenamiento_ann.png"),
    plot = grafico_loss,
    width = 9,
    height = 5,
    dpi = 200
  )

#Prediccion del Test
#si probabilidad ≥ 0.5 → default.
#si probabilidad < 0.5 → no default
  probabilidades_test <- as.numeric(modelo |> predict(X_test_scaled, verbose = 0))
  pred_test <- ifelse(probabilidades_test >= umbral, 1, 0) #Convierte esas probabilidades en clases 0/1 usando umbral 0.5.

  metricas <- metricas_clasificacion(y_test, pred_test)

# tabla que indica las clases reales y cuales de llas se predijeron como 0 o 1 
  matriz_confusion <- tibble(
    clase_real = c("0", "1"),
    pred_0 = c(sum(y_test == 0 & pred_test == 0), sum(y_test == 1 & pred_test == 0)),
    pred_1 = c(sum(y_test == 0 & pred_test == 1), sum(y_test == 1 & pred_test == 1))
  )

#resultado del test
  resultados_test <- as_tibble(X_test) |>
    setNames(variables_modelo) |>
    mutate(
      default_real = y_test,
      probabilidad_default = probabilidades_test,
      default_predicho = pred_test
    )

# ============================================================
# 7. Prediccion sobre toda la base
  
  prob_total <- as.numeric(modelo |> predict(X_total_scaled, verbose = 0))

  resultados_totales <- df |>
    mutate(
      probabilidad_default = prob_total,
      default_predicho = if_else(probabilidad_default >= umbral, 1, 0),
      nivel_riesgo = case_when( #clasificacion de riesgo por tramos
        probabilidad_default < 0.33 ~ "Bajo",
        probabilidad_default < 0.66 ~ "Medio",
        TRUE ~ "Alto"
      )
    )

# Exportacion de resultados
  readr::write_csv(metricas, file.path(output_dir, "metricas_modelo_ann.csv"))
  readr::write_csv(matriz_confusion, file.path(output_dir, "matriz_confusion.csv"))
  readr::write_csv(resultados_test, file.path(output_dir, "resultados_test_ann.csv"))
  readr::write_csv(resultados_totales, file.path(output_dir, "empresas_score_riesgo_ann.csv"))
  readr::write_csv(historial_df, file.path(output_dir, "historial_entrenamiento_ann.csv"))

  writexl::write_xlsx( #Exporta resultados del test en Excel.
    resultados_test,
    path = file.path(output_dir, "resultados_riesgo_credito_empresas.xlsx")
  )

  writexl::write_xlsx( #Exporta score total en Excel.
    resultados_totales,
    path = file.path(output_dir, "empresas_con_score_riesgo.xlsx")
  )

#Guarda el modelo entrenado para reutilizarlo después sin volver a entrenar
  save_model(modelo, file.path(output_dir, "modelo_ann_riesgo.keras"))

  saveRDS(
    list(
      center = centro,
      scale = escala,
      variables_modelo = variables_modelo,
      umbral = umbral
    ),
    file = file.path(output_dir, "parametros_preprocesamiento_ann.rds")
  )

  invisible(list(
    datos = df,
    metricas = metricas,
    matriz_confusion = matriz_confusion,
    resultados_test = resultados_test,
    resultados_totales = resultados_totales,
    modelo = modelo,
    historial = historial_df
  ))
}

