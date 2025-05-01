library(shiny)
library(shinydashboard)
library(dplyr)
library(ggplot2)
library(plotly)
library(DT)
library(lubridate)
library(reshape2) # Para mapas de calor
library(viridis)  # Para esquemas de colores
library(tidyr)    # Añadido para la función spread()

# Función para leer el CSV (con manejo de codificación)
leer_datos <- function() {
  # Usar la misma configuración que se usó para crear el archivo en limpieza.R
  datos <- read.csv("BD_limpio.csv", fileEncoding = "UTF-8", na.strings = c("", "NA", "N/A"), check.names = FALSE)
  
  # Imprimir los primeros nombres de columnas para diagnóstico
  cat("Primeras columnas:", paste(head(colnames(datos), 10), collapse=", "), "\n")
  
  # Convertir fechas
  if("FECHA.DE.REPORTE" %in% colnames(datos)) {
    datos$FECHA.DE.REPORTE <- as.Date(as.character(datos$FECHA.DE.REPORTE), format = "%d/%m/%Y")
    cat("Muestra de FECHA.DE.REPORTE:", paste(head(datos$FECHA.DE.REPORTE, 5), collapse=", "), "\n")
  } else {
    cat("Columna FECHA.DE.REPORTE no encontrada\n")
  }
  
  if("FECHA..ingreso" %in% colnames(datos)) {
    # Convertir a character primero para manejar formatos mixtos
    datos$FECHA..ingreso <- as.character(datos$FECHA..ingreso)
    cat("Muestra de FECHA..ingreso:", paste(head(datos$FECHA..ingreso, 5), collapse=", "), "\n")
  }
  
  # Asegurar que tenemos columnas de año y mes para análisis temporales
  if("Año" %in% colnames(datos) && "Mes" %in% colnames(datos)) {
    # Asegurar que son numéricos
    datos$Año <- as.numeric(as.character(datos$Año))
    datos$Mes <- as.numeric(as.character(datos$Mes))
    # Crear una columna de fecha con el primer día del mes
    datos$fecha_mes <- as.Date(paste(datos$Año, datos$Mes, "01", sep = "-"))
    cat("Muestra de fecha_mes:", paste(head(datos$fecha_mes, 5), collapse=", "), "\n")
  } else {
    cat("Columnas Año o Mes no encontradas\n")
  }
  
  # Imprimir información de diagnóstico
  cat("Columnas disponibles:", paste(colnames(datos), collapse=", "), "\n")
  cat("Número de filas:", nrow(datos), "\n")
  
  # Comprobar valores NA en columnas importantes
  cat("NAs en FECHA.DE.REPORTE:", sum(is.na(datos$FECHA.DE.REPORTE)), "\n")
  cat("NAs en Año:", sum(is.na(datos$Año)), "\n")
  cat("NAs en Mes:", sum(is.na(datos$Mes)), "\n")
  
  return(datos)
}

# UI
ui <- dashboardPage(
  dashboardHeader(title = "Monitor de Datos en Tiempo Real"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
      menuItem("Visualizaciones", tabName = "visualizaciones", icon = icon("chart-bar")),
      menuItem("Datos", tabName = "datos", icon = icon("table"))
    ),
    
    # Filtros en el sidebar
    hr(),
    # Estado de carga de datos
    textOutput("estado_carga"),
    hr(),
    selectInput("variable_filtro", "Filtrar por:", choices = c("Todos")),
    sliderInput("intervalo_actualizacion", "Intervalo de actualización (seg):",
                min = 1, max = 30, value = 5)
  ),
  
  dashboardBody(
    # Mensajes de error/notificaciones
    tags$div(
      id = "mensajes",
      style = "padding: 10px; margin-bottom: 10px;"
    ),
    
    tabItems(
      # Panel Dashboard
      tabItem(
        tabName = "dashboard",
        fluidRow(
          box(plotlyOutput("grafico_tiempo_real"), width = 12, 
              title = "Monitoreo en Tiempo Real",
              status = "primary", solidHeader = TRUE)
        ),
        fluidRow(
          box(plotlyOutput("grafico_distribucion"), width = 6,
              title = "Distribución de Datos"),
          box(plotlyOutput("grafico_tendencia"), width = 6,
              title = "Tendencia"),
          box(plotlyOutput("gauge_indicador"), width = 4,
              title = "Indicador de Nivel Actual"),
          box(plotlyOutput("grafico_lineas_tiempo"), width = 8,
              title = "Evolución Temporal",
              status = "info", solidHeader = TRUE)
        )
      ),
      
      # Nuevo panel de visualizaciones
      tabItem(
        tabName = "visualizaciones",
        fluidRow(
          box(
            selectInput("variable_categorica", "Variable categórica:", choices = c("Seleccionar")),
            plotlyOutput("grafico_barras"),
            width = 6,
            title = "Gráfico de Barras"
          ),
          box(
            selectInput("variable_pastel", "Variable para gráfico de pastel:", choices = c("Seleccionar")),
            plotlyOutput("grafico_pastel"),
            width = 6,
            title = "Gráfico de Pastel"
          )
        ),
        fluidRow(
          box(
            selectInput("variable_mapa_calor_x", "Variable X:", choices = c("Seleccionar")),
            selectInput("variable_mapa_calor_y", "Variable Y:", choices = c("Seleccionar")),
            plotlyOutput("mapa_calor"),
            width = 12,
            title = "Mapa de Calor"
          )
        )
      ),
      
      # Panel de Datos
      tabItem(
        tabName = "datos",
        fluidRow(
          box(DTOutput("tabla_datos"), width = 12,
              title = "Datos en Tiempo Real")
        )
      )
    )
  )
)

# Server
server <- function(input, output, session) {
  # Datos reactivos
  datos_actuales <- reactiveVal(data.frame())
  datos_cargados <- reactiveVal(FALSE)
  valores_filtro <- reactiveVal(list())
  
  # Mensaje de estado
  output$estado_carga <- renderText({
    if(datos_cargados()) {
      paste("Datos cargados exitosamente:", nrow(datos_actuales()), "registros")
    } else {
      "Esperando carga de datos..."
    }
  })
  
  # Carga inicial de datos
  observeEvent(1, {
    tryCatch({
      datos <- leer_datos()
      if(nrow(datos) > 0) {
        datos_actuales(datos)
        datos_cargados(TRUE)
        showNotification("Datos cargados correctamente", type = "message")
      } else {
        showNotification("El archivo CSV no contiene datos", type = "error")
      }
    }, error = function(e) {
      message("Error al cargar datos: ", e$message)
      showNotification(paste("Error al cargar datos:", e$message), type = "error")
    })
  }, once = TRUE)
  
  # Actualización automática de datos
  observe({
    invalidateLater(input$intervalo_actualizacion * 1000)
    if(datos_cargados()) {
      tryCatch({
        nuevos_datos <- leer_datos()
        datos_actuales(nuevos_datos)
        
        # Actualizar automáticamente los valores filtrados si hay una variable seleccionada
        if(input$variable_filtro != "Todos") {
          # Obtener todos los valores únicos para esta variable
          variable <- input$variable_filtro
          
          if(variable %in% names(nuevos_datos)) {
            # Obtener valores únicos
            if(is.numeric(nuevos_datos[[variable]])) {
              # Para variables numéricas, tomar valores únicos ordenados
              valores_unicos <- sort(unique(na.omit(nuevos_datos[[variable]])))
              if(length(valores_unicos) > 100) {
                # Si hay muchos valores, tomar algunos representativos
                valores_unicos <- unique(round(seq(min(valores_unicos), max(valores_unicos), length.out = 100)))
              }
            } else {
              # Para variables categóricas, tomar todos los valores únicos
              valores_unicos <- sort(unique(na.omit(nuevos_datos[[variable]])))
            }
            
            # Agregar cada valor individualmente
            valores_nuevos <- list()
            for(i in seq_along(valores_unicos)) {
              valores_nuevos[[i]] <- valores_unicos[i]
            }
            
            # Actualizar los valores de filtro
            valores_filtro(valores_nuevos)
          }
        }
      }, error = function(e) {
        showNotification(paste("Error en actualización:", e$message), type = "error")
      })
    }
  })
  
  # Datos filtrados
  datos_filtrados <- reactive({
    req(datos_actuales())
    req(datos_cargados())
    
    datos <- datos_actuales()
    
    if (input$variable_filtro != "Todos" && length(valores_filtro()) > 0) {
      variable <- input$variable_filtro
      if (is.numeric(datos[[variable]])) {
        # Para filtros numéricos, usar min y max de los valores seleccionados
        min_val <- min(unlist(valores_filtro()), na.rm = TRUE)
        max_val <- max(unlist(valores_filtro()), na.rm = TRUE)
        datos <- datos %>%
          filter(.data[[variable]] >= min_val & 
                 .data[[variable]] <= max_val)
      } else {
        # Para filtros categóricos, usar valores seleccionados
        datos <- datos %>%
          filter(.data[[variable]] %in% unlist(valores_filtro()))
      }
    }
    
    return(datos)
  })
  
  # Gráfico en tiempo real
  output$grafico_tiempo_real <- renderPlotly({
    # Imprimir mensaje de depuración
    cat("Renderizando gráfico_tiempo_real\n")
    
    # Verificar si tenemos datos
    if(!datos_cargados()) {
      cat("Datos no cargados aún\n")
      return(plot_ly() %>% layout(title = "Esperando datos..."))
    }
    
    datos <- datos_filtrados()
    cat("Número de filas en datos_filtrados:", nrow(datos), "\n")
    
    if (nrow(datos) == 0) {
      cat("No hay datos disponibles para graficar\n")
      return(plot_ly() %>% layout(title = "No hay datos disponibles para mostrar"))
    }
    
    # Verificar si tenemos la columna de fecha
    if (!"FECHA.DE.REPORTE" %in% names(datos)) {
      cat("No se encontró la columna FECHA.DE.REPORTE\n")
      cat("Columnas disponibles:", paste(names(datos), collapse=", "), "\n")
      return(plot_ly() %>% layout(title = "No se encontró la columna de fecha"))
    }
    
    # Verificar si hay fechas válidas
    fechas_validas <- !is.na(datos$FECHA.DE.REPORTE)
    if (sum(fechas_validas) == 0) {
      cat("No hay fechas válidas en FECHA.DE.REPORTE\n")
      return(plot_ly() %>% layout(title = "No hay fechas válidas para mostrar"))
    }
    
    # Imprimir algunas fechas para verificar
    cat("Muestra de fechas:", paste(head(datos$FECHA.DE.REPORTE, 5), collapse=", "), "\n")
    
    # Agrupar por fecha y contar
    datos_agrupados <- datos %>%
      group_by(FECHA.DE.REPORTE) %>%
      summarise(Cantidad = n()) %>%
      arrange(FECHA.DE.REPORTE)
    
    cat("Datos agrupados creados con", nrow(datos_agrupados), "filas\n")
    if (nrow(datos_agrupados) > 0) {
      cat("Primera fecha:", min(datos_agrupados$FECHA.DE.REPORTE), 
          "Última fecha:", max(datos_agrupados$FECHA.DE.REPORTE), "\n")
      cat("Muestra de cantidades:", paste(head(datos_agrupados$Cantidad, 5), collapse=", "), "\n")
    }
    
    # Crear el gráfico
    if(nrow(datos_agrupados) > 0) {
      p <- plot_ly(datos_agrupados, x = ~FECHA.DE.REPORTE, y = ~Cantidad, 
              type = 'scatter', mode = 'lines+markers')
      
      return(p %>% 
        layout(
          title = "Evolución Temporal",
          xaxis = list(title = "Fecha"),
          yaxis = list(title = "Cantidad"),
          hovermode = "closest"
        ) %>%
        config(displayModeBar = TRUE))
    } else {
      return(plot_ly() %>% layout(title = "No hay datos agrupados para mostrar"))
    }
  })
  
  # Gráfico de distribución
  output$grafico_distribucion <- renderPlotly({
    # Imprimir mensaje de depuración
    cat("Renderizando grafico_distribucion\n")
    
    # Verificar si tenemos datos
    if(!datos_cargados()) {
      cat("Datos no cargados aún para grafico_distribucion\n")
      return(plot_ly() %>% layout(title = "Esperando datos..."))
    }
    
    datos <- datos_filtrados()
    cat("Número de filas en datos_filtrados para distribucion:", nrow(datos), "\n")
    
    if (nrow(datos) == 0) {
      cat("No hay datos disponibles para graficar distribución\n")
      return(plot_ly() %>% layout(title = "No hay datos disponibles para mostrar"))
    }
    
    # Buscar la primera columna numérica que no sea una fecha
    cols_numericas <- names(datos)[sapply(datos, is.numeric)]
    cols_numericas <- cols_numericas[!grepl("fecha|año|mes", cols_numericas, ignore.case = TRUE)]
    
    cat("Columnas numéricas disponibles:", paste(cols_numericas, collapse=", "), "\n")
    
    if(length(cols_numericas) > 0) {
      variable_numerica <- cols_numericas[1]
      cat("Usando variable numérica:", variable_numerica, "\n")
      
      # Verificar si hay valores válidos
      valores_validos <- !is.na(datos[[variable_numerica]])
      cat("Número de valores válidos:", sum(valores_validos), "\n")
      
      if(sum(valores_validos) > 0) {
        # Imprimir algunos valores para verificar
        cat("Muestra de valores:", paste(head(datos[[variable_numerica]][valores_validos], 5), collapse=", "), "\n")
        
        # Crear el histograma directamente con plotly
        p <- plot_ly(x = datos[[variable_numerica]][valores_validos], type = "histogram") %>%
          layout(
            title = paste("Distribución de", variable_numerica),
            xaxis = list(title = variable_numerica),
            yaxis = list(title = "Frecuencia")
          )
        
        return(p)
      } else {
        cat("No hay valores válidos para la variable", variable_numerica, "\n")
        return(plot_ly() %>% layout(title = paste("No hay valores válidos para", variable_numerica)))
      }
    } else {
      cat("No hay variables numéricas disponibles\n")
      return(plot_ly() %>% layout(title = "No hay variables numéricas disponibles para mostrar"))
    }
  })
  
  # Gráfico de tendencia
  output$grafico_tendencia <- renderPlotly({
    # Imprimir mensaje de depuración
    cat("Renderizando grafico_tendencia\n")
    
    # Verificar si tenemos datos
    if(!datos_cargados()) {
      cat("Datos no cargados aún para grafico_tendencia\n")
      return(plot_ly() %>% layout(title = "Esperando datos..."))
    }
    
    datos <- datos_filtrados()
    cat("Número de filas en datos_filtrados para tendencia:", nrow(datos), "\n")
    
    if (nrow(datos) == 0) {
      cat("No hay datos disponibles para graficar tendencia\n")
      return(plot_ly() %>% layout(title = "No hay datos disponibles para mostrar"))
    }
    
    # Verificar si tenemos la columna de fecha
    if (!"FECHA.DE.REPORTE" %in% names(datos)) {
      cat("No se encontró la columna FECHA.DE.REPORTE para tendencia\n")
      cat("Columnas disponibles:", paste(names(datos), collapse=", "), "\n")
      return(plot_ly() %>% layout(title = "No se encontró la columna de fecha"))
    }
    
    # Verificar si hay fechas válidas
    fechas_validas <- !is.na(datos$FECHA.DE.REPORTE)
    if (sum(fechas_validas) == 0) {
      cat("No hay fechas válidas en FECHA.DE.REPORTE para tendencia\n")
      return(plot_ly() %>% layout(title = "No hay fechas válidas para mostrar"))
    }
    
    # Agrupar por fecha y contar
    datos_tendencia <- datos %>%
      group_by(FECHA.DE.REPORTE) %>%
      summarise(Cantidad = n()) %>%
      arrange(FECHA.DE.REPORTE)
    
    cat("Datos agrupados creados con", nrow(datos_tendencia), "filas para tendencia\n")
    
    if(nrow(datos_tendencia) > 1) {
      tryCatch({
        # Convertir fechas a números para la regresión
        datos_tendencia$fecha_num <- as.numeric(datos_tendencia$FECHA.DE.REPORTE)
        
        # Ajustar modelo de regresión lineal
        modelo <- lm(Cantidad ~ fecha_num, data = datos_tendencia)
        
        # Predecir valores
        datos_tendencia$Tendencia <- predict(modelo)
        
        cat("Modelo de tendencia ajustado correctamente\n")
        cat("Coeficientes:", paste(coef(modelo), collapse=", "), "\n")
        
        # Crear gráfico
        p <- plot_ly() %>%
          add_trace(data = datos_tendencia, 
                   x = ~FECHA.DE.REPORTE, 
                   y = ~Cantidad,
                   type = "scatter",
                   mode = "lines+markers",
                   name = "Datos reales", 
                   line = list(color = 'blue')) %>%
          add_trace(data = datos_tendencia, 
                   x = ~FECHA.DE.REPORTE, 
                   y = ~Tendencia,
                   type = "scatter",
                   mode = "lines",
                   name = "Tendencia", 
                   line = list(color = 'red', dash = 'dash'))
        
        return(p %>% layout(
          title = "Tendencia Temporal",
          xaxis = list(title = "Fecha"),
          yaxis = list(title = "Cantidad"),
          showlegend = TRUE
        ))
      }, error = function(e) {
        cat("Error al calcular la tendencia:", e$message, "\n")
        return(plot_ly() %>% layout(title = paste("Error al calcular la tendencia:", e$message)))
      })
    } else {
      cat("Se necesitan más datos para mostrar la tendencia (", nrow(datos_tendencia), "puntos disponibles)\n")
      return(plot_ly() %>% layout(title = "Se necesitan más datos para mostrar la tendencia"))
    }
  })
  
  # Gráfico de indicador (gauge)
  output$gauge_indicador <- renderPlotly({
    # Imprimir mensaje de depuración
    cat("Renderizando gauge_indicador\n")
    
    # Verificar si tenemos datos
    if(!datos_cargados()) {
      cat("Datos no cargados aún para gauge_indicador\n")
      return(plot_ly() %>% layout(title = "Esperando datos..."))
    }
    
    datos <- datos_filtrados()
    cat("Número de filas en datos_filtrados para indicador:", nrow(datos), "\n")
    
    if (nrow(datos) == 0) {
      cat("No hay datos disponibles para el indicador\n")
      return(plot_ly() %>% layout(title = "No hay datos disponibles para mostrar"))
    }
    
    # Buscar una columna numérica para mostrar en el indicador
    cols_numericas <- names(datos)[sapply(datos, is.numeric)]
    cols_numericas <- cols_numericas[!grepl("fecha|año|mes|No\\.|FPS", cols_numericas, ignore.case = TRUE)]
    
    cat("Columnas numéricas disponibles para indicador:", paste(cols_numericas, collapse=", "), "\n")
    
    if(length(cols_numericas) > 0) {
      # Usar EDAD como ejemplo, o la primera columna numérica disponible
      if("EDAD" %in% cols_numericas) {
        variable_numerica <- "EDAD"
      } else {
        variable_numerica <- cols_numericas[1]
      }
      
      cat("Usando variable numérica para indicador:", variable_numerica, "\n")
      
      # Calcular valor medio actual y valores de referencia
      valores_validos <- !is.na(datos[[variable_numerica]])
      if(sum(valores_validos) > 0) {
        valor_actual <- mean(datos[[variable_numerica]][valores_validos], na.rm = TRUE)
        valor_min <- min(datos[[variable_numerica]][valores_validos], na.rm = TRUE)
        valor_max <- max(datos[[variable_numerica]][valores_validos], na.rm = TRUE)
        
        # Crear el gráfico de indicador (gauge)
        p <- plot_ly(
          type = "indicator",
          mode = "gauge+number",
          value = valor_actual,
          title = {list(text = variable_numerica)},
          gauge = list(
            axis = list(range = list(valor_min, valor_max)),
            bar = list(color = "royalblue"),
            steps = list(
              list(range = c(valor_min, valor_min + (valor_max - valor_min)/3), color = "lightgray"),
              list(range = c(valor_min + (valor_max - valor_min)/3, valor_min + 2*(valor_max - valor_min)/3), color = "gray"),
              list(range = c(valor_min + 2*(valor_max - valor_min)/3, valor_max), color = "darkgray")
            ),
            threshold = list(
              line = list(color = "red", width = 4),
              thickness = 0.75,
              value = valor_actual
            )
          )
        )
        
        return(p)
      } else {
        cat("No hay valores válidos para la variable", variable_numerica, "\n")
        return(plot_ly() %>% layout(title = paste("No hay valores válidos para", variable_numerica)))
      }
    } else {
      cat("No hay variables numéricas disponibles para el indicador\n")
      return(plot_ly() %>% layout(title = "No hay variables numéricas disponibles para mostrar"))
    }
  })
  
  # Gráfico animado (reemplazado por un gráfico de líneas de tiempo)
  output$grafico_lineas_tiempo <- renderPlotly({
    # Imprimir mensaje de depuración
    cat("Renderizando grafico_lineas_tiempo\n")
    
    # Verificar si tenemos datos
    if(!datos_cargados()) {
      cat("Datos no cargados aún para grafico_lineas_tiempo\n")
      return(plot_ly() %>% layout(title = "Esperando datos..."))
    }
    
    datos <- datos_filtrados()
    cat("Número de filas en datos_filtrados para líneas de tiempo:", nrow(datos), "\n")
    
    if (nrow(datos) == 0) {
      cat("No hay datos disponibles para las líneas de tiempo\n")
      return(plot_ly() %>% layout(title = "No hay datos disponibles para mostrar"))
    }
    
    # Verificar si tenemos la columna de fecha
    if (!"FECHA.DE.REPORTE" %in% names(datos) && !(all(c("Año", "Mes") %in% names(datos)))) {
      cat("No se encontraron columnas de fecha necesarias para líneas de tiempo\n")
      return(plot_ly() %>% layout(title = "Faltan columnas de fecha necesarias"))
    }
    
    tryCatch({
      # Agrupar datos por fecha y otra variable categórica
      var_cat <- NULL
      for (col in c("TIPO.DE.Arbovirosis", "Diagnóstico", "Sexo", "Área.de.Salud", "Municipio")) {
        if (col %in% names(datos) && !all(is.na(datos[[col]]))) {
          var_cat <- col
          break
        }
      }
      
      if (is.null(var_cat)) {
        cat("No se encontró una variable categórica adecuada para líneas de tiempo\n")
        
        # Si no hay variable categórica, creamos una línea de tiempo general
        if ("FECHA.DE.REPORTE" %in% names(datos)) {
          datos_linea <- datos %>%
            filter(!is.na(FECHA.DE.REPORTE)) %>%
            group_by(FECHA.DE.REPORTE) %>%
            summarise(Cantidad = n(), .groups = "drop") %>%
            arrange(FECHA.DE.REPORTE)
          
          if (nrow(datos_linea) > 0) {
            p <- plot_ly() %>%
              add_trace(
                data = datos_linea,
                x = ~FECHA.DE.REPORTE,
                y = ~Cantidad,
                type = "scatter",
                mode = "lines+markers",
                name = "Total",
                line = list(color = "royalblue", width = 2)
              ) %>%
              layout(
                title = "Evolución temporal general",
                xaxis = list(title = "Fecha"),
                yaxis = list(title = "Cantidad"),
                showlegend = TRUE
              )
            
            return(p)
          } else {
            return(plot_ly() %>% layout(title = "No hay datos suficientes para líneas de tiempo"))
          }
        } else {
          # Usar Año y Mes para crear fecha
          datos_linea <- datos %>%
            filter(!is.na(Año), !is.na(Mes)) %>%
            mutate(Fecha = as.Date(paste(Año, Mes, "01", sep = "-"))) %>%
            group_by(Fecha) %>%
            summarise(Cantidad = n(), .groups = "drop") %>%
            arrange(Fecha)
          
          if (nrow(datos_linea) > 0) {
            p <- plot_ly() %>%
              add_trace(
                data = datos_linea,
                x = ~Fecha,
                y = ~Cantidad,
                type = "scatter",
                mode = "lines+markers",
                name = "Total",
                line = list(color = "royalblue", width = 2)
              ) %>%
              layout(
                title = "Evolución temporal general",
                xaxis = list(title = "Fecha"),
                yaxis = list(title = "Cantidad"),
                showlegend = TRUE
              )
            
            return(p)
          } else {
            return(plot_ly() %>% layout(title = "No hay datos suficientes para líneas de tiempo"))
          }
        }
      }
      
      cat("Usando variable categórica para líneas de tiempo:", var_cat, "\n")
      
      # Verificar que hay suficientes valores únicos no NA
      valores_unicos <- unique(na.omit(datos[[var_cat]]))
      if (length(valores_unicos) == 0) {
        cat("No hay valores categóricos válidos para", var_cat, "\n")
        return(plot_ly() %>% layout(title = paste("No hay valores válidos para", var_cat)))
      }
      
      # Obtener las categorías principales (máximo 5 para mantener el gráfico legible)
      if ("FECHA.DE.REPORTE" %in% names(datos)) {
        # Usar FECHA.DE.REPORTE directamente
        datos_categoria <- datos %>%
          filter(!is.na(.data[[var_cat]]), !is.na(FECHA.DE.REPORTE)) %>%
          group_by(.data[[var_cat]]) %>%
          summarise(Total = n(), .groups = "drop") %>%
          arrange(desc(Total))
        
        if (nrow(datos_categoria) == 0) {
          return(plot_ly() %>% layout(title = "No hay datos suficientes después de filtrar"))
        }
        
        # Seleccionar hasta 5 categorías principales
        top_categorias <- head(datos_categoria, 5)
        
        # Preparar datos para el gráfico
        datos_lineas <- datos %>%
          filter(!is.na(.data[[var_cat]]), !is.na(FECHA.DE.REPORTE), 
                 .data[[var_cat]] %in% top_categorias[[var_cat]]) %>%
          group_by(FECHA.DE.REPORTE, .data[[var_cat]]) %>%
          summarise(Cantidad = n(), .groups = "drop") %>%
          arrange(FECHA.DE.REPORTE)
        
      } else {
        # Usar Año y Mes para crear fecha
        datos_categoria <- datos %>%
          filter(!is.na(.data[[var_cat]]), !is.na(Año), !is.na(Mes)) %>%
          group_by(.data[[var_cat]]) %>%
          summarise(Total = n(), .groups = "drop") %>%
          arrange(desc(Total))
        
        if (nrow(datos_categoria) == 0) {
          return(plot_ly() %>% layout(title = "No hay datos suficientes después de filtrar"))
        }
        
        # Seleccionar hasta 5 categorías principales
        top_categorias <- head(datos_categoria, 5)
        
        # Preparar datos para el gráfico
        datos_lineas <- datos %>%
          filter(!is.na(.data[[var_cat]]), !is.na(Año), !is.na(Mes), 
                 .data[[var_cat]] %in% top_categorias[[var_cat]]) %>%
          mutate(Fecha = as.Date(paste(Año, Mes, "01", sep = "-"))) %>%
          group_by(Fecha, .data[[var_cat]]) %>%
          summarise(Cantidad = n(), .groups = "drop") %>%
          arrange(Fecha)
      }
      
      # Crear el gráfico interactivo
      if (nrow(datos_lineas) > 0) {
        # Verificar qué columna de fecha usar
        fecha_col <- if ("FECHA.DE.REPORTE" %in% names(datos_lineas)) "FECHA.DE.REPORTE" else "Fecha"
        
        # Iniciar el gráfico
        p <- plot_ly()
        
        # Añadir una línea por cada categoría
        for (cat in unique(datos_lineas[[var_cat]])) {
          datos_cat <- datos_lineas %>% 
            filter(.data[[var_cat]] == cat)
          
          if (nrow(datos_cat) > 0) {
            p <- p %>% add_trace(
              data = datos_cat,
              x = ~.data[[fecha_col]],
              y = ~Cantidad,
              type = "scatter",
              mode = "lines+markers",
              name = cat,
              hoverinfo = "text",
              text = ~paste(
                "Fecha:", format(.data[[fecha_col]], "%d-%m-%Y"), 
                "<br>", var_cat, ":", cat,
                "<br>Cantidad:", Cantidad
              )
            )
          }
        }
        
        # Configurar el diseño
        p <- p %>% layout(
          title = paste("Evolución temporal por", var_cat),
          xaxis = list(title = "Fecha"),
          yaxis = list(title = "Cantidad"),
          showlegend = TRUE,
          legend = list(orientation = "h", y = -0.2),
          hovermode = "closest"
        )
        
        return(p)
      } else {
        return(plot_ly() %>% layout(title = "No hay datos suficientes para crear líneas de tiempo"))
      }
    }, error = function(e) {
      cat("Error al crear el gráfico de líneas de tiempo:", e$message, "\n")
      return(plot_ly() %>% layout(title = paste("Error:", e$message)))
    })
  })
  
  # Tabla de datos
  output$tabla_datos <- renderDT({
    # Imprimir mensaje de depuración
    cat("Renderizando tabla_datos\n")
    
    # Verificar si tenemos datos
    if(!datos_cargados()) {
      cat("Datos no cargados aún para tabla_datos\n")
      return(datatable(data.frame(Mensaje = "Esperando datos..."),
                       options = list(searching = FALSE, paging = FALSE)))
    }
    
    datos <- datos_filtrados()
    cat("Número de filas en datos_filtrados para tabla:", nrow(datos), "\n")
    
    if (nrow(datos) == 0) {
      cat("No hay datos disponibles para mostrar en tabla\n")
      return(datatable(data.frame(Mensaje = "No hay datos disponibles"),
                       options = list(searching = FALSE, paging = FALSE)))
    }
    
    # Limitar a las primeras 20 columnas si hay muchas (para evitar sobrecarga)
    if (ncol(datos) > 20) {
      cat("Limitando a las primeras 20 columnas de", ncol(datos), "para la tabla\n")
      datos_tabla <- datos[, 1:20]
    } else {
      datos_tabla <- datos
    }
    
    # Mostrar las primeras 10 filas para optimizar renderizado
    return(datatable(
      datos_tabla,
      options = list(
        pageLength = 10,
        scrollX = TRUE,
        order = list(list(0, 'desc')),
        searching = TRUE
      ),
      rownames = FALSE,
      caption = paste("Mostrando", nrow(datos), "registros")
    ))
  })
  
  # Actualizar opciones de filtrado
  observe({
    req(datos_actuales())
    req(datos_cargados())
    
    datos <- datos_actuales()
    
    if(ncol(datos) > 0) {
      # Obtener nombres de columnas disponibles
      nombres_columnas <- names(datos)
      
      # Actualizar las opciones del filtro
      updateSelectInput(session, "variable_filtro", 
                       choices = c("Todos", nombres_columnas))
      
      # Variables categóricas (tomar columnas character o factor)
      cols_categoricas <- names(datos)[sapply(datos, function(x) is.character(x) || is.factor(x))]
      if(length(cols_categoricas) > 0) {
        updateSelectInput(session, "variable_categorica", 
                         choices = c("Seleccionar", cols_categoricas))
        updateSelectInput(session, "variable_pastel", 
                         choices = c("Seleccionar", cols_categoricas))
        updateSelectInput(session, "variable_mapa_calor_y", 
                         choices = c("Seleccionar", cols_categoricas))
      }
      
      # Variables numéricas para ejes X/Y y tamaño
      cols_numericas <- names(datos)[sapply(datos, is.numeric)]
      if(length(cols_numericas) > 0) {
        updateSelectInput(session, "variable_mapa_calor_x", 
                         choices = c("Seleccionar", cols_numericas))
      }
      
      # Mostrar mensaje de éxito
      if(length(nombres_columnas) > 0) {
        showNotification(paste("Filtros actualizados con", length(nombres_columnas), "columnas disponibles"), 
                        type = "message")
      }
    } else {
      showNotification("No hay columnas disponibles para filtrar", type = "warning")
    }
  })
  
  # Gráfico de barras para variables categóricas
  output$grafico_barras <- renderPlotly({
    req(input$variable_categorica != "Seleccionar")
    req(datos_filtrados())
    
    datos <- datos_filtrados()
    var_cat <- input$variable_categorica
    
    if(!var_cat %in% names(datos)) {
      return(plot_ly() %>% layout(title = "Variable no encontrada"))
    }
    
    # Eliminar NAs y contar frecuencias
    conteo <- datos %>%
      filter(!is.na(.data[[var_cat]])) %>%
      count(.data[[var_cat]], sort = TRUE, name = "Frecuencia")
    
    # Limitar a las 20 categorías más frecuentes si hay muchas
    if(nrow(conteo) > 20) {
      conteo <- head(conteo, 20)
    }
    
    # Crear gráfico de barras
    p <- plot_ly(
      data = conteo,
      x = ~get(var_cat),
      y = ~Frecuencia,
      type = "bar",
      marker = list(color = "skyblue")
    ) %>%
    layout(
      title = paste("Distribución de", var_cat),
      xaxis = list(title = var_cat, categoryorder = "total descending"),
      yaxis = list(title = "Frecuencia")
    )
    
    return(p)
  })
  
  # Gráfico de pastel
  output$grafico_pastel <- renderPlotly({
    req(input$variable_pastel != "Seleccionar")
    req(datos_filtrados())
    
    datos <- datos_filtrados()
    var_cat <- input$variable_pastel
    
    if(!var_cat %in% names(datos)) {
      return(plot_ly() %>% layout(title = "Variable no encontrada"))
    }
    
    # Eliminar NAs y contar frecuencias
    conteo <- datos %>%
      filter(!is.na(.data[[var_cat]])) %>%
      count(.data[[var_cat]], sort = TRUE, name = "Frecuencia")
    
    # Limitar a las 10 categorías más frecuentes si hay muchas
    if(nrow(conteo) > 10) {
      otros <- conteo %>% 
        slice(11:n()) %>%
        summarise(!!var_cat := "Otros", Frecuencia = sum(Frecuencia))
      
      conteo <- bind_rows(conteo %>% slice(1:10), otros)
    }
    
    # Crear gráfico de pastel
    p <- plot_ly(
      data = conteo,
      labels = ~get(var_cat),
      values = ~Frecuencia,
      type = "pie",
      marker = list(colors = viridis(nrow(conteo)))
    ) %>%
    layout(
      title = paste("Distribución de", var_cat),
      showlegend = TRUE
    )
    
    return(p)
  })
  
  # Mapa de calor
  output$mapa_calor <- renderPlotly({
    req(input$variable_mapa_calor_x != "Seleccionar")
    req(input$variable_mapa_calor_y != "Seleccionar")
    req(datos_filtrados())
    
    datos <- datos_filtrados()
    var_x <- input$variable_mapa_calor_x
    var_y <- input$variable_mapa_calor_y
    
    if(!all(c(var_x, var_y) %in% names(datos))) {
      return(plot_ly() %>% layout(title = "Variables no encontradas"))
    }
    
    # Eliminar filas con NA en las variables seleccionadas
    datos_filtrados_na <- datos %>%
      filter(!is.na(.data[[var_x]]), !is.na(.data[[var_y]]))
    
    if(nrow(datos_filtrados_na) == 0) {
      return(plot_ly() %>% layout(title = "No hay datos válidos para estas variables"))
    }
    
    # Proceso según si la variable Y es categórica o numérica
    if(is.numeric(datos_filtrados_na[[var_x]]) && 
       (is.character(datos_filtrados_na[[var_y]]) || is.factor(datos_filtrados_na[[var_y]]))) {
      
      # Categorizar la variable X si es numérica, para crear un mapa de calor discreto
      if(length(unique(datos_filtrados_na[[var_x]])) > 20) {
        datos_filtrados_na$x_binned <- cut(datos_filtrados_na[[var_x]], 
                                       breaks = 10, 
                                       labels = seq(1, 10))
        var_x_uso <- "x_binned"
      } else {
        var_x_uso <- var_x
      }
      
      # Contar frecuencias para el mapa de calor
      datos_heatmap <- datos_filtrados_na %>%
        count(.data[[var_x_uso]], .data[[var_y]]) %>%
        spread(key = .data[[var_y]], value = n, fill = 0)
      
      # Convertir a formato largo para plotly
      datos_melt <- melt(as.data.frame(datos_heatmap), id.vars = var_x_uso)
      colnames(datos_melt) <- c("x", "y", "valor")
      
      # Crear mapa de calor
      p <- plot_ly(
        data = datos_melt,
        x = ~x,
        y = ~y,
        z = ~valor,
        type = "heatmap",
        colorscale = "Viridis"
      ) %>%
      layout(
        title = "Mapa de Calor",
        xaxis = list(title = var_x),
        yaxis = list(title = var_y)
      )
      
      return(p)
    } else {
      # Si ambas variables son numéricas, crear un mapa de densidad 2D
      p <- plot_ly(
        data = datos_filtrados_na,
        x = ~get(var_x),
        y = ~get(var_y),
        type = "histogram2dcontour",
        colorscale = "Viridis"
      ) %>%
      layout(
        title = "Mapa de Densidad 2D",
        xaxis = list(title = var_x),
        yaxis = list(title = var_y)
      )
      
      return(p)
    }
  })
}

# Ejecutar la aplicación
shinyApp(ui, server) 