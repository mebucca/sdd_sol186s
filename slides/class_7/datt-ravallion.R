# Aplicación Shiny para la Visualización de Pobreza con una Distribución de Ingresos
# Control directo para manipular la media y distribución de ingresos

library(shiny)
library(ggplot2)
library(dplyr)
library(scales)
library(shinythemes)

# Colores personalizados
primary_color = "#3A4B8D"

# Tema personalizado para los gráficos
theme_statplot <- function(base_size = 30, 
                           base_family = "",
                           primary_color = primary_color,
                           bar_alpha = 0.8) {
  
  theme_minimal(base_size = base_size, base_family = base_family) + 
    theme(
      panel.background = element_rect(color = "black", fill = NA, size = 1),
      panel.grid.major = element_line(color = "grey90", size = 0.2),
      panel.grid.minor = element_line(color = "grey95", size = 0.1),
      
      axis.line = element_line(color = "black", size = 0.5),
      axis.ticks = element_line(color = "black", size = 0.5),
      axis.title = element_text(face = "bold", size = rel(1.1)),
      axis.text = element_text(color = "black", size = rel(0.9)),
      
      legend.background = element_rect(fill = NA, color = NA),
      legend.key = element_rect(fill = NA, color = NA),
      legend.title = element_text(face = "bold"),
      
      plot.title = element_text(face = "bold", size = rel(1.3), hjust = 0),
      plot.subtitle = element_text(size = rel(1.1), hjust = 0),
      plot.caption = element_text(size = rel(0.8), hjust = 1),
      
      plot.margin = margin(10, 10, 10, 10)
    )
}

# Función para calcular la tasa de pobreza dado el ingreso medio, desviación estándar y línea de pobreza
# Usando distribución log-normal
calcular_pobreza_logn <- function(mu, sigma, linea_pobreza) {
  # Calcular la tasa de pobreza (proporción bajo la línea de pobreza)
  pobreza <- plnorm(linea_pobreza, meanlog = mu, sdlog = sigma)
  return(pobreza)
}

# Función para convertir entre Gini y sigma (parámetro de distribución log-normal)
gini_a_sigma <- function(gini) {
  sigma <- sqrt(2) * qnorm((gini + 1)/2)
  return(sigma)
}

sigma_a_gini <- function(sigma) {
  gini <- 2 * pnorm(sigma/sqrt(2)) - 1
  return(gini)
}

# Función para calcular sigma de la distribución log-normal dado ingreso medio y sd
calcular_sigma_desde_sd <- function(ingreso_medio, sd) {
  # Relación entre CV (coeficiente de variación) y sigma
  cv <- sd / ingreso_medio
  sigma <- sqrt(log(1 + cv^2))
  return(sigma)
}

# Función para calcular mu de la distribución log-normal dado ingreso medio y sigma
calcular_mu_desde_media <- function(ingreso_medio, sigma) {
  mu <- log(ingreso_medio) - sigma^2/2
  return(mu)
}

# Función para calcular SD desde sigma y media
calcular_sd_desde_sigma <- function(sigma, ingreso_medio) {
  sd <- ingreso_medio * sqrt(exp(sigma^2) - 1)
  return(sd)
}

# Datos realistas para Chile (aproximados a 2023-2024)
datos_chile <- list(
  ingreso_medio = 800000,   # Ingreso medio mensual circa 2022
  sd = 650000,              # Desviación estándar circa 2022
  linea_pobreza = 185000    # Línea de pobreza por persona circa 2022
)

# UI de la aplicación
ui <- fluidPage(
  theme = shinythemes::shinytheme("flatly"),
  titlePanel("Visualización de Pobreza y Distribución del Ingreso"),
  
  sidebarLayout(
    sidebarPanel(
      h4("Parámetros de la Distribución"),
      numericInput("ingreso_medio", "Ingreso medio mensual (CLP):", 
                   value = datos_chile$ingreso_medio, min = 100000, max = 2000000, step = 10000),
      numericInput("sd", "Desviación estándar (CLP):", 
                   value = datos_chile$sd, min = 50000, max = 1500000, step = 10000),
      textOutput("gini_output"),
      
      h4("Línea de Pobreza"),
      numericInput("linea_pobreza", "Línea de pobreza (CLP):", 
                   value = datos_chile$linea_pobreza, min = 50000, max = 500000, step = 5000),
      
      hr(),
      actionButton("reset", "Restablecer a valores de Chile", 
                   icon = icon("refresh"), 
                   style = "color: #fff; background-color: #3A4B8D"),
      
      hr(),
      h4("Información"),
      p("Esta aplicación visualiza cómo la distribución del ingreso y la línea de pobreza afectan la tasa de pobreza en una población."),
      p("Puedes ajustar el ingreso medio, la desviación estándar (que determina la desigualdad) y el nivel de la línea de pobreza para ver cómo estos parámetros influyen en la proporción de personas en situación de pobreza."),
      p("Los valores predeterminados son aproximaciones para Chile circa 2022.")
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Distribución del Ingreso", 
                 br(),
                 plotOutput("plot_distribution", height = "500px"),
                 br(),
                 tableOutput("tabla_resultados_pobreza"),
                 br(),
                 p("Este gráfico muestra la distribución del ingreso y la línea de pobreza. El área sombreada representa la proporción de la población en situación de pobreza.")
        ),
        tabPanel("Explicación", 
                 br(),
                 h3("Visualización de la Distribución del Ingreso y Pobreza"),
                 h4("Interpretación del gráfico de distribución:"),
                 p("El gráfico muestra la distribución de ingresos junto con la línea de pobreza (línea vertical roja). El área sombreada bajo la curva y a la izquierda de la línea de pobreza representa la proporción de la población que está en situación de pobreza."),
                 
                 h4("Parámetros que puedes modificar:"),
                 tags$ul(
                   tags$li(strong("Ingreso medio:"), " Representa el ingreso promedio en la economía. Aumentar este parámetro desplaza la distribución hacia la derecha, lo que tiende a reducir la pobreza."),
                   tags$li(strong("Desviación estándar:"), " Representa la dispersión o desigualdad en la distribución del ingreso. Una mayor desviación estándar implica mayor desigualdad, lo que generalmente aumenta la pobreza si el ingreso medio se mantiene constante."),
                   tags$li(strong("Línea de pobreza:"), " El umbral de ingreso por debajo del cual se considera que una persona está en situación de pobreza.")
                 ),
                 
                 h4("Relación entre Desviación Estándar y Coeficiente de Gini:"),
                 p("En esta aplicación, puedes manipular directamente la desviación estándar de los ingresos. El correspondiente coeficiente de Gini (una medida más común de desigualdad) se muestra automáticamente. Recuerda que:"),
                 tags$ul(
                   tags$li("Mayor desviación estándar → Mayor coeficiente de Gini → Mayor desigualdad"),
                   tags$li("Menor desviación estándar → Menor coeficiente de Gini → Menor desigualdad")
                 ),
                 
                 h4("Simulación con la distribución Log-Normal:"),
                 p("Esta aplicación utiliza la distribución log-normal para modelar los ingresos, lo cual es una práctica común en economía debido a que:"),
                 tags$ul(
                   tags$li("Los ingresos no pueden ser negativos"),
                   tags$li("La distribución de ingresos suele tener una cola larga hacia la derecha (personas con ingresos muy altos)"),
                   tags$li("Permite parametrizar fácilmente la media y la desigualdad")
                 )
        )
      )
    )
  )
)

# Server de la aplicación
server <- function(input, output, session) {
  
  # Calcular y mostrar el equivalente de Gini
  output$gini_output <- renderText({
    sigma <- calcular_sigma_desde_sd(input$ingreso_medio, input$sd)
    gini <- sigma_a_gini(sigma)
    paste("Coeficiente de Gini equivalente:", round(gini, 3))
  })
  
  # Botón para restablecer a valores predeterminados
  observeEvent(input$reset, {
    updateNumericInput(session, "ingreso_medio", value = datos_chile$ingreso_medio)
    updateNumericInput(session, "sd", value = datos_chile$sd)
    updateNumericInput(session, "linea_pobreza", value = datos_chile$linea_pobreza)
  })
  
  # Tabla de resultados de pobreza
  output$tabla_resultados_pobreza <- renderTable({
    # Calcular sigma y mu
    sigma <- calcular_sigma_desde_sd(input$ingreso_medio, input$sd)
    mu <- calcular_mu_desde_media(input$ingreso_medio, sigma)
    
    # Calcular tasa de pobreza
    pobreza <- calcular_pobreza_logn(mu, sigma, input$linea_pobreza)
    gini <- sigma_a_gini(sigma)
    
    # Crear tabla de resultados de pobreza
    data.frame(
      Medida = c(
        "Tasa de Pobreza (%)",
        "Ingreso Medio (CLP)",
        "Desviación Estándar (CLP)",
        "Coeficiente de Gini",
        "Línea de Pobreza (CLP)"
      ),
      Valor = c(
        round(pobreza * 100, 2),
        format(input$ingreso_medio, big.mark = "."),
        format(input$sd, big.mark = "."),
        round(gini, 3),
        format(input$linea_pobreza, big.mark = ".")
      )
    )
  }, align = 'lr', width = "100%", striped = TRUE, hover = TRUE, rownames = FALSE)
  
  # Gráfico de distribución de ingresos
  output$plot_distribution <- renderPlot({
    # Calcular parámetros de distribución log-normal
    sigma <- calcular_sigma_desde_sd(input$ingreso_medio, input$sd)
    mu <- calcular_mu_desde_media(input$ingreso_medio, sigma)
    gini <- sigma_a_gini(sigma)
    
    # Generar datos para la distribución log-normal
    max_ingreso <- input$ingreso_medio * 3
    x <- seq(0, max_ingreso, length.out = 1000)
    
    # Densidad
    density <- dlnorm(x, meanlog = mu, sdlog = sigma)
    
    # Calcular pobreza
    pobreza <- calcular_pobreza_logn(mu, sigma, input$linea_pobreza)
    
    # Combinar datos
    df <- data.frame(
      ingreso = x,
      densidad = density
    )
    
    # Crear el gráfico
    p <- ggplot(df, aes(x = ingreso, y = densidad)) +
      geom_line(size = 1.2, color = primary_color) +
      geom_vline(xintercept = input$linea_pobreza, linetype = "dashed", color = "red", size = 1) +
      annotate("text", x = input$linea_pobreza * 1.1, y = max(density) * 0.9, 
               label = "Línea de Pobreza", color = "red", hjust = 0, fontface = "bold", size = 5) +
      geom_area(data = subset(df, ingreso <= input$linea_pobreza),
                aes(x = ingreso, y = densidad), fill = primary_color, alpha = 0.5) +
      annotate("text", x = input$linea_pobreza * 0.5, 
               y = max(density[x <= input$linea_pobreza]) * 1.2, 
               label = paste0("Pobreza: ", round(pobreza*100, 1), "%"), 
               color = primary_color, fontface = "bold", size = 5) +
      scale_x_continuous(labels = scales::label_number(big.mark = ".", decimal.mark = ",")) +
      labs(
        title = "Distribución del Ingreso y Pobreza",
        subtitle = paste0("Media: $", format(input$ingreso_medio, big.mark = "."), 
                          " | SD: $", format(input$sd, big.mark = "."), 
                          " | Gini: ", round(gini, 2)),
        x = "Ingreso Mensual (CLP)",
        y = "Densidad",
        caption = paste0("Línea de Pobreza: CLP ", format(input$linea_pobreza, big.mark = "."))
      ) +
      theme_statplot(base_size = 18) +
      # Añadir línea vertical para la media
      geom_vline(xintercept = input$ingreso_medio, 
                 color = primary_color, linetype = "dotted", size = 0.8) +
      annotate("text", x = input$ingreso_medio * 1.05, y = max(density) * 0.7, 
               label = "Ingreso Medio", color = primary_color, hjust = 0, size = 4)
    
    # Determinar límites adecuados para el eje x
    xmax <- min(max_ingreso, input$ingreso_medio * 2.5)
    xmin <- max(0, input$linea_pobreza * 0.2)
    
    # Aplicar los límites
    p + coord_cartesian(xlim = c(xmin, xmax))
  })
}

# Iniciar la aplicación
shinyApp(ui = ui, server = server)