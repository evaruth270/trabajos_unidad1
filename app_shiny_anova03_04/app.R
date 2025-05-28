# Carga de librerías necesarias
library(shiny)
library(DT)

# Interfaz de usuario
ui <- navbarPage("App Estadística Interactiva",
                 
                 # 🟡 DATOS: subir/generar
                 tabPanel("Datos",
                          sidebarLayout(
                            sidebarPanel(
                              fileInput("archivo", "Sube tu archivo CSV"),
                              tags$hr(),
                              h4("Generar datos aleatorios"),
                              selectInput("distribucion", "Distribución:",
                                          choices = c("Normal", "Uniforme", "Exponencial")),
                              numericInput("tamano", "Tamaño de muestra:", value = 100, min = 10),
                              numericInput("semilla", "Semilla (opcional):", value = 123),
                              radioButtons("muestreo", "Muestreo:",
                                           choices = c("Con reemplazo" = "con", "Sin reemplazo" = "sin")),
                              actionButton("generar", "Generar datos")
                            ),
                            mainPanel(
                              h4("Vista previa de los datos"),
                              DT::dataTableOutput("tabla_datos"),
                              plotOutput("histograma"),
                              tags$hr(),
                              h4("🔍 Recomendación estadística"),
                              verbatimTextOutput("recomendacion")
                            )
                          )
                 ),
                 
                 # 🔵 TEORMA DEL LÍMITE CENTRAL
                 tabPanel("Teorema del Límite Central",
                          tabsetPanel(
                            tabPanel("Simulación", "Aquí irá la simulación del TLC..."),
                            tabPanel("Histograma de medias", "Visualización de histogramas de medias muestrales...")
                          )
                 ),
                 
                 # 🟣 NORMALIDAD
                 tabPanel("Normalidad",
                          tabsetPanel(
                            tabPanel("Kolmogorov-Smirnov", "Aquí irá la prueba KS..."),
                            tabPanel("Lilliefors", "Aquí irá la prueba de Lilliefors..."),
                            tabPanel("Shapiro-Wilk", "Aquí irá la prueba Shapiro-Wilk..."),
                            tabPanel("Jarque-Bera", "Aquí irá la prueba Jarque-Bera...")
                          )
                 ),
                 
                 # 🔴 t-STUDENT Y ANOVA
                 tabPanel("t-Student y ANOVA",
                          tabsetPanel(
                            tabPanel("t independientes", "Aquí irá la prueba t para muestras independientes..."),
                            tabPanel("t pareadas", "Aquí irá la prueba t para muestras relacionadas..."),
                            tabPanel("ANOVA", "Aquí se implementará ANOVA..."),
                            tabPanel("Wilcoxon", "Aquí irá la prueba de Wilcoxon...")
                          )
                 ),
                 
                 # 🟠 CHI-CUADRADO
                 tabPanel("Chi-cuadrado",
                          tabsetPanel(
                            tabPanel("Independencia", "Aquí se hará chi-cuadrado de independencia..."),
                            tabPanel("Bondad de ajuste", "Aquí se hará bondad de ajuste..."),
                            tabPanel("McNemar", "Aquí se aplicará la prueba de McNemar..."),
                            tabPanel("Q de Cochrane", "Aquí estará la prueba Q de Cochrane...")
                          )
                 ),
                 
                 # 🟢 CORRELACIÓN
                 tabPanel("Correlación",
                          tabsetPanel(
                            tabPanel("Pearson", "Aquí irá la correlación de Pearson (n > 30)..."),
                            tabPanel("Spearman", "Aquí irá la correlación de Spearman (n < 30)...")
                          )
                 ),
                 
                 # ⚪ MUESTREO Y ALEATORIEDAD
                 tabPanel("Muestreo y Aleatoriedad",
                          tabsetPanel(
                            tabPanel("Generar números aleatorios", "Herramientas para generación aleatoria..."),
                            tabPanel("Muestreo con/sin reemplazo", "Conceptos y funciones..."),
                            tabPanel("Nivel de independencia", "Medición del nivel de independencia...")
                          )
                 ),
                 
                 # 📄 REPORTE
                 tabPanel("Reporte",
                          h4("Aquí se podrá generar un reporte en PDF con los resultados.")
                 )
)

# Servidor
server <- function(input, output, session) {
  datos <- reactiveVal(NULL)
  
  # Leer archivo
  observe({
    req(input$archivo)
    archivo <- input$archivo
    df <- read.csv(archivo$datapath)
    datos(df)
  })
  
  # Generar datos aleatorios
  observeEvent(input$generar, {
    set.seed(input$semilla)
    
    n <- input$tamano
    dist <- input$distribucion
    muestreo <- input$muestreo
    
    # Generar población base
    base <- switch(dist,
                   "Normal" = rnorm(1000),
                   "Uniforme" = runif(1000),
                   "Exponencial" = rexp(1000))
    
    # Muestreo
    muestra <- if (muestreo == "con") {
      sample(base, n, replace = TRUE)
    } else {
      sample(base, n, replace = FALSE)
    }
    
    df <- data.frame(valor = muestra)
    datos(df)
  })
  
  # Mostrar tabla
  output$tabla_datos <- DT::renderDataTable({
    req(datos())
    DT::datatable(datos(), options = list(pageLength = 5))
  })
  
  # Mostrar histograma
  output$histograma <- renderPlot({
    req(datos())
    hist(datos()$valor, col = "steelblue", main = "Histograma de la muestra", xlab = "Valor")
  })
  
  # Recomendación según los datos
  output$recomendacion <- renderText({
    req(datos())
    df <- datos()
    
    if (!"valor" %in% names(df)) return("Sube o genera datos para obtener una recomendación.")
    
    n <- nrow(df)
    media <- mean(df$valor)
    desv <- sd(df$valor)
    
    if (n < 30) {
      recomendacion <- "🔸 Tamaño de muestra pequeño (n < 30): se sugiere usar pruebas NO paramétricas como Wilcoxon o Spearman."
    } else {
      recomendacion <- "🔹 Tamaño de muestra adecuado (n ≥ 30): puedes usar pruebas paramétricas como t-Student, ANOVA o Pearson."
    }
    
    recomendacion <- paste0(recomendacion,
                            "\n\n📊 Estadísticas básicas:",
                            "\nMedia: ", round(media, 2),
                            "\nDesviación estándar: ", round(desv, 2),
                            "\nCantidad de datos: ", n)
    
    return(recomendacion)
  })
}

# Ejecutar la aplicación
shinyApp(ui = ui, server = server)


