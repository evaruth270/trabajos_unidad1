library(shiny)
library(ggplot2)
library(DT)
library(shinyjs)
library(shinydashboard)
library(readxl)
library(nortest)
library(tseries)

ui <- dashboardPage(
  dashboardHeader(title = "Aplicación Estadística Avanzada"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Inicio", tabName = "inicio", icon = icon("home")),
      menuItem("Teorema del Límite Central", tabName = "TLC", icon = icon("bar-chart")),
      menuItem("Pruebas de Normalidad", tabName = "normalidad", icon = icon("check-circle")),
      menuItem("Chi-Cuadrada", tabName = "chi_squared", icon = icon("cogs")),
      menuItem("Prueba t de Student", tabName = "t_test", icon = icon("users")),
      menuItem("ANOVA", tabName = "anova", icon = icon("area-chart")),
      menuItem("Correlación", tabName = "correlacion", icon = icon("search"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "inicio", 
              h3("Bienvenido a la App de Estadística"),
              p("Explora diferentes pruebas estadísticas y visualizaciones.")
      ),
      tabItem(tabName = "TLC", 
              sidebarLayout(
                sidebarPanel(
                  numericInput("n_size", "Tamaño de muestra:", value = 30, min = 1),
                  numericInput("n_sims", "Número de simulaciones:", value = 1000, min = 1)
                ),
                mainPanel(
                  plotOutput("TLCPlot")
                )
              )
      ),
      tabItem(tabName = "normalidad", 
              sidebarLayout(
                sidebarPanel(
                  fileInput("file", "Sube CSV o XLSX", accept = c(".csv", ".xlsx")),
                  selectInput("normal_test", "Prueba de normalidad", 
                              choices = c("Shapiro-Wilk", "Kolmogorov-Smirnov", "Lilliefors", "Jarque-Bera"))
                ),
                mainPanel(
                  verbatimTextOutput("normality_result"),
                  plotOutput("normality_plot")
                )
              )
      ),
      tabItem(tabName = "chi_squared", 
              sidebarLayout(
                sidebarPanel(
                  textInput("observed", "Observados (separados por coma):", value = "10, 20, 30"),
                  textInput("expected", "Esperados (separados por coma):", value = "20, 20, 20")
                ),
                mainPanel(
                  verbatimTextOutput("chi_squared_result"),
                  plotOutput("chi_plot")
                )
              )
      ),
      tabItem(tabName = "t_test", 
              sidebarLayout(
                sidebarPanel(
                  fileInput("t_file", "Sube CSV o XLSX", accept = c(".csv", ".xlsx")),
                  selectInput("t_test_type", "Tipo de prueba t", 
                              choices = c("Muestras independientes", "Muestras pareadas"))
                ),
                mainPanel(
                  verbatimTextOutput("t_test_result"),
                  plotOutput("t_plot")
                )
              )
      ),
      tabItem(tabName = "anova", 
              sidebarLayout(
                sidebarPanel(
                  fileInput("anova_file", "Sube CSV o XLSX", accept = c(".csv", ".xlsx"))
                ),
                mainPanel(
                  verbatimTextOutput("anova_result"),
                  plotOutput("anova_plot")
                )
              )
      ),
      tabItem(tabName = "correlacion", 
              sidebarLayout(
                sidebarPanel(
                  fileInput("corr_file", "Sube CSV o XLSX", accept = c(".csv", ".xlsx")),
                  selectInput("corr_type", "Tipo de correlación", choices = c("Pearson", "Spearman"))
                ),
                mainPanel(
                  verbatimTextOutput("correlation_result"),
                  plotOutput("corr_plot")
                )
              )
      )
    )
  )
)

server <- function(input, output) {
  # TLC
  output$TLCPlot <- renderPlot({
    set.seed(123)
    sample_means <- replicate(input$n_sims, mean(sample(1:100, input$n_size, replace = TRUE)))
    ggplot(data.frame(x = sample_means), aes(x = x)) +
      geom_histogram(bins = 30, fill = "skyblue", color = "black") +
      ggtitle("Distribución de medias (TLC)") +
      theme_minimal()
  })
  
  # Pruebas de Normalidad
  output$normality_result <- renderPrint({
    req(input$file)
    ext <- tools::file_ext(input$file$name)
    data <- if (ext == "csv") read.csv(input$file$datapath) else read_xlsx(input$file$datapath)
    x <- data[[1]]
    switch(input$normal_test,
           "Shapiro-Wilk" = shapiro.test(x),
           "Kolmogorov-Smirnov" = ks.test(x, "pnorm", mean = mean(x), sd = sd(x)),
           "Lilliefors" = lillie.test(x),
           "Jarque-Bera" = jarque.bera.test(x))
  })
  
  output$normality_plot <- renderPlot({
    req(input$file)
    ext <- tools::file_ext(input$file$name)
    data <- if (ext == "csv") read.csv(input$file$datapath) else read_xlsx(input$file$datapath)
    x <- data[[1]]
    ggplot(data.frame(x), aes(x = x)) +
      geom_histogram(bins = 30, fill = "lightgreen", color = "black") +
      ggtitle("Histograma de la variable") +
      theme_minimal()
  })
  
  # Chi-Cuadrada
  output$chi_squared_result <- renderPrint({
    obs <- as.numeric(strsplit(input$observed, ",")[[1]])
    exp <- as.numeric(strsplit(input$expected, ",")[[1]])
    chisq.test(obs, p = exp / sum(exp))
  })
  
  output$chi_plot <- renderPlot({
    obs <- as.numeric(strsplit(input$observed, ",")[[1]])
    exp <- as.numeric(strsplit(input$expected, ",")[[1]])
    df <- data.frame(Grupo = factor(1:length(obs)), Observado = obs, Esperado = exp)
    ggplot(df, aes(x = Grupo)) +
      geom_bar(aes(y = Observado), stat = "identity", fill = "orange") +
      geom_point(aes(y = Esperado), color = "red", size = 3) +
      ggtitle("Comparación Observado vs Esperado") +
      theme_minimal()
  })
  
  # Prueba t
  output$t_test_result <- renderPrint({
    req(input$t_file)
    ext <- tools::file_ext(input$t_file$name)
    data <- if (ext == "csv") read.csv(input$t_file$datapath) else read_xlsx(input$t_file$datapath)
    if (input$t_test_type == "Muestras independientes") {
      t.test(data[[1]], data[[2]])
    } else {
      t.test(data[[1]], data[[2]], paired = TRUE)
    }
  })
  
  output$t_plot <- renderPlot({
    req(input$t_file)
    ext <- tools::file_ext(input$t_file$name)
    data <- if (ext == "csv") read.csv(input$t_file$datapath) else read_xlsx(input$t_file$datapath)
    df <- data.frame(grupo = rep(c("Grupo 1", "Grupo 2"), each = nrow(data)),
                     valor = c(data[[1]], data[[2]]))
    ggplot(df, aes(x = grupo, y = valor, fill = grupo)) +
      geom_boxplot() +
      ggtitle("Boxplot Comparativo") +
      theme_minimal()
  })
  
  # ANOVA
  output$anova_result <- renderPrint({
    req(input$anova_file)
    ext <- tools::file_ext(input$anova_file$name)
    data <- if (ext == "csv") read.csv(input$anova_file$datapath) else read_xlsx(input$anova_file$datapath)
    colnames(data) <- c("valor", "grupo")
    aov_result <- aov(valor ~ as.factor(grupo), data = data)
    summary(aov_result)
  })
  
  output$anova_plot <- renderPlot({
    req(input$anova_file)
    ext <- tools::file_ext(input$anova_file$name)
    data <- if (ext == "csv") read.csv(input$anova_file$datapath) else read_xlsx(input$anova_file$datapath)
    colnames(data) <- c("valor", "grupo")
    ggplot(data, aes(x = as.factor(grupo), y = valor, fill = grupo)) +
      geom_boxplot() +
      ggtitle("Boxplot por grupo (ANOVA)") +
      theme_minimal()
  })
  
  # Correlación
  output$correlation_result <- renderPrint({
    req(input$corr_file)
    ext <- tools::file_ext(input$corr_file$name)
    data <- if (ext == "csv") read.csv(input$corr_file$datapath) else read_xlsx(input$corr_file$datapath)
    if (input$corr_type == "Pearson") {
      cor.test(data[[1]], data[[2]], method = "pearson")
    } else {
      cor.test(data[[1]], data[[2]], method = "spearman")
    }
  })
  
  output$corr_plot <- renderPlot({
    req(input$corr_file)
    ext <- tools::file_ext(input$corr_file$name)
    data <- if (ext == "csv") read.csv(input$corr_file$datapath) else read_xlsx(input$corr_file$datapath)
    ggplot(data, aes(x = data[[1]], y = data[[2]])) +
      geom_point(color = "blue", size = 2) +
      geom_smooth(method = "lm", se = FALSE, color = "red") +
      ggtitle("Gráfico de dispersión (correlación)") +
      theme_minimal()
  })
}

shinyApp(ui, server)

