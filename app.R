library(shiny)
library(readxl)
library(dplyr)
library(ggplot2)
library(shinythemes)
library(plotly)
library(tidyr)

ui <- fluidPage(
  theme = shinytheme("cerulean"),
  titlePanel("✨ Pruebas Estadísticas"),
  
  sidebarLayout(
    sidebarPanel(
      fileInput("archivo", "📥 Sube un archivo Excel (.xlsx):", accept = ".xlsx"),
      radioButtons(
        "tipo_prueba",
        "Seleccione el tipo de prueba estadística:",
        choices = list("Prueba t-test" = "ttest", "ANOVA" = "anova"),
        inline = TRUE
      ),
      conditionalPanel(
        condition = "input.tipo_prueba == 'anova'",
        numericInput("num_variables", "¿Cuántas variables desea comparar?", value = 3, min = 3, step = 1)
      ),
      uiOutput("seleccion_categorica"),
      uiOutput("seleccion_numerica"),
      actionButton("analizar", "📊 Analizar Archivo", icon = icon("chart-bar")),
      downloadButton("descargar_resultados", "Descargar Resultados")
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Datos", tableOutput("vista_datos")),
        tabPanel("Gráficos", plotlyOutput("grafico")),
        tabPanel("Análisis", verbatimTextOutput("resultado_test")),
        tabPanel("Interpretación", htmlOutput("interpretacion")),
        tabPanel("Resumen Descriptivo", tableOutput("resumen"))
      )
    )
  )
)

server <- function(input, output) {
  datos <- eventReactive(input$analizar, {
    req(input$archivo)
    read_excel(input$archivo$datapath)
  })
  
  output$seleccion_categorica <- renderUI({
    req(datos())
    columnas_categoricas <- names(datos())[sapply(datos(), function(x) is.character(x) || is.factor(x))]
    
    if (input$tipo_prueba == "ttest") {
      selectInput("col_categorica", "Seleccione la columna categórica (2 grupos):", choices = columnas_categoricas)
    } else if (input$tipo_prueba == "anova") {
      selectInput("col_categorica", "Seleccione la columna categórica (3 o más grupos):", choices = columnas_categoricas)
    }
  })
  
  output$seleccion_numerica <- renderUI({
    req(datos())
    columnas_numericas <- names(datos())[sapply(datos(), is.numeric)]
    
    if (input$tipo_prueba == "ttest") {
      selectInput("col_numerica", "Seleccione la columna numérica:", choices = columnas_numericas)
    } else if (input$tipo_prueba == "anova") {
      req(input$num_variables)
      lapply(1:input$num_variables, function(i) {
        selectInput(paste0("col_numerica_", i), paste("Seleccione la columna numérica ", i, ":"), choices = columnas_numericas)
      })
    }
  })
  
  output$vista_datos <- renderTable({
    head(datos(), 10)
  })
  
  output$resumen <- renderTable({
    req(datos())
    summary(datos())
  })
  
  output$grafico <- renderPlotly({
    req(input$col_categorica)
    df <- datos()
    
    if (input$tipo_prueba == "ttest") {
      req(input$col_numerica)
      p <- ggplot(df, aes(x = df[[input$col_categorica]], y = df[[input$col_numerica]], fill = df[[input$col_categorica]])) +
        geom_boxplot() +
        theme_minimal() +
        labs(
          x = input$col_categorica,
          y = input$col_numerica,
          title = paste("Comparación de", input$col_numerica, "según", input$col_categorica)
        )
    } else if (input$tipo_prueba == "anova") {
      req(input$num_variables)
      num_vars <- sapply(1:input$num_variables, function(i) input[[paste0("col_numerica_", i)]])
      df_long <- df %>%
        select(input$col_categorica, all_of(num_vars)) %>%
        pivot_longer(-all_of(input$col_categorica), names_to = "variable", values_to = "valor")
      
      p <- ggplot(df_long, aes(x = df_long[[input$col_categorica]], y = valor, fill = variable)) +
        geom_boxplot() +
        theme_minimal() +
        labs(
          x = input$col_categorica,
          y = "Valor",
          title = paste("Comparación de variables según", input$col_categorica)
        )
    }
    
    ggplotly(p)
  })
  
  output$resultado_test <- renderPrint({
    req(input$col_categorica)
    df <- datos()
    grupo <- as.factor(df[[input$col_categorica]])
    
    if (input$tipo_prueba == "ttest" && length(unique(grupo)) == 2) {
      req(input$col_numerica)
      valor <- df[[input$col_numerica]]
      print(t.test(valor ~ grupo))
    } else if (input$tipo_prueba == "anova" && length(unique(grupo)) > 2) {
      req(input$num_variables)
      num_vars <- sapply(1:input$num_variables, function(i) input[[paste0("col_numerica_", i)]])
      df_long <- df %>%
        select(input$col_categorica, all_of(num_vars)) %>%
        pivot_longer(-all_of(input$col_categorica), names_to = "variable", values_to = "valor")
      
      resultado_anova <- aov(valor ~ df_long[[input$col_categorica]], data = df_long)
      print(summary(resultado_anova))
      
      cat("\nTabla ANOVA:\n")
      anova_table <- summary(resultado_anova)[[1]]
      print(anova_table)
      
      cat("\nPrueba de Tukey:\n")
      print(TukeyHSD(resultado_anova))
    } else {
      cat("⚠️ Prueba no válida para los datos seleccionados.")
    }
  })
  
  output$interpretacion <- renderUI({
    req(input$col_categorica)
    df <- datos()
    grupo <- as.factor(df[[input$col_categorica]])
    p <- NA
    
    if (input$tipo_prueba == "ttest" && length(unique(grupo)) == 2) {
      req(input$col_numerica)
      valor <- df[[input$col_numerica]]
      prueba <- t.test(valor ~ grupo)
      p <- prueba$p.value
    } else if (input$tipo_prueba == "anova" && length(unique(grupo)) > 2) {
      req(input$num_variables)
      num_vars <- sapply(1:input$num_variables, function(i) input[[paste0("col_numerica_", i)]])
      df_long <- df %>%
        select(input$col_categorica, all_of(num_vars)) %>%
        pivot_longer(-all_of(input$col_categorica), names_to = "variable", values_to = "valor")
      
      resultado_anova <- aov(valor ~ df_long[[input$col_categorica]], data = df_long)
      p <- summary(resultado_anova)[[1]][["Pr(>F)"]][1]
    }
    
    if (is.na(p)) {
      HTML("⚠️ No se pudo calcular el valor p.")
    } else if (p < 0.05) {
      if (input$tipo_prueba == "ttest") {
        HTML(paste0(
          "🧠 <b>Interpretación:</b> La prueba t indica que hay una 
          <span style='color:green'><b>diferencia significativa</b></span> entre los dos grupos en la variable seleccionada (p = ", 
          round(p, 4), "). Esto sugiere que los grupos difieren en promedio en la medida analizada."
        ))
      } else if (input$tipo_prueba == "anova") {
        HTML(paste0(
          "🧠 <b>Interpretación:</b> El análisis ANOVA muestra una 
          <span style='color:green'><b>diferencia significativa</b></span> entre los grupos en las variables seleccionadas (p = ", 
          round(p, 4), "). Esto implica que al menos un grupo difiere significativamente de los demás."
        ))
      }
    } else {
      HTML(paste0(
        "🧠 <b>Interpretación:</b> No se encontró una diferencia significativa entre los grupos (p = ", 
        round(p, 4), "). Esto indica que no hay suficiente evidencia para rechazar la hipótesis nula."
      ))
    }
  })
  
  output$descargar_resultados <- downloadHandler(
    filename = function() { paste("resultados-", Sys.Date(), ".csv", sep = "") },
    content = function(file) {
      write.csv(datos(), file)
    }
  )
}

shinyApp(ui = ui, server = server)
