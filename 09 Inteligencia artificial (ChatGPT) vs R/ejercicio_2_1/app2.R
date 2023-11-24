library(shiny)
library(ggplot2)
mtcars$cyl <- as.factor(mtcars$cyl)

# Define la interfaz de usuario de la aplicación Shiny
ui <- fluidPage(
  titlePanel("Diagrama de Dispersión de mtcars"),
  
  # Crear menús desplegables para las variables del eje x y eje y
  sidebarLayout(
    sidebarPanel(
      selectInput("x_var", "Eje X:", choices = colnames(mtcars)),
      selectInput("y_var", "Eje Y:", choices = colnames(mtcars))
    ),
    
    # Crear el área de visualización del gráfico
    mainPanel(
      plotOutput("scatterplot")
    )
  )
)

# Define el servidor de la aplicación Shiny
server <- function(input, output) {
  
  # Crear un gráfico de dispersión interactivo basado en las selecciones del usuario
  output$scatterplot <- renderPlot({
    ggplot(mtcars, aes_string(x = input$x_var, y = input$y_var)) +
      geom_point(aes(colour = cyl), size = 3) +
      labs(x = input$x_var, y = input$y_var) +
      theme_minimal()
  })
}

# Ejecutar la aplicación Shiny
shinyApp(ui = ui, server = server)
