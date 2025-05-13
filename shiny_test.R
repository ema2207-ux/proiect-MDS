library(shiny)

# UI
ui <- fluidPage(
  titlePanel("Aplicație Analiză Studenți"),
  
  sidebarLayout(
    sidebarPanel(
      textInput("user", "Utilizator:", ""),
      passwordInput("password", "Parolă:", ""),
      actionButton("login", "Autentificare")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Analize", plotOutput("statPlot")),
        tabPanel("Statistici", textOutput("stats"))
      )
    )
  )
)

# Server
server <- function(input, output, session) {
  
  # Autentificare (exemplu simplificat)
  observeEvent(input$login, {
    if(input$user == "admin" && input$password == "admin") {
      output$stats <- renderText("Autentificare reușită!")
    } else {
      output$stats <- renderText("Autentificare eșuată!")
    }
  })
  
  # Grafic pentru analize
  output$statPlot <- renderPlot({
    plot(cars)
  })
}

shinyApp(ui = ui, server = server)
