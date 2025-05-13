library(shiny)
library(shinyjs)
library(ggplot2)
library(dplyr)
library(DT)

# Generare date de exemplu (în loc de citirea din CSV)
set.seed(123)
studenti <- data.frame(
  ID = 1:50,
  Nume = paste("Student", 1:50),
  Liceu = sample(c("Informatica", "Psihologie", "Economie"), 50, replace = TRUE),
  Media = round(runif(50, 4, 10), 2),
  Absente = sample(0:30, 50, replace = TRUE),
  Activitati = sample(c("Da", "Nu"), 50, replace = TRUE),
  Mediu = sample(c("Urban", "Rural"), 50, replace = TRUE),
  Clasa = sample(c("IX", "X", "XI", "XII"), 50, replace = TRUE)
)

ui <- fluidPage(
  useShinyjs(),
  titlePanel("Filtru Studenți - Dashboard"),
  
  sidebarLayout(
    sidebarPanel(
      width = 3,
      selectInput("liceu", "Liceu:", 
                  choices = c("Toate", "Informatica", "Psihologie", "Economie")),
      selectInput("mediu", "Mediu:", 
                  choices = c("Toate", "Urban", "Rural")),
      selectInput("clasa", "Clasa:", 
                  choices = c("Toate", "IX", "X", "XI", "XII")),
      sliderInput("media", "Interval medii:",
                  min = 4, max = 10, value = c(4, 10)),
      sliderInput("absente", "Număr absențe:",
                  min = 0, max = 30, value = c(0, 30)),
      checkboxInput("activitati", "Participă la activități extracuriculare"),
      actionButton("filtreaza", "Aplică filtre", class = "btn-primary"),
      actionButton("reset", "Resetează filtre")
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Tabel Studenți", 
                 DTOutput("tabel_studenti")),
        tabPanel("Statistici", 
                 fluidRow(
                   column(6, plotOutput("grafic_medii")),
                   column(6, plotOutput("grafic_absente"))
                 ),
                 fluidRow(
                   column(6, plotOutput("grafic_liceu")),
                   column(6, plotOutput("grafic_clasa"))
                 )),
        tabPanel("Sumar", 
                 verbatimTextOutput("sumar_statistici"))
      )
    )
  )
)

server <- function(input, output, session) {
  
  # Reactive expression pentru datele filtrate
  studenti_filtrati <- eventReactive(input$filtreaza, {
    studenti %>%
      filter(
        if (input$liceu != "Toate") Liceu == input$liceu else TRUE,
        if (input$mediu != "Toate") Mediu == input$mediu else TRUE,
        if (input$clasa != "Toate") Clasa == input$clasa else TRUE,
        Media >= input$media[1],
        Media <= input$media[2],
        Absente >= input$absente[1],
        Absente <= input$absente[2],
        if (input$activitati) Activitati == "Da" else TRUE
      )
  })
  
  # Buton reset
  observeEvent(input$reset, {
    updateSelectInput(session, "liceu", selected = "Toate")
    updateSelectInput(session, "mediu", selected = "Toate")
    updateSelectInput(session, "clasa", selected = "Toate")
    updateSliderInput(session, "media", value = c(4, 10))
    updateSliderInput(session, "absente", value = c(0, 30))
    updateCheckboxInput(session, "activitati", value = FALSE)
  })
  
  # Tabel interactiv
  output$tabel_studenti <- renderDT({
    datatable(studenti_filtrati(),
              options = list(pageLength = 10, scrollX = TRUE),
              rownames = FALSE) %>%
      formatStyle(columns = c(1:8), fontSize = '80%')
  })
  
  # Grafic: Distributia mediilor
  output$grafic_medii <- renderPlot({
    df <- studenti_filtrati()
    ggplot(df, aes(x = Media)) +
      geom_histogram(fill = "#3498db", bins = 15, color = "white") +
      labs(title = "Distribuția mediilor", x = "Medie", y = "Număr studenți") +
      theme_minimal()
  })
  
  # Grafic: Relatia absente-media
  output$grafic_absente <- renderPlot({
    df <- studenti_filtrati()
    ggplot(df, aes(x = Absente, y = Media)) +
      geom_point(color = "#e74c3c", alpha = 0.7) +
      geom_smooth(method = "lm", color = "#2c3e50") +
      labs(title = "Relația absențe-medie", x = "Absențe", y = "Medie") +
      theme_minimal()
  })
  
  # Grafic: Medii pe licee
  output$grafic_liceu <- renderPlot({
    df <- studenti_filtrati()
    ggplot(df, aes(x = Liceu, y = Media, fill = Liceu)) +
      geom_boxplot() +
      labs(title = "Medii pe licee", x = "", y = "Medie") +
      theme_minimal() +
      theme(legend.position = "none")
  })
  
  # Grafic: Medii pe clase
  output$grafic_clasa <- renderPlot({
    df <- studenti_filtrati()
    ggplot(df, aes(x = Clasa, y = Media, fill = Clasa)) +
      geom_boxplot() +
      labs(title = "Medii pe clase", x = "", y = "Medie") +
      theme_minimal() +
      theme(legend.position = "none")
  })
  
  # Sumar statistic
  output$sumar_statistici <- renderPrint({
    df <- studenti_filtrati()
    cat("NUMĂR STUDENȚI FILTRATI:", nrow(df), "\n\n")
    cat("MEDIA GENERALĂ:", round(mean(df$Media), 2), "\n")
    cat("MEDIA ABSENȚELOR:", round(mean(df$Absente), 1), "\n\n")
    cat("DISTRIBUȚIE PE LICEE:\n")
    print(table(df$Liceu))
    cat("\nDISTRIBUȚIE PE MEDII:\n")
    print(summary(df$Media))
  })
}

shinyApp(ui, server)