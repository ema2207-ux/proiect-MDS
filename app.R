library(shiny)
library(shinyjs)
library(ggplot2)
library(dplyr)
library(DT)
library(rmarkdown)
library(knitr)
library(tinytex)

# 1. Generare date 
studenti <- read.csv("studenti.csv", stringsAsFactors = FALSE)



# Ajustări pentru corelații realiste
studenti$Media[studenti$Activitati == "Da"] <- pmin(10, studenti$Media[studenti$Activitati == "Da"] + 0.3)
studenti$Absente[studenti$Liceu == "Informatica"] <- pmin(15, studenti$Absente[studenti$Liceu == "Informatica"] + 2)

# 2. Sistem de autentificare 
user_base <- data.frame(
  username = c("profesor", "admin"),
  password = c("profesor123", "admin123"),
  permissions = c("standard", "admin"),
  stringsAsFactors = FALSE
)

# 3. Interfață utilizator (UI) 
ui <- fluidPage(
  useShinyjs(),
  
  # Pagina de login
  div(
    id = "login_page",
    style = "width: 300px; max-width: 100%; margin: 0 auto; padding-top: 50px;",
    wellPanel(
      h2("Autentificare", class = "text-center"),
      textInput("user", "Utilizator:"),
      passwordInput("password", "Parolă:"),
      actionButton("login", "Conectare", class = "btn-primary btn-block"),
      textOutput("login_message")
    )
  ),
  
  # Aplicația principală
  hidden(
    div(
      id = "main_app",
      titlePanel("Analiză Studenți - Dashboard"),
      
      sidebarLayout(
        sidebarPanel(
          width = 3,
          h4("Filtre principale"),
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
          checkboxInput("activitati", "Doar cu activități extracuriculare"),
          actionButton("filtreaza", "Aplică filtre", class = "btn-primary"),
          actionButton("reset", "Resetează filtre"),
          
          hr(),
          h4("Statistici rapide"),
          verbatimTextOutput("sumar_rapid"),
          
          actionButton("logout", "Deconectare", class = "btn-danger btn-sm")
        ),
        
        mainPanel(
          tabsetPanel(
            
            # Tab 1 - Tabel Studenți
            tabPanel("Tabel studenți",
                     downloadButton("export_tabel", "Exportă ca PDF"),
                     DTOutput("tabel_studenti")),
            
            # Tab 2 - Analize Statistice
            tabPanel("Analize statistice",
                     downloadButton("export_analize", "Exportă ca PDF"),
                     fluidRow(
                       column(6, plotOutput("hist_medii")),
                       column(6, plotOutput("scatter_absente"))
                     ),
                     fluidRow(
                       column(6, plotOutput("boxplot_liceu")),
                       column(6, plotOutput("boxplot_clasa"))
                     )),
            
            # Tab 3 - Raport Complet
            tabPanel("Raport complet",
                     downloadButton("export_raport", "Exportă ca PDF"),
                     uiOutput("raport_complet"))
          )
        )
      )
    )
  )
)


# 4. Server logic
server <- function(input, output, session) {
  # Variabile pentru autentificare
  user_auth <- reactiveVal(FALSE)
  user_role <- reactiveVal("")
  
  # Observator pentru butonul de login
  observeEvent(input$login, {
    user_row <- which(user_base$username == input$user)
    
    if (length(user_row) > 0 && 
        user_base$password[user_row] == input$password) {
      user_auth(TRUE)
      user_role(user_base$permissions[user_row])
      hide("login_page")
      show("main_app")
      output$login_message <- renderText("")
    } else {
      user_auth(FALSE)
      output$login_message <- renderText("Nume utilizator sau parolă incorectă")
    }
  })
  
  # Observator pentru butonul de logout
  observeEvent(input$logout, {
    user_auth(FALSE)
    user_role("")
    hide("main_app")
    show("login_page")
    updateTextInput(session, "user", value = "")
    updateTextInput(session, "password", value = "")
  })
  
  # Date filtrate reactive
  date_filtrate <- eventReactive(input$filtreaza, {
    req(user_auth())  # Doar dacă e autentificat
    
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
  
  # Resetare filtre
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
    req(user_auth())
    datatable(date_filtrate(),
              extensions = 'Buttons',
              options = list(
                pageLength = 10,
                dom = 'Bfrtip',
                buttons = c('copy', 'csv', 'excel'),
                scrollX = TRUE
              ),
              rownames = FALSE) %>%
      formatRound(columns = 'Media', digits = 2)
  })
  
  # Grafic: Histograma mediilor
  output$hist_medii <- renderPlot({
    req(user_auth())
    df <- date_filtrate()
    ggplot(df, aes(x = Media)) +
      geom_histogram(fill = "#3498db", bins = 15, color = "white") +
      labs(title = "Distribuția mediilor", x = "Medie", y = "Număr studenți") +
      theme_minimal()
  })
  
  # Grafic: Relația absențe-medie
  output$scatter_absente <- renderPlot({
    req(user_auth())
    df <- date_filtrate()
    ggplot(df, aes(x = Absente, y = Media)) +
      geom_point(color = "#e74c3c", alpha = 0.7, size = 3) +
      geom_smooth(method = "lm", color = "#2c3e50") +
      labs(title = "Corelația absențe-medie", x = "Absențe", y = "Medie") +
      theme_minimal()
  })
  
  # Grafic: Boxplot pe licee
  output$boxplot_liceu <- renderPlot({
    req(user_auth())
    df <- date_filtrate()
    ggplot(df, aes(x = Liceu, y = Media, fill = Liceu)) +
      geom_boxplot() +
      labs(title = "Medii pe licee", x = "", y = "Medie") +
      theme_minimal() +
      theme(legend.position = "none")
  })
  
  # Grafic: Boxplot pe clase
  output$boxplot_clasa <- renderPlot({
    req(user_auth())
    df <- date_filtrate()
    ggplot(df, aes(x = Clasa, y = Media, fill = Clasa)) +
      geom_boxplot() +
      labs(title = "Medii pe clase", x = "", y = "Medie") +
      theme_minimal() +
      theme(legend.position = "none")
  })
  
  # Sumar rapid
  output$sumar_rapid <- renderPrint({
    req(user_auth())
    df <- date_filtrate()
    cat("Studenți filtrați:", nrow(df), "\n")
    cat("Media generală:", round(mean(df$Media), 2), "\n")
    cat("Medie absențe:", round(mean(df$Absente), 1), "\n")
    cat("Participare activități:", 
        sum(df$Activitati == "Da"), "/", nrow(df))
  })
  
  # Raport complet
  output$raport_complet <- renderUI({
    req(user_auth())
    df <- date_filtrate()
    
    tagList(
      h3("Raport complet - Analiză studenți"),
      hr(),
      h4("Statistici descriptive"),
      verbatimTextOutput("sumar_statistici"),
      hr(),
      h4("Distribuții"),
      fluidRow(
        column(6, plotOutput("dist_liceu")),
        column(6, plotOutput("dist_clasa"))
      )
    )
  })
  
  # Elemente suplimentare pentru raport
  output$sumar_statistici <- renderPrint({
    req(user_auth())
    df <- date_filtrate()
    summary(df[, c("Media", "Absente")])
  })
  
  output$dist_liceu <- renderPlot({
    req(user_auth())
    df <- date_filtrate()
    ggplot(df, aes(x = Liceu, fill = Liceu)) +
      geom_bar() +
      labs(title = "Distribuție pe licee", x = "", y = "") +
      theme_minimal()
  })
  
  output$dist_clasa <- renderPlot({
    req(user_auth())
    df <- date_filtrate()
    ggplot(df, aes(x = Clasa, fill = Clasa)) +
      geom_bar() +
      labs(title = "Distribuție pe clase", x = "", y = "") +
      theme_minimal()
  })
  
  
  # Export PDF
  output$export_tabel <- downloadHandler(
    filename = function() {
      paste("tabel_studenti_", format(Sys.Date(), "%Y-%m-%d"), ".pdf", sep = "")
    },
    content = function(file) {
      # am creat un RDM FILE
      tempReport <- file.path(tempdir(), "tabel_studenti.Rmd")
      df <- isolate(date_filtrate())
      # rescrie intr un temporal file
      writeLines(c(
        "---",
        "title: \"Tabel Studenți\"",
        "output: pdf_document",
        "---",
        "",
        "```{r setup, include=FALSE}",
        "knitr::opts_chunk$set(echo = FALSE)",
        "library(knitr)",
        "```",
        "",
        "## Tabel Studenți",
        "",
        "```{r}",
        "knitr::kable(df, format = 'latex', booktabs = TRUE)",
        "```"
      ), tempReport)
      report_env <- new.env(parent = globalenv())
      report_env$df <- df
      
      # bara de generare in curs a pdf ului
      withProgress(message = 'Generare PDF în curs...', {
        tryCatch({
          rmarkdown::render(tempReport, 
                            output_file = file,
                            envir = report_env)
        }, error = function(e) {
          showNotification(paste("Eroare la generarea PDF:", e$message), 
                           type = "error", duration = 10)
        })
      })
    }
  )
  
  # Export la date analize  PDF
  output$export_analize <- downloadHandler(
    filename = function() {
      paste("analize_statistice_", format(Sys.Date(), "%Y-%m-%d"), ".pdf", sep = "")
    },
    content = function(file) {
      df <- isolate(date_filtrate())
   
      tempReport <- file.path(tempdir(), "analize_statistice.Rmd")
   
      writeLines(c(
        "---",
        "title: \"Analize Statistice\"",
        "output: pdf_document",
        "---",
        "",
        "```{r setup, include=FALSE}",
        "knitr::opts_chunk$set(echo = FALSE, message = FALSE, warning = FALSE, fig.width = 6, fig.height = 4)",
        "library(ggplot2)",
        "library(dplyr)",
        "```",
        "",
        "## Distribuția mediilor",
        "",
        "```{r}",
        "ggplot(df, aes(x = Media)) +",
        "  geom_histogram(fill = \"#3498db\", bins = 15, color = \"white\") +",
        "  labs(title = \"Distribuția mediilor\", x = \"Medie\", y = \"Număr studenți\") +",
        "  theme_minimal()",
        "```",
        "",
        "## Corelația absențe-medie",
        "",
        "```{r}",
        "ggplot(df, aes(x = Absente, y = Media)) +",
        "  geom_point(color = \"#e74c3c\", alpha = 0.7, size = 3) +",
        "  geom_smooth(method = \"lm\", color = \"#2c3e50\") +",
        "  labs(title = \"Corelația absențe-medie\", x = \"Absențe\", y = \"Medie\") +",
        "  theme_minimal()",
        "```",
        "",
        "## Medii pe licee",
        "",
        "```{r}",
        "ggplot(df, aes(x = Liceu, y = Media, fill = Liceu)) +",
        "  geom_boxplot() +",
        "  labs(title = \"Medii pe licee\", x = \"\", y = \"Medie\") +",
        "  theme_minimal() +",
        "  theme(legend.position = \"none\")",
        "```",
        "",
        "## Medii pe clase",
        "",
        "```{r}",
        "ggplot(df, aes(x = Clasa, y = Media, fill = Clasa)) +",
        "  geom_boxplot() +",
        "  labs(title = \"Medii pe clase\", x = \"\", y = \"Medie\") +",
        "  theme_minimal() +",
        "  theme(legend.position = \"none\")",
        "```"
      ), tempReport)
      
      report_env <- new.env(parent = globalenv())
      report_env$df <- df
      
      withProgress(message = 'Generare PDF în curs...', {
        tryCatch({
          rmarkdown::render(tempReport, 
                            output_file = file,
                            envir = report_env)
        }, error = function(e) {
          showNotification(paste("Eroare la generarea PDF:", e$message), 
                           type = "error", duration = 10)
        })
      })
    }
  )
  output$export_raport <- downloadHandler(
    filename = function() {
      paste("raport_complet_", format(Sys.Date(), "%Y-%m-%d"), ".pdf", sep = "")
    },
    content = function(file) {
      df <- isolate(date_filtrate())
      
      tempReport <- file.path(tempdir(), "raport_complet.Rmd")
      
      writeLines(c(
        "---",
        "title: \"Raport Complet - Analiză Studenți\"",
        "output: pdf_document",
        "---",
        "",
        "```{r setup, include=FALSE}",
        "knitr::opts_chunk$set(echo = FALSE, message = FALSE, warning = FALSE, fig.width = 6, fig.height = 4)",
        "library(ggplot2)",
        "library(dplyr)",
        "library(knitr)",
        "library(gridExtra)",
        "```",
        "",
        "## Sumar date",
        "",
        "```{r}",
        "cat(\"Studenți filtrați:\", nrow(df), \"\\n\")",
        "cat(\"Media generală:\", round(mean(df$Media), 2), \"\\n\")",
        "cat(\"Medie absențe:\", round(mean(df$Absente), 1), \"\\n\")",
        "cat(\"Participare activități:\", sum(df$Activitati == \"Da\"), \"/\", nrow(df))",
        "```",
        "",
        "## Statistici descriptive",
        "",
        "```{r}",
        "kable(summary(df[, c(\"Media\", \"Absente\")]), format = \"latex\")",
        "```",
        "",
        "## Distribuția mediilor",
        "",
        "```{r}",
        "ggplot(df, aes(x = Media)) +",
        "  geom_histogram(fill = \"#3498db\", bins = 15, color = \"white\") +",
        "  labs(title = \"Distribuția mediilor\", x = \"Medie\", y = \"Număr studenți\") +",
        "  theme_minimal()",
        "```",
        "",
        "## Distribuții pe licee și clase",
        "",
        "```{r, fig.width=10}",
        "p1 <- ggplot(df, aes(x = Liceu, fill = Liceu)) +",
        "  geom_bar() +",
        "  labs(title = \"Distribuție pe licee\", x = \"\", y = \"\") +",
        "  theme_minimal() +",
        "  theme(legend.position = \"none\")",
        "",
        "p2 <- ggplot(df, aes(x = Clasa, fill = Clasa)) +",
        "  geom_bar() +",
        "  labs(title = \"Distribuție pe clase\", x = \"\", y = \"\") +",
        "  theme_minimal() +",
        "  theme(legend.position = \"none\")",
        "",
        "gridExtra::grid.arrange(p1, p2, ncol = 2)",
        "```",
        "",
        "## Tabel complet",
        "",
        "```{r}",
        "kable(df, format = \"latex\", booktabs = TRUE)",
        "```"
      ), tempReport)
      
      report_env <- new.env(parent = globalenv())
      report_env$df <- df
      
      withProgress(message = 'Generare PDF în curs...', {
        tryCatch({
          rmarkdown::render(tempReport, 
                            output_file = file,
                            envir = report_env)
        }, error = function(e) {
          showNotification(paste("Eroare la generarea PDF:", e$message), 
                           type = "error", duration = 10)
        })
      })
    }
  )
  
}


# 5. Pornire aplicație --------------------------------------------------
shinyApp(ui = ui, server = server)