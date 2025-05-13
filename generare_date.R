set.seed(123)
if (!require("ggplot2")) install.packages("ggplot2")

# Generare date
studenti <- data.frame(
  ID = 1:20,
  Nume = paste("Student", 1:20),
  Liceu = sample(c("Informatica", "Psihologie", "Economie"), 20, replace = TRUE),
  Media = round(runif(20, 5, 10), 2),
  Absente = sample(0:20, 20, replace = TRUE),
  Activitati = sample(0:1, 20, replace = TRUE),
  Feedback = sample(5:10, 20, replace = TRUE),
  Mediu = sample(c("Urban", "Rural"), 20, replace = TRUE),  # Adăugat câmpul Mediu (Urban/Rural)
  Clasa = sample(c("IX", "X", "XI", "XII"), 20, replace = TRUE)  # Adăugat câmpul Clasa (IX, X, XI, XII)
)