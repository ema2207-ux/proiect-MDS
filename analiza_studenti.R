
# 2. Citim datele
studenti <- read.csv("studenti.csv")

# 3. Statistici descriptive
cat("Statistici descriptive:\n")
summary(studenti)

cat("\nMedia generală a notelor:", mean(studenti$Media), "\n")
cat("Media generală a absențelor:", mean(studenti$Absente), "\n")
cat("Participare activități extracurriculare:\n")
print(table(studenti$Activitati))

# 4. Histogramă – Medii
ggplot(studenti, aes(x = Media)) +
  geom_histogram(fill = "skyblue", color = "white", bins = 10) +
  labs(title = "Distribuția Mediilor", x = "Media", y = "Număr studenți")

# 5. Scatterplot – Absențe vs Medii
ggplot(studenti, aes(x = Absente, y = Media)) +
  geom_point(color = "tomato", size = 3) +
  labs(title = "Relația dintre absențe și medii", x = "Nr. absențe", y = "Media")

# 6. Identificare studenți cu risc
studenti_risc <- studenti %>%
  filter(Media < 6 | Absente > 10)

cat("\nStudenți cu risc identificat:\n")
print(studenti_risc)

# 7. Sugestii de îmbunătățire
studenti_risc <- studenti_risc %>%
  mutate(Sugestie = case_when(
    Media < 6 & Absente > 10 ~ "Recomandare: Recuperare teme + reducerea absențelor",
    Media < 6 ~ "Recomandare: Meditații / asistență academică",
    Absente > 10 ~ "Recomandare: Disciplină și prezență constantă",
    TRUE ~ "OK"
  ))

cat("\nSugestii pentru studenții cu risc:\n")
print(studenti_risc[, c("Nume", "Media", "Absente", "Sugestie")])