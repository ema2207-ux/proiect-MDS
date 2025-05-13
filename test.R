library(testthat)

# Test pentru o funcție care calculează media notelor
test_that("media este calculată corect", {
  note <- c(7, 8, 9)
  expect_equal(mean(note), 8)
})

# Test pentru numărul de absențe
test_that("Numărul de absențe este corect", {
  studenti <- data.frame(Absente = c(3, 5, 2, 0))
  absente_totale <- sum(studenti$Absente)
  expect_equal(absente_totale, 10)
})


# Funcția de validare pe care o vom testa
validate_username <- function(username) {
  if (username == "") {
    return("Numele de utilizator nu poate fi gol.")
  }
  return(NULL)
}
# Testul: Verifică dacă validarea funcționează corect
test_that("Validarea numelui de utilizator", {
  # Test 1: Numele de utilizator valid
  valid_username <- "test_user"
  result <- validate_username(valid_username)
  expect_null(result)  # Nu ar trebui să fie mesaje de eroare

  # Test 2: Numele de utilizator gol
  invalid_username <- ""
  result <- validate_username(invalid_username)
  expect_equal(result, "Numele de utilizator nu poate fi gol.")  # Verifică mesajul de eroare
})




  