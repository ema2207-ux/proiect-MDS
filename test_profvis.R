library(profvis)

test_function <- function() {
  # Crearea unui vector mare
  large_vector <- rnorm(1000000)
  
  # Calculul mediei
  mean_value <- mean(large_vector)
  
  # Calculul sumelor parțiale
  partial_sums <- cumsum(large_vector)
  
  # Returnarea valorilor
  return(list(mean = mean_value, sums = partial_sums))
}

# Profilarea funcției
profvis({
  result <- test_function()
})