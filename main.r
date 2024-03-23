wydatki_1 <- c(99, 150, 105, 137, 86, 94, 138, 89, 59, 167, 116, 115, 182, 78, 144, 121, 67, 121, 109, 145, 79, 173, 126, 149, 98, 96, 82, 75, 118, 106, 72, 45, 155, 97, 157, 63, 113, 170, 105, 71, 144, 68, 175, 116, 115, 137, 87)

wydatki_2 <- c(123, 108, 100, 127, 188, 71, 136, 76, 41, 176, 147, 57, 117, 179, 53, 86, 100, 117, 123, 138, 161, 116, 117, 89, 231, 137, 93, 151, 153, 78, 86, 82, 155, 89, 93, 31, 106, 141, 15, 225, 149, 114, 167, 88, 130, 167, 155, 82, 126, 129, 131)

oblicz_statystyki <- function(wydatki) {
  srednia <- mean(wydatki)
  mediana <- median(wydatki)
  return(list(srednia = srednia, mediana = mediana))
}

wydatki3 <- c(wydatki_1, wydatki_2)
print(wydatki3)

wyniki_1 <- oblicz_statystyki(wydatki_1)
print(wyniki_1)

wyniki_2 <- oblicz_statystyki(wydatki_2)
print(wyniki_2)