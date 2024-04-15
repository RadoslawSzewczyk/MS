wydatki_1 <- c(99, 150, 105, 137, 86, 94, 138, 89, 59, 167, 116, 115, 182, 78, 144, 121, 67, 121, 109, 145, 79, 173, 126, 149, 98, 96, 82, 75, 118, 106, 72, 45, 155, 97, 157, 63, 113, 170, 105, 71, 144, 68, 175, 116, 115, 137, 87)
wydatki_2 <- c(123, 108, 100, 127, 188, 71, 136, 76, 41, 176, 147, 57, 117, 179, 53, 86, 100, 117, 123, 138, 161, 116, 117, 89, 231, 137, 93, 151, 153, 78, 86, 82, 155, 89, 93, 31, 106, 141, 15, 225, 149, 114, 167, 88, 130, 167, 155, 82, 126, 129, 131)

wydatkiC <- c(wydatki_1, wydatki_2)

all_modes <- function(x)
{
tab <- table(x)
max_freq <- max(tab)
names(tab)[tab == max_freq]
}

zadanie_1a <- function(wektor)
{
  srednia <- mean(wektor)
  mediana <- median(wektor)
  moda <- all_modes(wektor)
  kwartyle <- quantile(wektor, probs = c(0.25, 0.75))

  wariancja_obciazona <- sum((wektor - srednia)^2) / length(wektor)
  odchylenie_standardowe_obciazone <- sqrt(wariancja_obciazona)
  wariancja_nieobciazona <- var(wektor)
  odchylenie_standardowe_nieobciazone <- sqrt(wariancja_nieobciazona)
  odchylenie_przecietne_d1 <- sum(abs(wektor - srednia)) / length(wektor)
  odchylenie_przecietne_od_mediany <- sum(abs(wektor - mediana)) / length(wektor)
  odchylenie_cwiartkowe <- (kwartyle[2] - kwartyle[1]) 
  wspolczynnik_zmiennosci_v <- (odchylenie_standardowe_obciazone / srednia) * 100
  pozycyjny_wspolczynnik_zmiennosci_vq <- (odchylenie_cwiartkowe / mediana) * 100


  skosnosc <- sum((wektor-srednia)^3)/ ( length(wektor) * odchylenie_standardowe_obciazone ^3)
  kurtoza <- sum((wektor-srednia)^4)/ ( length(wektor) * odchylenie_standardowe_obciazone ^4)
  eksces <- kurtoza -3

 
  return(list(srednia = srednia, mediana = mediana, moda = moda, kwartyle = kwartyle, wariancja_obciazona = wariancja_obciazona, odchylenie_standardowe_obciazone = odchylenie_standardowe_obciazone, wariancja_nieobciazona = wariancja_nieobciazona, odchylenie_standardowe_nieobciazone = odchylenie_standardowe_nieobciazone, odchylenie_przecietne_d1 = odchylenie_przecietne_d1, odchylenie_przecietne_od_mediany = odchylenie_przecietne_od_mediany, odchylenie_cwiartkowe = odchylenie_cwiartkowe, wspolczynnik_zmiennosci_v = wspolczynnik_zmiennosci_v, pozycyjny_wspolczynnik_zmiennosci_vq = pozycyjny_wspolczynnik_zmiennosci_vq, skosnosc = skosnosc, kurtoza = kurtoza, eksces = eksces))
}

# Funkcja `zad5` wykonuje test t dla średniej
# wartości w zadanym wektorze `wydatki`.
# Test sprawdza, czy średnia w pierwszym punkcie jest większa niż w drugim z poziomem istotności 0.05.

zad5 <- function(wydatki_1, wydatki_2){
  # Sprawdzenie, czy argument `wydatki` jest wektorem
  if (!is.numeric(wydatki_1) || !is.numeric(wydatki_2)) {
    stop("Argument `wydatki` musi być wektorem liczbowym.")
  }
  wynik_testu <- var.test(wydatki_1, wydatki_2)
  p_wartosc <- wynik_testu$p.value
  poziom_ufnosci <- 0.05

  if (p_wartosc > poziom_ufnosci) {
    # Wariancje równe
    # Test t-Studenta
    print(t.test(wydatki_1, wydatki_2, mu = mean(wydatki_2), alternative = "greater", conf.level = 0.95))
  } else {
    # Wariancje różne
    # test corchana coxa
    print(t.test(wydatki_1, wydatki_2, var.equal = FALSE, alternative = "greater", conf.level = 0.95))
  }
}

print("Zadanie 1 wyniki: ")
wyniki <- zadanie_1a(wydatkiC)
print(wyniki)


wynik_zad_5 <- zad5(wydatki_1, wydatki_2)
print("Zadanie 5 wyniki: ")
print(wynik_zad_5)

p_value <- wynik_zad_5$p.value

if (p_value < 0.05) {
  print("Można stwierdzić, że średnie miesięczne wydatki na wyroby drogeryjne w pierwszym punkcie są większe niż w drugim punkcie z poziomem istotności 0.05.")
} else {
  print("Brak wystarczających dowodów, aby stwierdzić, że średnie miesięczne wydatki na wyroby drogeryjne w pierwszym punkcie są większe niż w drugim punkcie z poziomem istotności 0.05.")
}
