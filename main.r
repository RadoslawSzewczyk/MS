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


getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}


zadanie_1b <- function(dane)
{
  ilosc_klas = 8  
  dlugosc_klasy = (max(dane) - min(dane))/ilosc_klas
  klasy = hist(dane, breaks = ilosc_klas,main = "Histogram szeregu rozdzielczego", xlab = "Wartości", ylab = "Częstość")
  wartosci = rep(klasy$mids, times = klasy$counts)
  srednia = mean(wartosci)
  mediana = median(wartosci)
  moda = getmode(wartosci)
  kwartyle <- quantile(wartosci, probs = c(0.25, 0.5, 0.75))
  wariancja_obciazona <- sum((wartosci-srednia)^2)/length(wartosci)
  wariancja_nieobciazona = var(wartosci)
  odchylenie_standardowe_obciazone = sqrt(wariancja_obciazona)
  odchylenie_standardowe_nieobciazone = sqrt(wariancja_nieobciazona)
  odchylenie_przecietne_d1 = sum(abs(wartosci-srednia))/length(wartosci)
  odchylenie_przecietne_od_mediany = sum(abs(wartosci-mediana))/length(wartosci)
  odchylenie_cwiartkowe = kwartyle[3]-kwartyle[1]
  wspolczynik_zmiennosc_v1 = (odchylenie_standardowe_obciazone/srednia)*100
  pozycyjny_wspolczynik_zmiennosc_vq = (odchylenie_cwiartkowe/mediana)*100
  
  
  skosnosc <- sum((wartosci-srednia)^3)/(length(wartosci)*odchylenie_standardowe_obciazone^3)
  
  kurtoza <- sum((wartosci-srednia)^4)/(length(wartosci)*odchylenie_standardowe_obciazone^4)
  eksces = kurtoza -3
  
  return(list(srednia = srednia, mediana = mediana, moda = moda, kwartyle = kwartyle, wariancja_obciazona = wariancja_obciazona, odchylenie_standardowe_obciazone = odchylenie_standardowe_obciazone, wariancja_nieobciazona = wariancja_nieobciazona, odchylenie_standardowe_nieobciazone = odchylenie_standardowe_nieobciazone, odchylenie_przecietne_d1 = odchylenie_przecietne_d1, odchylenie_przecietne_od_mediany = odchylenie_przecietne_od_mediany, odchylenie_cwiartkowe = odchylenie_cwiartkowe, wspolczynnik_zmiennosci_v = wspolczynik_zmiennosc_v1 , pozycyjny_wspolczynnik_zmiennosci_vq =pozycyjny_wspolczynik_zmiennosc_vq , skosnosc = skosnosc, kurtoza = kurtoza, eksces = eksces))
  
}


zadanie2 <- function(data)
{
  data <- sort(data)
  n <- length(data)
  if(length(data) == 47)
    { k <- 0.1292 }
  else if(length(data) == 51)
    { k <- 0.1241 }
  p <- pnorm((data - mean(data))/sd(data))
  Dplus <- max(seq(1:n)/n - p)
  Dminus <- max(p - (seq(1:n) - 1)/n)
  d <- max(Dplus, Dminus)
  if(d < k)
    { cat("Wydatki na jedna osobe na wyroby drogeryjne w sieci Rossmann maja rozklad normalny.\n") } 
  else
    { cat("Wydatki na jedna osobe na wyroby drogeryjne w sieci Rossmann nie maja rozkladu normalnego.\n") }
}


zadanie3 <- function(dane, mu, alfa) {
  n <- length(dane)
  s <- sd(dane)
  średnia <- mean(dane)
  t_wartość <- (średnia - mu) / (s / sqrt(n))
  stopnie_swobody <- n - 1
  krytyczna <- qt(1 - alfa / 2, df = stopnie_swobody)
  
  cat("Wartość t-testu:", t_wartość, "\n")
  cat("Wartość krytyczna:", krytyczna, "\n")
  
  if (abs(t_wartość) > krytyczna) {
    cat("Odrzucamy hipotezę zerową - istnieje istotna różnica między średnią próbkową a wartością", mu, "\n")
  } else {
    cat("Nie ma podstaw do odrzucenia hipotezy zerowej - brak istotnej różnicy między średnią próbkową a wartością", mu, "\n")
  }
}

zadanie4 <- function(dane, alfa, sigma0) {
  n <- length(dane)
  S2 <- var(dane)
  X2 <- (n * S2) / (sigma0^2)
  krytyczny_lewo <- qchisq(alfa/2, df = n-1)
  krytyczny_prawo <- qchisq(1 - alfa/2, df = n-1)
  
  cat("Wartość statystyki Chi-kwadrat:", X2, "\n")
  cat("Przedział krytyczny (lewostronny): (-∞, ", krytyczny_lewo, ")\n")
  cat("Przedział krytyczny (prawostronny): (", krytyczny_prawo, ", ∞)\n")
  
  if (X2 > krytyczny_lewo & X2 < krytyczny_prawo) {
      cat("Nie ma podstaw do odrzucenia hipotezy zerowej. Odchylenie jest równe", sigma0, "\n")
  } else {
      cat("Odrzucamy hipotezę zerową na rzecz hipotezy alternatywnej. Odchylenie jest różne od", sigma0, "\n")
  }
}


zadanie5 <- function(wydatki_1, wydatki_2){
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
a = zadanie_1b(wydatki_1)
b = zadanie_1b(wydatki_2)
c = zadanie_1a(wydatki_1)
d = zadanie_1a(wydatki_2)

print(c)
print(d)
print(a)
print(b)


print("Zadanie 2 wyniki: ")
zadanie2(wydatki_1)
zadanie2(wydatki_2)

print("Zadanie 3 wyniki: ")
alfa <- 0.05
mu <- 119

print(zadanie3(wydatki_1, mu, alfa))

print("Zadanie 4 wyniki:  ")
alfa <- 0.05
sigma0 <- 41

print(zadanie4(wydatki_2, alfa, sigma0))


print("Zadanie 5 wyniki: ")
wynik_zad_5 <- zadanie5(wydatki_1, wydatki_2)
print(wynik_zad_5)

p_value <- wynik_zad_5$p.value

if (p_value < 0.05) {
  print("Można stwierdzić, że średnie miesięczne wydatki na wyroby drogeryjne w pierwszym punkcie są większe niż w drugim punkcie z poziomem istotności 0.05.")
} else {
  print("Brak wystarczających dowodów, aby stwierdzić, że średnie miesięczne wydatki na wyroby drogeryjne w pierwszym punkcie są większe niż w drugim punkcie z poziomem istotności 0.05.")
}
