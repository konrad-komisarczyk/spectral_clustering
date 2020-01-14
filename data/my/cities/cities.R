# Generowanie zbioru testowego cities
# PDU 18/19 - PD nr 2
# autor: Konrad Komisarczyk

library(dplyr)
#setwd(file.path("data", "my", "cities"))

# wczytujemy dane zescrapowane pythonem
coords <- read.table("coords", sep = ";", stringsAsFactors = FALSE)
colnames(coords) <- c("C1", "C2", "C3")

population <- read.table("population", sep = ";", stringsAsFactors = FALSE)

# tabele zgadzają się wierszami, złączamy je kolumnami
cities <- cbind(population, coords)


voivodeships <- unique(cities$V3)
powiats <- unique(cities$V2)
n <- nrow(cities)

cities <- cities %>% mutate(
  # skalujemy współrzędne na powierzchnię euklidesową
  y = C3 * 110.574, x = C2 * 111.32 * cos(0.01745329 * C3), 
  # obliczamy promień koła przybliżającego miasto
  area = V4, r = sqrt(area / pi),
  # obliczamy liczbę punktów reprezentujących miasto
  popul = V4 * V6, points = round(popul / 4000),
  # kategoryzujemy miasta po województwie, powiecie i mieście
  voiv = match(V3, voivodeships), 
  powi = match(V2, powiats), 
  city = 1:n)


# tabela zawierająca wiersze odpowiadające naszym punktom - tyle kopii 
# wierszy odpowiadających miastu z tabeli cities, ile punktów je reprezentuje
cities2 <- cities %>% 
  tidyr::uncount(points)


# rozrzucamy punkty rozkładami normalnymi względem środków miast, które 
# reprezentują i promieni tych miast
n <- nrow(cities2)
t <- runif(n, 0, 2*pi)
a <- rnorm(n, 0, cities2$r)
cities2$x <- sin(t) * a + cities2$x
cities2$y <- cos(t) * a + cities2$y






cities_coords <- t(cbind(cities2$x, cities2$y))

## podział na grupy po województwie
write(cities_coords, "by_voivodeship.data", ncolumns = 2)
write(cities2$voiv, "by_voivodeship.labels0", ncolumns = 1)

## podział na grupy po powiecie
write(cities_coords, "by_powiat.data", ncolumns = 2)
write(cities2$powi, "by_powiat.labels0", ncolumns = 1)

## podział na grupy po mieście
write(cities_coords, "by_city.data", ncolumns = 2)
write(cities2$city, "by_city.labels0", ncolumns = 1)


