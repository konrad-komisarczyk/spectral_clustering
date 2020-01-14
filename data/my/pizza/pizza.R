# Generowanie zbioru testowego pizza
# PDU 18/19 - PD nr 2
# autor: Konrad Komisarczyk


## oliwki na  pizzy pokrojonej na równe kawałki
## sets - wektor z liczbą punktów należących do kolejnych kawałków pizzy
## space - promień, o jaki odsunięte są kawałki od środka
pizza <- function(sets, space) {
  n <- sum(sets)
  parts <- length(sets)
  
  # punkty należące do nr-tego kawałka
  points_in_part <- function(n, nr, parts, space) {
    # generujemy n punktów rozłożonych równomiernie na nr-tym kawałku pizzy
    t <- runif(n, (nr-1)*2*pi/parts, nr*2*pi/parts)
    a <- runif(n, 0, 1)
    a <- acos(a)
    x <- sin(t) * a
    y <- cos(t) * a
    
    # odsuwamy kawałek od środka
    v <- (nr - 0.5)*2*pi/parts
    x <- x + sin(v) * space
    y <- y + cos(v) * space
    
    return(cbind(x, y))
  }
  
  X <- matrix(numeric(0), ncol = 2)
  labels <- numeric(0)
  for (i in 1:parts) {
    p <- points_in_part(sets[i], i, parts, space)
    X <- rbind(X, p)
    labels <- c(labels, rep(i, sets[i]))
  }
  
  return(list(coords = X, labels = labels))
}



#setwd(file.path("data", "my", "simple_agglomerations"))

# pizza1
set.seed(1234)
n <- 4
sets <- runif(n, 100, 200)
pitca1 <- pizza(sets, 0.15)
write(t(as.matrix(pitca1$coords)), "pizza1.data", ncolumns = 2)
write(as.vector(pitca1$labels), "pizza1.labels0", ncolumns = 1)

# pizza2
set.seed(7312)
n <- 6
sets <- runif(n, 50, 150)
pitca2 <- pizza(sets, 0.3)
write(t(as.matrix(pitca2$coords)), "pizza2.data", ncolumns = 2)
write(as.vector(pitca2$labels), "pizza2.labels0", ncolumns = 1)
