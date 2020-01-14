# Generowanie zbioru testowego simple_agglomerations
# PDU 18/19 - PD nr 2
# autor: Konrad Komisarczyk


## kilka skupisk punktów rozkładających się rozkładem normalnym względem 
## pewnych środków aglomeracji
## n_points - wektor z liczbą punktów w kolejnych aglomeracjach
## radius - promienie kolejnych aglomeracji
## lim - bok kwadratu, na którym umieszczone są środki aglomeracji
simple_agglomerations <- function(n_points, radius, lim) {
  n_aggls <- length(n_points)
  
  agglomeration <- function(center, n, radius) {
    rnorm(n, mean = center, sd = radius)
  }
  
  centers_x <- runif(n_aggls, min = 0, max = lim)
  centers_y <- runif(n_aggls, min = 0, max = lim)
  
  X <- matrix(numeric(0), ncol = 2)
  labels <- numeric(0)
  for (i in 1:n_aggls) {
    X <- rbind(X, (cbind(agglomeration(centers_x[i], n_points[i], 
                                       radius[i]), 
                         agglomeration(centers_y[i], n_points[i], 
                                       radius[i]))))
    labels <- c(labels, rep(i, n_points[i]))
  }
  
  return(list(coords = X, labels = labels))
}



#setwd(file.path("data", "my", "simple_agglomerations"))

# simple_agglomerations1
set.seed(1234)
n <- 5
set1 <- simple_agglomerations(n_points = runif(n, 100, 400),
                              lim = 400, radius = runif(n, 20, 36))

write(t(as.matrix(set1$coords)), "simple_agglomerations1.data", ncolumns = 2)
write(as.vector(set1$labels), "simple_agglomerations1.labels0", ncolumns = 1)

# simple_agglomerations2
set.seed(7312)
n <- 3
set2 <- simple_agglomerations(n_points = runif(n, 300, 600),
                              lim = 300, radius = runif(n, 30, 50))


write(t(as.matrix(set2$coords)), "simple_agglomerations2.data", ncolumns = 2)
write(as.vector(set2$labels), "simple_agglomerations2.labels0", ncolumns = 1)

# simple_agglomerations3
set.seed(123)
n <- 8
set3 <- simple_agglomerations(n_points = runif(n, 10, 200),
                              lim = 200, radius = rep(n, 13))


write(t(as.matrix(set3$coords)), "simple_agglomerations3.data", ncolumns = 2)
write(as.vector(set3$labels), "simple_agglomerations3.labels0", ncolumns = 1)