# Implementacja algorytmu spektralnego analizy skupień
# PDU 18/19 - PD nr 2
# autor: Konrad Komisarczyk

library(Matrix)
library(RSpectra)


Mnn <- function(X, M) {
  stopifnot(is.matrix(X) & is.numeric(X))
  stopifnot(is.numeric(M) & length(M) == 1 & M <= nrow(X))
  
  # obliczamy macierz distances o wymiarze n x n, taką że 
  # distances[a, b] == odległość między a-tym i b-tym punktem z X
  distances <- as.matrix(dist(X))
  
  # indeksy 2, 3, ..., k-tej wartości w wektorze x
  # działa w O(n + k*logk), 
  # czyli szybciej od order(x)[2:k] działającego w O(n*logn) (n = length(x))
  almost_smallest_indices <- function(x, k) {
    kth <- max(sort(x, partial = k)[1:k])
    smallest <- c(which(x < kth), which(x == kth))[1:k]
    smallest[-which.min(x[smallest])]
  }
  
  # dla każdego punktu wybieramy M punktów o najmniejszej odległości od niego
  # (od 2, bo najmniejsza jest zawsze odległość punktu od samego siebie)
  mnn <- apply(distances, 1, function(x) almost_smallest_indices(x, M + 1))
  
  # dla M == 1 mnn jest wektorem, a w p. p. mnn jest obrócona
  if (M == 1) {
    return(matrix(mnn, ncol = 1))
  } else {
    return(t(mnn))
  }
}


Mnn_graph <- function(S) {
  stopifnot(is.matrix(S) & is.numeric(S))
  n <- nrow(S)
  M <- ncol(S)
  
  # obliczamy macierz sąsiedztwa
  # najpierw obliczamy macierz dla grafu skierowanego, w którym
  # istnieje krawędź (a, b) <=> b jest wśród sąsiadów a
  G_directed <- apply(S, 1, function(x) {
    # ciąg wartości logicznych długości n taki, że przyjmuje wartość TRUE
    # na i-tej pozycji, jeśli i występuje w x,
    # funkcja tabulate zwraca ciąg długości max(x), trzeba więc uzupełnić
    # do długości n
    c(as.logical(tabulate(x)), rep(FALSE, times = n - max(x)))
  })
  # musimy teraz dodać do grafu odwrócone krawędzie, bo ma być nieskierowany
  G <- G_directed | t(G_directed)
  
  # znajdujemy spójne składowe
  graph <- igraph::graph_from_adjacency_matrix(G, mode = "undirected")
  components <- igraph::components(graph)
  
  if (components$no > 1) {
    # uspójniamy graf
    # znajdujemy po jednym wierzchołku z każdej spójnej składowej
    comp_representants <- match(1:components$no, components$membership)
    
    # dodajemy krawędzie wychodzące z reprezanta pierwszej spójnej składowej
    # do reprezentantów pozostałych składowych
    G[comp_representants[1], comp_representants[-1]] <- TRUE
    G[comp_representants[-1], comp_representants[1]] <- TRUE
  }
  
  return(G)
}

Laplacian_eigen <- function(G, k) {
  stopifnot(is.matrix(G) & is.logical(G) & nrow(G) == ncol(G))
  stopifnot(is.numeric(k) & length(k) == 1 & k >= 2 & k <= nrow(G))
  
  n <- nrow(G)
  
  # będziemy używać macierzy rzadkiej
  G <- Matrix(G, sparse = TRUE)
  
  # obliczamy stopnie wierzchołków w G
  G.degrees <- apply(G, 1, sum)
  
  # wyznaczamy laplasjan
  L <- diag(G.degrees, n, n) - G
  
  # wyznaczamy wektory własne
  # funkcja RSpectra::eigs pozwala na obliczenie tylko k+1 najmniejszych
  # wartości własnych, użycie jej bardzo znacznie pozwala zmniejszyć
  # czas działania algorytmu w porównaniu do użycia bazowej funkcji eigen()
  eig <- RSpectra::eigs(L, k + 1, which = "SM")$vectors
  
  # zwracamy wektory własne odpowiadające 2, ..., k+1 najmniejszej wartości 
  # własnej
  # wartości własne zwracane przez funkcję eigs posortowane są malejąco,
  # a i-tej wartości własnej odpowiada wektor z i-tej kolumny, zatem
  # wyrzucamy ostatni wektor odpowiadający 1 najmniejszej wartości własnej
  return(eig[, 1:k])
}


spectral_clustering <- function(X, k, M) {
  stopifnot(is.matrix(X) & is.numeric(X))
  stopifnot(is.numeric(M) & length(M) == 1 & M <= nrow(X))
  stopifnot(is.numeric(k) & length(k) == 1 & k >= 2 & k <= nrow(X))
  
  mnn <- Mnn(X, M)
  mnn_graph <- Mnn_graph(mnn)
  laplacian_eigen <- Laplacian_eigen(mnn_graph, k)
  return(kmeans(laplacian_eigen, centers = k)$cluster)
}

