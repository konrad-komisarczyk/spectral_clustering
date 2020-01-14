# Generowanie wyników testów algorytmów analizy skupień na benchmarkach
# PDU: PD nr 2
# autor: Konrad Komisarczyk

source("spectral.R")

library(stringi)
library(dplyr)
options(stringsAsFactors = FALSE)



# - jeżeli plik "[nazwa zbioru].[nazwa metody].out" nie istnieje, to
# wykonuje algorytm clustering_fun(X, k) na zbiorze path i zapisuje wynikowe
# etykiety do tego pliku, w p. p. wczytuje etykiety z tego pliku
# - zwraca obliczone etykiety, oraz indeksy Fowlkesa–Mallowsa i Randa 
# z porównania wyniku z referencyjnymi etykietami
test_file <- function(path, clustering_fun, alg, arg, scale) {
  stopifnot(is.character(path) & is.character(alg) & is.character(arg))
  
  file_path <- path %s+% ".data.gz"
  stopifnot(file.exists(file_path))
  
  labels0_path <- path %s+% ".labels0.gz"
  stopifnot(file.exists(labels0_path))
  
  out_path <- path %s+% "." %s+% 
    ifelse(scale, "scaled_", "") %s+%
    alg %s+% "_" %s+% arg %s+% ".out"
  
  # wczytujemy sbiór wejściowy
  X <- as.matrix(read.table(file_path))
  
  if (scale) {
    # standaryzujemy zbiór
    X <- scale(X)
  }
  
  # wczytujemy etykiety referencyjne
  labels0 <- scan(labels0_path)
  
  #sprawdzamy poprawność danych w plikach
  stopifnot(is.numeric(X))
  stopifnot(is.numeric(labels0))
  stopifnot(nrow(X) == length(labels0))
  
  if (file.exists(out_path)) {
    print(path %s+% " already done.")
    
    # wczytujemy już wczesniej policzone labels
    out <- scan(out_path)
    
    # sprawdzamy poprawność danych w pliku
    stopifnot(is.numeric(out))
    stopifnot(nrow(X) == length(out))
    
    print(out_path %s+% " read.")
  } else {
    # wyliczamy wskazaną metodą
    k <- max(labels0)
    out <- clustering_fun(X = X, k = k)
    
    # sprawdzamy poprawność formatu policzonych etykiet
    stopifnot(is.numeric(out))
    stopifnot(nrow(X) == length(out))
    
    print(out_path %s+% " computed.")
    
    # zapisujemy wynik do pliku
    write(as.vector(out), out_path, ncolumns = 1)
    print(out_path %s+% " written.")
  }
  
  # liczymy indeksy
  AR <- mclust::adjustedRandIndex(labels0, out)
  FM <- as.vector(clues::adjustedRand(labels0, out, randMethod = "FM"))
  #print("Indexes calculated.")
  
  return(list(out = out, AR = AR, FM = FM))
}



# funkcje opakowywujące testowane algorytmy (do użycia w clustering_funs)
.spectral <- function(M) {
  function(X, k) spectral_clustering(X, k, M)
}

# M dla których będziemy testować funkcję na nie skalowanych zbiorach
spectral_args = c(3:9, 12, 16, 20, 32, 44)

# M dla których będziemy testować funkcję na skalowanych zbiorach
spectral_args2 = 5:9



.genie <- function(thresholdGini) {
  function(X, k) cutree(genie::hclust2(d = "euclidean", 
                                       objects = X,
                                       thresholdGini = thresholdGini), 
                        k = k)
}

# wartości parametru thresholdGini które będziemy testować
genie_args = c(0.1, 0.2, 0.3, 0.6, 0.8, 1)



.hclust <- function(method) {
  function(X, k) cutree(hclust(dist(X), method = method), k = k)
}

# warianty method funkcji hclust, które będziemy testować
hclust_args = c("ward.D", "ward.D2", "single", "complete", "average", 
                "mcquitty", "median", "centroid")



.kmeans <- function(method) {
  function(X, k) kmeans(X, k, algorithm = method)$cluster
}

# warianty algorithm funkcji kmeans, które będziemy testować
kmeans_args = c("Hartigan-Wong", "Forgy", "MacQueen")




# wszystkie rodzaje testów jakie będziemy wykonywać
tests <- list(
  funs = c(lapply(as.list(spectral_args), FUN = .spectral), 
           lapply(as.list(genie_args), FUN = .genie),
           lapply(as.list(hclust_args), FUN = .hclust), 
           lapply(as.list(kmeans_args), FUN = .kmeans),
           
           lapply(as.list(spectral_args2), FUN = .spectral), 
           lapply(as.list(genie_args), FUN = .genie),
           lapply(as.list(hclust_args), FUN = .hclust), 
           lapply(as.list(kmeans_args), FUN = .kmeans)),
  
  args = c(as.character(spectral_args),
           as.character(genie_args),
           as.character(hclust_args),
           as.character(kmeans_args),
           
           as.character(spectral_args2),
           as.character(genie_args),
           as.character(hclust_args),
           as.character(kmeans_args)),
  
  algos = c(rep("spectral", length(spectral_args)), 
            rep("genie", length(genie_args)),
            rep("hclust", length(hclust_args)),
            rep("kmeans", length(kmeans_args)), 
            
            rep("spectral", length(spectral_args2)), 
            rep("genie", length(genie_args)),
            rep("hclust", length(hclust_args)),
            rep("kmeans", length(kmeans_args))),
  
  scaled = c(rep(FALSE, 
                 length(spectral_args)  
                 + length(genie_args) 
                 + length(hclust_args) 
                 + length(kmeans_args)), 
             rep(TRUE, 
                 length(spectral_args2)  
                 + length(genie_args)
                 + length(hclust_args) 
                 + length(kmeans_args)))
)

n_tests <- 51
stopifnot(length(tests$scaled) == n_tests)
stopifnot(length(tests$algos) == n_tests)
stopifnot(length(tests$args) == n_tests)
stopifnot(length(tests$funs) == n_tests)



# zwraca wektor ścieżek do plików benchmarkowych, 
# czyli wszystkich plików w strukturze drzewa katalogów poniżej katalogu
# bieżącego, kończących się na ".data.gz" - z uciętą tą końcówką
get_paths <- function(d) {
  files <- list.files(d, "\\.data\\.gz$", recursive = TRUE)
  paths <- stri_sub(files, 1, stri_length(files) - 8)
  return(paths)
}


# wykonujemy wszystkie rodzaje testów na wszystkich plikach:
ratings_df <- data_frame()
output_labels_list <- list()
j = 1
for (i in 1:n_tests) {
  for (path in get_paths(getwd())) {
    fun <- tests$funs[[i]]
    algo <- tests$algos[i]
    arg <- tests$args[i]
    scaled <- tests$scaled[i]
    
    print(path)
    print(algo)
    print(arg)
    
    # wykonujemy test
    result <- test_file(path, fun, algo, arg, scaled)
    
    # zapisujemy do zbiorów, w których kolekcjonujemy wyniki
    set_name = basename(path)
    row = data.frame(nr = j, alg = algo, argument = arg, set = set_name,
                     scaled = scaled, FM = result$FM, AR = result$AR)
    ratings_df <- ratings_df %>% rbind(row)
    
    output_labels_list[[j]] <- list(result$out)
    j <- j + 1
  }
}



# zapisujemy wyniki do plików:

# plik z listą etykiet wynikowych:
output_labels_file = file.path("out", "output_labels.rds")
saveRDS(outs_list, file = output_labels_file)
# plik z informacjami o kolejnych testach - ramka danych o kolumnach
# [nr, alg, argument, set, FM, AR]:
indexes_file = file.path("out", "indexes.csv")
write.csv(ratings_df, file = indexes_file)
