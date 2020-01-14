# Przygotowanie funkcji i wykresów podsumowujacych zebrane wyniki
# PDU 18/19 - PD nr 2
# autor: Konrad Komisarczyk

library(stringi)
library(dplyr)
library(RColorBrewer)


# średnie indeksy dla poszczególnych wariantów algorytmu alg_name
alg_arguments_means <- function(indexes, alg_name) {
  indexes %>%
    filter(alg == alg_name) %>%
    group_by(argument) %>%
    summarise(meanFM = mean(FM), 
              meanAR = mean(AR),
              sdFM = sd(FM), 
              sdAR = sd(AR)) %>%
    arrange(argument)
}

# leg - czy podpisywać "indeks Fowlkesa-Mallowsa", "skorygowany indeks Randa"
alg_arguments_means_barplot <-  
  function(indexes, alg_name, 
           las = 1, mar = c(2, 3, 2, 2), leg = TRUE, ...) {
  df <- alg_arguments_means(indexes, alg_name)
  
  par(mar = mar)
  
  palette(brewer.pal(n = 8, name = "Accent"))
  
  bar <- barplot(cbind(df$meanFM, df$meanAR), beside = TRUE, 
          names.arg = rep(df$argument, 2), 
          ylim = c(0, 1),
          las = las, # 2 dla hclust i kmeans
          col = 1:length(df$meanAR),
          ...)
  box()
  text(x = bar, 
       y = c(df$meanFM, df$meanAR) - 0.075,  
       labels = round(c(df$meanFM, df$meanAR), 2), 
       cex = 0.75)
  if (leg) {
    text(x = c(bar[1] - 0.5, bar[length(bar)] + 0.5), 
         y = c(0.92, 0.92), 
         labels = c("indeks Fowlkesa-Mallowsa", "skorygowany indeks Randa"), 
         pos = c(4, 2), 
         col = "#00000050")
  }
}



#wpływ standaryzacji na średnie indeksy dla różnych wariantów algorytmu
alg_scaling_arguments_means <- function(indexes, alg_name) {
  indexes %>%
    filter(alg == alg_name) %>%
    group_by(argument, scaled) %>%
    summarise(FM = mean(FM), 
              AR = mean(AR))
}

alg_scaling_arguments_means_barplot <- 
  function(indexes, alg_name, index = "FM", 
           las = 1, mar = c(2, 3, 2, 2), ...) {
  df <- alg_scaling_arguments_means(indexes, alg_name)
  
  scaled <- df %>% filter(scaled == TRUE)
  notscaled <- df %>% filter(scaled == FALSE)
  n <- length(scaled$FM)
  
  vals <- rep(0, 2*n)
  pal <- rep(0, 2*n)
  if (index == "FM") {
    vals[2*(1:n) - 1] <- notscaled$FM
    vals[2*(1:n)] <- scaled$FM
    
    pal[2*(1:n) - 1] <- brewer.pal(n = n, name = "Dark2")
    pal[2*(1:n)] <- brewer.pal(n = n, name = "Pastel2")
  } else {
    stopifnot(index == "AR")
    vals[2*(1:n) - 1] <- notscaled$AR
    vals[2*(1:n)] <- scaled$AR
    
    pal[2*(1:n) - 1] <- brewer.pal(n = n, name = "Set1")
    pal[2*(1:n)] <- brewer.pal(n = n, name = "Pastel1")
  }
  palette(pal)
  
  par(mar = mar)
  
  barplot(vals,
          #names.arg = rep(df$argument, 2), 
          ylim = c(0, 1),
          col = 1:(2*n),
          space = rep(c(0.5, 0), n),
          ...)
  box()
  axis(1, 
       tck = 0, 
       at = 2.5 * (1:n) - 1, 
       labels = scaled$argument, 
       las = las)
  text(x = 2.5 * (1:n) - 0.5, 
       y = vals[2*(1:n)] / 2, 
       col = pal[2*(1:n) - 1],
       labels = "scaled",
       font = 2,
       srt = 90)
}

# najsłabsze indeksy dla poszczególnych wariantów algorytmu
alg_arguments_worst <- function(indexes, alg_name) {
  indexes %>%
    filter(alg == alg_name) %>%
    group_by(argument) %>%
    summarise(minFM = min(FM), set[which.min(FM)], 
              minAR = min(AR), set[which.min(AR)])
}



# średnie indeksy dla poszczególnych algorytmów 
# uśrednionych względem wariantu
all_alg_mean_means <- function(indexes) {
  indexes %>%
    group_by(alg) %>%
    summarise(meanFM = mean(FM), 
              meanAR = mean(AR))
}

all_alg_mean_means_barplot <- function(indexes, ...) {
  df <- all_alg_mean_means(indexes)
  
  par(mar = c(4.2, 3, 2, 2))
  
  palette(brewer.pal(n = 8, name = "Set2"))
  
  bar <- barplot(cbind(df$meanFM, df$meanAR), beside = TRUE, 
                 names.arg = rep(df$alg, 2), 
                 ylim = c(0, 1),
                 las = 2,
                 col = 1:length(df$meanAR),
                 ...)
  box()
  text(x = bar, 
       y = c(df$meanFM, df$meanAR) - 0.075,  
       labels = round(c(df$meanFM, df$meanAR), 2), 
       cex = 1)
  text(x = c(bar[1] - 0.5, bar[length(bar)] + 0.5), 
       y = c(0.915, 0.915), 
       labels = c("indeks Fowlkesa-Mallowsa", "skorygowany indeks Randa"), 
       pos = c(4, 2), 
       col = "#00000050")
}



# średnie indeksy dla poszczególnych algorytmów 
# dla wariantu algorytmu o największej średniej
all_alg_best_means <- function(indexes) {
  indexes %>%
    group_by(alg, argument) %>%
    summarise(meanFM = mean(FM), 
              meanAR = mean(AR)) %>%
    group_by(alg) %>%
    summarise(maxFM = max(meanFM), argFM = argument[which.max(meanFM)], 
              maxAR = max(meanAR), argAR = argument[which.max(meanAR)])
}

all_alg_best_means_barplot <- function(indexes, ...) {
  df <- all_alg_best_means(indexes)
  
  par(mar = c(4.2, 3, 2, 2))
  
  palette(brewer.pal(n = 8, name = "Set2"))
  
  bar <- barplot(cbind(df$maxFM, df$maxAR), beside = TRUE, 
                 names.arg = rep(df$alg, 2), 
                 ylim = c(0, 1),
                 las = 2,
                 col = 1:length(df$maxAR),
                 ...)
  box()
  text(x = bar, 
       y = c(df$maxFM, df$maxAR) / 2,  
       labels = c(df$argFM, df$argAR), 
       cex = 1, 
       font = 2,
       srt = 90)
  text(x = c(bar[1] - 0.5, bar[length(bar)] + 0.5), 
       y = c(0.93, 0.93), 
       labels = c("indeks Fowlkesa-Mallowsa", "skorygowany indeks Randa"), 
       pos = c(4, 2), 
       col = "#00000050")
}




# najgorsza, średnia i najlepsza ocena  i odchylenie standardowe ocen 
# dla każdego zbioru
sets_quality <- function(indexes) {
  indexes %>% 
    group_by(set) %>%
    summarise(sd(FM), min(FM), mean(FM), max(FM), 
              sd(AR), min(AR), mean(AR), max(AR))
}



# porównanie średnich wyników algorytmów dla każdego zbioru
sets_algos <- function(indexes) {
  indexes %>%
    group_by(set) %>%
    summarise("spektralny" = round(mean(AR[alg == "spectral"]), 2),
              "genie" = round(mean(AR[alg == "genie"]), 2),
              "hclust" = round(mean(AR[alg == "hclust"]), 2),
              "kmeans" = round(mean(AR[alg == "kmeans"]), 2))
}
