# Korelacja ---------------------------------------------------------------
# Funckcja korel obejmuje 3 korelacje: Persona (domyślna), Spearman'a, Kenadll'a 
# Dopisano nową funkcje kendall zwracającą wartość korelacji oraz wartość p odczytaną z rozkładu normalnego
# Dodano warunki zabezpieczające przed błędnym typem zmiennych i nierówną długością wektorów

korel <- function(x,
                  y,
                  method = 'pearson') {
  kendall <- function(x, y) {
    Rx <- rank(x)
    Ry <- rank(y)
    new.frame <- data.frame(X = Rx, Y = Ry)
    sorted <- new.frame[order(new.frame$X),]
    len.Y = length(sorted$Y)
    id = 2
    C = c()
    D = c()
    for (i in sorted$X) {
      count_C = 0
      count_D = 0
      for (z in sorted$Y[id:len.Y]) {
        if (z > i) {
          count_C = count_C + 1
        }
        else {
          count_D = count_D + 1
        }
      }
      C <- c(C, count_C)
      D <- c(D, count_D)
      id  = id + 1
      if (id == len.Y + 1)
        break
    }
    sum_c <- sum(C)
    sum_d <- sum(D)
    n <- len.Y
    ns = (n * (n - 1)) / 2
    ken <- (sum_c - sum_d) / ns
    
    Tk <- (3 * ken * sqrt(n * n - 1)) / (sqrt(2 * (2 * n + 5)))
    p = 2 * pnorm(abs(Tk), lower.tail = FALSE)
    return(list(Rk = ken, p = p))
  }
  
  pearson = function(x, y) {
    n = length(x)
    mX = mean(x)
    mY = mean(y)
    R = sum((x - mX) * (y - mY)) /
      sqrt(sum((x - mX) ^ 2) * sum((y - mY) ^ 2))
    T = R / sqrt(1 - R ^ 2) * sqrt(n - 2)
    p = 2 * pt(abs(T), n - 2, lower.tail = F)
    
    return(list(R = R, p = p))
  }
  
  spearman = function(x, y) {
    rangi = function(x) {
      n = length(x)
      new.x = sort(x)
      new.x = as.data.frame(new.x)
      colnames(new.x) = 'obs'
      x = as.data.frame(x)
      x$ranga = 0
      colnames(x) = 'obs'
      new.n = length(unique(x))
      if (n == new.n) {
        new.x$rangi = 1:n
        for (i in 1:n) {
          x$ranga[i] = new.x$rangi[new.x$obs == x$obs[i]]
        }
      } else {
        new.x = cbind(new.x, 1:n)
        colnames(new.x) = c('obs', 'rank1')
        new.x = as.data.frame(new.x)
        tmp = aggregate(new.x$rank1,
                        by = list(new.x$obs),
                        FUN = 'mean')
        colnames(tmp) = c('obs', 'ranga')
        for (i in 1:n) {
          x$ranga[i] = tmp$ranga[tmp$obs == x$obs[i]]
        }
      }
      return(rangi = x$ranga)
    }
    R = rangi(x)
    S = rangi(y)
    n = length(x)
    
    Rs = 1 - 6 * sum((R - S) ^ 2) / (n * (n ^ 2 - 1))
    Ts = Rs / sqrt(1 - Rs ^ 2) * sqrt(n - 2)
    p = 2 * pt(abs(Ts), n - 2, lower.tail = FALSE)
    return(list(Rs = Rs, p = p))
  }
  
  if (length(x) != length(y))
    stop('Wektory muszą być tej samej długości')
  
  else if (!is.numeric(x) || !is.numeric(y))
    stop('Zmienna musi być typu numeric')
  
  
  if (method == 'pearson') {
    pearson(x, y)
    
  }
  
  else if (method == 'spearman') {
    spearman(x, y)
    
  }
  
  else if (method == 'kendall') {
    kendall(x, y)
    
  }
  
}
