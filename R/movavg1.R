movavg1 <- function(X,Y, bn, fn, ord = 1){
  if( (length(bn) != 1)|(length(fn) != 1) ){
    stop('Inputs to fn and fb should be numeric and single digit')
  }else if(length(X) != length(Y)){
    stop('length of input vectors X and Y need to be the same')
  }else if( ord <= 0 | ord%%1 != 0 ){
    stop('Parmater ord needs to be a positive integer')
  }else{
    y_res <- NULL
    for (k in 1:ord) {
      y_res <- NULL
      for (i in 1:length(Y)) {
        y_res <- c( y_res, sum( Y[max(1,i-bn): min(i+fn, length(Y))] )/( min(i+fn, length(Y)) - max(1, i-bn) + 1 ) )
      }
      Y <- y_res

    }
    df <- data.frame(X, Y)
    return(df)
  }
}





#############Testing####################################################################################################
library(StatsChitran)
df <- noisy.gaussians
df1 <- movavg1(df$X, df$Y, bn = 3, fn = 3, ord = 1)
df10 <- movavg1(X = df$X, Y = df$Y, bn = 3, fn = 3, ord = 10)
df100 <- movavg1(df$X, df$Y, bn = 3, fn = 3, ord = 100)
plot(df$X, df$Y)
lines(df1$X, df1$Y, col = 'green')
lines(df10$X, df10$Y, col = 'red')
lines(df100$X, df100$Y, col = 'blue')
