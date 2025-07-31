#write a function which shows the probability of one single succesful event

success.prob <- function(p, n, pl=T){
  X <- seq(0, n) #The no of trials as a vector from 0 to n
  Y <- 1 - (1 - p)^X
  if (pl){
    plot(X, Y, xlab='no of trials', ylab = 'probability of success', ylim=c(-0.0,1))
  }
  df <- data.frame(X,Y)
  names(df) <- c('trials', 'probability of success')
  return(df)
}
