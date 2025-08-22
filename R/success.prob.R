#write a function which shows the probability of one single succesful event

success.prob <- function(p, n, pl=T){
  ###error checking
  if(p > 1){
    stop('The argument p should be less than or equal to 1')
  }
  if(n%%1 != 0){
    stop('n has to be a positive integer')
  }
  if(n <= 0){
    stop('n has to be a positive integer')
  }
  ###code
  X <- seq(0, n) #The no of trials as a vector from 0 to n
  Y <- 1 - (1 - p)^X
  df <- data.frame(X,Y)
  names(df) <- c('trials', 'probability of success')
  if (pl){
    plot(X, Y, xlab='no of trials', ylab = 'probability of at least one successful event', ylim=c(-0.0,1))
  }
  return(df)
}
