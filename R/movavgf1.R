#write a movavgf function to accept another function and use it as a moving window for moving average calculations
rm(list=ls())
library(StatsChitran)
movavgf1 <- function(X,Y, bn, fn, f, val, ord = 1){
  err.vec.dim(val, 1)
  err.WN.vec.dim(bn, 1)
  err.WN.vec.dim(fn, 1)
  if( (length(bn) != 1)|(length(fn) != 1) ){
    stop('Inputs to fn and fb should be numeric and single digit')
  }else if(length(X) != length(Y)){
    stop('length of input vectors X and Y need to be the same')
  }else if(is.function(f) == F){
    stop('parameter f needs to be a function')
  }else{
    samp <- mean(diff(X)) #The sampling difference
    dev <- sd(diff(X)) #Standard dev. of sampling difference. Only for input to user.
    warning('The sampling mean is ', samp,' while the standard deviation is ', dev, '\n' )
    warning('values of sampling mean will be used to create smoothing function for moving averages', '\n')
    #build the window sequence##############################################
    span1 <- ArithProg(st=val, n = fn+1, d = samp)
    span2 <- ArithProg(st= val - samp, n = bn, d = -samp)
    span <- c(rev(span2), span1)
    if(length(span) != bn + fn + 1){
      stop('Check length of span vector')
    }
    k <- bn + 1 #index value of 'val'
    f_seq <- f(span) #write the function into a sequence
    if(sum(abs(f_seq) == Inf ) != 0){
      stop('Infinities exist in your window function f')
    }
    if(sum(is.na(f_seq)) != 0){
      stop('NAs exist in the window function')
    }
    #iterate over the window sequence############################
    for (k1 in 1:ord) {
      y_res <- NULL
      for (i in 1:length(Y)) {
        dmp <- sum((f_seq[max(k + 1 - i, 1) : min(k + fn, k + length(Y) - i) ])*(Y[max(1,i-bn): min(i+fn, length(Y))]))/sum((f_seq[max(k + 1 - i, 1) : min(k + fn, k + length(Y) - i) ]))
        y_res <- c(y_res, dmp)
      }
      Y <- y_res
    }

  }
  ###############################################################
  #build and return the smoothed dataframe#######################
  df <- data.frame(X, Y)
  names(df) <- c('X','Y')
  return(df)
  ###############################################################
}























#########Testing#####################################################
library(StatsChitran)
#build the dataset
df <- noisy.gaussians


##define the kernel function (lorentzian)
lor.avg <- function(v){
  return(lorentz(X = v, x_0 = 0, gamm = 1, probability = T))
}

##divide the plot into two
subplot(c(1,2))

##plot the kernel function
Y_kernel <- lor.avg(df$X)
plot(df$X, Y_kernel, type = 'l', col = 'red', main = 'The lorentzian kernel')
abline(v = 0)


##Perform kernel smoothening of ord = 1, 10, 100
df1 <- movavgf1(X = df$X, Y = df$Y, bn=3, fn=3, f = lor.avg, val = 0, ord = 1)
df2 <- movavgf1(X = df$X, Y = df$Y, bn=3, fn=3, f = lor.avg, val = 0, ord = 10)
df3 <- movavgf1(X = df$X, Y = df$Y, bn=3, fn=3, f = lor.avg, val = 0, ord = 100)

##Plot the smoothed dataframes against the main data
plot(df$X, df$Y, main = 'Data Processing')
lines(df1$X, df1$Y, col = 'green')
lines(df2$X, df2$Y, col = 'red')
lines(df3$X, df3$Y, col = 'blue')
legend(x = 'topright', legend = c('data', 'smooth. ord = 1', 'smooth. ord = 10', 'smooth. ord = 100'), fill = c('black', 'green', 'red', 'blue'))
