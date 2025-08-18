####write a moving average function wih a regularization penalty
rm(list = ls())
library(StatsChitran)
ClearPlot()
movavg.reg <- function(X, Y, bn, fn, lambda, ord.min = 1, ord.max = 40, pl = T, grid.search = T){

  ##The order function which returns the objective function (SSE + regularazation term)
  obj.fun <- function(ord){
    ord <- round(ord)
    df <- movavg(X, Y, bn, fn, ord)
    SSE <- sum((df$Y - Y)^2) #The sum squared error term
    MSE <- SSE/length(df$Y) #The Mean squared error term
    d2Y <- num_diff(df$X, df$Y, pl = F, order = 2)
    d2Y$Y <- d2Y$Y^2
    integration <- num_integrate(d2Y$X, d2Y$Y, xmin = min(d2Y$X), xmax = max(d2Y$X))
    pen <- lambda*integration
    res <- MSE + pen
    return(res)
  }
  if(!grid.search){
    L <- constrOptim(theta = (ord.min+ord.max)/2, f = obj.fun, ui = matrix(data = c(1, -1), nrow = 2, byrow = T), grad = NULL, ci = matrix(data = c(ord.min, -ord.max), nrow = 2, byrow = T), method = 'SANN')
    df <- movavg(X, Y, bn, fn, ord = round(L$par))
    L_ret <- vector(mode = 'list', length = 3)
    L_ret[[1]] <- L
    L_ret[[2]] <- df
    L_ret[[3]] <- L$par
    if(pl){
      plot(X, Y)
      lines(df$X, df$Y, col = 'red')
    }
    return(L_ret)
  }else{
    ord.val <- seq(ord.min, ord.max) #sequence of values to be sequenced
    ord.reg <- NULL #sequence of returned MSE+penalty regularization terms
    for (k in ord.val) {
      res <- obj.fun(k)
      ord.reg <- c(ord.reg, res)
    }
    df_grid <- data.frame(ord.val, ord.reg)
    if(pl){
      df <- movavg( X, Y, bn, fn, ord = df_grid$ord.val[df_grid$ord.reg == min(df_grid$ord.reg)] )
      plot(X, Y)
      lines(df$X, df$Y, col = 'red')
    }
    L_ret <- vector(mode = 'list', length = 3)
    L_ret[[1]] <- df_grid
    L_ret[[2]] <- df
    L_ret[[3]] <- df_grid$ord.val[df_grid$ord.reg == min(df_grid$ord.reg)]
    return(L_ret)
  }

}



######Testing the function
###create the noisy dataset -------- #################
X <- seq(-10, 10, by=0.25)
noise.data <- function(X){
  G1 <- gauss(X, amp = 3, mu = -3, sig = 2, probability = F)
  L1 <- gauss(X, amp = sqrt(3), mu=5, sig = 3, probability = F)
  Y <- G1 - L1
  set.seed(13)
  noise <- runif(min = -1, max = 1, length(Y))
  Y <- Y + noise
  plot(X, Y, col = rgb(0.25,0,0,0.25))
  return(data.frame(X, Y))
}

df <- noise.data(X)
dfm1 <- movavg(df$X, df$Y, bn = 3, fn = 3, ord = 1)
dfm2 <- movavg(df$X, df$Y, bn = 3, fn = 3, ord = 10)
plot(df$X, df$Y)
lines(dfm1$X, dfm1$Y, col = 'red')
lines(dfm2$X, dfm2$Y, col = 'green')
L <- movavg.reg(X = df$X, Y = df$Y, bn = 3, fn = 3, ord.min = 1, ord.max = 100, lambda = 0.1, grid.search = T)
