####write a moving average function wih a regularization penalty
library(StatsChitran)
movavg.reg <- function(X, Y, bn, fn, lambda, pl = T){

  ##The order function which returns the objective function (SSE + regularazation term)
  obj.fun <- function(ord){
    ord <- round(ord)
    df <- movavg(X, Y, bn, fn, ord)
    SSE <- sum((df$Y - Y)^2) #The sum squared error term
    MSE <- SSE/length(df$Y) #The Mean squared error term
    d2Y <- num_diff(df$X, df$Y, pl = F, order = 2)
    d2Y$Y <- d2Y$Y^2
    integration <- num_integrate(d2Y$X, d2Y$Y, xmin = min(d2Y$X), xmax = max(d2Y$Y))
    pen <- lambda*integration
    res <- MSE + pen
    return(res)
  }

  L <- constrOptim(theta = 1, f = obj.fun, ui = matrix(data = c(1, -1), nrow = 2, byrow = T), ci = matrix(data = c(0, -5000), nrow = 2, byrow = T))
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
}



