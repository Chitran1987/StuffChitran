rm(list=ls())
library(StatsChitran)

###create the noisy dataset -------- #################
X <- seq(-10, 10, by=0.5)
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
#####################################################

###Start the optimization process with the gaussian kernel
fit.kernel <- function(df, lambda, sdev){
  #op_vec has to be equal to length of df$X

  #Write the structure function
  strct <- function(op_vec, df, sdev){L <- vector(mode = 'list', length = length(op_vec))
  for (i in 1:dim(df)[1]) {
    L[[i]] <- gauss(X = df[,1], amp = op_vec[i], mu = df[,1][i], sig = sdev, probability = F)
  }
  L_f <- Reduce('+', L) ###This is a vector (The final list for now)}
  return(L_f)
  }

  #Write the function for the 2nd derivative
  pen.d2Y <- function(op_vec, df, sdev){
    D2L_f <- num_diff(X = df[,1], Y = strct(op_vec, df, sdev), pl = F, order = 2) #The second derivative function
    pen_d2Y <- num_integrate(X = D2L_f$X, Y = abs(D2L_f$Y), xmin = min(D2L_f$X), xmax = max(D2L_f$X)) #The integral part of the penalty term
    return(pen_d2Y^2)
  }


  ###This is the objective function
  obj.fun <- function(op_vec){
    SSE <- sum((strct(op_vec, df, sdev) - df$Y)^2)
    pen <- lambda*pen.d2Y(op_vec, df, sdev)
    return(SSE + pen)
  }

  ###Call the optimization function
  L_res <- optim(par = df$Y, fn = obj.fun)
  return(L_res)
}




L <- fit.kernel(df, lambda = 0, sdev = 0.1)
L1 <- fit.kernel(df, lambda = 0.1, sdev = 0.1)
#lines(X, L$par, col = 'red')
lines(X, L1$par, col = 'green')
#df_m <- movavg(X, df$Y, 3, 3)
#lines(X, df_m$Y, col = 'blue')
