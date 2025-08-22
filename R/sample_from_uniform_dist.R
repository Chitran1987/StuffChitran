#sampling from a uniform distribution and then calculating the mean and sd
samp.from.unidistr <- function(){
  library(StatsChitran)
  set.seed(14)
  ##sample from a set
  #build the population from a uniform distribution
  set.pop <- runif(10^9, min = -1, max = 1)
  avg <- NULL
  sdev <- NULL
  df <- data.frame(avg, sdev)
  for (i in 1:10^3) {
    set.sample <- sample(set.pop, size = 10^5, replace = F)
    df1 <- data.frame(avg = mean(set.sample), sdev = sd(set.sample))
    df <- rbind(df, df1)
  }

  ###Build the histograms
  subplot(c(1,2))
  hist(df$avg, breaks = seq(-0.01, 0.01, by = 0.000375))
  hist(df$sdev, breaks = seq(0.57, 0.585, by = 0.00025))
  subplot(c(1,1))

  ###Return
  return(NULL)
}

