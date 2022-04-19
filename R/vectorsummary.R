#My package: vectorsummary

vectorstats <- function(x) {
  #input error handling
  if(!is.numeric(x)) stop("Input a numeric vector")

  #get quantiles
  qt <- quantile(x)
  names(qt) <- c("Min", "1st Quant.", "Median", "3rd Quant.", "Max")

  #get mean and stats
  avg <- mean(x)
  stdev <- sd(x)
  se <- stdev/sqrt(length(x))
  stats <- c(avg, stdev, se)
  names(stats) <- c("Mean", "Std Dev", "Std Error")

  #assign output object to class
  out <- list(x)
  class(out) <- "statsummary"
  out$qt <- qt
  out$stats <- stats

  #return output
  out
}

print.statsummary <- function(out) {
  cat("Quantiles:\n")
  print(out$qt)
  cat("\nStatistics:\n")
  print(out$stats)
}

vectorplots <- function(x) {
  #input error handling
  if(!is.numeric(x)) stop("Input a numeric vector")

  #change plot view and display plots
  par(mfrow=c(2,2))
  plot(x, main="Scatterplot", ylab="Values")
  boxplot(x, main="Boxplot")
  hist(x, main="Histogram", xlab="Values")
  if(length(x)>1)
    plot(density(x),main="Kernel Density")

  #reset plot view
  par(mfrow=c(1,1))
}
