# Just for your own information, R includes many different datasets that
# can be useful for examples. You can access them below. Just put the one 
# you want to use inside the parentheses of'data'.

library(MASS)
data()

# Just put the one you want to use inside the parentheses of'data'. For
# example, to use the 'austres' time series: data(austres).

# Now your model is an arima(p,1,0) model I do believe. Let us define a
# function called Regression which runs the arima model of length p.

Regression <- function(p){
  arima(austres , order=c(p,1,0) , method="ML")
}

# Note that the default argument of the function is to include a mean (or 
# constant as it is in your model). So you don't need to worry about that.

# Let us check over a number of different p for the "best" length according
# to AIC. 

AIC <- function(p){
  Criterion <- vector(length = p)
  Corresponding <- vector(length = p)
  for(i in 1:p){
    Criterion[i] <- Regression(i)$aic
    Corresponding[i] <- i
  }
  Data <- data.frame(Corresponding , Criterion)
  colnames(Data) <- c("p" , "aic")
  return(Data)
}

Best.fit <- function(p){
  Data <- AIC(p)
  Maximum <- max(Data$aic)
  return(Data[Data$aic == Maximum , "p"])
}

Best.fit(10)

# So to run this yourself, you will need to replace austres in line 15
# with your own time series data. Then simply change the value 10 on line 
# 42 to whichever maximum p you want to check to.